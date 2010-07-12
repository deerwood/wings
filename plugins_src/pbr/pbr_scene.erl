%%
%%  pbr_scene.erl
%%
%%     Pbr scene handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_scene).

-include("pbr.hrl").
-include("wings.hrl").

-export([init/3,
	 intersect/2]).

-export([diffuse/2]).

-record(scene, 
	{data,
	 isect,
	 get_mat
	}).

-record(mesh, {fv, size, mats}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(St = #st{shapes=Shapes}, Opts, R = #renderer{cl=CL}) ->
    Subdivs  = proplists:get_value(subdivisions, Opts, 1),
    Wes = ?TC([ prepare_mesh(We, Subdivs, St)
		|| We <- gb_trees:values(Shapes),
		   ?IS_VISIBLE(We#we.perm)]),
    RawData = [{Sz, fun(Face) -> 
			    Skip = Face * 96,
			    <<_:Skip/binary, 
			      V1x:?F32, V1y:?F32, V1z:?F32, _:20/binary,
			      V2x:?F32, V2y:?F32, V2z:?F32, _:20/binary,
			      V3x:?F32, V3y:?F32, V3z:?F32, _/binary>> = FV,
			    {{V1x,V1y,V1z}, {V2x,V2y,V2z}, {V3x,V3y,V3z}}
		    end}
	       || #mesh{fv=FV,size=Sz} <- Wes],
    F2M = array:from_list(lists:append([Mesh#mesh.mats || Mesh <- Wes])),
    GetMat = fun(Face) -> array:get(Face, F2M) end,
    AccelBin = ?TC(e3d_qbvh:init(RawData)),
    %% test(AccelBin, GetMat, R),
    Scene = init_accel(CL, AccelBin, #scene{data=Wes, get_mat=GetMat}),
    R#renderer{scene=Scene}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_mesh(We, SubDiv, St = #st{mat=Mtab}) ->
    Options = [{smooth, true}, {subdiv, SubDiv}, {attribs, uv}],
    Vab = wings_draw_setup:we(We, Options, St),
    %% I know something about this implementation :-)
    try 
	#vab{face_vs={32,Vs}, face_sn={0,Ns}, mat_map=MatMap} = Vab,
	%% Change Normals
	Data = swap_normals(Vs, Ns, <<>>),
	MM = lists:reverse(lists:keysort(3, MatMap)),
	Mats = fix_matmap(MM, Mtab, []), %% This should be a tree?
	#mesh{fv=Data, mats=Mats, size=byte_size(Vs) div 96}
    catch _:badmatch ->
	    erlang:error({?MODULE, internal_format_changed})
    end.

swap_normals(<<Vs:12/binary, _:12/binary, Uv:8/binary, NextVs/binary>>, 
	     <<Ns:12/binary, NextNs/binary>>, Acc) ->
    swap_normals(NextVs, NextNs, <<Acc/binary, Vs/binary, Ns/binary, Uv/binary>>);
swap_normals(<<>>,<<>>, Acc) -> Acc.

fix_matmap([_MI={Mat, _, _Start, Count}|Mats], Mtab, Acc0) ->
    Diff = get_color(Mat, Mtab),
    Acc = append_color(Count div 3, Diff, Acc0),
    fix_matmap(Mats, Mtab, Acc);
fix_matmap([], _, Acc) -> Acc.

append_color(N, Diff, Acc) when N > 0 ->
    append_color(N-1, Diff, [Diff|Acc]);
append_color(_, _, Acc) -> Acc.

get_color(Name, Mats) ->
    Mat = gb_trees:get(Name, Mats),
    OpenGL = proplists:get_value(opengl, Mat),
    {R,G,B,A} = proplists:get_value(diffuse, OpenGL),
    <<(trunc(R*255)), (trunc(G*255)), (trunc(B*255)), (trunc(A*255))>>.

diffuse(Face, #scene{get_mat=GetMat}) ->
    GetMat(Face).

init_accel(CL, {_BB, Qnodes, Qtris, _Map}, Scene) ->
    Kernel  = pbr_cl:compile(CL, "qbvh_kernel.cl"),
    Context = pbr_cl:get_context(CL),
    Device  = pbr_cl:get_device(CL),
    Copy = [read_only, copy_host_ptr],
    {ok, QN} = cl:create_buffer(Context, Copy, byte_size(Qnodes), Qnodes),
    {ok, QT} = cl:create_buffer(Context, Copy, byte_size(Qtris), Qtris),
    {ok,Local} = cl:get_kernel_workgroup_info(Kernel, Device, work_group_size),
    {ok,Mem} = cl:get_kernel_workgroup_info(Kernel, Device, local_mem_size),
    
    io:format("Scene: WG ~p LMem ~p~n",[Local,Mem]),
    
    Scene#scene{isect={Kernel, QN, QT, 64}}.

intersect({NoRays, RaysBin}, #renderer{scene=Scene, cl=CL}) ->
    {Kernel, Qn, Qt, WGSz} = Scene#scene.isect,
    Context = pbr_cl:get_context(CL),
    {ok, Rays} = cl:create_buffer(Context, [read_only],  byte_size(RaysBin), RaysBin),
    HitSz = NoRays * ?RAYHIT_SZ,
    {ok, Hits} = cl:create_buffer(Context, [write_only], HitSz),
    Args = [Rays,Hits,Qn,Qt, NoRays, {local,24*WGSz*4}],
    Running = pbr_cl:run(CL, ?MAX_RAYS, WGSz, Kernel, Args, {Hits, HitSz}),
    {ok, <<Result:HitSz/binary, _/binary>>} = cl:wait(Running, 1000),
    {Rays, Result}.

test(QBVH, GetMat, Cam) ->
    Rays = [{X,pbr_camera:generate_ray(Cam, float(X), 127.0)} || 
	       X <- [260,261]], %%lists:seq(260, 264, 2)],
    Test = fun({Pos, {hit,Dist,B1,B2,F}}) ->
		   io:format("~n~n**** HIT ~p: ~p ~w *****~n",[Pos, F, GetMat(F)])
	   end,
    [Test({X,e3d_qbvh:ray_trace(R, QBVH)}) || {X,R} <- Rays].
    
    


   
    
    
