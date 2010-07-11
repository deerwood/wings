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

-record(mesh, {we, fv, size, mats}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(#st{shapes=Shapes, mat=Mats}, Opts, R = #renderer{cl=CL}) ->
    Subdivs  = proplists:get_value(subdivisions, Opts, 0),
    Wes = ?TC([ prepare_mesh(We, Subdivs, Mats)
		|| We <- gb_trees:values(Shapes),
		   ?IS_VISIBLE(We#we.perm)]),
    RawData = [{Sz, fun(Face) -> 
			    [V1,V2,V3] = array:get(Face, FV),
			    {array:get(V1,We#we.vp),
			     array:get(V2,We#we.vp),
			     array:get(V3,We#we.vp)}
		    end} 
	       || #mesh{we=We,fv=FV,size=Sz} <- Wes],
    F2M = array:from_list(lists:append([Mesh#mesh.mats || Mesh <- Wes])),
    GetMat = fun(Face) -> array:get(Face, F2M) end,
    AccelBin = ?TC(e3d_qbvh:init(RawData)),
    %% test(AccelBin, GetMat, R),
    Scene = init_accel(CL, AccelBin, #scene{data=Wes, get_mat=GetMat}),
    R#renderer{scene=Scene}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_mesh(We0, SubDiv, Mtab) ->
    %% test:debug(array:to_orddict(We0#we.vp), R#renderer.cam),
    We = if SubDiv =:= 0 -> 
		 We1 = wpa:triangulate(We0),
		 wpa:vm_freeze(We1);
	    true -> 
		 We1 = wpa:vm_freeze(We0),
		 We2 = sub_divide(SubDiv, We1),
		 wpa:triangulate(We2)
	 end,
    Faces = wings_we:visible(We),
    FV = [wings_face:vertices_cw(Face, We) || Face <- Faces],
    Mats = [get_color(wings_facemat:face(Face, We),Mtab) || Face <- Faces],
    #mesh{we=We, fv=array:from_list(FV), mats=Mats, size=length(Mats)}.

sub_divide(0, We) -> We;
sub_divide(N, We) -> 
    sub_divide(N-1, wings_subdiv:smooth(We)).

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
    
    


   
    
    
