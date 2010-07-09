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

-record(scene, 
	{data,
	 isect
	}).

-record(mesh, {we, fv, size}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(#st{shapes=Shapes} = St, Opts, R = #renderer{cl=CL}) ->
    Subdivs  = proplists:get_value(subdivisions, Opts, 0),
    Wes = ?TC([ prepare_mesh(We, Subdivs)
		|| We <- gb_trees:values(Shapes),
		   ?IS_VISIBLE(We#we.perm)]),
    RawData = [{array:to_list(We#we.vp), FV} || 
		  #mesh{we=We,fv=FV} <- Wes],
    AccelBin = ?TC(e3d_qbvh:init(RawData)),
    Scene = init_accel(CL, AccelBin, #scene{data=St}),
    R#renderer{scene=Scene}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prepare_mesh(We0, SubDiv) ->
    We = if SubDiv =:= 0 -> 
		 We1 = wpa:triangulate(We0),
		 wpa:vm_freeze(We1);
	    true -> 
		 We1 = wpa:vm_freeze(We0),
		 We2 = sub_divide(SubDiv, We1),
		 wpa:triangulate(We2)
	 end,
    Faces = wings_we:visible(We),
    FV = [wings_face:vertices_ccw(Face, We) || Face <- Faces],
    #mesh{we=We, fv=FV, size=length(FV)}.

sub_divide(0, We) -> We;
sub_divide(N, We) -> 
    sub_divide(N-1, wings_subdiv:smooth(We)).

init_accel(CL, {Qnodes, Qtris, _}, Scene) ->
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
