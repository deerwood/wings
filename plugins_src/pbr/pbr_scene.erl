%%
%%  pbr_scene.erl
%%
%%     Pbr scene handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_scene).

-include("e3d.hrl").
-include("wings.hrl").

-export([init/2]).

-record(scene, 
	{data
	}).

-record(mesh, {we, fv, size}).

init(#st{shapes=Shapes} = St, Opts) ->
    Subdivs  = proplists:get_value(subdivisions, Opts, 0),
    Wes = ?TC([ prepare_mesh(We, Subdivs)
		|| We <- gb_trees:values(Shapes),
		   ?IS_VISIBLE(We#we.perm)]),
    RawData = [{array:to_list(We#we.vp), FV} || 
		  #mesh{we=We,fv=FV} <- Wes],
    ?TC(e3d_qbvh:init(RawData)),

    #scene{data=St}.

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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
