%%
%%  pbr_scene.erl
%%
%%     Pbr scene handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_scene).

-include("e3d.hrl").

-export([init/1]).

-record(scene, 
	{data
	}).

init(Data) ->
    #scene{data=Data}.


