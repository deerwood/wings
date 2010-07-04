%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_renderer).

-include("e3d.hrl").

-export([init/1, start/2]).

-record(renderer, 
	{
	  cam
	}).

init(Attrs) ->
    Cam = pbr_camera:init(Attrs),
    #renderer{cam = Cam}.

start(Renderer, Scene) ->
    ok.
