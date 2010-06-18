%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_renderer).

-include("e3d.hrl").

-export([init/1, render/2]).

-record(renderer, 
	{
	  cam
	}).

init(Attrs) ->
    Cam = init_camera(Attrs),
    #renderer{cam = Cam}.

init_camera(Attrs) ->
    cam.

render(Renderer, Scene) ->
    ok.
