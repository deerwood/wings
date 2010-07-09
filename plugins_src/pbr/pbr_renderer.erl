%%
%%  pbr_renderer.erl
%%
%%     Pbr renderer handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_renderer).

%%-include("e3d.hrl").
-include("pbr.hrl").
-include("wings.hrl").
-include("e3d_image.hrl").

-export([init/2]).

%% -record(hp, 
%% 	{photons 

init(St, Attrs) ->
    S0 = pbr_cl:init(#renderer{}),
    S1 = pbr_scene:init(St, Attrs, S0),
    S2 = pbr_camera:init(Attrs, S1),
    start(S2),
    pbr_cl:stop(S2).

start(Renderer) ->
    HitPoints0 = array:new(),
    HT = ?TC(ray_trace(Renderer, HitPoints0)),
    io:format("Done ~p rays ~p~n", [array:size(HT),
				    pbr_camera:get_size(Renderer)]),
    Image = erlang:iolist_to_binary(array:to_list(HT)),
    show(Image, pbr_camera:get_size(Renderer)),
    ok.

ray_trace(Renderer, Hitpoints0) ->
    {W,H} = pbr_camera:get_size(Renderer),
    Rays = [{X+Y*W, pbr_camera:generate_ray(Renderer, float(X), float(Y))}
	    || Y <- lists:seq(0, H-1), X <- lists:seq(0, W-1)],
    trace_rays(Rays, Hitpoints0, Renderer).

trace_rays([], Hitpoints, _) -> Hitpoints;
trace_rays(Rays0, Hitpoints0, Renderer) ->
    {Buffer, Rays1}   = create_raybuffer(Rays0, ?MAX_RAYS, <<>>),
    {_RaysB, Hits}    = pbr_scene:intersect(Buffer, Renderer),
    {Hitpoints, Rays} = update_hitpoints(Hits, Rays0, Rays1, Hitpoints0),
    trace_rays(Rays, Hitpoints, Renderer).

update_hitpoints(<<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
		 [{Pos,_}|Rays], Work, Hitpoints) -> %% Miss
    update_hitpoints(Rest, Rays, Work, array:set(Pos, <<255:32>>, Hitpoints));
update_hitpoints(<<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
		 [{Pos,_}|Rays], Work, Hitpoints) -> %% Hit
    update_hitpoints(Rest, Rays, Work, array:set(Pos, <<16#FFFFFFFF:32>>, Hitpoints));
update_hitpoints(<<>>, _, Work, Hitpoints) ->
    {Hitpoints, Work}.
   
create_raybuffer([{_,#ray{o={OX,OY,OZ},d={DX,DY,DZ},n=N,f=F}}|Rest], No, Buff0) 
  when No > 0 ->
    Buff = <<Buff0/binary,  
	     OX:?F32, OY:?F32, OZ:?F32, 
	     DX:?F32, DY:?F32, DZ:?F32, 
	     N:?F32,  F:?F32>>,
    create_raybuffer(Rest, No-1, Buff);
create_raybuffer(Rays, _, Buff) ->
    {{byte_size(Buff) div ?RAY_SZ, Buff}, Rays}.

%%% Move to film

show(Pixels, {W,H}) ->
    Image = #e3d_image{image=Pixels,width=W,height=H, 
		       type=r8g8b8a8, bytes_pp=4},
    Id = wings_image:new_temp(?__(2,"<<Render>>"), Image),
    wings_image:window(Id).
