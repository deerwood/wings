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

-record(s,
	{renderer,
	 max_rad2,
	 hp,
	 lup}).

init(St, Attrs) ->
    S0 = pbr_cl:init(Attrs, #renderer{}),
    S1 = pbr_camera:init(Attrs, S0),
    S2 = pbr_scene:init(St, Attrs, S1),
    spawn_link(fun() -> start(Attrs, S2) end).

start(Attrs, Renderer) ->
    Scale = proplists:get_value(photon_radius_scale, Attrs, 1.0),
    {BBmin,BBmax}  = pbr_scene:bb(Renderer),
    {Sx,Sy,Sz} = e3d_vec:sub(BBmax, BBmin),
    {W, H} = pbr_scene:get_size(Renderer),
    PhotonRadius = Scale * ((Sx+Sy+Sz) / 3.0) / ((W+H)/2) *2,
    PRad2 = PhotonRadius * PhotonRadius,
    
    S1 = init_hitpoints(#s{renderer=Renderer,     
			   hp = array:new([{default, #hp{}}]),
			   max_rad2 = PRad2}),
    loop(S1),
    
%%    HT = ?TC(ray_trace(Renderer, HitPoints0)),
    %% io:format("Done ~p rays ~p~n", [array:size(HT),
    %% 				    pbr_camera:get_size(Renderer)]),
%    Image = erlang:iolist_to_binary(array:to_list(HT)),
%    show(Image, pbr_camera:get_size(Renderer)),
    pbr_cl:stop(Renderer),
    normal.
    
loop(S0) ->
    S1 = photon_pass(S0),
    S2 = update_stats(S1),
    S3 = accum_flux(S2),
    S4 = init_hitpoints(S3),
    S5 = update_hitpoint_info(S4),
    update_film(S5),
    loop(S5).

%% Photon pass




%% Cam to hitpoints pass
init_hitpoints(State0=#s{renderer=Renderer, hp=HP0, max_rad2=Rad2}) ->
    HP1 = cam_pass(HP0, State0),
    BB  = pbr_scene:bb(Renderer),
    G0  = pbr_grid:init(HP1, Rad2, BB),
    State0#s{hp=HP1, lup=G0}.

cam_pass(Hp0, #s{renderer=Renderer}) ->
    {W,H} = pbr_camera:get_size(Renderer),
    Rays = [{X+Y*W, pbr_camera:generate_ray(Renderer, float(X), float(Y))}
	    || Y <- lists:seq(0, H-1), X <- lists:seq(0, W-1)],
    trace_rays(Rays, Hp0, Renderer).

trace_rays([], Hp, _) -> Hp;
trace_rays(Rays0, Hp0, Renderer = #renderer{scene=Scene}) ->
    {Buffer, Rays1}   = create_raybuffer(Rays0, ?MAX_RAYS, <<>>),
    {_RaysB, Hits}    = pbr_scene:intersect(Buffer, Renderer),
    {Hp, Rays} = update_hitpoints(Hits, Rays0, Rays1, Scene, Hp0),
    trace_rays(Rays, Hp, Renderer).

update_hitpoints(<<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
		 [_|Rays], Work, Scene, Hp) -> %% Miss
    update_hitpoints(Rest, Rays, Work, Scene, Hp);
update_hitpoints(<<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
		 [{Pos,_}|Rays], Work, Scene, Hp0) -> %% Hit
    Color = pbr_scene:diffuse(Face, Scene),
    %% Check if diffuse
    HitP0 = array:get(Pos, Hp0),
    HitP  = HitP0#hp{pos=Pos, color=Color},
    update_hitpoints(Rest, Rays, Work, Scene, array:set(Pos, HitP, Hp0));
update_hitpoints(<<>>, _, Work, _, Hp) ->
    {Hp, Work}.
   
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
    ShowImage = 
	fun(_) -> 
		Id = wings_image:new_temp(?__(2,"<<Render>>"), Image),
		wings_image:window(Id) 
	end,	
    wings ! {external, ShowImage},
    ok.
    
