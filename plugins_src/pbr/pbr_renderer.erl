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

%TODO
{{pbr_hp,add_flux,5},{pbr_mat,f,4}},
{{pbr_hp,splat_radiance,3},{pbr_film,splat,3}},
{{pbr_renderer,update_hitpoints,5},{pbr_light,get_light,2}},
{{pbr_renderer,update_hitpoints,5},{pbr_light,le,2}},
{{pbr_renderer,update_hitpoints,5},{pbr_mat,is_diffuse,1}},
{{pbr_renderer,update_hitpoints,5},{pbr_scene,get_infinite_light,1}},

-record(s,
	{renderer,				% Render state
	 %% Photon info
	 max_rad2,				% Max Photon Radius
	 ps=0,   				% Current Photons
	 %% Stats
	 no_p = 0,			        % Total number photons
	 pass = 0,				% Number of photon passes
	 %% Hitpoint info
	 hp,					% Hitpoints
	 lup}).					% Hitpoint lookup

init(St, Attrs) ->
    S0 = pbr_cl:init(Attrs, #renderer{}),
    S1 = pbr_camera:init(Attrs, S0),
    S2 = pbr_scene:init(St, Attrs, S1),
    spawn_link(fun() -> start(Attrs, S2) end).

start(Attrs, Renderer) ->
    Scale = proplists:get_value(photon_radius_scale, Attrs, 1.0),
    {BBmin,BBmax}  = pbr_scene:bb(Renderer),
    {Sx,Sy,Sz} = e3d_vec:sub(BBmax, BBmin),
    {W, H} = pbr_camera:get_size(Renderer),
    PhotonRadius = Scale * ((Sx+Sy+Sz) / 3.0) / ((W+H)/2) *2,
    PRad2 = PhotonRadius * PhotonRadius,
    
    S1 = init_hitpoints(#s{renderer=Renderer,     
			   hp = pbr_hp:new(W*H, PRad2),
			   max_rad2 = PRad2}),
    S2 = init_photon_pass(S1),
    loop(S2),
    
%%    HT = ?TC(ray_trace(Renderer, HitPoints0)),
    %% io:format("Done ~p rays ~p~n", [array:size(HT),
    %% 				    pbr_camera:get_size(Renderer)]),
%    Image = erlang:iolist_to_binary(array:to_list(HT)),
%    show(Image, pbr_camera:get_size(Renderer)),
    pbr_cl:stop(Renderer),
    normal.
    
loop(S0) ->
    S1 = photon_passes(S0),
    S2 = accum_flux(S1),
    S3 = eval_hitpoints(S2),
    S4 = init_hitpoints(S3),
    loop(S4).

accum_flux(S = #s{hp=Hp}) ->
    S#s{hp=pbr_hp:accum_flux(Hp)}.

eval_hitpoints(S = #s{renderer=R0, hp=Hp, no_p=TotalPhotons}) ->
    {R, MaxRadius} = pbr_hp:splat_radiance(TotalPhotons, R0, Hp),
    S#s{renderer=R, max_rad2=MaxRadius}.
     
%% Photon pass
photon_passes(S0) ->
    photon_passes(0, S0).
photon_passes(N, S = #s{renderer=R, ps=Ps0, lup=Lup, hp=Hp0}) 
  when N < ?PHOTONS_PER_PASS ->
    {Buffer, []} = create_raybuffer(Ps0, ?MAX_RAYS, <<>>),
    {_, Hits} = pbr_scene:intersect(Buffer, R),
    {Count, Ps, Hp} = photon_pass(Ps0, Hits, R, Lup, Hp0, 0, []),
    photon_passes(Count+N, S#s{ps=Ps, hp=Hp});
photon_passes(Count, S=#s{no_p=Ps}) ->
    S#s{no_p=Ps+Count}.

photon_pass([_|Ps], <<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
	    R, Lup, Hp, New, Acc) -> %% Miss; cast a new photon
    PPath = init_photon_path(R),
    photon_pass(Ps, Rest, R, Lup, Hp, New+1, [PPath|Acc]);
photon_pass([{Ray=#ray{d=RayD},Flux,Depth}|Ps],
	    <<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
	    R, Lup, Hp0, New, Acc) -> %% Hit something
    case pbr_scene:get_face_info(Ray,T,B1,B2,Face,R) of
	{transparent, Pos} ->
	    photon_pass(Ps,Rest,R,Lup,Hp0,New,[{Ray#ray{o=Pos},Flux,Depth}|Acc]);
	{light, _Light} ->
	    PPath = init_photon_path(R),
	    photon_pass(Ps,Rest,R,Lup,Hp0,New+1,[PPath|Acc]);
	{Point, Mat, SurfaceCol, N, ShadeN} ->
	    {F0, Wi, Fpdf, SpecBounce} = pbr_mat:sample_f(Mat, RayD, N, ShadeN),
	    Hp = if SpecBounce == false ->
			 add_flux(Point, RayD, Flux, Lup, Hp0);
		    true ->
			 Hp0
		 end,
	    case Depth < ?MAX_PHOTON_DEPTH of
		false ->
		    PPath = init_photon_path(R),
		    photon_pass(Ps, Rest, R, Lup, Hp, New+1, [PPath|Acc]);
		true ->
		    F  = pbr_mat:smul(F0, SurfaceCol),
		    Rand = sftm:uniform(),
		    if Depth < 1 orelse SpecBounce ->
			    PFlux = pbr_mat:smul(Flux, pbr_mat:smul(F,Fpdf)),
			    PPath = {{PFlux, Depth+1}, #ray{o=Point,d=Wi}},
			    photon_pass(Ps,Rest,R,Lup,Hp,New,[PPath|Acc]);
		       Rand < 0.7 -> %% Russian Roulette
			    PFlux = pbr_mat:smul(Flux, pbr_mat:smul(F,Fpdf/0.7)),
			    PPath = {{PFlux, Depth+1}, #ray{o=Point,d=Wi}},
			    photon_pass(Ps,Rest,R,Lup,Hp,New,[PPath|Acc]);
		       true ->
			    PPath = init_photon_path(R),
			    photon_pass(Ps,Rest,R,Lup,Hp,New+1,[PPath|Acc])
		    end
	    end
    end;
photon_pass([], <<>>, _R, _Lup, Hp, New, Acc) -> 
    {New, Acc, Hp}.

add_flux(Point, RayD, Flux, Lup, Hp) ->
    HPs = pbr_grid:nearest(Point, Lup),
    pbr_hp:add_flux(HPs, Point, e3d_vec:neg(RayD), Flux, Hp).
   
%% init_photon_pass
init_photon_pass(S = #s{renderer=R}) ->
    Photons = init_photon_pass(0, R, []),
    S#s{ps=Photons, no_p=?MAX_RAYS}.

init_photon_pass(N, R, Acc) when N < ?MAX_RAYS ->
    Photon = init_photon_path(R),
    init_photon_pass(N+1, R, [Photon|Acc]);
init_photon_pass(_, _, Acc) -> Acc.

init_photon_path(Renderer) ->
    Ls = pbr_scene:get_lights(Renderer),
    {Light, Lpdf} = pbr_light:sample_all_lights(Ls),
    {Flux0, Ray, Pdf} = pbr_light:sample_L(Light),
    Flux = pbr_mat:sdiv(Flux0,(Lpdf*Pdf)),
    {Ray, Flux, 0}.

%% Cam to hitpoint pass
init_hitpoints(State0=#s{renderer=Renderer, hp=HP0, max_rad2=Rad2}) ->
    HP1 = cam_pass(HP0, State0),
    BB  = pbr_scene:bb(Renderer),
    G0  = pbr_grid:init(HP1, Rad2, BB),
    State0#s{hp=HP1, lup=G0}.

cam_pass(Hp0, #s{renderer=Renderer}) ->
    {W,H} = pbr_camera:get_size(Renderer),
    Spectrum = {1.0,1.0,1.0},
    Rays = [{pbr_camera:generate_ray(Renderer, float(X), float(Y)),
	     X+Y*W,0,Spectrum}
	    || Y <- lists:seq(0, H-1), X <- lists:seq(0, W-1)],
    trace_rays(Rays, Hp0, Renderer).

trace_rays([], Hp, _) -> Hp;
trace_rays(Rays0, Hp0, Renderer) ->
    {Buffer, Rays1}   = create_raybuffer(Rays0, ?MAX_RAYS, <<>>),
    {_RaysB, Hits}    = pbr_scene:intersect(Buffer, Renderer),
    {Hp, Rays} = update_hitpoints(Hits, Rays0, Rays1, Renderer, Hp0),
    trace_rays(Rays, Hp, Renderer).

update_hitpoints(<<_:12/binary, 16#FFFFFFFF:32, Rest/binary >>, 
		 [{Ray,Pos,_,TP}|Rays], Work, R, Hp0) -> 
    %% Miss, check background
    Troughput = 
	case pbr_scene:get_infinite_light(R) of
	    false -> pbr_mat:snew();
	    Light -> 
		pbr_mat:smul(pbr_light:le(Light, Ray),TP)
	end,
    Hp = pbr_hp:update_const(Pos, background, Troughput, Hp0),
    update_hitpoints(Rest, Rays, Work, R, Hp);
update_hitpoints(<<T:?F32,B1:?F32,B2:?F32,Face:?I32, Rest/binary>>,
		 [{Ray, Pos, Depth, TP}|Rays], Work, R, Hp0) -> 
    %% Hit
    case pbr_scene:get_face_info(Ray,T,B1,B2,Face,R) of
	{transparent, Point} ->
	    update_hitpoints(Rest,Rays, [{Ray#ray{o=Point},Pos,Depth,TP}|Work], R, Hp0);
	{light, LightId} ->
	    Light = pbr_light:get_light(LightId,R),
	    TPd = pbr_mat:smul(pbr_light:le(Light, Ray), TP),
	    Hp  = pbr_hp:update_const(Pos, light, TPd, Hp0),
	    update_hitpoints(Rest, Rays, Work, R, Hp);
	{Point, Mat, SurfaceCol, N, ShadeN} ->
	    {F0, Wi, Fpdf, SpecBounce} = pbr_mat:sample_f(Mat, Ray, N, ShadeN),
	    IsNotDiffuse = not pbr_mat:is_diffuse(Mat),
	    if 
		Fpdf =:= 0.0, F0 =:= {0.0,0.0,0.0} -> %% Black
		    Hp = pbr_hp:update_const(Pos, surface, pbr_mat:snew(), Hp0),
		    update_hitpoints(Rest, Rays, Work, R, Hp);
		SpecBounce, IsNotDiffuse ->
		    case Depth > ?MAX_EYE_DEPTH of
			false ->
			    TPd = pbr_mat:smul(TP, pbr_mat:sdiv(F0, Fpdf)),
			    Bounce = {#ray{o=Point, d=Wi}, Pos, Depth+1, TPd},
			    update_hitpoints(Rest, Rays, [Bounce|Work], R, Hp0);
			true ->
			    Hp = pbr_hp:update_const(Pos, surface, pbr_mat:snew(), Hp0),
			    update_hitpoints(Rest, Rays, Work, R, Hp)
		    end;
		true ->
		    W0 = e3d_vec:neg(Ray#ray.d),
		    Color = pbr_mat:smul(SurfaceCol, TP),
		    Hp = pbr_hp:update_hit(Pos, Point, Mat, Color, W0, ShadeN, Hp0),
		    update_hitpoints(Rest, Rays, Work, R, Hp)
	    end
    end;
update_hitpoints(<<>>, _, Work, _, Hp) ->
    {Hp, Work}.
   
create_raybuffer([RayInfo|Rest], No, Buff0) 
  when No > 0 ->
    #ray{o={OX,OY,OZ},d={DX,DY,DZ},n=N,f=F} = element(1, RayInfo),
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
    
