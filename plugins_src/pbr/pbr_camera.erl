%%
%%  pbr_camera.erl
%%
%%     Pbr camera handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_camera).

-include("pbr.hrl").

-export([init/2,
	 get_size/1,
	 generate_ray/3]).

-record(cam, 
	{
	  w,h,		                        % Width Height
	  near,far,
	  c2w,					% Camera to world
	  r2c,					% Raster to camera
	  %%  TODO unneeded? keeping temporary for debugging.
	  s2r,					% Screen to raster
	  c2s					% Camera to screen
	}).

init(Attr, R=#renderer{}) ->
    CamProps = proplists:get_value(cam, Attr),
    {XRes, YRes} = proplists:get_value(resolution, Attr, {256,256}),
    {Pos,Dir,Up} = proplists:get_value(pos_dir_up, CamProps),
    %% io:format("Pos Dir Up ~p ~p~n", [(Pos), (Dir)]),
    World2Cam = e3d_transform:lookat(Pos, e3d_vec:add(Pos,Dir), Up),

    AspectRatio = XRes/YRes,

    Near = proplists:get_value(hither, CamProps),
    Far  = proplists:get_value(yon,  CamProps),
    Camera2Screen = 
	case proplists:get_value(camera_type, Attr, perspective) of
	    perspective ->
		Fov  = proplists:get_value(fov, CamProps),
		e3d_transform:perspective(Fov, Near, Far);
	    ortho ->
		e3d_transform:ortho(Near, Far)
	end,
    Screen2Camera = e3d_transform:inverse(Camera2Screen),
    
    {Xmin,Xmax,Ymin,Ymax} = 
	case AspectRatio >= 1.0 of
	    true  -> {-AspectRatio,AspectRatio,-1.0,1.0};
	    false -> {-1.0,1.0,-1.0/AspectRatio,1.0/AspectRatio}
	end,
    Center = e3d_transform:translate(e3d_transform:identity(),
    				     {-Xmin, Ymax, 0.0}),
    AspectScale = e3d_transform:scale(Center, 
    				      {1.0/(Xmax-Xmin),1.0/(Ymax-Ymin),1.0}),
    Screen2Raster = e3d_transform:scale(AspectScale, {XRes,YRes, 1.0}),
    %% Screen = e3d_transform:scale(e3d_transform:identity(),{XRes,YRes, 1.0}),
    %% Aspect = e3d_transform:scale(Screen, {1.0/(Xmax-Xmin),1.0/(Ymax-Ymin),1.0}),
    %% Screen2Raster = e3d_transform:translate(Aspect, {-Xmin, Ymax, 0.0}),

    Raster2Screen = e3d_transform:inverse(Screen2Raster),
    
    Raster2Camera = e3d_transform:mul(Raster2Screen, Screen2Camera),
    
    Cam = #cam{w=XRes, h=YRes,
	       near=Near, far=Far,
	       c2w = e3d_transform:inverse(World2Cam),
	       r2c = Raster2Camera,
	       s2r = Screen2Raster,
	       c2s = Camera2Screen
	      },
    R#renderer{cam=Cam}.

get_size(#renderer{cam=#cam{w=W, h=H}}) ->
    {W,H}.

generate_ray(#renderer{cam=Cam},X,Y) when is_float(X), is_float(Y) ->
    #cam{r2c=R2C, c2w=C2W, near=Near, far=Far} = Cam,
    Origo = e3d_mat:mul_point(e3d_transform:matrix(R2C), {X,Y,0.0}),
    Vec = case false of
	      true -> 
		  %% Adjust for camera lens
		  ok;
	      false ->
		  Origo
	  end,
    C2Wm = e3d_transform:matrix(C2W),
    #ray{o=e3d_mat:mul_point(C2Wm,Origo),
	 d=e3d_mat:mul_vector(C2Wm,e3d_vec:norm(Vec)),
	 n=?RAY_EPS,
	 f=Far-Near}.

