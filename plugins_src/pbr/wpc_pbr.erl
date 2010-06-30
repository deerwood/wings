%%
%%  wpc_pbr.erl
%%
%%     Renderer for wings
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%


-module(wpc_pbr).

-include("wings.hrl").
-include("e3d.hrl").
-include("e3d_image.hrl").

-export([init/0,menu/2,command/2]).

-define(TAG, wings_pbr).

%% Initialize and check for OpenCL support
init() ->
    case cl:start() of
	ok ->    
	    true;
	{error, {already_started,cl}} ->
	    true;
	{error, _} ->
	    io:format(?__(1, "Warning: OpenCL Support not available rendering disabled~n"),[]),
	    false
    end.

%% Attach to menu 
menu({file,render}, Menu) ->
    Menu++[{"Wings PBR",?TAG,[option]}];
menu(_, Menu) ->
    Menu.

command({file,{render,{?TAG,A}}}, St) ->
    do_export(A, St);
command(_Spec, _St) ->
    next.

%% Do the export
do_export(Ask, _St) when is_atom(Ask) ->
    wpa:dialog(Ask, "Wings Render Options", export_dialog(),
	       fun(Res) ->
		       {file, {render, {?TAG, Res}}}
	       end);
do_export(Attr, St) when is_list(Attr) ->
    set_prefs(Attr),
    %% Basic additional settings
    SubDivs = proplists:get_value(subdivisions, Attr, 0),
    %% Add Camera, lights to list
    CameraInfo = 
	%% [{Pos, Dir, Up}, Fov, Azimuth, Elevation, Track, Dist] = 
	wpa:camera_info([pos_dir_up, fov, azimuth, elevation, 
			 tracking, distance_to_aim]),
    All = [{cam,CameraInfo}, {lights, wpa:lights(St)},
	   {subdivisions,SubDivs} | Attr],
    render(St, All).

export_dialog() ->
    wpa:pref_set_default(?MODULE, default_filetype, ".png"),
    [wpa:dialog_template(?MODULE, export)].

render(St, Opts) ->
    Scene = pbr_scene:init(St, Opts),
    Renderer = pbr_renderer:init(Opts),
    pbr_renderer:start(Renderer, Scene),
    keep.

%% Helpers

set_prefs(Attr) ->
    wpa:scene_pref_set(?MODULE, Attr).

%% get_pref(Key, Def) ->
%%     [{Key,Val}] = get_prefs([{Key,Def}]),
%%     Val.

%% get_prefs(KeyDefs) when is_list(KeyDefs) ->
%%     get_prefs_1(KeyDefs, make_ref()).

%% get_prefs_1([], _Undefined) ->
%%     [];
%% get_prefs_1([{Key,Def}|KeyDefs], Undefined) ->
%%     [{Key,case wpa:scene_pref_get(?MODULE, Key, Undefined) of
%% 	      Undefined ->
%% 		  wpa:pref_get(?MODULE, Key, Def);
%% 	      Val ->
%% 		  Val
%% 	  end}|get_prefs_1(KeyDefs, Undefined)].
