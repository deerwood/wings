%%
%%  pbr_mat.erl
%%
%%     Pbr material handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_mat).

-export([snew/0, sdiv/2, smul/2, sadd/2, sY/1,
	 init/1,
	 sample_f/4, f/4, lookup/3, is_light/1, is_diffuse/1]).
-include("pbr.hrl").

%% Spectrum functions
snew() ->
    {0.0,0.0,0.0}.

sdiv({R,G,B}, S) 
  when is_float(R), is_float(G), is_float(B), is_float(S) ->
    {R/S,G/S,B/S}.
		     
smul({R,G,B}, S) 
  when is_float(R), is_float(G), is_float(B), is_float(S) ->
    {R*S,G*S,B*S};

smul({R,G,B}, {X,Y,Z}) 
  when is_float(R), is_float(G), is_float(B), 
       is_float(X), is_float(Y), is_float(Z) ->
    {R*X,G*Y,B*Z}.

sadd({R,G,B}, {X,Y,Z}) 
  when is_float(R), is_float(G), is_float(B), 
       is_float(X), is_float(Y), is_float(Z) ->
    {R+X,G+Y,B+Z}.

sY({R,G,B}) ->
    0.212671 * R + 0.715160 * G + 0.072169 * B.

%%%%%%%%%%%

-record(material, 
	{mod = ?MODULE,				% Module info
	 m_info,				% Term module specific
	 diff_map,				% Color map
	 norm_map	 			% Normal map
	}).

-record(m_info, {kd, kdOverPi}).
    
%% Material functions
init(Mtab) ->
    Converted = [{Id, create_mat(WMat)} || 
		    {Id,WMat} <- gb_trees:to_list(Mtab)],
    gb_trees:from_orddict(Converted).

create_mat(WM) ->
    OpenGL = proplists:get_value(opengl, WM),
    {R,G,B,_}   = proplists:get_value(diffuse, OpenGL),
    Maps   = proplists:get_value(maps, WM),
    #material{m_info=#m_info{kd={R,G,B}, kdOverPi=smul({R,G,B}, ?INV_PI)}}.
    
sample_f(#material{mod=?MODULE, m_info=#m_info{kdOverPi=KdOPi}}, 
	 _RayD, _N, ShadeN = {NX,NY,NZ}) ->
    {X,Y,Z} = pbr_mc:sample_hemisphere(sfmt:uniform(), sfmt:uniform()),
    Pdf = Z * ?INV_PI,
    
    {{V1X,V1Y,V1Z}, {V2X,V2Y,V2Z}} = pbr_scene:coord_sys(ShadeN),
    Wi = {V1X * X + V2X * Y + NX * Z,
	  V1Y * X + V2Y * Y + NY * Z,
	  V1Z * X + V2Z * Y + NZ * Z},

    Dp = e3d_vec:dot(ShadeN, Wi),
    SpecBounce = false,
    case Dp =< 0.0001 of
	true -> 
	    {snew(), Wi, 0.0, SpecBounce};
	false ->	    
	    {KdOPi, Wi, Pdf / Dp, SpecBounce}
    end.

f(#material{mod=?MODULE, m_info=#m_info{kdOverPi=KdOPi}}, _W0,_Wi, _N) ->
    KdOPi.

is_diffuse(#material{mod=?MODULE}) ->
    true.

is_light(Material) ->
    is_integer(Material).

%% Texture functions
lookup(Mat, UV, Type) ->
    false.

