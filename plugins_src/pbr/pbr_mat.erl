%%
%%  pbr_mat.erl
%%
%%     Pbr material handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_mat).

-export([sdiv/2, smul/2, sadd/2, snew/0, sample_f/4, lookup/3, is_light/1]).
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

%% Material functions
sample_f(Material, RayD, N, ShadeN) ->
    %% Fixme
    Spectrum = {0.8,0.8,0.8},
    Wi = e3d_vec:cross(e3d_vec:neg(RayD), ShadeN),
    Pdf = 0.8,
    SpecBounce = false,
    {Spectrum, Wi, Pdf, SpecBounce}.

is_light(Material) ->
    false.

%% Texture functions
lookup(Mat, UV, Type) ->
    false.

