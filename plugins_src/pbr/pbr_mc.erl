%%
%%  pbr_mc.erl
%%
%%     Pbr montecarlo sampling functions
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_mc).

-export([sample_hemisphere/2, hemisphere_pdf/0,
	 sample_sphere/2, sphere_pdf/0,
	 sample_disk/2, sample_triangle/2,
	 sample_cone/3, cone_pdf/1	 
	]).

-include("pbr.hrl").

%%% Uniform 3D sampling %%%

sample_hemisphere(U1,U2) ->
    Z = U1,
    R = math:sqrt(max(0.0, 1.0 - Z*Z)),
    Phi = 2*?PI * U2,
    {R * math:cos(Phi), R*math:sin(Phi), Z}.
hemisphere_pdf() ->
    ?INV_PI.

sample_sphere(U1, U2) ->
    Z = 1.0 - 2.0 * U1,
    R = math:sqrt(max(0.0, 1.0 - Z*Z)),
    Phi = 2.0 * ?PI * U2,
    X = R * math:cos(Phi),
    Y = R * math:sin(Phi),
    {X, Y, Z}.
sphere_pdf() ->
    (1 / 4) * ?INV_PI.

sample_cone(U1,U2,CosThetaMax) ->
    CosTheta = (1.0 - U1) + U1 * CosThetaMax,
    SinTheta = math:sqrt(1.0 - CosTheta*CosTheta),
    Phi = U2 * 2.0 * ?PI,
    {math:cos(Phi) * SinTheta, math:sin(Phi) * SinTheta, CosTheta}.
cone_pdf(CosThetaMax) ->
    1.0 / (2.0 * ?PI * (1.0 - CosThetaMax)).

%%% Uniform 2D sampling %%%

sample_disk(U1,U2) ->
    R = math:sqrt(U1),    
    Theta = 2.0 * ?PI * U2,
    X = R * math:cos(Theta),
    Y = R * math:sin(Theta),
    {X,Y}.

sample_triangle(U1,U2) ->
    Su1 = math:sqrt(U1),
    {1.0 - Su1, U2 * Su1}.
    

