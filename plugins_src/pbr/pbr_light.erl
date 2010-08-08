%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(pbr_light).

-export([new/2, 
	 sample_all_lights/1,	 
	 sample_L/1, sample_L/2,
	 pdf/1, power/1
	]).

-include("pbr.hrl").

%% List of lights
%% Light is {type, type_specific_record}

-record(point, {pos, intensity}).
-record(spot,  {pos, l2wq, intensity, dir, cos_w_max, cos_fall_start}).

%%--------------------------------------------------------------------
%% @doc
%% @spec new(term) -> light()
%% @end
%%--------------------------------------------------------------------
new({point,Pos,Intensity}, WBB) ->
    #point{pos=Pos, intensity=Intensity};
new({spot,Pos,Dir0,Intensity, Width, Falloff}, _WBB) ->
    CosW = math:cos(Width*?PI/180.0),
    CosF = math:cos(Falloff*?PI/180.0),
    Dir = e3d_vec:norm(Dir0),
    #spot{pos=Pos, intensity=Intensity, dir=Dir, 
	  l2wq=e3d_q:rotate_s_to_t({0.0,0.0,1.0}, Dir),
	  cos_w_max=CosW, cos_fall_start=CosF}.


%%--------------------------------------------------------------------
%% @doc
%% @spec sample_all_lights([Lights]) -> {Light, Pdf :: float()}
%% @end
%%--------------------------------------------------------------------

sample_all_lights(Lights) ->
    N = length(Lights),
    U = sfmt:uniform(),
    Light = lists:nth(U*N+1, Lights),
    {Light, 1.0/N}.

%%--------------------------------------------------------------------
%% @doc  
%% @spec sample_L(Light, Point) -> {::spectrum(), Wi::vector(), Pdf :: float()}
%% @end
%%--------------------------------------------------------------------
sample_L(#point{pos=LPos, intensity=I}, Point) ->
    Wi = e3d_vec:norm_sub(LPos, Point),
    {e3d_vec:divide(I, e3d_vec:dist_sqr(LPos, Point)), Wi, 1.0};
sample_L(S=#spot{pos=LPos, intensity=I}, Point) ->
    Wi = e3d_vec:norm_sub(LPos, Point),
    Fallof = spot_falloff(S,Wi),
    {e3d_vec:mul(I, Fallof/e3d_vec:dist_sqr(LPos, Point)), Wi, 1.0}.

%%--------------------------------------------------------------------
%% @doc  
%% @spec sample_L(Light) -> {::spectrum(), ray(), Pdf :: float()}
%% @end
%%--------------------------------------------------------------------
sample_L(#point{pos=LPos, intensity=I}) ->
    Dir = pbr_mc:sample_sphere(sfmt:uniform(),sfmt:uniform()),
    Ray = #ray{o=LPos, d=Dir},
    {I, Ray, pbr_mc:sphere_pdf()};
sample_L(S=#spot{pos=LPos, intensity=I, l2wq=L2Wq, cos_w_max=CosW}) ->
    Vec = pbr_mc:sample_cone(sfmt:uniform(),sfmt:uniform(),CosW),
    Dir = e3d_q:vec_rotate(Vec, L2Wq),
    Ray = #ray{o=LPos, d=Dir},
    {e3d_vec:mul(I,spot_falloff(S,Dir)), Ray, pbr_mc:cone_pdf(CosW)}.


%%--------------------------------------------------------------------
%% @doc  
%% @spec power(Light) -> float()
%% @end
%%--------------------------------------------------------------------
power(#point{intensity=I}) ->
    4.0 * ?PI * I;
power(#spot{intensity=I, cos_w_max=CosW, cos_fall_start=CosF}) ->
    2.0 * ?PI * I * (1.0 - 0.5*(CosF+CosW)).

%%--------------------------------------------------------------------
%% @doc  
%% @spec pdf(Light) -> float()
%% @end
%%--------------------------------------------------------------------
pdf(_) ->
    0.0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spot_falloff(#spot{dir=Dir, cos_w_max=CosW, cos_fall_start=CosF}, Wi) ->
    CosT = abs(e3d_vec:dot(Dir,Wi))*2.0,
    if CosT < CosW -> 0.0;
       CosT > CosF -> 1.0;
       true ->
	    Delta = (CosT-CosW) / (CosF-CosW),
	    Delta*Delta*Delta*Delta
    end.

