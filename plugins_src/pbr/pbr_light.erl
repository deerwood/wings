%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2010, Dan Gudmundsson
%%% @doc
%%%
%%% @end
%%% Created : 13 Jul 2010 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------
-module(pbr_light).

-export([create_lookup/1, lookup_id/2, init/2,
	 new/2, get_light/2, get_infinite_light/1,
	 sample_all_lights/1,
	 sample_L/1, sample_L/2,
	 pdf/1, le/2, power/1
	]).

-include("pbr.hrl").

%% List of lights
%% Light is {type, type_specific_record}

-record(point, {pos, intensity}).
-record(spot,  {pos, l2wq, intensity, dir, cos_w_max, cos_fall_start}).

%%--------------------------------------------------------------------
%% @doc
%% @spec create_lookup([]) -> {array(), proplist()}
%% @end
%%--------------------------------------------------------------------

create_lookup(Ls) ->
    La = array:from_list(Ls),
    Lookup = array:foldr(fun(Id, {Light,_}, Acc) ->
				 [{Light, Id}|Acc]
			 end, [], La),
    {La, gb_trees:from_orddict(Lookup)}.

lookup_id(Name, {_, Lookup}) ->
    gb_trees:get(Name, Lookup).

%% Input is returned from create_lookup/1
init({La, _}, WBB) ->
    array:map(fun(_, {_Name, LO}) -> 
		      L = proplists:get_value(opengl,LO),
		      init_light(proplists:get_value(type, L), L,WBB) 
	      end, La).

init_light(point, L, WBB) ->
    Pos = proplists:get_value(position, L),
    {IR,IG,IB,_} = proplists:get_value(diffuse, L),
    io:format("~p ~n",[L]),
    I = 1.0,
    new({point, Pos, {IR*I,IG*I,IB*I}}, WBB).

get_light(Id, Ls) ->
    array:get(Id, Ls).

get_infinite_light(Ls) ->
    false.

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
    N = array:size(Lights),
    U = sfmt:uniform(),
    Light = array:get(trunc(U*N), Lights),
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

%%--------------------------------------------------------------------
%% @doc  
%% @spec le(Light) -> spectrum().
%% @end
%%--------------------------------------------------------------------
le(#point{intensity=I}, _Ray) ->
    I;
le(_Light, _Ray) ->
    {0.0,0.0,0.0}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
spot_falloff(#spot{dir=Dir, cos_w_max=CosW, cos_fall_start=CosF}, Wi) ->
    CosT = abs(e3d_vec:dot(Dir,Wi))*2.0,
    if CosT < CosW -> 0.0;
       CosT > CosF -> 1.0;
       true ->
	    Delta = (CosT-CosW) / (CosF-CosW),
	    Delta*Delta*Delta*Delta
    end.

