%%
%%  pbr_film.erl
%%
%%     Pbr film handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%
%%  An abstraction above array so I can change implementation later

-module(pbr_film).
-export([init/2, get_raw/1, set_raw/2, 
	 resolution/1,
	 splat/3, show/1]).

-include("pbr.hrl").
-include("e3d_image.hrl").

-record(f, {res, raw}).

init(_Attrs, R = #renderer{}) ->
    {W,H} = pbr_camera:get_size(R),
    Raw = array:new([{default, {0,0,0}},  {size, W*H}]),
    Film = #f{res={W,H}, raw=Raw},
    R#renderer{film=Film}.

resolution(#renderer{film=#f{res=Res}}) -> Res.

get_raw(#renderer{film=#f{raw=Raw}}) -> Raw.
set_raw(Raw, R = #renderer{film=F}) -> 
    R#renderer{film=F#f{raw=Raw}}.

splat(Index, Splat, Raw) ->
    array:set(Index, Splat, Raw).

%%% Move to film
show(#renderer{film=#f{raw=Raw, res={W,H}}}) ->
    Pixels = array:foldl(fun(_, {R,G,B}, Acc) ->
    				 <<Acc/binary, 
    				   (trunc(255*R)):8,
    				   (trunc(255*G)):8, 
    				   (trunc(255*B)):8,
				   255:8>>
    			 end, <<>>, Raw),
    Image = #e3d_image{image=Pixels,width=W,height=H, 
    		       type=r8g8b8a8, bytes_pp=4},
    ShowImage = 
    	fun(_) -> 
    		Id = wings_image:new_temp("<<Render>>", Image),
    		wings_image:window(Id) 
    	end,	
    wings ! {external, ShowImage},
    ok.
    
