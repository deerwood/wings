%%
%%  pbr.hrl
%%
%%     Data structures
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-record(renderer, 
	{
	  cl,
	  cam,
	  scene
	}).

-define(RAY_SZ, 32).		                % Binary size of ray structure
-define(RAYHIT_SZ, 16).				% Binary size of rayhit structure
-define(RAYBUFFER_SZ, 65536).			% Max Buffer size
-define(MAX_RAYS, ?RAYBUFFER_SZ div ?RAY_SZ).   % Max number of rays

-define(RAY_EPS, 0.00001).			% Error margin

-record(ray, 
	{o,					% Origo
	 d,					% Dir
	 n,					% Near = mint
	 f}).					% Far  = maxt

%-define(F32, 32/float-native).
-define(I32, 32/native).
