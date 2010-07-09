%%
%%  pbr_cl.erl
%%
%%     Pbr OpenCL handling
%%
%%  Copyright (c) 2010 Dan Gudmundsson
%%

-module(pbr_cl).

-include("e3d.hrl").
-include("pbr.hrl").
-include_lib("cl/include/cl.hrl").

-export([init/1, stop/1,
	 get_context/1, get_device/1,
	 compile/2, run/6]).

-record(cli, 					% Cache some info
	{cl,					% CL context
	 image,					% Image suppport
	 wg,					% max_work_group_size
	 queue					% Queue 
	}).

init(R=#renderer{}) ->
    CL = clu:setup(cpu),
    [Device] = CL#cl.devices,
    {ok,DeviceInfo} = cl:get_device_info(Device),
    ImageSupport = proplists:get_value(image_support, DeviceInfo, false),
    MaxWorkGroupSize = proplists:get_value(max_work_group_size, DeviceInfo, 4),
    {ok,Queue} = cl:create_queue(CL#cl.context,Device,[]),
    R#renderer{cl=#cli{cl=CL, image=ImageSupport, wg=MaxWorkGroupSize, queue=Queue}}.

compile(#cli{cl=CL}, File) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Bin} = file:read_file(filename:join([Dir,"kernels", File])),
    {ok, Program} = clu:build_source(CL, Bin),
    {ok, [Kernel]} = cl:create_kernels_in_program(Program),
    Kernel.

get_context(#cli{cl=#cl{context=Context}}) -> 
    Context.
get_device(#cli{cl=#cl{devices=[Device|_]}}) -> 
    Device.

run(#cli{queue=Q}, Global, Local, Kernel, Args, {Out, OutSz}) ->
    clu:apply_kernel_args(Kernel, Args),
    {ok, Run} = cl:enqueue_nd_range_kernel(Q, Kernel,[Global],[Local],[]),
    {ok, Done} = cl:enqueue_read_buffer(Q, Out,0, OutSz, [Run]),
    Done.

stop(#renderer{cl=#cli{cl=CL}}) ->
    clu:teardown(CL).
