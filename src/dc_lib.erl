-module(dc_lib).

-include("dc_internal.hrl").
-include("data_col.hrl").

-export([read_cfg/0,get_cfg/1]).


read_cfg() ->
    P1=get_cfg(dc_devices,[]),
    P2=get_cfg(dc_inputs,[]),
    P3=get_cfg(dc_datasources,[]),
    P5=[], % dc_lib:get_cfg(dc_graphs,[]),
    P6=get_cfg(dc_timeseries,[]),
    Cfg0=#dc_cfg_data{devices=[],time_series=[],input=[],datasources=[],
		      graphs=[]},
    dc_db:process_config([P1,P2,P3,P5,P6],Cfg0).


get_cfg(Cfg) ->
    emd_cfg:get_cfg_a(?APP_NAME,Cfg).

get_cfg(Cfg,Def) ->
    emd_cfg:get_cfg_a(?APP_NAME,Cfg,Def).



