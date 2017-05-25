%%% @author Johan Blom <>
%%% @copyright (C) 2013, Johan Blom
%%% @doc
%%%
%%% @end
%%% Created : 12 Sep 2013 by Johan Blom <>

-module(dc_manager).
-behaviour(gen_server).

-include("data_col.hrl").
-include("dc_internal.hrl").

%% Gen server exports
-export([start_link/0,
	 init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3,

	 update/2,
	 last/1,

	 %% Management 
	 info/0,
	 load_cfg/0]).

-record(state,{
%	  cfg_db, % All data_col configuration data
	  ds_db   % Each configured data source 
	 }).


-record(ds_inst,{
	  id,   % #dc_ds.id
	  title,% #dc_ds.title
	  pid   % Pid to handler process
	 }).

%% Public API
update(Id,Data) ->
    gen_server:cast(?MODULE,{update,Id,Data}).

last(Id) ->
    gen_server:call(?MODULE,{last,Id}).

%% @spec load_cfg() -> ok
%% @doc
%%  Load new Erly Marsh configuration
%% @end
load_cfg() ->
    gen_server:call(?MODULE,load_cfg).

info() ->
    gen_server:call(?MODULE,info).


%% @spec start_link() -> Result
%%   Result = {ok,Pid} | ignore | {error,Error}
%%     Pid = pid()
%%     Error = {already_started,Pid} | shutdown | term()
%% @doc cClls gen_server:start_link
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE,[], []).



%% Gen server interface

%% @hidden
init([]) -> 
%    Cfg=validate_cfg(undefined),
    DSdb=ets:new(ds_db,[{keypos, #ds_inst.id}]),
    load_cfg(DSdb),
    {ok, #state{ds_db=DSdb}}.

%% terminate
%% @hidden
terminate(_Reason, _State) -> ok.


%%
%% handle_call
%% @hidden
handle_call({last,DSid}, _From,State=#state{ds_db=DSdb}) ->
    Resp=case lookup_handler(DSid,DSdb) of
	     Pid when is_pid(Pid) -> dc_handler:last(Pid);
	     Error -> Error
	 end,
    {reply,Resp, State};
handle_call(info, _From,State=#state{ds_db=DSdb}) ->
    pp_dsdb(ets:tab2list(DSdb)),
    {reply,ok, State};
handle_call(load_cfg, _From,State=#state{ds_db=DSdb}) ->
    %% FIXME! This just reloads the configuration, but we should also:
    %% - Create delta between old and new and remove configuration not in new
    %% - There may be active jobs , what to do with these?
    %% - Handle HTTP and SMTP configuration changes also
    %% - Sync login status for bugtrackers
    load_cfg(DSdb),
    {reply,ok, State}.


%% handle_cast
%% @hidden
handle_cast({update,DSid,Data}, State=#state{ds_db=DSdb}) -> 
    io:format("~p UPDATE data source ~p data ~p~n",[?MODULE,DSid,Data]),
    Resp=case lookup_handler(DSid,DSdb) of
	     Pid when is_pid(Pid) -> dc_handler:update(Pid,Data);
	     Error -> Error
	 end,
    {noreply, State}.

%% handle_info
%% @hidden
handle_info(_Msg, State) -> 
    {noreply, State}.


%% @hidden
code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.


%%% ----------------------------------------------------------------------------
load_cfg(DSdb) ->
    NewCfg=dc_lib:read_cfg(),
    io:format("~p:load_cfg NewCfg=~p~n",[?MODULE,NewCfg]),
    DSinstlist=inst_ds(NewCfg),
    io:format("~p:load_cfg DSinstlist=~p~n",[?MODULE,DSinstlist]),
    start_handlers(DSinstlist,DSdb).




inst_ds(#dc_cfg_data{devices=P1,
		     input=P2,
		     datasources=P3}) ->
    io:format("inst_ds DS=~p~n",[P3]),
    inst_ds1(P3,P1,P2,[]).

inst_ds1([],_,_,Out) ->
    lists:reverse(Out);
inst_ds1([H=#dc_ds{device=Dev,input=Input,tables=T}|Rest],
	 Devs,Inputs,Out) ->
    DevI=case lists:keysearch(Dev,#dc_dev.id,Devs) of
	     {value,DevI0} ->
		 DevI0
	 end,
    InpI=if
	     Input==undefined -> Input;
	     true ->
		 case lists:keysearch(Input,#dc_input.id,Inputs) of
		     {value,InpI0} ->
			 InpI0
		 end
	 end,
    TI=inst_ds_ts(T,[]),
    NewH=H#dc_ds{device=DevI,input=InpI,tables=TI},
    inst_ds1(Rest,Devs,Inputs,[NewH|Out]).

inst_ds_ts([],Out) ->
    lists:reverse(Out);
inst_ds_ts([T|Rest],Out) ->
    %% Should validate in circdb that this table exists
    inst_ds_ts(Rest,[T|Out]).



start_handlers([],_DSdb) ->
    ok;
start_handlers([DS=#dc_ds{id=Id,title=Name}|Rest],DSdb) ->
    io:format("JB DS=~p~n",[DS]),
    case lookup_handler(Id,DSdb) of
	Pid when is_pid(Pid) ->
	    dc_handler:reconfig(Pid,DS);
	_ ->
	    case dc_handler:start_link(DS) of
		{ok,Pid} ->
		    insert_handler(#ds_inst{id=Id,
					    title=lists:flatten(Name),
					    pid=Pid},
				   DSdb)
	    end
    end,
    start_handlers(Rest,DSdb).


%% DB primitives
lookup_handler(DSid,DSdb) ->
    case ets:lookup(DSdb,DSid) of
	[#ds_inst{pid=Pid}] -> Pid;
	_ -> undefined
    end.
	    
insert_handler(Inst,DSdb) ->
    ets:insert(DSdb,Inst).




pp_dsdb([]) ->
    ok;
pp_dsdb([#ds_inst{id=Id,title=Title,pid=Pid}|Rest]) ->
    io:format("Id=~p Pid=~p Title=~s~n",[Id,Pid,Title]),
    pp_dsdb(Rest).


