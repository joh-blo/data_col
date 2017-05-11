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

	 %% Management 
	 info/0,
	 load_cfg/0]).

-record(state,{
	  cfg_db, % All data_col configuration data
	  ds_db   % Each configured data source 
	 }).


-record(ds_inst,{
	  id,   % #dc_ds.id
	  title,% #dc_ds.title
	  pid   % Pid to handler process
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Gen server interface poo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
init([]) -> 
%    Cfg=validate_cfg(undefined),
    DSdb=ets:new(ds_db,[{keypos, #ds_inst.id}]),
    {ok, #state{ds_db=DSdb}}.

%% terminate
%% @hidden
terminate(_Reason, _State) -> ok.


%%
%% handle_call
%% @hidden
%% handle_call({create,Name,Step,Repeats}, _From,State=#state{}) ->
%%     %% 
%%     Table=alloc_table(Name,Step,Repeats),

%%     {reply, ok, State#state{}};

handle_call(info, _From,State=#state{ds_db=DSdb}) ->
    pp_dsdb(ets:tab2list(DSdb)),
    {reply,ok, State};
handle_call(load_cfg, _From,State=#state{ds_db=DSdb}) ->
    %% FIXME! This just reloads the configuration, but we should also:
    %% - Create delta between old and new and remove configuration not in new
    %% - There may be active jobs , what to do with these?
    %% - Handle HTTP and SMTP configuration changes also
    %% - Sync login status for bugtrackers
    NewCfg=dc_lib:read_cfg(),
%%    validate_cfg(OldCfg,NewCfg),

    DSinstlist=inst_ds(NewCfg),
    start_handlers(DSinstlist,DSdb),
    {reply,ok, State#state{cfg_db=NewCfg}}.


%% handle_cast
%% @hidden
handle_cast(_Msg, State) -> 
%   ?debug("Got unexpected cast msg: ~p~n", [Msg], erlrrd),
  {noreply, State}.

%% handle_info
%% @hidden
handle_info(_Msg, State) -> 
%    ?debug("Got unexpected info msg: ~p~n", [Msg], erlrrd),
    {noreply, State}.


%% @hidden
code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.


%%% ----------------------------------------------------------------------------
inst_ds(#dc_cfg_data{devices=P1,
		     input=P2,
		     datasources=P3,
%		     graphs=P5,
		     time_series=P6}) ->
    io:format("inst_ds DS=~p~n",[P3]),
    inst_ds1(P3,P1,P2,P6,[]).

inst_ds1([],_,_,_,Out) ->
    lists:reverse(Out);
inst_ds1([H=#dc_ds{device=Dev,input=Input,tables=T}|Rest],
	 Devs,Inputs,Tables,Out) ->
    DevI=case lists:keysearch(Dev,#dc_dev.id,Devs) of
	     {value,DevI0} ->
		 DevI0
	 end,
    InpI=case lists:keysearch(Input,#dc_input.id,Inputs) of
	     {value,InpI0} ->
		 InpI0
	 end,
    TI=inst_ds_ts(T,Tables,[]),
    NewH=H#dc_ds{device=DevI,input=InpI,tables=TI},
    inst_ds1(Rest,Devs,Inputs,Tables,[NewH|Out]).

inst_ds_ts([],_Tables,Out) ->
    lists:reverse(Out);
inst_ds_ts([T|Rest],Tables,Out) ->
    TI=case lists:keysearch(T,#dc_ts.id,Tables) of
	   {value,TI0} ->
	       TI0
       end,
    inst_ds_ts(Rest,Tables,[TI|Out]).



start_handlers([],_DSdb) ->
    ok;
start_handlers([DSinst=#dc_ds{id=Id,title=Name}|Rest],DSdb) ->
    io:format("JB DSinst=~p~n Name=~p~n",[DSinst,Name]),
    case ets:lookup(DSdb,Id) of
      [#ds_inst{pid=Pid}] ->
	dc_handler:reconfig(Pid,DSinst);
      _ ->
	case dc_handler:start_link(DSinst) of
	  {ok,Pid} ->
	    ets:insert(DSdb,#ds_inst{id=Id,title=lists:flatten(Name),pid=Pid})
	end
    end,
    start_handlers(Rest,DSdb).


%% DB primitives


%% validate_cfg(OldCfg,NewCfg) ->
%%     dc_db:validate_cfg(OldCfg,NewCfg).


pp_dsdb([]) ->
    ok;
pp_dsdb([#ds_inst{id=Id,title=Title,pid=Pid}|Rest]) ->
  io:format("Id=~p Pid=~p Title=~s~n",[Id,Pid,Title]),
  pp_dsdb(Rest).


