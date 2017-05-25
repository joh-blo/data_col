%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2015, 
%%% @doc
%%  Responsible for a single data source as defined in a #dc_ds{}
%%
%%  All operations on this data source is forwared to this process.
%%
%%% @end
-module(dc_handler).

-behaviour(gen_server).

%% API
-export([start_link/1,
	 last/1,
	 update/2,
	 reconfig/2
	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("data_col.hrl").


-define(SERVER, ?MODULE).
-define(OTP_INTERNAL_GENCAST,'$gen_cast').


-record(state,{
	  id,           % Data source id
	  device_type,  % Device type
	  input,        % Input
	  poll_interval,% (ms) Interval between each poll
	  db
	 }).

%%%===================================================================
%%% API
%%%===================================================================
update(Pid,V) ->
    gen_server:cast(Pid,{update,V}).

last(Pid) ->
    gen_server:call(Pid,last).


reconfig(Pid,Cfg) ->
    gen_server:cast(Pid,{reconfig,Cfg}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(DSinst) ->
    gen_server:start_link(?MODULE, [DSinst], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([#dc_ds{id=DSid,
	     device=#dc_dev{type=Type},
	     input=Input,
	     tables=Tables,
	     poll=PT}]) ->

    io:format("~p;init DSid=~p Type=~p~n Input=~p~n",[?MODULE,DSid,Type,Input]),
    CircHPid=circdb_manager:create(Tables,DSid),
    io:format("~p;init CircHPidlibre+8=~p~n",[?MODULE,CircHPid]),
    
    if
	is_integer(PT) ->
	    erlang:send_after(PT,self(),{?OTP_INTERNAL_GENCAST,update});
	true ->
	    ok
    end,
    {ok, #state{id=DSid,db=CircHPid,
		device_type=Type,input=Input,poll_interval=PT}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(last, _From, State=#state{id=DSid}) ->
    Reply = circdb_manager:last(DSid),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({reconfig,#dc_ds{device=#dc_dev{type=Type},
			     input=Input,
			     tables=_Tables,
			     poll=PT}},State) ->
    {noreply, State#state{device_type=Type,
			  input=Input,
			  poll_interval=PT}};
handle_cast({update,Y},State=#state{id=Id}) ->
    %% New update was pushed to us!
    if
	Y==undefined ->
	    ok;
	true ->
	    circdb_manager:update(Id,Y)
    end,
    {noreply, State};
handle_cast(update,State=#state{id=Id,
				device_type={node,Node},
				input=Input,
				poll_interval=PT}) ->
    Y=case Input#dc_input.input of
	  [#dc_in_cmd{cmd={_,M,F,[A]}}] when M==mon_otp;
					     M==mon_time;
					     M==mon_proc ->
	      case catch M:F(A,Node) of
		  {'EXIT',_} -> undefined;
		  Y0 -> Y0
	      end;
	  [#dc_in_cmd{cmd={_,M,F,A}}] ->
	      case rpc:call(Node,M,F,A) of
		  {badrpc,_} -> undefined;
		  Y0 -> Y0
	      end
      end,
    io:format("~p:update ~p ~p:~p~n",[?MODULE,{Node,M,F,A},Id,Y]),
    if
	Y==undefined ->
	    ok;
	true ->
	    circdb_manager:update(Id,Y)
    end,
    erlang:send_after(PT,self(),{?OTP_INTERNAL_GENCAST,update}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%% ----------------------------------------------------------------------------
%% 
%% 
%% alloc_tables([],_DsId,Out) ->
%%     lists:reverse(Out);
%% alloc_tables([H=#dc_ts{id=TsId,interval=Interval,buckets=Buckets}|Rest],
%% 	     DsId,Out) ->
%%     io:format("alloc_tables ~p Interval=~p Buckets=~p~n",
%% 	      [{DsId,TsId},Interval,Buckets]),
%%   NewOut=case circdb_manager:create(DsId,Interval,Buckets) of
%% 	  {ok,Pid} ->
%% 	     [{H,Pid}|Out];
%% 	  {error,{already_started,Pid}} ->
%% 	     [{H,Pid}|Out];
%% 	  Error ->
%%     io:format("alloc_tables ERROR ~p~n",[Error]),
%% 	     Out
%%       end,
%%     alloc_tables(Rest,DsId,NewOut).
