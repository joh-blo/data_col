%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 11 May 2017 by Johan <>

-module(dc_sup).

-behaviour(supervisor).

%% public
-export([start_link/1,init/1]).



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(AppList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, AppList).



%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%----------------------------------------------------------------------
init(_AppList) ->
    %% Create a supervisor for each application in AppList
    Spec={dc_manager,
	  {dc_manager,start_link,[]},
	  transient,2000,worker,[data_col]},
    {ok,{{one_for_all,10,3},[Spec]}}.
