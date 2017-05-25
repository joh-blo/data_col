%%% @author  <>
%%% @copyright (C) 2015, 
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2015 by  <>

-module(dc_db).

-include("dc_internal.hrl").
-include("data_col.hrl").

-export([gen_default/0,
         gen_default/1,

	process_config/2]).

-export([calc_startdate/2]).

%% Test
% -export([gen_graph/3]).

%% For storing with text 
-export([validate_cfg/2]).

%% Some sort of default Round Robin palette of colors to use.
-define(DEFAULT_COLOR, ["EACC00","EA8F00","FF0000","0000FF","00FF00",
			"00FFFF","FF00FF","FFFF00","000000","AAAA00"]).


%% Store/update/delete all configured data.
validate_cfg(Config,undefined) ->
    %% This is the initial configuration, just store everything
    check_config(Config,true),
    Add=process_config_data(Config),
    store_config_data(Add);
validate_cfg(Config,Config_old) ->
io:format("~p:validate_cfg~n NEW=~p~n OLD=~p~n",[?MODULE,Config,Config_old]),
    check_config(Config,true),
    CfgCurr=process_config_data(Config),
    check_config(Config_old,false),
    case catch process_config_data(Config_old) of
	{Err,_Reason} when Err==error;
			  Err=='EXIT' ->
	    %% Some problems with old config data.
	    %% Just replace with new...
	    store_config_data(CfgCurr);
	CfgOld ->
	    {Remove,Add,_Keep}=diff_configs(CfgCurr,CfgOld),
    %% Should we also verify that all the rest is actually there ?
    %% In case of errors here things may get lost in the real world (but not in configuration)
	    remove_config_data(Remove),
	    store_config_data(Add),
	    validate_config_data(CfgCurr)
    end,
    ok.


%% Validate new configuration so that all connections actually exists
check_config(#dc_cfg_data{devices=P1,
			  input=P2,
			  datasources=P3,
%			  graphs=P5,
			  time_series=P6},Validate) ->
    if
	Validate ->
	    validate_datasources(P3,P1,P2,P3,P6);
	true ->
	    ok
    end.
	

validate_datasources([],_Pars1,_Pars2,_Pars3,_Pars6) ->
    ok;
validate_datasources([#dc_ds{device=DevId,input=Inp}|Rest],
		     Pars1,Pars2,Pars3,Pars6) ->
    case DevId of
	undefined when is_tuple(Inp) ->
	    ok;
	_ when is_integer(Inp) ->
	    case lists:keysearch(DevId,#dc_dev.id,Pars1) of
		false ->
		    %% ?debug("~p not a defined device",[DevId],
		    %% 	   validate_datasources),
		    throw({error,bad_device_in_datasource});
		_ ->
		    ok
	    end
    end,
    validate_datasources2(Inp,Pars2,Pars3,Pars6),
    validate_datasources(Rest,Pars1,Pars2,Pars3,Pars6).


validate_datasources2(Inp,Pars2,_Pars3,_Pars6) when is_integer(Inp) ->
    case lists:keysearch(Inp,#dc_dev.id,Pars2) of
	false ->
	    %% ?debug("~p not a defined input",[Inp],validate_datasources),
	    io:format("~p not a defined input",[Inp]),
	    throw({error,bad_input_in_datasource});
	{value,#dc_input{}} ->
	    ok
%	    validate_datasources3(Rra,Pars6)
    end;
validate_datasources2({computed,List},_Pars2,Pars3,_Pars6) ->
    validate_datasources_comp(List,Pars3).

validate_datasources_comp([],_Pars3) ->
    ok;
validate_datasources_comp([Num|Rest],Pars3) when is_integer(Num);
						 is_float(Num) ->
    validate_datasources_comp(Rest,Pars3);
validate_datasources_comp([Op|Rest],Pars3) when Op=='*';Op=='/';
						Op=='+';Op=='-';
						Op=='POP' ->
    validate_datasources_comp(Rest,Pars3);
validate_datasources_comp([H|Rest],Pars3) ->
    {DsId,_DsIdSub}=
	case H of
	    {ds,DsId0} when is_integer(DsId0) -> 
		{DsId0,undefined};
	    {ds,DsId0,DsIdSub0} when is_integer(DsId0),is_integer(DsIdSub0)->
		{DsId0,DsIdSub0};
	    _Err ->
		%% ?debug("Bad compute expression ~p",[Err],validate_datasources_comp),
		throw({error,bad_compute_expr})
	end,
    case lists:keysearch(DsId,#dc_ds.id,Pars3) of
	false ->
	    %% ?debug("Bad input, ~p not a defined datasource",[DsId],
	    %% 	   validate_datasources_comp),
	    throw({error,unknown_datasource});
	_ ->
	    ok
    end,
    validate_datasources_comp(Rest,Pars3).



store_config_data(DFD=#dc_cfg_data{% devices=P1,
				   % input=P2,
				   datasources=P3
				   % graphs=P5,
				   % time_series=P6
			}) ->
    %% Do not store "computed" data sources.
    NewP3=[Ds || Ds=#dc_ds{device=DevId} <- P3,is_integer(DevId)],
    DFD#dc_cfg_data{datasources=NewP3}.


%% Remove configuration data
%% - FIXME Must ensure circdb is updated accordingly
remove_config_data(#dc_cfg_data{devices=P1,
				input=P2,
				datasources=P3,
				graphs=P5,
				time_series=P6}) ->
    remove_config_data(P1++P2++P3++P5++P6,[]).


%% Note that the corresponding RRD for each DS is removed from disc!
remove_config_data([],_Out) ->
    % remove_db(Out);
    ok;
remove_config_data([H|Rest],Out) ->
    NewH=case H of
	     #dc_dev{id=Id} ->    {dc_dev,Id};
%	     #dc_ts{id=Id} ->     {dc_ts,Id};
	     #dc_input{id=Id} ->  {dc_input,Id};
	     #dc_ds{id=Id} ->     delete_rrd(H),{dc_ds,Id}
%	     #dc_graph{id=Id} ->  {dc_graph,Id}
	 end,
    remove_config_data(Rest,[NewH|Out]).


%% Validate requested/assumed configuration with actual configuration in db
validate_config_data(#dc_cfg_data{devices=P1,
				  input=P2,
				  datasources=P3,
				  graphs=P5,
				  time_series=P6}) ->
    validate_config_data2(P1++P2++P3++P5++P6).

validate_config_data2([]) ->
    ok;
validate_config_data2([H|Rest]) ->
    case H of
	#dc_dev{id=Id} ->
	    case dc_api_dev:get(Id) of
		{ok,H} ->
		    ok;
		_Error ->
		    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
		    %% 	   validate_config_data2),
		    % write_db([H])
		    ok
	    end;
	%% #dc_ts{id=Id} ->
	%%     case dc_api_ts:get(Id) of
	%% 	{ok,H} ->
	%% 	    ok;
	%% 	_Error ->
	%% 	    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
	%% 	    %% 	   validate_config_data2),
	%% 	    % write_db([H])
	%% 	    ok
	%%     end;
	#dc_input{id=Id} ->
	    validate_config_data_input(H,Id);
	#dc_ds{input={computed,_}} ->
	    %% Not stored in database
	    ok;
	#dc_ds{id=Id} ->
	    validate_config_data_ds(H,Id)
	%% #dc_graph{id=Id} ->
	%%     case dc_api_graph:get(Id) of
	%% 	{ok,H1} ->
	%% 	    case H1#dc_graph{images=[],graphed_ts=undefined} of
	%% 		H ->
	%% 		    ok;
	%% 		_Error ->
	%% 		    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
	%% 		    %% 	   validate_config_data2),
	%% 		    % write_db([H])
	%% 		    ok
	%% 	    end;
	%% 	_Error ->
	%% 	    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
	%% 	    %% 	   validate_config_data2),
	%% 	    % write_db([H])
	%% 	    ok
	%%     end
    end,
    validate_config_data2(Rest).

%% Validate both Mnesia and RRD database
validate_config_data_ds(H,DsId) ->
    case dc_api_ds:get(DsId) of
	{ok,H} ->
	    case dc_api_ds:fetch_data(DsId) of
		{value,_} ->
		    ok;
		_Error ->
		    %% ?debug("WARNING:Cannot find RRD for ~p, got ~p",
		    %% 	   [H,Error],
		    %% 	   validate_config_data2),
		    % write_db([H])
		    ok
	    end;
	_Error ->
	    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
	    %% 	   validate_config_data2),
	    % write_db([H])
	    ok
    end.


%% Note that a change of input may affect many data sources that we have to tune
%% to the updated value
validate_config_data_input(H,InputId) ->
    case dc_api_input:get(InputId) of
	{ok,H=#dc_input{input=[#dc_in_cmd{field=InCmds}]}} ->
	    %% Mnesia Database OK.
	    %% Now collect all data sources that depends on this input
	    L=[I || I=#dc_ds{input=II} <-
			dc_api_ds:get_all(),II==InputId],
	    validate_config_data_input_type(L,InCmds);
	_Error ->
	    %% ?debug("WARNING:Cannot find ~p in Mnesia, got ~p",[H,Error],
	    %% 	   validate_config_data2),
	    % write_db([H])
	    ok
    end.


%% Validate type with all #dc_in_cmd_field{} 
validate_config_data_input_type([],_InCmds) ->
    ok;
validate_config_data_input_type([#dc_ds{id=Id}|Rest],InCmds) ->
    RrrdCmds=dc_api_ds:info(Id),
    diff_types(InCmds,RrrdCmds,Id),
    validate_config_data_input_type(Rest,InCmds).

diff_types([],[],_DsId) ->
    ok;
diff_types([#dc_in_cmd_field{type=T1}|Rest1],
	   [{Id,#dc_in_cmd_field{type=T2}}|Rest2],DsId) ->
    if
	T1==T2 ->
	    ok;
	true ->
	    %% Tune RRD to Mnesia Type
	    A=dc_api_ds:tune(DsId,Id,T1),
	    io:format("Tuning ~p got ~p~n",[DsId,A])
    end,
    diff_types(Rest1,Rest2,DsId).



%% Delete RRD database
%% Note:
%% - RRD database are created when dataprov is started 
delete_rrd(#dc_ds{id=Id}) ->
    dc_api_ds:delete_rrd_db(Id).



process_config_data(Cfg0=#dc_cfg_data{devices=P1,
				      input=P2,
				      datasources=P3,
%				      graphs=P5,
				      time_series=P6}) ->
    Cfg1=add_config_data(P1,Cfg0#dc_cfg_data{devices=[]}),
    Cfg2=add_config_data(P2,Cfg1#dc_cfg_data{input=[]}),
    Cfg3=add_config_data(P3,Cfg2#dc_cfg_data{datasources=[]}),
%    Cfg5=add_config_data(P5,Cfg3#dc_cfg_data{graphs=[]}),
    Cfg6=add_config_data(P6,Cfg3#dc_cfg_data{time_series=[]}),
    UpdatedGraphs=[],
    Cfg6#dc_cfg_data{graphs=UpdatedGraphs}.



add_config_data([],Cfg) ->
    Cfg;
add_config_data([H=#dc_dev{type=Type}|Rest],
		Cfg=#dc_cfg_data{devices=Devices}) ->
    Desc=case Type of
	     {node,Node} when is_atom(Node) -> atom_to_list(Node);
	     _ -> "some device"
	 end,
    Dev=H#dc_dev{description=Desc,
		 access=[rpc],
%		 tags=[{fm_group, cast:to_str(Type)}]},
		 tags=[]},
    add_config_data(Rest,Cfg#dc_cfg_data{devices=[Dev|Devices]});
%% add_config_data([H=#dc_ts{consolidation=Co0
%% %			       xfactor=Xf0
%% 			       }|Rest],
%% 		Cfg=#dc_cfg_data{time_series=Rras}) ->
%%     Co=if
%% 	   Co0==undefined -> ["AVERAGE","MIN","MAX"];
%% 	   true -> Co0
%%        end,
%%     %% Xf=if
%%     %% 	   Xf0==undefined -> 0.5;
%%     %% 	   true -> Xf0
%%     %%    end,
%%     Rra=H#dc_ts{consolidation=Co
%% %		     xfactor=Xf
%% 		    },
%%     add_config_data(Rest,Cfg#dc_cfg_data{time_series=[Rra|Rras]});
add_config_data([H=#dc_input{input={M,F,Args},
%				 step=Step0,
%				 heartbeat=HeartBeat0,
%				 time_series=RRA0,
%				 description=Desc0,
			         label=Labels0,
				 opt=Opt0}|Rest],
		Cfg=#dc_cfg_data{input=Input}) ->
    Opt=if
	    Opt0==undefined -> [];
	    true -> Opt0
	end,
%    Args=store_input_genargs(A,[]),
    DefLabels=case Labels0 of
		undefined ->
		  atom_to_list(M)++":"++atom_to_list(F)++
		    io_lib:format("~p",[Args])++" values";
		_ ->
		  Labels0
	      end,
    InpType=set_input_type({M,F},Opt),
    InCmds=[#dc_in_cmd{type = rpc,
		       cmd={{node,placeholder},M,F,Args},
		       parse=undefined,
		       field=store_input_genfields(Args,{M,F},InpType,0,[])}],
    %% Step=if
    %% 	     Step0==undefined -> 300;
    %% 	     true -> Step0
    %% 	 end,
    %% HeartBeat=if
    %% 		  HeartBeat0==undefined -> 600;
    %% 		  true -> HeartBeat0
    %% 	      end,
    In=H#dc_input{label=DefLabels,
		  input=InCmds,
% type=rrd,
%		      step=Step,
%		      heartbeat=HeartBeat,
		      opt=Opt
		     },
    add_config_data(Rest,Cfg#dc_cfg_data{input=[In|Input]});
add_config_data([H=#dc_ds{device=undefined,
				type=computed}|Rest],
		Cfg=#dc_cfg_data{datasources=Ds}) ->
    add_config_data(Rest,Cfg#dc_cfg_data{datasources=[H|Ds]});
add_config_data([H=#dc_ds{device=DevId,input=InId}|Rest],
		Cfg=#dc_cfg_data{datasources=Ds,
				 devices=De,
				 input=I}) ->
  {N1,_Dev}=case lists:keysearch(DevId,#dc_dev.id,De) of
	      {value,Dev0=#dc_dev{description=N10}} ->
		{N10,Dev0};
	      _E1 when is_integer(DevId) ->
		{"Device/"++integer_to_list(DevId),undefined}
	    end,
  {N2,_In}=case lists:keysearch(InId,#dc_input.id,I) of
	       {value,In0=#dc_input{label=N20}} ->
		   {N20,In0};
	       _E2 when is_integer(InId) ->
		   {"Input/"++integer_to_list(InId),undefined};
	       _E2 ->
		   {"Provided",undefined}
	   end,
  
    Name=N1++" "++N2,
    HDS=H#dc_ds{title=Name},
    add_config_data(Rest,Cfg#dc_cfg_data{datasources=[HDS|Ds]}).
%% add_config_data([H=#dc_graph{}|Rest],
%% 		Cfg=#dc_cfg_data{graphs=Gr,
%% 				     input=I,
%% 				     datasources=Ds}) ->
%%     Graph=gen_graph(H,I,Ds),
%%     add_config_data(Rest,Cfg#dc_cfg_data{graphs=[Graph|Gr]}).


%% FIXME!
%% Add possibility to modify graph attributes
%% ylabel,
%% graphic, 
%% comp_ds
%    {dc_graph_cdef,"cdef1",
%     "field7,field8,+,field9,+,field10,+,field11,+,field12,ADDNAN"},
%% gen_graph(Gr=#dc_graph{ds=#dc_graph_ds{plot_type=PlotType0,
%% 					       stack=Stack0,
%% 					       con_fun=CF0,
%% 					       data_src=DS},
%% 			   title=Title0},
%% 	  I,Ds) ->
%%     CF=case CF0 of
%% 	   min -> "MIN";
%% 	   max -> "MAX";
%% 	   last -> "LAST";
%% 	   _ -> "AVERAGE"
%%        end,
%%     PlotType=case PlotType0 of
%% 		 {line,W} when is_integer(W);is_float(W) -> PlotType0;
%% 		 area -> PlotType0;
%% 		 _ -> {line,1.5}
%% 	     end,
%%     Stack=case Stack0 of
%% 	      true -> Stack0;
%% 	      _ -> false
%% 	  end,
%%     {FieldData,Title1,DevId}=gen_graph_fields(DS,I,Ds,[],[],undefined),
%%     Title=if
%% 	      is_list(Title0) -> Title0;
%% 	      true -> Title1
%% 	  end,
%%     {Defs,Items}=gen_graph_items(FieldData,PlotType,Stack,CF,1,[],[],[]),
%%     Gr#dc_graph{title=Title,
%% 		    description=Title,
%% 		    ds=Defs,
%% 		    item=Items,
%% 		    displayId=DevId, % Replace with display Id later
%% 		    images=[],
%% 		    ttl=?GRAPH_TTL,
%% 		    opt=[]}.

%% Generates a "graph_item" for each counter etc that should be visible in the
%% graph. Returns a tuple {FieldData,Title,MenuTitle,InvDefs} where
%% - FieldData [{Def,Opt}] where
%%     + Def is #dc_graph_def{} or #dc_graph_cdef{} (computed data)
%%     + Opt is an option list for a field
%% - Title is the title of the graph
%% - MenuTitle is the menu the graph should be associated to
%% - InvDefs is a list with #dc_graph_def{}, ie references to data source
%%   not directly visible in the graph (but used in some #dc_graph_cdef{})
%%
%% Note:
%% - When computed data source:
%%   + RawCmd is an expression over data source references
%%   + One (or several) of the data sources in RawCmd may be "vectors",
%%     ie more than one #dc_in_cmd_field{}, but operators are not vector
%%     operators. We must therefore first "expand" RawCmd into expressions
%%     including no vectors.
%% - The field id (ie "fieldXX") in all #dc_graph_def{} and
%%   #dc_graph_cdef{} are set later.
%%   Similarly, 
%%   + the data source references in RawCmd are replaced with "fieldXX" later
%%   + the field in #dc_graph_def{} (holding DsIdSub) is replaced with
%%     "fXX" later.
gen_graph_fields([],_I,_Ds,FieldData,Title,MenuTitle) ->
    {FieldData,string:sub_string(Title,1,80),MenuTitle};
gen_graph_fields([H|Rest],I,Ds,FieldData,Title,MenuTitle) ->
    {DsId,DsIdSub,Opts1}=
	case H of
	    DsId0 when is_integer(DsId0) ->
		{DsId0,undefined,[]};
	    {DsId0,Opts0} when is_integer(DsId0) ->
		{DsId0,undefined,Opts0};
	    {DsId0,DsIdSub0,Opts0} when is_integer(DsId0),is_integer(DsIdSub0)->
		{DsId0,DsIdSub0,Opts0}
	end,
    {NewMenuTitle,VDefs,T,IDefs}=
	case lists:keysearch(DsId,#dc_ds.id,Ds) of
	    {value,#dc_ds{device=undefined,
			  input={computed,RawCmd}
			  % title=T00
			 }} ->
		{VDefs0,IDefs0}=gen_cdef_cmd(RawCmd,FieldData,[[]],[],[]),
		{MenuTitle,VDefs0,"",IDefs0};
	    {value,#dc_ds{device=DevId,input=InId,title=T00}}
	      when is_integer(DevId) ->
		MenuTitle0=case MenuTitle of
			       undefined -> DevId;
			       _ -> MenuTitle
			   end,
		VDef0=case lists:keysearch(InId,#dc_input.id,I) of
%% 			  {value,
%% 			   #dc_input{input=[#dc_in_cmd{field=F0}]}} ->
%% 			      gen_graph_fields1(F0,DsId,[]);
%% %			   gen_graph_fields1(F0,DsId,[],[]);
			  _ ->
			      %% ?debug("~p not a defined input",[InId],
			      %% 	     gen_graph_fields),
			      throw({error,unknown_input})
		      end,
		{MenuTitle0,VDef0,T00,[]};
	    _ ->
		%% ?debug("~p not a defined datasource",[DsId],gen_graph_fields),
		throw({error,unknown_datasource})
	end,
%%    FD0=gen_graph_fields3(IDefs,Opts1,[]),
%%    FD1=gen_graph_fields2(VDefs,DsId,DsIdSub,Opts1,0,[]),
    FD0=[],
    FD1=[],
    gen_graph_fields(Rest,I,Ds,FieldData++FD0++FD1,Title++T,NewMenuTitle).


%% Create list with #dc_graph_def{} from #dc_input{}
%% Note:
%% - The UID must later be replaced with a proper reference ("fXX")
%% gen_graph_fields1([],_DsId,Out) ->
%%     lists:reverse(Out);
%% gen_graph_fields1([H=#dc_in_cmd_field{uid=UID}|Rest],DsId,Out) ->
%%     Def=#dc_graph_def{ds=DsId,field=UID},
%%     gen_graph_fields1(Rest,DsId,[{Def,H}|Out]).


%% Add title and dsid to the option list for each data source reference
%% 
%% gen_graph_fields2([],_DsId,_DsIdSub,_Opt,_N,Out) ->
%%     lists:reverse(Out);
%% gen_graph_fields2([{Def,#dc_in_cmd_field{title=FieldLabel}}|Rest],
%% 		  DsId,DsIdSub,Opt,N,Out) ->
%%     NewOut=if
%% 	       is_integer(DsIdSub),DsIdSub=/=N ->
%% 		   Out;
%% 	       true ->
%% 		   NewOpt=case proplists:get_value(label,Opt) of
%% 			      undefined -> [{label,FieldLabel},{dsid,DsId}|Opt];
%% 			      _ ->         [{dsid,DsId}|Opt]
%% 			  end,
%% 		   [{Def,NewOpt}|Out]
%% 	   end,
%%     gen_graph_fields2(Rest,DsId,DsIdSub,Opt,N+1,NewOut).

gen_graph_fields3([],_Opts,Out) ->
    lists:reverse(Out);
gen_graph_fields3([H|Rest],Opts,Out) ->
    NewH={H,[{invisible,true}|Opts]},
    gen_graph_fields3(Rest,Opts,[NewH|Out]).


%% gen_cdef_cmd(RawCmd,Fields,CmdsOut,CurrentFieldN,Ndb,InvisbleDefsOut) ->
%%    {CDefs,NextCurrentFieldN,InvisbleDefsOut}
gen_cdef_cmd([],_Fields,CmdsOut,_Ndb,InvDefs) ->
%    CDefs=gen_cdef_cmd1(CmdsOut,[]),
    CDefs=[],
    {CDefs,InvDefs};
%    {CmdsOut,InvDefs};
gen_cdef_cmd([Op|Rest],Fields,CmdsOut,Ndb,InvDefs)
  when Op=='*';Op=='/';Op=='+';Op=='-';Op=='POP' ->
    NewCmdsOut=gen_cdef_cmd_merge(CmdsOut,atom_to_list(Op),Rest,[]),
    gen_cdef_cmd(Rest,Fields,NewCmdsOut,Ndb,InvDefs);
gen_cdef_cmd([Op|Rest],Fields,CmdsOut,Ndb,InvDefs)
  when is_integer(Op) ->
    NewCmdsOut=gen_cdef_cmd_merge(CmdsOut,integer_to_list(Op),Rest,[]),
    gen_cdef_cmd(Rest,Fields,NewCmdsOut,Ndb,InvDefs);
gen_cdef_cmd([Op|Rest],Fields,CmdsOut,Ndb,InvDefs)
  when is_float(Op) ->
    NewCmdsOut=gen_cdef_cmd_merge(CmdsOut,float_to_list(Op),Rest,[]),
    gen_cdef_cmd(Rest,Fields,NewCmdsOut,Ndb,InvDefs);
gen_cdef_cmd([Op|Rest],Fields,CmdsOut,Ndb,InvDefs) ->
    {NewOp,NewInvDefs,NewNdb}=
	case Op of
	    {ds,DsId} ->
		%% Found reference to data source
		%% Now:
		%% - Update Ndb with new field number for this data source
		%% - Find out the visible elements in the (vector) data source
		{_FN,Ndb2}=
		    case proplists:get_value(DsId,Ndb) of
			undefined ->
			    {0,[{DsId,0}|Ndb]};
			OldFN ->
			    {OldFN+1,lists:keyreplace(DsId,1,Ndb,{DsId,OldFN+1})}
		    end;
		%% case gen_cdef_cmd_lookup(Fields,DsId,[]) of
%% 		    [] ->
%% 			%% This data source is invisble in this graph
%% 			IDef=#dc_graph_def{ds=DsId,
%% 					       field=0
%% %					       field="f0"++cast:to_str(FN)
%% 					      },
%% 			{[IDef],[{IDef,DsId}|InvDefs],Ndb2};
		%%     DefList ->
		%% 	{DefList,InvDefs,Ndb2}
		%% end;
	    {ds,DsId,DsIdsub} ->
		{_FN,Ndb2}=
		    case proplists:get_value(DsId,Ndb) of
			undefined ->
			    {0,[{DsId,0}|Ndb]};
			OldFN ->
			    {OldFN+1,lists:keyreplace(DsId,1,Ndb,{DsId,OldFN+1})}
		    end
%%	    ->
%%		case gen_cdef_cmd_lookup(Fields,DsId,[]) of
		    %% [] ->
		    %% 	%% This data source is invisble in this graph
		    %% 	IDef=#dc_graph_def{ds=DsId,
		    %% 			       field=DsIdsub
		    %% 			      },
		    %% 	{{DsId,DsIdsub},[IDef|InvDefs],Ndb2};
		%%     _DefList ->
		%% 	{{DsId,DsIdsub},InvDefs,Ndb2}
		%% end
		    
	end,
    NewCmdsOut=gen_cdef_cmd_merge(CmdsOut,NewOp,Rest,[]),
    gen_cdef_cmd(Rest,Fields,NewCmdsOut,NewNdb,NewInvDefs).

%% Create lists 
gen_cdef_cmd_merge([],_Op,_RestOp,Out) ->
    lists:reverse(Out);
%% gen_cdef_cmd_merge([CmdOut|Rest],Op=[D|_],RestOp,Out) 
%%   when is_record(D,dc_graph_def) ->
%%     NewOut=gen_cdef_cmd_merge1(Op,CmdOut,RestOp,Out),
%%     gen_cdef_cmd_merge(Rest,Op,RestOp,NewOut);
gen_cdef_cmd_merge([CmdOut|Rest],Op,RestOp,Out) ->
    NewCmdOut=case RestOp of
		  [] -> [Op|CmdOut];
		  _ ->  [",",Op|CmdOut]
	      end,
    gen_cdef_cmd_merge(Rest,Op,RestOp,[NewCmdOut|Out]).


%% Generate CDEF commands (#dc_graph_cdef{}) for all elements in
%% CmdOut. 
%% Note that vector data generates several Cmd's.
%% gen_cdef_cmd_merge1([],_CmdOut,_RestOp,Out) ->
%%     CDef=#dc_graph_cdef{cmd=lists:reverse(Out)},
%%     {CDef,#dc_in_cmd_field{}};
%% gen_cdef_cmd_merge1([#dc_graph_def{id=Id}|Rest],CmdOut,RestOp,Out) ->
%%     NewCmdOut=case RestOp of
%% 		  [] -> [Id|CmdOut];
%% 		  _ ->  [",",Id|CmdOut]
%% 	      end,
%%     gen_cdef_cmd_merge1(Rest,CmdOut,RestOp,[NewCmdOut|Out]);
gen_cdef_cmd_merge1([Id|Rest],CmdOut,RestOp,Out) ->
    NewCmdOut=case RestOp of
		  [] -> [Id|CmdOut];
		  _ ->  [",",Id|CmdOut]
	      end,
    gen_cdef_cmd_merge1(Rest,CmdOut,RestOp,[NewCmdOut|Out]).


%% Generate CDEF commands (#dc_graph_cdef{}) for all CmdOut's.
%% Note: 
%% -We are dealing with vector data and operators are applied on vectors.
%%  A CmdOut thus holds all operations applied on an element on those vectors.
%% -Wait with instantiating references to other fields.
%% gen_cdef_cmd1([],Out) ->
%%     lists:reverse(Out);
%% gen_cdef_cmd1([CmdOut|Rest],Out) ->
%%     CDef=#dc_graph_cdef{cmd=lists:reverse(CmdOut)},
%%     H={CDef,#dc_in_cmd_field{}},
%%     gen_cdef_cmd1(Rest,[H|Out]).


%% gen_cdef_cmd_lookup([],_DsId,Out) ->
%%     lists:reverse(Out);
%% gen_cdef_cmd_lookup([{D=#dc_graph_def{ds=DsId},_}|Rest],DsId,Out) ->
%%     gen_cdef_cmd_lookup(Rest,DsId,[D|Out]);
%% gen_cdef_cmd_lookup([_|Rest],DsId,Out) ->
%%     gen_cdef_cmd_lookup(Rest,DsId,Out).



%% gen_graph_items([],_PlotType,_Stack,_CF,_N,_Ndb,Defs,Items) ->
%%     {lists:reverse(Defs),lists:reverse(Items)};
%% gen_graph_items([{Def0,Opts}|Rest],PlotType,Stack0,CF,N,Ndb,Defs,Items)
%%   when is_integer(N) ->
%%     Color=case proplists:get_value(color,Opts) of
%% 	      [A10,A2,B10,B2,C10,C2] ->
%% 		  %% A1=(to_hex((to_int(A10)+3*N) rem 16)),
%% 		  %% B1=(to_hex((to_int(B10)+3*N) rem 16)),
%% 		  %% C1=(to_hex((to_int(C10)+3*N) rem 16)),
%% 		  %% [A1,A2,B1,B2,C1,C2];
%% 		  [A10,A2,B10,B2,C10,C2];
%% 	      _ ->
%% 		  lists:nth((N rem length(?DEFAULT_COLOR))+1,?DEFAULT_COLOR)	
%% 	  end,
%%     Stack=case proplists:get_value(stack,Opts) of
%% 	      Stack1 when Stack1==false,Stack1==true ->
%% 		  Stack1;
%% 	      _ ->
%% 		  Stack0
%% 	  end,
%%     Title=proplists:get_value(label,Opts),
%%     FieldTitle="field"++integer_to_list(N),
%%     {Def,DSref}=
%% 	case Def0 of
%% 	    #dc_graph_def{ds=DsId,field=DsIdSub} when is_atom(DsIdSub) ->
%% 		{Def0#dc_graph_def{id=FieldTitle,
%% 				   field="f0"++atom_to_list(DsIdSub),
%% 				   cf=CF,
%% 				   step=0},
%% 		 {DsId,DsIdSub}};
%% 	    #dc_graph_cdef{cmd=Cmd0} ->
%% 		Cmd=update_cdef_cmd(Cmd0,Ndb,[]),
%% 		{Def0#dc_graph_cdef{id=FieldTitle,
%% 					cmd=Cmd},
%% 		 cdef}
%% 	end,
%%     NewItems=case proplists:get_value(invisible,Opts) of
%% 		 true ->
%% 		     Items;
%% 		 _ ->
%% 		     Item=#dc_graph_item{field=FieldTitle,
%% 					     title=Title,
%% 					     color=Color,
%% 					     type=PlotType,  % {line,1.5},
%% 					     visible=true,
%% 					     stack=Stack  % false
%% 					    },
%% 		     [Item|Items]
%% 	     end,
%%     NewNdb=[{DSref,FieldTitle}|Ndb],
%%     gen_graph_items(Rest,PlotType,Stack,CF,N+1,NewNdb,[Def|Defs],NewItems).

update_cdef_cmd([],_Ndb,Out) ->
    lists:reverse(Out);
update_cdef_cmd([DsRef={_DSid,_DSidSub}|Rest],Ndb,Out) ->
    %% Lookup the field name for this graph item
    FieldName=proplists:get_value(DsRef,Ndb),
    update_cdef_cmd(Rest,Ndb,[FieldName|Out]);
update_cdef_cmd([H|Rest],Ndb,Out) ->
    update_cdef_cmd(Rest,Ndb,[H|Out]).




calc_startdate(DeltaTime,{Date={SY,SM,SD},_Time}) ->
    StartDate=
	case parse_deltatime(lists:reverse(DeltaTime),0,{0,undefined}) of
	    {N,day} ->
		D1=calendar:date_to_gregorian_days(Date)-N,
		calendar:gregorian_days_to_date(D1);
	    {N,week} ->
		D1=calendar:date_to_gregorian_days(Date)-N*7,
		calendar:gregorian_days_to_date(D1);
	    {N,month} ->
		SYdiff=case SM-N of
			 SMdiff0 when SMdiff0>=0 -> (SMdiff0 div 12);
			 SMdiff0 -> (SMdiff0 div 12)-1
		     end,
		Year=SY+SYdiff,
		Month=case SM-N of
			  SMdiff1 when SMdiff1>0 -> SMdiff1 rem 12;
			  SMdiff1 -> (((SYdiff*12*-1)+SMdiff1-1) rem 12)+1
		      end,
		{Year,Month,SD};
	    _ ->
		%% ?debug("~p not a valid interval",[DeltaTime],calc_startdate),
		throw({error,bad_interval_in_report})
	end,
    {StartDate,{0,0,0}}.


parse_deltatime([],_Pow,Out) ->
    Out;
parse_deltatime([H|Rest],Pow,{N,P}) when $0=<H,H=<$9,P=/=undefined ->
    NewN=N+(H-$0)*trunc(math:pow(10,Pow)),
    parse_deltatime(Rest,Pow+1,{NewN,P});
parse_deltatime([H|Rest],Pow,{N,undefined}) ->
    P=case H of
	  $d -> day;
	  $w -> week;
	  $m -> month
      end,
    parse_deltatime(Rest,Pow,{N,P}).



    
diff_configs(#dc_cfg_data{devices=P1,
				input=P2,
				datasources=P3,
				graphs=P5},
	     #dc_cfg_data{devices=OldP1,
				input=OldP2,
				datasources=OldP3,
				graphs=OldP5}) ->
    {RemP1,AddP1,KeepP1}=diff_config(P1,OldP1,[],[],[]),
    {RemP2,AddP2,KeepP2}=diff_config(P2,OldP2,[],[],[]),
    {RemP3,AddP3,KeepP3}=diff_config(P3,OldP3,[],[],[]),
    {RemP5,AddP5,KeepP5}=diff_config(P5,OldP5,[],[],[]),
    {#dc_cfg_data{devices=RemP1,input=RemP2,datasources=RemP3,
			graphs=RemP5},
     #dc_cfg_data{devices=AddP1,input=AddP2,datasources=AddP3,
			graphs=AddP5},
     #dc_cfg_data{devices=KeepP1,input=KeepP2,datasources=KeepP3,
			graphs=KeepP5}
    }.


%% diff_config(NewCfg,OldCfg,OutRemove,OutAdd,OutKeep) ->

diff_config([],[],Remove,Add,Keep) ->
    {lists:reverse(Remove),lists:reverse(Add),lists:reverse(Keep)};
diff_config([],[H|Rest],Remove,Add,Keep) ->
    {lists:reverse([H|Remove])++Rest,lists:reverse(Add),lists:reverse(Keep)};
%    diff_config([],Rest,[H|Remove],Add,Keep);
diff_config([H|Rest],[],Remove,Add,Keep) ->
    {lists:reverse(Remove),lists:reverse([H|Add])++Rest,lists:reverse(Keep)};
%    diff_config(Rest,[],Remove,[H|Add],Keep);
diff_config([H|Rest],Old,Remove,Add,Keep) ->
    {Res,PVO}=case H of
		  #dc_dev{id=Id} -> 
		      {lists:keysearch(Id,#dc_dev.id,Old),
		       lists:keydelete(Id,#dc_dev.id,Old)};
		  #dc_input{id=Id} -> 
		      {lists:keysearch(Id,#dc_input.id,Old),
		       lists:keydelete(Id,#dc_input.id,Old)};
		  #dc_ds{id=Id} -> 
		      {lists:keysearch(Id,#dc_ds.id,Old),
		       lists:keydelete(Id,#dc_ds.id,Old)}
		  %% #dc_graph{id=Id} -> 
		  %%     {lists:keysearch(Id,#dc_graph.id,Old),
		  %%      lists:keydelete(Id,#dc_graph.id,Old)}
	      end,
    case Res of
	{value,H} ->
	    %% Not modified
	    diff_config(Rest,PVO,Remove,Add,[H|Keep]);
	{value,OldH} ->
	    %% Modified
	    diff_config(Rest,PVO,[OldH|Remove],[H|Add],Keep);
	_ ->
	    %% New
	    diff_config(Rest,PVO,Remove,[H|Add],Keep)
    end.

%% Remove min and max limits from counters/gauges
%% store_input_genargs([],Out) ->
%%     %% List of arguments passed as single argument, thus wrap arguments in list
%%     case lists:reverse(Out) of
%% 	[] -> [];
%% 	L -> [L]
%%     end;
%% store_input_genargs([{A,Mi,Ma}|Rest],Out) when is_integer(Mi),
%% 					       is_integer(Ma) ->
%%     store_input_genargs(Rest,[A|Out]);
%% store_input_genargs([{Mi,Ma}|Rest],Out) when is_integer(Mi),
%% 					     is_integer(Ma) ->
%%     store_input_genargs(Rest,Out);
%% store_input_genargs([A|Rest],Out) ->
%%     store_input_genargs(Rest,[A|Out]).


%% Set input type
%% Force input type on known inputs
set_input_type({ets,all},_Opt) ->
    gauge;
set_input_type({erlang,processes},_Opt) ->
    gauge;
set_input_type({erlang,memory},_Opt) ->
    gauge;
set_input_type({disksup,get_disk_data},_Opt) ->
    gauge;
set_input_type(_,Opt) ->
    proplists:get_value(type,Opt,counter).

%% Rewrite to format accepted by bt
store_input_genfields([],{M,F},InpType,Uid,[]) when is_atom(M),is_atom(F) ->
    Title=atom_to_list(M)++","++atom_to_list(F),
    [#dc_in_cmd_field{uid=Uid,
			  title=Title,
			  type=InpType,
			  min=0,
			  max=10000000000
			 }];
store_input_genfields([],_MF,_InpType,_Uid,Out) ->
    lists:reverse(Out);
store_input_genfields([InH|Rest],{M,F},InpType,Uid,Out) when is_atom(M),is_atom(F) ->
    {Title,Min,Max}=
	case InH of
	    {Min0,Max0} when is_integer(Min0),is_integer(Max0) ->
		{atom_to_list(M)++","++atom_to_list(F),
		 Min0,Max0};
	    {Key0,Min0,Max0} when is_atom(Key0),
				  is_integer(Min0),is_integer(Max0) ->
		{atom_to_list(M)++","++atom_to_list(F)++","++atom_to_list(Key0),
		 Min0,Max0};
	    Key0 when is_atom(Key0) ->
		{atom_to_list(M)++","++atom_to_list(F)++","++atom_to_list(Key0),
		 0,10000000000}
	end,
    %% Default value.
    H=#dc_in_cmd_field{uid=Uid,
			   title=Title,
			   type=InpType,
			   min=Min,
			   max=Max
			  },
    store_input_genfields(Rest,{M,F},InpType,Uid+1,[H|Out]);
store_input_genfields([{Key,Min,Max}|Rest],{M,F},InpType,Uid,Out)
  when is_atom(M),
       is_atom(F) ->
    Title=atom_to_list(M)++","++atom_to_list(F)++","++atom_to_list(Key),
    H=#dc_in_cmd_field{uid=Uid,
			   title=Title,
			   type=InpType,
			   min=Min,
			   max=Max
			  },
    store_input_genfields(Rest,{M,F},InpType,Uid+1,[H|Out]).

gen_default() ->
    gen_default( ha_lib:get_node_type() ).

gen_default( _ ) ->
    ok.




process_config([],Cfg) ->
  Cfg;
process_config([H|Rest],Cfg0) when is_list(H) ->
  Cfg1=process_config(H,Cfg0),
  process_config(Rest,Cfg1);
process_config([H=#dc_dev{type=Type}|Rest],
	       Cfg=#dc_cfg_data{devices=Devices}) ->
    Desc=case Type of
	     {node,Node} when is_atom(Node) -> atom_to_list(Node);
	     _ -> "some device"
	 end,
    Dev=H#dc_dev{description=Desc,
		 access=[rpc],
		 tags=[]},
    process_config(Rest,Cfg#dc_cfg_data{devices=[Dev|Devices]});
%% process_config([H=#dc_ts{consolidation=Co0}|Rest],
%% 	       Cfg=#dc_cfg_data{time_series=Rras}) ->
%%     Co=if
%% 	   Co0==undefined -> ["AVERAGE","MIN","MAX"];
%% 	   true -> Co0
%%        end,
%%     Rra=H#dc_ts{consolidation=Co},
%%     process_config(Rest,Cfg#dc_cfg_data{time_series=[Rra|Rras]});
process_config([H=#dc_input{input={M,F,Args},
			    label=Label0,
			    opt=Opt0}|Rest],
	       Cfg=#dc_cfg_data{input=Input}) ->
    Opt=if
	    Opt0==undefined -> [];
	    true -> Opt0
	end,
  DefLabels=case Label0 of
	      undefined ->
		atom_to_list(M)++":"++atom_to_list(F)++
		  io_lib:format("~p",[Args])++" values";
	      _ ->
		Label0
	    end,
    InpType=set_input_type({M,F},Opt),
    InCmds=[#dc_in_cmd{type = rpc,
		       cmd={{node,placeholder},M,F,Args},
		       parse=undefined,
		       field=store_input_genfields(Args,{M,F},InpType,0,[])}],
    In=H#dc_input{% type=rrd,
		  label=DefLabels,
		  input=InCmds,
		  opt=Opt
		 },
    process_config(Rest,Cfg#dc_cfg_data{input=[In|Input]});
process_config([H=#dc_ds{device=undefined,
			 type=computed}|Rest],
	       Cfg=#dc_cfg_data{datasources=Ds}) ->
    process_config(Rest,Cfg#dc_cfg_data{datasources=[H|Ds]});
process_config([H=#dc_ds{device=DevId,input=InId}|Rest],
	       Cfg=#dc_cfg_data{datasources=Ds,
				devices=De,
				input=I}) ->
    {N1,_Dev}=case lists:keysearch(DevId,#dc_dev.id,De) of
		  {value,Dev0=#dc_dev{description=N10}} ->
		      {N10,Dev0};
		  _E1 when is_integer(DevId) ->
		      {"Device/"++integer_to_list(DevId),undefined}
	      end,
    {N2,_In}=case lists:keysearch(InId,#dc_input.id,I) of
		 {value,In0=#dc_input{label=N20}} ->
		     {N20,In0};
		 _E2 when is_integer(InId) ->
		     {"Input/"++integer_to_list(InId),undefined};
		 _E2 ->
		     {"Provided",undefined}
	     end,

    Name=N1++" "++N2,
    HDS=H#dc_ds{title=Name},
    process_config(Rest,Cfg#dc_cfg_data{datasources=[HDS|Ds]}).
%% process_config([H=#dc_graph{}|Rest],
%% 		Cfg=#dc_cfg_data{graphs=Gr,
%% 				     input=I,
%% 				     datasources=Ds}) ->
%%     Graph=gen_graph(H,I,Ds),
%%     process_config(Rest,Cfg#dc_cfg_data{graphs=[Graph|Gr]}).

