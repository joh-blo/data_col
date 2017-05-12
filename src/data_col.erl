%%% @author Johan <>
%%% @copyright (C) 2017, Johan
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2017 by Johan <>

-module(data_col).

-export([map_record/2]).

-include("data_col.hrl").

%% --- Specific configuration records for data_col
%% Note:
%% - dc_dev replaces the special hostname "<thishost>" with the hostname of
%%   node(), i.e., the host we are currently running on.
map_record(RecordType,Fields) ->
    case RecordType of
	%% dc_ts ->
	%%     DecFields=record_info(fields,dc_ts),
	%%     Defaults=[],
	%%     FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	%%     list_to_tuple([RecordType|FieldList]);
	dc_dev ->
	    DecFields=record_info(fields,dc_dev),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    Rec=list_to_tuple([RecordType|FieldList]),
	    Type=case Rec#dc_dev.type of
		     {node,NodeName0} ->
			 NodeName=
			     case string:tokens(atom_to_list(NodeName0),"@") of
				 [N,"<thishost>"] ->
				     [_,H]=string:tokens(atom_to_list(node()),
							 "@"),
				     list_to_atom(N++"@"++H);
				 [_,_] -> NodeName0
			     end,
			 {node,NodeName}
		 end,
	    Rec#dc_dev{type=Type};
	dc_input ->
	    DecFields=record_info(fields,dc_input),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    list_to_tuple([RecordType|FieldList]);
	dc_ds ->
	    DecFields=record_info(fields,dc_ds),
	    Defaults=[],
	    FieldList=emd_cfg:create_record_list(DecFields,Fields,Defaults,[]),
	    list_to_tuple([RecordType|FieldList])
    end.
