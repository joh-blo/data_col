% Default title for a datasource
-define( DS_TITLE,         {" %dev% - %input%",[]} ).

% Time when a graph may be regraphed [if (!force)]
-define( GRAPH_TTL,        300 ).

%%--------------------------------------------------------------------
%% A Data Input contains a serie of command that together collect
%% data to be used for statistics. The Data Input is applied on a
%% Device by creating a Datasource. The Datasource can be translated
%% as: Every X seconds, run this DataInput on Device and store the
%% return data for display later in i.e. Graph.
%%--------------------------------------------------------------------
-record(dc_input,{
	  id,      % integer() Key
	  label,   % str() | [str()] Short description/Default label in graphs 
          input,   % [#dc_in_cmd{}] Collect method with parsing
          opt=[]   % [{atom(),term()}] Optional Values
%          type=rrd,      % atom()    :: rrd | alarm | {cb, {m,f,a}}
%          description, % string()  :: Short desciprtion
%          step=300,      % integer() :: Step for data collection
%          heartbeat=600, % integer() :: Heartbeat
         }).

%%--------------------------------------------------------------------
%% A single command in a data input with definitions of how to parse
%% the return values, naming of those field and the definition of
%% how long to store each value.
%%--------------------------------------------------------------------
-record(dc_in_cmd,{
	  type,          % atom() :: rpc * NYI: snmpv1,snmpv2,...
          cmd,           % tuple() :: 
          parse,         % fun() | undefined :: 
          field          % [record(dc_in_cmd_field)]
         }).

-record(dc_in_cmd_field,{
	  uid,
          title,
          type=counter,
          min=0,
          max=1000
         }).

%%--------------------------------------------------------------------
%% A RRA is a format for storing data in RRD fashion that describe
%% the timespan, consolidation and grainularity of the stored
%% values.
%%--------------------------------------------------------------------
-record(dc_ts,{
	  id,            % integer() :: Key
          interval,      % integer() :: Milli seconds between 2 buckets
          buckets,       % integer() :: Number of time slots
          consolidation, % [atom()]  :: [average,min,max,last]
          description,   % string()  :: Friendly description

          time_series=[1,2,3,4] % [integer()] :: References to time series db



	  %% Remove ?
%          xfactor,       % float()   :: 
%          timespan       % integer() :: Seconds to show in graph
         }).

%%--------------------------------------------------------------------
%% Is a device definition where a device where a device is
%% a specific type of device. The most common definition is a
%% single erlang node where dc_dev.type equals {node,node()}.
%%--------------------------------------------------------------------
-record(dc_dev,{
	  id,            % integer() :: Key
          type,          % {atom(),atom()|string()} :: Node type and name
          description,   % string()  :: Friendly description
          access,        % [term()]  :: Access methods for the device
          alive,         % bool()    :: True if recently polled as alive
          tags=[],       % [{atom(),term()}] :: Tags on this device (fm_group etc)
          opt=[]         % [{atom(),term()}] :: Optional Values
	 }).

%%--------------------------------------------------------------------
%% Datasource that connects a device and data input.
%% A DataSource is a way to collect and parse one sort of
%% values. The DS needs to connect a RRA to each value that
%% shall be stored in RRD fashion, AND a device/access method
%% to be able to fetch the information.
%% A DS can therefore be reused for a number of devices.
%%--------------------------------------------------------------------
-record(dc_ds,{
	  id,               % integer() :: Key
          title=?DS_TITLE,  % {string(),[{K,V}]} :: Name of datasource 
          device,           % integer() :: Key for the device
          input,            % integer() :: Key for the input source
	  type,             % (pushed, polled, computed) 
	  tables,           % list() [integer()] :: Keys for times series

          poll,             % integer() :: Polling interval (seconds)
          active,           % bool()    :: Is the datasource active
          opt=[]            % [{K,V}]   :: Options
         }).

%%--------------------------------------------------------------------
%%
%%--------------------------------------------------------------------
-record(dc_ds_update,{
	  id,       % integer()  :: Id of datasource
	  time,     % string()   :: Epoch date as string() or "N"
	  status,   % atom()     :: complete | incomplete
	  fields,   % [string()] :: Fields to be updated
	  data      % [float()]  :: Data to be inserted
	 }).

%%--------------------------------------------------------------------
%% A graph is a presentation of collected data from different
%% Datasources.
%%--------------------------------------------------------------------
-record(dc_graph,{
	  id,             % integer()                   :: Key
          title,          % string()                    :: Title of the graph
          description,    % string()                    :: Description of Graph
          vlabel,         % string()                    :: Vertical Label
          ds,             % [record(dc_graph_def)|  :: Def or CDef
	                  %  record(dc_graph_cdef)] :: 
          item,           % [record(dc_graph_item)] :: Items like lines, areas
          tmpl,           % integer()                   :: Key for used graph_tmpl
          cmd,            % string()                    :: Graph command
          displayId=[],   % [integer()]                 :: Graph Tree Position
          images=[],      % [record(dc_graph_tag)]  :: The images
          graphed_ts,     % now()                       :: Latest Graphed
          ttl=?GRAPH_TTL, % seconds()                   :: Time until forced regraph
          opt=[]          % [{atom(),term()}]           :: Optional Values
         }).

%%--------------------------------------------------------------------
%% DataSource for graphing
%%--------------------------------------------------------------------
-record(dc_graph_def,{
	  id,
	  ds,             
	  field,          
	  cf,
	  step
	 }).

%%--------------------------------------------------------------------
%% Computated data field for graphing
%%--------------------------------------------------------------------
-record(dc_graph_cdef,{
	  id,           % integer() 
	  cmd           % string() :: TODO: Verify
	 }).

%%--------------------------------------------------------------------
%% Contains an already generated graph image
%%--------------------------------------------------------------------
-record(dc_graph_tag,{
	  inv,          % integer()|{Ts,Ts} :: 
          img_name,     % string()          :: 
          cmd,          % string()          :: 
          title,        % string()          :: 
          ts            % now()             :: 
         }).

%%--------------------------------------------------------------------
%% An item in a graph such as a line, area with corresponding attr
%%--------------------------------------------------------------------
-record(dc_graph_item,{
	  id,           % integer() ::
          ds,           % integer() ::
          field,        %
          title,        % string()
          color,        %
          type,         % 
          stack,        % bool()    :: 
          cf,           % string()  :: 
          cdef,         % 
          value,        %
          legend,       % string()  :: 
          hr,           %
	  visible,      % bool()    :: 
          sequence,     % integer() :: 
          opt=[]        % kvlist()  ::
         }).


-record(dc_graph_ds,{
	  stack,     % (bool) Stack multiple data sources
	  con_fun,   % (min | max | average | last) Consolidation function 
	  plot_type, % ({line,Width} | area ) How data is plotted
	  data_src   %
	 }).


%%--------------------------------------------------------------------
%% Container for exported data from a rrdtool database
%%--------------------------------------------------------------------
-record(dc_xport,{
	  id,      % integer()  :: 
          start,   % integer()  ::
          'end',   % integer()  ::
          step,    % integer()  ::
          rows,    % integer()  :: 
          columns, % integer()  ::
          legend,  % [record(dc_graph_item)] ::
          data=[], % [Data]     ::
          opt=[]   % kvlist()   ::
         }).

%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
-record(dc_mon,
        { id,
          desc="No Description",
          severity=indeterminate,
          mode=one, % one | all
          rule=[]
         }).


%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
-record(dc_mon_rule,
        { op,
          item         
         }).

%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
-record(dc_snmp_var,
        { oid,
          name,
          type,
          value
         }).


%%--------------------------------------------------------------------
%% 
%%--------------------------------------------------------------------
-record(dc_ds_poll,
        { id,
          status,
          ts,
          lat,
          ds,
          data,
          opt=[]
         }).


