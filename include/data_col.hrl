% Default title for a datasource
-define( DS_TITLE,         {" %dev% - %input%",[]} ).

%%--------------------------------------------------------------------
%% A Data Input contains a serie of command that together collect
%% data to be used for statistics. The Data Input is applied on a
%% Device by creating a Datasource. The Datasource can be translated
%% as: Every X seconds, run this DataInput on Device and store the
%% return data for display later in i.e. Graph.
-record(dc_input,{
	  id,      % integer() Key
	  label,   % str() | [str()] Short description/Default label in graphs 
          input,   % [#dc_in_cmd{}] Collect method with parsing
          opt=[]   % [{atom(),term()}] Optional Values
         }).


%% A single command in a data input with definitions of how to parse
%% the return values, naming of those field and the definition of
%% how long to store each value.
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

%% A RRA is a format for storing data in RRD fashion that describe
%% the timespan, consolidation and grainularity of the stored
%% values.
%% -record(dc_ts,{
%% 	  id,            % integer() :: Key
%%           interval,      % integer() :: Milli seconds between 2 buckets
%%           buckets,       % integer() :: Number of time slots
%%           consolidation, % [atom()]  :: [average,min,max,last]
%%           description,   % string()  :: Friendly description

%%           time_series=[1,2,3,4] % [integer()] :: References to time series db
%%          }).

%% Is a device definition where a device is a specific type of device.
-record(dc_dev,{
	  id,            % integer() :: Key
          type,          % {atom(),atom()|string()} :: Node type and name
          description,   % string()  :: Friendly description
          access,        % [term()]  :: Access methods for the device
          alive,         % bool()    :: True if recently polled as alive
          tags=[],       % [{atom(),term()}] :: Tags on this device
          opt=[]         % [{atom(),term()}] :: Optional Values
	 }).

%% Datasource that connects a device and data input.
%% A DataSource is a way to collect and parse one sort of
%% values. The DS needs to connect a RRA to each value that
%% shall be stored in RRD fashion, AND a device/access method
%% to be able to fetch the information.
%% A DS can therefore be reused for a number of devices.
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

%%
-record(dc_ds_update,{
	  id,       % integer()  :: Id of datasource
	  time,     % string()   :: Epoch date as string() or "N"
	  status,   % atom()     :: complete | incomplete
	  fields,   % [string()] :: Fields to be updated
	  data      % [float()]  :: Data to be inserted
	 }).



