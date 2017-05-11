-define(APP_NAME,data_col).

-record(dc_cfg_data,{
	  devices=[],
	  input=[],
	  datasources=[],
	  graphs=[],
	  time_series=[]
	 }).
