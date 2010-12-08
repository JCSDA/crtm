PRO get_sensor_information, $
  SensorInfo_File  , $ ; Input
  Sensor_Id        , $ ; Input
  Sensor_Channel   , $ ; Output 
  Sensor_Type      , $ ; Output 
  WMO_Satellite_Id , $ ; Output 
  WMO_Sensor_Id    , $ ; Output 
  n_channels       
  
  ; Get the sensorinfo list data
  ; ...Determine if SensorInfo file is available
  finfo = FILE_INFO(SensorInfo_File)
  IF ( NOT finfo.EXISTS ) THEN $
    MESSAGE, "SensorInfo file "+SensorInfo_File+" not found.", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Read the list
  sinfo_list = OBJ_NEW("SensorInfo_List", SensorInfo_File, Debug = Debug)
  sinfo_list->Read, Debug = Debug
  ; ...Read the SensorInfo information
  sinfo = sinfo_list->Get(Sensor_Id = Sensor_Id, Debug = Debug)
  sinfo->Get_Property, Debug=Debug, $
    Sensor_Channel   = sensor_channel  , $
    Sensor_Type      = sensor_type     , $
    WMO_Satellite_Id = wmo_satellite_id, $ 
    WMO_Sensor_Id    = wmo_sensor_id   
  n_channels = N_ELEMENTS(sensor_channel)
  
END 
