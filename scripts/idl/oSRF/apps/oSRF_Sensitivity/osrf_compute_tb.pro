;+
; Function to compute LBL brightness temperatures
; using the specified input SRF file.
;
; Returns the array of channel brightness temperatures for
; the atmospheric profile defined in the LBL input file.
;
FUNCTION oSRF_Compute_Tb, $
  SensorInfo                 , $ ; Input
  LBL_InFile                 , $ ; Input (generic input file with no frequencies)
  SRF_File                   , $ ; Input
  Channel_List = channel_list, $ ; Input keyword (Default = [1,2,...,n_Channels] from SensorInfo)
                                 ;   Default is returned if keyword is empty on input
  Debug = debug
;-
                         
  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_func_err_handler
  
  
  ; Get the sensor infomation
  SensorInfo->Get_Property, $
    Debug            = debug           , $
    Sensor_Id        = sensor_id       , $
    Sensor_Name      = sensor_name     , $
    Satellite_Name   = satellite_name  , $
    WMO_Satellite_ID = wmo_satellite_id, $
    WMO_Sensor_ID    = wmo_sensor_id   , $
    Sensor_Type      = sensor_type     , $
    Sensor_Channel   = sensor_channel
 
  
  ; Check Channel limits
  IF ( N_ELEMENTS(channel_list) GT 0 ) THEN BEGIN
    sensor_channel = channel_list[SORT(channel_list)]
  ENDIF ELSE $
    channel_list = sensor_channel ; For output
  n_channels = N_ELEMENTS(sensor_channel)

  
  ; Create array to hold output
  Tb = DBLARR(n_channels)
  

  ; Extract all the oSRF objects from the file
  osrf_file = OBJ_NEW("OSRF_File", SRF_File, Debug=debug)
  osrf_file->Read, Debug=debug
  osrf = osrf_file->Get(Channel=sensor_channel, COUNT=n_channels_available, Debug=debug)
  ; ...Check the all the channels were retrieved
  IF ( n_channels_available NE n_channels ) THEN $
      MESSAGE, "Requested and retrieved channels are different!", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    
  
  ; Check that oSRF Sensor_Id agrees with Sensor_Id
  osrf[0]->Get_Property, $
    Debug     = debug, $
    Sensor_Id = osrf_sensor_id
  IF ( STRTRIM(osrf_sensor_id,2) NE STRTRIM(sensor_id,2) ) THEN $
      MESSAGE, "oSRF Sensor_Id is different from SensorInfo Sensor_Id!", $
               /INFORMATIONAL

  
  ; Begin channel computations
  FOR l = 0, n_channels - 1 DO BEGIN    
    
    ; Get the channel number for the current osrf
    osrf[l]->Get_Property, Channel=channel, Debug=debug
    PRINT, FORMAT='(9x,"Processing channel #",i5)', channel
    
    ; Apply the SRF to the LBL radiances
    osrf[l]->Apply_to_LBL, LBL_Infile, Debug=debug

    ; Get the resultant brightness temperature
    osrf[l]->Get_Property, Convolved_T=T, Debug=debug

    ; Save it in the output file in the correct slot
    idx = WHERE( sensor_channel EQ channel, count )
    IF ( count GT 0 ) THEN Tb[idx[0]] = T
    
  ENDFOR
  
  ; Cleanup
  OBJ_DESTROY, osrf_file
  
  
  ; Done
  RETURN, Tb
  
END
