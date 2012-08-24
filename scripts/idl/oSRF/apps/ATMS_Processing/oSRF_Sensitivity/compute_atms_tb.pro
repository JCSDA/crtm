;+
; Function to compute MonoRTM brightness temperatures
; for ATMS.
;
; Returns the array of channel brightness temperatures for
; the atmospheric profile defined in the LBL input file.
;
; A SensorInfo file is read to obtain the information for the
; specified Sensor_Id.
;
FUNCTION compute_atms_tb, $
  Sensor_Id                  , $ ; Input
  MonoRTM_InFile             , $ ; Input (generic input file with no frequencies)
  SRF_File                   , $ ; Input
  Channel_List = channel_list    ; Input keyword (Default = [1,2,...,n_Channels] from SensorInfo)
                                 ;   Default is returned if keyword is empty on input
;-
                         
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
  
  
  ; Get the sensorinfo
  ; ...Determine if SensorInfo file is available
  SensorInfo_File = "SensorInfo"
  IF ( NOT (FILE_INFO(SensorInfo_File)).EXISTS ) THEN $
    MESSAGE, "SensorInfo file not found", /NONAME, /NOPRINT
  ; ...Read the list
  list = OBJ_NEW("SensorInfo_List", SensorInfo_File)
  list->Read
  ; ...Get the SensorInfo node for the required sensor
  si = list->Get(Sensor_Id=Sensor_Id) 
  ; ...Get the channel list
  si->Get_Property, Sensor_Channel = Sensor_Channel  
  n_Channels = N_ELEMENTS(Sensor_Channel)
  ; ...Cleanup
  OBJ_DESTROY, list
 
  
  ; Check Channel limits
  IF ( N_ELEMENTS(Channel_List) GT 0 ) THEN BEGIN
    Sensor_Channel = Channel_List[SORT(Channel_List)]
    n_Channels = N_ELEMENTS(Sensor_Channel)
  ENDIF ELSE $
    Channel_List = Sensor_Channel ; For output

  
  ; Create array to hold output
  Tb = DBLARR(n_Channels)
  

  ; Extract all the oSRF objects from the file
  osrf_file = OBJ_NEW("OSRF_File", SRF_File, Debug=Debug)
  osrf_file->Read, Debug=Debug
  osrf = osrf_file->Get(Channel=Sensor_Channel, COUNT=n_Channels_Available, Debug=Debug)
  ; ...Check the all the channels were retrieved
  IF ( n_Channels_Available NE n_Channels ) THEN $
      MESSAGE, "Requested and retrieved channels are different!", /NONAME, /NOPRINT
    
  
  
  ; Check that oSRF Sensor_Id agrees with Sensor_Id
  ;.......
  
  
  ; Begin channel computations
  FOR l=0, n_Channels - 1 DO BEGIN    
    
    ; Get the channel number for the current osrf
    osrf[l]->Get_Property, Channel=Channel, Debug=Debug
    PRINT, FORMAT='(9x,"Processing channel #",i5)', Channel
    
    ; Apply the SRF to the MonoRTM radiances
    osrf[l]->Apply_to_LBL, MonoRTM_Infile, Debug=Debug

    ; Get the resultant brightness temperature
    osrf[l]->Get_Property, Convolved_T=T, Debug=Debug

    ; Save it in the output file in the correct slot
    idx = WHERE( Sensor_Channel EQ Channel, count )
    IF ( count GT 0 ) THEN Tb[idx[0]] = T
    
  ENDFOR
  
  ; Cleanup
  OBJ_DESTROY, osrf_file
  
  
  ; Done
  RETURN, Tb
  
END
