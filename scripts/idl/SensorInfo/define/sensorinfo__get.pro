;+
; Function to get the values from a SensorInfo structure

FUNCTION SensorInfo::Get, Sensor_Name     =Sensor_Name     , $  ; Output keyword 
                          Satellite_Name  =Satellite_Name  , $  ; Output keyword 
                          Sensor_Id       =Sensor_Id       , $  ; Output keyword 
                          WMO_Satellite_ID=WMO_Satellite_ID, $  ; Output keyword 
                          WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Output keyword 
                          Microwave_Flag  =Microwave_Flag  , $  ; Output keyword 
                          Sensor_Type     =Sensor_Type     , $  ; Output keyword 
                          Sensor_Channel  =Sensor_Channel  , $  ; Output keyword 
                          Use_Flag        =Use_Flag        , $  ; Output keyword 
                          Noise           =Noise           , $  ; Output keyword 
                          Debug=Debug  ; Input keyword
;-

  ; Set up
  ; ------
  ; Include SensorInfo parameters
  @sensorinfo_parameters
  
  ; error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FAILURE
    ENDIF
    MsgSwitch = 1
  ENDELSE
  
  ; Check if structure has been allocated
  IF ( self.n_Channels EQ 0 ) THEN $
    MESSAGE, 'n_Channels structure dimension is zero!', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Get data
  ; --------
  ; Scalars
  Sensor_Name      = self.Sensor_Name     
  Satellite_Name   = self.Satellite_Name  
  Sensor_Id        = self.Sensor_Id       
  WMO_Satellite_ID = self.WMO_Satellite_ID
  WMO_Sensor_ID    = self.WMO_Sensor_ID   
  Microwave_Flag   = self.Microwave_Flag  
  Sensor_Type      = self.Sensor_Type     
  
  ; Arrays
  Sensor_Channel = *self.Sensor_Channel
  Use_Flag       = *self.Use_Flag      
  Noise          = *self.Noise         
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Get
