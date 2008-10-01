;+
; Function to set the values in a SensorInfo structure

FUNCTION SensorInfo::Set, Sensor_Name     =Sensor_Name     , $  ; Input keyword 
                          Satellite_Name  =Satellite_Name  , $  ; Input keyword 
                          Sensor_Id       =Sensor_Id       , $  ; Input keyword 
                          WMO_Satellite_ID=WMO_Satellite_ID, $  ; Input keyword 
                          WMO_Sensor_ID   =WMO_Sensor_ID   , $  ; Input keyword 
                          Microwave_Flag  =Microwave_Flag  , $  ; Input keyword 
                          Sensor_Type     =Sensor_Type     , $  ; Input keyword 
                          Sensor_Channel  =Sensor_Channel  , $  ; Input keyword 
                          Use_Flag        =Use_Flag        , $  ; Input keyword 
                          Noise           =Noise           , $  ; Input keyword 
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

  ; Add data
  ; --------
  ; Scalars
  IF ( N_ELEMENTS(Sensor_Name     ) GT 0 ) THEN self.Sensor_Name      = Sensor_Name     
  IF ( N_ELEMENTS(Satellite_Name  ) GT 0 ) THEN self.Satellite_Name   = Satellite_Name  
  IF ( N_ELEMENTS(Sensor_Id       ) GT 0 ) THEN self.Sensor_Id        = Sensor_Id       
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Microwave_Flag  ) GT 0 ) THEN self.Microwave_Flag   = Microwave_Flag  
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type
  
  ; Arrays
  IF ( N_ELEMENTS(Sensor_Channel) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Sensor_Channel) EQ self.n_Channels ) THEN $
      *self.Sensor_Channel = Sensor_Channel $
    ELSE $
      MESSAGE, 'Size of Sensor_Channel input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
             
  IF ( N_ELEMENTS(Use_Flag) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Use_Flag) EQ self.n_Channels ) THEN $
      *self.Use_Flag = Use_Flag $
    ELSE $
      MESSAGE, 'Size of Use_Flag input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
             
  IF ( N_ELEMENTS(Noise) GT 0 ) THEN BEGIN
    IF ( N_ELEMENTS(Noise) EQ self.n_Channels ) THEN $
      *self.Noise = Noise $
    ELSE $
      MESSAGE, 'Size of Noise input inconsistent with n_Channels dimension.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Set
