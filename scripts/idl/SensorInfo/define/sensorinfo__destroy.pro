;+
; Procedure to deallocate the pointer
; components of a SensorInfo structure

FUNCTION SensorInfo::Destroy, No_Clear=No_Clear, $  ; Input keyword
                              Debug=Debug           ; Input keyword
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

 
  ; Reinitialise the dimensions
  self.n_Channels = 0
  
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.Sensor_Name      = ' '
    self.Satellite_Name   = ' '
    self.Sensor_Id        = ' '
    self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    self.Microwave_Flag   = INVALID
    self.Sensor_Type      = INVALID_SENSOR
  ENDIF

  ; If ALL pointer members are NOT associated, do nothing
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN GOTO, Done

  ; Deallocate the pointer members and nullify
  PTR_FREE, self.Sensor_Channel, $
            self.Use_Flag      , $
            self.Noise         
  self.Sensor_Channel = PTR_NEW()
  self.Use_Flag       = PTR_NEW()
  self.Noise          = PTR_NEW()

  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION SensorInfo::Destroy
