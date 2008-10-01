;+
; Function to allocate a SensorInfo structure

FUNCTION SensorInfo::Allocate, n_Channels, $  ; Input
                               Debug=Debug    ; Input keyword
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
 
  ; Check dimension input
  IF ( n_Channels LT 1 ) THEN $
    MESSAGE, 'Input N_POINTS must be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  ; Check if ANY pointers are already associated
  ; If they are, deallocate them but leave scalars.
  IF ( self->Associated(/ANY_Test,Debug=Debug) EQ TRUE ) THEN BEGIN
    Result = self->Destroy(/No_Clear,Debug=Debug)
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error destroying SensorInfo structure', NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
 
  ; Perform the allocations 
  self.Sensor_Channel = PTR_NEW(LONARR(n_Channels))
  self.Use_Flag       = PTR_NEW(LONARR(n_Channels))
  self.Noise          = PTR_NEW(DBLARR(n_Channels))
 
  ; Assign the dimensions
  self.n_Channels = n_Channels
 
  ; Increment and test allocation counter
  self.n_Allocates = self.n_Allocates + 1
  IF ( self.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(self.n_Allocates,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION SensorInfo::Allocate
