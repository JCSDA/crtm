;+
; NAME:
;       SensorInfo::Destroy
;
; PURPOSE:
;       The SensorInfo::Destroy function method deallocates and frees the
;       pointer components of a SensorInfo object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Destroy( No_Clear=No_Clear, $  ; Input keyword
;                                            Debug=Debug        )  ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       No_Clear:    Set this keyword to NOT reinitialise the scalar
;                    components to their default values. The default
;                    is to also clear the scalar values.
;                    If NOT SET => scalar components are reinitialised (DEFAULT)
;                       SET,    => scalar components are NOT reinitialised
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the deallocations were sucessful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       After creating a SensorInfo object,
;
;         IDL> x = OBJ_NEW('SensorInfo')
;
;       and allocating it,
;
;         IDL> Result = x->Allocate(10)
;
;       the object internals can be reinitialised like so,
;
;         IDL> Result = x->Destroy()
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Destroy, No_Clear=No_Clear, $  ; Input keyword
                              Debug=Debug           ; Input keyword
 
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
