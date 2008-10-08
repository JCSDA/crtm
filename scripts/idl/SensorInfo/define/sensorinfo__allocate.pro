;+
; NAME:
;       SensorInfo::Allocate
;
; PURPOSE:
;       The SensorInfo::Allocate function method allocates the SensorInfo
;       object data arrays.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Allocate( n_Channels, $  ; Input
;                                             Debug=Debug )  ; Input keyword
;
; INPUTS:
;       n_Channels:  The number of channels to which the SensorInfo
;                    data arrays are to be allocated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the computation was sucessful
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
;       it can be allocated to the required number of channels, in this
;       example 25:
;
;         IDL> n_Channels = 25
;         IDL> Result = Obj->Allocate(n_Channels)
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Allocate, n_Channels, $  ; Input
                               Debug=Debug    ; Input keyword

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
