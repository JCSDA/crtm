;+
; NAME:
;       OSRF::Init
;
; PURPOSE:
;       The OSRF::Init function method initialises an OSRF object.
;
;       NOTE: Init methods are special *lifecycle methods* and, as
;             such, cannot be called outside the context of object
;             creation. This means that in most cases you cannot call
;             the Init method directly. There is one exception to this
;             rule: if you write your own subclass of this class, you
;             can call the Init method from within the Init method of
;             the subclass.
;
; CALLING SEQUENCE:
;       Obj = OBJ_NEW( 'OSRF', Debug=Debug )
;
;         or
;
;       Result = Obj->[OSRF::]Init( Debug=Debug )  (In a lifecycle method only)
;
; INPUT KEYWORD PARAMETERS:
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
;                    If == TRUE the object creation was sucessful
;                       == FALSE an unrecoverable error occurred
;                    
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an OSRF object is created,
;
;         IDL> x = OBJ_NEW('OSRF')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Init, Debug=Debug  ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
    MsgSwitch = 1
  ENDELSE
 

  ; Set default values
  self.Release          = OSRF_RELEASE
  self.Version          = OSRF_VERSION
  self.Sensor_Id        = ' '
  self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
  self.Sensor_Type      = INVALID_SENSOR
  self.Channel          = INVALID
  self.Integral         = ZERO
  self.Flags            = 0L
  self.f0               = ZERO
  self.Planck_C1        = ZERO
  self.Planck_C2        = ZERO
  self.Band_C1          = ZERO
  self.Band_C2          = ZERO

  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION OSRF::Init
