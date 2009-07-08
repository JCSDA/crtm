;+
; NAME:
;       AtmProfile::Init
;
; PURPOSE:
;       The AtmProfile::Init function method initialises an AtmProfile object.
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
;       Obj = OBJ_NEW( 'AtmProfile', Debug=Debug )
;
;         or
;
;       Result = Obj->[AtmProfile::]Init( Debug=Debug )  (In a lifecycle method only)
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
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an AtmProfile object is created,
;
;         IDL> x = OBJ_NEW('AtmProfile')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION AtmProfile::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Object parameters
  @atmprofile_parameters
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
 

  ; Set scalar values
  self.Release           = ATMPROFILE_RELEASE
  self.Version           = ATMPROFILE_VERSION
  self.n_Levels          = 0L
  self.n_Layers          = 0L
  self.n_Absorbers       = 0L
  self.Profile           = 0L
  self.Description       = ''
  self.Climatology_Model = INVALID_CLIMATOLOGY
  self.Year              = IP_INVALID
  self.Month             = IP_INVALID
  self.Day               = IP_INVALID
  self.Hour              = IP_INVALID
  self.Latitude          = FP_INVALID
  self.Longitude         = FP_INVALID
  self.Surface_Altitude  = ZERO

  ; Done
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION AtmProfile::Init
