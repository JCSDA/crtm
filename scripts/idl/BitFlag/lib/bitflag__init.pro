;+
; NAME:
;       BitFlag::Init
;
; PURPOSE:
;       The BitFlag::Init function method initialises an BitFlag object.
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
;       Obj = OBJ_NEW( 'BitFlag', Debug=Debug )
;
;         or
;
;       Result = Obj->[BitFlag::]Init( Debug=Debug )  (In a lifecycle method only)
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
;       bitflag_parameters: Include file containing BitFlag specific
;                            parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an BitFlag object is created,
;
;         IDL> x = OBJ_NEW('BitFlag')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION BitFlag::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...BitFlag parameters
  @bitflag_parameters
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
  self.Release = BITFLAG_RELEASE
  self.Version = BITFLAG_VERSION
  self.value   = BITFLAG_CLEAR_ALL
  
  RETURN, TRUE
 
END
