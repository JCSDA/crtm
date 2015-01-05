;+
; NAME:
;       IRwaterCoeff::Init
;
; PURPOSE:
;       The IRwaterCoeff::Init function method initialises an IRwaterCoeff object.
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
;       Obj = OBJ_NEW( 'IRwaterCoeff', Debug=Debug )
;
;         or
;
;       Result = Obj->[IRwaterCoeff::]Init( Debug=Debug )  (In a lifecycle method only)
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
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;-

FUNCTION IRwaterCoeff::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @irwatercoeff_parameters
  @irwatercoeff_func_err_handler

  ; Set object parameters
  self.Release = IRWATERCOEFF_RELEASE
  self.Version = IRWATERCOEFF_VERSION

  ; Create list objects
  self.Angle      = LIST()
  self.Frequency  = LIST()
  self.Wind_Speed = LIST()
  self.Emissivity = LIST()

  ; Display in debug mode
  IF ( KEYWORD_SET(Debug) ) THEN self->Inspect, Debug=Debug
  
  ; Initialise
  self->Destroy, Debug = Debug
  RETURN, TRUE
 
END
