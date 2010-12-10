;+
; NAME:
;       Atmosphere::Init
;
; PURPOSE:
;       The Atmosphere::Init function method initialises an Atmosphere object.
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
;       Obj = OBJ_NEW( 'Atmosphere', Debug=Debug )
;
;         or
;
;       Result = Obj->[Atmosphere::]Init( Debug=Debug )  (In a lifecycle method only)
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
;       atmosphere_parameters: Include file for atmosphere specific parameters.
;
;       atmosphere_func_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 10-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Atmosphere::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_func_err_handler
 

  ; Initialise
  self.Is_Allocated = FALSE
  self.n_Layers     = 0L
  self.n_Absorbers  = 0L
  self.n_Clouds     = 0L
  self.n_Aerosols   = 0L
  self.Climatology  = US_STANDARD_ATMOSPHERE
 
  RETURN, TRUE
 
END
