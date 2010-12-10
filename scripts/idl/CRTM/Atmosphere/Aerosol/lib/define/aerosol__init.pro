;+
; NAME:
;       Aerosol::Init
;
; PURPOSE:
;       The Aerosol::Init function method initialises an Aerosol object.
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
;       Obj = OBJ_NEW( 'Aerosol', Debug=Debug )
;
;         or
;
;       Result = Obj->[Aerosol::]Init( Debug=Debug )  (In a lifecycle method only)
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
;       aerosol_parameters: Include file for aerosol specific parameters.
;
;       aerosol_func_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 09-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Aerosol::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @aerosol_func_err_handler
 

  ; Initialise
  self->Destroy, Debug = Debug
  RETURN, TRUE
 
END
