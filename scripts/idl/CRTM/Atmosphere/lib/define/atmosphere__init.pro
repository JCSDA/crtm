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
;-

FUNCTION Atmosphere::Init, Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_func_err_handler
 

  ; Create list objects
  self.Absorber_ID     = LIST()      
  self.Absorber_Units  = LIST()      
  self.Level_Pressure  = LIST()      
  self.Pressure        = LIST()      
  self.Temperature     = LIST()     
  self.Absorber        = LIST()    
  self.CFraction       = LIST()    
  self.Cloud           = Cloud_List()    
  self.Aerosol         = Aerosol_List()   
  
  ; Display in debug mode
  IF ( KEYWORD_SET(Debug) ) THEN self->Inspect, Debug=Debug
  
  ; Initialise
  self->Destroy, Debug=Debug
  RETURN, TRUE
 
END
