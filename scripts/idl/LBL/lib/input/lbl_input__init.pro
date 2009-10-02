;+
; NAME:
;       LBL_Input::Init
;
; PURPOSE:
;       The LBL_Input::Init function method initialises an LBL_Input object.
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
;       Obj = OBJ_NEW( 'LBL_Input', $
;                      [Filename, $] 
;                      lbl_type = lbl_type, $
;                      Debug    = Debug )
;
;         or, in a lifecycle method,
;
;       Result = Obj->[AtmProfile::]Init( $
;         [Filename, $] 
;         lbl_type = lbl_type, $
;         Debug    = Debug )
;
; OPTIONAL INPUT ARGUMENTS:
;       Filename:    Specify this argument to set the filename
;                    associated with the LBL_Input object. If not
;                    supplied, the default value is "lbl_input.txt"
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       lbl_type:    Integer flag used to define the type of LBL input
;                    file. Valid values are defined in the lbl_parameters
;                    include file.
;                    If not specified, the default value is that for ASCII.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the object
;                    initialisation status. The return codes are defined
;                    in the error_codes include file.
;                    If == TRUE the object creation was sucessful
;                       == FALSE an unrecoverable error occurred
;                    
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       lbl_parameters: Include file containing lbl specific
;                       parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an LBL_Input object is created,
;
;         IDL> x = OBJ_NEW('LBL_Input', 'TAPE5')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION LBL_Input::Init, $
  Filename, $
  Debug    = Debug   , $  ; Input keyword
  lbl_type = lbl_type, $  ; Input keyword
  _EXTRA   = ignore

  ; Set up
  COMPILE_OPT HIDDEN
  @lbl_parameters
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
  ; ..Check input
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = 'lbl_input.txt'
  IF ( N_ELEMENTS(lbl_type) GT 0 ) THEN BEGIN
    _lbl_type = LONG(lbl_type[0])
    IF ( _lbl_type LT 0 OR _lbl_type GT N_LBL_TYPES-1 ) THEN $
      MESSAGE, 'Invalid LBL type!', $
        NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF ELSE $
    _lbl_type = ASCII_LBL_TYPE
 

  ; Set subclass values
  self.filename = _Filename


  ; Set superclass values
  self->Set_Property, lbl_type = _lbl_type
  
  
  ; Done
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION LBL_Input::Init
