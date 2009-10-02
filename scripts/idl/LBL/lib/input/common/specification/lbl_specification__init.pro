;+
; NAME:
;       LBL_Specification::Init
;
; PURPOSE:
;       The LBL_Specification::Init function method initialises an LBL_Specification object.
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
;       Obj = OBJ_NEW( 'LBL_Specification', Debug=Debug )
;
;         or, in a lifecycle method,
;
;       Result = Obj->[LBL_Specification::]Init( Debug=Debug )
;
; KEYWORDS:
;       Along with any of the group of keywords accepted by the
;       LBL_Specification::Set_Property procedure method, the following
;       keywords are also accepted:
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
;       lbl_parameters: Include file containing LBL specific
;                       parameter value definitions.
;
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       The Init method is invoked when an LBL_Specification object is created,
;
;         IDL> x = OBJ_NEW('LBL_Specification')
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 21-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION LBL_Specification::Init, $
  Debug  = Debug, $
  _EXTRA = Properties  ; Keywords passed onto LBL_Specification::Set_Property

  ; Set up
  COMPILE_OPT HIDDEN
  @lbl_specification_parameters
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
 

  ; Set default values for public members
  self.v1         = ZERO
  self.v2         = ZERO
  self.sample     = ZERO
  self.dvset      = ZERO
  self.alfal0     = ZERO
  self.avmass     = ZERO
  self.dptmin     = ZERO
  self.dptfac     = ZERO
  self.ilnflg     = 0L
  self.dvout      = ZERO
  self.nmol_scale = 0L
  self.hmol_scale = REPLICATE(' ',LBL_N_MOLECULES_MAX)
  self.xmol_scale = ZERO


  ; Set supplied properties.
  self->Set_Property, _EXTRA = Properties, Debug = Debug
  

  ; Done  
  CATCH, /CANCEL
  RETURN, TRUE
 
END ; FUNCTION LBL_Specification::Init
