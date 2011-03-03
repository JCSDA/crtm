;+
; NAME:
;       BitFlag::Set_Flag
;
; PURPOSE:
;       The BitFlag::Set_Flag procedure method sets the bit value(s) of the
;       flags property for this object.
;
; CALLING SEQUENCE:
;       Obj->[BitFlag::]Set_Flag, $
;         Mask, $
;         All   = All  , $ ; Input keyword
;         Debug = Debug    ; Input keyword
;         
; INPUTS:
;       Mask:                  Mask to use in setting the bit flags.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       All:                   Set this keyword to set ALL of the bit flags.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       bitflag_parameters: Include file containing BitFlag specific
;                            parameter value definitions.
;
;       bitflag_pro_err_handler: Error handler code for BitFlag functions.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO BitFlag::Set_Flag, $
  Mask, $
  Debug= Debug, $  ; Input keyword
  All  = All       ; Input keyword

  ; Set up
  ; ...BitFlag parameters
  @bitflag_parameters
  ; ...Set up error handler
  @bitflag_pro_err_handler
  ; ...Check keywords
  IF ( KEYWORD_SET(All) ) THEN BEGIN
    self.value = BITFLAG_SET_ALL
    RETURN
  ENDIF
  ; ...Check input
  _Mask = LONG(Mask[0])
 
  
  ; Set flags
  self.value = self.value OR _Mask

END ; PRO BitFlag::Set_Flag
