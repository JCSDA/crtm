;+
; NAME:
;       BitFlag::Clear_Flag
;
; PURPOSE:
;       The BitFlag::Clear_Flag procedure method clears the bit value(s) of the
;       flags property for this object.
;
; CALLING SEQUENCE:
;       Obj->[BitFlag::]Clear_Flag, $
;         Mask, $
;         All   = All  , $ ; Input keyword
;         Debug = Debug    ; Input keyword
;         
; INPUTS:
;       Mask:                  Mask to use in clearing the bit flags.
;                              Not required if the /ALL keyword is set.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       All:                   Set this keyword to clear ALL of the bit flags.
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
;                           parameter value definitions.
;
;       bitflag_pro_err_handler: Error handler code for BitFlag functions.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO BitFlag::Clear_Flag, $
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
    self.value = BITFLAG_CLEAR_ALL
    RETURN
  ENDIF
  ; ...Check input
  _Mask = LONG(Mask[0])


  ; Clear flags
  self.value = self.value AND ( NOT _Mask )
 
END ; PRO BitFlag::Clear_Flag
