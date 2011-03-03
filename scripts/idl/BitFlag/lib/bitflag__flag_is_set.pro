;+
; NAME:
;       BitFlag::Flag_Is_Set
;
; PURPOSE:
;       The BitFlag::Flag_Is_Set function method determines if the specified bit
;       value(s) of the flags are set for this object.
;
; CALLING SEQUENCE:
;       result = Obj->[BitFlag::]Flag_Is_Set( $
;         Mask, $         ; Input     
;         Debug = Debug ) ; Input keyword
;         
; INPUTS:
;       Mask:                  Mask to use inquiring the bit flags.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       result:                Integer flag indicating if all the bit flags specified
;                              in the Mask input were set.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;
; INCLUDE FILES:
;       bitflag_parameters: Include file containing BitFlag specific
;                           parameter value definitions.
;
;       bitflag_func_err_handler: Error handler code for BitFlag functions.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION BitFlag::Flag_Is_Set, $
  Mask, $
  Debug=Debug
  
  ; Set up
  ; ...Set up error handler
  @bitflag_func_err_handler
  ; ...Check input
  _Mask = LONG(Mask[0])


  ; Get flag status
  result = self.value AND _Mask
  RETURN, result EQ _Mask
  
END ; FUNCTION BitFlag::Flag_Is_Set

