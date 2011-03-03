;+
; NAME:
;       BitFlag::To_Array
;
; PURPOSE:
;       The BitFlag::To_Array function method converts the bitflags to an
;       byte array where the array index is the corresponding bit position.
;
; CALLING SEQUENCE:
;       result = Obj->[BitFlag::]To_Array( Debug = Debug )
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
;       result:                Array of flags corresponding to the bitflags.
;                                0 == NOT SET
;                                1 == SET
;                              UNITS:      N/A
;                              TYPE:       BYTE
;                              DIMENSION:  Rank-1
;
; INCLUDE FILES:
;       bitflag_func_err_handler: Error handler code for BitFlag functions.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 28-Sep-2010
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION BitFlag::To_Array, $
  Debug=Debug
  
  ; Set up
  @bitflag_func_err_handler

  n = 31
  flag_array = BYTARR(n)
  
  FOR i = 0L, n-1L DO BEGIN
    flag = 2L^i
    value = self.value AND flag
    IF ( value EQ 0 ) THEN CONTINUE
    flag_array[i] = 1B
  ENDFOR

  RETURN, flag_array
  
END ; FUNCTION BitFlag::To_Array

