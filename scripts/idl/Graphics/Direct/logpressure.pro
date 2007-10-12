;+
;
; NAME:
;       logpressure
;
; PURPOSE:
;       Function to print logarithmic pressures as regular pressure values.
;
; CATEGORY:
;       Graphics
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       PLOT, x, log(y), YICKFORMAT = 'logpressure'
;
; INPUTS:
;       axis:  the axis number. 0 for X axis, 1 for Y axis, 2 for Z axis.
;       index: the tick mark index which starts at 0.
;       value: the default tick mark value (a floating-point number);
;
; INPUT KEYWORD PARAMETERS:
;       None
;
; OUTPUTS:
;       Function returns a string containing the tick mark labels.
;
; OUTPUT KEYWORD PARAMETERS:
;       None
;
; CALLS:
;       None
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None
;
; RESTRICTIONS:
;       None
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 18-Aug-2003
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION logpressure, axis, index, value


  ; ------------------------------
  ; Detrermine the actual pressure
  ; ------------------------------

  Pressure = EXP( Value )


; ------------------------------
; Determine the base-e exponent
; ------------------------------

  exponent   = LONG(value) ;LONG( ALOG10( Pressure ) )


; ---------------------------------------------------
; Construct the tickmark string based on the exponent
; ---------------------------------------------------

  CASE 1 OF

;   -- Exponent is less than zero ->
;   -- fractional ticklabel
    ( exponent LT 0 ): format = '( f' + $
                                STRTRIM( ABS( exponent ) + 2, 2 ) + $
                                '.' + $
                                STRTRIM( ABS( exponent ), 2 ) + $
                                ' )'

;   -- Exponent is greater than or = to zero ->
;   -- whole number ticklabel
    ( exponent GE 0 ): format = '( i' + $
                                STRTRIM( ABS( exponent ) + 1, 2 ) + $
                                ' )'

  ENDCASE


; ------------------------------------
; Return the formatted tickmark string
; ------------------------------------
;print, value,pressure, exponent, format
;  RETURN, STRING( Pressure, FORMAT = format )
  RETURN, STRING( Pressure )

END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Id: logpressure.pro,v 1.1 2005/09/16 21:22:28 paulv Exp $
; $Log: logpressure.pro,v $
; Revision 1.1  2005/09/16 21:22:28  paulv
; Initial checkin.
;
;
;
;==============================================================================

