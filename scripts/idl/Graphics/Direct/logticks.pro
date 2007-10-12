;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 2000
;
; NAME:
;       logticks
;
; PURPOSE:
;       Function to print logarithmic axis tickmarks with the minimum
;       number of decimal places.
;
; CATEGORY:
;       Graphics
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       PLOT, x, y, /YLOG, YICKFORMAT = 'logticks'
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
;       Written by:     Paul van Delst, CIMSS/SSEC, 10-May-2000
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION logticks, axis, index, value


; ------------------------------
; Determine the base-10 exponent
; ------------------------------

  exponent   = LONG( ALOG10( value ) )


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

  RETURN, STRING( value, FORMAT = format )

END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Id: logticks.pro,v 1.1 2000/05/16 17:51:43 paulv Exp $
; $Log: logticks.pro,v $
; Revision 1.1  2000/05/16 17:51:43  paulv
; Initial checked in version
;
;
;==============================================================================

