;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 2000
;
; NAME:
;       logticks_exp
;
; PURPOSE:
;       Function to print logarithmic axis tickmarks with exponential
;       output.
;
; CATEGORY:
;       Graphics
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       PLOT, x, y, /YLOG, YICKFORMAT = 'logticks_exp'
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
;       Written by:     Paul van Delst, CIMSS/SSEC, 08-Nov-2000
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION expticks_log, axis, index, value


; ---------------------------------------------------
; Construct the tickmark string based on the exponent
; ---------------------------------------------------

  tickmark = '10!E' + STRTRIM( STRING( value, FORMAT = '(f5.1)' ), 2 ) + '!N'


; ------------------------------------
; Return the formatted tickmark string
; ------------------------------------

  RETURN, tickmark

END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Id: expticks_log.pro,v 1.1 2005/09/16 21:22:28 paulv Exp $
; $Log: expticks_log.pro,v $
; Revision 1.1  2005/09/16 21:22:28  paulv
; Initial checkin.
;
;
;
;==============================================================================

