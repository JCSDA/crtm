;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 2000
;
; NAME:
;       timeticks
;
; PURPOSE:
;       Function to print time tickmarks in the format HH:MM
;
; CATEGORY:
;       Graphics
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       PLOT, decimal_time, y, XICKFORMAT = 'timeticks'
;
; INPUTS:
;       Not required for usage shown above.
;
;       axis:         the axis number. 0 for X axis, 1 for Y axis, 2 for Z axis.
;       index:        the tick mark index which starts at 0.
;       decimal_time: the decimal time in hours (a floating-point number);
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

FUNCTION timeticks, axis, index, decimal_time


; -------------------------------
; Determine the hours and minutes
; -------------------------------

  hour   = LONG( decimal_time )
  minute = LONG( decimal_time - FLOAT( hour ) ) * 60.0 )


; ------------------------------------
; Construct the tickmark format string
; ------------------------------------

  format = '( i2.2, ":", i2.2 )' )


; ------------------------------------
; Return the formatted tickmark string
; ------------------------------------

  RETURN, STRING( decimal_time, FORMAT = format )

END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Id: timeticks.pro,v 1.1 2000/05/16 17:51:43 paulv Exp $
; $Log: timeticks.pro,v $
; Revision 1.1  2000/05/16 17:51:43  paulv
; Initial checked in version
;
;
;==============================================================================

