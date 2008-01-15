;+
;
; NAME:
;       ascii_indgen
;
; PURPOSE:
;       Procedure to create an ASCII string index vector
;
; CALLING SEQUENCE:
;       result = ASCII_Indgen(n,ISTART=i0,CSTART=c0)
;
; INPUTS:
;       n:   Number of vector elements to return.
;
; OPTIONAL INPUTS:
;       ISTART: ASCII decimal index at which to begin.
;               Default is 97.
;               This keyword is ignored if the CSTART
;               keyword is specified.
;
;       CSTART: ASCII character at which to begin.
;               Default is 'a'
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 15-Jan-2008
;                       paul.vandelst@noaa.gov
;
;-
FUNCTION ASCII_Indgen, n, ISTART=iStart, CSTART=cStart
  i0 = 97
  IF ( N_ELEMENTS(iStart) EQ 1 ) THEN i0 = (LONG(iStart))[0]
  IF ( N_ELEMENTS(cStart) EQ 1 ) THEN i0 = (BYTE(cStart))[0]
  b = INDGEN(n)+i0
  s = STRARR(n)
  FOR i = 0, n-1 DO s[i] = STRING(BYTE(b[i]))
  RETURN, s
END
