;+
;
; NAME:
;       CrIS_X
;
; PURPOSE:
;       Pure function to compute the CrIS double-sided optical delay grid.
;
; CALLING SEQUENCE:
;       x = CrIS_X(band, n, nominal=nominal)
;
; INPUTS:
;       band:      CrIS band number (1, 2, or 3).
;                  If band < 1, then 1 is used.
;                     band > 3, then 3 is used.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  SCALAR
;                  ATTRIBUTES: INTENT(IN)
;
;       n:         The number of points in the double-sided interferogram
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN)
;
; OPTIONAL INPUTS:
;       nominal:  Set this argument to return the nominal value of the CRIS
;                 max. OPD rather than the resampled max. OPD..
;                 If NOT SET, the resampled value is returned, [DEFAULT]
;                    SET,     the nominal value is returned
;                 If not specified, the resampled value of maxX is returned.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  Scalar
;                 ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       x:        CrIS double-sided optical delay grid.
;                 UNITS:      Centimetres (cm)
;                 TYPE:       DOUBLE
;                 DIMENSION:  Rank-1 (n)
;
; COMMENTS:
;       The output array looks like,
;
;          X=dx-maxX     X=0cm           X=maxX
;              |         (ZPD)             |
;              |           |               |
;              v           v               v
;
;              x   x   x   #   o   o   o   o
;
;                      --->|   |<---
;                           dx
;-

FUNCTION CRIS_X, band, n, nominal=nominal

  ; Create the grid array
  X = DBLARR(n)
  n_half = n/2
  X[n_half-1:n-1] = DINDGEN(n_Half+1)/DOUBLE(n_half)
  X[0:n_half-2]   = REVERSE(-X[n_half:n-2])
  
  ; Compute ptical delay
  RETURN, X * CrIS_MaxX(band, nominal=nominal)
  
END
