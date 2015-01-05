;+
;
; NAME:
;       CrIS_MaxX
;
; PURPOSE:
;       Pure function to return the CRIS maximum optical path delay.
;
; CALLING SEQUENCE:
;       maxX = CrIS_MaxX(band, nominal=nominal)
;
; INPUTS:
;       band:     CrIS band number (1, 2, or 3).
;                 If Band < 1, then 1 is used.
;                    Band > 3, then 3 is used.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  SCALAR
;                 ATTRIBUTES: INTENT(IN)
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
;       maxX:     Maximum optical path delay of the CRIS instrument.
;                 UNITS:      Centimetres (cm)
;                 TYPE:       DOUBLE
;                 DIMENSION:  Scalar
;-

FUNCTION CrIS_MaxX, band, nominal=nominal
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  IF ( KEYWORD_SET(nominal) ) THEN $
    maxX = NOMINAL_MAXX[ib] $
  ELSE $
    maxX = RESAMPLED_MAXX[ib]

  RETURN, maxX

END
