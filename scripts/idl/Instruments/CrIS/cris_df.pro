
;+
;
; NAME:
;       CrIS_dF
;
; PURPOSE:
;       Pure function to return the CrIS band frequency interval.
;
; CALLING SEQUENCE:
;       df = CrIS_dF(band)
;
; INPUTS:
;       band:     CrIS band number (1, 2, or 3).
;                 If Band < 1, then 1 is used.
;                    Band > 3, then 3 is used.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  Scalar
;                 ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       df:       Frequency interval for the CrIS band.
;                 UNITS:      Inverse centimetres (cm^-1)
;                 TYPE:       DOUBLE
;                 DIMENSION:  Scalar
;-

FUNCTION CrIS_dF, band
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1
  RETURN, D_FREQUENCY[ib]

END
