;+
;
; NAME:
;       CrIS_BandName
;
; PURPOSE:
;       Pure function to return the CrIS band name string.
;
; CALLING SEQUENCE:
;       name = CrIS_BandName(band)
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
;       name:     String containing the CrIS band name.
;                 UNITS:      N/A
;                 TYPE:       CHARACTER(*)
;                 DIMENSION:  Scalar
;-

FUNCTION CrIS_BandName, band
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1
  RETURN, BAND_NAME[ib]

END
