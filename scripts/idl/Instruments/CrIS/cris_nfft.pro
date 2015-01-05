;+
;
; NAME:
;       CrIS_nFFT
;
; PURPOSE:
;       Pure function to return the number of double-sided FFT points
;       for a CrIS instrument band.
;
; CALLING SEQUENCE:
;       n = CrIS_nFFT(band)
;
; INPUTS:
;       band:     CrIS band number (1, 2, or 3).
;                 If band < 1, then 1 is used.
;                    band > 3, then 3 is used.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  SCALAR
;                 ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       n:        Number of double-sided FFT points for the specified
;                 CrIS band.
;                 UNITS:      N/A
;                 TYPE:       INTEGER
;                 DIMENSION:  Scalar
;
;-

FUNCTION CrIS_nFFT, band
  @cris_parameters
  ib = ((band < N_CRIS_BANDS) > 1) - 1
  RETURN, N_FFT[ib]
END
