;+
;
; NAME:
;       CrIS_nPts
;
; PURPOSE:
;       Pure function to compute the number of spectral points in an CrIS band.
;
; CALLING SEQUENCE:
;       n = CrIS_nPts(band, include_guard_channels=include_guard_channels)
;
; INPUTS:
;       band:                    CrIS band number (1, 2, or 3).
;                                If band < 1, then 1 is used.
;                                   band > 3, then 3 is used.
;                                UNITS:      N/A
;                                TYPE:       INTEGER
;                                DIMENSION:  Scalar
;                                ATTRIBUTES: INTENT(IN)
;
; OPTIONAL INPUTS:
;       include_guard_channels:  Set this keyword to include the guard
;                                channels on either side of the band.
;                                If NOT SET, no guard channels are used. [DEFAULT]
;                                   SET,     guard channels are used.
;                                If not specified, no guard channels are used.
;                                UNITS:      N/A
;                                TYPE:       INTEGER
;                                DIMENSION:  Scalar
;                                ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       n:                       Number of spectral points for the specified
;                                CrIS band.
;                                UNITS:      N/A
;                                TYPE:       INTEGER
;                                DIMENSION:  Scalar
;
;-

FUNCTION CrIS_nPts, band, include_guard_channels=include_guard_channels
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  ; Select channels numbers
  IF ( KEYWORD_SET(include_guard_channels) THEN $
    n = N_GCHANNELS_PER_BAND[ib] $
  ELSE $
    n = N_CHANNELS_PER_BAND[ib]

  RETURN, n
  
END
