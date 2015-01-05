
;+
;
; NAME:
;       CrIS_EndF
;
; PURPOSE:
;       Pure function to return the CRIS band end frequency.
;
; CALLING SEQUENCE:
;       f2 = CrIS_EndF(band, include_guard_channels=include_guard_channels)
;
; INPUTS:
;       band:                    CrIS band number (1, 2, or 3).
;                                If Band < 1, then 1 is used.
;                                   Band > 3, then 3 is used.
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
;       f2:                      End frequency for the CrIS band.
;                                UNITS:      Inverse centimetres (cm^-1)
;                                TYPE:       DOUBLE
;                                DIMENSION:  Scalar
;-

FUNCTION CrIS_EndF, band, include_guard_channels=include_guard_channels
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  ; Retrieve the begin frequency
  IF ( KEYWORD_SET(include_guard_channels) ) THEN $
    f2 = BAND_GF2[ib] $
  ELSE $
    f2 = BAND_F2[ib]

  RETURN, f2

END
