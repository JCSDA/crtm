;+
;
; NAME:
;       CrIS_BeginF
;
; PURPOSE:
;       Pure function to return the CRIS band begin frequency.
;
; CALLING SEQUENCE:
;       f1 = CrIS_BeginF(band, include_guard_channels=include_guard_channels)
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
;       f1:                      Begin frequency for the CrIS band.
;                                UNITS:      Inverse centimetres (cm^-1)
;                                TYPE:       DOUBLE
;                                DIMENSION:  Scalar
;-

FUNCTION CrIS_BeginF, band, include_guard_channels=include_guard_channels
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  ; Retrieve the begin frequency
  IF ( KEYWORD_SET(include_guard_channels) ) THEN $
    f1 = BAND_GF1[ib] $
  ELSE $
    f1 = BAND_F1[ib]

  RETURN, f1
  
END
