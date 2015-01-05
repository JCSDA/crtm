;+
;
; NAME:
;       CrIS_F
;
; PURPOSE:
;       Pure function to compute the resampled frequency grid for an CrIS band.
;
; CALLING SEQUENCE:
;       f = CrIS_F(band, include_guard_channels=include_guard_channels)
;
; INPUTS:
;       band:                    CRIS band number (1, 2, or 3).
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
;       f:                       The spectral frequency grid for the specified
;                                band.
;                                UNITS:      Inverse centimetres (cm^-1)
;                                TYPE:       DOUBLE
;                                DIMENSION:  Rank-1
;
;-

FUNCTION CrIS_F, band, include_guard_channels=include_guard_channels

  ; Construct grid array
  n = CRIS_nPts(band, include_guard_channels=include_guard_channels)
  f = DINDGEN(n)/DOUBLE(n-1)

  ; Select begin and end frequency
  f1 = CrIS_BeginF(band, include_guard_channels=include_guard_channels)
  f2 = CrIS_EndF(  band, include_guard_channels=include_guard_channels)

  ; Compute frequency array
  f = f*(f2-f1) + f1
  RETURN, f
  
END
