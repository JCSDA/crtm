;+
;
; NAME:
;       CrIS_Channels
;
; PURPOSE:
;       Pure function to compute the channel numbers for an CrIS band.
;
; CALLING SEQUENCE:
;       ch = CrIS_Channels(band, include_guard_channels=include_guard_channels)
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
;       ch:                      The channel numbers for the specified CrIS band.
;                                UNITS:      N/A
;                                TYPE:       INTEGER
;                                DIMENSION:  Rank-1
;
;-

FUNCTION CrIS_Channels, band, include_guard_channels=include_guard_channels

  ; Select channel limits
  n_channels = CrIS_nPts(band, include_guard_channels=include_guard_channels)
  i1 = CrIS_BeginChannel(band, include_guard_channels=include_guard_channels)

  ; Construct channel array
  RETURN, LINDGEN(n_channels) + i1

END
