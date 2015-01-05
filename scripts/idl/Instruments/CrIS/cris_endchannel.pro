
;+
;
; NAME:
;       CrIS_EndChannel
;
; PURPOSE:
;       Pure function to return the CRIS band end channel number.
;
; CALLING SEQUENCE:
;       ch2 = CrIS_EndChannel(band, include_guard_channels=include_guard_channels)
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
;       ch2:                     End channel number for the CrIS band.
;                                UNITS:      N/A
;                                TYPE:       INTEGER
;                                DIMENSION:  Scalar
;-

FUNCTION CrIS_EndChannel, band, include_guard_channels=include_guard_channels
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  ; Retrieve the end channel number
  IF ( KEYWORD_SET(include_guard_channels) ) THEN $
    ch2 = END_GCHANNEL[ib] $
  ELSE $
    ch2 = END_CHANNEL[ib]

  RETURN, ch2

END
