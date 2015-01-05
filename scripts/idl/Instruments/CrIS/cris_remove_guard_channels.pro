;+
;
; NAME:
;       CrIS_Remove_Guard_Channels
;
; PURPOSE:
;       Function to remove the guard channels from input for a CrIS band.
;
; CALLING SEQUENCE:
;       output_vector = CrIS_Remove_Guard_Channels(band, input_vector)
;
; INPUTS:
;       band:            CrIS band number (1, 2, or 3).
;                        If band < 1, then 1 is used.
;                           band > 3, then 3 is used.
;                        UNITS:      N/A
;                        TYPE:       INTEGER
;                        DIMENSION:  Scalar
;                        ATTRIBUTES: INTENT(IN)
;
;       input_vector:    CrIS band-length vector for which the guard
;                        channel data is to be removed.
;                        UNITS:      Variable
;                        TYPE:       INTEGER or DOUBLE
;                        DIMENSION:  Rank-1
;                        ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       output_vector:   Same data as input_vector but with the guard
;                        channels removed.
;                        UNITS:      Variable
;                        TYPE:       Same as input_vector
;                        DIMENSION:  Rank-1
;
;-

FUNCTION CrIS_Remove_Guard_Channels, band, input_vector
  @cris_parameters

  ib = ((band < N_CRIS_BANDS) > 1) - 1

  ; Test input
  n = N_ELEMENTS(input_vector)
  IF ( n /= CrIS_nPts(band, /include_guard_channels) ) THEN $
    MESSAGE, 'Input vector size ('+STRTRIM(n,2)+') inconsistent for CrIS band '+STRTRIM(ib+1,2)


  ; Pick out the data
  i1 = N_GUARD_CHANNELS[1, ib] + 1
  i2 = n - N_GUARD_CHANNELS[2, ib]
  RETURN, input_vector[i1:i2]

END
