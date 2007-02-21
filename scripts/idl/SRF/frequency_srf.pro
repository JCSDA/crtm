;+
; Function to compute the frequency grid
; for a supplied SRF data structure.

FUNCTION Frequency_SRF, SRF  ; Input
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF
 
  ; Check input
  n_Arguments = 1
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
  IF ( Is_A_SRF_Structure( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
  IF ( Associated_SRF( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Some or all INPUT SRF pointer members are NOT associated.', $
             /NONAME, /NOPRINT
  IF ( SRF.n_Points LT 2 ) THEN $
    MESSAGE, 'Allocated SRF structure arrays must contain at least 2 points.', $
             /NONAME, /NOPRINT

  ; Construct a frequency grid of 0->1
  *SRF.Frequency = DINDGEN( SRF.n_Points ) / DOUBLE( SRF.n_Points - 1L )

  ; Scale it to the actual values
  *SRF.Frequency = SRF.Begin_Frequency + $
                   ( *SRF.Frequency * ( SRF.End_Frequency - SRF.Begin_Frequency ) )

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Frequency_SRF
