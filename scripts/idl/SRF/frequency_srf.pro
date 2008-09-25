;+
; Function to compute the frequency grid
; for a supplied SRF data structure.

FUNCTION Frequency_SRF, SRF  ; Input
;-

  ; Generic SRF parameters
  @srf_parameters
  
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FALSE
  ENDIF
 
  ; Check input
  ; -----------
  IF ( Is_A_SRF_Structure( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
  IF ( Associated_SRF( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Some or all INPUT SRF pointer members are NOT associated.', $
             /NONAME, /NOPRINT
  ; Check the number of bands
  IF ( SRF.n_Bands LT 1 ) THEN $
    MESSAGE, 'SRF structure must contain at least 1 band.', $
             /NONAME, /NOPRINT
  ; Check the number of points
  IF ( SRF.n_Points LT 2 ) THEN $
    MESSAGE, 'SRF structure must contain at least 2 points.', $
             /NONAME, /NOPRINT
  ; Check the number of points in each band
  loc = WHERE(*SRF.npts_Band LT 2, count)
  IF ( count NE 0 ) THEN $
    MESSAGE, 'SRF must contain at least 2 points for each band.', $
             /NONAME, /NOPRINT
  ; Check the total points                                                   
  IF ( TOTAL(*SRF.npts_Band) NE SRF.n_Points ) THEN $
    MESSAGE, 'SRF must have consistent data points.', $
             /NONAME, /NOPRINT
             
  ; Compute the SRF frequency grid
  ; ------------------------------
  i2 = -1L
  ; Begin band loop
  FOR m = 0L, SRF.n_Bands-1L DO BEGIN
    ; Current band limits
    f1 = (*SRF.f1_Band)[m]
    f2 = (*SRF.f2_Band)[m]
    n  = (*SRF.npts_Band)[m]
    ; The point limits for this bands
    i1 = i2 + 1L
    i2 = i1 + n - 1L
    ; Construct a frequency grid of 0->1 for this band
    (*SRF.Frequency)[i1:i2] = DINDGEN(n)/DOUBLE(n-1L)
    (*SRF.Frequency)[i1:i2] = f1 + (*SRF.Frequency)[i1:i2]*(f2-f1)
  ENDFOR

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Frequency_SRF
