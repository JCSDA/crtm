;+
; Function to integrate the response
; in a supplied SRF data structure.

FUNCTION Integrate_SRF, SRF  ; Input
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
  ; Intialisation
  SRF.Integrated_SRF = ZERO
  SRF.Summation_SRF  = ZERO
  i2 = -1L
  ; Begin band loop
  FOR m = 0L, SRF.n_Bands-1L DO BEGIN
    ; The point limits for this band
    n  = (*SRF.npts_Band)[m]
    i1 = i2 + 1L
    i2 = i1 + n - 1L
    ; Integrate using Simpson's Rule
    Int_SRF = Integral((*SRF.Frequency)[i1:i2],(*SRF.Response)[i1:i2])
    ; Integrate by simply summation
    df = (*SRF.Frequency)[1] - (*SRF.Frequency)[0] 
    Sum_SRF = TOTAL((*SRF.Response)[i1:i2],/DOUBLE)*df
    ; Accumulate the band sums
    SRF.Integrated_SRF = SRF.Integrated_SRF + Int_SRF
    SRF.Summation_SRF  = SRF.Summation_SRF  + Sum_SRF
  ENDFOR

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Integrate_SRF
