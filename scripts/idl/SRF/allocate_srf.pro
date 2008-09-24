;+
; Function to allocate an SRF structure

FUNCTION Allocate_SRF, n_Points       , $  ; Input
                       SRF            , $  ; Output
                       n_Bands=n_Bands     ; Optional input
;-

  ; Generic SRF parameters
  @srf_parameters
  
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
 
  ; Check dimension input
  IF ( n_Points LT 1 ) THEN $
    MESSAGE, 'Input N_POINTS must be > 0.', $
             /NONAME, /NOPRINT
  IF ( N_ELEMENTS(n_Bands) EQ 0 ) THEN n_Bands=1
 
  ; Check the structure
  IF ( Is_A_SRF_Structure( SRF, /Quiet ) EQ TRUE ) THEN BEGIN

    ; Check if ANY pointers are already associated
    ; If they are, deallocate them but leave scalars.
    IF ( Associated_SRF( SRF, /ANY_Test ) EQ TRUE ) THEN BEGIN
      Result = Destroy_SRF( SRF, /No_Clear )
      IF ( Result NE SUCCESS ) THEN $
        MESSAGE, 'Error deallocating SRF pointer members.', $
                 /NONAME, /NOPRINT
    ENDIF
  ENDIF ELSE BEGIN

    ; SRF argument is not a SRF structure.
    ; So, make it one.
    SRF = {SRF}
  ENDELSE
 
 
  ; Perform the allocations 
  SRF.f1_Band   = PTR_NEW(DBLARR(n_Bands))
  SRF.f2_Band   = PTR_NEW(DBLARR(n_Bands))
  SRF.npts_Band = PTR_NEW(DBLARR(n_Bands))
  SRF.Frequency = PTR_NEW(DBLARR(n_Points))
  SRF.Response  = PTR_NEW(DBLARR(n_Points))
 
  ; Assign the dimensions
  SRF.n_Points = n_Points
  SRF.n_Bands  = n_Bands
 
  ; Increment and test allocation counter
  SRF.n_Allocates = SRF.n_Allocates + 1
  IF ( SRF.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(SRF.n_Allocates,2), $
             /NONAME, /NOPRINT
 
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION Allocate_SRF
