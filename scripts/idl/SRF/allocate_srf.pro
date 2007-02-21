;+
; Function to allocate an SRF structure

FUNCTION Allocate_SRF, n_Points, $  ; Input
                       SRF          ; Output
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
 
 
  ; Check input
  n_Arguments = 2
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
  ; Check that required arguments are defined
  IF ( N_ELEMENTS( n_Points ) EQ 0 ) THEN $
    MESSAGE, 'Input SRF dimensions are not defined.', $
             /NONAME, /NOPRINT
  ; Check that dimensions are valid
  IF ( n_Points LT 1 ) THEN $
    MESSAGE, 'Input SRF dimensions must all be > 0.', $
             /NONAME, /NOPRINT
 
 
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
    SRF = { SRF }
  ENDELSE
 
 
  ; Perform the allocation 
  SRF.Frequency = PTR_NEW( DBLARR( n_Points ) )
  SRF.Response  = PTR_NEW( DBLARR( n_Points ) )
 
  ; Assign the dimensions
  SRF.n_Points = n_Points
 
  ; Increment and test allocation counter
  SRF.n_Allocates = SRF.n_Allocates + 1
  IF ( SRF.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM( SRF.n_Allocates, 2 ), $
             /NONAME, /NOPRINT
 
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION Allocate_SRF
