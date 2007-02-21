;+
; Function to deallocate the pointer
; components of an SRF structure

FUNCTION Destroy_SRF, SRF, $                ; Output
                      No_Clear = No_Clear   ; Input keyword
;-
 
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
 
  ; Check the structure
  IF ( Is_A_SRF_Structure( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
 
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET( No_Clear ) ) THEN BEGIN
    ZERO = 0.0d0
    SRF.n_Points         = 0
    SRF.Sensor_Name      = ' '
    SRF.Platform_Name    = ' '
    SRF.NCEP_Sensor_Id   = -1
    SRF.WMO_Satellite_Id = 1023
    SRF.WMO_Sensor_Id    = 2047
    SRF.Channel          = 0
    SRF.Begin_Frequency  = ZERO
    SRF.End_Frequency    = ZERO
    SRF.Integrated_SRF   = ZERO
    SRF.Summation_SRF    = ZERO
  ENDIF

  ; If ALL pointer members are NOT associated, do nothing
  IF ( Associated_SRF(SRF) EQ FALSE ) THEN GOTO, Done

  ; Deallocate the pointer members
  ;
  ; Deallocate the Frequency array
  IF ( PTR_VALID( SRF.Frequency ) ) THEN BEGIN
    PTR_FREE, SRF.Frequency
    SRF.Frequency = PTR_NEW()
  ENDIF
  ; Deallocate the Response array
  IF ( PTR_VALID( SRF.Response ) ) THEN BEGIN
    PTR_FREE, SRF.Response
    SRF.Response = PTR_NEW()
  ENDIF

  ; Decrement and test allocation counter
  SRF.n_Allocates = SRF.n_Allocates - 1
  IF ( SRF.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM( SRF.n_Allocates, 2 ), $
             /NONAME, /NOPRINT

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Destroy_SRF
