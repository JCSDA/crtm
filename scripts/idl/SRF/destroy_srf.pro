;+
; Function to deallocate the pointer
; components of an SRF structure

FUNCTION Destroy_SRF, SRF, $                ; Output
                      No_Clear = No_Clear   ; Input keyword
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
 
  ; Check the structure
  IF ( Is_A_SRF_Structure( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
 
  ; Reinitialise the dimensions
  SRF.n_Points = 0
  SRF.n_Bands  = 0
  
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    SRF.Release          = SRF_RELEASE
    SRF.Version          = SRF_VERSION
    SRF.Sensor_Id        = ' '
    SRF.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    SRF.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    SRF.Sensor_Type      = INVALID_SENSOR
    SRF.Channel          = INVALID
    SRF.Integrated_SRF   = ZERO
    SRF.Summation_SRF    = ZERO
  ENDIF

  ; If ALL pointer members are NOT associated, do nothing
  IF ( Associated_SRF(SRF) EQ FALSE ) THEN GOTO, Done

  ; Deallocate the pointer members and nullify
  PTR_FREE, SRF.Frequency, $
            SRF.Response , $
            SRF.npts_Band, $
            SRF.f1_Band  , $
            SRF.f2_Band
  SRF.Frequency = PTR_NEW()
  SRF.Response  = PTR_NEW()
  SRF.npts_Band = PTR_NEW()
  SRF.f1_Band   = PTR_NEW()
  SRF.f2_Band   = PTR_NEW()

  ; Decrement and test allocation counter
  SRF.n_Allocates = SRF.n_Allocates - 1
  IF ( SRF.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(SRF.n_Allocates, 2), $
             /NONAME, /NOPRINT

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Destroy_SRF
