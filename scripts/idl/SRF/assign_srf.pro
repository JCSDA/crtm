;+
; Function to copy an SRF structure

FUNCTION Assign_SRF, SRF_In,  $  ; Input
                     SRF_Out     ; Output
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

  ; The input structure
  ;
  ; Is the input an SRF structure?
  IF ( Is_A_SRF_Structure( SRF_In ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
  ; ALL *input* pointers must be associated
  IF ( Associated_SRF( SRF_In ) EQ FALSE ) THEN $
    MESSAGE, 'Some or all INPUT SRF pointer members are NOT associated.', $
             /NONAME, /NOPRINT

  ; The output structure
  ;
  ; Is the input an SRF structure?
  IF ( Is_A_SRF_Structure( SRF_Out ) EQ TRUE ) THEN BEGIN
    Result = Destroy_SRF( SRF_Out )
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error destroying OUTPUT SRF structure.', $
               /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    SRF = {SRF}
  ENDELSE


  ; Allocate the pointer members
  Result = Allocate_SRF( SRF_In.n_Points, $  ; Input
                         SRF_Out          )  ; Output
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating output SRF structure', $
             /NONAME, /NOPRINT

  ; Assign scalar members
  SRF_out.Sensor_Name   = SRF_in.Sensor_Name
  SRF_out.Platform_Name = SRF_in.Platform_Name
  SRF_out.NCEP_Sensor_Id   = SRF_in.NCEP_Sensor_Id
  SRF_out.WMO_Satellite_Id = SRF_in.WMO_Satellite_Id
  SRF_out.WMO_Sensor_Id    = SRF_in.WMO_Sensor_Id
  SRF_out.Channel          = SRF_in.Channel
  SRF_out.Begin_Frequency = SRF_in.Begin_Frequency
  SRF_out.End_Frequency   = SRF_in.End_Frequency
  SRF_out.Integrated_SRF = SRF_in.Integrated_SRF
  SRF_out.Summation_SRF  = SRF_in.Summation_SRF

  ; Assign array data
  *SRF_Out.Frequency = *SRF_In.Frequency
  *SRF_Out.Response  = *SRF_In.Response

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Assign_SRF
