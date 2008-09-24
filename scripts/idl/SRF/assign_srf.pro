;+
; Function to copy an SRF structure

FUNCTION Assign_SRF, SRF_In,  $  ; Input
                     SRF_Out     ; Output
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

  ; Is the input an SRF structure?
  IF ( Is_A_SRF_Structure( SRF_In ) EQ FALSE ) THEN $
    MESSAGE, 'Input structure is not a SRF structure', $
             /NONAME, /NOPRINT
  ; ALL *input* pointers must be associated
  IF ( Associated_SRF( SRF_In ) EQ FALSE ) THEN $
    MESSAGE, 'Some or all INPUT SRF pointer members are NOT associated.', $
             /NONAME, /NOPRINT

  ; Is the output an SRF structure?
  IF ( Is_A_SRF_Structure( SRF_Out ) EQ TRUE ) THEN BEGIN
    Result = Destroy_SRF( SRF_Out )
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error destroying OUTPUT SRF structure.', $
               /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    SRF_Out = {SRF}
  ENDELSE


  ; Allocate the pointer members
  Result = Allocate_SRF( SRF_In.n_Points, $
                         SRF_Out, $
                         n_Bands=SRF_In.n_Bands )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating output SRF structure', $
             /NONAME, /NOPRINT

  ; Assign data components
  SRF_out.Release = SRF_in.Release
  SRF_out.Version = SRF_in.Version
  
  SRF_out.Sensor_ID        = SRF_in.Sensor_ID  
  SRF_out.WMO_Satellite_Id = SRF_in.WMO_Satellite_Id
  SRF_out.WMO_Sensor_Id    = SRF_in.WMO_Sensor_Id
  SRF_out.Sensor_Type      = SRF_in.Sensor_Type   
  SRF_out.Channel          = SRF_in.Channel
  SRF_out.Integrated_SRF   = SRF_in.Integrated_SRF
  SRF_out.Summation_SRF    = SRF_in.Summation_SRF
  *SRF_out.f1_Band         = *SRF_in.f1_Band 
  *SRF_out.f2_Band         = *SRF_in.f2_Band 
  *SRF_out.npts_Band       = *SRF_in.npts_Band 
  *SRF_out.Frequency       = *SRF_in.Frequency
  *SRF_out.Response        = *SRF_in.Response

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Assign_SRF
