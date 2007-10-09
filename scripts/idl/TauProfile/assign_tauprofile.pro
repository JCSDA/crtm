;--------------------------------------------
FUNCTION Assign_TauProfile, TauProfile_In,  $  ; Input
                            TauProfile_Out     ; Output
;--------------------------------------------

  ; Set up
  ; ------
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

  ; ALL *input* pointers must be associated
  IF ( Associated_TauProfile( TauProfile_In ) EQ 0 ) THEN $
    MESSAGE, 'Some or all INPUT TauProfile pointer members are NOT associated.', $
             /NONAME, /NOPRINT

  ; ALL *output* pointers must NOT be associated
  IF ( Associated_TauProfile( TauProfile_Out, /ANY_Test ) EQ 1 ) THEN $
    MESSAGE, 'Some or all OUTPUT TauProfile pointer members are associated.', $
             /NONAME, /NOPRINT


  ; Allocate the structure
  ; ----------------------
  Result = Allocate_TauProfile( TauProfile_In.n_Layers,        $  ; Input
                                TauProfile_In.n_Channels,      $  ; Input
                                TauProfile_In.n_Angles,        $  ; Input
                                TauProfile_In.n_Profiles,      $  ; Input
                                TauProfile_In.n_Molecule_Sets, $  ; Input
                                TauProfile_Out                 )  ; Output
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating output TauProfile structure', $
             /NONAME, /NOPRINT


  ; Perform the assignment
  ; ----------------------
  ; Assign scalar members
  TauProfile_out.Release          = TauProfile_in.Release
  TauProfile_out.Version          = TauProfile_in.Version
  TauProfile_out.Sensor_ID        = TauProfile_in.Sensor_ID
  TauProfile_out.WMO_Satellite_ID = TauProfile_in.WMO_Satellite_ID
  TauProfile_out.WMO_Sensor_ID    = TauProfile_in.WMO_Sensor_ID

  ; Assign array data
  *TauProfile_out.Level_Pressure = *TauProfile_in.Level_Pressure
  *TauProfile_out.Channel        = *TauProfile_in.Channel
  *TauProfile_out.Angle          = *TauProfile_in.Angle
  *TauProfile_out.Profile        = *TauProfile_in.Profile
  *TauProfile_out.Molecule_Set   = *TauProfile_in.Molecule_Set
  *TauProfile_out.Tau            = *TauProfile_in.Tau

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Assign_TauProfile
