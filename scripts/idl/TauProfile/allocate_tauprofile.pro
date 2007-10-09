;-----------------------------------------------
FUNCTION Allocate_TauProfile, n_Layers,        $  ; Input
                              n_Channels,      $  ; Input
                              n_Angles,        $  ; Input
                              n_Profiles,      $  ; Input
                              n_Molecule_Sets, $  ; Input
                              TauProfile          ; Output
;-----------------------------------------------

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
  n_Arguments = 5
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT

  ; Check that required arguments are defined
  IF ( N_ELEMENTS( n_Layers        ) EQ 0 OR $
       N_ELEMENTS( n_Channels      ) EQ 0 OR $
       N_ELEMENTS( n_Angles        ) EQ 0 OR $
       N_ELEMENTS( n_Profiles      ) EQ 0 OR $
       N_ELEMENTS( n_Molecule_Sets ) EQ 0    ) THEN $
    MESSAGE, 'Input TauProfile dimensions are not defined.', $
             /NONAME, /NOPRINT

  ; Check that dimensions are valid
  IF ( n_Layers        LT 1 OR $
       n_Channels      LT 1 OR $
       n_Angles        LT 1 OR $
       n_Profiles      LT 1 OR $
       n_Molecule_Sets LT 1    ) THEN $
    MESSAGE, 'Input TauProfile dimensions must all be > 0.', $
             /NONAME, /NOPRINT

  ; Check association status of the
  ; input structure pointer members
  IF ( Associated_TauProfile( TauProfile, /ANY_Test ) EQ 1 ) THEN $
    MESSAGE, 'One or more TauProfile pointer members are associated.', $
             /NONAME, /NOPRINT


  ; Perform the allocation
  ; ----------------------
  TauProfile.Level_Pressure = PTR_NEW( DBLARR( n_Layers + 1    ) )
  TauProfile.Channel        = PTR_NEW( DBLARR( n_Channels      ) )
  TauProfile.Angle          = PTR_NEW( DBLARR( n_Angles        ) )
  TauProfile.Profile        = PTR_NEW( DBLARR( n_Profiles      ) )
  TauProfile.Molecule_Set   = PTR_NEW( DBLARR( n_Molecule_Sets ) )
  TauProfile.Tau            = PTR_NEW( DBLARR( n_Layers,       $
                                               n_Channels,     $
                                               n_Angles,       $
                                               n_Profiles,     $
                                               n_Molecule_Sets ) )

  ; Assign the dimensions
  ; ---------------------
  TauProfile.n_Layers        = n_Layers       
  TauProfile.n_Channels      = n_Channels     
  TauProfile.n_Angles        = n_Angles       
  TauProfile.n_Profiles      = n_Profiles     
  TauProfile.n_Molecule_Sets = n_Molecule_Sets


  ; Increment and test the allocation counter
  ; -----------------------------------------
  TauProfile.n_Allocates = TauProfile.n_Allocates + 1
  IF ( TauProfile.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM( TauProfile.n_Allocates, 2 ), $
             /NONAME, /NOPRINT

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Allocate_TauProfile
