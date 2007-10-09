;--------------------------------------
FUNCTION Destroy_TauProfile, TauProfile ; Output
;--------------------------------------

  ; Set up
  ; ------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Reset the dimension indicators
  TauProfile.n_Layers        = 0L
  TauProfile.n_Channels      = 0L
  TauProfile.n_Angles        = 0L
  TauProfile.n_Profiles      = 0L
  TauProfile.n_Molecule_Sets = 0L

    
  ; Clear the scalar members
  ; ------------------------
  Clear_TauProfile, TauProfile


  ; Free the pointer members
  ; ------------------------
  PTR_FREE, TauProfile.Level_Pressure, $
            TauProfile.Channel       , $
            TauProfile.Angle         , $
            TauProfile.Profile       , $
            TauProfile.Molecule_Set  , $
            TauProfile.Tau


  ; Decrement and test allocation counter
  ; -------------------------------------
  TauProfile.n_Allocates = TauProfile.n_Allocates - 1
  IF ( TauProfile.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM( TauProfile.n_Allocates, 2 ), $
             /NONAME, /NOPRINT

  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Destroy_TauProfile
