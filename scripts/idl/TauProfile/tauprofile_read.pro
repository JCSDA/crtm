FUNCTION TauProfile_Read, TauProfile_File, $
                          TauProfile

  ; --------------------
  ; Set up error handler
  ; --------------------

  @error_codes

  CATCH, Error_Status

  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    


  ; -------------------
  ; Check file argument
  ; -------------------

  IF ( NOT Valid_String( TauProfile_File ) ) THEN $
    MESSAGE, 'No input file specified.', $
             /NONAME, /NOPRINT


  ; ------------------
  ; Read the data file
  ; ------------------

  PRINT, FORMAT = '( /5x, "Reading the TauProfile data file ", a, "...." )', $
                  STRTRIM( TauProfile_File, 2 )

  Error_Status = Read_NetCDF( TauProfile_File, TauProfile, /Quiet )

  IF ( Error_Status NE SUCCESS ) THEN $
    MESSAGE, 'Error reading TauProfile file ' + STRTRIM( TauProfile_File, 2 ), $
             /NONAME, /NOPRINT


  ; -------------------------------------
  ; Put the dimensions into the structure
  ; -------------------------------------

;  n_Layers        = LONG( N_ELEMENTS( TauProfile.Level_Pressure ) - 1 )
;  n_Channels      = LONG( N_ELEMENTS( TauProfile.Channel_List ) )
;  n_Angles        = LONG( N_ELEMENTS( TauProfile.Angle_List ) )
;  n_Profiles      = LONG( N_ELEMENTS( TauProfile.Profile_List ) )
;  n_Molecule_Sets = LONG( N_ELEMENTS( TauProfile.Molecule_Set_List ) )
;
;  TauProfile = PTR_NEW( CREATE_STRUCT( 'n_Layers',        n_Layers, $
;                                       'n_Channels',      n_Channels, $
;                                       'n_Angles',        n_Angles, $
;                                       'n_Profiles',      n_Profiles, $
;                                       'n_Molecule_Sets', n_Molecule_Sets, $
;                                       TEMPORARY( TauProfile ) ) )
                              

  ; ----
  ; Done
  ; ----

  CATCH, /CANCEL
  RETURN, SUCCESS

END


