;--------------------------------------------------
FUNCTION Associated_TauProfile, TauProfile, $         ; Input
                                ANY_Test = ANY_Test   ; Input keyword
;--------------------------------------------------

  ; Set up
  ; ------
  @error_codes
  Association_Status = FALSE


  ; Test the association status
  ; ---------------------------
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( TauProfile.Level_Pressure ) AND $
         PTR_VALID( TauProfile.Channel        ) AND $
         PTR_VALID( TauProfile.Angle          ) AND $
         PTR_VALID( TauProfile.Profile        ) AND $
         PTR_VALID( TauProfile.Molecule_Set   ) AND $
         PTR_VALID( TauProfile.Tau            )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( TauProfile.Level_Pressure ) OR $
         PTR_VALID( TauProfile.Channel        ) OR $
         PTR_VALID( TauProfile.Angle          ) OR $
         PTR_VALID( TauProfile.Profile        ) OR $
         PTR_VALID( TauProfile.Molecule_Set   ) OR $
         PTR_VALID( TauProfile.Tau            )    ) THEN Association_Status = TRUE
  ENDELSE

  RETURN, Association_Status

END ; FUNCTION Associated_TauProfile
