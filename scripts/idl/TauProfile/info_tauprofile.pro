;-----------------------------------
FUNCTION Info_TauProfile, TauProfile
;-----------------------------------

  ; Set up
  ; ------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  CR = STRING(13B)
  LF = STRING(10B)

  ; Create information string
  ; -------------------------
  Information = STRING( CR+LF                     , $
                        TauProfile.Sensor_ID      , $
                        TauProfile.n_Layers       , $
                        TauProfile.n_Channels     , $
                        TauProfile.n_Angles       , $
                        TauProfile.n_Profiles     , $
                        TauProfile.n_Molecule_Sets, $
                        FORMAT = '(a,1x,a,1x,"TauProfile RELEASE.VERSION: ",i2,".",i2.2,2x,' + $
                                   '"N_LAYERS=",i3,2x,'     + $
                                   '"N_CHANNELS=",i4,2x,'   + $
                                   '"N_ANGLES=",i1,2x,'     + $
                                   '"N_PROFILES=",i3,2x,'   + $
                                   '"N_MOLECULE_SETS=",i2 )'  )
  
  CATCH, /CANCEL
  RETURN, Information
END ; FUNCTION Info_TauProfile
