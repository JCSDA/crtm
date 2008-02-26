;+
FUNCTION CRTM_Write_Atmosphere_Record, FileID     , $  ; Input
                                       Atm        , $  ; Output
                                       DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Write the data dimensions
  ; -------------------------
  WRITEU, FileID, Atm.n_Layers   , $
                  Atm.n_Absorbers, $
                  Atm.n_Clouds   , $
                  Atm.n_Aerosols
  
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(Atm.n_Layers,2)+$
          '; n_Absorbers='+STRTRIM(Atm.n_Absorbers,2)+$
          '; n_Clouds='+STRTRIM(Atm.n_Clouds,2)+$
          '; n_Aerosols='+STRTRIM(Atm.n_Aerosols,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Write the climatology model flag and absorber IDs
  ; -------------------------------------------------
  WRITEU, FileID, Atm.Climatology, $
                 *Atm.Absorber_ID, $
                 *Atm.Absorber_Units

  ; Write the atmospheric profile data
  ; ----------------------------------
  WRITEU, FileID, *Atm.Level_Pressure, $
                  *Atm.Pressure      , $
                  *Atm.Temperature   , $
                  *Atm.Absorber      

  ; Write the cloud data
  ; --------------------
  IF ( Atm.n_Clouds GT 0 ) THEN BEGIN
    WRITEU, FileID, Atm.n_Clouds
    FOR n = 0, Atm.n_Clouds-1 DO BEGIN
      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Writing cloud #'+STRTRIM(n+1,2)
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      result = CRTM_Write_Cloud_Record( FileID, (*Atm.Cloud)[n], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error writing Atmosphere Cloud element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF
  
  ; Write the Aerosol data
  ; ----------------------
  IF ( Atm.n_Aerosols GT 0 ) THEN BEGIN
    WRITEU, FileID, Atm.n_Aerosols
    FOR n = 0, Atm.n_Aerosols-1 DO BEGIN
      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Writing aerosol #'+STRTRIM(n+1,2)
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      result = CRTM_Write_Aerosol_Record( FileID, (*Atm.Aerosol)[n], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error writing Atmosphere Aerosol element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
