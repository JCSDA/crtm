;+
FUNCTION CRTM_Read_Atmosphere_Record, FileID     , $  ; Input
                                      Atm        , $  ; Output
                                      DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Read the data dimensions
  ; ------------------------
  n_Layers    = 0L
  n_Absorbers = 0L
  n_Clouds    = 0L
  n_Aerosols  = 0L
  READU, FileID, n_Layers   , $
                 n_Absorbers, $
                 n_Clouds   , $
                 n_Aerosols

  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(n_Layers,2)+$
          '; n_Absorbers='+STRTRIM(n_Absorbers,2)+$
          '; n_Clouds='+STRTRIM(n_Clouds,2)+$
          '; n_Aerosols='+STRTRIM(n_Aerosols,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Allocate the atmosphere structure
  ; ---------------------------------
  result = CRTM_Allocate_Atmosphere( n_Layers   , $
                                     n_Absorbers, $
                                     n_Clouds   , $
                                     n_Aerosols , $
                                     Atm          )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating Atmosphere structure', $
             /NONAME, /NOPRINT
  
  ; Read the climatology model flag and absorber IDs
  ; ------------------------------------------------
  Climatology = Atm.Climatology
  READU, FileID, Climatology, $
                 *Atm.Absorber_ID, $
                 *Atm.Absorber_Units
  Atm.Climatology = Climatology

  ; Read the atmospheric profile data
  ; ---------------------------------
  READU, FileID, *Atm.Level_Pressure, $
                 *Atm.Pressure      , $
                 *Atm.Temperature   , $
                 *Atm.Absorber      

  ; Read the cloud data
  ; -------------------
  IF ( n_Clouds GT 0 ) THEN BEGIN
    ; How many clouds?
    n_Input_Clouds = 0L
    READU, FileID, n_Input_Clouds
    IF ( n_Input_Clouds GT n_Clouds ) THEN $
      MESSAGE, 'Number of clouds, ' + $
               STRTRIM(n_Input_Clouds,2) + $
               ', is > size of Cloud structure array, ' + $
               STRTRIM(n_Clouds,2), $
               /NONAME, /NOPRINT
    Atm.n_Clouds = n_Input_Clouds
    ; Read each cloud's data
    FOR n = 0, Atm.n_Clouds-1 DO BEGIN
      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Reading cloud #'+STRTRIM(n+1,2)
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      result = CRTM_Read_Cloud_Record( FileID, (*Atm.Cloud)[n], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error reading Atmosphere Cloud element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF

  ; Read the Aerosol data
  ; ---------------------
  IF ( n_Aerosols GT 0 ) THEN BEGIN
    ; How many Aerosols?
    n_Input_Aerosols = 0L
    READU, FileID, n_Input_Aerosols
    IF ( n_Input_Aerosols GT n_Aerosols ) THEN $
      MESSAGE, 'Number of Aerosols, ' + $
               STRTRIM(n_Input_Aerosols,2) + $
               ', is > size of Aerosol structure array, ' + $
               STRTRIM(n_Aerosols,2), $
               /NONAME, /NOPRINT
    Atm.n_Aerosols = n_Input_Aerosols
    ; Read each Aerosol's data
    FOR n = 0, Atm.n_Aerosols-1 DO BEGIN
      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Reading aerosol #'+STRTRIM(n+1,2)
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      result = CRTM_Read_Aerosol_Record( FileID, (*Atm.Aerosol)[n], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error reading Atmosphere Aerosol element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
