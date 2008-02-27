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
    IF ( n_Input_Clouds NE n_Clouds ) THEN $
      MESSAGE, 'Number of clouds, ' + $
               STRTRIM(n_Input_Clouds,2) + $
               ', is different from the size of Cloud structure array, ' + $
               STRTRIM(n_Clouds,2), $
               /NONAME, /NOPRINT
    ; Read cloud data
    result = CRTM_Read_Cloud_Record( FileID, *Atm.Cloud, DEBUG=Debug )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Cloud elements', $
                 /NONAME, /NOPRINT
  ENDIF

  ; Read the Aerosol data
  ; ---------------------
  IF ( n_Aerosols GT 0 ) THEN BEGIN
    ; How many Aerosols?
    n_Input_Aerosols = 0L
    READU, FileID, n_Input_Aerosols
    IF ( n_Input_Aerosols NE n_Aerosols ) THEN $
      MESSAGE, 'Number of Aerosols, ' + $
               STRTRIM(n_Input_Aerosols,2) + $
               ', is different from the size of Aerosol structure array, ' + $
               STRTRIM(n_Aerosols,2), $
               /NONAME, /NOPRINT
    ; Read Aerosol data
    result = CRTM_Read_Aerosol_Record( FileID, *Atm.Aerosol, DEBUG=Debug )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Aerosol element', $
                 /NONAME, /NOPRINT
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
