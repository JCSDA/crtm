;+
; NAME:
;       Atmosphere_ReadFile
;
; PURPOSE:
;       The Atmosphere_ReadFile procedure reads atmosphere object data files
;       returning a list object of the Atmosphere data in the file
;
; CALLING SEQUENCE:
;       Atmosphere_ReadFile, $
;         Filename     , $
;         Atmospheres  , $
;         Quiet = Quiet, $
;         Debug = Debug, $
;         Count = Count  )
;
; INPUTS:
;       Filename:       The name of the Atmosphere object data file to read.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; OUTPUTS:
;       Atmospheres:    List containing the Atmosphere objects read from file.
;                       UNITS:      N/A
;                       TYPE:       LIST
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORDS:
;       Quiet:          Set this keyword to disable informational output
;                       If NOT SET => Record information is output. (DEFAULT)
;                          SET     => Record information is NOT output.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:          Set this keyword for debugging.
;                       If NOT SET => Error handler is enabled. (DEFAULT)
;                          SET     => Error handler is disabled; Routine
;                                     traceback output is enabled.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       Count:          The number of atmosphere objects read from file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO Atmosphere::ReadRecord, $
  filename     , $  ; Input
  fid          , $  ; Input
  Quiet = quiet, $  ; Optional input
  Debug = debug     ; Optional input

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet))


  ; Read the current atmosphere data dimensions
  n_layers    = 0L
  n_absorbers = 0L
  n_clouds    = 0L
  n_aerosols  = 0L
  READU, fid, $
    n_layers   , $
    n_absorbers, $
    n_clouds   , $
    n_aerosols


  ; Create the structure
  self->Create, $
    n_layers   , $
    n_absorbers, $
    Debug = debug
  IF ( ~self->Associated(Debug = debug) ) THEN $
    MESSAGE, 'Atmosphere object allocation failed.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the climatology model flag and absorber IDs
  climatology    = 0L
  absorber_id    = LONARR(n_absorbers)
  absorber_units = LONARR(n_absorbers)
  READU, fid, $
    climatology   , $
    absorber_id   , $
    absorber_units
  self->Atmosphere::Set_Property, $
    Debug          = debug         , $
    Climatology    = climatology   , $
    Absorber_Id    = absorber_id   , $
    Absorber_Units = absorber_units


  ; Read the atmospheric profile data
  level_pressure = DBLARR(n_layers+1L)
  pressure       = DBLARR(n_layers)
  temperature    = DBLARR(n_layers)
  absorber       = DBLARR(n_layers, n_absorbers)
  READU, fid, $
    level_pressure, $
    pressure      , $
    temperature   , $
    absorber
  self->Atmosphere::Set_Property, $
    Debug           = debug         , $
    Level_Pressure  = level_pressure, $
    Pressure        = pressure      , $
    Temperature     = temperature   , $
    Absorber_Amount = absorber


  ; Read the cloud data
  IF ( n_clouds GT 0 ) THEN BEGIN
    Cloud_ReadFile, $
      filename      , $
      clouds        , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
    self->Atmosphere::Set_Property, $
      Debug = debug , $
      Cloud = clouds
  ENDIF


  ; Read the aerosol data
  IF ( n_aerosols GT 0 ) THEN BEGIN
    Aerosol_ReadFile, $
      filename      , $
      aerosols      , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
    self->Atmosphere::Set_Property, $
      Debug   = debug   , $
      Aerosol = aerosols
  ENDIF

END


; The main script
PRO Atmosphere_ReadFile, $
  filename      , $  ; Input
  atmospheres   , $  ; Output
  Quiet  = Quiet, $  ; Optional input
  Debug  = Debug, $  ; Optional input
  Count  = Count     ; Optional output

  ; Set up
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))
  Count = 0L


  ; Process input
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file
  fid = Open_Binary_File(filename, Debug = Debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the file dimensions
  n_channels = 0L
  n_profiles = 0L
  READU, fid, n_channels, n_profiles


  ; Create the atmosphere list
  atmospheres = LIST()


  ; Loop over all the profiles
  FOR m = 1, n_profiles DO BEGIN


    ; Output an info message
    IF ( noisy ) THEN $
      MESSAGE, 'Reading Atmosphere profile #'+STRTRIM(m,2), /INFORMATIONAL


    ; Do we have channel data?
    IF ( n_channels GT 0 ) THEN BEGIN

      ; YES - THIS IS K-MATRIX DATA
      ; ---------------------------

      ; Create a channel list
      channels = LIST()

      ; Loop over all the channels
      FOR l = 0, n_channels-1 DO BEGIN

        ; Read an atmosphere record
        channel = Atmosphere(Debug=debug)
        channel->Atmosphere::ReadRecord, $
          filename, $
          fid, $
          Quiet = Quiet, $
          Debug = Debug

        ; Add the current channel to the list object
        channels.Add, channel

      ENDFOR

      ; Add the channel list to the output list object
      atmospheres.Add, channels


    ENDIF ELSE BEGIN

      ; NO CHANNELS - THIS IS FWD/TL/AD DATA
      ; ------------------------------------

      ; Read an atmosphere record
      atmosphere = Atmosphere(Debug=debug)
      atmosphere->Atmosphere::ReadRecord, $
        filename, $
        fid, $
        Quiet = Quiet, $
        Debug = Debug

      ; Add the current atmosphere to the output list object
      atmospheres.Add, atmosphere

    ENDELSE

  ENDFOR


  ; Close the file
  FREE_LUN, fid


  ; Set the output keywords
  Count = atmospheres.Count()


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of atmospheres read from '+filename+' : '+STRTRIM(Count,2), /INFORMATIONAL

END
