;+
; NAME:
;       Atmosphere_WriteFile
;
; PURPOSE:
;       The Atmosphere_WriteFile procedure writes a list of atmosphere objects
;       to a data file.
;
;       Note: If the file already exists, it is overwritten.
;
; CALLING SEQUENCE:
;       Atmosphere_WriteFile, $
;         Filename     , $
;         Atmospheres  , $
;         Swap  = Swap , &
;         Quiet = Quiet, $
;         Debug = Debug  )
;
; INPUTS:
;       Filename:       The name of the Atmosphere object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
;       Atmospheres:    List containing the Atmosphere objects to write to file.
;                       UNITS:      N/A
;                       TYPE:       LIST
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Swap:           Set this keyword parameter to byte swap data written
;                       to the file if it opened by this procedure.
;                       If NOT SET => data is written to file in the native
;                                     platform endian format. (DEFAULT)
;                          SET     => data written to file is byte-swapped
;                                     compared to the native platform byte
;                                     endian format.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
;-

PRO Atmosphere::WriteRecord, $
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


  ; Write the current atmosphere data dimensions
  self->Atmosphere::Get_Property, $
    Debug       = debug      , $
    n_Layers    = n_layers   , $
    n_Absorbers = n_absorbers, $
    n_Clouds    = n_clouds   , $
    n_Aerosols  = n_aerosols 
  WRITEU, fid, $
    n_layers   , $
    n_absorbers, $
    n_clouds   , $
    n_aerosols


  ; Write the climatology model flag and absorber IDs
  self->Atmosphere::Get_Property, $
    Debug          = debug         , $
    Climatology    = climatology   , $
    Absorber_Id    = absorber_id   , $
    Absorber_Units = absorber_units
  WRITEU, fid, $
    climatology   , $
    absorber_id   , $
    absorber_units


  ; Read the atmospheric profile data
  self->Atmosphere::Get_Property, $
    Debug           = debug         , $
    Level_Pressure  = level_pressure, $
    Pressure        = pressure      , $
    Temperature     = temperature   , $
    Absorber_Amount = absorber
  WRITEU, fid, $
    level_pressure, $
    pressure      , $
    temperature   , $
    absorber


  ; Write the cloud data
  IF ( n_clouds GT 0 ) THEN BEGIN
    self->Atmosphere::Get_Property, $
      Debug = debug , $
      Cloud = clouds
    Cloud_WriteFile, $
      filename      , $
      clouds        , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
  ENDIF


  ; Write the aerosol data
  IF ( n_aerosols GT 0 ) THEN BEGIN
    self->Atmosphere::get_Property, $
      Debug   = debug   , $
      Aerosol = aerosols
    Aerosol_WriteFile, $
      filename      , $
      aerosols      , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
  ENDIF

END


; The main script
PRO Atmosphere_WriteFile, $
  filename      , $  ; Input
  atmospheres   , $  ; Input
  Swap   = swap , $  ; Optional input
  Quiet  = quiet, $  ; Optional input
  Debug  = debug     ; Optional input

  ; Set up
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet))
  

  ; Process input
  ; ...Check filename
  IF ( ~ Valid_String(Filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check atmosphere list
  IF ( ~ OBJ_VALID(atmospheres) ) THEN $
    MESSAGE, 'Atmospheres input is not a valid object', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( ~ OBJ_ISA(atmospheres,'LIST') ) THEN $
    MESSAGE, 'Atmospheres input is not a LIST object', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  

  ; If no atmospheres, do nothing
  IF ( atmospheres.IsEmpty() ) THEN BEGIN
    MESSAGE, 'Atmospheres input LIST is empty. Nothing to do.', $
             /INFORMATIONAL
    RETURN
  ENDIF
  
  
  ; Open the file
  fid = Open_Binary_File(filename, /Write, Swap = swap, Debug = debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename+' for output', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Determine the list dimensions
  n_profiles = LONG(atmospheres.Count())
  ; ...Does list contain channel data?
  IF ( OBJ_ISA(atmospheres[0],'LIST') ) THEN $
    ; YES - THIS IS K-MATRIX DATA
    n_channels = LONG(atmospheres[0].Count()) $
  ELSE $
    ; NO CHANNELS - THIS IS FWD/TL/AD DATA
    n_channels = 0L
  
  
  ; Write the list dimensions
  WRITEU, fid, n_channels, n_profiles


  ; Loop over all the profiles
  FOR m = 0L, n_profiles-1 DO BEGIN


    ; Output an info message
    IF ( noisy ) THEN $
      MESSAGE, 'Writing Atmosphere profile #'+STRTRIM(m,2), /INFORMATIONAL


    ; Do we have channel data?
    IF ( n_channels GT 0 ) THEN BEGIN

      ; YES - THIS IS K-MATRIX DATA
      ; ---------------------------

      ; Loop over all the channels
      FOR l = 0L, n_channels-1 DO BEGIN

        ; Write an atmosphere record
        (atmospheres[m])[l]->Atmosphere::WriteRecord, $
          filename, $
          fid, $
          Quiet = quiet, $
          Debug = debug

      ENDFOR

    ENDIF ELSE BEGIN

      ; NO CHANNELS - THIS IS FWD/TL/AD DATA
      ; ------------------------------------

      ; Write an atmosphere record
      atmospheres[m]->Atmosphere::WriteRecord, $
        filename, $
        fid, $
        Quiet = quiet, $
        Debug = debug

    ENDELSE

  ENDFOR


  ; Close the file
  FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of atmospheres written to '+filename+' : '+STRTRIM(n_profiles,2), $
      /INFORMATIONAL

END
