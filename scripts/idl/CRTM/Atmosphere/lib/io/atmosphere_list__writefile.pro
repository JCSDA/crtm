;+
; NAME:
;       Atmosphere_List::WriteFile
;
; PURPOSE:
;       The Atmosphere_List::WriteFile procedure method writes a list of
;       atmosphere objectsto a data file.
;
;       Note: If the file already exists, it is overwritten.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere_List::]WriteFile, $
;         Filename     , $
;         Swap  = Swap , &
;         Quiet = Quiet, $
;         Debug = Debug
;
; INPUTS:
;       Filename:       The name of the Atmosphere object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
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
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)


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


  ; Write the atmospheric profile data
  self->Atmosphere::Get_Property, $
    Debug           = debug         , $
    Level_Pressure  = level_pressure, $
    Pressure        = pressure      , $
    Temperature     = temperature   , $
    Absorber_Amount = absorber      , $
    CFraction       = cloud_fraction
  WRITEU, fid, $
    level_pressure, $
    pressure      , $
    temperature   , $
    absorber      , $
    cloud_fraction


  ; Write the cloud data
  IF ( n_clouds GT 0 ) THEN BEGIN
    self->Atmosphere::Get_Property, $
      Debug = debug , $
      Cloud = clouds
    clouds->Cloud_List::WriteFile, $
      filename      , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
  ENDIF


  ; Write the aerosol data
  IF ( n_aerosols GT 0 ) THEN BEGIN
    self->Atmosphere::get_Property, $
      Debug   = debug   , $
      Aerosol = aerosols
    aerosols->Aerosol_List::WriteFile, $
      filename      , $
      FileId = fid  , $
      Quiet  = quiet, $
      Debug  = debug
  ENDIF

END


; The main script
PRO Atmosphere_List::WriteFile, $
  filename      , $  ; Input
  Swap   = swap , $  ; Optional input
  Quiet  = quiet, $  ; Optional input
  Debug  = debug     ; Optional input

  ; Set up
  @atmosphere_parameters
  @atmosphere_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)
  

  ; Process input
  ; ...If no data, do nothing
  IF ( self.IsEmpty() ) THEN BEGIN
    MESSAGE, 'Atmosphere_List object is empty. Nothing to do.', /INFORMATIONAL
    RETURN
  ENDIF
  ; ...Check filename
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  
  ; Open the file
  fid = Open_Binary_File(filename, /Write, Swap = swap, Debug = debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename+' for output', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Determine the list dimensions
  n_profiles = LONG(self.Count())
  ; ...Does list contain channel data?
  IF ( OBJ_ISA(self[0],'LIST') ) THEN $
    ; YES - THIS IS K-MATRIX DATA
    n_channels = LONG(self[0].Count()) $
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
        (self[m])[l]->Atmosphere::WriteRecord, $
          filename, $
          fid, $
          Quiet = quiet, $
          Debug = debug

      ENDFOR

    ENDIF ELSE BEGIN

      ; NO CHANNELS - THIS IS FWD/TL/AD DATA
      ; ------------------------------------

      ; Write an atmosphere record
      self[m]->Atmosphere::WriteRecord, $
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
