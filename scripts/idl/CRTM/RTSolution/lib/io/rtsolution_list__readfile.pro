;+
; NAME:
;       RTSolution_List::ReadFile
;
; PURPOSE:
;       The RTSolution_List::ReadFile procedure method reads RTSolution object
;       data files filling the RTSolution_List object with the RTSolution data
;       in the file
;
; CALLING SEQUENCE:
;       Obj->[RTSolution_List::]ReadFile, $
;         Filename               , $
;         Quiet      = Quiet     , $
;         Debug      = Debug     , $
;         n_Profiles = n_Profiles, $
;         n_Channels = n_Channels
;
; INPUTS:
;       Filename:       The name of the RTSolution object data file to read.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
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
;       n_Profiles:     The number of profiles of RTSolution objects read from file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Channels:     The number of channels of RTSolution objects read from file
;                       for each profile.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;-


PRO RTSolution::ReadRecord, $
  filename     , $  ; Input
  fid          , $  ; Input
  Quiet = quiet, $  ; Optional input
  Debug = debug     ; Optional input

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_parameters
  @rtsolution_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)


  ; Read the current RTSolution data dimensions
  n_layers = 0L
  READU, fid, n_layers


  ; Create, or reinitialise, the structure as necessary
  IF ( n_layers GT 0 ) THEN BEGIN
    self->Create, $
      n_layers, $
      Debug = debug
    IF ( ~self->Associated(Debug = debug) ) THEN $
      MESSAGE, 'RTSolution object allocation failed.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF ELSE BEGIN
    self->Destroy, Debug = debug
    IF ( self->Associated(Debug = debug) ) THEN $
      MESSAGE, 'RTSolution object reinitialisation failed.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDELSE


  ; Read the sensor info
  sensor_id        = BYTARR(SENSOR_ID_STRLEN)
  wmo_satellite_id = 0L
  wmo_sensor_id    = 0L
  sensor_channel   = 0L
  READU, fid, $
    sensor_id       , $
    wmo_satellite_id, $
    wmo_sensor_id   , $
    sensor_channel
  self->Set_Property, $
    Debug = debug, $
    Sensor_Id        = STRING(sensor_id), $
    WMO_Satellite_Id = wmo_satellite_id , $
    WMO_Sensor_Id    = wmo_sensor_id    , $
    Sensor_Channel   = sensor_channel


  ; Read the algorithm name
  algorithm = BYTARR(ALGORITHM_STRLEN)
  READU, fid, algorithm
  self->Set_Property, $
    Debug = debug, $
    Algorithm = STRING(algorithm)


  ; Read the forward radiative transfer intermediate results
  ; ... Scalars
  sod                     = ZERO
  surface_emissivity      = ZERO
  up_radiance             = ZERO
  down_radiance           = ZERO
  down_solar_radiance     = ZERO
  surface_planck_radiance = ZERO
  READU, fid, $
    sod                    , $
    surface_emissivity     , $
    up_radiance            , $
    down_radiance          , $
    down_solar_radiance    , $
    surface_planck_radiance
  self->Set_Property, $
    Debug = debug, $
    SOD                     = sod                    , $
    Surface_Emissivity      = surface_emissivity     , $
    Up_Radiance             = up_radiance            , $
    Down_Radiance           = down_radiance          , $
    Down_Solar_Radiance     = down_solar_radiance    , $
    Surface_Planck_Radiance = surface_planck_radiance
  ; ...Arrays
  IF ( n_layers GT 0 ) THEN BEGIN
    upwelling_radiance  = MAKE_ARRAY(n_layers, VALUE = ZERO)
    layer_optical_depth = MAKE_ARRAY(n_layers, VALUE = ZERO)
    READU, fid, $
      upwelling_radiance , $
      layer_optical_depth
    self->Set_Property, $
      Debug = debug, $
      Upwelling_Radiance  = upwelling_radiance , $
      Layer_Optical_Depth = layer_optical_depth
  ENDIF


  ; Read the final radiative transfer results
    radiance               = ZERO
    brightness_temperature = ZERO
  READU, fid, $
    radiance              , $
    brightness_temperature
  self->Set_Property, $
    Debug = debug, $
    Radiance               = radiance              , $
    Brightness_Temperature = brightness_temperature

END


; The main script
PRO RTSolution_List::ReadFile, $
  filename               , $  ; Input
  Quiet      = quiet     , $  ; Optional input
  Debug      = debug     , $  ; Optional input
  n_Profiles = n_profiles, $  ; Optional output
  n_Channels = n_channels     ; Optional output

  ; Set up
  @rtsolution_parameters
  @rtsolution_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet)) || KEYWORD_SET(debug)
  ; ...Ensure the list is empty
  self.Remove, /ALL
  

  ; Process input
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file
  fid = Open_Binary_File(filename, Debug = debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the file dimensions
  n_channels = 0L
  n_profiles = 0L
  READU, fid, n_channels, n_profiles


  ; Loop over the number of profiles
  FOR m = 1L, n_profiles DO BEGIN


    ; Output an info message
    IF ( noisy ) THEN $
      MESSAGE, 'Reading RTSolution profile #'+STRTRIM(m,2), /INFORMATIONAL


    ; Create a channel list
    channels = RTSolution_List()
    

    ; Loop over the number of channels
    FOR n = 1, n_channels DO BEGIN

      ; Read an RTSolution record
      channel = RTSolution(Debug = debug)
      channel->RTSolution::ReadRecord, $
        filename, $
        fid, $
        Quiet = quiet, $
        Debug = debug

      ; Add the current channel to the list object
      channels.Add, channel

    ENDFOR
    
    
    ; Add the channel list to the output list object
    self.Add, channels
    
  ENDFOR

  
  ; Close the file
  FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of profiles/channels read from ' + filename + ' : ' + $
             STRTRIM(n_profiles,2) + '/' + STRTRIM(n_channels,2), $
             /INFORMATIONAL

END
