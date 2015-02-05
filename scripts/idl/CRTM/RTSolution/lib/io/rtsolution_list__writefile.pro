;+
; NAME:
;       RTSolution_List::WriteFile
;
; PURPOSE:
;       The RTSolution_List::WriteFile procedure method writes a list of
;       RTSolution objectsto a data file.
;
;       Note: If the file already exists, it is overwritten.
;
; CALLING SEQUENCE:
;       Obj->[RTSolution_List::]WriteFile, $
;         Filename     , $
;         Swap  = Swap , &
;         Quiet = Quiet, $
;         Debug = Debug     
;
; INPUTS:
;       Filename:       The name of the RTSolution object data file to read.
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


PRO RTSolution::WriteRecord, $
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


  ; Write the current RTSolution data dimensions
  self->RTSolution::Get_Property, $
    Debug    = debug   , $
    n_Layers = n_layers
  WRITEU, fid, n_layers


  ; Write the sensor info
  sensor_id = BYTARR(SENSOR_ID_STRLEN)
  self->Get_Property, $
    Debug = debug, $
    Sensor_Id        = sid, $
    WMO_Satellite_Id = wmo_satellite_id , $
    WMO_Sensor_Id    = wmo_sensor_id    , $
    Sensor_Channel   = sensor_channel
  sid = BYTE(sid)
  sensor_id[0:N_ELEMENTS(sid)-1] = sid
  WRITEU, fid, $
    sensor_id       , $
    wmo_satellite_id, $
    wmo_sensor_id   , $
    sensor_channel


  ; Write the algorithm name
  algorithm = BYTARR(ALGORITHM_STRLEN)
  self->Get_Property, $
    Debug     = debug, $
    Algorithm = alg
  alg = BYTE(alg)
  algorithm[0:N_ELEMENTS(alg)-1] = alg
  WRITEU, fid, algorithm


  ; Write the forward radiative transfer intermediate results
  ; ... Scalars
  self->Get_Property, $
    Debug = debug, $
    SOD                     = sod                    , $
    Surface_Emissivity      = surface_emissivity     , $
    Surface_Reflectivity    = surface_reflectivity   , $
    Up_Radiance             = up_radiance            , $
    Down_Radiance           = down_radiance          , $
    Down_Solar_Radiance     = down_solar_radiance    , $
    Surface_Planck_Radiance = surface_planck_radiance
  WRITEU, fid, $
    sod                    , $
    surface_emissivity     , $
    surface_reflectivity   , $
    up_radiance            , $
    down_radiance          , $
    down_solar_radiance    , $
    surface_planck_radiance
  ; ...Arrays
  IF ( n_layers GT 0 ) THEN BEGIN
    self->Get_Property, $
      Debug = debug, $
      Upwelling_Radiance  = upwelling_radiance , $
      Layer_Optical_Depth = layer_optical_depth
    WRITEU, fid, $
      upwelling_radiance , $
      layer_optical_depth
  ENDIF


  ; Write the final radiative transfer results
  self->Get_Property, $
    Debug = debug, $
    Radiance               = radiance              , $
    Brightness_Temperature = brightness_temperature
  WRITEU, fid, $
    radiance              , $
    brightness_temperature

END


; The main script
PRO RTSolution_List::WriteFile, $
  filename      , $  ; Input
  Swap   = swap , $  ; Optional input
  Quiet  = quiet, $  ; Optional input
  Debug  = debug     ; Optional input

  ; Set up
  @rtsolution_parameters
  @rtsolution_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet)) || KEYWORD_SET(debug)
  

  ; Process input
  ; ...If no data, do nothing
  IF ( self.IsEmpty() ) THEN BEGIN
    MESSAGE, 'RTSolution_List object is empty. Nothing to do.', /INFORMATIONAL
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
  n_channels = LONG(self[0].Count())


  ; Write the file dimensions
  WRITEU, fid, n_channels, n_profiles


  ; Loop over the number of profiles
  FOR m = 0L, n_profiles-1L DO BEGIN


    ; Output an info message
    IF ( noisy ) THEN $
      MESSAGE, 'Writing RTSolution profile #'+STRTRIM(m+1,2), /INFORMATIONAL


    ; Loop over the number of channels
    FOR l = 0L, n_channels-1L DO BEGIN

      ; Write an RTSolution record
      (self[m])[l]->RTSolution::WriteRecord, $
        filename, $
        fid, $
        Quiet = quiet, $
        Debug = debug

    ENDFOR
    
  ENDFOR

  
  ; Close the file
  FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of profiles/channels written to ' + filename + ' : ' + $
             STRTRIM(n_profiles,2) + '/' + STRTRIM(n_channels,2), $
             /INFORMATIONAL

END
