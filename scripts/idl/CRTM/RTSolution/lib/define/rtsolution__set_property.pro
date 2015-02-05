;+
PRO RTSolution::Set_Property, $
  Debug                   = debug                  , $  ; Input keyword
  Algorithm               = algorithm              , $  ; Input keyword
  Sensor_Id               = sensor_id              , $  ; Input keyword
  WMO_Satellite_Id        = wmo_satellite_id       , $  ; Input keyword
  WMO_Sensor_Id           = wmo_sensor_id          , $  ; Input keyword
  Sensor_Channel          = sensor_channel         , $  ; Input keyword
  SOD                     = sod                    , $  ; Input keyword
  Surface_Emissivity      = surface_emissivity     , $  ; Input keyword
  Surface_Reflectivity    = surface_reflectivity   , $  ; Input keyword
  Up_Radiance             = up_radiance            , $  ; Input keyword
  Down_Radiance           = down_radiance          , $  ; Input keyword
  Down_Solar_Radiance     = down_solar_radiance    , $  ; Input keyword
  Surface_Planck_Radiance = surface_planck_radiance, $  ; Input keyword
  Radiance                = radiance               , $  ; Input keyword
  Brightness_Temperature  = brightness_temperature , $  ; Input keyword
  Upwelling_Radiance      = upwelling_radiance     , $  ; Input keyword
  Layer_Optical_Depth     = layer_optical_depth         ; Input keyword
;-

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_parameters
  @rtsolution_pro_err_handler


  ; Set scalar data
  IF ( N_ELEMENTS(algorithm              ) GT 0 ) THEN self.Algorithm               = algorithm        
  IF ( N_ELEMENTS(sensor_id              ) GT 0 ) THEN self.Sensor_Id               = sensor_id
  IF ( N_ELEMENTS(wmo_satellite_id       ) GT 0 ) THEN self.WMO_Satellite_Id        = wmo_satellite_id
  IF ( N_ELEMENTS(wmo_sensor_id          ) GT 0 ) THEN self.WMO_Sensor_Id           = wmo_sensor_id
  IF ( N_ELEMENTS(sensor_channel         ) GT 0 ) THEN self.Sensor_Channel          = sensor_channel
  IF ( N_ELEMENTS(sod                    ) GT 0 ) THEN self.SOD                     = sod
  IF ( N_ELEMENTS(surface_emissivity     ) GT 0 ) THEN self.Surface_Emissivity      = surface_emissivity
  IF ( N_ELEMENTS(surface_reflectivity   ) GT 0 ) THEN self.Surface_Reflectivity    = surface_reflectivity
  IF ( N_ELEMENTS(up_radiance            ) GT 0 ) THEN self.Up_Radiance             = up_radiance
  IF ( N_ELEMENTS(down_radiance          ) GT 0 ) THEN self.Down_Radiance           = down_radiance
  IF ( N_ELEMENTS(down_solar_radiance    ) GT 0 ) THEN self.Down_Solar_Radiance     = down_solar_radiance
  IF ( N_ELEMENTS(surface_planck_radiance) GT 0 ) THEN self.Surface_Planck_Radiance = surface_planck_radiance
  IF ( N_ELEMENTS(radiance               ) GT 0 ) THEN self.Radiance                = radiance
  IF ( N_ELEMENTS(brightness_temperature ) GT 0 ) THEN self.Brightness_Temperature  = brightness_temperature


  ; Set array data
  IF ( self->Associated(Debug = debug) ) THEN BEGIN

    self->Get_Property, $
      n_Layers = n_layers, $
      Debug    = debug

    n = N_ELEMENTS(upwelling_radiance)
    IF ( n GT 0 ) THEN BEGIN
      IF ( n NE n_layers ) THEN $
        MESSAGE, 'Size of input Upwelling_Radiance different from RTSolution allocation.', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
      (self.Upwelling_Radiance).Add, Upwelling_Radiance
    ENDIF

    n = N_ELEMENTS(layer_optical_depth)
    IF ( n GT 0 ) THEN BEGIN
      IF ( n NE n_layers ) THEN $
        MESSAGE, 'Size of input Layer_Optical_Depth different from RTSolution allocation.', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
      (self.Layer_Optical_Depth).Add, layer_optical_depth
    ENDIF
  ENDIF

END
