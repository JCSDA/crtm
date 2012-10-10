;+
PRO RTSolution::Get_Property, $
  Debug                   = debug                  , $  ; Input keyword
  n_Layers                = n_layers               , $  ; Output keyword
  Algorithm               = algorithm              , $  ; Output keyword
  Sensor_Id               = sensor_id              , $  ; Output keyword
  WMO_Satellite_Id        = wmo_satellite_id       , $  ; Output keyword
  WMO_Sensor_Id           = wmo_sensor_id          , $  ; Output keyword
  Sensor_Channel          = sensor_channel         , $  ; Output keyword
  SOD                     = sod                    , $  ; Output keyword
  Surface_Emissivity      = surface_emissivity     , $  ; Output keyword
  Up_Radiance             = up_radiance            , $  ; Output keyword
  Down_Radiance           = down_radiance          , $  ; Output keyword
  Down_Solar_Radiance     = down_solar_radiance    , $  ; Output keyword
  Surface_Planck_Radiance = surface_planck_radiance, $  ; Output keyword
  Radiance                = radiance               , $  ; Output keyword
  Brightness_Temperature  = brightness_temperature , $  ; Output keyword
  Upwelling_Radiance      = upwelling_radiance     , $  ; Output keyword
  Layer_Optical_Depth     = layer_optical_depth         ; Output keyword
;-

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_pro_err_handler


  ; Get data
  ; ...Scalars
  IF ( ARG_PRESENT(n_layers               ) ) THEN n_layers                = self.n_Layers
  IF ( ARG_PRESENT(algorithm              ) ) THEN algorithm               = self.Algorithm
  IF ( ARG_PRESENT(sensor_id              ) ) THEN sensor_id               = self.Sensor_Id
  IF ( ARG_PRESENT(wmo_satellite_id       ) ) THEN wmo_satellite_id        = self.WMO_Satellite_Id
  IF ( ARG_PRESENT(wmo_sensor_id          ) ) THEN wmo_sensor_id           = self.WMO_Sensor_Id
  IF ( ARG_PRESENT(sensor_channel         ) ) THEN sensor_channel          = self.Sensor_Channel
  IF ( ARG_PRESENT(sod                    ) ) THEN sod                     = self.SOD
  IF ( ARG_PRESENT(surface_emissivity     ) ) THEN surface_emissivity      = self.Surface_Emissivity
  IF ( ARG_PRESENT(up_radiance            ) ) THEN up_radiance             = self.Up_Radiance
  IF ( ARG_PRESENT(down_radiance          ) ) THEN down_radiance           = self.Down_Radiance
  IF ( ARG_PRESENT(down_solar_radiance    ) ) THEN down_solar_radiance     = self.Down_Solar_Radiance
  IF ( ARG_PRESENT(surface_planck_radiance) ) THEN surface_planck_radiance = self.Surface_Planck_Radiance
  IF ( ARG_PRESENT(radiance               ) ) THEN radiance                = self.Radiance
  IF ( ARG_PRESENT(brightness_temperature ) ) THEN brightness_temperature  = self.Brightness_Temperature
  ; ...Arrays
  IF ( self->Associated(Debug = debug) ) THEN BEGIN
    IF ( ARG_PRESENT(upwelling_radiance ) ) THEN upwelling_radiance  = (self.Upwelling_Radiance )[0]
    IF ( ARG_PRESENT(layer_optical_depth) ) THEN layer_optical_depth = (self.Layer_Optical_Depth)[0]
  ENDIF

END
