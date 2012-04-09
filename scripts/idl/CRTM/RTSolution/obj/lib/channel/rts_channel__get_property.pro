;+
PRO RTS_Channel::Get_Property, $
  Debug                   = Debug                  , $  ; Input keyword
  n_Layers                = n_Layers               , $  ; Output keyword
  Sensor_Id               = Sensor_Id              , $  ; Output keyword
  WMO_Satellite_Id        = WMO_Satellite_Id       , $  ; Output keyword
  WMO_Sensor_Id           = WMO_Sensor_Id          , $  ; Output keyword
  Sensor_Channel          = Sensor_Channel         , $  ; Output keyword
  RT_Algorithm_Name       = RT_Algorithm_Name      , $  ; Output keyword
  SOD                     = SOD                    , $  ; Output keyword
  Surface_Emissivity      = Surface_Emissivity     , $  ; Output keyword
  Up_Radiance             = Up_Radiance            , $  ; Output keyword
  Down_Radiance           = Down_Radiance          , $  ; Output keyword
  Down_Solar_Radiance     = Down_Solar_Radiance    , $  ; Output keyword
  Surface_Planck_Radiance = Surface_Planck_Radiance, $  ; Output keyword
  Radiance                = Radiance               , $  ; Output keyword
  Brightness_Temperature  = Brightness_Temperature , $  ; Output keyword
  Upwelling_Radiance      = Upwelling_Radiance     , $  ; Output keyword
  Layer_Optical_Depth     = Layer_Optical_Depth         ; Output keyword
;-

  ; Set up
  @rts_pro_err_handler


  ; Get scalar data
  IF ( ARG_PRESENT(n_Layers               ) ) THEN n_Layers                = self.n_Layers
  IF ( ARG_PRESENT(Sensor_Id              ) ) THEN Sensor_Id               = self.Sensor_Id
  IF ( ARG_PRESENT(WMO_Satellite_Id       ) ) THEN WMO_Satellite_Id        = self.WMO_Satellite_Id
  IF ( ARG_PRESENT(WMO_Sensor_Id          ) ) THEN WMO_Sensor_Id           = self.WMO_Sensor_Id
  IF ( ARG_PRESENT(Sensor_Channel         ) ) THEN Sensor_Channel          = self.Sensor_Channel
  IF ( ARG_PRESENT(RT_Algorithm_Name      ) ) THEN RT_Algorithm_Name       = self.RT_Algorithm_Name
  IF ( ARG_PRESENT(SOD                    ) ) THEN SOD                     = self.SOD
  IF ( ARG_PRESENT(Surface_Emissivity     ) ) THEN Surface_Emissivity      = self.Surface_Emissivity
  IF ( ARG_PRESENT(Up_Radiance            ) ) THEN Up_Radiance             = self.Up_Radiance
  IF ( ARG_PRESENT(Down_Radiance          ) ) THEN Down_Radiance           = self.Down_Radiance
  IF ( ARG_PRESENT(Down_Solar_Radiance    ) ) THEN Down_Solar_Radiance     = self.Down_Solar_Radiance
  IF ( ARG_PRESENT(Surface_Planck_Radiance) ) THEN Surface_Planck_Radiance = self.Surface_Planck_Radiance
  IF ( ARG_PRESENT(Radiance               ) ) THEN Radiance                = self.Radiance
  IF ( ARG_PRESENT(Brightness_Temperature ) ) THEN Brightness_Temperature  = self.Brightness_Temperature


  ; Get array data
  IF ( self->Associated(Debug=Debug) ) THEN BEGIN
    IF ( ARG_PRESENT(Upwelling_Radiance ) ) THEN Upwelling_Radiance  = *self.Upwelling_Radiance
    IF ( ARG_PRESENT(Layer_Optical_Depth) ) THEN Layer_Optical_Depth = *self.Layer_Optical_Depth
  ENDIF

END
