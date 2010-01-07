PRO RTS_Channel__Define
  void = { RTS_Channel, $
           n_Allocates : 0L, $
           n_Layers                : 0L, $
           SOD                     : 0.0d0, $
           Surface_Emissivity      : 0.0d0, $
           Up_Radiance             : 0.0d0, $
           Down_Radiance           : 0.0d0, $
           Down_Solar_Radiance     : 0.0d0, $
           Surface_Planck_Radiance : 0.0d0, $
           Radiance                : 0.0d0, $
           Brightness_Temperature  : 0.0d0, $
           Upwelling_Radiance      : PTR_NEW(), $
           Layer_Optical_Depth     : PTR_NEW() }
END
