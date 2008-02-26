PRO CRTM_Atmosphere__Define
  void = { CRTM_Atmosphere, $
           n_Layers       : 0L, $
           n_Absorbers    : 0L, $
           n_Clouds       : 0L, $
           n_Aerosols     : 0L, $
           Climatology    : 0L, $
           Absorber_ID    : PTR_NEW(), $
           Absorber_Units : PTR_NEW(), $
           Level_Pressure : PTR_NEW(), $
           Pressure       : PTR_NEW(), $
           Temperature    : PTR_NEW(), $
           Absorber       : PTR_NEW(), $
           Cloud          : PTR_NEW(), $
           Aerosol        : PTR_NEW()  }
END
