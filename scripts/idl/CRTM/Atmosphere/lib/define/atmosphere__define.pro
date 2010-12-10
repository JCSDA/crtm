;+
; Atmosphere object definition procedure

PRO Atmosphere__Define

;-
  COMPILE_OPT HIDDEN
  
  void = { Atmosphere, $
           Is_Allocated   : 0,  $
           n_Layers       : 0L, $
           n_Absorbers    : 0L, $
           n_Clouds       : 0L, $
           n_Aerosols     : 0L, $
           Climatology    : 0L, $
           Absorber_ID    : LIST(), $
           Absorber_Units : LIST(), $
           Level_Pressure : LIST(), $
           Pressure       : LIST(), $
           Temperature    : LIST(), $
           Absorber       : LIST(), $
           Cloud          : LIST(), $
           Aerosol        : LIST()  }
END
