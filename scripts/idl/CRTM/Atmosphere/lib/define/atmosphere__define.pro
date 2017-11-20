;+
; Atmosphere object definition procedure

PRO Atmosphere__Define

  COMPILE_OPT HIDDEN
  
  void = { Atmosphere, $
           Is_Allocated   : 0,  $
           n_Layers       : 0L, $
           n_Absorbers    : 0L, $
           n_Clouds       : 0L, $
           n_Aerosols     : 0L, $
           Climatology    : 0L, $
           Absorber_ID    : OBJ_NEW(), $
           Absorber_Units : OBJ_NEW(), $
           Level_Pressure : OBJ_NEW(), $
           Pressure       : OBJ_NEW(), $
           Temperature    : OBJ_NEW(), $
           Absorber       : OBJ_NEW(), $
           CFraction      : OBJ_NEW(), $
           Cloud          : OBJ_NEW(), $
           Aerosol        : OBJ_NEW(), $
           INHERITS IDL_Object  }
END

;-
