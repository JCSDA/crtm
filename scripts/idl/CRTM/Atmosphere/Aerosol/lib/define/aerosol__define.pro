; Aerosol syntactic sugar functions and procedures
@aerosol_syntactic_sugar

;+
; Aerosol object definition procedure

PRO Aerosol__Define

  COMPILE_OPT HIDDEN
  
  void = { Aerosol, $
           Is_Allocated     : 0,  $
           n_Layers         : 0L, $
           Type             : 0L, $
           Effective_Radius : OBJ_NEW(), $
           Concentration    : OBJ_NEW(), $
           INHERITS IDL_Object }
END

;-
