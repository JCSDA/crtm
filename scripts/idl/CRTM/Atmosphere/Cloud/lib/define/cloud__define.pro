; Cloud syntactic sugar functions and procedures
@cloud_syntactic_sugar

;+
; Cloud object definition procedure

PRO Cloud__Define

  COMPILE_OPT HIDDEN
  
  void = { Cloud, $
           Is_Allocated       : 0,  $
           n_Layers           : 0L, $
           Type               : 0L, $
           Effective_Radius   : OBJ_NEW(), $
           Effective_Variance : OBJ_NEW(), $
           Water_Content      : OBJ_NEW(), $
           INHERITS IDL_Object  }
END

;-
