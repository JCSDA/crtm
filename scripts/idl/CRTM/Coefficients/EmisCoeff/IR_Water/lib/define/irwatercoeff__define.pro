;+
; IRwaterCoeff object definition procedure

PRO IRwaterCoeff__Define

  COMPILE_OPT HIDDEN
  
  void = { IRwaterCoeff, $
           Is_Allocated  : 0,  $
           Release       : 0L, $
           Version       : 0L, $
           n_Angles      : 0L, $
           n_Frequencies : 0L, $
           n_Wind_Speeds : 0L, $
           Angle         : OBJ_NEW(), $
           Frequency     : OBJ_NEW(), $
           Wind_Speed    : OBJ_NEW(), $
           Emissivity    : OBJ_NEW(), $
           INHERITS IDL_Object  }
END

;-
