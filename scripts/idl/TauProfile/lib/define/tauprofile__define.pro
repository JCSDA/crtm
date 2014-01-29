;+
; TauProfile object definition procedure

PRO TauProfile__Define

  COMPILE_OPT HIDDEN
  
  void = { TauProfile, $
           Is_Allocated    : 0,  $
           Release         : 0L, $
           Version         : 0L, $
           n_Layers        : 0L, $  ; K
           n_Channels      : 0L, $  ; L
           n_Angles        : 0L, $  ; I
           n_Profiles      : 0L, $  ; M
           n_Molecule_Sets : 0L, $  ; J
           Sensor_ID       : '', $
           WMO_Satellite_ID: 0L, $
           WMO_Sensor_ID   : 0L, $
           Level_Pressure  : OBJ_NEW(), $  ; K+1
           Channel         : OBJ_NEW(), $  ; L
           Angle           : OBJ_NEW(), $  ; I
           Profile         : OBJ_NEW(), $  ; M
           Molecule_Set    : OBJ_NEW(), $  ; J
           Tau             : OBJ_NEW(), $  ; K x L x I x M x J
           INHERITS IDL_Object  }
END

;-
