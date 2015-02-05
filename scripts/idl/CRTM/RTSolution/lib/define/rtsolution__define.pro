;+
; RTSolution object definition procedure

PRO RTSolution__Define

  COMPILE_OPT HIDDEN
  @rtsolution_parameters

  void = { RTSolution, $
           Is_Allocated            : 0L, $
           n_Layers                : 0L, $
           Algorithm               : '', $
           Sensor_Id               : '', $
           WMO_Satellite_Id        : 0L, $
           WMO_Sensor_Id           : 0L, $
           Sensor_Channel          : 0L, $
           SOD                     : ZERO, $
           Surface_Emissivity      : ZERO, $
           Surface_Reflectivity    : ZERO, $
           Up_Radiance             : ZERO, $
           Down_Radiance           : ZERO, $
           Down_Solar_Radiance     : ZERO, $
           Surface_Planck_Radiance : ZERO, $
           Radiance                : ZERO, $
           Brightness_Temperature  : ZERO, $
           Upwelling_Radiance      : OBJ_NEW(), $
           Layer_Optical_Depth     : OBJ_NEW(), $
           INHERITS IDL_Object }
END

;-
