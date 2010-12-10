; Cloud type IsA functions
FUNCTION Cloud::IsA_Water_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ WATER_CLOUD
END
FUNCTION Cloud::IsA_Ice_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ ICE_CLOUD
END
FUNCTION Cloud::IsA_Rain_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ RAIN_CLOUD
END
FUNCTION Cloud::IsA_Snow_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SNOW_CLOUD
END
FUNCTION Cloud::IsA_Graupel_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ GRAUPEL_CLOUD
END
FUNCTION Cloud::IsA_Hail_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ HAIL_CLOUD
END

; Generic TypeOf function
FUNCTION Cloud::TypeOf, $
  Water   = Water  , $
  Ice     = Ice    , $
  Rain    = Rain   , $
  Snow    = Snow   , $
  Graupel = Graupel, $
  Hail    = Hail   , $
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  IF ( KEYWORD_SET(Water  ) ) THEN RETURN, self->IsA_Water_Cloud(Debug = Debug)
  IF ( KEYWORD_SET(Ice    ) ) THEN RETURN, self->IsA_Ice_Cloud(Debug = Debug)
  IF ( KEYWORD_SET(Rain   ) ) THEN RETURN, self->IsA_Rain_Cloud(Debug = Debug)
  IF ( KEYWORD_SET(Snow   ) ) THEN RETURN, self->IsA_Snow_Cloud(Debug = Debug)
  IF ( KEYWORD_SET(Graupel) ) THEN RETURN, self->IsA_Graupel_Cloud(Debug = Debug)
  IF ( KEYWORD_SET(Hail   ) ) THEN RETURN, self->IsA_Hail_Cloud(Debug = Debug)
  RETURN, FALSE
END

; Cloud type SetTo procedures
PRO Cloud::SetTo_Water_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = WATER_CLOUD, Debug = Debug
END
PRO Cloud::SetTo_Ice_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = ICE_CLOUD, Debug = Debug
END
PRO Cloud::SetTo_Rain_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = RAIN_CLOUD, Debug = Debug
END
PRO Cloud::SetTo_Snow_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = SNOW_CLOUD, Debug = Debug
END
PRO Cloud::SetTo_Graupel_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = GRAUPEL_CLOUD, Debug = Debug
END
PRO Cloud::SetTo_Hail_Cloud, Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  self->Set_Property, Type = HAIL_CLOUD, Debug = Debug
END

; Cloud type code definition function
FUNCTION Cloud::TypeCode, $
  Water   = Water  , $
  Ice     = Ice    , $
  Rain    = Rain   , $
  Snow    = Snow   , $
  Graupel = Graupel, $
  Hail    = Hail   , $
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  IF ( KEYWORD_SET(Water  ) ) THEN RETURN, WATER_CLOUD
  IF ( KEYWORD_SET(Ice    ) ) THEN RETURN, ICE_CLOUD
  IF ( KEYWORD_SET(Rain   ) ) THEN RETURN, RAIN_CLOUD
  IF ( KEYWORD_SET(Snow   ) ) THEN RETURN, SNOW_CLOUD
  IF ( KEYWORD_SET(Graupel) ) THEN RETURN, GRAUPEL_CLOUD
  IF ( KEYWORD_SET(Hail   ) ) THEN RETURN, HAIL_CLOUD
  RETURN, INVALID_CLOUD
END

; Cloud type name function
FUNCTION Cloud::TypeName, $
  Debug = Debug
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, CLOUD_TYPE_NAME[Type]
END
