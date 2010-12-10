; Aerosol type IsA functions
FUNCTION Aerosol::IsA_Dust_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ DUST_AEROSOL
END
FUNCTION Aerosol::IsA_SeaSalt_AM_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SEASALT_AM_AEROSOL
END
FUNCTION Aerosol::IsA_SeaSalt_CM1_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SEASALT_CM1_AEROSOL
END
FUNCTION Aerosol::IsA_SeaSalt_CM2_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SEASALT_CM2_AEROSOL
END
FUNCTION Aerosol::IsA_SeaSalt_CM3_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SEASALT_CM3_AEROSOL
END
FUNCTION Aerosol::IsA_Organic_Carbon_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ ORGANIC_CARBON_AEROSOL
END
FUNCTION Aerosol::IsA_Black_Carbon_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ BLACK_CARBON_AEROSOL
END
FUNCTION Aerosol::IsA_Sulfate_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, Type EQ SULFATE_AEROSOL
END

; Generic TypeOf function
FUNCTION Aerosol::TypeOf, $
  Dust           = Dust          , $
  SeaSalt_AM     = SeaSalt_AM    , $
  SeaSalt_CM1    = SeaSalt_CM1   , $
  SeaSalt_CM2    = SeaSalt_CM2   , $
  SeaSalt_CM3    = SeaSalt_CM3   , $
  Organic_Carbon = Organic_Carbon, $
  Black_Carbon   = Black_Carbon  , $
  Sulfate        = Sulfate       , $
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  IF ( KEYWORD_SET(Dust          ) ) THEN RETURN, self->IsA_Dust_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(SeaSalt_AM    ) ) THEN RETURN, self->IsA_SeaSalt_AM_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(SeaSalt_CM1   ) ) THEN RETURN, self->IsA_SeaSalt_CM1_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(SeaSalt_CM2   ) ) THEN RETURN, self->IsA_SeaSalt_CM2_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(SeaSalt_CM3   ) ) THEN RETURN, self->IsA_SeaSalt_CM3_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(Organic_Carbon) ) THEN RETURN, self->IsA_Organic_Carbon_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(Black_Carbon  ) ) THEN RETURN, self->IsA_Black_Carbon_Aerosol(Debug = Debug)
  IF ( KEYWORD_SET(Sulfate       ) ) THEN RETURN, self->IsA_Sulfate_Aerosol(Debug = Debug)
  RETURN, FALSE
END

; Aerosol type SetTo procedures
PRO Aerosol::SetTo_Dust_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = DUST_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_SeaSalt_AM_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = SEASALT_AM_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_SeaSalt_CM1_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = SEASALT_CM1_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_SeaSalt_CM2_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = SEASALT_CM2_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_SeaSalt_CM3_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = SEASALT_CM3_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_Organic_Carbon_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = ORGANIC_CARBON_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_Black_Carbon_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = BLACK_CARBON_AEROSOL, Debug = Debug
END
PRO Aerosol::SetTo_Sulfate_Aerosol, Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_pro_err_handler
  self->Set_Property, Type = SULFATE_AEROSOL, Debug = Debug
END

; Aerosol type code definition function
FUNCTION Aerosol::TypeCode, $
  Dust           = Dust          , $
  SeaSalt_AM     = SeaSalt_AM    , $
  SeaSalt_CM1    = SeaSalt_CM1   , $
  SeaSalt_CM2    = SeaSalt_CM2   , $
  SeaSalt_CM3    = SeaSalt_CM3   , $
  Organic_Carbon = Organic_Carbon, $
  Black_Carbon   = Black_Carbon  , $
  Sulfate        = Sulfate       , $
  Debug   = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  IF ( KEYWORD_SET(Dust          ) ) THEN RETURN, DUST_AEROSOL
  IF ( KEYWORD_SET(SeaSalt_AM    ) ) THEN RETURN, SEASALT_AM_AEROSOL
  IF ( KEYWORD_SET(SeaSalt_CM1   ) ) THEN RETURN, SEASALT_CM1_AEROSOL
  IF ( KEYWORD_SET(SeaSalt_CM2   ) ) THEN RETURN, SEASALT_CM2_AEROSOL
  IF ( KEYWORD_SET(SeaSalt_CM3   ) ) THEN RETURN, SEASALT_CM3_AEROSOL
  IF ( KEYWORD_SET(Organic_Carbon) ) THEN RETURN, ORGANIC_CARBON_AEROSOL
  IF ( KEYWORD_SET(Black_Carbon  ) ) THEN RETURN, BLACK_CARBON_AEROSOL
  IF ( KEYWORD_SET(Sulfate       ) ) THEN RETURN, SULFATE_AEROSOL
  RETURN, INVALID_AEROSOL
END

; Aerosol type name function
FUNCTION Aerosol::TypeName, $
  Debug = Debug
  COMPILE_OPT HIDDEN
  @aerosol_parameters
  @aerosol_func_err_handler
  self->Get_Property, Type = Type, Debug = Debug
  RETURN, AEROSOL_TYPE_NAME[Type]
END
