FUNCTION Atmosphere::_overloadEQ, left, right

  COMPILE_OPT HIDDEN
  @atmosphere_func_err_handler

  ; Exit if:
  ; ...Both arguments are not atmosphere objects
  IF ( (~ ISA(left,'Atmosphere')) || (~ ISA(right,'Atmosphere')) ) THEN RETURN, FALSE
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated()) || (~ right->Associated()) ) THEN RETURN, FALSE

  ; Get the properties of each object
  left.Get_Property, $
    n_Layers        = left_n_layers      , $
    n_Absorbers     = left_n_absorbers   , $
    n_Clouds        = left_n_clouds      , $
    n_Aerosols      = left_n_aerosols    , $
    Climatology     = left_climatology   , $
    Absorber_ID     = left_absorber_id   , $
    Absorber_Units  = left_absorber_units, $
    Level_Pressure  = left_level_pressure, $
    Pressure        = left_pressure      , $
    Temperature     = left_temperature   , $
    Absorber_Amount = left_absorber      , $
    CFraction       = left_cfraction     , $
    Cloud           = left_cloud         , $
    Aerosol         = left_aerosol       
  right.Get_Property, $
    n_Layers        = right_n_layers      , $
    n_Absorbers     = right_n_absorbers   , $
    n_Clouds        = right_n_clouds      , $
    n_Aerosols      = right_n_aerosols    , $
    Climatology     = right_climatology   , $
    Absorber_ID     = right_absorber_id   , $
    Absorber_Units  = right_absorber_units, $
    Level_Pressure  = right_level_pressure, $
    Pressure        = right_pressure      , $
    Temperature     = right_temperature   , $
    Absorber_Amount = right_absorber      , $
    CFraction       = right_cfraction     , $
    Cloud           = right_cloud         , $
    Aerosol         = right_aerosol       

  ; And test their equality  
  is_equal = (left_n_layers    EQ right_n_layers   ) && $
             (left_n_absorbers EQ right_n_absorbers) && $
             (left_n_clouds    EQ right_n_clouds   ) && $
             (left_n_aerosols  EQ right_n_aerosols ) && $
             (left_climatology EQ right_climatology) && $
             ARRAY_EQUAL(left_absorber_id   , right_absorber_id   ) && $
             ARRAY_EQUAL(left_absorber_units, right_absorber_units) && $
             ARRAY_EQUAL(left_level_pressure, right_level_pressure) && $
             ARRAY_EQUAL(left_pressure      , right_pressure      ) && $
             ARRAY_EQUAL(left_temperature   , right_temperature   ) && $
             ARRAY_EQUAL(left_absorber      , right_absorber      ) && $
             ARRAY_EQUAL(left_cfraction     , right_cfraction     )
  IF ( left_n_clouds GT 0 ) THEN $
    FOR i = 0, left_cloud.Count() - 1 DO $
      is_equal = is_equal AND (left_cloud[i] EQ right_cloud[i])
  IF ( left_n_aerosols GT 0 ) THEN $
    FOR i = 0, left_aerosol.Count() - 1 DO $
      is_equal = is_equal AND (left_aerosol[i] EQ right_aerosol[i])
  
  RETURN, is_equal

END

