FUNCTION IRwaterCoeff::_overloadEQ, left, right

  COMPILE_OPT HIDDEN
  @irwatercoeff_func_err_handler

  ; Exit if:
  ; ...Both arguments are not irwatercoeff objects
  IF ( (~ ISA(left,'IRwaterCoeff')) || (~ ISA(right,'IRwaterCoeff')) ) THEN RETURN, FALSE
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated()) || (~ right->Associated()) ) THEN RETURN, FALSE

  ; Get the properties of each object
  left.Get_Property, $
    n_Angles      = left_n_angles     , $
    n_Frequencies = left_n_frequencies, $
    n_Wind_Speeds = left_n_wind_speeds, $
    Angle         = left_angle        , $
    Frequency     = left_frequency    , $
    Wind_Speed    = left_wind_speed   , $
    Emissivity    = left_emissivity      
  right.Get_Property, $
    n_Angles      = right_n_angles     , $
    n_Frequencies = right_n_frequencies, $
    n_Wind_Speeds = right_n_wind_speeds, $
    Angle         = right_angle        , $
    Frequency     = right_frequency    , $
    Wind_Speed    = right_wind_speed   , $
    Emissivity    = right_emissivity      

  ; And test their equality  
  is_equal = (left_n_angles      EQ right_n_angles     ) && $
             (left_n_frequencies EQ right_n_frequencies) && $
             (left_n_wind_speeds EQ right_n_wind_speeds) && $
             ARRAY_EQUAL(left_angle     , right_angle     ) && $
             ARRAY_EQUAL(left_frequency , right_frequency ) && $
             ARRAY_EQUAL(left_wind_speed, right_wind_speed) && $
             ARRAY_EQUAL(left_emissivity, right_emissivity)
  
  RETURN, is_equal

END

