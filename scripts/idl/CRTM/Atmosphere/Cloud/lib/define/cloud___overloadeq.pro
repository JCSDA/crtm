FUNCTION Cloud::_overloadEQ, left, right

  COMPILE_OPT HIDDEN
  @cloud_func_err_handler

  ; Exit if:
  ; ...Both arguments are not cloud objects
  IF ( (~ ISA(left,'Cloud')) || (~ ISA(right,'Cloud')) ) THEN RETURN, FALSE
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated()) || (~ right->Associated()) ) THEN RETURN, FALSE

  ; Get the properties of each object
  left.Get_Property, $
    n_Layers           = left_n_layers          , $
    Type               = left_type              , $
    Effective_Radius   = left_effective_radius  , $
    Effective_Variance = left_effective_variance, $
    Water_Content      = left_water_content        
  right.Get_Property, $
    n_Layers           = right_n_layers          , $
    Type               = right_type              , $
    Effective_Radius   = right_effective_radius  , $
    Effective_Variance = right_effective_variance, $
    Water_Content      = right_water_content        

  ; And test their equality  
  is_equal = (left_n_Layers EQ right_n_Layers) && $
             (left_type     EQ right_type)     && $
             ARRAY_EQUAL(left_effective_radius  , right_effective_radius  ) && $
             ARRAY_EQUAL(left_effective_variance, right_effective_variance) && $
             ARRAY_EQUAL(left_water_content     , right_water_content     )
  
  RETURN, is_equal

END

