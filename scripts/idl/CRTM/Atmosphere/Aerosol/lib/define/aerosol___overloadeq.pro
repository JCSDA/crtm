FUNCTION Aerosol::_overloadEQ, left, right

  COMPILE_OPT HIDDEN
  @aerosol_func_err_handler

  ; Exit if:
  ; ...Both arguments are not aerosol objects
  IF ( (~ ISA(left,'Aerosol')) || (~ ISA(right,'Aerosol')) ) THEN RETURN, FALSE
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated()) || (~ right->Associated()) ) THEN RETURN, FALSE

  ; Get the properties of each object
  left.Get_Property, $
    n_Layers         = left_n_layers          , $
    Type             = left_type              , $
    Effective_Radius = left_effective_radius  , $
    Concentration    = left_concentration        
  right.Get_Property, $
    n_Layers         = right_n_layers          , $
    Type             = right_type              , $
    Effective_Radius = right_effective_radius  , $
    Concentration    = right_concentration        

  ; And test their equality  
  is_equal = (left_n_Layers EQ right_n_Layers) && $
             (left_type     EQ right_type)     && $
             ARRAY_EQUAL(left_effective_radius, right_effective_radius  ) && $
             ARRAY_EQUAL(left_concentration   , right_concentration     )
  
  RETURN, is_equal

END

