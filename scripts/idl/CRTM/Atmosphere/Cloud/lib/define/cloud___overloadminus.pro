FUNCTION Cloud::_overloadMinus, left, right

  COMPILE_OPT HIDDEN

  ; Exit if:
  ; ...Both arguments are not cloud objects
  IF ( (~ ISA(left,'Cloud')) || (~ ISA(right,'Cloud')) ) THEN $
    MESSAGE, 'Must supply two Cloud objects for subtraction'
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated()) || (~ right->Associated()) ) THEN $
    MESSAGE, 'Both Cloud objects must be allocated for subtraction'

  ; Get the properties of each object
  left.Get_Property, $
    n_Layers           = n_layers          , $
    Type               = type              , $
    Effective_Radius   = effective_radius  , $
    Effective_Variance = effective_variance, $
    Water_Content      = water_content        
  right.Get_Property, $
    n_Layers           = right_n_layers          , $
    Type               = right_type              , $
    Effective_Radius   = right_effective_radius  , $
    Effective_Variance = right_effective_variance, $
    Water_Content      = right_water_content        

  ; Exit if dimensions and type are different
  IF ( n_layers NE right_n_Layers ) THEN $
    MESSAGE, 'Input Cloud objects have different number of layers'
  IF ( type NE right_type ) THEN $
    MESSAGE, 'Input Cloud objects have different cloud type designations'

  ; Create new object
  result = Cloud()
  result.Create, n_layers
  
  ; Set the properties of the object
  result.Set_Property, $
    Type               = type              , $
    Effective_Radius   = effective_radius   - right_effective_radius  , $
    Effective_Variance = effective_variance - right_effective_variance, $
    Water_Content      = water_content      - right_water_content        

  RETURN, result

END
