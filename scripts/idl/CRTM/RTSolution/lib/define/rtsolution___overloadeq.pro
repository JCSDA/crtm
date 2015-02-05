FUNCTION RTSolution::_overloadEQ, left, right

  COMPILE_OPT HIDDEN
  @rtsolution_func_err_handler

  ; Exit if:
  ; ...Both arguments are not RTSolution objects
  IF ( (~ ISA(left,'RTSolution')) || (~ ISA(right,'RTSolution')) ) THEN RETURN, FALSE
  ; ...Both arguments are either allocated or not allocated (poor man's XOR)
  IF ( left->Associated() NE right->Associated() ) THEN RETURN, FALSE

  ; Get the SCALAR properties of each object
  left.Get_Property, $
    n_Layers                = left_n_layers               , $
    Algorithm               = left_algorithm              , $
    Sensor_Id               = left_sensor_id              , $
    WMO_Satellite_Id        = left_wmo_satellite_id       , $
    WMO_Sensor_Id           = left_wmo_sensor_id          , $
    Sensor_Channel          = left_sensor_channel         , $
    SOD                     = left_sod                    , $
    Surface_Emissivity      = left_surface_emissivity     , $
    Surface_Reflectivity    = left_surface_reflectivity   , $
    Up_Radiance             = left_up_radiance            , $
    Down_Radiance           = left_down_radiance          , $
    Down_Solar_Radiance     = left_down_solar_radiance    , $
    Surface_Planck_Radiance = left_surface_planck_radiance, $
    Radiance                = left_radiance               , $
    Brightness_Temperature  = left_brightness_temperature 
  right.Get_Property, $
    n_Layers                = right_n_layers               , $
    Algorithm               = right_algorithm              , $
    Sensor_Id               = right_sensor_id              , $
    WMO_Satellite_Id        = right_wmo_satellite_id       , $
    WMO_Sensor_Id           = right_wmo_sensor_id          , $
    Sensor_Channel          = right_sensor_channel         , $
    SOD                     = right_sod                    , $
    Surface_Emissivity      = right_surface_emissivity     , $
    Surface_Reflectivity    = right_surface_reflectivity   , $
    Up_Radiance             = right_up_radiance            , $
    Down_Radiance           = right_down_radiance          , $
    Down_Solar_Radiance     = right_down_solar_radiance    , $
    Surface_Planck_Radiance = right_surface_planck_radiance, $
    Radiance                = right_radiance               , $
    Brightness_Temperature  = right_brightness_temperature 

  ; And test their equality  
  is_equal = (left_n_layers                EQ right_n_layers               ) && $
             (left_algorithm               EQ right_algorithm              ) && $
             (left_sensor_id               EQ right_sensor_id              ) && $
             (left_wmo_satellite_id        EQ right_wmo_satellite_id       ) && $
             (left_wmo_sensor_id           EQ right_wmo_sensor_id          ) && $
             (left_sensor_channel          EQ right_sensor_channel         ) && $
             (left_sod                     EQ right_sod                    ) && $
             (left_surface_emissivity      EQ right_surface_emissivity     ) && $
             (left_surface_reflectivity    EQ right_surface_reflectivity   ) && $
             (left_up_radiance             EQ right_up_radiance            ) && $
             (left_down_radiance           EQ right_down_radiance          ) && $
             (left_down_solar_radiance     EQ right_down_solar_radiance    ) && $
             (left_surface_planck_radiance EQ right_surface_planck_radiance) && $
             (left_radiance                EQ right_radiance               ) && $
             (left_brightness_temperature  EQ right_brightness_temperature )


  IF ( left_n_layers GT 0 ) THEN BEGIN

    ; Get the ARRAY properties of each object
    left.Get_Property, $
      Upwelling_Radiance  = left_upwelling_radiance , $
      Layer_Optical_Depth = left_layer_optical_depth
    right.Get_Property, $
      Upwelling_Radiance  = right_upwelling_radiance , $
      Layer_Optical_Depth = right_layer_optical_depth

    ; And test their equality  
    is_equal = is_equal && $
               ARRAY_EQUAL(left_upwelling_radiance , right_upwelling_radiance ) && $
               ARRAY_EQUAL(left_layer_optical_depth, right_layer_optical_depth)
  ENDIF
  
  RETURN, is_equal

END

