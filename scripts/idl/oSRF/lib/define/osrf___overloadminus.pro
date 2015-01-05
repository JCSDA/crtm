FUNCTION oSRF::_overloadMinus, $
  left, right

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_func_err_handler
  ; ...Turn off debug
  debug = FALSE
  
  ; Exit if:
  ; ...Both arguments are not oSRF objects
  IF ( (~ ISA(left,'oSRF')) || (~ ISA(right,'oSRF')) ) THEN RETURN, !NULL
  ; ...Both arguments are not allocated
  IF ( (~ left->Associated(Debug=debug)) || (~ right->Associated(Debug=debug)) ) THEN RETURN, !NULL


  ; Check the gross congruency of the left and right
  left.Get_Property, $
    Debug            = debug           , $
    n_Bands          = n_bands         , $
    Sensor_Id        = sensor_id       , $
    WMO_Satellite_ID = wmo_satellite_id, $
    WMO_Sensor_ID    = wmo_sensor_id   , $
    Sensor_Type      = sensor_type     , $
    Channel          = channel         , $
    Integral         = integral        , $
    Flags            = flags           , $
    f0               = f0              , $
    n_Points         = n_points
  right.Get_Property, $
    Debug            = debug                 , $
    n_Bands          = right_n_bands         , $
    Sensor_Id        = right_sensor_id       , $
    WMO_Satellite_ID = right_wmo_satellite_id, $
    WMO_Sensor_ID    = right_wmo_sensor_id   , $
    Sensor_Type      = right_sensor_type     , $
    Channel          = right_channel         , $
    n_Points         = right_n_points
  ; ...Same sensor/channel?
  IF ( n_bands          NE right_n_bands          OR $
       sensor_id        NE right_sensor_id        OR $
       wmo_satellite_id NE right_wmo_satellite_id OR $
       wmo_sensor_id    NE right_wmo_sensor_id    OR $
       sensor_type      NE right_sensor_type      OR $
       channel          NE right_channel          ) THEN BEGIN
    MESSAGE, 'oSRF objects do not match', /INFORMATIONAL
    RETURN, !NULL
  ENDIF
END
