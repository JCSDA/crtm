;+

; IDL script to run the oSRF_Writer application for all the
; required sensors in this directory hierarchy.

; NOTE: Typically for IR  sensors, response_threshold = 0.001 (0.1%)
;                 for VIS sensors, response_threshold = 0.01  (1.0%)
;
;       to limit the amount of LBL calculations required. But these
;       limits should always be verified via visual inspection to
;       ensure potentially significant channel responses are not
;       being tossed out!

; Format of the "tauprod_create_osrf.config" include file is an array of structures,
; and each structure defines the sensor id, response threshold, and linear
; interpolation flag, e.g.:
;
;  config = [ $
;    {sensor_id:'sensor1_platform1', linear_interpolate:0, response_threshold=0.0  }, $
;    {sensor_id:'sensor1_platform2', linear_interpolate:0, response_threshold=0.001}, $
;    ....
;    {sensor_id:'sensorN_platformK', linear_interpolate:0, response_threshold=0.01 }  ]
;


PRO tauprod_create_osrf, $
  response_threshold = response_threshold, $  ; Overrides config. Must be conformable with config.
  linear_interpolate = linear_interpolate, $  ; Overrides config. Must be conformable with config.
  _extra = extra
;-

  @osrf_parameters

  
  ; Define processing parameters
  @tauprod_create_osrf.config
  ; ...Check fields
  config_fields=["SENSOR_ID", "RESPONSE_THRESHOLD", "LINEAR_INTERPOLATE"]
  config_tags = TAG_NAMES(config[0]) ; Only need to check first one because it's an array
  FOR i = 0, N_ELEMENTS(config_fields)-1 DO BEGIN
    loc = WHERE(config_tags EQ config_fields[i], count)
    IF ( count EQ 0 ) THEN MESSAGE, config_fields[i]+" field not specified in config include file"
  ENDFOR
  ; ...Count sensors
  n_sensors = N_ELEMENTS(config)

  
  ; Check keywords
  ; ...The response threshold
  n_thresholds = N_ELEMENTS(response_threshold)
  CASE n_thresholds OF
    0: _response_threshold = DOUBLE(config.response_threshold)
    1: _response_threshold = MAKE_ARRAY(n_sensors, VALUE=DOUBLE(response_threshold))
    n_sensors: _response_threshold = DOUBLE(response_threshold)
    ELSE: MESSAGE, "response_threshold keyword not conformable with config"
  ENDCASE
  ; ...The linear interpolation flag
  n_interpolates = N_ELEMENTS(linear_interpolate)
  CASE n_interpolates OF
    0: _linear_interpolate = config.linear_interpolate EQ 1
    1: _linear_interpolate = MAKE_ARRAY(n_sensors, VALUE=(linear_interpolate EQ 1))
    n_sensors: _linear_interpolate = linear_interpolate EQ 1
    ELSE: MESSAGE, "linear_interpolate keyword not conformable with config"
  ENDCASE
  

  ; Loop over sensors.
  FOREACH element, config, idx DO BEGIN
    oSRF_Writer, element.sensor_id, $
                 linear_interpolate = _linear_interpolate[idx], $
                 response_threshold = _response_threshold[idx], $
                 _extra = extra
  ENDFOREACH

END
