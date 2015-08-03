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

; Format of the "create_osrf.config" include file is an array of structures,
; and each structure defines the sensor id and linear interpolation flag, e.g.:
;
;  config = [ $
;    {sensor_id:'sensor1_platform1'    , linear_interpolate:0}, $
;    {sensor_id:'sensor1_platform2'    , linear_interpolate:0}, $
;    ....
;    {sensor_id:'sensorN_platformK'    , linear_interpolate:0}  ]


PRO tauprod_create_osrf, $
  response_threshold = response_threshold, $
  no_plot = no_plot
;-

  @osrf_parameters
  
  ; Define processing parameters
  @create_osrf.config
  
  ; Loop over sensors.
  FOREACH element, config DO $
    oSRF_Writer, element.sensor_id, $
                 linear_interpolate = element.linear_interpolate, $
                 response_threshold = response_threshold, $
                 no_plot = no_plot

END
