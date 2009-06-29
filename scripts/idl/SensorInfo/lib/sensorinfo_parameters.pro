;+
; Parameters for SensorInfo routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  ; Invalid SensorInfo values
  INVALID = -1L
  ; Invalid WMO sensor ids
  INVALID_WMO_SATELLITE_ID = 1023L
  INVALID_WMO_SENSOR_ID    = 2047L
  ; The instrument types
  N_SENSOR_TYPES     = 4L
  INVALID_SENSOR     = 0L
  MICROWAVE_SENSOR   = 1L
  INFRARED_SENSOR    = 2L
  VISIBLE_SENSOR     = 3L
  ULTRAVIOLET_SENSOR = 4L
  SENSOR_TYPE_NAME = [ 'Invalid    ', $
                       'Microwave  ', $
                       'Infrared   ', $
                       'Visible    ', $
                       'Ultraviolet'  ]
  ; Input data formats
  SENSORINFO_FORMAT  = '(1x,2(1x,a12),1x,a20,1x,i1,6x,3(1x,i5))'
  CHANNELINFO_FORMAT = '(i5,3x,i2,5x,e13.6)'
;-
