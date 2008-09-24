;+
; Parameters for SRF routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  ; Current valid release and version numbers
  SRF_RELEASE = 2L  ; This determines structure and file formats.
  SRF_VERSION = 1L  ; This is just the data version.
  ; Invalid SRF values
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
;-
