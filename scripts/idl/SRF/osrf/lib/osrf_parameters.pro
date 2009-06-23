;+
; Parameters for OSRF routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  ; Current valid release and version numbers
  OSRF_RELEASE = 2L  ; This determines structure and file formats.
  OSRF_VERSION = 1L  ; This is just the data version.
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
  SENSOR_FREQUENCY_UNITS = [ 'Invalid'                    , $
                             'Gigahertz (GHz)'            , $
                             'Inverse centimetres (cm^-1)', $
                             'Inverse centimetres (cm^-1)', $
                             'Inverse centimetres (cm^-1)'  ]

  ; Bit Flags
  INTERPOLATED_FLAG         =  1L  ; 0==no     , 1==yes
  INTEGRATED_FLAG           =  2L  ; 0==no     , 1==yes
  F0_COMPUTED_FLAG          =  4L  ; 0==no     , 1==yes
  FREQUENCY_UNITS_FLAG      =  8L  ; 0==cm^-1  , 1==GHz
  INTERPOLATION_METHOD_FLAG = 16L  ; 0==Spline , 1==Linear
  INTEGRATION_METHOD_FLAG   = 32L  ; 0==Simpson, 1==Gaussian
  ;...INTERPOLATED_FLAG parameters
  OSRF_NOT_INTERPOLATED = 0L
  OSRF_INTERPOLATED     = 1L
  OSRF_INTERPOLATED_FLAG_STATUS = ['Not interpolated', 'Interpolated']
  ;...INTEGRATED_FLAG parameters
  OSRF_NOT_INTEGRATED = 0L
  OSRF_INTEGRATED     = 1L
  OSRF_INTEGRATED_FLAG_STATUS = ['Not integrated', 'Integrated']
  ;...F0_COMPUTEDED_FLAG parameters
  OSRF_F0_NOT_COMPUTED = 0L
  OSRF_F0_COMPUTED     = 1L
  OSRF_F0_COMPUTED_FLAG_STATUS = ['f0 not computed', 'f0 computed']
  ;...FREQUENCY_UNITS_FLAG parameters
  OSRF_INVERSE_CM = 0L
  OSRF_GHZ        = 1L
  OSRF_FREQUENCY_UNITS_FLAG_STATUS = ['cm^-1', 'GHz']
  ;...INTERPOLATION_METHOD_FLAG parameters
  OSRF_SPLINE = 0L
  OSRF_LINEAR = 1L
  OSRF_INTERPOLATION_METHOD_FLAG_STATUS = ['Spline', 'Linear']
  ;...INTEGRATION_METHOD_FLAG parameters
  OSRF_SIMPSON  = 0L
  OSRF_GAUSSIAN = 1L
  OSRF_INTEGRATION_METHOD_FLAG_STATUS = ['Simpson', 'Gaussian']
  
  ; Fixed dimensions
  N_PLANCK_COEFFS        = 2L
  N_POLYCHROMATIC_COEFFS = 2L
  
  ; Default interpolation frequencies
  DF_LORES = 0.1d0
  DF_HIRES = 0.0025d0
;-
