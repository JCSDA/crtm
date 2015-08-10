;+
; Parameters for OSRF routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  TWO  = 2.0d0

  ; Threshold for floating point comparisons
  THRESHOLD = 10.0d0 * (MACHAR(/DOUBLE)).EPS

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
  SENSOR_FREQUENCY_UNITS = 'Inverse centimetres (cm^-1)'

  ; Bit Flags
  IS_INTERPOLATED_FLAG      = { position: 1L, $
                                off     : 0L, $  ; Is not interpolated
                                on      : 1L, $  ; Is interpolated
                                status  : ['Not interpolated', 'Interpolated'], $
                                name    : 'Is_Interpolated_Flag' }
  IS_INTEGRATED_FLAG        = { position: 2L, $
                                off     : 0L, $  ; Is not integrated
                                on      : 1L, $  ; Is integrated
                                status  : ['Not integrated', 'Integrated'], $
                                name    : 'Is_Integrated_Flag' }
  F0_COMPUTED_FLAG          = { position: 4L, $
                                off     : 0L, $  ; f0 is not computed
                                on      : 1L, $  ; f0 is computed
                                status  : ['f0 not computed', 'f0 computed'], $
                                name    : 'f0_Computed_Flag' }
  LINEAR_INTERPOLATION_FLAG = { position: 8L, $
                                off     : 0L, $  ; Spline interpolation
                                on      : 1L, $  ; Linear interpolation
                                status  : ['Spline', 'Linear'], $
                                name    : 'Linear_Interpolation_Flag' }
  GAUSSIAN_INTEGRATION_FLAG = { position: 16L, $
                                off     : 0L, $  ; Simpson's integration
                                on      : 1L, $  ; Gaussian integration
                                status  : ['Simpson', 'Gaussian'], $
                                name    : 'Gaussian_Integration_Flag' }
  IS_DIFFERENCE_FLAG        = { position: 32L, $
                                off     : 0L, $  ; Is not a difference
                                on      : 1L, $  ; Is a difference
                                status  : ['Not a difference', 'Difference'], $
                                name    : 'Is_Difference_Flag' }
  IS_HIRES_FLAG             = { position: 64L, $
                                off     : 0L, $  ; Interpolation df is LORES
                                on      : 1L, $  ; Interpolation df is HIRES
                                status  : ['Low resolution', 'High resolution'], $
                                name    : 'Is_HiRes_Flag' }

  ; Fixed dimensions
  N_PLANCK_COEFFS        = 2L
  N_POLYCHROMATIC_COEFFS = 2L
  
  ; Default infrared sensor interpolation frequencies
  DF_LORES = 0.1d0
  DF_HIRES = 0.0025d0

  ; Cutoff response FRACTION for linear-only interpolation. All SRF data
  ; *BELOW* this relative cutoff is *ALWAYS* linearly interpolated to
  ;  minimise artifacts due to noise at these low levels
  LINEAR_INTERPOLATION_CUTOFF_FRACTION = 0.01d0

  ; Cutoff central frequency percentage difference parameters.
  ; ...When a response threshold is applied to an SRF, if the percentage
  ;    difference in the SRF central frequencies pre- and post-threshold
  ;    differ by larger than this, a warning is issued.
  FO_DIFFERENCE_PERCENTAGE_CUTOFF = 0.15d0
  ; ...When the above cutoff is not satisfied, the user-supplied threshold
  ;    is halved and applied again. If the cutoff is not satisfied after
  ;    this number of attempts, the applied threshold is set to zero.
  F0_DIFFERENCE_MAXIMUM_ITERATION = 5
  
  ; Data for polychromatic correction coefficient calculation.
  ; ...Limits for IR/MW sensors
  BC_MIN_T = 150.0d0
  BC_MAX_T = 340.0d0
  BC_DT_THRESHOLD = 1.0d-04
  ; ...Limits for visible sensors
  VISIBLE_BC_MIN_T = 2982.0d0
  VISIBLE_BC_MAX_T = 5718.0d0
  VISIBLE_BC_DT_THRESHOLD = 1.0d-04
  ; ...Temperature increment
  BC_D_T = 5.0d0
  VISIBLE_BC_D_T = 72.0d0

  ; Some plotting parameters
  WIN_FONT_SIZE = 9
  EPS_FONT_SIZE = WIN_FONT_SIZE * 1.25
  
;-
