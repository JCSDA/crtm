  ; Literal constants
  ZERO      = 0.0d0
  POINT5    = 0.5d0
  ONE       = 1.0d0
  ONEPOINT5 = 1.5d0
  TWO       = 2.0d0
  HUNDRED   = 100.0d0
  M2CM      = HUNDRED
  LN2       = 0.693147180559945309417232d0
  
  ; Gaussian function FWHM (cm^-1)
  GFT_FWHM = POINT5
  GFT_HWHM = GFT_FWHM/TWO

  ; Laser wavelength (m)
  LASER_WAVELENGTH       = 1.537656349d-06
  LASER_WAVELENGTH_IN_CM = LASER_WAVELENGTH*M2CM

  ; Laser frequency (m^-1)
  LASER_FREQUENCY   = ONE/LASER_WAVELENGTH
  NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO

  ; Field angle (rad)
  FIELD_ANGLE = 0.01605073d0

  ; Number of double-sided FFT points
  N_FFT = 51200L

  ; Nominal maximum optical path delay for N_FFT (m)
  NOMINAL_MAXX       = 1.9679466d-02
  NOMINAL_MAXX_IN_CM = NOMINAL_MAXX*M2CM

  ; Parameters for the resampled frequency grid
  MIN_FREQUENCY = 645.0d0
  MAX_FREQUENCY = 2760.0d0
  D_FREQUENCY   = 0.25d0
  MAX_NCHANNELS = 8461L
  RESAMPLE_MAXX = TWO

  ; Number of IASI channels
  MAX_N_CHANNELS = 8461L

  ; Band limits
  N_BANDS = 3L
  BAND_F1 = [ 645.00d0, 1210.00d0, 2000.0d0 ]
  BAND_F2 = [1209.75d0, 1999.75d0, 2760.0d0 ]
  
  ; The band discard limit. Useful data is thus
  ; from BAND_F1+DISCARD_LIMIT --> BAND_F2-DISCARD_LIMIT 
  DISCARD_LIMIT = 40.0d0


;  ; -- Literal constants
;  ZERO = 0.0d0
;  ONE  = 1.0d0
;  TWO  = 2.0d0
;  HUNDRED = 100.0d0
;
;  ; -- Gaussian function FWHM (cm^-1)
;  GFT_FWHM = 0.5d0
;  GFT_HWHM = 0.5d0/TWO
;
;  ; -- Laser wavelength (m)
;  LASER_WAVELENGTH = 1.537656349d-06
;
;  ; -- Laser frequency (m^-1)
;  LASER_FREQUENCY   = ONE/LASER_WAVELENGTH
;  NYQUIST_FREQUENCY = LASER_FREQUENCY/TWO
;
;  ; -- Field angle (rad)
;  FIELD_ANGLE = 0.01605073d0
;
;  ; -- Number of double-sided FFT points
;  N_FFT = 51200L
;
;  ; -- Maximum optical path delay (m)
;  X_MAX = (N_FFT/2)*(LASER_WAVELENGTH/TWO)*COS(FIELD_ANGLE)
;  NOMINAL_X_MAX = 1.9679466d-02

