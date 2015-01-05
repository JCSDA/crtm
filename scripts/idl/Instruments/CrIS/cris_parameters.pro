  ; Literal constants
  ZERO      = 0.0d0
  POINT5    = 0.5d0
  ONE       = 1.0d0
  ONEPOINT5 = 1.5d0
  TWO       = 2.0d0
  THREE     = 3.0d0
  HUNDRED   = 100.0d0
  M2CM      = HUNDRED
  PI        = 3.141592653589793238462643d0
  LN2       = 0.693147180559945309417232d0
  
  ; Apodisation function types
  CRIS_HAMMING          = 1
  CRIS_BLACKMANHARRIS_3 = 2
  CRIS_BLACKMANHARRIS_4 = 3

  ; Instrument parameters
  ; ...Number of bands and channels
  N_CRIS_BANDS = 3
  N_CRIS_CHANNELS = 1305
  ; ...Laser wavelength (m)
  LASER_WAVELENGTH_IN_M = 1.550d-06
  LASER_WAVELENGTH      = LASER_WAVELENGTH_IN_M * M2CM
  ; ...Laser frequency (m^-1)
  LASER_FREQUENCY   = ONE/LASER_WAVELENGTH_IN_M
  ; ...Sampling and Nyquist frequencies
  SAMPLING_FREQUENCY = LASER_FREQUENCY*TWO  ; Every zero crossing of laser signal
  NYQUIST_FREQUENCY  = SAMPLING_FREQUENCY/TWO
  ; ...Field angle (rad)
  FIELD_ANGLE = 0.0168d0
  ; ...Number of double-sided FFT points
  N_FFT = [ 20736, 10560, 5200 ]
  ; ...Nominal maximum optical path delay for N_CRIS_FFT (m)
  NOMINAL_MAXX_IN_M = [ 8.03520d-03, $
                        4.09200d-03, $
                        2.01500d-03  ]
  NOMINAL_MAXX = NOMINAL_MAXX_IN_M * M2CM

  ; Band parameters
  ; ...Band names
  BAND_NAME = [ 'B1','B2','B3' ]
  ; ...Frequencies
  BAND_F1 = [  650.00d0, 1210.00d0, 2155.0d0 ]
  BAND_F2 = [ 1095.00d0, 1750.00d0, 2550.0d0 ]
  ; ...Channel numbering
  BEGIN_CHANNEL = [   1,  714, 1147 ]
  END_CHANNEL   = [ 713, 1146, 1305 ]
  N_CHANNELS_PER_BAND = [ 713,  433,  159 ]
  MAX_N_BAND_CHANNELS = 713
  ; ...Guard channel count
  N_GUARD_CHANNELS = [ [76, 75], [48, 47], [21, 20] ]
  ; ...Frequencies with guard channels
  BAND_GF1 = [  602.500d0, 1150.000d0, 2102.500d0 ]
  BAND_GF2 = [ 1141.875d0, 1808.750d0, 2600.000d0 ]
  ; ...Channel numbering including guard channels
  BEGIN_GCHANNEL = [   1,  865, 1393 ]
  END_GCHANNEL   = [ 864, 1392, 1592 ]
  N_GCHANNELS_PER_BAND = [ 864,  528,  200 ]
  MAX_N_BAND_GCHANNELS = 864


  ; Parameters for the resampled frequency grid
  MIN_FREQUENCY = 650.0d0
  MAX_FREQUENCY = 2550.0d0
  D_FREQUENCY    = [ 0.625d0, 1.25d0, 2.5d0 ]
  RESAMPLED_MAXX = [ 0.8d0  , 0.4d0 , 0.2d0 ]

