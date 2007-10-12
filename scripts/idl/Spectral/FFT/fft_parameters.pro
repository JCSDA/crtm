  ; Literal constants
  ZERO      = 0.0d0
  POINT5    = 0.5d0
  ONE       = 1.0d0
  ONEPOINT5 = 1.5d0
  TWO       = 2.0d0
  THREE     = 3.0d0
  POINT46   = 0.46d0
  POINT54   = 0.54d0
  LN2       = 0.693147180559945309417232d0
  
  ; Apodisation function type values
  BARTLETT_APOD = 1
  WELCH_APOD    = 2
  CONNES_APOD   = 3
  COSINE_APOD   = 4
  HAMMING_APOD  = 5
  HANNING_APOD  = 6
  BEER_APOD       = WELCH_APOD
  STRONGBEER_APOD = CONNES_APOD
  
  ; Cos Filter default rolloff width
  DEFAULT_WIDTH = 10.0d0
  
  ; Fourier interpolation default power-of-two
  DEFAULT_PO2 = 14L
