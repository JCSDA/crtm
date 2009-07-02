;+
; Parameters for AtmProfile routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  ; Current valid release and version numbers
  ATMPROFILE_RELEASE = 2L  ; This determines structure and file formats.
  ATMPROFILE_VERSION = 1L  ; This is just the data version.
  ; Invalid values
  IP_INVALID = -1L
  FP_INVALID = -999.0d0
  ; The climatology parameters
  N_CLIMATOLOGIES = 6
  INVALID_CLIMATOLOGY = 0
  TROPICAL            = 1
  MIDLATITUDE_SUMMER  = 2
  MIDLATITUDE_WINTER  = 3
  SUBARCTIC_SUMMER    = 4
  SUBARCTIC_WINTER    = 5
  US_STD_ATMOSPHERE   = 6
  CLIMATOLOGY_NAME = [ 'Invalid           ', $
                       'Tropical          ', $
                       'Midlatitude summer', $
                       'Midlatitude winter', $
                       'Subarctic summer  ', $
                       'Subarctic winter  ', $
                       'US Std atmosphere '  ]
  ; Maximum number of absorbers
  ATMPROFILE_MAX_N_ABSORBERS = 32
  ; Absorber units parameters
  ATMPROFILE_N_ABSORBER_UNITS = 8
  ATMPROFILE_ABSORBER_UNITS_ID = LINDGEN(ATMPROFILE_N_ABSORBER_UNITS+1L)
  ATMPROFILE_ABSORBER_UNITS_NAME = [ 'Invalid             ', $
                                     'ppmv                ', $
                                     'cm^-3               ', $
                                     'g/kg                ', $
                                     'g.m^-3              ', $
                                     'hPa                 ', $
                                     'Dew point, K        ', $  ; [H2O only]
                                     'Dew point, deg.C    ', $  ; [H2O only]
                                     'Relative humidity, %'  ]  ; [H2O only]
  ATMPROFILE_ABSORBER_UNITS_CHAR = [ '-', $  ; Invalid
                                     'A', $  ; Volume mixing ratio (ppmv)
                                     'B', $  ; Number density (cm^-3)
                                     'C', $  ; Mass mixing ratio (g/kg)
                                     'D', $  ; Mass density (g.m^-3)
                                     'E', $  ; Partial pressure (hPa)
                                     'F', $  ; Dew point (Kelvin)    [H2O only]
                                     'G', $  ; Dew point (Celsius)   [H2O only]
                                     'H'  ]  ; Relative humidity (%) [H2O only]
  ; Profile description string length
  PDSL = 512L
  ; Absorber units name string length
  AUNSL = 32L
;-
