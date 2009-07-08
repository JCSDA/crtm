;+
; Parameters for AtmProfile routines

  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
  ; Current valid release and version numbers
  ATMPROFILE_RELEASE = 1L; 2L  ; This determines structure and file formats.
  ATMPROFILE_VERSION = 1L  ; This is just the data version.
  ; Invalid values
  IP_INVALID = -1L
  FP_INVALID = -999.0d0
  ; The climatology parameters
  N_CLIMATOLOGIES = 6L
  INVALID_CLIMATOLOGY = 0L
  TROPICAL            = 1L
  MIDLATITUDE_SUMMER  = 2L
  MIDLATITUDE_WINTER  = 3L
  SUBARCTIC_SUMMER    = 4L
  SUBARCTIC_WINTER    = 5L
  US_STD_ATMOSPHERE   = 6L
  CLIMATOLOGY_NAME = [ 'Invalid           ', $
                       'Tropical          ', $
                       'Midlatitude summer', $
                       'Midlatitude winter', $
                       'Subarctic summer  ', $
                       'Subarctic winter  ', $
                       'US Std atmosphere '  ]
  ; Maximum number of absorbers
  ATMPROFILE_MAX_N_ABSORBERS = 32
  ATMPROFILE_ABSORBER_NAME = [ 'Invalid', $
                               'H2O    ', 'CO2    ', 'O3     ', 'N2O    ', $
                               'CO     ', 'CH4    ', 'O2     ', 'NO     ', $
                               'SO2    ', 'NO2    ', 'NH3    ', 'HNO3   ', $
                               'OH     ', 'HF     ', 'HCl    ', 'HBr    ', $
                               'HI     ', 'ClO    ', 'OCS    ', 'H2CO   ', $
                               'HOCl   ', 'N2     ', 'HCN    ', 'CH3Cl  ', $
                               'H2O2   ', 'C2H2   ', 'C2H6   ', 'PH3    ', $
                               'COF2   ', 'SF6    ', 'H2S    ', 'HCOOH  '  ]
  ; Absorber units parameters
  ATMPROFILE_N_ABSORBER_UNITS = 8L
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
