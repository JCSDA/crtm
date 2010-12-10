  ; The absorber IDs. Use HITRAN definitions
  N_VALID_ABSORBER_IDS = 32
  INVALID_ABSORBER_ID =  0
    H2O_ID =  1
    CO2_ID =  2
     O3_ID =  3
    N2O_ID =  4
     CO_ID =  5
    CH4_ID =  6
     O2_ID =  7
     NO_ID =  8
    SO2_ID =  9
    NO2_ID = 10
    NH3_ID = 11
   HNO3_ID = 12
     OH_ID = 13
     HF_ID = 14
    HCl_ID = 15
    HBr_ID = 16
     HI_ID = 17
    ClO_ID = 18
    OCS_ID = 19
   H2CO_ID = 20
   HOCl_ID = 21
     N2_ID = 22
    HCN_ID = 23
   CH3l_ID = 24
   H2O2_ID = 25
   C2H2_ID = 26
   C2H6_ID = 27
    PH3_ID = 28
   COF2_ID = 29
    SF6_ID = 30
    H2S_ID = 31
  HCOOH_ID = 32
  ABSORBER_ID_NAME = [ 'Invalid', $
                       'H2O    ', 'CO2    ', 'O3     ', 'N2O    ', $
                       'CO     ', 'CH4    ', 'O2     ', 'NO     ', $
                       'SO2    ', 'NO2    ', 'NH3    ', 'HNO3   ', $
                       'OH     ', 'HF     ', 'HCl    ', 'HBr    ', $
                       'HI     ', 'ClO    ', 'OCS    ', 'H2CO   ', $
                       'HOCl   ', 'N2     ', 'HCN    ', 'CH3Cl  ', $
                       'H2O2   ', 'C2H2   ', 'C2H6   ', 'PH3    ', $
                       'COF2   ', 'SF6    ', 'H2S    ', 'HCOOH  ' ]

  ; The absorber units. Use LBLRTM definitions and then some.
  N_VALID_ABSORBER_UNITS = 10
  INVALID_ABSORBER_UNITS       =  0
  VOLUME_MIXING_RATIO_UNITS    =  1
  NUMBER_DENSITY_UNITS         =  2
  MASS_MIXING_RATIO_UNITS      =  3
  MASS_DENSITY_UNITS           =  4
  PARTIAL_PRESSURE_UNITS       =  5
  DEWPOINT_TEMPERATURE_K_UNITS =  6 ; H2O only
  DEWPOINT_TEMPERATURE_C_UNITS =  7 ; H2O only
  RELATIVE_HUMIDITY_UNITS      =  8 ; H2O only
  SPECIFIC_AMOUNT_UNITS        =  9
  INTEGRATED_PATH_UNITS        = 10
  ABSORBER_UNITS_NAME = [ 'Invalid units                      ', $
                          'Volume mixing ratio, ppmv          ', $
                          'Number density, cm^-3              ', $
                          'Mass mixing ratio, g/kg            ', $
                          'Mass density, g.m^-3               ', $
                          'Partial pressure, hPa              ', $
                          'Dewpoint temperature, K  (H2O ONLY)', $
                          'Dewpoint temperature, C  (H2O ONLY)', $
                          'Relative humidity, %     (H2O ONLY)', $
                          'Specific amount, g/g               ', $
                          'Integrated path, mm                '  ]
    H2O_ONLY_UNITS_FLAG = [ 0, $  ; None
                            0, $  ; Volume mixing ratio, ppmv
                            0, $  ; Number density, cm^-3
                            0, $  ; Mass mixing ratio, g/kg
                            0, $  ; Mass density, g.m^-3
                            0, $  ; Partial pressure, hPa
                            1, $  ; Dewpoint temperature, K  (H2O ONLY)
                            1, $  ; Dewpoint temperature, C  (H2O ONLY)
                            1, $  ; Relative humidity, %     (H2O ONLY)
                            0, $  ; Specific amount, g/g
                            0  ]  ; Integrated path, mm

  ; The climatology models
  N_VALID_CLIMATOLOGY_MODELS = 6
  INVALID_MODEL          = 0
  TROPICAL               = 1
  MIDLATITUDE_SUMMER     = 2
  MIDLATITUDE_WINTER     = 3
  SUBARCTIC_SUMMER       = 4
  SUBARCTIC_WINTER       = 5
  US_STANDARD_ATMOSPHERE = 6
  CLIMATOLOGY_MODEL_NAME = [ 'Invalid                 ', $
                             'Tropical                ', $
                             'Midlatitude summer      ', $
                             'Midlatitude winter      ', $
                             'Subarctic summer        ', $
                             'Subarctic winter        ', $
                             'U.S. Standard Atmosphere'  ]
