PROGRAM Create_Input_Test

  USE type_kinds
  USE error_handler
  USE LBLRTM_Input

  IMPLICIT NONE

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Create_Input_Test'
  INTEGER,         PARAMETER :: N_LEVELS       = 45
  INTEGER,         PARAMETER :: N_ABSORBERS    = 5
  INTEGER,         PARAMETER :: N_XS_ABSORBERS = 14
  REAL( fp_kind ), PARAMETER :: ONE            = 1.0_fp_kind

  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: pressure
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: temperature

  REAL( fp_kind ), DIMENSION( N_LEVELS, N_ABSORBERS ) :: absorber_amount
  CHARACTER( 1 ),  DIMENSION( N_ABSORBERS )           :: absorber_units
  INTEGER,         DIMENSION( N_ABSORBERS )           :: absorber_id

  REAL( fp_kind ) :: surface_altitude
  REAL( fp_kind ) :: begin_frequency, end_frequency

  TYPE( Calculation_Flags_type ) :: calculation_flags
  CHARACTER( 10 ),  DIMENSION( N_XS_ABSORBERS ) :: xs_absorber_name

  INTEGER :: error_status

  pressure = (/ &
    1.013e+03_fp_kind,  9.040e+02_fp_kind,  8.050e+02_fp_kind,  7.150e+02_fp_kind, &
    6.330e+02_fp_kind,  5.590e+02_fp_kind,  4.920e+02_fp_kind,  4.320e+02_fp_kind, &
    3.780e+02_fp_kind,  3.290e+02_fp_kind,  2.860e+02_fp_kind,  2.470e+02_fp_kind, &
    2.130e+02_fp_kind,  1.820e+02_fp_kind,  1.560e+02_fp_kind,  1.320e+02_fp_kind, &
    1.110e+02_fp_kind,  9.370e+01_fp_kind,  7.890e+01_fp_kind,  6.660e+01_fp_kind, &
    5.650e+01_fp_kind,  4.800e+01_fp_kind,  4.090e+01_fp_kind,  3.500e+01_fp_kind, &
    3.000e+01_fp_kind,  2.570e+01_fp_kind,  1.763e+01_fp_kind,  1.220e+01_fp_kind, &
    8.520e+00_fp_kind,  6.000e+00_fp_kind,  4.260e+00_fp_kind,  3.050e+00_fp_kind, &
    2.200e+00_fp_kind,  1.590e+00_fp_kind,  1.160e+00_fp_kind,  8.540e-01_fp_kind, &
    4.560e-01_fp_kind,  2.390e-01_fp_kind,  1.210e-01_fp_kind,  5.800e-02_fp_kind, &
    2.600e-02_fp_kind,  1.100e-02_fp_kind,  4.400e-03_fp_kind,  1.720e-03_fp_kind, &
    6.880e-04_fp_kind /)

  temperature = (/ &
    299.7_fp_kind, 293.7_fp_kind, 287.7_fp_kind, 283.7_fp_kind, &
    277.0_fp_kind, 270.3_fp_kind, 263.6_fp_kind, 257.0_fp_kind, &
    250.3_fp_kind, 243.6_fp_kind, 237.0_fp_kind, 230.1_fp_kind, &
    223.6_fp_kind, 217.0_fp_kind, 210.3_fp_kind, 203.7_fp_kind, &
    197.0_fp_kind, 194.8_fp_kind, 198.8_fp_kind, 202.7_fp_kind, &
    206.7_fp_kind, 210.7_fp_kind, 214.6_fp_kind, 217.0_fp_kind, &
    219.2_fp_kind, 221.4_fp_kind, 227.0_fp_kind, 232.3_fp_kind, &
    237.7_fp_kind, 243.1_fp_kind, 248.5_fp_kind, 254.0_fp_kind, &
    259.4_fp_kind, 264.8_fp_kind, 269.6_fp_kind, 270.2_fp_kind, &
    263.4_fp_kind, 253.1_fp_kind, 236.0_fp_kind, 218.9_fp_kind, &
    201.8_fp_kind, 184.8_fp_kind, 177.1_fp_kind, 177.0_fp_kind, &
    184.3_fp_kind /)

  absorber_amount( :, 1 ) = (/ &
    2.593e+04_fp_kind,  1.949e+04_fp_kind,  1.534e+04_fp_kind,  8.600e+03_fp_kind, &
    4.441e+03_fp_kind,  3.346e+03_fp_kind,  2.101e+03_fp_kind,  1.289e+03_fp_kind, &
    7.637e+02_fp_kind,  4.098e+02_fp_kind,  1.912e+02_fp_kind,  7.306e+01_fp_kind, &
    2.905e+01_fp_kind,  9.900e+00_fp_kind,  6.220e+00_fp_kind,  4.000e+00_fp_kind, &
    3.000e+00_fp_kind,  2.900e+00_fp_kind,  2.750e+00_fp_kind,  2.600e+00_fp_kind, &
    2.600e+00_fp_kind,  2.650e+00_fp_kind,  2.800e+00_fp_kind,  2.900e+00_fp_kind, &
    3.200e+00_fp_kind,  3.250e+00_fp_kind,  3.600e+00_fp_kind,  4.000e+00_fp_kind, &
    4.300e+00_fp_kind,  4.600e+00_fp_kind,  4.900e+00_fp_kind,  5.200e+00_fp_kind, &
    5.500e+00_fp_kind,  5.700e+00_fp_kind,  5.900e+00_fp_kind,  6.000e+00_fp_kind, &
    6.000e+00_fp_kind,  6.000e+00_fp_kind,  5.400e+00_fp_kind,  4.500e+00_fp_kind, &
    3.300e+00_fp_kind,  2.100e+00_fp_kind,  1.300e+00_fp_kind,  8.500e-01_fp_kind, &
    5.400e-01_fp_kind /)

  absorber_amount( :, 2 ) = (/ &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind,  3.700e+02_fp_kind, &
    3.700e+02_fp_kind,  3.678e+02_fp_kind,  3.588e+02_fp_kind,  3.476e+02_fp_kind, &
    3.027e+02_fp_kind /)

  absorber_amount( :, 3 ) = (/ &
    2.869e-02_fp_kind,  3.150e-02_fp_kind,  3.342e-02_fp_kind,  3.504e-02_fp_kind, &
    3.561e-02_fp_kind,  3.767e-02_fp_kind,  3.989e-02_fp_kind,  4.223e-02_fp_kind, &
    4.471e-02_fp_kind,  5.000e-02_fp_kind,  5.595e-02_fp_kind,  6.613e-02_fp_kind, &
    7.815e-02_fp_kind,  9.289e-02_fp_kind,  1.050e-01_fp_kind,  1.256e-01_fp_kind, &
    1.444e-01_fp_kind,  2.500e-01_fp_kind,  5.000e-01_fp_kind,  9.500e-01_fp_kind, &
    1.400e+00_fp_kind,  1.800e+00_fp_kind,  2.400e+00_fp_kind,  3.400e+00_fp_kind, &
    4.300e+00_fp_kind,  5.400e+00_fp_kind,  7.800e+00_fp_kind,  9.300e+00_fp_kind, &
    9.850e+00_fp_kind,  9.700e+00_fp_kind,  8.800e+00_fp_kind,  7.500e+00_fp_kind, &
    5.900e+00_fp_kind,  4.500e+00_fp_kind,  3.450e+00_fp_kind,  2.800e+00_fp_kind, &
    1.800e+00_fp_kind,  1.100e+00_fp_kind,  6.500e-01_fp_kind,  3.000e-01_fp_kind, &
    1.800e-01_fp_kind,  3.300e-01_fp_kind,  5.000e-01_fp_kind,  5.200e-01_fp_kind, &
    5.000e-01_fp_kind /)

  absorber_amount( :, 4 ) = (/ &
    1.441e-01_fp_kind,  1.301e-01_fp_kind,  1.205e-01_fp_kind,  1.185e-01_fp_kind, &
    1.300e-01_fp_kind,  1.433e-01_fp_kind,  1.394e-01_fp_kind,  1.467e-01_fp_kind, &
    9.681e-02_fp_kind,  4.414e-02_fp_kind,  4.255e-02_fp_kind,  4.168e-02_fp_kind, &
    3.936e-02_fp_kind,  4.099e-02_fp_kind,  3.844e-02_fp_kind,  3.595e-02_fp_kind, &
    3.398e-02_fp_kind,  3.555e-02_fp_kind,  2.084e-02_fp_kind,  2.102e-02_fp_kind, &
    2.016e-02_fp_kind,  1.808e-02_fp_kind,  1.507e-02_fp_kind,  1.872e-02_fp_kind, &
    1.781e-02_fp_kind,  2.180e-02_fp_kind,  1.975e-02_fp_kind,  2.079e-02_fp_kind, &
    1.893e-02_fp_kind,  2.145e-02_fp_kind,  2.706e-02_fp_kind,  3.170e-02_fp_kind, &
    3.084e-02_fp_kind,  4.384e-02_fp_kind,  5.032e-02_fp_kind,  6.413e-02_fp_kind, &
    9.016e-02_fp_kind,  1.318e-01_fp_kind,  2.354e-01_fp_kind,  3.243e-01_fp_kind, &
    7.947e-01_fp_kind,  1.873e+00_fp_kind,  3.739e+00_fp_kind,  7.558e+00_fp_kind, &
    1.412e+01_fp_kind /)

  absorber_amount( :, 5 ) = (/ &
    1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind, &
    1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.800e+00_fp_kind,  1.799e+00_fp_kind, &
    1.797e+00_fp_kind,  1.793e+00_fp_kind,  1.784e+00_fp_kind,  1.774e+00_fp_kind, &
    1.760e+00_fp_kind,  1.742e+00_fp_kind,  1.722e+00_fp_kind,  1.699e+00_fp_kind, &
    1.675e+00_fp_kind,  1.644e+00_fp_kind,  1.610e+00_fp_kind,  1.567e+00_fp_kind, &
    1.508e+00_fp_kind,  1.435e+00_fp_kind,  1.347e+00_fp_kind,  1.261e+00_fp_kind, &
    1.184e+00_fp_kind,  1.117e+00_fp_kind,  1.045e+00_fp_kind,  9.673e-01_fp_kind, &
    8.788e-01_fp_kind,  7.899e-01_fp_kind,  7.007e-01_fp_kind,  5.970e-01_fp_kind, &
    4.885e-01_fp_kind,  3.845e-01_fp_kind,  2.936e-01_fp_kind,  2.224e-01_fp_kind, &
    1.748e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind, &
    1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.588e-01_fp_kind,  1.482e-01_fp_kind, &
    1.376e-01_fp_kind /)

  absorber_units = (/ 'A', 'A', 'A', 'A', 'A' /)

  absorber_id = (/ 1, 2, 3, 5, 6 /)

  surface_altitude = 0.0_fp_kind

  begin_frequency = 820.0_fp_kind
  end_frequency   = 840.0_fp_kind

!  xs_absorber_name = (/ 'CCL4', 'F11', 'F12' /)
  xs_absorber_name = (/ 'CCL4      ','F11       ','F12       ','CHCLF2    ', &
                        'C2CL2F4   ','C2CL3F3   ','C2CLF5    ','CClF3     ', &
                        'CF4       ','CHCL2F    ','CLONO2    ','HNO4      ', &
                        'N2O5      ','HNO3      ' /)

  calculation_flags = DEFAULT_CALCULATION_FLAGS
  calculation_flags%continuum = 6
  calculation_flags%xsection  = 1

  error_status = create_LBLRTM_TAPE5( pressure, &
                                      temperature, &
                                      absorber_amount, &
                                      absorber_units, &
                                      absorber_id, &
                                      surface_altitude, &
                                      begin_frequency, &
                                      end_frequency, &
                                      calculation_flags = calculation_flags, &
                                      absorber_format = 1, &
                                      continuum_scale = (/ ONE, ONE, ONE, ONE, ONE, ONE, ONE /), &
                                      xsection_name = xs_absorber_name, &
                                      placeholder = 1 )
  IF ( error_status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error createing LBLRTM TAPE5', &
                          error_status )
    STOP
  END IF

END PROGRAM Create_Input_Test
