

MODULE Model_Profile_Set


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE Profile_Utility_Parameters
  USE Units_Conversion


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Load_Model_Profile


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Model_Profile_Set.f90,v 1.2 2006/06/30 16:47:16 dgroff Exp $'

  ! -- Private dimensions.
  INTEGER, PRIVATE, PARAMETER :: N_MODEL_LEVELS    = 50
  INTEGER, PRIVATE, PARAMETER :: N_MODEL_LAYERS    = N_MODEL_LEVELS - 1

  ! -- Public dimensions
  INTEGER, PUBLIC,  PARAMETER :: N_MODEL_ABSORBERS = 28
  INTEGER, PUBLIC,  PARAMETER :: N_MODEL_PROFILES  = 6

  ! -- Model latitudes (Northern hemisphere annual averages)
  REAL( fp_kind ), PRIVATE, PARAMETER, DIMENSION( N_MODEL_PROFILES ) :: DEFAULT_MODEL_LATITUDE = &
    (/ 15.0_fp_kind, &  ! Tropical
       45.0_fp_kind, &  ! Midlatitude summer
       45.0_fp_kind, &  ! Midlatitude winter
       60.0_fp_kind, &  ! Subarctic summer
       60.0_fp_kind, &  ! Subarctic winter
        0.0_fp_kind /)  ! US Std Atm (no specified latitude)

  ! -- Invalid value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1


CONTAINS



  FUNCTION Load_Model_Profile ( Profile,      &  ! Input
                               Level_Pressure,    &  ! Output
                               Level_Temperature, &  ! Output
                               Level_Absorber,    &  ! Output
                               Absorber_ID,       &  ! Optional output
                               Absorber_Units_ID, &  ! Optional output
                               Description,       &  ! Optional output
                               Climatology_Model, &  ! Optional output
                               Year,              &  ! Optional output
                               Month,             &  ! Optional output
                               Day,               &  ! Optional output
                               Hour,              &  ! Optional output
                               Latitude,          &  ! Optional output
                               Longitude,         &  ! Optional output
                               Surface_Altitude,  &  ! Optional output
                               RCS_Id,            &  ! Revision control
                               Message_Log )      &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                                      INTENT( IN )  :: Profile

    ! -- Output
    REAL( fp_kind ),           DIMENSION( : ),    POINTER       :: Level_Pressure
    REAL( fp_kind ),           DIMENSION( : ),    POINTER       :: Level_Temperature
    REAL( fp_kind ),           DIMENSION( :, : ), POINTER       :: Level_Absorber

    ! -- Optional output
    INTEGER,         OPTIONAL, DIMENSION( : ),    INTENT( OUT ) :: Absorber_ID
    INTEGER,         OPTIONAL, DIMENSION( : ),    INTENT( OUT ) :: Absorber_Units_ID
    CHARACTER( * ),  OPTIONAL,                    INTENT( OUT ) :: Description
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Climatology_Model
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Year
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Month
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Day
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Hour
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Latitude
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Longitude
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Surface_Altitude

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                    INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Load_Model_Profile'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status
    INTEGER :: j
    INTEGER, DIMENSION( N_MODEL_ABSORBERS ) :: Model_Absorber_Units_ID
    CHARACTER( 512 )                        :: Model_Description
    INTEGER                                 :: Model_Climatology_Model
    INTEGER                                 :: Model_Year
    INTEGER                                 :: Model_Month
    INTEGER                                 :: Model_Day
    INTEGER                                 :: Model_Hour
    REAL( fp_kind )                         :: Model_Latitude
    REAL( fp_kind )                         :: Model_Longitude
    REAL( fp_kind )                         :: Model_Surface_Altitude

    REAL( fp_kind ), DIMENSION( N_MODEL_LEVELS ) :: H2O_Mixing_Ratio



    !#--------------------------------------------------------------------------#
    !#                   -- SET A SUCCESSFUL EXIT STATUS --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Check the requested profile number
    ! ----------------------------------

    IF ( Profile < 1 .OR. Profile > N_MODEL_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid model profile number ", i5, " specified." )' ) Profile
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Check output pointers
    ! ---------------------

    ! -- Pressure
    IF ( ASSOCIATED( Level_Pressure ) ) THEN
      DEALLOCATE( Level_Pressure, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Pressure output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Temperature
    IF ( ASSOCIATED( Level_Temperature ) ) THEN
      DEALLOCATE( Level_Temperature, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Temperature output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Absorber
    IF ( ASSOCIATED( Level_Absorber ) ) THEN
      DEALLOCATE( Level_Absorber, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Absorber output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ------------------------
    ! Check output array sizes
    ! ------------------------

    ! -- Absorber ID array
    IF ( PRESENT( Absorber_ID ) ) THEN
      IF ( SIZE( Absorber_ID ) /= N_MODEL_ABSORBERS      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Size of output Absorber_ID array must be ", i1, " elements." )' ) &
                        N_MODEL_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- Absorber Units ID array
    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      IF ( SIZE( Absorber_Units_ID ) /= N_MODEL_ABSORBERS      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Size of output Absorber_Units_ID array must be ", i1, " elements." )' ) &
                        N_MODEL_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- INITIALISE LOCAL VALUES --                      #
    !#--------------------------------------------------------------------------#

    Model_Description = ' '
    Model_Climatology_Model = INVALID
    Model_Year              = INVALID
    Model_Month             = INVALID
    Model_Day               = INVALID
    Model_Hour              = INVALID
    Model_Latitude          = REAL( INVALID, fp_kind )
    Model_Longitude         = REAL( INVALID, fp_kind )
    Model_Surface_Altitude  = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                   -- ALLOCATE OUTPUT POINTER ARRAYS --                   #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Level_Pressure( N_MODEL_LEVELS ), &
              Level_Temperature( N_MODEL_LEVELS ), &
              Level_Absorber( N_MODEL_LEVELS, N_MODEL_ABSORBERS ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating output arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- FILL THE MODEL DEPENDENT ARRAYS --                  #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( Profile )


      ! --------------
      ! Tropical model
      ! --------------

      CASE ( 1 )

        Model_Description = 'Tropical'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.013e+03_fp_kind, 9.040e+02_fp_kind, 8.050e+02_fp_kind, 7.150e+02_fp_kind, 6.330e+02_fp_kind, &
          5.590e+02_fp_kind, 4.920e+02_fp_kind, 4.320e+02_fp_kind, 3.780e+02_fp_kind, 3.290e+02_fp_kind, &
          2.860e+02_fp_kind, 2.470e+02_fp_kind, 2.130e+02_fp_kind, 1.820e+02_fp_kind, 1.560e+02_fp_kind, &
          1.320e+02_fp_kind, 1.110e+02_fp_kind, 9.370e+01_fp_kind, 7.890e+01_fp_kind, 6.660e+01_fp_kind, &
          5.650e+01_fp_kind, 4.800e+01_fp_kind, 4.090e+01_fp_kind, 3.500e+01_fp_kind, 3.000e+01_fp_kind, &
          2.570e+01_fp_kind, 1.763e+01_fp_kind, 1.220e+01_fp_kind, 8.520e+00_fp_kind, 6.000e+00_fp_kind, &
          4.260e+00_fp_kind, 3.050e+00_fp_kind, 2.200e+00_fp_kind, 1.590e+00_fp_kind, 1.160e+00_fp_kind, &
          8.540e-01_fp_kind, 4.560e-01_fp_kind, 2.390e-01_fp_kind, 1.210e-01_fp_kind, 5.800e-02_fp_kind, &
          2.600e-02_fp_kind, 1.100e-02_fp_kind, 4.400e-03_fp_kind, 1.720e-03_fp_kind, 6.880e-04_fp_kind, &
          2.890e-04_fp_kind, 1.300e-04_fp_kind, 6.470e-05_fp_kind, 3.600e-05_fp_kind, 2.250e-05_fp_kind /)

        ! -- Temperature
        Level_Temperature = (/ &
          299.70_fp_kind, 293.70_fp_kind, 287.70_fp_kind, 283.70_fp_kind, 277.00_fp_kind, &
          270.30_fp_kind, 263.60_fp_kind, 257.00_fp_kind, 250.30_fp_kind, 243.60_fp_kind, &
          237.00_fp_kind, 230.10_fp_kind, 223.60_fp_kind, 217.00_fp_kind, 210.30_fp_kind, &
          203.70_fp_kind, 197.00_fp_kind, 194.80_fp_kind, 198.80_fp_kind, 202.70_fp_kind, &
          206.70_fp_kind, 210.70_fp_kind, 214.60_fp_kind, 217.00_fp_kind, 219.20_fp_kind, &
          221.40_fp_kind, 227.00_fp_kind, 232.30_fp_kind, 237.70_fp_kind, 243.10_fp_kind, &
          248.50_fp_kind, 254.00_fp_kind, 259.40_fp_kind, 264.80_fp_kind, 269.60_fp_kind, &
          270.20_fp_kind, 263.40_fp_kind, 253.10_fp_kind, 236.00_fp_kind, 218.90_fp_kind, &
          201.80_fp_kind, 184.80_fp_kind, 177.10_fp_kind, 177.00_fp_kind, 184.30_fp_kind, &
          190.70_fp_kind, 212.00_fp_kind, 241.60_fp_kind, 299.70_fp_kind, 380.00_fp_kind /)
 
        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          2.593e+04_fp_kind, 1.949e+04_fp_kind, 1.534e+04_fp_kind, 8.600e+03_fp_kind, 4.441e+03_fp_kind, &
          3.346e+03_fp_kind, 2.101e+03_fp_kind, 1.289e+03_fp_kind, 7.637e+02_fp_kind, 4.098e+02_fp_kind, &
          1.912e+02_fp_kind, 7.306e+01_fp_kind, 2.905e+01_fp_kind, 9.900e+00_fp_kind, 6.220e+00_fp_kind, &
          4.000e+00_fp_kind, 3.000e+00_fp_kind, 2.900e+00_fp_kind, 2.750e+00_fp_kind, 2.600e+00_fp_kind, &
          2.600e+00_fp_kind, 2.650e+00_fp_kind, 2.800e+00_fp_kind, 2.900e+00_fp_kind, 3.200e+00_fp_kind, &
          3.250e+00_fp_kind, 3.600e+00_fp_kind, 4.000e+00_fp_kind, 4.300e+00_fp_kind, 4.600e+00_fp_kind, &
          4.900e+00_fp_kind, 5.200e+00_fp_kind, 5.500e+00_fp_kind, 5.700e+00_fp_kind, 5.900e+00_fp_kind, &
          6.000e+00_fp_kind, 6.000e+00_fp_kind, 6.000e+00_fp_kind, 5.400e+00_fp_kind, 4.500e+00_fp_kind, &
          3.300e+00_fp_kind, 2.100e+00_fp_kind, 1.300e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)


        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          2.869e-02_fp_kind, 3.150e-02_fp_kind, 3.342e-02_fp_kind, 3.504e-02_fp_kind, 3.561e-02_fp_kind, &
          3.767e-02_fp_kind, 3.989e-02_fp_kind, 4.223e-02_fp_kind, 4.471e-02_fp_kind, 5.000e-02_fp_kind, &
          5.595e-02_fp_kind, 6.613e-02_fp_kind, 7.815e-02_fp_kind, 9.289e-02_fp_kind, 1.050e-01_fp_kind, &
          1.256e-01_fp_kind, 1.444e-01_fp_kind, 2.500e-01_fp_kind, 5.000e-01_fp_kind, 9.500e-01_fp_kind, &
          1.400e+00_fp_kind, 1.800e+00_fp_kind, 2.400e+00_fp_kind, 3.400e+00_fp_kind, 4.300e+00_fp_kind, &
          5.400e+00_fp_kind, 7.800e+00_fp_kind, 9.300e+00_fp_kind, 9.850e+00_fp_kind, 9.700e+00_fp_kind, &
          8.800e+00_fp_kind, 7.500e+00_fp_kind, 5.900e+00_fp_kind, 4.500e+00_fp_kind, 3.450e+00_fp_kind, &
          2.800e+00_fp_kind, 1.800e+00_fp_kind, 1.100e+00_fp_kind, 6.500e-01_fp_kind, 3.000e-01_fp_kind, &
          1.800e-01_fp_kind, 3.300e-01_fp_kind, 5.000e-01_fp_kind, 5.200e-01_fp_kind, 5.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, &
          3.179e-01_fp_kind, 3.140e-01_fp_kind, 3.095e-01_fp_kind, 3.048e-01_fp_kind, 2.999e-01_fp_kind, &
          2.944e-01_fp_kind, 2.877e-01_fp_kind, 2.783e-01_fp_kind, 2.671e-01_fp_kind, 2.527e-01_fp_kind, &
          2.365e-01_fp_kind, 2.194e-01_fp_kind, 2.051e-01_fp_kind, 1.967e-01_fp_kind, 1.875e-01_fp_kind, &
          1.756e-01_fp_kind, 1.588e-01_fp_kind, 1.416e-01_fp_kind, 1.165e-01_fp_kind, 9.275e-02_fp_kind, &
          6.693e-02_fp_kind, 4.513e-02_fp_kind, 2.751e-02_fp_kind, 1.591e-02_fp_kind, 9.378e-03_fp_kind, &
          4.752e-03_fp_kind, 3.000e-03_fp_kind, 2.065e-03_fp_kind, 1.507e-03_fp_kind, 1.149e-03_fp_kind, &
          8.890e-04_fp_kind, 7.056e-04_fp_kind, 5.716e-04_fp_kind, 4.708e-04_fp_kind, 3.932e-04_fp_kind, &
          3.323e-04_fp_kind, 2.837e-04_fp_kind, 2.443e-04_fp_kind, 2.120e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.521e-02_fp_kind, 1.722e-02_fp_kind, 1.995e-02_fp_kind, 2.266e-02_fp_kind, 2.487e-02_fp_kind, &
          2.738e-02_fp_kind, 3.098e-02_fp_kind, 3.510e-02_fp_kind, 3.987e-02_fp_kind, 4.482e-02_fp_kind, &
          5.092e-02_fp_kind, 5.985e-02_fp_kind, 6.960e-02_fp_kind, 9.188e-02_fp_kind, 1.938e-01_fp_kind, &
          5.688e-01_fp_kind, 1.549e+00_fp_kind, 3.849e+00_fp_kind, 6.590e+00_fp_kind, 1.044e+01_fp_kind, &
          1.705e+01_fp_kind, 2.471e+01_fp_kind, 3.358e+01_fp_kind, 4.148e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.699e+00_fp_kind, 1.697e+00_fp_kind, 1.693e+00_fp_kind, &
          1.685e+00_fp_kind, 1.675e+00_fp_kind, 1.662e+00_fp_kind, 1.645e+00_fp_kind, 1.626e+00_fp_kind, &
          1.605e+00_fp_kind, 1.582e+00_fp_kind, 1.553e+00_fp_kind, 1.521e+00_fp_kind, 1.480e+00_fp_kind, &
          1.424e+00_fp_kind, 1.355e+00_fp_kind, 1.272e+00_fp_kind, 1.191e+00_fp_kind, 1.118e+00_fp_kind, &
          1.055e+00_fp_kind, 9.870e-01_fp_kind, 9.136e-01_fp_kind, 8.300e-01_fp_kind, 7.460e-01_fp_kind, &
          6.618e-01_fp_kind, 5.638e-01_fp_kind, 4.614e-01_fp_kind, 3.631e-01_fp_kind, 2.773e-01_fp_kind, &
          2.100e-01_fp_kind, 1.651e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)


      ! ------------------------
      ! Midlatitude summer model
      ! ------------------------

      CASE ( 2 )

        Model_Description = 'Midlatitude Summer'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.013e+03_fp_kind, 9.020e+02_fp_kind, 8.020e+02_fp_kind, 7.100e+02_fp_kind, 6.280e+02_fp_kind, &
          5.540e+02_fp_kind, 4.870e+02_fp_kind, 4.260e+02_fp_kind, 3.720e+02_fp_kind, 3.240e+02_fp_kind, &
          2.810e+02_fp_kind, 2.430e+02_fp_kind, 2.090e+02_fp_kind, 1.790e+02_fp_kind, 1.530e+02_fp_kind, &
          1.300e+02_fp_kind, 1.110e+02_fp_kind, 9.500e+01_fp_kind, 8.120e+01_fp_kind, 6.950e+01_fp_kind, &
          5.950e+01_fp_kind, 5.100e+01_fp_kind, 4.370e+01_fp_kind, 3.760e+01_fp_kind, 3.220e+01_fp_kind, &
          2.770e+01_fp_kind, 1.907e+01_fp_kind, 1.320e+01_fp_kind, 9.300e+00_fp_kind, 6.520e+00_fp_kind, &
          4.640e+00_fp_kind, 3.330e+00_fp_kind, 2.410e+00_fp_kind, 1.760e+00_fp_kind, 1.290e+00_fp_kind, &
          9.510e-01_fp_kind, 5.150e-01_fp_kind, 2.720e-01_fp_kind, 1.390e-01_fp_kind, 6.700e-02_fp_kind, &
          3.000e-02_fp_kind, 1.200e-02_fp_kind, 4.480e-03_fp_kind, 1.640e-03_fp_kind, 6.250e-04_fp_kind, &
          2.580e-04_fp_kind, 1.170e-04_fp_kind, 6.110e-05_fp_kind, 3.560e-05_fp_kind, 2.270e-05_fp_kind /)
 
        ! -- Temperature
        Level_Temperature = (/ &
          294.20_fp_kind, 289.70_fp_kind, 285.20_fp_kind, 279.20_fp_kind, 273.20_fp_kind, &
          267.20_fp_kind, 261.20_fp_kind, 254.70_fp_kind, 248.20_fp_kind, 241.70_fp_kind, &
          235.30_fp_kind, 228.80_fp_kind, 222.30_fp_kind, 215.80_fp_kind, 215.70_fp_kind, &
          215.70_fp_kind, 215.70_fp_kind, 215.70_fp_kind, 216.80_fp_kind, 217.90_fp_kind, &
          219.20_fp_kind, 220.40_fp_kind, 221.60_fp_kind, 222.80_fp_kind, 223.90_fp_kind, &
          225.10_fp_kind, 228.45_fp_kind, 233.70_fp_kind, 239.00_fp_kind, 245.20_fp_kind, &
          251.30_fp_kind, 257.50_fp_kind, 263.70_fp_kind, 269.90_fp_kind, 275.20_fp_kind, &
          275.70_fp_kind, 269.30_fp_kind, 257.10_fp_kind, 240.10_fp_kind, 218.10_fp_kind, &
          196.10_fp_kind, 174.10_fp_kind, 165.10_fp_kind, 165.00_fp_kind, 178.30_fp_kind, &
          190.50_fp_kind, 222.20_fp_kind, 262.40_fp_kind, 316.80_fp_kind, 380.00_fp_kind /)

        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          1.876e+04_fp_kind, 1.378e+04_fp_kind, 9.680e+03_fp_kind, 5.984e+03_fp_kind, 3.813e+03_fp_kind, &
          2.225e+03_fp_kind, 1.510e+03_fp_kind, 1.020e+03_fp_kind, 6.464e+02_fp_kind, 4.129e+02_fp_kind, &
          2.472e+02_fp_kind, 9.556e+01_fp_kind, 2.944e+01_fp_kind, 8.000e+00_fp_kind, 5.000e+00_fp_kind, &
          3.400e+00_fp_kind, 3.300e+00_fp_kind, 3.200e+00_fp_kind, 3.150e+00_fp_kind, 3.200e+00_fp_kind, &
          3.300e+00_fp_kind, 3.450e+00_fp_kind, 3.600e+00_fp_kind, 3.850e+00_fp_kind, 4.000e+00_fp_kind, &
          4.200e+00_fp_kind, 4.450e+00_fp_kind, 4.700e+00_fp_kind, 4.850e+00_fp_kind, 4.950e+00_fp_kind, &
          5.000e+00_fp_kind, 5.100e+00_fp_kind, 5.300e+00_fp_kind, 5.450e+00_fp_kind, 5.500e+00_fp_kind, &
          5.500e+00_fp_kind, 5.350e+00_fp_kind, 5.000e+00_fp_kind, 4.400e+00_fp_kind, 3.700e+00_fp_kind, &
          2.950e+00_fp_kind, 2.100e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          3.017e-02_fp_kind, 3.337e-02_fp_kind, 3.694e-02_fp_kind, 4.222e-02_fp_kind, 4.821e-02_fp_kind, &
          5.512e-02_fp_kind, 6.408e-02_fp_kind, 7.764e-02_fp_kind, 9.126e-02_fp_kind, 1.111e-01_fp_kind, &
          1.304e-01_fp_kind, 1.793e-01_fp_kind, 2.230e-01_fp_kind, 3.000e-01_fp_kind, 4.400e-01_fp_kind, &
          5.000e-01_fp_kind, 6.000e-01_fp_kind, 7.000e-01_fp_kind, 1.000e+00_fp_kind, 1.500e+00_fp_kind, &
          2.000e+00_fp_kind, 2.400e+00_fp_kind, 2.900e+00_fp_kind, 3.400e+00_fp_kind, 4.000e+00_fp_kind, &
          4.800e+00_fp_kind, 6.000e+00_fp_kind, 7.000e+00_fp_kind, 8.100e+00_fp_kind, 8.900e+00_fp_kind, &
          8.700e+00_fp_kind, 7.550e+00_fp_kind, 5.900e+00_fp_kind, 4.500e+00_fp_kind, 3.500e+00_fp_kind, &
          2.800e+00_fp_kind, 1.800e+00_fp_kind, 1.300e+00_fp_kind, 8.000e-01_fp_kind, 4.000e-01_fp_kind, &
          1.900e-01_fp_kind, 2.000e-01_fp_kind, 5.700e-01_fp_kind, 7.500e-01_fp_kind, 7.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, 3.163e-01_fp_kind, &
          3.096e-01_fp_kind, 2.989e-01_fp_kind, 2.936e-01_fp_kind, 2.860e-01_fp_kind, 2.800e-01_fp_kind, &
          2.724e-01_fp_kind, 2.611e-01_fp_kind, 2.421e-01_fp_kind, 2.174e-01_fp_kind, 1.843e-01_fp_kind, &
          1.607e-01_fp_kind, 1.323e-01_fp_kind, 1.146e-01_fp_kind, 1.035e-01_fp_kind, 9.622e-02_fp_kind, &
          8.958e-02_fp_kind, 8.006e-02_fp_kind, 6.698e-02_fp_kind, 4.958e-02_fp_kind, 3.695e-02_fp_kind, &
          2.519e-02_fp_kind, 1.736e-02_fp_kind, 1.158e-02_fp_kind, 7.665e-03_fp_kind, 5.321e-03_fp_kind, &
          3.215e-03_fp_kind, 2.030e-03_fp_kind, 1.397e-03_fp_kind, 1.020e-03_fp_kind, 7.772e-04_fp_kind, &
          6.257e-04_fp_kind, 5.166e-04_fp_kind, 4.352e-04_fp_kind, 3.727e-04_fp_kind, 3.237e-04_fp_kind, &
          2.844e-04_fp_kind, 2.524e-04_fp_kind, 2.260e-04_fp_kind, 2.039e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.521e-02_fp_kind, 1.722e-02_fp_kind, 1.995e-02_fp_kind, 2.266e-02_fp_kind, 2.487e-02_fp_kind, &
          2.716e-02_fp_kind, 2.962e-02_fp_kind, 3.138e-02_fp_kind, 3.307e-02_fp_kind, 3.487e-02_fp_kind, &
          3.645e-02_fp_kind, 3.923e-02_fp_kind, 4.673e-02_fp_kind, 6.404e-02_fp_kind, 1.177e-01_fp_kind, &
          2.935e-01_fp_kind, 6.815e-01_fp_kind, 1.465e+00_fp_kind, 2.849e+00_fp_kind, 5.166e+00_fp_kind, &
          1.008e+01_fp_kind, 1.865e+01_fp_kind, 2.863e+01_fp_kind, 3.890e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.697e+00_fp_kind, &
          1.687e+00_fp_kind, 1.672e+00_fp_kind, 1.649e+00_fp_kind, 1.629e+00_fp_kind, 1.615e+00_fp_kind, &
          1.579e+00_fp_kind, 1.542e+00_fp_kind, 1.508e+00_fp_kind, 1.479e+00_fp_kind, 1.451e+00_fp_kind, &
          1.422e+00_fp_kind, 1.390e+00_fp_kind, 1.356e+00_fp_kind, 1.323e+00_fp_kind, 1.281e+00_fp_kind, &
          1.224e+00_fp_kind, 1.154e+00_fp_kind, 1.066e+00_fp_kind, 9.730e-01_fp_kind, 8.800e-01_fp_kind, &
          7.888e-01_fp_kind, 7.046e-01_fp_kind, 6.315e-01_fp_kind, 5.592e-01_fp_kind, 5.008e-01_fp_kind, &
          4.453e-01_fp_kind, 3.916e-01_fp_kind, 3.389e-01_fp_kind, 2.873e-01_fp_kind, 2.384e-01_fp_kind, &
          1.944e-01_fp_kind, 1.574e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)


      ! ------------------------
      ! Midlatitude winter model
      ! ------------------------

      CASE ( 3 )

        Model_Description = 'Midlatitude Winter'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.018e+03_fp_kind, 8.973e+02_fp_kind, 7.897e+02_fp_kind, 6.938e+02_fp_kind, 6.081e+02_fp_kind, &
          5.313e+02_fp_kind, 4.627e+02_fp_kind, 4.016e+02_fp_kind, 3.473e+02_fp_kind, 2.993e+02_fp_kind, &
          2.568e+02_fp_kind, 2.199e+02_fp_kind, 1.882e+02_fp_kind, 1.611e+02_fp_kind, 1.378e+02_fp_kind, &
          1.178e+02_fp_kind, 1.007e+02_fp_kind, 8.610e+01_fp_kind, 7.360e+01_fp_kind, 6.280e+01_fp_kind, &
          5.370e+01_fp_kind, 4.580e+01_fp_kind, 3.910e+01_fp_kind, 3.340e+01_fp_kind, 2.860e+01_fp_kind, &
          2.440e+01_fp_kind, 1.646e+01_fp_kind, 1.110e+01_fp_kind, 7.560e+00_fp_kind, 5.180e+00_fp_kind, &
          3.600e+00_fp_kind, 2.530e+00_fp_kind, 1.800e+00_fp_kind, 1.290e+00_fp_kind, 9.400e-01_fp_kind, &
          6.830e-01_fp_kind, 3.620e-01_fp_kind, 1.880e-01_fp_kind, 9.500e-02_fp_kind, 4.700e-02_fp_kind, &
          2.220e-02_fp_kind, 1.030e-02_fp_kind, 4.560e-03_fp_kind, 1.980e-03_fp_kind, 8.770e-04_fp_kind, &
          4.074e-04_fp_kind, 2.000e-04_fp_kind, 1.057e-04_fp_kind, 5.980e-05_fp_kind, 3.600e-05_fp_kind /)
 
        ! -- Temperature
        Level_Temperature = (/ &
          272.20_fp_kind, 268.70_fp_kind, 265.20_fp_kind, 261.70_fp_kind, 255.70_fp_kind, &
          249.70_fp_kind, 243.70_fp_kind, 237.70_fp_kind, 231.70_fp_kind, 225.70_fp_kind, &
          219.70_fp_kind, 219.20_fp_kind, 218.70_fp_kind, 218.20_fp_kind, 217.70_fp_kind, &
          217.20_fp_kind, 216.70_fp_kind, 216.20_fp_kind, 215.70_fp_kind, 215.20_fp_kind, &
          215.20_fp_kind, 215.20_fp_kind, 215.20_fp_kind, 215.20_fp_kind, 215.20_fp_kind, &
          215.20_fp_kind, 215.50_fp_kind, 217.40_fp_kind, 220.40_fp_kind, 227.90_fp_kind, &
          235.50_fp_kind, 243.20_fp_kind, 250.80_fp_kind, 258.50_fp_kind, 265.10_fp_kind, &
          265.70_fp_kind, 260.60_fp_kind, 250.80_fp_kind, 240.90_fp_kind, 230.70_fp_kind, &
          220.40_fp_kind, 210.10_fp_kind, 199.80_fp_kind, 199.50_fp_kind, 208.30_fp_kind, &
          218.60_fp_kind, 237.10_fp_kind, 259.50_fp_kind, 293.00_fp_kind, 333.00_fp_kind /)

        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          4.316e+03_fp_kind, 3.454e+03_fp_kind, 2.788e+03_fp_kind, 2.088e+03_fp_kind, 1.280e+03_fp_kind, &
          8.241e+02_fp_kind, 5.103e+02_fp_kind, 2.321e+02_fp_kind, 1.077e+02_fp_kind, 5.566e+01_fp_kind, &
          2.960e+01_fp_kind, 1.000e+01_fp_kind, 6.000e+00_fp_kind, 5.000e+00_fp_kind, 4.800e+00_fp_kind, &
          4.700e+00_fp_kind, 4.600e+00_fp_kind, 4.500e+00_fp_kind, 4.500e+00_fp_kind, 4.500e+00_fp_kind, &
          4.500e+00_fp_kind, 4.500e+00_fp_kind, 4.530e+00_fp_kind, 4.550e+00_fp_kind, 4.600e+00_fp_kind, &
          4.650e+00_fp_kind, 4.700e+00_fp_kind, 4.750e+00_fp_kind, 4.800e+00_fp_kind, 4.850e+00_fp_kind, &
          4.900e+00_fp_kind, 4.950e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, &
          4.950e+00_fp_kind, 4.850e+00_fp_kind, 4.500e+00_fp_kind, 4.000e+00_fp_kind, 3.300e+00_fp_kind, &
          2.700e+00_fp_kind, 2.000e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          2.778e-02_fp_kind, 2.800e-02_fp_kind, 2.849e-02_fp_kind, 3.200e-02_fp_kind, 3.567e-02_fp_kind, &
          4.720e-02_fp_kind, 5.837e-02_fp_kind, 7.891e-02_fp_kind, 1.039e-01_fp_kind, 1.567e-01_fp_kind, &
          2.370e-01_fp_kind, 3.624e-01_fp_kind, 5.232e-01_fp_kind, 7.036e-01_fp_kind, 8.000e-01_fp_kind, &
          9.000e-01_fp_kind, 1.100e+00_fp_kind, 1.400e+00_fp_kind, 1.800e+00_fp_kind, 2.300e+00_fp_kind, &
          2.900e+00_fp_kind, 3.500e+00_fp_kind, 3.900e+00_fp_kind, 4.300e+00_fp_kind, 4.700e+00_fp_kind, &
          5.100e+00_fp_kind, 5.600e+00_fp_kind, 6.100e+00_fp_kind, 6.800e+00_fp_kind, 7.100e+00_fp_kind, &
          7.200e+00_fp_kind, 6.900e+00_fp_kind, 5.900e+00_fp_kind, 4.600e+00_fp_kind, 3.700e+00_fp_kind, &
          2.750e+00_fp_kind, 1.700e+00_fp_kind, 1.000e-00_fp_kind, 5.500e-01_fp_kind, 3.200e-01_fp_kind, &
          2.500e-01_fp_kind, 2.300e-01_fp_kind, 5.500e-01_fp_kind, 8.000e-01_fp_kind, 8.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, 3.163e-01_fp_kind, &
          3.096e-01_fp_kind, 2.989e-01_fp_kind, 2.936e-01_fp_kind, 2.860e-01_fp_kind, 2.800e-01_fp_kind, &
          2.724e-01_fp_kind, 2.611e-01_fp_kind, 2.421e-01_fp_kind, 2.174e-01_fp_kind, 1.843e-01_fp_kind, &
          1.621e-01_fp_kind, 1.362e-01_fp_kind, 1.230e-01_fp_kind, 1.124e-01_fp_kind, 1.048e-01_fp_kind, &
          9.661e-02_fp_kind, 8.693e-02_fp_kind, 7.524e-02_fp_kind, 6.126e-02_fp_kind, 5.116e-02_fp_kind, &
          3.968e-02_fp_kind, 2.995e-02_fp_kind, 2.080e-02_fp_kind, 1.311e-02_fp_kind, 8.071e-03_fp_kind, &
          4.164e-03_fp_kind, 2.629e-03_fp_kind, 1.809e-03_fp_kind, 1.321e-03_fp_kind, 1.007e-03_fp_kind, &
          7.883e-04_fp_kind, 6.333e-04_fp_kind, 5.194e-04_fp_kind, 4.333e-04_fp_kind, 3.666e-04_fp_kind, &
          3.140e-04_fp_kind, 2.717e-04_fp_kind, 2.373e-04_fp_kind, 2.089e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.498e-02_fp_kind, 1.598e-02_fp_kind, 1.710e-02_fp_kind, 1.850e-02_fp_kind, 1.997e-02_fp_kind, &
          2.147e-02_fp_kind, 2.331e-02_fp_kind, 2.622e-02_fp_kind, 3.057e-02_fp_kind, 3.803e-02_fp_kind, &
          6.245e-02_fp_kind, 1.480e-01_fp_kind, 2.926e-01_fp_kind, 5.586e-01_fp_kind, 1.078e+00_fp_kind, &
          1.897e+00_fp_kind, 2.960e+00_fp_kind, 4.526e+00_fp_kind, 6.862e+00_fp_kind, 1.054e+01_fp_kind, &
          1.709e+01_fp_kind, 2.473e+01_fp_kind, 3.359e+01_fp_kind, 4.149e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.697e+00_fp_kind, &
          1.687e+00_fp_kind, 1.672e+00_fp_kind, 1.649e+00_fp_kind, 1.629e+00_fp_kind, 1.615e+00_fp_kind, &
          1.579e+00_fp_kind, 1.542e+00_fp_kind, 1.508e+00_fp_kind, 1.479e+00_fp_kind, 1.451e+00_fp_kind, &
          1.422e+00_fp_kind, 1.390e+00_fp_kind, 1.356e+00_fp_kind, 1.323e+00_fp_kind, 1.281e+00_fp_kind, &
          1.224e+00_fp_kind, 1.154e+00_fp_kind, 1.066e+00_fp_kind, 9.730e-01_fp_kind, 8.800e-01_fp_kind, &
          7.931e-01_fp_kind, 7.130e-01_fp_kind, 6.438e-01_fp_kind, 5.746e-01_fp_kind, 5.050e-01_fp_kind, &
          4.481e-01_fp_kind, 3.931e-01_fp_kind, 3.395e-01_fp_kind, 2.876e-01_fp_kind, 2.386e-01_fp_kind, &
          1.944e-01_fp_kind, 1.574e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)


      ! ----------------------
      ! Subarctic summer model
      ! ----------------------

      CASE ( 4 )

        Model_Description = 'Subarctic Summer'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.010e+03_fp_kind, 8.960e+02_fp_kind, 7.929e+02_fp_kind, 7.000e+02_fp_kind, 6.160e+02_fp_kind, &
          5.410e+02_fp_kind, 4.740e+02_fp_kind, 4.130e+02_fp_kind, 3.590e+02_fp_kind, 3.108e+02_fp_kind, &
          2.677e+02_fp_kind, 2.300e+02_fp_kind, 1.977e+02_fp_kind, 1.700e+02_fp_kind, 1.460e+02_fp_kind, &
          1.260e+02_fp_kind, 1.080e+02_fp_kind, 9.280e+01_fp_kind, 7.980e+01_fp_kind, 6.860e+01_fp_kind, &
          5.900e+01_fp_kind, 5.070e+01_fp_kind, 4.360e+01_fp_kind, 3.750e+01_fp_kind, 3.228e+01_fp_kind, &
          2.780e+01_fp_kind, 1.923e+01_fp_kind, 1.340e+01_fp_kind, 9.400e+00_fp_kind, 6.610e+00_fp_kind, &
          4.720e+00_fp_kind, 3.400e+00_fp_kind, 2.480e+00_fp_kind, 1.820e+00_fp_kind, 1.340e+00_fp_kind, &
          9.870e-01_fp_kind, 5.370e-01_fp_kind, 2.880e-01_fp_kind, 1.470e-01_fp_kind, 7.100e-02_fp_kind, &
          3.200e-02_fp_kind, 1.250e-02_fp_kind, 4.510e-03_fp_kind, 1.610e-03_fp_kind, 6.060e-04_fp_kind, &
          2.480e-04_fp_kind, 1.130e-04_fp_kind, 6.000e-05_fp_kind, 3.540e-05_fp_kind, 2.260e-05_fp_kind /)
 
        ! -- Temperature
        Level_Temperature = (/ &
          287.20_fp_kind, 281.70_fp_kind, 276.30_fp_kind, 270.90_fp_kind, 265.50_fp_kind, &
          260.10_fp_kind, 253.10_fp_kind, 246.10_fp_kind, 239.20_fp_kind, 232.20_fp_kind, &
          225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, &
          225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, &
          225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 225.20_fp_kind, 226.60_fp_kind, &
          228.10_fp_kind, 231.00_fp_kind, 235.10_fp_kind, 240.00_fp_kind, 247.20_fp_kind, &
          254.60_fp_kind, 262.10_fp_kind, 269.50_fp_kind, 273.60_fp_kind, 276.20_fp_kind, &
          277.20_fp_kind, 274.00_fp_kind, 262.70_fp_kind, 239.70_fp_kind, 216.60_fp_kind, &
          193.60_fp_kind, 170.60_fp_kind, 161.70_fp_kind, 161.60_fp_kind, 176.80_fp_kind, &
          190.40_fp_kind, 226.00_fp_kind, 270.10_fp_kind, 322.70_fp_kind, 380.00_fp_kind /)

        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          1.194e+04_fp_kind, 8.701e+03_fp_kind, 6.750e+03_fp_kind, 4.820e+03_fp_kind, 3.380e+03_fp_kind, &
          2.218e+03_fp_kind, 1.330e+03_fp_kind, 7.971e+02_fp_kind, 3.996e+02_fp_kind, 1.300e+02_fp_kind, &
          4.240e+01_fp_kind, 1.330e+01_fp_kind, 6.000e+00_fp_kind, 4.450e+00_fp_kind, 4.000e+00_fp_kind, &
          4.000e+00_fp_kind, 4.000e+00_fp_kind, 4.050e+00_fp_kind, 4.300e+00_fp_kind, 4.500e+00_fp_kind, &
          4.600e+00_fp_kind, 4.700e+00_fp_kind, 4.800e+00_fp_kind, 4.830e+00_fp_kind, 4.850e+00_fp_kind, &
          4.900e+00_fp_kind, 4.950e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, &
          5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, &
          4.950e+00_fp_kind, 4.850e+00_fp_kind, 4.500e+00_fp_kind, 4.000e+00_fp_kind, 3.300e+00_fp_kind, &
          2.700e+00_fp_kind, 2.000e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          2.412e-02_fp_kind, 2.940e-02_fp_kind, 3.379e-02_fp_kind, 3.887e-02_fp_kind, 4.478e-02_fp_kind, &
          5.328e-02_fp_kind, 6.564e-02_fp_kind, 7.738e-02_fp_kind, 9.114e-02_fp_kind, 1.420e-01_fp_kind, &
          1.890e-01_fp_kind, 3.050e-01_fp_kind, 4.100e-01_fp_kind, 5.000e-01_fp_kind, 6.000e-01_fp_kind, &
          7.000e-01_fp_kind, 8.500e-01_fp_kind, 1.000e+00_fp_kind, 1.300e+00_fp_kind, 1.700e+00_fp_kind, &
          2.100e+00_fp_kind, 2.700e+00_fp_kind, 3.300e+00_fp_kind, 3.700e+00_fp_kind, 4.200e+00_fp_kind, &
          4.500e+00_fp_kind, 5.300e+00_fp_kind, 5.700e+00_fp_kind, 6.900e+00_fp_kind, 7.700e+00_fp_kind, &
          7.800e+00_fp_kind, 7.000e+00_fp_kind, 5.400e+00_fp_kind, 4.200e+00_fp_kind, 3.200e+00_fp_kind, &
          2.500e+00_fp_kind, 1.700e+00_fp_kind, 1.200e+00_fp_kind, 8.000e-01_fp_kind, 4.000e-01_fp_kind, &
          2.000e-01_fp_kind, 1.800e-01_fp_kind, 6.500e-01_fp_kind, 9.000e-01_fp_kind, 8.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.100e-01_fp_kind, 3.100e-01_fp_kind, 3.100e-01_fp_kind, 3.100e-01_fp_kind, 3.079e-01_fp_kind, &
          3.024e-01_fp_kind, 2.906e-01_fp_kind, 2.822e-01_fp_kind, 2.759e-01_fp_kind, 2.703e-01_fp_kind, &
          2.651e-01_fp_kind, 2.600e-01_fp_kind, 2.549e-01_fp_kind, 2.494e-01_fp_kind, 2.433e-01_fp_kind, &
          2.355e-01_fp_kind, 2.282e-01_fp_kind, 2.179e-01_fp_kind, 2.035e-01_fp_kind, 1.817e-01_fp_kind, &
          1.567e-01_fp_kind, 1.350e-01_fp_kind, 1.218e-01_fp_kind, 1.102e-01_fp_kind, 9.893e-02_fp_kind, &
          8.775e-02_fp_kind, 7.327e-02_fp_kind, 5.941e-02_fp_kind, 4.154e-02_fp_kind, 3.032e-02_fp_kind, &
          1.949e-02_fp_kind, 1.274e-02_fp_kind, 9.001e-03_fp_kind, 6.286e-03_fp_kind, 4.558e-03_fp_kind, &
          2.795e-03_fp_kind, 1.765e-03_fp_kind, 1.214e-03_fp_kind, 8.866e-04_fp_kind, 6.756e-04_fp_kind, &
          5.538e-04_fp_kind, 4.649e-04_fp_kind, 3.979e-04_fp_kind, 3.459e-04_fp_kind, 3.047e-04_fp_kind, &
          2.713e-04_fp_kind, 2.439e-04_fp_kind, 2.210e-04_fp_kind, 2.017e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.510e-02_fp_kind, 1.649e-02_fp_kind, 1.808e-02_fp_kind, 1.997e-02_fp_kind, 2.183e-02_fp_kind, &
          2.343e-02_fp_kind, 2.496e-02_fp_kind, 2.647e-02_fp_kind, 2.809e-02_fp_kind, 2.999e-02_fp_kind, &
          3.220e-02_fp_kind, 3.650e-02_fp_kind, 4.589e-02_fp_kind, 6.375e-02_fp_kind, 1.176e-01_fp_kind, &
          3.033e-01_fp_kind, 7.894e-01_fp_kind, 1.823e+00_fp_kind, 3.402e+00_fp_kind, 5.916e+00_fp_kind, &
          1.043e+01_fp_kind, 1.881e+01_fp_kind, 2.869e+01_fp_kind, 3.892e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.697e+00_fp_kind, &
          1.687e+00_fp_kind, 1.672e+00_fp_kind, 1.649e+00_fp_kind, 1.629e+00_fp_kind, 1.615e+00_fp_kind, &
          1.579e+00_fp_kind, 1.542e+00_fp_kind, 1.506e+00_fp_kind, 1.471e+00_fp_kind, 1.434e+00_fp_kind, &
          1.389e+00_fp_kind, 1.342e+00_fp_kind, 1.290e+00_fp_kind, 1.230e+00_fp_kind, 1.157e+00_fp_kind, &
          1.072e+00_fp_kind, 9.903e-01_fp_kind, 9.170e-01_fp_kind, 8.574e-01_fp_kind, 8.013e-01_fp_kind, &
          7.477e-01_fp_kind, 6.956e-01_fp_kind, 6.442e-01_fp_kind, 5.888e-01_fp_kind, 5.240e-01_fp_kind, &
          4.506e-01_fp_kind, 3.708e-01_fp_kind, 2.992e-01_fp_kind, 2.445e-01_fp_kind, 2.000e-01_fp_kind, &
          1.660e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)



      ! ----------------------
      ! Subarctic winter model
      ! ----------------------

      CASE ( 5 )

        Model_Description = 'Subarctic Winter'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.013e+03_fp_kind, 8.878e+02_fp_kind, 7.775e+02_fp_kind, 6.798e+02_fp_kind, 5.932e+02_fp_kind, &
          5.158e+02_fp_kind, 4.467e+02_fp_kind, 3.853e+02_fp_kind, 3.308e+02_fp_kind, 2.829e+02_fp_kind, &
          2.418e+02_fp_kind, 2.067e+02_fp_kind, 1.766e+02_fp_kind, 1.510e+02_fp_kind, 1.291e+02_fp_kind, &
          1.103e+02_fp_kind, 9.431e+01_fp_kind, 8.058e+01_fp_kind, 6.882e+01_fp_kind, 5.875e+01_fp_kind, &
          5.014e+01_fp_kind, 4.277e+01_fp_kind, 3.647e+01_fp_kind, 3.109e+01_fp_kind, 2.649e+01_fp_kind, &
          2.256e+01_fp_kind, 1.513e+01_fp_kind, 1.020e+01_fp_kind, 6.910e+00_fp_kind, 4.701e+00_fp_kind, &
          3.230e+00_fp_kind, 2.243e+00_fp_kind, 1.570e+00_fp_kind, 1.113e+00_fp_kind, 7.900e-01_fp_kind, &
          5.719e-01_fp_kind, 2.990e-01_fp_kind, 1.550e-01_fp_kind, 7.900e-02_fp_kind, 4.000e-02_fp_kind, &
          2.000e-02_fp_kind, 9.660e-03_fp_kind, 4.500e-03_fp_kind, 2.022e-03_fp_kind, 9.070e-04_fp_kind, &
          4.230e-04_fp_kind, 2.070e-04_fp_kind, 1.080e-04_fp_kind, 6.000e-05_fp_kind, 3.590e-05_fp_kind /)
 
        ! -- Temperature
        Level_Temperature = (/ &
          257.20_fp_kind, 259.10_fp_kind, 255.90_fp_kind, 252.70_fp_kind, 247.70_fp_kind, &
          240.90_fp_kind, 234.10_fp_kind, 227.30_fp_kind, 220.60_fp_kind, 217.20_fp_kind, &
          217.20_fp_kind, 217.20_fp_kind, 217.20_fp_kind, 217.20_fp_kind, 217.20_fp_kind, &
          217.20_fp_kind, 216.60_fp_kind, 216.00_fp_kind, 215.40_fp_kind, 214.80_fp_kind, &
          214.20_fp_kind, 213.60_fp_kind, 213.00_fp_kind, 212.40_fp_kind, 211.80_fp_kind, &
          211.20_fp_kind, 213.60_fp_kind, 216.00_fp_kind, 218.50_fp_kind, 222.30_fp_kind, &
          228.50_fp_kind, 234.70_fp_kind, 240.80_fp_kind, 247.00_fp_kind, 253.20_fp_kind, &
          259.30_fp_kind, 259.10_fp_kind, 250.90_fp_kind, 248.40_fp_kind, 245.40_fp_kind, &
          234.70_fp_kind, 223.90_fp_kind, 213.10_fp_kind, 202.30_fp_kind, 211.00_fp_kind, &
          218.50_fp_kind, 234.00_fp_kind, 252.60_fp_kind, 288.50_fp_kind, 333.00_fp_kind /)

        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          1.405e+03_fp_kind, 1.615e+03_fp_kind, 1.427e+03_fp_kind, 1.166e+03_fp_kind, 7.898e+02_fp_kind, &
          4.309e+02_fp_kind, 2.369e+02_fp_kind, 1.470e+02_fp_kind, 3.384e+01_fp_kind, 2.976e+01_fp_kind, &
          2.000e+01_fp_kind, 1.000e+01_fp_kind, 6.000e+00_fp_kind, 4.450e+00_fp_kind, 4.500e+00_fp_kind, &
          4.550e+00_fp_kind, 4.600e+00_fp_kind, 4.650e+00_fp_kind, 4.700e+00_fp_kind, 4.750e+00_fp_kind, &
          4.800e+00_fp_kind, 4.850e+00_fp_kind, 4.900e+00_fp_kind, 4.950e+00_fp_kind, 5.000e+00_fp_kind, &
          5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, &
          5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, 5.000e+00_fp_kind, &
          4.950e+00_fp_kind, 4.850e+00_fp_kind, 4.500e+00_fp_kind, 4.000e+00_fp_kind, 3.300e+00_fp_kind, &
          2.700e+00_fp_kind, 2.000e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          1.802e-02_fp_kind, 2.072e-02_fp_kind, 2.336e-02_fp_kind, 2.767e-02_fp_kind, 3.253e-02_fp_kind, &
          3.801e-02_fp_kind, 4.446e-02_fp_kind, 7.252e-02_fp_kind, 1.040e-01_fp_kind, 2.100e-01_fp_kind, &
          3.000e-01_fp_kind, 3.500e-01_fp_kind, 4.000e-01_fp_kind, 6.500e-01_fp_kind, 9.000e-01_fp_kind, &
          1.200e+00_fp_kind, 1.500e+00_fp_kind, 1.900e+00_fp_kind, 2.450e+00_fp_kind, 3.100e+00_fp_kind, &
          3.700e+00_fp_kind, 4.000e+00_fp_kind, 4.200e+00_fp_kind, 4.500e+00_fp_kind, 4.600e+00_fp_kind, &
          4.700e+00_fp_kind, 4.900e+00_fp_kind, 5.400e+00_fp_kind, 5.900e+00_fp_kind, 6.200e+00_fp_kind, &
          6.250e+00_fp_kind, 5.900e+00_fp_kind, 5.100e+00_fp_kind, 4.100e+00_fp_kind, 3.000e+00_fp_kind, &
          2.600e+00_fp_kind, 1.600e+00_fp_kind, 9.500e-01_fp_kind, 6.500e-01_fp_kind, 5.000e-01_fp_kind, &
          3.300e-01_fp_kind, 1.300e-01_fp_kind, 7.500e-01_fp_kind, 8.000e-01_fp_kind, 8.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, 3.163e-01_fp_kind, &
          3.096e-01_fp_kind, 2.989e-01_fp_kind, 2.936e-01_fp_kind, 2.860e-01_fp_kind, 2.800e-01_fp_kind, &
          2.724e-01_fp_kind, 2.611e-01_fp_kind, 2.421e-01_fp_kind, 2.174e-01_fp_kind, 1.843e-01_fp_kind, &
          1.621e-01_fp_kind, 1.362e-01_fp_kind, 1.230e-01_fp_kind, 1.122e-01_fp_kind, 1.043e-01_fp_kind, &
          9.570e-02_fp_kind, 8.598e-02_fp_kind, 7.314e-02_fp_kind, 5.710e-02_fp_kind, 4.670e-02_fp_kind, &
          3.439e-02_fp_kind, 2.471e-02_fp_kind, 1.631e-02_fp_kind, 1.066e-02_fp_kind, 7.064e-03_fp_kind, &
          3.972e-03_fp_kind, 2.508e-03_fp_kind, 1.726e-03_fp_kind, 1.260e-03_fp_kind, 9.602e-04_fp_kind, &
          7.554e-04_fp_kind, 6.097e-04_fp_kind, 5.024e-04_fp_kind, 4.210e-04_fp_kind, 3.579e-04_fp_kind, &
          3.080e-04_fp_kind, 2.678e-04_fp_kind, 2.350e-04_fp_kind, 2.079e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.521e-02_fp_kind, 1.722e-02_fp_kind, 2.037e-02_fp_kind, 2.486e-02_fp_kind, 3.168e-02_fp_kind, &
          4.429e-02_fp_kind, 6.472e-02_fp_kind, 1.041e-01_fp_kind, 1.507e-01_fp_kind, 2.163e-01_fp_kind, &
          3.141e-01_fp_kind, 4.842e-01_fp_kind, 7.147e-01_fp_kind, 1.067e+00_fp_kind, 1.516e+00_fp_kind, &
          2.166e+00_fp_kind, 3.060e+00_fp_kind, 4.564e+00_fp_kind, 6.877e+00_fp_kind, 1.055e+01_fp_kind, &
          1.710e+01_fp_kind, 2.473e+01_fp_kind, 3.359e+01_fp_kind, 4.149e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.697e+00_fp_kind, &
          1.687e+00_fp_kind, 1.672e+00_fp_kind, 1.649e+00_fp_kind, 1.629e+00_fp_kind, 1.615e+00_fp_kind, &
          1.579e+00_fp_kind, 1.542e+00_fp_kind, 1.506e+00_fp_kind, 1.471e+00_fp_kind, 1.434e+00_fp_kind, &
          1.389e+00_fp_kind, 1.342e+00_fp_kind, 1.290e+00_fp_kind, 1.230e+00_fp_kind, 1.161e+00_fp_kind, &
          1.084e+00_fp_kind, 1.014e+00_fp_kind, 9.561e-01_fp_kind, 9.009e-01_fp_kind, 8.479e-01_fp_kind, &
          7.961e-01_fp_kind, 7.449e-01_fp_kind, 6.941e-01_fp_kind, 6.434e-01_fp_kind, 5.883e-01_fp_kind, &
          5.238e-01_fp_kind, 4.505e-01_fp_kind, 3.708e-01_fp_kind, 3.004e-01_fp_kind, 2.453e-01_fp_kind, &
          1.980e-01_fp_kind, 1.590e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)


      ! ------------------------
      ! U.S. Standard 1976 model
      ! ------------------------

      CASE ( 6 )

        Model_Description = 'U.S. Standard Atmosphere'
        Model_Climatology_Model = Profile
        Model_Year  = 1976
        Model_Month = 0
        Model_Day   = 0
        Model_Hour  = 0
        Model_Latitude  = DEFAULT_MODEL_LATITUDE( Profile )
        Model_Longitude = -999.0_fp_kind
        Model_Surface_Altitude = ZERO

        ! -- Pressure
        Level_Pressure = (/ &
          1.013e+03_fp_kind, 8.988e+02_fp_kind, 7.950e+02_fp_kind, 7.012e+02_fp_kind, 6.166e+02_fp_kind, &
          5.405e+02_fp_kind, 4.722e+02_fp_kind, 4.111e+02_fp_kind, 3.565e+02_fp_kind, 3.080e+02_fp_kind, &
          2.650e+02_fp_kind, 2.270e+02_fp_kind, 1.940e+02_fp_kind, 1.658e+02_fp_kind, 1.417e+02_fp_kind, &
          1.211e+02_fp_kind, 1.035e+02_fp_kind, 8.850e+01_fp_kind, 7.565e+01_fp_kind, 6.467e+01_fp_kind, &
          5.529e+01_fp_kind, 4.729e+01_fp_kind, 4.047e+01_fp_kind, 3.467e+01_fp_kind, 2.972e+01_fp_kind, &
          2.549e+01_fp_kind, 1.743e+01_fp_kind, 1.197e+01_fp_kind, 8.010e+00_fp_kind, 5.746e+00_fp_kind, &
          4.150e+00_fp_kind, 2.871e+00_fp_kind, 2.060e+00_fp_kind, 1.491e+00_fp_kind, 1.090e+00_fp_kind, &
          7.978e-01_fp_kind, 4.250e-01_fp_kind, 2.190e-01_fp_kind, 1.090e-01_fp_kind, 5.220e-02_fp_kind, &
          2.400e-02_fp_kind, 1.050e-02_fp_kind, 4.460e-03_fp_kind, 1.840e-03_fp_kind, 7.600e-04_fp_kind, &
          3.200e-04_fp_kind, 1.450e-04_fp_kind, 7.100e-05_fp_kind, 4.010e-05_fp_kind, 2.540e-05_fp_kind /)


        ! -- Temperature
        Level_Temperature = (/ &
          288.20_fp_kind, 281.70_fp_kind, 275.20_fp_kind, 268.70_fp_kind, 262.20_fp_kind, &
          255.70_fp_kind, 249.20_fp_kind, 242.70_fp_kind, 236.20_fp_kind, 229.70_fp_kind, &
          223.30_fp_kind, 216.80_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
          216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
          216.70_fp_kind, 217.60_fp_kind, 218.60_fp_kind, 219.60_fp_kind, 220.60_fp_kind, &
          221.60_fp_kind, 224.00_fp_kind, 226.50_fp_kind, 230.00_fp_kind, 236.50_fp_kind, &
          242.90_fp_kind, 250.40_fp_kind, 257.30_fp_kind, 264.20_fp_kind, 270.60_fp_kind, &
          270.70_fp_kind, 260.80_fp_kind, 247.00_fp_kind, 233.30_fp_kind, 219.60_fp_kind, &
          208.40_fp_kind, 198.60_fp_kind, 188.90_fp_kind, 186.90_fp_kind, 188.40_fp_kind, &
          195.10_fp_kind, 208.80_fp_kind, 240.00_fp_kind, 300.00_fp_kind, 360.00_fp_kind /)

        ! -- Major absorber #1, H2O
        Level_Absorber( :, 1 ) = (/ &
          7.745e+03_fp_kind, 6.071e+03_fp_kind, 4.631e+03_fp_kind, 3.182e+03_fp_kind, 2.158e+03_fp_kind, &
          1.397e+03_fp_kind, 9.254e+02_fp_kind, 5.720e+02_fp_kind, 3.667e+02_fp_kind, 1.583e+02_fp_kind, &
          6.996e+01_fp_kind, 3.613e+01_fp_kind, 1.906e+01_fp_kind, 1.085e+01_fp_kind, 5.927e+00_fp_kind, &
          5.000e+00_fp_kind, 3.950e+00_fp_kind, 3.850e+00_fp_kind, 3.825e+00_fp_kind, 3.850e+00_fp_kind, &
          3.900e+00_fp_kind, 3.975e+00_fp_kind, 4.065e+00_fp_kind, 4.200e+00_fp_kind, 4.300e+00_fp_kind, &
          4.425e+00_fp_kind, 4.575e+00_fp_kind, 4.725e+00_fp_kind, 4.825e+00_fp_kind, 4.900e+00_fp_kind, &
          4.950e+00_fp_kind, 5.025e+00_fp_kind, 5.150e+00_fp_kind, 5.225e+00_fp_kind, 5.250e+00_fp_kind, &
          5.225e+00_fp_kind, 5.100e+00_fp_kind, 4.750e+00_fp_kind, 4.200e+00_fp_kind, 3.500e+00_fp_kind, &
          2.825e+00_fp_kind, 2.050e+00_fp_kind, 1.330e+00_fp_kind, 8.500e-01_fp_kind, 5.400e-01_fp_kind, &
          4.000e-01_fp_kind, 3.400e-01_fp_kind, 2.800e-01_fp_kind, 2.400e-01_fp_kind, 2.000e-01_fp_kind /)

        ! -- Major absorber #2, CO2
        Level_Absorber( :, 2 ) = (/ &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, 3.300e+02_fp_kind, &
          3.300e+02_fp_kind, 3.280e+02_fp_kind, 3.200e+02_fp_kind, 3.100e+02_fp_kind, 2.700e+02_fp_kind, &
          1.950e+02_fp_kind, 1.100e+02_fp_kind, 6.000e+01_fp_kind, 4.000e+01_fp_kind, 3.500e+01_fp_kind /)

        ! -- Major absorber #3, O3
        Level_Absorber( :, 3 ) = (/ &
          2.660e-02_fp_kind, 2.931e-02_fp_kind, 3.237e-02_fp_kind, 3.318e-02_fp_kind, 3.387e-02_fp_kind, &
          3.768e-02_fp_kind, 4.112e-02_fp_kind, 5.009e-02_fp_kind, 5.966e-02_fp_kind, 9.168e-02_fp_kind, &
          1.313e-01_fp_kind, 2.149e-01_fp_kind, 3.095e-01_fp_kind, 3.846e-01_fp_kind, 5.030e-01_fp_kind, &
          6.505e-01_fp_kind, 8.701e-01_fp_kind, 1.187e+00_fp_kind, 1.587e+00_fp_kind, 2.030e+00_fp_kind, &
          2.579e+00_fp_kind, 3.028e+00_fp_kind, 3.647e+00_fp_kind, 4.168e+00_fp_kind, 4.627e+00_fp_kind, &
          5.118e+00_fp_kind, 5.803e+00_fp_kind, 6.553e+00_fp_kind, 7.373e+00_fp_kind, 7.837e+00_fp_kind, &
          7.800e+00_fp_kind, 7.300e+00_fp_kind, 6.200e+00_fp_kind, 5.250e+00_fp_kind, 4.100e+00_fp_kind, &
          3.100e+00_fp_kind, 1.800e+00_fp_kind, 1.100e+00_fp_kind, 7.000e-01_fp_kind, 3.000e-01_fp_kind, &
          2.500e-01_fp_kind, 3.000e-01_fp_kind, 5.000e-01_fp_kind, 7.000e-01_fp_kind, 7.000e-01_fp_kind, &
          4.000e-01_fp_kind, 2.000e-01_fp_kind, 5.000e-02_fp_kind, 5.000e-03_fp_kind, 5.000e-04_fp_kind /)

        ! -- Major absorber #4, N2O
        Level_Absorber( :, 4 ) = (/ &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, &
          3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.200e-01_fp_kind, 3.195e-01_fp_kind, &
          3.179e-01_fp_kind, 3.140e-01_fp_kind, 3.095e-01_fp_kind, 3.048e-01_fp_kind, 2.999e-01_fp_kind, &
          2.944e-01_fp_kind, 2.877e-01_fp_kind, 2.783e-01_fp_kind, 2.671e-01_fp_kind, 2.527e-01_fp_kind, &
          2.365e-01_fp_kind, 2.194e-01_fp_kind, 2.051e-01_fp_kind, 1.967e-01_fp_kind, 1.875e-01_fp_kind, &
          1.756e-01_fp_kind, 1.588e-01_fp_kind, 1.416e-01_fp_kind, 1.165e-01_fp_kind, 9.275e-02_fp_kind, &
          6.693e-02_fp_kind, 4.513e-02_fp_kind, 2.751e-02_fp_kind, 1.591e-02_fp_kind, 9.378e-03_fp_kind, &
          4.752e-03_fp_kind, 3.000e-03_fp_kind, 2.065e-03_fp_kind, 1.507e-03_fp_kind, 1.149e-03_fp_kind, &
          8.890e-04_fp_kind, 7.056e-04_fp_kind, 5.716e-04_fp_kind, 4.708e-04_fp_kind, 3.932e-04_fp_kind, &
          3.323e-04_fp_kind, 2.837e-04_fp_kind, 2.443e-04_fp_kind, 2.120e-04_fp_kind, 1.851e-04_fp_kind /)

        ! -- Major absorber #5, CO
        Level_Absorber( :, 5 ) = (/ &
          1.500e-01_fp_kind, 1.450e-01_fp_kind, 1.399e-01_fp_kind, 1.349e-01_fp_kind, 1.312e-01_fp_kind, &
          1.303e-01_fp_kind, 1.288e-01_fp_kind, 1.247e-01_fp_kind, 1.185e-01_fp_kind, 1.094e-01_fp_kind, &
          9.962e-02_fp_kind, 8.964e-02_fp_kind, 7.814e-02_fp_kind, 6.374e-02_fp_kind, 5.025e-02_fp_kind, &
          3.941e-02_fp_kind, 3.069e-02_fp_kind, 2.489e-02_fp_kind, 1.966e-02_fp_kind, 1.549e-02_fp_kind, &
          1.331e-02_fp_kind, 1.232e-02_fp_kind, 1.232e-02_fp_kind, 1.307e-02_fp_kind, 1.400e-02_fp_kind, &
          1.498e-02_fp_kind, 1.598e-02_fp_kind, 1.710e-02_fp_kind, 1.850e-02_fp_kind, 2.009e-02_fp_kind, &
          2.220e-02_fp_kind, 2.497e-02_fp_kind, 2.824e-02_fp_kind, 3.241e-02_fp_kind, 3.717e-02_fp_kind, &
          4.597e-02_fp_kind, 6.639e-02_fp_kind, 1.073e-01_fp_kind, 1.862e-01_fp_kind, 3.059e-01_fp_kind, &
          6.375e-01_fp_kind, 1.497e+00_fp_kind, 3.239e+00_fp_kind, 5.843e+00_fp_kind, 1.013e+01_fp_kind, &
          1.692e+01_fp_kind, 2.467e+01_fp_kind, 3.356e+01_fp_kind, 4.148e+01_fp_kind, 5.000e+01_fp_kind /)

        ! -- Major absorber #6, CH4
        Level_Absorber( :, 6 ) = (/ &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.700e+00_fp_kind, &
          1.700e+00_fp_kind, 1.700e+00_fp_kind, 1.699e+00_fp_kind, 1.697e+00_fp_kind, 1.693e+00_fp_kind, &
          1.685e+00_fp_kind, 1.675e+00_fp_kind, 1.662e+00_fp_kind, 1.645e+00_fp_kind, 1.626e+00_fp_kind, &
          1.605e+00_fp_kind, 1.582e+00_fp_kind, 1.553e+00_fp_kind, 1.521e+00_fp_kind, 1.480e+00_fp_kind, &
          1.424e+00_fp_kind, 1.355e+00_fp_kind, 1.272e+00_fp_kind, 1.191e+00_fp_kind, 1.118e+00_fp_kind, &
          1.055e+00_fp_kind, 9.870e-01_fp_kind, 9.136e-01_fp_kind, 8.300e-01_fp_kind, 7.460e-01_fp_kind, &
          6.618e-01_fp_kind, 5.638e-01_fp_kind, 4.614e-01_fp_kind, 3.631e-01_fp_kind, 2.773e-01_fp_kind, &
          2.100e-01_fp_kind, 1.650e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, &
          1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.500e-01_fp_kind, 1.400e-01_fp_kind, 1.300e-01_fp_kind, &
          1.200e-01_fp_kind, 1.100e-01_fp_kind, 9.500e-02_fp_kind, 6.000e-02_fp_kind, 3.000e-02_fp_kind /)

    END SELECT



    !#--------------------------------------------------------------------------#
    !#                  -- FILL THE MODEL INDEPENDENT ARRAYS --                 #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Major absorber #7, O2
    ! ---------------------

    Level_Absorber( :, 7 ) = (/ &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.090e+05_fp_kind, &
      2.090e+05_fp_kind, 2.090e+05_fp_kind, 2.000e+05_fp_kind, 1.900e+05_fp_kind, 1.800e+05_fp_kind, &
      1.600e+05_fp_kind, 1.400e+05_fp_kind, 1.200e+05_fp_kind, 9.400e+04_fp_kind, 7.250e+04_fp_kind /)


    ! ---------------------------------------------------------------
    ! The minor absorbers are specified for the US Std Atm model only
    ! ---------------------------------------------------------------

    ! ---------------------
    ! Minor absorber #8, NO
    ! ---------------------

    Level_Absorber( :, 8 ) = (/ &
      3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, &
      3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, &
      3.00e-04_fp_kind, 3.00e-04_fp_kind, 3.00e-04_fp_kind, 2.99e-04_fp_kind, 2.95e-04_fp_kind, &
      2.83e-04_fp_kind, 2.68e-04_fp_kind, 2.52e-04_fp_kind, 2.40e-04_fp_kind, 2.44e-04_fp_kind, &
      2.55e-04_fp_kind, 2.77e-04_fp_kind, 3.07e-04_fp_kind, 3.60e-04_fp_kind, 4.51e-04_fp_kind, &
      6.85e-04_fp_kind, 1.28e-03_fp_kind, 2.45e-03_fp_kind, 4.53e-03_fp_kind, 7.14e-03_fp_kind, &
      9.34e-03_fp_kind, 1.12e-02_fp_kind, 1.19e-02_fp_kind, 1.17e-02_fp_kind, 1.10e-02_fp_kind, &
      1.03e-02_fp_kind, 1.01e-02_fp_kind, 1.01e-02_fp_kind, 1.03e-02_fp_kind, 1.15e-02_fp_kind, &
      1.61e-02_fp_kind, 2.68e-02_fp_kind, 7.01e-02_fp_kind, 2.13e-01_fp_kind, 7.12e-01_fp_kind, &
      2.08e+00_fp_kind, 4.50e+00_fp_kind, 7.98e+00_fp_kind, 1.00e+01_fp_kind, 1.00e+01_fp_kind /)


    ! ----------------------
    ! Minor absorber #9, SO2
    ! ----------------------

    Level_Absorber( :, 9 ) = (/ &
      3.00e-04_fp_kind, 2.74e-04_fp_kind, 2.36e-04_fp_kind, 1.90e-04_fp_kind, 1.46e-04_fp_kind, &
      1.18e-04_fp_kind, 9.71e-05_fp_kind, 8.30e-05_fp_kind, 7.21e-05_fp_kind, 6.56e-05_fp_kind, &
      6.08e-05_fp_kind, 5.79e-05_fp_kind, 5.60e-05_fp_kind, 5.59e-05_fp_kind, 5.64e-05_fp_kind, &
      5.75e-05_fp_kind, 5.75e-05_fp_kind, 5.37e-05_fp_kind, 4.78e-05_fp_kind, 3.97e-05_fp_kind, &
      3.19e-05_fp_kind, 2.67e-05_fp_kind, 2.28e-05_fp_kind, 2.07e-05_fp_kind, 1.90e-05_fp_kind, &
      1.75e-05_fp_kind, 1.54e-05_fp_kind, 1.34e-05_fp_kind, 1.21e-05_fp_kind, 1.16e-05_fp_kind, &
      1.21e-05_fp_kind, 1.36e-05_fp_kind, 1.65e-05_fp_kind, 2.10e-05_fp_kind, 2.77e-05_fp_kind, &
      3.56e-05_fp_kind, 4.59e-05_fp_kind, 5.15e-05_fp_kind, 5.11e-05_fp_kind, 4.32e-05_fp_kind, &
      2.83e-05_fp_kind, 1.33e-05_fp_kind, 5.56e-06_fp_kind, 2.24e-06_fp_kind, 8.96e-07_fp_kind, &
      3.58e-07_fp_kind, 1.43e-07_fp_kind, 5.73e-08_fp_kind, 2.29e-08_fp_kind, 9.17e-09_fp_kind /)


    ! -----------------------
    ! Minor absorber #10, NO2
    ! -----------------------

    Level_Absorber( :, 10 ) = (/ &
      2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.30e-05_fp_kind, &
      2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.30e-05_fp_kind, 2.32e-05_fp_kind, &
      2.38e-05_fp_kind, 2.62e-05_fp_kind, 3.15e-05_fp_kind, 4.45e-05_fp_kind, 7.48e-05_fp_kind, &
      1.71e-04_fp_kind, 3.19e-04_fp_kind, 5.19e-04_fp_kind, 7.71e-04_fp_kind, 1.06e-03_fp_kind, &
      1.39e-03_fp_kind, 1.76e-03_fp_kind, 2.16e-03_fp_kind, 2.58e-03_fp_kind, 3.06e-03_fp_kind, &
      3.74e-03_fp_kind, 4.81e-03_fp_kind, 6.16e-03_fp_kind, 7.21e-03_fp_kind, 7.28e-03_fp_kind, &
      6.26e-03_fp_kind, 4.03e-03_fp_kind, 2.17e-03_fp_kind, 1.15e-03_fp_kind, 6.66e-04_fp_kind, &
      4.43e-04_fp_kind, 3.39e-04_fp_kind, 2.85e-04_fp_kind, 2.53e-04_fp_kind, 2.31e-04_fp_kind, &
      2.15e-04_fp_kind, 2.02e-04_fp_kind, 1.92e-04_fp_kind, 1.83e-04_fp_kind, 1.76e-04_fp_kind, &
      1.70e-04_fp_kind, 1.64e-04_fp_kind, 1.59e-04_fp_kind, 1.55e-04_fp_kind, 1.51e-04_fp_kind /)


    ! -----------------------
    ! Minor absorber #11, NH3
    ! -----------------------

    Level_Absorber( :, 11 ) = (/ &
      5.00e-04_fp_kind, 5.00e-04_fp_kind, 4.63e-04_fp_kind, 3.80e-04_fp_kind, 2.88e-04_fp_kind, &
      2.04e-04_fp_kind, 1.46e-04_fp_kind, 9.88e-05_fp_kind, 6.48e-05_fp_kind, 3.77e-05_fp_kind, &
      2.03e-05_fp_kind, 1.09e-05_fp_kind, 6.30e-06_fp_kind, 3.12e-06_fp_kind, 1.11e-06_fp_kind, &
      4.47e-07_fp_kind, 2.11e-07_fp_kind, 1.10e-07_fp_kind, 6.70e-08_fp_kind, 3.97e-08_fp_kind, &
      2.41e-08_fp_kind, 1.92e-08_fp_kind, 1.72e-08_fp_kind, 1.59e-08_fp_kind, 1.44e-08_fp_kind, &
      1.23e-08_fp_kind, 9.37e-09_fp_kind, 6.35e-09_fp_kind, 3.68e-09_fp_kind, 1.82e-09_fp_kind, &
      9.26e-10_fp_kind, 2.94e-10_fp_kind, 8.72e-11_fp_kind, 2.98e-11_fp_kind, 1.30e-11_fp_kind, &
      7.13e-12_fp_kind, 4.80e-12_fp_kind, 3.66e-12_fp_kind, 3.00e-12_fp_kind, 2.57e-12_fp_kind, &
      2.27e-12_fp_kind, 2.04e-12_fp_kind, 1.85e-12_fp_kind, 1.71e-12_fp_kind, 1.59e-12_fp_kind, &
      1.48e-12_fp_kind, 1.40e-12_fp_kind, 1.32e-12_fp_kind, 1.25e-12_fp_kind, 1.19e-12_fp_kind /)


    ! ------------------------
    ! Minor absorber #12, HNO3
    ! ------------------------

    Level_Absorber( :, 12 ) = (/ &
      5.00e-05_fp_kind, 5.96e-05_fp_kind, 6.93e-05_fp_kind, 7.91e-05_fp_kind, 8.87e-05_fp_kind, &
      9.75e-05_fp_kind, 1.11e-04_fp_kind, 1.26e-04_fp_kind, 1.39e-04_fp_kind, 1.53e-04_fp_kind, &
      1.74e-04_fp_kind, 2.02e-04_fp_kind, 2.41e-04_fp_kind, 2.76e-04_fp_kind, 3.33e-04_fp_kind, &
      4.52e-04_fp_kind, 7.37e-04_fp_kind, 1.31e-03_fp_kind, 2.11e-03_fp_kind, 3.17e-03_fp_kind, &
      4.20e-03_fp_kind, 4.94e-03_fp_kind, 5.46e-03_fp_kind, 5.74e-03_fp_kind, 5.84e-03_fp_kind, &
      5.61e-03_fp_kind, 4.82e-03_fp_kind, 3.74e-03_fp_kind, 2.59e-03_fp_kind, 1.64e-03_fp_kind, &
      9.68e-04_fp_kind, 5.33e-04_fp_kind, 2.52e-04_fp_kind, 1.21e-04_fp_kind, 7.70e-05_fp_kind, &
      5.55e-05_fp_kind, 4.45e-05_fp_kind, 3.84e-05_fp_kind, 3.49e-05_fp_kind, 3.27e-05_fp_kind, &
      3.12e-05_fp_kind, 3.01e-05_fp_kind, 2.92e-05_fp_kind, 2.84e-05_fp_kind, 2.78e-05_fp_kind, &
      2.73e-05_fp_kind, 2.68e-05_fp_kind, 2.64e-05_fp_kind, 2.60e-05_fp_kind, 2.57e-05_fp_kind /)


    ! ----------------------
    ! Minor absorber #13, OH
    ! ----------------------

    Level_Absorber( :, 13 ) = (/ &
      4.40e-08_fp_kind, 4.40e-08_fp_kind, 4.40e-08_fp_kind, 4.40e-08_fp_kind, 4.40e-08_fp_kind, &
      4.40e-08_fp_kind, 4.40e-08_fp_kind, 4.41e-08_fp_kind, 4.45e-08_fp_kind, 4.56e-08_fp_kind, &
      4.68e-08_fp_kind, 4.80e-08_fp_kind, 4.94e-08_fp_kind, 5.19e-08_fp_kind, 5.65e-08_fp_kind, &
      6.75e-08_fp_kind, 8.25e-08_fp_kind, 1.04e-07_fp_kind, 1.30e-07_fp_kind, 1.64e-07_fp_kind, &
      2.16e-07_fp_kind, 3.40e-07_fp_kind, 5.09e-07_fp_kind, 7.59e-07_fp_kind, 1.16e-06_fp_kind, &
      2.18e-06_fp_kind, 5.00e-06_fp_kind, 1.17e-05_fp_kind, 3.40e-05_fp_kind, 8.35e-05_fp_kind, &
      1.70e-04_fp_kind, 2.85e-04_fp_kind, 4.06e-04_fp_kind, 5.11e-04_fp_kind, 5.79e-04_fp_kind, &
      6.75e-04_fp_kind, 9.53e-04_fp_kind, 1.76e-03_fp_kind, 3.74e-03_fp_kind, 7.19e-03_fp_kind, &
      1.12e-02_fp_kind, 1.13e-02_fp_kind, 6.10e-03_fp_kind, 1.51e-03_fp_kind, 2.42e-04_fp_kind, &
      4.47e-05_fp_kind, 1.77e-05_fp_kind, 1.19e-05_fp_kind, 1.35e-05_fp_kind, 2.20e-05_fp_kind /)


    ! ----------------------
    ! Minor absorber #14, HF
    ! ----------------------

    Level_Absorber( :, 14 ) = (/ &
      1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.23e-08_fp_kind, 1.97e-08_fp_kind, 3.18e-08_fp_kind, &
      5.63e-08_fp_kind, 9.18e-08_fp_kind, 1.53e-07_fp_kind, 2.41e-07_fp_kind, 4.04e-07_fp_kind, &
      6.57e-07_fp_kind, 1.20e-06_fp_kind, 1.96e-06_fp_kind, 3.12e-06_fp_kind, 4.62e-06_fp_kind, &
      7.09e-06_fp_kind, 1.05e-05_fp_kind, 1.69e-05_fp_kind, 2.57e-05_fp_kind, 4.02e-05_fp_kind, &
      5.77e-05_fp_kind, 7.77e-05_fp_kind, 9.90e-05_fp_kind, 1.23e-04_fp_kind, 1.50e-04_fp_kind, &
      1.82e-04_fp_kind, 2.30e-04_fp_kind, 2.83e-04_fp_kind, 3.20e-04_fp_kind, 3.48e-04_fp_kind, &
      3.72e-04_fp_kind, 3.95e-04_fp_kind, 4.10e-04_fp_kind, 4.21e-04_fp_kind, 4.24e-04_fp_kind, &
      4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, &
      4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, &
      4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind, 4.25e-04_fp_kind /)


    ! -----------------------
    ! Minor absorber #15, HCl
    ! -----------------------

    Level_Absorber( :, 15 ) = (/ &
      1.00e-03_fp_kind, 7.49e-04_fp_kind, 5.61e-04_fp_kind, 4.22e-04_fp_kind, 3.19e-04_fp_kind, &
      2.39e-04_fp_kind, 1.79e-04_fp_kind, 1.32e-04_fp_kind, 9.96e-05_fp_kind, 7.48e-05_fp_kind, &
      5.68e-05_fp_kind, 4.59e-05_fp_kind, 4.36e-05_fp_kind, 6.51e-05_fp_kind, 1.01e-04_fp_kind, &
      1.63e-04_fp_kind, 2.37e-04_fp_kind, 3.13e-04_fp_kind, 3.85e-04_fp_kind, 4.42e-04_fp_kind, &
      4.89e-04_fp_kind, 5.22e-04_fp_kind, 5.49e-04_fp_kind, 5.75e-04_fp_kind, 6.04e-04_fp_kind, &
      6.51e-04_fp_kind, 7.51e-04_fp_kind, 9.88e-04_fp_kind, 1.28e-03_fp_kind, 1.57e-03_fp_kind, &
      1.69e-03_fp_kind, 1.74e-03_fp_kind, 1.76e-03_fp_kind, 1.79e-03_fp_kind, 1.80e-03_fp_kind, &
      1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, &
      1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, &
      1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind, 1.80e-03_fp_kind /)


    ! -----------------------
    ! Minor absorber #16, HBr
    ! -----------------------

    Level_Absorber( :, 16 ) = (/ &
      1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, &
      1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, &
      1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, &
      1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, &
      1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, 1.70e-06_fp_kind, &
      1.71e-06_fp_kind, 1.76e-06_fp_kind, 1.90e-06_fp_kind, 2.26e-06_fp_kind, 2.82e-06_fp_kind, &
      3.69e-06_fp_kind, 4.91e-06_fp_kind, 6.13e-06_fp_kind, 6.85e-06_fp_kind, 7.08e-06_fp_kind, &
      7.14e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, &
      7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, &
      7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind, 7.15e-06_fp_kind /)


    ! ----------------------
    ! Minor absorber #17, HI
    ! ----------------------

    Level_Absorber( :, 17 ) = (/ &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, &
      3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind, 3.00e-06_fp_kind /)


    ! -----------------------
    ! Minor absorber #18, ClO
    ! -----------------------

    Level_Absorber( :, 18 ) = (/ &
      1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.00e-08_fp_kind, &
      1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.00e-08_fp_kind, 1.01e-08_fp_kind, 1.05e-08_fp_kind, &
      1.21e-08_fp_kind, 1.87e-08_fp_kind, 3.18e-08_fp_kind, 5.61e-08_fp_kind, 9.99e-08_fp_kind, &
      1.78e-07_fp_kind, 3.16e-07_fp_kind, 5.65e-07_fp_kind, 1.04e-06_fp_kind, 2.04e-06_fp_kind, &
      4.64e-06_fp_kind, 8.15e-06_fp_kind, 1.07e-05_fp_kind, 1.52e-05_fp_kind, 2.24e-05_fp_kind, &
      3.97e-05_fp_kind, 8.48e-05_fp_kind, 1.85e-04_fp_kind, 3.57e-04_fp_kind, 5.08e-04_fp_kind, &
      6.07e-04_fp_kind, 5.95e-04_fp_kind, 4.33e-04_fp_kind, 2.51e-04_fp_kind, 1.56e-04_fp_kind, &
      1.04e-04_fp_kind, 7.69e-05_fp_kind, 6.30e-05_fp_kind, 5.52e-05_fp_kind, 5.04e-05_fp_kind, &
      4.72e-05_fp_kind, 4.49e-05_fp_kind, 4.30e-05_fp_kind, 4.16e-05_fp_kind, 4.03e-05_fp_kind, &
      3.93e-05_fp_kind, 3.83e-05_fp_kind, 3.75e-05_fp_kind, 3.68e-05_fp_kind, 3.61e-05_fp_kind /)


    ! -----------------------
    ! Minor absorber #19, OCS
    ! -----------------------

    Level_Absorber( :, 19 ) = (/ &
      6.00e-04_fp_kind, 5.90e-04_fp_kind, 5.80e-04_fp_kind, 5.70e-04_fp_kind, 5.62e-04_fp_kind, &
      5.55e-04_fp_kind, 5.48e-04_fp_kind, 5.40e-04_fp_kind, 5.32e-04_fp_kind, 5.25e-04_fp_kind, &
      5.18e-04_fp_kind, 5.09e-04_fp_kind, 4.98e-04_fp_kind, 4.82e-04_fp_kind, 4.60e-04_fp_kind, &
      4.26e-04_fp_kind, 3.88e-04_fp_kind, 3.48e-04_fp_kind, 3.09e-04_fp_kind, 2.74e-04_fp_kind, &
      2.41e-04_fp_kind, 2.14e-04_fp_kind, 1.88e-04_fp_kind, 1.64e-04_fp_kind, 1.37e-04_fp_kind, &
      1.08e-04_fp_kind, 6.70e-05_fp_kind, 2.96e-05_fp_kind, 1.21e-05_fp_kind, 4.31e-06_fp_kind, &
      1.60e-06_fp_kind, 6.71e-07_fp_kind, 4.35e-07_fp_kind, 3.34e-07_fp_kind, 2.80e-07_fp_kind, &
      2.47e-07_fp_kind, 2.28e-07_fp_kind, 2.16e-07_fp_kind, 2.08e-07_fp_kind, 2.03e-07_fp_kind, &
      1.98e-07_fp_kind, 1.95e-07_fp_kind, 1.92e-07_fp_kind, 1.89e-07_fp_kind, 1.87e-07_fp_kind, &
      1.85e-07_fp_kind, 1.83e-07_fp_kind, 1.81e-07_fp_kind, 1.80e-07_fp_kind, 1.78e-07_fp_kind /)


    ! ------------------------
    ! Minor absorber #20, H2CO
    ! ------------------------

    Level_Absorber( :, 20 ) = (/ &
      2.40e-03_fp_kind, 1.07e-03_fp_kind, 4.04e-04_fp_kind, 2.27e-04_fp_kind, 1.40e-04_fp_kind, &
      1.00e-04_fp_kind, 7.44e-05_fp_kind, 6.04e-05_fp_kind, 5.01e-05_fp_kind, 4.22e-05_fp_kind, &
      3.63e-05_fp_kind, 3.43e-05_fp_kind, 3.39e-05_fp_kind, 3.50e-05_fp_kind, 3.62e-05_fp_kind, &
      3.62e-05_fp_kind, 3.58e-05_fp_kind, 3.50e-05_fp_kind, 3.42e-05_fp_kind, 3.39e-05_fp_kind, &
      3.43e-05_fp_kind, 3.68e-05_fp_kind, 4.03e-05_fp_kind, 4.50e-05_fp_kind, 5.06e-05_fp_kind, &
      5.82e-05_fp_kind, 7.21e-05_fp_kind, 8.73e-05_fp_kind, 1.01e-04_fp_kind, 1.11e-04_fp_kind, &
      1.13e-04_fp_kind, 1.03e-04_fp_kind, 7.95e-05_fp_kind, 4.82e-05_fp_kind, 1.63e-05_fp_kind, &
      5.10e-06_fp_kind, 2.00e-06_fp_kind, 1.05e-06_fp_kind, 6.86e-07_fp_kind, 5.14e-07_fp_kind, &
      4.16e-07_fp_kind, 3.53e-07_fp_kind, 3.09e-07_fp_kind, 2.76e-07_fp_kind, 2.50e-07_fp_kind, &
      2.30e-07_fp_kind, 2.13e-07_fp_kind, 1.98e-07_fp_kind, 1.86e-07_fp_kind, 1.75e-07_fp_kind /)


    ! ------------------------
    ! Minor absorber #21, HOCl
    ! ------------------------

    Level_Absorber( :, 21 ) = (/ &
      7.70e-06_fp_kind, 1.06e-05_fp_kind, 1.22e-05_fp_kind, 1.14e-05_fp_kind, 9.80e-06_fp_kind, &
      8.01e-06_fp_kind, 6.42e-06_fp_kind, 5.42e-06_fp_kind, 4.70e-06_fp_kind, 4.41e-06_fp_kind, &
      4.34e-06_fp_kind, 4.65e-06_fp_kind, 5.01e-06_fp_kind, 5.22e-06_fp_kind, 5.60e-06_fp_kind, &
      6.86e-06_fp_kind, 8.77e-06_fp_kind, 1.20e-05_fp_kind, 1.63e-05_fp_kind, 2.26e-05_fp_kind, &
      3.07e-05_fp_kind, 4.29e-05_fp_kind, 5.76e-05_fp_kind, 7.65e-05_fp_kind, 9.92e-05_fp_kind, &
      1.31e-04_fp_kind, 1.84e-04_fp_kind, 2.45e-04_fp_kind, 2.96e-04_fp_kind, 3.21e-04_fp_kind, &
      3.04e-04_fp_kind, 2.48e-04_fp_kind, 1.64e-04_fp_kind, 9.74e-05_fp_kind, 4.92e-05_fp_kind, &
      2.53e-05_fp_kind, 1.50e-05_fp_kind, 1.05e-05_fp_kind, 8.34e-06_fp_kind, 7.11e-06_fp_kind, &
      6.33e-06_fp_kind, 5.78e-06_fp_kind, 5.37e-06_fp_kind, 5.05e-06_fp_kind, 4.78e-06_fp_kind, &
      4.56e-06_fp_kind, 4.37e-06_fp_kind, 4.21e-06_fp_kind, 4.06e-06_fp_kind, 3.93e-06_fp_kind /)


    ! ----------------------
    ! Minor absorber #22, N2
    ! ----------------------

    Level_Absorber( :, 22 ) = (/ &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, &
      7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.81e+05_fp_kind, 7.80e+05_fp_kind, 7.79e+05_fp_kind, &
      7.77e+05_fp_kind, 7.74e+05_fp_kind, 7.70e+05_fp_kind, 7.65e+05_fp_kind, 7.60e+05_fp_kind /)


    ! -----------------------
    ! Minor absorber #23, HCN
    ! -----------------------

    Level_Absorber( :, 23 ) = (/ &
      1.70e-04_fp_kind, 1.65e-04_fp_kind, 1.63e-04_fp_kind, 1.61e-04_fp_kind, 1.60e-04_fp_kind, &
      1.60e-04_fp_kind, 1.60e-04_fp_kind, 1.60e-04_fp_kind, 1.60e-04_fp_kind, 1.60e-04_fp_kind, &
      1.60e-04_fp_kind, 1.60e-04_fp_kind, 1.60e-04_fp_kind, 1.59e-04_fp_kind, 1.57e-04_fp_kind, &
      1.55e-04_fp_kind, 1.52e-04_fp_kind, 1.49e-04_fp_kind, 1.45e-04_fp_kind, 1.41e-04_fp_kind, &
      1.37e-04_fp_kind, 1.34e-04_fp_kind, 1.30e-04_fp_kind, 1.25e-04_fp_kind, 1.19e-04_fp_kind, &
      1.13e-04_fp_kind, 1.05e-04_fp_kind, 9.73e-05_fp_kind, 9.04e-05_fp_kind, 8.46e-05_fp_kind, &
      8.02e-05_fp_kind, 7.63e-05_fp_kind, 7.30e-05_fp_kind, 7.00e-05_fp_kind, 6.70e-05_fp_kind, &
      6.43e-05_fp_kind, 6.21e-05_fp_kind, 6.02e-05_fp_kind, 5.88e-05_fp_kind, 5.75e-05_fp_kind, &
      5.62e-05_fp_kind, 5.50e-05_fp_kind, 5.37e-05_fp_kind, 5.25e-05_fp_kind, 5.12e-05_fp_kind, &
      5.00e-05_fp_kind, 4.87e-05_fp_kind, 4.75e-05_fp_kind, 4.62e-05_fp_kind, 4.50e-05_fp_kind /)


    ! -------------------------
    ! Minor absorber #24, CH3Cl
    ! -------------------------

    Level_Absorber( :, 24 ) = (/ &
      7.00e-04_fp_kind, 6.70e-04_fp_kind, 6.43e-04_fp_kind, 6.22e-04_fp_kind, 6.07e-04_fp_kind, &
      6.02e-04_fp_kind, 6.00e-04_fp_kind, 6.00e-04_fp_kind, 5.98e-04_fp_kind, 5.94e-04_fp_kind, &
      5.88e-04_fp_kind, 5.79e-04_fp_kind, 5.66e-04_fp_kind, 5.48e-04_fp_kind, 5.28e-04_fp_kind, &
      5.03e-04_fp_kind, 4.77e-04_fp_kind, 4.49e-04_fp_kind, 4.21e-04_fp_kind, 3.95e-04_fp_kind, &
      3.69e-04_fp_kind, 3.43e-04_fp_kind, 3.17e-04_fp_kind, 2.86e-04_fp_kind, 2.48e-04_fp_kind, &
      1.91e-04_fp_kind, 1.10e-04_fp_kind, 4.72e-05_fp_kind, 1.79e-05_fp_kind, 7.35e-06_fp_kind, &
      3.03e-06_fp_kind, 1.32e-06_fp_kind, 8.69e-07_fp_kind, 6.68e-07_fp_kind, 5.60e-07_fp_kind, &
      4.94e-07_fp_kind, 4.56e-07_fp_kind, 4.32e-07_fp_kind, 4.17e-07_fp_kind, 4.05e-07_fp_kind, &
      3.96e-07_fp_kind, 3.89e-07_fp_kind, 3.83e-07_fp_kind, 3.78e-07_fp_kind, 3.73e-07_fp_kind, &
      3.69e-07_fp_kind, 3.66e-07_fp_kind, 3.62e-07_fp_kind, 3.59e-07_fp_kind, 3.56e-07_fp_kind /)


    ! ------------------------
    ! Minor absorber #25, H2O2
    ! ------------------------

    Level_Absorber( :, 25 ) = (/ &
      2.00e-04_fp_kind, 1.95e-04_fp_kind, 1.92e-04_fp_kind, 1.89e-04_fp_kind, 1.84e-04_fp_kind, &
      1.77e-04_fp_kind, 1.66e-04_fp_kind, 1.49e-04_fp_kind, 1.23e-04_fp_kind, 9.09e-05_fp_kind, &
      5.79e-05_fp_kind, 3.43e-05_fp_kind, 1.95e-05_fp_kind, 1.08e-05_fp_kind, 6.59e-06_fp_kind, &
      4.20e-06_fp_kind, 2.94e-06_fp_kind, 2.30e-06_fp_kind, 2.24e-06_fp_kind, 2.68e-06_fp_kind, &
      3.68e-06_fp_kind, 5.62e-06_fp_kind, 1.03e-05_fp_kind, 1.97e-05_fp_kind, 3.70e-05_fp_kind, &
      6.20e-05_fp_kind, 1.03e-04_fp_kind, 1.36e-04_fp_kind, 1.36e-04_fp_kind, 1.13e-04_fp_kind, &
      8.51e-05_fp_kind, 6.37e-05_fp_kind, 5.17e-05_fp_kind, 4.44e-05_fp_kind, 3.80e-05_fp_kind, &
      3.48e-05_fp_kind, 3.62e-05_fp_kind, 5.25e-05_fp_kind, 1.26e-04_fp_kind, 3.77e-04_fp_kind, &
      1.12e-03_fp_kind, 2.00e-03_fp_kind, 1.68e-03_fp_kind, 4.31e-04_fp_kind, 4.98e-05_fp_kind, &
      6.76e-06_fp_kind, 8.38e-07_fp_kind, 9.56e-08_fp_kind, 1.00e-08_fp_kind, 1.00e-09_fp_kind /)


    ! ------------------------
    ! Minor absorber #26, C2H2
    ! ------------------------

    Level_Absorber( :, 26 ) = (/ &
      3.00e-04_fp_kind, 1.72e-04_fp_kind, 9.57e-05_fp_kind, 6.74e-05_fp_kind, 5.07e-05_fp_kind, &
      3.99e-05_fp_kind, 3.19e-05_fp_kind, 2.80e-05_fp_kind, 2.55e-05_fp_kind, 2.40e-05_fp_kind, &
      2.27e-05_fp_kind, 2.08e-05_fp_kind, 1.76e-05_fp_kind, 1.23e-05_fp_kind, 7.32e-06_fp_kind, &
      4.52e-06_fp_kind, 2.59e-06_fp_kind, 1.55e-06_fp_kind, 8.63e-07_fp_kind, 5.30e-07_fp_kind, &
      3.10e-07_fp_kind, 1.89e-07_fp_kind, 1.04e-07_fp_kind, 5.75e-08_fp_kind, 2.23e-08_fp_kind, &
      8.51e-09_fp_kind, 4.09e-09_fp_kind, 2.52e-09_fp_kind, 1.86e-09_fp_kind, 1.52e-09_fp_kind, &
      1.32e-09_fp_kind, 1.18e-09_fp_kind, 1.08e-09_fp_kind, 9.97e-10_fp_kind, 9.34e-10_fp_kind, &
      8.83e-10_fp_kind, 8.43e-10_fp_kind, 8.10e-10_fp_kind, 7.83e-10_fp_kind, 7.60e-10_fp_kind, &
      7.40e-10_fp_kind, 7.23e-10_fp_kind, 7.07e-10_fp_kind, 6.94e-10_fp_kind, 6.81e-10_fp_kind, &
      6.70e-10_fp_kind, 6.59e-10_fp_kind, 6.49e-10_fp_kind, 6.40e-10_fp_kind, 6.32e-10_fp_kind /)


    ! ------------------------
    ! Minor absorber #27, C2H6
    ! ------------------------

    Level_Absorber( :, 27 ) = (/ &
      2.00e-03_fp_kind, 2.00e-03_fp_kind, 2.00e-03_fp_kind, 2.00e-03_fp_kind, 1.98e-03_fp_kind, &
      1.95e-03_fp_kind, 1.90e-03_fp_kind, 1.85e-03_fp_kind, 1.79e-03_fp_kind, 1.72e-03_fp_kind, &
      1.58e-03_fp_kind, 1.30e-03_fp_kind, 9.86e-04_fp_kind, 7.22e-04_fp_kind, 4.96e-04_fp_kind, &
      3.35e-04_fp_kind, 2.14e-04_fp_kind, 1.49e-04_fp_kind, 1.05e-04_fp_kind, 7.96e-05_fp_kind, &
      6.01e-05_fp_kind, 4.57e-05_fp_kind, 3.40e-05_fp_kind, 2.60e-05_fp_kind, 1.89e-05_fp_kind, &
      1.22e-05_fp_kind, 5.74e-06_fp_kind, 2.14e-06_fp_kind, 8.49e-07_fp_kind, 3.42e-07_fp_kind, &
      1.34e-07_fp_kind, 5.39e-08_fp_kind, 2.25e-08_fp_kind, 1.04e-08_fp_kind, 6.57e-09_fp_kind, &
      4.74e-09_fp_kind, 3.79e-09_fp_kind, 3.28e-09_fp_kind, 2.98e-09_fp_kind, 2.79e-09_fp_kind, &
      2.66e-09_fp_kind, 2.56e-09_fp_kind, 2.49e-09_fp_kind, 2.43e-09_fp_kind, 2.37e-09_fp_kind, &
      2.33e-09_fp_kind, 2.29e-09_fp_kind, 2.25e-09_fp_kind, 2.22e-09_fp_kind, 2.19e-09_fp_kind /)


    ! -----------------------
    ! Minor absorber #28, PH3
    ! -----------------------

    Level_Absorber( :, 28 ) = (/ &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, &
      1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind, 1.00e-14_fp_kind /)



    !#--------------------------------------------------------------------------#
    !#                 -- CONVERT THE WATER VAPOR DATA TO G/KG --               #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------
    ! Convert the water vapour units from ppmv to g/kg
    ! ------------------------------------------------

    CALL PPMV_to_MR( Level_Absorber(:,1), H2O_Mixing_Ratio )

    IF ( ANY( H2O_Mixing_Ratio < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error converting water vapour from ppmv->g/kg.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    Level_Absorber(:,1) = H2O_Mixing_Ratio


    ! -------------------------------
    ! Set the absorber units ID array
    ! -------------------------------

    ! -- Default is ppmv...
    Model_Absorber_Units_ID(:) = PPMV_UNITS

    ! -- ...except for H2O
    Model_Absorber_Units_ID(1) = MR_UNITS



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN THE OPTIONAL OUTPUT ARGUMENTS --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Absorber_ID ) ) THEN
      Absorber_ID = (/ ( j, j = 1, N_MODEL_ABSORBERS ) /)
    END IF

    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      Absorber_Units_ID = Model_Absorber_Units_ID
    END IF

    IF ( PRESENT( Description ) ) THEN
      Description = ' '
      Description = TRIM( Model_Description )
    END IF

    IF ( PRESENT( Climatology_Model ) ) THEN
      Climatology_Model = Model_Climatology_Model
    END IF

    IF ( PRESENT( Year ) ) THEN
      Year = Model_Year
    END IF

    IF ( PRESENT( Month ) ) THEN
      Month = Model_Month
    END IF

    IF ( PRESENT( Day ) ) THEN
      Day = Model_Day
    END IF

    IF ( PRESENT( Hour ) ) THEN
      Hour = Model_Hour
    END IF

    IF ( PRESENT( Latitude ) ) THEN
      Latitude = Model_Latitude
    END IF

    IF ( PRESENT( Longitude ) ) THEN
      Longitude = Model_Longitude
    END IF

    IF ( PRESENT( Surface_Altitude ) ) THEN
      Surface_Altitude = Model_Surface_Altitude
    END IF

  END FUNCTION Load_Model_Profile

END MODULE Model_Profile_Set


