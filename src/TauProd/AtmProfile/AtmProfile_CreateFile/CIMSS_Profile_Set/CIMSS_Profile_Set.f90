!------------------------------------------------------------------------------
!M+
! NAME:
!       CIMSS_Profile_Set
!
! PURPOSE:
!       Module containing the CIMSS atmospheric profile dependent set data 
!       definitions and access routines
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CIMSS_Profile_Set
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       Message_Handler:      Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
! CONTAINS:
!       Load_CIMSS_Profile: Function to load a requested atmospheric profile
!                           from the CIMSS dependent set.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE CIMSS_Profile_Set


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Load_CIMSS_Profile


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Private dimensions.
  ! -- Note the difference between N_PROFILE_MODEL_LEVELS and N_MODEL_LEVELS.
  ! -- The extra level in N_MODEL_LEVELS is for adjusting the model temperature
  ! -- profile so there is not a large discontinuity at the splice point (0.05hPa)
  INTEGER, PRIVATE, PARAMETER :: N_PROFILE_SONDE_LEVELS = 42
  INTEGER, PRIVATE, PARAMETER :: N_PROFILE_MODEL_LEVELS =  4
  INTEGER, PRIVATE, PARAMETER :: N_MODELS       = 6
  INTEGER, PRIVATE, PARAMETER :: N_MODEL_LEVELS = 5

  ! -- These are private since at some point they may differ on a profile
  ! -- by profile basis...as they do for the other profile sets
  INTEGER, PRIVATE,  PARAMETER :: N_CIMSS_LEVELS = N_PROFILE_SONDE_LEVELS + N_PROFILE_MODEL_LEVELS
  INTEGER, PRIVATE,  PARAMETER :: N_CIMSS_LAYERS = N_CIMSS_LEVELS - 1

  ! -- Public dimensions
  INTEGER, PUBLIC,  PARAMETER :: N_CIMSS_ABSORBERS = 2
  INTEGER, PUBLIC,  PARAMETER :: N_CIMSS_PROFILES  = 32

  ! -- Pressure levels for the CIMSS profile set.
  REAL( fp_kind ), PRIVATE, PARAMETER, DIMENSION( N_CIMSS_LEVELS ) :: CIMSS_Level_Pressure = &
    (/ 0.005_fp_kind,  0.01_fp_kind,  0.02_fp_kind,   0.05_fp_kind,    0.1_fp_kind, &
         0.2_fp_kind,   0.5_fp_kind,   1.0_fp_kind,    1.5_fp_kind,    2.0_fp_kind, &
         3.0_fp_kind,   4.0_fp_kind,   5.0_fp_kind,    7.0_fp_kind,   10.0_fp_kind, &
        15.0_fp_kind,  20.0_fp_kind,  25.0_fp_kind,   30.0_fp_kind,   50.0_fp_kind, &
        60.0_fp_kind,  70.0_fp_kind,  85.0_fp_kind,  100.0_fp_kind,  115.0_fp_kind, &
       135.0_fp_kind, 150.0_fp_kind, 200.0_fp_kind,  250.0_fp_kind,  300.0_fp_kind, &
       350.0_fp_kind, 400.0_fp_kind, 430.0_fp_kind,  475.0_fp_kind,  500.0_fp_kind, &
       570.0_fp_kind, 620.0_fp_kind, 670.0_fp_kind,  700.0_fp_kind,  780.0_fp_kind, &
       850.0_fp_kind, 920.0_fp_kind, 950.0_fp_kind, 1000.0_fp_kind, 1025.0_fp_kind, &
      1050.0_fp_kind /)

  ! -- Absorber info
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_CIMSS_ABSORBERS ) :: &
    CIMSS_ABSORBER_ID = (/ 1, &  ! H2O
                           3 /)  ! O3

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_CIMSS_ABSORBERS ) :: &
    CIMSS_ABSORBER_UNITS_ID = (/ 3, &  ! g/kg
                                 1 /)  ! ppmv


  ! -- Temperature and absorber model data for supplementing the TOA profiles to 0.005hPa
  REAL( fp_kind ), PRIVATE, PARAMETER, DIMENSION( N_MODEL_LEVELS, N_MODELS ) :: &
    MODEL_Level_Temperature = &
      RESHAPE( (/ 178.17_fp_kind, 184.00_fp_kind, 196.61_fp_kind, 215.74_fp_kind, 231.57_fp_kind, &     ! Mdl 1 T
                  166.10_fp_kind, 172.43_fp_kind, 186.36_fp_kind, 210.09_fp_kind, 230.17_fp_kind, &     ! Mdl 2 T
                  200.96_fp_kind, 209.73_fp_kind, 219.00_fp_kind, 231.60_fp_kind, 241.64_fp_kind, &     ! Mdl 3 T
                  162.60_fp_kind, 168.65_fp_kind, 182.10_fp_kind, 206.48_fp_kind, 227.47_fp_kind, &     ! Mdl 4 T
                  214.59_fp_kind, 224.41_fp_kind, 234.70_fp_kind, 246.38_fp_kind, 249.27_fp_kind, &     ! Mdl 5 T
                  190.19_fp_kind, 198.05_fp_kind, 206.24_fp_kind, 218.98_fp_kind, 231.70_fp_kind /), &  ! Mdl 6 T
             (/ N_MODEL_LEVELS, N_MODELS /) )

  REAL( fp_kind ), PRIVATE, PARAMETER, DIMENSION( N_MODEL_LEVELS, N_CIMSS_ABSORBERS, N_MODELS ) :: MODEL_Level_Absorber = &
    RESHAPE( (/ 0.0009_fp_kind,  0.0013_fp_kind,  0.0018_fp_kind,  0.0027_fp_kind,  0.0032_fp_kind, &     ! Mdl 1 H2O
               0.47628_fp_kind, 0.34768_fp_kind, 0.22575_fp_kind, 0.27780_fp_kind, 0.55927_fp_kind, &     ! Mdl 1 O3

                0.0009_fp_kind,  0.0012_fp_kind,  0.0016_fp_kind,  0.0021_fp_kind,  0.0025_fp_kind, &     ! Mdl 2 H2O
               0.52876_fp_kind, 0.26847_fp_kind, 0.19443_fp_kind, 0.32351_fp_kind, 0.61951_fp_kind, &     ! Mdl 2 O3

                0.0009_fp_kind,  0.0012_fp_kind,  0.0016_fp_kind,  0.0021_fp_kind,  0.0025_fp_kind, &     ! Mdl 3 H2O
               0.51382_fp_kind, 0.24161_fp_kind, 0.24728_fp_kind, 0.34022_fp_kind, 0.58382_fp_kind, &     ! Mdl 3 O3

                0.0009_fp_kind,  0.0012_fp_kind,  0.0015_fp_kind,  0.0019_fp_kind,  0.0023_fp_kind, &     ! Mdl 4 H2O
               0.60245_fp_kind, 0.28288_fp_kind, 0.19000_fp_kind, 0.31200_fp_kind, 0.58825_fp_kind, &     ! Mdl 4 O3

                0.0009_fp_kind,  0.0013_fp_kind,  0.0017_fp_kind,  0.0022_fp_kind,  0.0026_fp_kind, &     ! Mdl 5 H2O
               0.66449_fp_kind, 0.13951_fp_kind, 0.33000_fp_kind, 0.54918_fp_kind, 0.75492_fp_kind, &     ! Mdl 5 O3

                0.0009_fp_kind,  0.0012_fp_kind,  0.0016_fp_kind,  0.0022_fp_kind,  0.0026_fp_kind, &     ! Mdl 6 H2O
               0.47330_fp_kind, 0.31140_fp_kind, 0.26103_fp_kind, 0.29723_fp_kind, 0.65318_fp_kind /), &  ! Mdl 6 O3

             (/ N_MODEL_LEVELS, N_CIMSS_ABSORBERS, N_MODELS /) )


  ! -- Tolerance and scale factor for splicing temperature profile
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWO          = 2.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SCALE_FACTOR = 0.2_fp_kind

  ! -- Invalid value
  INTEGER,         PRIVATE, PARAMETER :: INVALID = -1

  ! -- Literals
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind


CONTAINS



  FUNCTION Load_CIMSS_Profile( Profile,           &  ! Input
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
    INTEGER,                                      INTENT( IN )  :: profile

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

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Load_CIMSS_Profile'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: message

    INTEGER :: allocate_status

    INTEGER :: j, k
    REAL( fp_kind ) :: dT
    
    CHARACTER( 60 ) :: CIMSS_Description
    INTEGER         :: CIMSS_Climatology_Model
    INTEGER         :: CIMSS_Year
    INTEGER         :: CIMSS_Month
    INTEGER         :: CIMSS_Day
    INTEGER         :: CIMSS_Hour
    REAL( fp_kind ) :: CIMSS_Latitude
    REAL( fp_kind ) :: CIMSS_Longitude
    REAL( fp_kind ) :: CIMSS_Surface_Altitude

    REAL( fp_kind ), DIMENSION( N_CIMSS_LEVELS )                    :: CIMSS_Level_Temperature
    REAL( fp_kind ), DIMENSION( N_CIMSS_LEVELS, N_CIMSS_ABSORBERS ) :: CIMSS_Level_Absorber




    !#--------------------------------------------------------------------------#
    !#                   -- SET A SUCCESSFUL ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Is requested profile valid?
    ! ---------------------------

    IF ( profile < 1 .OR. profile > N_CIMSS_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Invalid model profile number ", i5, " specified." )' ) &
                      profile
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Check output pointers
    ! ---------------------

    ! -- Pressure
    IF ( ASSOCIATED( Level_Pressure ) ) THEN
      DEALLOCATE( Level_Pressure, STAT = allocate_status )
      IF ( allocate_status /= 0 ) THEN
        WRITE( message, '( "Error deallocating Level_Pressure output array. STAT = ", i5 )' ) &
                        allocate_status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Temperature
    IF ( ASSOCIATED( Level_Temperature ) ) THEN
      DEALLOCATE( Level_Temperature, STAT = allocate_status )
      IF ( allocate_status /= 0 ) THEN
        WRITE( message, '( "Error deallocating Level_Temperature output array. STAT = ", i5 )' ) &
                        allocate_status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Absorber
    IF ( ASSOCIATED( Level_Absorber ) ) THEN
      DEALLOCATE( Level_Absorber, STAT = allocate_status )
      IF ( allocate_status /= 0 ) THEN
        WRITE( message, '( "Error deallocating Level_Absorber output array. STAT = ", i5 )' ) &
                        allocate_status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( message ), &
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
      IF ( SIZE( Absorber_ID ) /= N_CIMSS_ABSORBERS      ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Size of output Absorber_ID array must be ", i1, " elements." )' ) &
                        N_CIMSS_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- Absorber Units ID array
    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      IF ( SIZE( Absorber_Units_ID ) /= N_CIMSS_ABSORBERS      ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Size of output Absorber_Units_ID array must be ", i1, " elements." )' ) &
                        N_CIMSS_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- INITIALISE LOCAL VALUES --                      #
    !#--------------------------------------------------------------------------#

    CIMSS_Description = ' '
    CIMSS_Climatology_Model = INVALID
    CIMSS_Year              = INVALID
    CIMSS_Month             = INVALID
    CIMSS_Day               = INVALID
    CIMSS_Hour              = INVALID
    CIMSS_Latitude          = REAL( INVALID, fp_kind )
    CIMSS_Longitude         = REAL( INVALID, fp_kind )
    CIMSS_Surface_Altitude  = REAL( INVALID, fp_kind )



    !#--------------------------------------------------------------------------#
    !#                   -- ALLOCATE OUTPUT POINTER ARRAYS --                   #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Level_Pressure( N_CIMSS_LEVELS ), &
              Level_Temperature( N_CIMSS_LEVELS ), &
              Level_Absorber( N_CIMSS_LEVELS, N_CIMSS_ABSORBERS ), &
              STAT = allocate_status )

    IF ( allocate_status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error allocating output arrays. STATs = ", 4(1x,i5) )' ) &
                      allocate_status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- FILL THE PROFILE ARRAYS --                      #
    !#--------------------------------------------------------------------------#

    profile_select: SELECT CASE ( profile )

      CASE( 1 )

        CIMSS_Description = 'U.S.STD.ATM.1976'
        CIMSS_Climatology_Model = 6
        CIMSS_Year  = 1976
        CIMSS_Month = 0
        CIMSS_Day   = 0
        CIMSS_Hour  = 0
        CIMSS_Latitude  = -999.0_fp_kind
        CIMSS_Longitude = -999.0_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             231.70_fp_kind, 245.22_fp_kind, &
             263.35_fp_kind, 270.63_fp_kind, 264.07_fp_kind, 257.93_fp_kind, &
             249.51_fp_kind, 243.65_fp_kind, 239.24_fp_kind, 232.64_fp_kind, &
             228.07_fp_kind, 225.00_fp_kind, 223.13_fp_kind, 221.72_fp_kind, &
             220.54_fp_kind, 217.28_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
             216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, 216.70_fp_kind, &
             216.70_fp_kind, 216.72_fp_kind, 220.85_fp_kind, 228.58_fp_kind, &
             235.38_fp_kind, 241.45_fp_kind, 244.81_fp_kind, 249.48_fp_kind, &
             251.95_fp_kind, 258.32_fp_kind, 262.48_fp_kind, 266.40_fp_kind, &
             268.61_fp_kind, 274.21_fp_kind, 278.74_fp_kind, 282.97_fp_kind, &
             284.71_fp_kind, 287.50_fp_kind, 288.84_fp_kind, 290.15_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.004_fp_kind, &
             0.005_fp_kind,  0.014_fp_kind,  0.036_fp_kind,  0.089_fp_kind, &
             0.212_fp_kind,  0.331_fp_kind,  0.427_fp_kind,  0.588_fp_kind, &
             0.699_fp_kind,  1.059_fp_kind,  1.368_fp_kind,  1.752_fp_kind, &
             1.969_fp_kind,  2.741_fp_kind,  3.366_fp_kind,  3.976_fp_kind, &
             4.255_fp_kind,  4.701_fp_kind,  4.916_fp_kind,  5.125_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.65318_fp_kind,1.04797_fp_kind, &
             2.13548_fp_kind,3.82386_fp_kind,5.26768_fp_kind,6.11313_fp_kind, &
             7.35964_fp_kind,7.75004_fp_kind,7.82119_fp_kind,7.56126_fp_kind, &
             6.92006_fp_kind,6.10266_fp_kind,5.55513_fp_kind,5.15298_fp_kind, &
             4.59906_fp_kind,2.86792_fp_kind,2.29259_fp_kind,1.80627_fp_kind, &
             1.28988_fp_kind,0.93973_fp_kind,0.72277_fp_kind,0.54848_fp_kind, &
             0.46009_fp_kind,0.29116_fp_kind,0.16277_fp_kind,0.09861_fp_kind, &
             0.06369_fp_kind,0.05193_fp_kind,0.04718_fp_kind,0.04097_fp_kind, &
             0.03966_fp_kind,0.03614_fp_kind,0.03384_fp_kind,0.03342_fp_kind, &
             0.03319_fp_kind,0.03249_fp_kind,0.03070_fp_kind,0.02878_fp_kind, &
             0.02805_fp_kind,0.02689_fp_kind,0.02633_fp_kind,0.02579_fp_kind /)



      CASE( 2 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 4 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1966
        CIMSS_Month = 1
        CIMSS_Day   = 4
        CIMSS_Hour  = 16
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             252.55_fp_kind, 255.64_fp_kind, &
             256.36_fp_kind, 247.66_fp_kind, 238.53_fp_kind, 232.06_fp_kind, &
             231.26_fp_kind, 228.56_fp_kind, 226.46_fp_kind, 218.56_fp_kind, &
             217.56_fp_kind, 213.23_fp_kind, 210.16_fp_kind, 210.16_fp_kind, &
             210.16_fp_kind, 204.16_fp_kind, 206.33_fp_kind, 208.16_fp_kind, &
             209.79_fp_kind, 211.16_fp_kind, 211.85_fp_kind, 212.64_fp_kind, &
             213.16_fp_kind, 214.16_fp_kind, 215.16_fp_kind, 211.16_fp_kind, &
             213.84_fp_kind, 216.16_fp_kind, 218.75_fp_kind, 222.32_fp_kind, &
             224.16_fp_kind, 229.22_fp_kind, 232.47_fp_kind, 235.47_fp_kind, &
             237.16_fp_kind, 240.50_fp_kind, 243.16_fp_kind, 243.16_fp_kind, &
             243.16_fp_kind, 243.16_fp_kind, 243.16_fp_kind, 243.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.002_fp_kind,  0.002_fp_kind,  0.003_fp_kind,  0.004_fp_kind, &
             0.005_fp_kind,  0.053_fp_kind,  0.084_fp_kind,  0.112_fp_kind, &
             0.128_fp_kind,  0.172_fp_kind,  0.207_fp_kind,  0.191_fp_kind, &
             0.185_fp_kind,  0.176_fp_kind,  0.172_fp_kind,  0.167_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 3 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 5 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1966
        CIMSS_Month = 1
        CIMSS_Day   = 17
        CIMSS_Hour  = 16
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             255.59_fp_kind, 249.79_fp_kind, &
             240.07_fp_kind, 226.86_fp_kind, 227.04_fp_kind, 227.16_fp_kind, &
             214.36_fp_kind, 213.85_fp_kind, 213.46_fp_kind, 212.56_fp_kind, &
             203.66_fp_kind, 202.78_fp_kind, 202.16_fp_kind, 202.16_fp_kind, &
             202.16_fp_kind, 199.16_fp_kind, 200.79_fp_kind, 202.16_fp_kind, &
             204.88_fp_kind, 207.16_fp_kind, 208.54_fp_kind, 210.12_fp_kind, &
             211.16_fp_kind, 212.16_fp_kind, 213.16_fp_kind, 211.16_fp_kind, &
             213.30_fp_kind, 215.16_fp_kind, 218.08_fp_kind, 222.09_fp_kind, &
             224.16_fp_kind, 230.00_fp_kind, 233.75_fp_kind, 237.21_fp_kind, &
             239.16_fp_kind, 242.50_fp_kind, 245.16_fp_kind, 243.70_fp_kind, &
             243.11_fp_kind, 242.16_fp_kind, 241.70_fp_kind, 241.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.002_fp_kind,  0.002_fp_kind,  0.003_fp_kind,  0.004_fp_kind, &
             0.005_fp_kind,  0.073_fp_kind,  0.116_fp_kind,  0.156_fp_kind, &
             0.179_fp_kind,  0.236_fp_kind,  0.282_fp_kind,  0.224_fp_kind, &
             0.205_fp_kind,  0.175_fp_kind,  0.160_fp_kind,  0.146_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 4 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 7 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1966
        CIMSS_Month = 2
        CIMSS_Day   = 9
        CIMSS_Hour  = 16
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             225.60_fp_kind, 241.36_fp_kind, &
             263.44_fp_kind, 273.16_fp_kind, 275.70_fp_kind, 278.96_fp_kind, &
             282.16_fp_kind, 279.09_fp_kind, 275.86_fp_kind, 272.46_fp_kind, &
             252.56_fp_kind, 233.02_fp_kind, 219.16_fp_kind, 214.76_fp_kind, &
             211.16_fp_kind, 201.16_fp_kind, 201.16_fp_kind, 201.16_fp_kind, &
             203.34_fp_kind, 205.16_fp_kind, 207.23_fp_kind, 209.60_fp_kind, &
             211.16_fp_kind, 213.16_fp_kind, 216.16_fp_kind, 216.16_fp_kind, &
             219.91_fp_kind, 223.16_fp_kind, 225.75_fp_kind, 229.32_fp_kind, &
             231.16_fp_kind, 236.61_fp_kind, 240.11_fp_kind, 243.34_fp_kind, &
             245.16_fp_kind, 245.16_fp_kind, 245.16_fp_kind, 242.73_fp_kind, &
             241.74_fp_kind, 240.16_fp_kind, 239.40_fp_kind, 238.66_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.021_fp_kind,  0.037_fp_kind,  0.046_fp_kind,  0.059_fp_kind, &
             0.065_fp_kind,  0.123_fp_kind,  0.160_fp_kind,  0.195_fp_kind, &
             0.214_fp_kind,  0.146_fp_kind,  0.092_fp_kind,  0.066_fp_kind, &
             0.058_fp_kind,  0.046_fp_kind,  0.040_fp_kind,  0.034_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 5 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 11 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1967
        CIMSS_Month = 11
        CIMSS_Day   = 22
        CIMSS_Hour  = 12
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             211.57_fp_kind, 219.76_fp_kind, &
             226.36_fp_kind, 214.86_fp_kind, 214.51_fp_kind, 214.26_fp_kind, &
             215.36_fp_kind, 207.53_fp_kind, 201.46_fp_kind, 195.56_fp_kind, &
             191.56_fp_kind, 192.50_fp_kind, 193.16_fp_kind, 195.91_fp_kind, &
             198.16_fp_kind, 202.16_fp_kind, 204.87_fp_kind, 207.16_fp_kind, &
             209.34_fp_kind, 211.16_fp_kind, 212.19_fp_kind, 213.38_fp_kind, &
             214.16_fp_kind, 216.16_fp_kind, 216.16_fp_kind, 216.16_fp_kind, &
             220.45_fp_kind, 224.16_fp_kind, 227.40_fp_kind, 231.86_fp_kind, &
             234.16_fp_kind, 241.95_fp_kind, 246.95_fp_kind, 251.56_fp_kind, &
             254.16_fp_kind, 256.39_fp_kind, 258.16_fp_kind, 258.16_fp_kind, &
             258.16_fp_kind, 258.16_fp_kind, 258.16_fp_kind, 258.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.027_fp_kind,  0.048_fp_kind,  0.074_fp_kind,  0.110_fp_kind, &
             0.128_fp_kind,  0.351_fp_kind,  0.494_fp_kind,  0.626_fp_kind, &
             0.700_fp_kind,  0.963_fp_kind,  1.171_fp_kind,  1.082_fp_kind, &
             1.049_fp_kind,  0.995_fp_kind,  0.969_fp_kind,  0.944_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 6 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 17 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1968
        CIMSS_Month = 2
        CIMSS_Day   = 5
        CIMSS_Hour  = 10
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             284.74_fp_kind, 255.46_fp_kind, &
             213.36_fp_kind, 197.86_fp_kind, 198.62_fp_kind, 199.16_fp_kind, &
             198.26_fp_kind, 198.37_fp_kind, 198.46_fp_kind, 198.46_fp_kind, &
             198.56_fp_kind, 202.42_fp_kind, 205.16_fp_kind, 207.91_fp_kind, &
             210.16_fp_kind, 214.16_fp_kind, 214.70_fp_kind, 215.16_fp_kind, &
             217.34_fp_kind, 219.16_fp_kind, 219.85_fp_kind, 220.64_fp_kind, &
             221.16_fp_kind, 221.16_fp_kind, 219.16_fp_kind, 216.16_fp_kind, &
             219.38_fp_kind, 222.16_fp_kind, 225.40_fp_kind, 229.86_fp_kind, &
             232.16_fp_kind, 237.22_fp_kind, 240.47_fp_kind, 243.47_fp_kind, &
             245.16_fp_kind, 249.62_fp_kind, 253.16_fp_kind, 251.21_fp_kind, &
             250.42_fp_kind, 249.16_fp_kind, 248.55_fp_kind, 247.96_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.004_fp_kind,  0.005_fp_kind,  0.008_fp_kind,  0.012_fp_kind, &
             0.014_fp_kind,  0.142_fp_kind,  0.224_fp_kind,  0.299_fp_kind, &
             0.342_fp_kind,  0.563_fp_kind,  0.739_fp_kind,  0.500_fp_kind, &
             0.462_fp_kind,  0.402_fp_kind,  0.373_fp_kind,  0.345_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 7 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 18 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1968
        CIMSS_Month = 2
        CIMSS_Day   = 19
        CIMSS_Hour  = 9
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             234.18_fp_kind, 244.06_fp_kind, &
             251.16_fp_kind, 222.86_fp_kind, 223.04_fp_kind, 223.16_fp_kind, &
             218.36_fp_kind, 215.04_fp_kind, 212.46_fp_kind, 212.56_fp_kind, &
             209.56_fp_kind, 205.23_fp_kind, 202.16_fp_kind, 198.86_fp_kind, &
             196.16_fp_kind, 197.16_fp_kind, 199.87_fp_kind, 202.16_fp_kind, &
             201.62_fp_kind, 201.16_fp_kind, 202.88_fp_kind, 204.86_fp_kind, &
             206.16_fp_kind, 205.16_fp_kind, 205.16_fp_kind, 213.16_fp_kind, &
             217.98_fp_kind, 222.16_fp_kind, 226.37_fp_kind, 232.17_fp_kind, &
             235.16_fp_kind, 241.39_fp_kind, 245.39_fp_kind, 249.08_fp_kind, &
             251.16_fp_kind, 255.06_fp_kind, 258.16_fp_kind, 254.26_fp_kind, &
             252.68_fp_kind, 250.16_fp_kind, 248.95_fp_kind, 247.76_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.004_fp_kind,  0.005_fp_kind,  0.046_fp_kind,  0.103_fp_kind, &
             0.132_fp_kind,  0.289_fp_kind,  0.389_fp_kind,  0.482_fp_kind, &
             0.534_fp_kind,  0.780_fp_kind,  0.976_fp_kind,  0.726_fp_kind, &
             0.606_fp_kind,  0.415_fp_kind,  0.323_fp_kind,  0.233_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 8 )

        CIMSS_Description = 'West Geirinish,UK. Record 95 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1967
        CIMSS_Month = 12
        CIMSS_Day   = 21
        CIMSS_Hour  = 18
        CIMSS_Latitude  =  57.35_fp_kind
        CIMSS_Longitude = 352.63_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             207.19_fp_kind, 229.26_fp_kind, &
             258.16_fp_kind, 296.56_fp_kind, 289.83_fp_kind, 285.06_fp_kind, &
             263.26_fp_kind, 245.35_fp_kind, 231.46_fp_kind, 219.56_fp_kind, &
             204.56_fp_kind, 194.97_fp_kind, 188.16_fp_kind, 188.16_fp_kind, &
             188.16_fp_kind, 191.16_fp_kind, 192.24_fp_kind, 193.16_fp_kind, &
             193.70_fp_kind, 194.16_fp_kind, 194.50_fp_kind, 194.90_fp_kind, &
             195.16_fp_kind, 206.16_fp_kind, 216.16_fp_kind, 226.16_fp_kind, &
             235.27_fp_kind, 243.16_fp_kind, 246.73_fp_kind, 251.63_fp_kind, &
             254.16_fp_kind, 260.39_fp_kind, 264.39_fp_kind, 268.08_fp_kind, &
             270.16_fp_kind, 272.39_fp_kind, 274.16_fp_kind, 274.16_fp_kind, &
             274.16_fp_kind, 274.16_fp_kind, 274.16_fp_kind, 274.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.006_fp_kind,  0.011_fp_kind, &
             0.159_fp_kind,  0.287_fp_kind,  0.490_fp_kind,  0.769_fp_kind, &
             0.913_fp_kind,  1.625_fp_kind,  2.082_fp_kind,  2.504_fp_kind, &
             2.742_fp_kind,  3.518_fp_kind,  4.134_fp_kind,  3.820_fp_kind, &
             3.702_fp_kind,  3.514_fp_kind,  3.423_fp_kind,  3.335_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 9 )

        CIMSS_Description = 'West Geirinish,UK. Record 98 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1967
        CIMSS_Month = 12
        CIMSS_Day   = 23
        CIMSS_Hour  = 17
        CIMSS_Latitude  =  57.35_fp_kind
        CIMSS_Longitude = 352.63_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             208.36_fp_kind, 220.46_fp_kind, &
             254.76_fp_kind, 276.16_fp_kind, 282.48_fp_kind, 286.96_fp_kind, &
             294.16_fp_kind, 277.43_fp_kind, 264.46_fp_kind, 229.46_fp_kind, &
             215.56_fp_kind, 205.97_fp_kind, 199.16_fp_kind, 199.71_fp_kind, &
             200.16_fp_kind, 206.16_fp_kind, 206.16_fp_kind, 206.16_fp_kind, &
             206.70_fp_kind, 207.16_fp_kind, 209.57_fp_kind, 212.34_fp_kind, &
             214.16_fp_kind, 219.16_fp_kind, 219.16_fp_kind, 220.16_fp_kind, &
             228.20_fp_kind, 235.16_fp_kind, 239.37_fp_kind, 245.17_fp_kind, &
             248.16_fp_kind, 255.56_fp_kind, 260.31_fp_kind, 264.69_fp_kind, &
             267.16_fp_kind, 270.50_fp_kind, 273.16_fp_kind, 273.16_fp_kind, &
             273.16_fp_kind, 273.16_fp_kind, 273.16_fp_kind, 273.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.016_fp_kind,  0.025_fp_kind,  0.189_fp_kind,  0.415_fp_kind, &
             0.532_fp_kind,  1.392_fp_kind,  1.944_fp_kind,  2.453_fp_kind, &
             2.740_fp_kind,  3.530_fp_kind,  4.158_fp_kind,  3.842_fp_kind, &
             3.723_fp_kind,  3.534_fp_kind,  3.443_fp_kind,  3.354_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 10 )

        CIMSS_Description = 'West Geirinish,UK. Record 100 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1967
        CIMSS_Month = 12
        CIMSS_Day   = 19
        CIMSS_Hour  = 17
        CIMSS_Latitude  =  57.35_fp_kind
        CIMSS_Longitude = 352.63_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             235.12_fp_kind, 253.86_fp_kind, &
             268.56_fp_kind, 263.66_fp_kind, 259.86_fp_kind, 257.16_fp_kind, &
             249.36_fp_kind, 240.41_fp_kind, 233.46_fp_kind, 217.56_fp_kind, &
             201.66_fp_kind, 197.27_fp_kind, 194.16_fp_kind, 191.96_fp_kind, &
             190.16_fp_kind, 190.16_fp_kind, 192.33_fp_kind, 194.16_fp_kind, &
             196.34_fp_kind, 198.16_fp_kind, 199.19_fp_kind, 200.38_fp_kind, &
             201.16_fp_kind, 202.16_fp_kind, 210.16_fp_kind, 218.16_fp_kind, &
             227.27_fp_kind, 235.16_fp_kind, 238.73_fp_kind, 243.63_fp_kind, &
             246.16_fp_kind, 252.78_fp_kind, 257.03_fp_kind, 260.95_fp_kind, &
             263.16_fp_kind, 263.16_fp_kind, 263.16_fp_kind, 268.03_fp_kind, &
             270.00_fp_kind, 273.16_fp_kind, 274.68_fp_kind, 276.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.004_fp_kind, &
             0.015_fp_kind,  0.025_fp_kind,  0.056_fp_kind,  0.099_fp_kind, &
             0.121_fp_kind,  0.519_fp_kind,  0.774_fp_kind,  1.010_fp_kind, &
             1.143_fp_kind,  1.345_fp_kind,  1.506_fp_kind,  2.223_fp_kind, &
             2.625_fp_kind,  3.268_fp_kind,  3.577_fp_kind,  3.879_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 11 )

        CIMSS_Description = 'Fort Greely,AK. Record 44 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1966
        CIMSS_Month = 12
        CIMSS_Day   = 15
        CIMSS_Hour  = 21
        CIMSS_Latitude  =  63.97_fp_kind
        CIMSS_Longitude = 214.30_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             273.12_fp_kind, 266.86_fp_kind, &
             253.56_fp_kind, 235.66_fp_kind, 221.27_fp_kind, 211.06_fp_kind, &
             209.26_fp_kind, 205.37_fp_kind, 202.36_fp_kind, 205.46_fp_kind, &
             205.56_fp_kind, 209.71_fp_kind, 212.66_fp_kind, 214.59_fp_kind, &
             216.16_fp_kind, 220.16_fp_kind, 220.70_fp_kind, 221.16_fp_kind, &
             222.79_fp_kind, 224.16_fp_kind, 224.16_fp_kind, 224.16_fp_kind, &
             224.16_fp_kind, 223.16_fp_kind, 223.16_fp_kind, 222.16_fp_kind, &
             226.98_fp_kind, 231.16_fp_kind, 234.40_fp_kind, 238.86_fp_kind, &
             241.16_fp_kind, 249.34_fp_kind, 254.59_fp_kind, 259.43_fp_kind, &
             262.16_fp_kind, 261.05_fp_kind, 260.16_fp_kind, 260.16_fp_kind, &
             260.16_fp_kind, 260.16_fp_kind, 260.16_fp_kind, 260.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.004_fp_kind,  0.007_fp_kind, &
             0.012_fp_kind,  0.016_fp_kind,  0.081_fp_kind,  0.170_fp_kind, &
             0.216_fp_kind,  0.634_fp_kind,  0.903_fp_kind,  1.150_fp_kind, &
             1.290_fp_kind,  1.011_fp_kind,  0.789_fp_kind,  0.707_fp_kind, &
             0.677_fp_kind,  0.629_fp_kind,  0.606_fp_kind,  0.583_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 12 )

        CIMSS_Description = 'Ostrov Kheisa,RA. Record 104 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1966
        CIMSS_Month = 1
        CIMSS_Day   = 2
        CIMSS_Hour  = 12
        CIMSS_Latitude  = 80.62_fp_kind
        CIMSS_Longitude = 58.05_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             252.55_fp_kind, 255.64_fp_kind, &
             256.36_fp_kind, 247.66_fp_kind, 238.53_fp_kind, 232.06_fp_kind, &
             231.26_fp_kind, 228.56_fp_kind, 226.46_fp_kind, 218.46_fp_kind, &
             217.56_fp_kind, 213.23_fp_kind, 210.16_fp_kind, 210.16_fp_kind, &
             210.16_fp_kind, 204.16_fp_kind, 206.33_fp_kind, 208.16_fp_kind, &
             210.34_fp_kind, 212.16_fp_kind, 212.85_fp_kind, 213.64_fp_kind, &
             214.16_fp_kind, 216.16_fp_kind, 215.16_fp_kind, 214.16_fp_kind, &
             217.38_fp_kind, 220.16_fp_kind, 223.08_fp_kind, 227.09_fp_kind, &
             229.16_fp_kind, 233.05_fp_kind, 235.55_fp_kind, 237.86_fp_kind, &
             239.16_fp_kind, 242.50_fp_kind, 245.16_fp_kind, 245.16_fp_kind, &
             245.16_fp_kind, 245.16_fp_kind, 245.16_fp_kind, 245.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.004_fp_kind,  0.006_fp_kind,  0.009_fp_kind, &
             0.010_fp_kind,  0.015_fp_kind,  0.018_fp_kind,  0.020_fp_kind, &
             0.022_fp_kind,  0.068_fp_kind,  0.105_fp_kind,  0.097_fp_kind, &
             0.094_fp_kind,  0.089_fp_kind,  0.087_fp_kind,  0.084_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.75492_fp_kind,1.20217_fp_kind, &
             2.39283_fp_kind,3.75644_fp_kind,4.96742_fp_kind,5.64285_fp_kind, &
             6.17910_fp_kind,6.22151_fp_kind,6.15197_fp_kind,5.88338_fp_kind, &
             5.42542_fp_kind,4.91094_fp_kind,4.76030_fp_kind,4.63605_fp_kind, &
             4.52229_fp_kind,3.70528_fp_kind,3.01350_fp_kind,2.39073_fp_kind, &
             1.76424_fp_kind,1.38778_fp_kind,1.12046_fp_kind,0.82870_fp_kind, &
             0.66060_fp_kind,0.36047_fp_kind,0.28088_fp_kind,0.17023_fp_kind, &
             0.09235_fp_kind,0.06541_fp_kind,0.05169_fp_kind,0.04171_fp_kind, &
             0.03941_fp_kind,0.03409_fp_kind,0.03095_fp_kind,0.02819_fp_kind, &
             0.02673_fp_kind,0.02330_fp_kind,0.02159_fp_kind,0.01999_fp_kind, &
             0.01933_fp_kind,0.01828_fp_kind,0.01777_fp_kind,0.01728_fp_kind /)


      CASE( 13 )

        CIMSS_Description = 'Churchill,MB,CN. Record 158 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1966
        CIMSS_Month = 1
        CIMSS_Day   = 4
        CIMSS_Hour  = 0
        CIMSS_Latitude  =  58.75_fp_kind
        CIMSS_Longitude = 265.93_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             264.60_fp_kind, 264.86_fp_kind, &
             239.56_fp_kind, 245.66_fp_kind, 233.61_fp_kind, 225.06_fp_kind, &
             224.26_fp_kind, 217.61_fp_kind, 212.46_fp_kind, 211.46_fp_kind, &
             209.56_fp_kind, 208.16_fp_kind, 207.16_fp_kind, 207.71_fp_kind, &
             208.16_fp_kind, 218.16_fp_kind, 219.24_fp_kind, 220.16_fp_kind, &
             220.16_fp_kind, 220.16_fp_kind, 219.13_fp_kind, 217.94_fp_kind, &
             217.16_fp_kind, 215.16_fp_kind, 213.16_fp_kind, 207.16_fp_kind, &
             215.20_fp_kind, 222.16_fp_kind, 225.73_fp_kind, 230.63_fp_kind, &
             233.16_fp_kind, 238.22_fp_kind, 241.47_fp_kind, 244.47_fp_kind, &
             246.16_fp_kind, 247.83_fp_kind, 249.16_fp_kind, 243.80_fp_kind, &
             241.63_fp_kind, 238.16_fp_kind, 236.49_fp_kind, 234.86_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.005_fp_kind,  0.009_fp_kind,  0.013_fp_kind, &
             0.016_fp_kind,  0.125_fp_kind,  0.196_fp_kind,  0.260_fp_kind, &
             0.297_fp_kind,  0.341_fp_kind,  0.376_fp_kind,  0.201_fp_kind, &
             0.163_fp_kind,  0.102_fp_kind,  0.073_fp_kind,  0.044_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 14 )

        CIMSS_Description = 'Churchill,MB,CN. Record 161 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1967
        CIMSS_Month = 1
        CIMSS_Day   = 19
        CIMSS_Hour  = 0
        CIMSS_Latitude  =  58.75_fp_kind
        CIMSS_Longitude = 265.93_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             228.19_fp_kind, 234.26_fp_kind, &
             237.46_fp_kind, 246.56_fp_kind, 248.02_fp_kind, 249.06_fp_kind, &
             253.26_fp_kind, 254.50_fp_kind, 255.46_fp_kind, 240.46_fp_kind, &
             230.56_fp_kind, 223.31_fp_kind, 218.16_fp_kind, 216.51_fp_kind, &
             215.16_fp_kind, 203.16_fp_kind, 205.33_fp_kind, 207.16_fp_kind, &
             208.79_fp_kind, 210.16_fp_kind, 211.54_fp_kind, 213.12_fp_kind, &
             214.16_fp_kind, 214.16_fp_kind, 216.16_fp_kind, 215.16_fp_kind, &
             218.38_fp_kind, 221.16_fp_kind, 223.43_fp_kind, 226.55_fp_kind, &
             228.16_fp_kind, 232.44_fp_kind, 235.19_fp_kind, 237.73_fp_kind, &
             239.16_fp_kind, 240.27_fp_kind, 241.16_fp_kind, 237.26_fp_kind, &
             235.68_fp_kind, 233.16_fp_kind, 231.95_fp_kind, 230.76_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.004_fp_kind,  0.005_fp_kind,  0.006_fp_kind,  0.008_fp_kind, &
             0.009_fp_kind,  0.080_fp_kind,  0.126_fp_kind,  0.168_fp_kind, &
             0.192_fp_kind,  0.203_fp_kind,  0.212_fp_kind,  0.128_fp_kind, &
             0.108_fp_kind,  0.075_fp_kind,  0.059_fp_kind,  0.044_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 15 )

        CIMSS_Description = 'Thule AFB,GL. Record 225 from set of 1200.'
        CIMSS_Climatology_Model = 4
        CIMSS_Year  = 1968
        CIMSS_Month = 5
        CIMSS_Day   = 14
        CIMSS_Hour  = 16
        CIMSS_Latitude  =  76.52_fp_kind
        CIMSS_Longitude = 291.25_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             275.23_fp_kind, 280.58_fp_kind, &
             285.67_fp_kind, 286.06_fp_kind, 275.36_fp_kind, 267.76_fp_kind, &
             265.06_fp_kind, 256.73_fp_kind, 250.26_fp_kind, 246.46_fp_kind, &
             241.46_fp_kind, 237.48_fp_kind, 234.66_fp_kind, 233.28_fp_kind, &
             232.16_fp_kind, 229.16_fp_kind, 228.08_fp_kind, 227.16_fp_kind, &
             225.53_fp_kind, 224.16_fp_kind, 224.85_fp_kind, 225.64_fp_kind, &
             226.16_fp_kind, 225.16_fp_kind, 222.16_fp_kind, 222.16_fp_kind, &
             228.05_fp_kind, 233.16_fp_kind, 236.08_fp_kind, 240.09_fp_kind, &
             242.16_fp_kind, 246.83_fp_kind, 249.83_fp_kind, 252.60_fp_kind, &
             254.16_fp_kind, 258.06_fp_kind, 261.16_fp_kind, 264.08_fp_kind, &
             265.27_fp_kind, 267.16_fp_kind, 268.07_fp_kind, 268.96_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.007_fp_kind,  0.013_fp_kind,  0.023_fp_kind, &
             0.046_fp_kind,  0.065_fp_kind,  0.090_fp_kind,  0.125_fp_kind, &
             0.143_fp_kind,  0.228_fp_kind,  0.283_fp_kind,  0.333_fp_kind, &
             0.362_fp_kind,  0.488_fp_kind,  0.588_fp_kind,  0.736_fp_kind, &
             0.808_fp_kind,  0.923_fp_kind,  0.978_fp_kind,  1.032_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58825_fp_kind,0.98312_fp_kind, &
             1.64271_fp_kind,2.52996_fp_kind,3.56841_fp_kind,4.56575_fp_kind, &
             6.36529_fp_kind,7.39635_fp_kind,7.78289_fp_kind,7.56976_fp_kind, &
             6.69058_fp_kind,5.57509_fp_kind,5.21478_fp_kind,4.73043_fp_kind, &
             4.34708_fp_kind,2.75529_fp_kind,2.05541_fp_kind,1.64657_fp_kind, &
             1.17452_fp_kind,0.92611_fp_kind,0.78889_fp_kind,0.65317_fp_kind, &
             0.58224_fp_kind,0.40198_fp_kind,0.24128_fp_kind,0.15314_fp_kind, &
             0.10010_fp_kind,0.08052_fp_kind,0.07394_fp_kind,0.06544_fp_kind, &
             0.06065_fp_kind,0.04986_fp_kind,0.04448_fp_kind,0.04090_fp_kind, &
             0.03887_fp_kind,0.03446_fp_kind,0.03129_fp_kind,0.02823_fp_kind, &
             0.02682_fp_kind,0.02456_fp_kind,0.02347_fp_kind,0.02241_fp_kind /)


      CASE( 16 )

        CIMSS_Description = 'McMurdo(US),AA. Record 290 from set of 1200.'
        CIMSS_Climatology_Model = 4
        CIMSS_Year  = 1963
        CIMSS_Month = 1
        CIMSS_Day   = 9
        CIMSS_Hour  = 3
        CIMSS_Latitude  = -77.85_fp_kind
        CIMSS_Longitude = 166.67_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             242.04_fp_kind, 270.06_fp_kind, &
             297.26_fp_kind, 303.16_fp_kind, 302.22_fp_kind, 301.56_fp_kind, &
             291.96_fp_kind, 281.99_fp_kind, 274.26_fp_kind, 264.36_fp_kind, &
             254.46_fp_kind, 247.56_fp_kind, 242.66_fp_kind, 241.28_fp_kind, &
             240.16_fp_kind, 238.16_fp_kind, 237.62_fp_kind, 237.16_fp_kind, &
             236.62_fp_kind, 236.16_fp_kind, 235.13_fp_kind, 233.94_fp_kind, &
             233.16_fp_kind, 232.16_fp_kind, 226.16_fp_kind, 220.16_fp_kind, &
             227.13_fp_kind, 233.16_fp_kind, 236.73_fp_kind, 241.63_fp_kind, &
             244.16_fp_kind, 248.83_fp_kind, 251.83_fp_kind, 254.60_fp_kind, &
             256.16_fp_kind, 258.39_fp_kind, 260.16_fp_kind, 264.54_fp_kind, &
             266.32_fp_kind, 269.16_fp_kind, 270.53_fp_kind, 271.86_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.013_fp_kind,  0.020_fp_kind,  0.030_fp_kind,  0.045_fp_kind, &
             0.052_fp_kind,  0.087_fp_kind,  0.110_fp_kind,  0.131_fp_kind, &
             0.143_fp_kind,  0.155_fp_kind,  0.164_fp_kind,  0.215_fp_kind, &
             0.241_fp_kind,  0.283_fp_kind,  0.303_fp_kind,  0.323_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58825_fp_kind,0.98312_fp_kind, &
             1.64271_fp_kind,2.52996_fp_kind,3.56841_fp_kind,4.56575_fp_kind, &
             6.36529_fp_kind,7.39635_fp_kind,7.78289_fp_kind,7.56976_fp_kind, &
             6.69058_fp_kind,5.57509_fp_kind,5.21478_fp_kind,4.73043_fp_kind, &
             4.34708_fp_kind,2.75529_fp_kind,2.05541_fp_kind,1.64657_fp_kind, &
             1.17452_fp_kind,0.92611_fp_kind,0.78889_fp_kind,0.65317_fp_kind, &
             0.58224_fp_kind,0.40198_fp_kind,0.24128_fp_kind,0.15314_fp_kind, &
             0.10010_fp_kind,0.08052_fp_kind,0.07394_fp_kind,0.06544_fp_kind, &
             0.06065_fp_kind,0.04986_fp_kind,0.04448_fp_kind,0.04090_fp_kind, &
             0.03887_fp_kind,0.03446_fp_kind,0.03129_fp_kind,0.02823_fp_kind, &
             0.02682_fp_kind,0.02456_fp_kind,0.02347_fp_kind,0.02241_fp_kind /)


      CASE( 17 )

        CIMSS_Description = 'White Sands,NM. Record 599 from set of 1200.'
        CIMSS_Climatology_Model = 3
        CIMSS_Year  = 1967
        CIMSS_Month = 2
        CIMSS_Day   = 26
        CIMSS_Hour  = 0
        CIMSS_Latitude  =  32.38_fp_kind
        CIMSS_Longitude = 253.52_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             243.19_fp_kind, 251.87_fp_kind, &
             259.76_fp_kind, 263.16_fp_kind, 260.12_fp_kind, 257.96_fp_kind, &
             249.16_fp_kind, 245.89_fp_kind, 243.36_fp_kind, 240.46_fp_kind, &
             220.56_fp_kind, 218.86_fp_kind, 217.66_fp_kind, 217.94_fp_kind, &
             218.16_fp_kind, 213.16_fp_kind, 210.99_fp_kind, 209.16_fp_kind, &
             208.07_fp_kind, 207.16_fp_kind, 208.88_fp_kind, 210.86_fp_kind, &
             212.16_fp_kind, 214.16_fp_kind, 224.16_fp_kind, 233.16_fp_kind, &
             241.73_fp_kind, 249.16_fp_kind, 252.73_fp_kind, 257.63_fp_kind, &
             260.16_fp_kind, 266.00_fp_kind, 269.75_fp_kind, 273.21_fp_kind, &
             275.16_fp_kind, 281.85_fp_kind, 287.16_fp_kind, 289.11_fp_kind, &
             289.90_fp_kind, 291.16_fp_kind, 291.77_fp_kind, 292.36_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.005_fp_kind,  0.008_fp_kind,  0.013_fp_kind, &
             0.018_fp_kind,  0.038_fp_kind,  0.082_fp_kind,  0.141_fp_kind, &
             0.378_fp_kind,  0.584_fp_kind,  1.137_fp_kind,  1.899_fp_kind, &
             2.291_fp_kind,  3.695_fp_kind,  4.596_fp_kind,  5.427_fp_kind, &
             5.896_fp_kind,  4.371_fp_kind,  3.159_fp_kind,  3.070_fp_kind, &
             3.025_fp_kind,  2.954_fp_kind,  2.920_fp_kind,  2.886_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58382_fp_kind,1.06610_fp_kind, &
             2.23416_fp_kind,3.87594_fp_kind,5.18854_fp_kind,6.20949_fp_kind, &
             7.04493_fp_kind,7.17104_fp_kind,7.10972_fp_kind,6.86107_fp_kind, &
             6.29020_fp_kind,5.71788_fp_kind,5.35257_fp_kind,5.03882_fp_kind, &
             4.57679_fp_kind,3.16918_fp_kind,2.47482_fp_kind,1.95801_fp_kind, &
             1.43279_fp_kind,1.11336_fp_kind,0.93068_fp_kind,0.81309_fp_kind, &
             0.74765_fp_kind,0.46038_fp_kind,0.25869_fp_kind,0.15587_fp_kind, &
             0.10257_fp_kind,0.07960_fp_kind,0.06900_fp_kind,0.05625_fp_kind, &
             0.05211_fp_kind,0.04120_fp_kind,0.03513_fp_kind,0.03297_fp_kind, &
             0.03176_fp_kind,0.02883_fp_kind,0.02821_fp_kind,0.02796_fp_kind, &
             0.02790_fp_kind,0.02781_fp_kind,0.02777_fp_kind,0.02772_fp_kind /)


      CASE( 18 )

        CIMSS_Description = 'Thule AFB,GL. Record 329 from set of 1200.'
        CIMSS_Climatology_Model = 4
        CIMSS_Year  = 1968
        CIMSS_Month = 6
        CIMSS_Day   = 30
        CIMSS_Hour  = 12
        CIMSS_Latitude  =  76.52_fp_kind
        CIMSS_Longitude = 291.25_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             239.37_fp_kind, 257.47_fp_kind, &
             277.46_fp_kind, 283.56_fp_kind, 280.69_fp_kind, 278.66_fp_kind, &
             273.06_fp_kind, 266.41_fp_kind, 261.26_fp_kind, 254.36_fp_kind, &
             248.46_fp_kind, 243.31_fp_kind, 239.66_fp_kind, 236.63_fp_kind, &
             234.16_fp_kind, 231.16_fp_kind, 231.16_fp_kind, 231.16_fp_kind, &
             231.70_fp_kind, 232.16_fp_kind, 232.50_fp_kind, 232.90_fp_kind, &
             233.16_fp_kind, 235.16_fp_kind, 233.16_fp_kind, 226.16_fp_kind, &
             229.91_fp_kind, 233.16_fp_kind, 236.73_fp_kind, 241.63_fp_kind, &
             244.16_fp_kind, 250.00_fp_kind, 253.75_fp_kind, 257.21_fp_kind, &
             259.16_fp_kind, 264.73_fp_kind, 269.16_fp_kind, 272.57_fp_kind, &
             273.95_fp_kind, 276.16_fp_kind, 277.22_fp_kind, 278.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.006_fp_kind,  0.011_fp_kind, &
             0.055_fp_kind,  0.094_fp_kind,  0.159_fp_kind,  0.250_fp_kind, &
             0.296_fp_kind,  0.603_fp_kind,  0.800_fp_kind,  0.982_fp_kind, &
             1.085_fp_kind,  1.926_fp_kind,  2.594_fp_kind,  2.759_fp_kind, &
             2.804_fp_kind,  2.876_fp_kind,  2.911_fp_kind,  2.944_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.58825_fp_kind,0.98312_fp_kind, &
             1.64271_fp_kind,2.52996_fp_kind,3.56841_fp_kind,4.56575_fp_kind, &
             6.36529_fp_kind,7.39635_fp_kind,7.78289_fp_kind,7.56976_fp_kind, &
             6.69058_fp_kind,5.57509_fp_kind,5.21478_fp_kind,4.73043_fp_kind, &
             4.34708_fp_kind,2.75529_fp_kind,2.05541_fp_kind,1.64657_fp_kind, &
             1.17452_fp_kind,0.92611_fp_kind,0.78889_fp_kind,0.65317_fp_kind, &
             0.58224_fp_kind,0.40198_fp_kind,0.24128_fp_kind,0.15314_fp_kind, &
             0.10010_fp_kind,0.08052_fp_kind,0.07394_fp_kind,0.06544_fp_kind, &
             0.06065_fp_kind,0.04986_fp_kind,0.04448_fp_kind,0.04090_fp_kind, &
             0.03887_fp_kind,0.03446_fp_kind,0.03129_fp_kind,0.02823_fp_kind, &
             0.02682_fp_kind,0.02456_fp_kind,0.02347_fp_kind,0.02241_fp_kind /)


      CASE( 19 )

        CIMSS_Description = 'White Sands,NM. Record 492 from set of 1200.'
        CIMSS_Climatology_Model = 5
        CIMSS_Year  = 1968
        CIMSS_Month = 10
        CIMSS_Day   = 9
        CIMSS_Hour  = 17
        CIMSS_Latitude  =  32.38_fp_kind
        CIMSS_Longitude = 253.52_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             242.37_fp_kind, 251.46_fp_kind, &
             262.76_fp_kind, 260.16_fp_kind, 254.72_fp_kind, 250.86_fp_kind, &
             248.06_fp_kind, 241.47_fp_kind, 236.36_fp_kind, 232.46_fp_kind, &
             227.56_fp_kind, 225.28_fp_kind, 223.66_fp_kind, 221.73_fp_kind, &
             220.16_fp_kind, 215.16_fp_kind, 211.91_fp_kind, 209.16_fp_kind, &
             205.35_fp_kind, 202.16_fp_kind, 204.57_fp_kind, 207.34_fp_kind, &
             209.16_fp_kind, 220.16_fp_kind, 230.16_fp_kind, 239.16_fp_kind, &
             247.20_fp_kind, 254.16_fp_kind, 258.05_fp_kind, 263.40_fp_kind, &
             266.16_fp_kind, 271.61_fp_kind, 275.11_fp_kind, 278.34_fp_kind, &
             280.16_fp_kind, 287.96_fp_kind, 294.16_fp_kind, 299.52_fp_kind, &
             301.69_fp_kind, 305.16_fp_kind, 306.83_fp_kind, 308.46_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.006_fp_kind,  0.015_fp_kind,  0.030_fp_kind,  0.051_fp_kind, &
             0.273_fp_kind,  0.465_fp_kind,  0.576_fp_kind,  0.730_fp_kind, &
             0.809_fp_kind,  1.604_fp_kind,  2.114_fp_kind,  2.584_fp_kind, &
             2.850_fp_kind,  4.204_fp_kind,  5.279_fp_kind,  6.280_fp_kind, &
             6.709_fp_kind,  7.396_fp_kind,  7.726_fp_kind,  8.049_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.65318_fp_kind,1.04797_fp_kind, &
             2.13548_fp_kind,3.82386_fp_kind,5.26768_fp_kind,6.11313_fp_kind, &
             7.35964_fp_kind,7.75004_fp_kind,7.82119_fp_kind,7.56126_fp_kind, &
             6.92006_fp_kind,6.10266_fp_kind,5.55513_fp_kind,5.15298_fp_kind, &
             4.59906_fp_kind,2.86792_fp_kind,2.29259_fp_kind,1.80627_fp_kind, &
             1.28988_fp_kind,0.93973_fp_kind,0.72277_fp_kind,0.54848_fp_kind, &
             0.46009_fp_kind,0.29116_fp_kind,0.16277_fp_kind,0.09861_fp_kind, &
             0.06369_fp_kind,0.05193_fp_kind,0.04718_fp_kind,0.04097_fp_kind, &
             0.03966_fp_kind,0.03614_fp_kind,0.03384_fp_kind,0.03342_fp_kind, &
             0.03319_fp_kind,0.03249_fp_kind,0.03070_fp_kind,0.02878_fp_kind, &
             0.02805_fp_kind,0.02689_fp_kind,0.02633_fp_kind,0.02579_fp_kind /)


      CASE( 20 )

        CIMSS_Description = 'Point Mugu NAS,CA. Record 635 from set of 1200.'
        CIMSS_Climatology_Model = 2
        CIMSS_Year  = 1968
        CIMSS_Month = 7
        CIMSS_Day   = 31
        CIMSS_Hour  = 17
        CIMSS_Latitude  =  34.12_fp_kind
        CIMSS_Longitude = 240.88_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             230.77_fp_kind, 244.51_fp_kind, &
             260.46_fp_kind, 270.06_fp_kind, 263.51_fp_kind, 258.86_fp_kind, &
             257.06_fp_kind, 248.78_fp_kind, 242.36_fp_kind, 244.46_fp_kind, &
             238.46_fp_kind, 231.56_fp_kind, 226.66_fp_kind, 224.73_fp_kind, &
             223.16_fp_kind, 217.16_fp_kind, 213.91_fp_kind, 211.16_fp_kind, &
             208.44_fp_kind, 206.16_fp_kind, 208.23_fp_kind, 210.60_fp_kind, &
             212.16_fp_kind, 220.16_fp_kind, 230.16_fp_kind, 239.16_fp_kind, &
             247.73_fp_kind, 255.16_fp_kind, 259.05_fp_kind, 264.40_fp_kind, &
             267.16_fp_kind, 273.00_fp_kind, 276.75_fp_kind, 280.21_fp_kind, &
             282.16_fp_kind, 286.06_fp_kind, 289.16_fp_kind, 291.59_fp_kind, &
             292.58_fp_kind, 294.16_fp_kind, 294.92_fp_kind, 295.66_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.006_fp_kind,  0.010_fp_kind,  0.015_fp_kind,  0.024_fp_kind, &
             0.032_fp_kind,  0.077_fp_kind,  0.150_fp_kind,  0.260_fp_kind, &
             1.071_fp_kind,  1.957_fp_kind,  2.818_fp_kind,  4.003_fp_kind, &
             4.614_fp_kind,  6.036_fp_kind,  6.949_fp_kind,  7.791_fp_kind, &
             8.266_fp_kind,  6.849_fp_kind,  5.723_fp_kind,  9.940_fp_kind, &
            11.891_fp_kind, 15.009_fp_kind, 15.805_fp_kind, 16.140_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.61951_fp_kind,1.07099_fp_kind, &
             1.77685_fp_kind,2.91535_fp_kind,3.98547_fp_kind,5.06939_fp_kind, &
             7.01746_fp_kind,8.18549_fp_kind,8.74393_fp_kind,8.73998_fp_kind, &
             7.87205_fp_kind,6.65253_fp_kind,5.84694_fp_kind,5.12966_fp_kind, &
             4.37609_fp_kind,2.46410_fp_kind,1.97307_fp_kind,1.47696_fp_kind, &
             0.91259_fp_kind,0.66705_fp_kind,0.57759_fp_kind,0.48610_fp_kind, &
             0.44729_fp_kind,0.24487_fp_kind,0.16974_fp_kind,0.12153_fp_kind, &
             0.10001_fp_kind,0.08397_fp_kind,0.07669_fp_kind,0.06661_fp_kind, &
             0.06225_fp_kind,0.05355_fp_kind,0.04892_fp_kind,0.04505_fp_kind, &
             0.04291_fp_kind,0.03815_fp_kind,0.03517_fp_kind,0.03283_fp_kind, &
             0.03194_fp_kind,0.03053_fp_kind,0.02985_fp_kind,0.02919_fp_kind /)


      CASE( 21 )

        CIMSS_Description = 'Point Mugu NAS,CA. Record 662 from set of 1200.'
        CIMSS_Climatology_Model = 2
        CIMSS_Year  = 1966
        CIMSS_Month = 7
        CIMSS_Day   = 18
        CIMSS_Hour  = 17
        CIMSS_Latitude  =  34.12_fp_kind
        CIMSS_Longitude = 240.88_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             234.68_fp_kind, 246.46_fp_kind, &
             262.46_fp_kind, 273.06_fp_kind, 268.85_fp_kind, 265.86_fp_kind, &
             254.06_fp_kind, 250.85_fp_kind, 248.36_fp_kind, 245.46_fp_kind, &
             240.56_fp_kind, 233.01_fp_kind, 227.66_fp_kind, 226.28_fp_kind, &
             225.16_fp_kind, 215.16_fp_kind, 210.83_fp_kind, 207.16_fp_kind, &
             203.89_fp_kind, 201.16_fp_kind, 203.92_fp_kind, 207.08_fp_kind, &
             209.16_fp_kind, 220.16_fp_kind, 230.16_fp_kind, 239.16_fp_kind, &
             247.73_fp_kind, 255.16_fp_kind, 258.72_fp_kind, 263.63_fp_kind, &
             266.16_fp_kind, 273.95_fp_kind, 278.95_fp_kind, 283.56_fp_kind, &
             286.16_fp_kind, 287.27_fp_kind, 288.16_fp_kind, 288.16_fp_kind, &
             288.16_fp_kind, 288.16_fp_kind, 288.16_fp_kind, 288.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.006_fp_kind,  0.015_fp_kind,  0.030_fp_kind,  0.051_fp_kind, &
             0.295_fp_kind,  0.507_fp_kind,  0.489_fp_kind,  0.463_fp_kind, &
             0.450_fp_kind,  0.793_fp_kind,  1.013_fp_kind,  1.216_fp_kind, &
             1.331_fp_kind,  1.842_fp_kind,  2.247_fp_kind,  6.681_fp_kind, &
             8.193_fp_kind, 10.610_fp_kind, 11.774_fp_kind, 12.909_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.61951_fp_kind,1.07099_fp_kind, &
             1.77685_fp_kind,2.91535_fp_kind,3.98547_fp_kind,5.06939_fp_kind, &
             7.01746_fp_kind,8.18549_fp_kind,8.74393_fp_kind,8.73998_fp_kind, &
             7.87205_fp_kind,6.65253_fp_kind,5.84694_fp_kind,5.12966_fp_kind, &
             4.37609_fp_kind,2.46410_fp_kind,1.97307_fp_kind,1.47696_fp_kind, &
             0.91259_fp_kind,0.66705_fp_kind,0.57759_fp_kind,0.48610_fp_kind, &
             0.44729_fp_kind,0.24487_fp_kind,0.16974_fp_kind,0.12153_fp_kind, &
             0.10001_fp_kind,0.08397_fp_kind,0.07669_fp_kind,0.06661_fp_kind, &
             0.06225_fp_kind,0.05355_fp_kind,0.04892_fp_kind,0.04505_fp_kind, &
             0.04291_fp_kind,0.03815_fp_kind,0.03517_fp_kind,0.03283_fp_kind, &
             0.03194_fp_kind,0.03053_fp_kind,0.02985_fp_kind,0.02919_fp_kind /)


      CASE( 22 )

        CIMSS_Description = 'Point Mugu NAS,CA. Record 749 from set of 1200.'
        CIMSS_Climatology_Model = 2
        CIMSS_Year  = 1967
        CIMSS_Month = 7
        CIMSS_Day   = 25
        CIMSS_Hour  = 12
        CIMSS_Latitude  =  34.12_fp_kind
        CIMSS_Longitude = 240.88_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             244.68_fp_kind, 252.46_fp_kind, &
             257.46_fp_kind, 265.06_fp_kind, 264.36_fp_kind, 263.86_fp_kind, &
             256.06_fp_kind, 250.60_fp_kind, 246.36_fp_kind, 238.46_fp_kind, &
             233.56_fp_kind, 228.94_fp_kind, 225.66_fp_kind, 224.28_fp_kind, &
             223.16_fp_kind, 217.16_fp_kind, 213.37_fp_kind, 210.16_fp_kind, &
             207.98_fp_kind, 206.16_fp_kind, 206.50_fp_kind, 206.90_fp_kind, &
             207.16_fp_kind, 219.16_fp_kind, 230.16_fp_kind, 240.16_fp_kind, &
             248.73_fp_kind, 256.16_fp_kind, 260.37_fp_kind, 266.17_fp_kind, &
             269.16_fp_kind, 275.39_fp_kind, 279.39_fp_kind, 283.08_fp_kind, &
             285.16_fp_kind, 291.85_fp_kind, 297.16_fp_kind, 291.32_fp_kind, &
             288.95_fp_kind, 285.16_fp_kind, 283.34_fp_kind, 281.56_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.004_fp_kind,  0.007_fp_kind,  0.011_fp_kind, &
             0.015_fp_kind,  0.035_fp_kind,  0.069_fp_kind,  0.119_fp_kind, &
             0.324_fp_kind,  0.501_fp_kind,  0.650_fp_kind,  0.855_fp_kind, &
             0.961_fp_kind,  2.043_fp_kind,  2.738_fp_kind,  3.378_fp_kind, &
             3.740_fp_kind,  4.577_fp_kind,  5.242_fp_kind,  8.531_fp_kind, &
             8.573_fp_kind,  8.639_fp_kind,  7.538_fp_kind,  6.526_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.61951_fp_kind,1.07099_fp_kind, &
             1.77685_fp_kind,2.91535_fp_kind,3.98547_fp_kind,5.06939_fp_kind, &
             7.01746_fp_kind,8.18549_fp_kind,8.74393_fp_kind,8.73998_fp_kind, &
             7.87205_fp_kind,6.65253_fp_kind,5.84694_fp_kind,5.12966_fp_kind, &
             4.37609_fp_kind,2.46410_fp_kind,1.97307_fp_kind,1.47696_fp_kind, &
             0.91259_fp_kind,0.66705_fp_kind,0.57759_fp_kind,0.48610_fp_kind, &
             0.44729_fp_kind,0.24487_fp_kind,0.16974_fp_kind,0.12153_fp_kind, &
             0.10001_fp_kind,0.08397_fp_kind,0.07669_fp_kind,0.06661_fp_kind, &
             0.06225_fp_kind,0.05355_fp_kind,0.04892_fp_kind,0.04505_fp_kind, &
             0.04291_fp_kind,0.03815_fp_kind,0.03517_fp_kind,0.03283_fp_kind, &
             0.03194_fp_kind,0.03053_fp_kind,0.02985_fp_kind,0.02919_fp_kind /)


      CASE( 23 )

        CIMSS_Description = 'Point Mugu NAS,CA. Record 756 from set of 1200.'
        CIMSS_Climatology_Model = 2
        CIMSS_Year  = 1966
        CIMSS_Month = 7
        CIMSS_Day   = 30
        CIMSS_Hour  = 12
        CIMSS_Latitude  =  34.12_fp_kind
        CIMSS_Longitude = 240.88_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             246.67_fp_kind, 253.46_fp_kind, &
             260.46_fp_kind, 268.06_fp_kind, 266.19_fp_kind, 264.86_fp_kind, &
             254.06_fp_kind, 249.16_fp_kind, 245.36_fp_kind, 243.46_fp_kind, &
             237.56_fp_kind, 233.52_fp_kind, 230.66_fp_kind, 225.98_fp_kind, &
             222.16_fp_kind, 215.16_fp_kind, 213.53_fp_kind, 212.16_fp_kind, &
             208.35_fp_kind, 205.16_fp_kind, 206.19_fp_kind, 207.38_fp_kind, &
             208.16_fp_kind, 218.16_fp_kind, 230.16_fp_kind, 240.16_fp_kind, &
             248.73_fp_kind, 256.16_fp_kind, 259.72_fp_kind, 264.63_fp_kind, &
             267.16_fp_kind, 273.00_fp_kind, 276.75_fp_kind, 280.21_fp_kind, &
             282.16_fp_kind, 287.73_fp_kind, 292.16_fp_kind, 289.24_fp_kind, &
             288.05_fp_kind, 286.16_fp_kind, 285.25_fp_kind, 284.36_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind,  0.008_fp_kind, &
             0.011_fp_kind,  0.025_fp_kind,  0.049_fp_kind,  0.085_fp_kind, &
             0.979_fp_kind,  1.754_fp_kind,  2.634_fp_kind,  3.844_fp_kind, &
             4.468_fp_kind,  6.424_fp_kind,  7.679_fp_kind,  8.836_fp_kind, &
             9.490_fp_kind, 11.193_fp_kind, 12.546_fp_kind, 10.963_fp_kind, &
            10.330_fp_kind,  9.310_fp_kind,  8.557_fp_kind,  7.876_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.61951_fp_kind,1.07099_fp_kind, &
             1.77685_fp_kind,2.91535_fp_kind,3.98547_fp_kind,5.06939_fp_kind, &
             7.01746_fp_kind,8.18549_fp_kind,8.74393_fp_kind,8.73998_fp_kind, &
             7.87205_fp_kind,6.65253_fp_kind,5.84694_fp_kind,5.12966_fp_kind, &
             4.37609_fp_kind,2.46410_fp_kind,1.97307_fp_kind,1.47696_fp_kind, &
             0.91259_fp_kind,0.66705_fp_kind,0.57759_fp_kind,0.48610_fp_kind, &
             0.44729_fp_kind,0.24487_fp_kind,0.16974_fp_kind,0.12153_fp_kind, &
             0.10001_fp_kind,0.08397_fp_kind,0.07669_fp_kind,0.06661_fp_kind, &
             0.06225_fp_kind,0.05355_fp_kind,0.04892_fp_kind,0.04505_fp_kind, &
             0.04291_fp_kind,0.03815_fp_kind,0.03517_fp_kind,0.03283_fp_kind, &
             0.03194_fp_kind,0.03053_fp_kind,0.02985_fp_kind,0.02919_fp_kind /)


      CASE( 24 )

        CIMSS_Description = 'Coolidge Fld,Antigua,AT. Record 838 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1966
        CIMSS_Month = 2
        CIMSS_Day   = 9
        CIMSS_Hour  = 15
        CIMSS_Latitude  =  17.12_fp_kind
        CIMSS_Longitude = 298.22_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             247.23_fp_kind, 256.12_fp_kind, &
             267.46_fp_kind, 273.16_fp_kind, 268.30_fp_kind, 264.86_fp_kind, &
             257.06_fp_kind, 252.72_fp_kind, 249.36_fp_kind, 238.46_fp_kind, &
             230.56_fp_kind, 224.77_fp_kind, 220.66_fp_kind, 218.73_fp_kind, &
             217.16_fp_kind, 208.16_fp_kind, 203.28_fp_kind, 199.16_fp_kind, &
             198.62_fp_kind, 198.16_fp_kind, 202.99_fp_kind, 208.52_fp_kind, &
             212.16_fp_kind, 222.16_fp_kind, 239.16_fp_kind, 243.16_fp_kind, &
             249.05_fp_kind, 254.16_fp_kind, 258.05_fp_kind, 263.40_fp_kind, &
             266.16_fp_kind, 270.83_fp_kind, 273.83_fp_kind, 276.60_fp_kind, &
             278.16_fp_kind, 283.73_fp_kind, 288.16_fp_kind, 293.03_fp_kind, &
             295.00_fp_kind, 298.16_fp_kind, 299.68_fp_kind, 301.16_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.005_fp_kind,  0.007_fp_kind,  0.012_fp_kind, &
             0.017_fp_kind,  0.039_fp_kind,  0.077_fp_kind,  0.133_fp_kind, &
             0.232_fp_kind,  0.317_fp_kind,  0.447_fp_kind,  0.627_fp_kind, &
             0.719_fp_kind,  1.677_fp_kind,  2.292_fp_kind,  2.859_fp_kind, &
             3.179_fp_kind,  5.582_fp_kind,  7.490_fp_kind, 10.328_fp_kind, &
            11.814_fp_kind, 14.190_fp_kind, 15.334_fp_kind, 16.450_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 25 )

        CIMSS_Description = 'Fort Sherman,C.Z.,PM. Record 872 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1968
        CIMSS_Month = 12
        CIMSS_Day   = 20
        CIMSS_Hour  = 16
        CIMSS_Latitude  =   9.33_fp_kind
        CIMSS_Longitude = 280.02_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             254.36_fp_kind, 258.46_fp_kind, &
             261.76_fp_kind, 261.16_fp_kind, 255.78_fp_kind, 251.96_fp_kind, &
             249.16_fp_kind, 247.02_fp_kind, 245.36_fp_kind, 232.46_fp_kind, &
             230.56_fp_kind, 225.94_fp_kind, 222.66_fp_kind, 220.73_fp_kind, &
             219.16_fp_kind, 205.16_fp_kind, 199.74_fp_kind, 195.16_fp_kind, &
             194.62_fp_kind, 194.16_fp_kind, 197.95_fp_kind, 202.30_fp_kind, &
             205.16_fp_kind, 218.16_fp_kind, 230.16_fp_kind, 240.16_fp_kind, &
             249.27_fp_kind, 257.16_fp_kind, 261.37_fp_kind, 267.17_fp_kind, &
             270.16_fp_kind, 275.61_fp_kind, 279.11_fp_kind, 282.34_fp_kind, &
             284.16_fp_kind, 289.18_fp_kind, 293.16_fp_kind, 297.06_fp_kind, &
             298.64_fp_kind, 301.16_fp_kind, 302.37_fp_kind, 303.56_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.007_fp_kind,  0.017_fp_kind,  0.033_fp_kind,  0.057_fp_kind, &
             0.348_fp_kind,  0.600_fp_kind,  0.603_fp_kind,  0.607_fp_kind, &
             0.609_fp_kind,  1.826_fp_kind,  2.607_fp_kind,  3.327_fp_kind, &
             3.734_fp_kind,  7.472_fp_kind, 10.440_fp_kind, 14.576_fp_kind, &
            16.659_fp_kind, 19.990_fp_kind, 21.593_fp_kind, 23.158_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 26 )

        CIMSS_Description = 'Wallops Is.,VA. Record 669 from set of 1200.'
        CIMSS_Climatology_Model = 2
        CIMSS_Year  = 1966
        CIMSS_Month = 8
        CIMSS_Day   = 16
        CIMSS_Hour  = 7
        CIMSS_Latitude  =  37.85_fp_kind
        CIMSS_Longitude = 284.52_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             225.48_fp_kind, 239.36_fp_kind, &
             254.46_fp_kind, 265.16_fp_kind, 261.47_fp_kind, 258.86_fp_kind, &
             256.06_fp_kind, 247.22_fp_kind, 240.36_fp_kind, 239.46_fp_kind, &
             235.56_fp_kind, 229.18_fp_kind, 224.66_fp_kind, 223.28_fp_kind, &
             222.16_fp_kind, 220.16_fp_kind, 217.45_fp_kind, 215.16_fp_kind, &
             211.35_fp_kind, 208.16_fp_kind, 207.13_fp_kind, 205.94_fp_kind, &
             205.16_fp_kind, 217.16_fp_kind, 230.16_fp_kind, 241.16_fp_kind, &
             249.20_fp_kind, 256.16_fp_kind, 259.40_fp_kind, 263.86_fp_kind, &
             266.16_fp_kind, 271.61_fp_kind, 275.11_fp_kind, 278.34_fp_kind, &
             280.16_fp_kind, 285.18_fp_kind, 289.16_fp_kind, 292.57_fp_kind, &
             293.95_fp_kind, 296.16_fp_kind, 297.22_fp_kind, 298.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.004_fp_kind,  0.005_fp_kind,  0.009_fp_kind, &
             0.012_fp_kind,  0.028_fp_kind,  0.055_fp_kind,  0.095_fp_kind, &
             0.219_fp_kind,  0.326_fp_kind,  0.555_fp_kind,  0.871_fp_kind, &
             1.034_fp_kind,  2.920_fp_kind,  4.130_fp_kind,  5.247_fp_kind, &
             5.877_fp_kind,  8.461_fp_kind, 10.514_fp_kind, 13.527_fp_kind, &
            14.981_fp_kind, 17.305_fp_kind, 18.167_fp_kind, 18.873_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.61951_fp_kind,1.07099_fp_kind, &
             1.77685_fp_kind,2.91535_fp_kind,3.98547_fp_kind,5.06939_fp_kind, &
             7.01746_fp_kind,8.18549_fp_kind,8.74393_fp_kind,8.73998_fp_kind, &
             7.87205_fp_kind,6.65253_fp_kind,5.84694_fp_kind,5.12966_fp_kind, &
             4.37609_fp_kind,2.46410_fp_kind,1.97307_fp_kind,1.47696_fp_kind, &
             0.91259_fp_kind,0.66705_fp_kind,0.57759_fp_kind,0.48610_fp_kind, &
             0.44729_fp_kind,0.24487_fp_kind,0.16974_fp_kind,0.12153_fp_kind, &
             0.10001_fp_kind,0.08397_fp_kind,0.07669_fp_kind,0.06661_fp_kind, &
             0.06225_fp_kind,0.05355_fp_kind,0.04892_fp_kind,0.04505_fp_kind, &
             0.04291_fp_kind,0.03815_fp_kind,0.03517_fp_kind,0.03283_fp_kind, &
             0.03194_fp_kind,0.03053_fp_kind,0.02985_fp_kind,0.02919_fp_kind /)


      CASE( 27 )

        CIMSS_Description = 'Fort Sherman,C.Z.,PM. Record 971 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1968
        CIMSS_Month = 11
        CIMSS_Day   = 23
        CIMSS_Hour  = 12
        CIMSS_Latitude  =   9.33_fp_kind
        CIMSS_Longitude = 280.02_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             247.61_fp_kind, 256.94_fp_kind, &
             264.76_fp_kind, 267.16_fp_kind, 268.21_fp_kind, 268.96_fp_kind, &
             252.16_fp_kind, 248.89_fp_kind, 246.36_fp_kind, 237.46_fp_kind, &
             231.56_fp_kind, 228.69_fp_kind, 226.66_fp_kind, 222.53_fp_kind, &
             219.16_fp_kind, 207.16_fp_kind, 200.66_fp_kind, 195.16_fp_kind, &
             191.35_fp_kind, 188.16_fp_kind, 192.64_fp_kind, 197.78_fp_kind, &
             201.16_fp_kind, 217.16_fp_kind, 229.16_fp_kind, 240.16_fp_kind, &
             248.73_fp_kind, 256.16_fp_kind, 259.40_fp_kind, 263.86_fp_kind, &
             266.16_fp_kind, 271.61_fp_kind, 275.11_fp_kind, 278.34_fp_kind, &
             280.16_fp_kind, 285.18_fp_kind, 289.16_fp_kind, 292.57_fp_kind, &
             293.95_fp_kind, 296.16_fp_kind, 297.22_fp_kind, 298.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.004_fp_kind, &
             0.003_fp_kind,  0.001_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.008_fp_kind,  0.057_fp_kind,  0.174_fp_kind,  0.301_fp_kind, &
             0.691_fp_kind,  1.028_fp_kind,  1.336_fp_kind,  1.760_fp_kind, &
             1.979_fp_kind,  4.468_fp_kind,  6.065_fp_kind,  7.539_fp_kind, &
             8.371_fp_kind, 10.085_fp_kind, 11.446_fp_kind, 14.074_fp_kind, &
            15.317_fp_kind, 17.305_fp_kind, 18.167_fp_kind, 18.873_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 28 )

        CIMSS_Description = 'Ascension Is.,AI. Record 1050 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1968
        CIMSS_Month = 7
        CIMSS_Day   = 23
        CIMSS_Hour  = 15
        CIMSS_Latitude  =  -7.97_fp_kind
        CIMSS_Longitude = 345.60_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             236.69_fp_kind, 248.47_fp_kind, &
             264.46_fp_kind, 264.06_fp_kind, 265.70_fp_kind, 266.86_fp_kind, &
             253.06_fp_kind, 248.16_fp_kind, 244.36_fp_kind, 247.46_fp_kind, &
             243.56_fp_kind, 234.26_fp_kind, 227.66_fp_kind, 224.08_fp_kind, &
             221.16_fp_kind, 213.16_fp_kind, 210.45_fp_kind, 208.16_fp_kind, &
             203.81_fp_kind, 200.16_fp_kind, 202.57_fp_kind, 205.34_fp_kind, &
             207.16_fp_kind, 217.16_fp_kind, 229.16_fp_kind, 238.16_fp_kind, &
             246.73_fp_kind, 254.16_fp_kind, 258.05_fp_kind, 263.40_fp_kind, &
             266.16_fp_kind, 272.00_fp_kind, 275.75_fp_kind, 279.21_fp_kind, &
             281.16_fp_kind, 286.18_fp_kind, 290.16_fp_kind, 293.57_fp_kind, &
             294.95_fp_kind, 297.16_fp_kind, 298.22_fp_kind, 299.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.006_fp_kind,  0.015_fp_kind,  0.029_fp_kind,  0.050_fp_kind, &
             0.182_fp_kind,  0.296_fp_kind,  0.477_fp_kind,  0.726_fp_kind, &
             0.854_fp_kind,  1.487_fp_kind,  1.893_fp_kind,  2.267_fp_kind, &
             2.479_fp_kind,  4.892_fp_kind,  6.808_fp_kind,  9.522_fp_kind, &
            10.859_fp_kind, 12.996_fp_kind, 14.025_fp_kind, 15.029_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 29 )

        CIMSS_Description = 'Ascension Is.,AI. Record 1079 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1967
        CIMSS_Month = 6
        CIMSS_Day   = 22
        CIMSS_Hour  = 15
        CIMSS_Latitude  =  -7.97_fp_kind
        CIMSS_Longitude = 345.60_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             240.14_fp_kind, 248.69_fp_kind, &
             261.46_fp_kind, 274.06_fp_kind, 268.09_fp_kind, 263.86_fp_kind, &
             260.06_fp_kind, 254.03_fp_kind, 249.36_fp_kind, 246.46_fp_kind, &
             240.56_fp_kind, 230.67_fp_kind, 223.66_fp_kind, 221.73_fp_kind, &
             220.16_fp_kind, 211.16_fp_kind, 206.83_fp_kind, 203.16_fp_kind, &
             201.53_fp_kind, 200.16_fp_kind, 203.61_fp_kind, 207.56_fp_kind, &
             210.16_fp_kind, 221.16_fp_kind, 233.16_fp_kind, 244.16_fp_kind, &
             252.20_fp_kind, 259.16_fp_kind, 262.72_fp_kind, 267.63_fp_kind, &
             270.16_fp_kind, 275.61_fp_kind, 279.11_fp_kind, 282.34_fp_kind, &
             284.16_fp_kind, 288.62_fp_kind, 292.16_fp_kind, 295.08_fp_kind, &
             296.27_fp_kind, 298.16_fp_kind, 299.07_fp_kind, 299.96_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.004_fp_kind,  0.006_fp_kind,  0.009_fp_kind,  0.015_fp_kind, &
             0.021_fp_kind,  0.049_fp_kind,  0.095_fp_kind,  0.165_fp_kind, &
             0.370_fp_kind,  0.547_fp_kind,  0.824_fp_kind,  1.205_fp_kind, &
             1.401_fp_kind,  1.901_fp_kind,  2.221_fp_kind,  2.517_fp_kind, &
             2.684_fp_kind,  5.312_fp_kind,  7.399_fp_kind,  9.139_fp_kind, &
             9.945_fp_kind, 11.234_fp_kind, 11.854_fp_kind, 12.460_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, & 
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 30 )

        CIMSS_Description = 'Fort Sherman,C.Z.,PM. Record 1093 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1967
        CIMSS_Month = 6
        CIMSS_Day   = 26
        CIMSS_Hour  = 17
        CIMSS_Latitude  =   9.33_fp_kind
        CIMSS_Longitude = 280.02_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             243.42_fp_kind, 256.26_fp_kind, &
             264.46_fp_kind, 276.06_fp_kind, 267.75_fp_kind, 261.86_fp_kind, &
             262.06_fp_kind, 257.72_fp_kind, 254.36_fp_kind, 249.46_fp_kind, &
             242.56_fp_kind, 232.67_fp_kind, 225.66_fp_kind, 221.53_fp_kind, &
             218.16_fp_kind, 214.16_fp_kind, 208.74_fp_kind, 204.16_fp_kind, &
             201.98_fp_kind, 200.16_fp_kind, 202.92_fp_kind, 206.08_fp_kind, &
             208.16_fp_kind, 219.16_fp_kind, 230.16_fp_kind, 240.16_fp_kind, &
             248.20_fp_kind, 255.16_fp_kind, 258.72_fp_kind, 263.63_fp_kind, &
             266.16_fp_kind, 272.39_fp_kind, 276.39_fp_kind, 280.08_fp_kind, &
             282.16_fp_kind, 287.18_fp_kind, 291.16_fp_kind, 294.57_fp_kind, &
             295.95_fp_kind, 298.16_fp_kind, 299.22_fp_kind, 300.26_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.007_fp_kind,  0.017_fp_kind,  0.033_fp_kind,  0.057_fp_kind, &
             0.779_fp_kind,  1.404_fp_kind,  1.925_fp_kind,  2.643_fp_kind, &
             3.013_fp_kind,  5.575_fp_kind,  7.219_fp_kind,  8.736_fp_kind, &
             9.592_fp_kind, 12.161_fp_kind, 14.201_fp_kind, 16.211_fp_kind, &
            17.102_fp_kind, 18.526_fp_kind, 19.212_fp_kind, 19.881_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 31 )

        CIMSS_Description = 'Fort Sherman,C.Z.,PM. Record 1169 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1967
        CIMSS_Month = 6
        CIMSS_Day   = 14
        CIMSS_Hour  = 12
        CIMSS_Latitude  =   9.33_fp_kind
        CIMSS_Longitude = 280.02_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             238.01_fp_kind, 248.63_fp_kind, &
             261.46_fp_kind, 270.06_fp_kind, 268.19_fp_kind, 266.86_fp_kind, &
             262.06_fp_kind, 258.85_fp_kind, 256.36_fp_kind, 246.46_fp_kind, &
             239.56_fp_kind, 234.35_fp_kind, 230.66_fp_kind, 226.53_fp_kind, &
             223.16_fp_kind, 213.16_fp_kind, 209.91_fp_kind, 207.16_fp_kind, &
             199.54_fp_kind, 193.16_fp_kind, 196.95_fp_kind, 201.30_fp_kind, &
             204.16_fp_kind, 219.16_fp_kind, 231.16_fp_kind, 241.16_fp_kind, &
             249.73_fp_kind, 257.16_fp_kind, 260.08_fp_kind, 264.09_fp_kind, &
             266.16_fp_kind, 272.78_fp_kind, 277.03_fp_kind, 280.95_fp_kind, &
             283.16_fp_kind, 287.62_fp_kind, 291.16_fp_kind, 294.08_fp_kind, &
             295.27_fp_kind, 297.16_fp_kind, 298.07_fp_kind, 298.96_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.005_fp_kind, &
             0.009_fp_kind,  0.003_fp_kind,  0.005_fp_kind,  0.009_fp_kind, &
             0.013_fp_kind,  0.074_fp_kind,  0.219_fp_kind,  0.379_fp_kind, &
             1.307_fp_kind,  2.426_fp_kind,  2.951_fp_kind,  3.674_fp_kind, &
             4.047_fp_kind,  5.404_fp_kind,  6.274_fp_kind,  7.077_fp_kind, &
             7.531_fp_kind, 10.996_fp_kind, 13.748_fp_kind, 15.232_fp_kind, &
            15.872_fp_kind, 16.894_fp_kind, 17.386_fp_kind, 17.867_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE( 32 )

        CIMSS_Description = 'Fort Sherman,C.Z.,PM. Record 1173 from set of 1200.'
        CIMSS_Climatology_Model = 1
        CIMSS_Year  = 1967
        CIMSS_Month = 8
        CIMSS_Day   = 9
        CIMSS_Hour  = 12
        CIMSS_Latitude  =   9.33_fp_kind
        CIMSS_Longitude = 280.02_fp_kind
        CIMSS_Surface_Altitude = ZERO

        CIMSS_Level_Temperature = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             237.55_fp_kind, 248.56_fp_kind, &
             260.76_fp_kind, 267.16_fp_kind, 263.47_fp_kind, 260.86_fp_kind, &
             251.06_fp_kind, 248.41_fp_kind, 246.36_fp_kind, 239.46_fp_kind, &
             230.56_fp_kind, 227.11_fp_kind, 224.66_fp_kind, 219.98_fp_kind, &
             216.16_fp_kind, 208.16_fp_kind, 204.91_fp_kind, 202.16_fp_kind, &
             199.44_fp_kind, 197.16_fp_kind, 199.57_fp_kind, 202.34_fp_kind, &
             204.16_fp_kind, 219.16_fp_kind, 232.16_fp_kind, 243.16_fp_kind, &
             251.20_fp_kind, 258.16_fp_kind, 261.40_fp_kind, 265.86_fp_kind, &
             268.16_fp_kind, 273.61_fp_kind, 277.11_fp_kind, 280.34_fp_kind, &
             282.16_fp_kind, 286.06_fp_kind, 289.16_fp_kind, 292.08_fp_kind, &
             293.27_fp_kind, 295.16_fp_kind, 296.07_fp_kind, 296.96_fp_kind /)
        CIMSS_Level_Absorber( :, 1 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind,  0.003_fp_kind, &
             0.003_fp_kind,  0.003_fp_kind,  0.004_fp_kind,  0.006_fp_kind, &
             0.011_fp_kind,  0.006_fp_kind,  0.008_fp_kind,  0.011_fp_kind, &
             0.013_fp_kind,  0.074_fp_kind,  0.285_fp_kind,  0.492_fp_kind, &
             1.340_fp_kind,  2.074_fp_kind,  2.761_fp_kind,  3.706_fp_kind, &
             4.193_fp_kind,  5.938_fp_kind,  7.058_fp_kind,  8.091_fp_kind, &
             8.674_fp_kind, 10.887_fp_kind, 12.644_fp_kind, 14.337_fp_kind, &
            15.087_fp_kind, 16.285_fp_kind, 16.862_fp_kind, 17.425_fp_kind /)
        CIMSS_Level_Absorber( :, 2 ) = &
          (/ ZERO,           ZERO,           ZERO,           ZERO, &
             0.55927_fp_kind,0.98223_fp_kind, &
             1.94681_fp_kind,3.13498_fp_kind,4.30596_fp_kind,5.48908_fp_kind, &
             7.41904_fp_kind,8.55498_fp_kind,9.22089_fp_kind,9.76594_fp_kind, &
             9.60463_fp_kind,8.45820_fp_kind,6.99682_fp_kind,5.57585_fp_kind, &
             4.30000_fp_kind,1.69985_fp_kind,1.23555_fp_kind,0.81780_fp_kind, &
             0.39171_fp_kind,0.20944_fp_kind,0.14056_fp_kind,0.12283_fp_kind, &
             0.10984_fp_kind,0.08405_fp_kind,0.06529_fp_kind,0.05392_fp_kind, &
             0.04764_fp_kind,0.04366_fp_kind,0.04232_fp_kind,0.04052_fp_kind, &
             0.03961_fp_kind,0.03735_fp_kind,0.03595_fp_kind,0.03534_fp_kind, &
             0.03514_fp_kind,0.03385_fp_kind,0.03252_fp_kind,0.03107_fp_kind, &
             0.03027_fp_kind,0.02901_fp_kind,0.02840_fp_kind,0.02781_fp_kind /)


      CASE DEFAULT

        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'CASE DEFAULT branch taken. '//&
                                'Invalid PROFILE value not caught on input checking', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN

    END SELECT profile_select



    !#--------------------------------------------------------------------------#
    !# -- SPLICE ON THE UPPER ATMOSPHERE TEMPERATURE FROM THE MODEL PROFILES -- #
    !#--------------------------------------------------------------------------#

    dT = CIMSS_Level_Temperature( N_MODEL_LEVELS ) - MODEL_Level_Temperature( N_MODEL_LEVELS, CIMSS_Climatology_Model )

    IF ( ABS( dT ) > TWO ) THEN
      CIMSS_Level_Temperature( 1:N_MODEL_LEVELS-1 ) = MODEL_Level_Temperature( 1:N_MODEL_LEVELS-1, CIMSS_Climatology_Model ) + &
                                                      ( SCALE_FACTOR * (/ ( REAL( k, fp_kind ), k = 1,N_MODEL_LEVELS-1 ) /) )
    ELSE
      CIMSS_Level_Temperature( 1:N_MODEL_LEVELS-1 ) = MODEL_Level_Temperature( 1:N_MODEL_LEVELS-1, CIMSS_Climatology_Model )
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- SIMPLY ADD ON THE ABSORBER AMOUNTS --                #
    !#--------------------------------------------------------------------------#

    DO j = 1, N_CIMSS_ABSORBERS
      CIMSS_Level_Absorber( 1:N_MODEL_LEVELS-1, j ) = MODEL_Level_Absorber( 1:N_MODEL_LEVELS-1, j, CIMSS_Climatology_Model )
    END DO



    !#--------------------------------------------------------------------------#
    !#             -- LOAD DATA INTO RETURN ARRAYS IN REVERSE ORDER --          #
    !#--------------------------------------------------------------------------#

    DO k = 1, N_CIMSS_LEVELS
      Level_Pressure( k )    = CIMSS_Level_Pressure(    N_CIMSS_LEVELS - k + 1 )
      Level_Temperature( k ) = CIMSS_Level_Temperature( N_CIMSS_LEVELS - k + 1 )
    END DO

    DO j = 1, N_CIMSS_ABSORBERS
      DO k = 1, N_CIMSS_LEVELS
        Level_Absorber( k, j ) = CIMSS_Level_Absorber( N_CIMSS_LEVELS - k + 1, j )
      END DO
    END DO



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN THE OPTIONAL OUTPUT ARGUMENTS --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Absorber_ID ) ) THEN
      Absorber_ID = CIMSS_Absorber_ID
    END IF

    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      Absorber_Units_ID = CIMSS_Absorber_Units_ID
    END IF

    IF ( PRESENT( Description ) ) THEN
      Description = ' '
      Description = TRIM( CIMSS_Description )
    END IF

    IF ( PRESENT( Climatology_Model ) ) THEN
      Climatology_Model = CIMSS_Climatology_Model
    END IF

    IF ( PRESENT( Year ) ) THEN
      Year = CIMSS_Year
    END IF

    IF ( PRESENT( Month ) ) THEN
      Month = CIMSS_Month
    END IF

    IF ( PRESENT( Day ) ) THEN
      Day = CIMSS_Day
    END IF

    IF ( PRESENT( Hour ) ) THEN
      Hour = CIMSS_Hour
    END IF

    IF ( PRESENT( Latitude ) ) THEN
      Latitude = CIMSS_Latitude
    END IF

    IF ( PRESENT( Longitude ) ) THEN
      Longitude = CIMSS_Longitude
    END IF

    IF ( PRESENT( Surface_Altitude ) ) THEN
      Surface_Altitude = CIMSS_Surface_Altitude
    END IF

  END FUNCTION Load_CIMSS_Profile

END MODULE CIMSS_Profile_Set


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CIMSS_Profile_Set.f90,v $
! Revision 2.3  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.2  2005/01/04 22:05:05  paulv
! - Cosmetic changes only.
!
! Revision 2.1  2002/07/22 17:08:58  paulv
! - Changed interface to use pointer arrays.
!
! Revision 1.4  2002/07/18 18:33:19  paulv
! - Correced bug in temperature splicing code.
!
! Revision 1.3  2002/07/15 21:14:24  paulv
! - Altered interface so that data is passed out in the argument list rather than
!   via module shared data.
!
! Revision 1.2  2002/07/15 02:28:14  paulv
! - Completed profile load.
! - Added upper atmosphere splicing from Hal's code.
!
! Revision 1.1  2002/07/12 21:37:05  paulv
! Initial checkin. Incomplete.
!
!
!
