!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_MWSE
!
! PURPOSE:
!       Program to test the microwave surface emissivity routines for
!       benchmarking and refactoring.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       kinds:     Module containing definitions for kinds
!                  of variable types.
!
! CONTAINS:
!       None.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

PROGRAM Test_MWSE


  ! ------------
  ! Module usage
  ! ------------

  USE kinds

  USE Message_Handler

  USE EmisTestData_Define
  USE EmisTestData_Binary_IO

  USE MWSE_Snow


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_MWSE'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Test data files
  INTEGER, PARAMETER :: N_FILES = 8
  CHARACTER( * ), PARAMETER, DIMENSION( N_FILES ) :: &
    FILENAME = (/ 'emin_amsua15.dat.DirectBinary', &
                  'emin_amsua16.dat.DirectBinary', &
                  'emin_amsub15.dat.DirectBinary', &
                  'emin_amsub16.dat.DirectBinary', &
                  'emin_amsub17.dat.DirectBinary', &
                  'emin_ssmi13.dat.DirectBinary ', &
                  'emin_ssmi14.dat.DirectBinary ', &
                  'emin_ssmi15.dat.DirectBinary ' /)

  INTEGER, PARAMETER, DIMENSION( N_FILES ) :: &
    N_CHANNELS = (/ 15, 15, 5, 5, 5, 7, 7, 7 /)

  INTEGER, PARAMETER, DIMENSION( N_FILES ) :: &
    N_RECORDS = (/ 1395, 1035, 170, 130, 165, 413, 406, 378 /)

  LOGICAL,PARAMETER, DIMENSION( N_FILES ) :: &
    IS_AMSUA = (/ .TRUE., .TRUE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .FALSE. /) 

  LOGICAL,PARAMETER, DIMENSION( N_FILES ) :: &
    IS_AMSUB = (/ .FALSE., .FALSE., .TRUE., .TRUE., .TRUE., .FALSE., .FALSE., .FALSE. /) 

  LOGICAL,PARAMETER, DIMENSION( N_FILES ) :: &
    IS_SSMI = (/ .FALSE., .FALSE., .FALSE., .FALSE., .FALSE., .TRUE., .TRUE., .TRUE. /) 


  ! -- The channel indices for the observations
  INTEGER, PARAMETER :: N_AMSUA_CHANNELS = 4
  INTEGER, PARAMETER, DIMENSION( N_AMSUA_CHANNELS ) :: AMSU_A_TB_INDEX = (/ 1, 2, 3, 15 /)

  INTEGER, PARAMETER :: N_AMSUB_CHANNELS = 2
  INTEGER, PARAMETER, DIMENSION( N_AMSUB_CHANNELS ) :: AMSU_B_TB_INDEX = (/ 1, 2 /)

  ! -- Invalid brightness temperature
  REAL( r_kind ), PARAMETER :: TB_INVALID = -999.0_r_kind

  ! -- Frequency limits
  REAL( r_kind ), PARAMETER :: FREQUENCY_BEGIN =   4.0_r_kind
  REAL( r_kind ), PARAMETER :: FREQUENCY_END   = 150.0_r_kind
  INTEGER,        PARAMETER :: N_FREQUENCIES   = 100


  ! -- Data flag values
  INTEGER, PARAMETER ::  SEA = 0
  INTEGER, PARAMETER :: LAND = 1

  INTEGER, PARAMETER :: NO_SNOW_OR_ICE = 0
  INTEGER, PARAMETER ::    SNOW_OR_ICE = 1

  INTEGER, PARAMETER ::  INFRARED = 0
  INTEGER, PARAMETER :: MICROWAVE = 1

  ! -- Data threshold values
  REAL( r_kind ), PARAMETER :: MINIMUM_SNOW_DEPTH = 0.1_r_kind

  ! --Literal constants
  REAL( r_kind ), PARAMETER :: ZERO = 0.0_r_kind


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: MEssage
  INTEGER :: Error_Status
  INTEGER :: l, n

  TYPE( EmisTestData_type ) :: EmisTestData

  REAL( r_kind ), DIMENSION( N_AMSUA_CHANNELS ) :: AMSU_A_Tb
  REAL( r_kind ), DIMENSION( N_AMSUB_CHANNELS ) :: AMSU_B_Tb

  REAL( r_kind ) :: Emissivity_Horizontal
  REAL( r_kind ) :: Emissivity_Vertical
  REAL( r_kind ) :: Emissivity

  INTEGER :: n_de
  REAL( r_kind ) :: de
  REAL( r_kind ) :: Ave_de
  REAL( r_kind ) :: RMS_de

  INTEGER :: n_Cases



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the microwave surface emissivity routines" )' )
  WRITE( *, '( 5x, "   for benchmarking and refactoring." )' )
  WRITE( *, '(/5x, " $Revision: 1.6 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- INITIALISE AVERAGE AND RMS VARIABLES --                 #
  !#----------------------------------------------------------------------------#

  n_de = 0
  AVE_de = ZERO
  RMS_de = ZERO



  !#----------------------------------------------------------------------------#
  !#                     -- LOOP OVER TEST DATA FILES --                        #
  !#----------------------------------------------------------------------------#

  File_Loop: DO n = 1, N_FILES


    WRITE( *, '( /5x, "Reading file ", a )' ) FILENAME( n )


    ! ------------------------------
    ! Initialise the compute counter
    ! ------------------------------

    n_Cases = 0


    ! ----------------------
    ! Loop over file records
    ! ----------------------

    Record_Loop: DO l = 1, N_RECORDS( n )


      ! -----------------------
      ! Read a test data record
      ! -----------------------

      Error_Status = Read_EmisTestData_Binary( FILENAME( n ), &
                                               N_CHANNELS( n ), &
                                               l, &
                                               EmisTestData )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading record # ", i5, " from ", a )' ) &
                        l, TRIM( FILENAME( n ) )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! ------------------------------------
      ! Re-initialize the observation arrays
      ! ------------------------------------

      AMSU_A_Tb = TB_INVALID
      AMSU_B_Tb = TB_INVALID


      ! ---------------------
      ! Extract the AMSU data
      ! ---------------------

      IF ( IS_AMSUA( n ) ) THEN
        AMSU_A_Tb = EmisTestData%ObsTb(AMSU_A_TB_INDEX)
      ELSE IF ( IS_AMSUB( n ) ) THEN
        AMSU_B_Tb = EmisTestData%ObsTb(AMSU_B_TB_INDEX)
      END IF


      ! -----------------
      ! Call the routines
      ! -----------------

      ! -- Test if over land or sea
      Land_or_Sea: IF ( EmisTestData%LandSea_Flag == LAND ) THEN

        ! -- Test if land surface is snow covered or not
        Snow_or_BareLand: IF ( EmisTestData%IceSnow_Flag == SNOW_OR_ICE .AND. &
                               EmisTestData%Snow_Depth > MINIMUM_SNOW_DEPTH ) THEN

          ! -- We have a snow covered land surface.
          Snow: IF ( IS_AMSUA( n ) .OR. IS_AMSUB( n ) ) THEN

            ! -- Call the AMSU routine
            CALL snwem_amsu( EmisTestData%Satellite_Zenith_Angle, &
                             EmisTestData%Frequency, &
                             EmisTestData%Snow_Depth, &
                             EmisTestData%Skin_Temperature, &
                             AMSU_A_Tb, &
                             AMSU_B_Tb, &
                             Emissivity_Horizontal, &
                             Emissivity_Vertical )

            Emissivity = Emissivity_Vertical
            de = EmisTestData%Emissivity - Emissivity

            AVE_de = AVE_de + de
            RMS_de = RMS_de + de**2

            n_de = n_de + 1

            n_Cases = n_Cases + 1

          ELSE IF ( IS_SSMI( n ) ) THEN Snow


          END IF Snow

        END IF Snow_or_BareLand

      END IF Land_or_Sea

    END DO Record_Loop

    WRITE( *, '( 10x, "Number of cases: ", i4, " out of ", i4 )' ) &
              n_Cases, N_RECORDS( n )

  END DO File_Loop


  ! ----------------------------------
  ! Destroy the EmisTestData structure
  ! ----------------------------------

  Error_Status = Destroy_EmisTestData( EmisTestData )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisTestData structure.', &
                          Error_Status )
  END IF



  !#----------------------------------------------------------------------------#
  !#           -- COMPUTE THE AVERAGE AND RMS EMISSIVITY DIFFERENCE --          #
  !#----------------------------------------------------------------------------#

  AVE_de = AVE_de / REAL( n_de, r_kind )
  RMS_de = SQRT( RMS_de / REAL( n_de, r_kind ) )

  WRITE( *, '( /5x, "AVE emissivity difference = ", es13.6, &
              &/5x, "RMS emissivity difference = ", es13.6 )' ) AVE_de, RMS_de

END PROGRAM Test_MWSE


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_MWSE.f90,v $
! Revision 1.6  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.5  2005/08/05 16:49:42  paulv
! - Changes introduced to allow for eventual testing of other routines.
!
! Revision 1.4  2004/12/09 20:37:48  paulv
! - Added EmisTestData structure destruction call. g95 reported left over
!   memory. Cool.
!
! Revision 1.3  2004/12/09 20:08:19  paulv
! - Updated to use the MWSE_Snow* modules.
!
! Revision 1.2  2004/12/08 17:38:29  paulv
! - Added output to report the number of land/snow cases out of the total
!   number processed.
!
! Revision 1.1  2004/12/08 16:49:01  paulv
! Initial checkin.
!
!
!

