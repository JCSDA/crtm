!-------------------------------------------------------
!
! Description:
!       Simple test program to inspect the CRTM Coeff files.
!
!       Date: 2018-08-14        Author: P. Stegmann

! MODIFICATION HISTORY:
! =====================
!
! Author:           Date:          Description:
! =======           =====          ============
! Patrick Stegmann  2021-02-05     Refactored as a CRTM
!                                  unit test.
! Cheng Dang        2021-07-28     Modified for Aerosol
!                                  Coeff look-up table
! Cheng Dang        2022-03-14     Modified for EmisCoeff
!                                  look-up table (VIS,IR)
!-------------------------------------------------------

PROGRAM test_emis_coeff_io

  ! ====================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !

  ! Module usage
  USE UnitTest_Define, ONLY: UnitTest_type,   &
                             UnitTest_Init,   &
                             UnitTest_Setup,  &
                             UnitTest_Assert, &
                             UnitTest_Passed
  ! ...Infrared surface emissivities
  USE CRTM_IRwaterCoeff      , ONLY: CRTM_IRwaterCoeff_Load
  USE CRTM_IRlandCoeff       , ONLY: CRTM_IRlandCoeff_Load
  USE CRTM_IRsnowCoeff       , ONLY: CRTM_IRsnowCoeff_Load
  USE CRTM_IRiceCoeff        , ONLY: CRTM_IRiceCoeff_Load
  ! ...Visible surface emissivities
  USE CRTM_VISwaterCoeff     , ONLY: CRTM_VISwaterCoeff_Load
  USE CRTM_VISlandCoeff      , ONLY: CRTM_VISlandCoeff_Load
  USE CRTM_VISsnowCoeff      , ONLY: CRTM_VISsnowCoeff_Load
  USE CRTM_VISiceCoeff       , ONLY: CRTM_VISiceCoeff_Load
  USE Message_Handler        , ONLY: SUCCESS, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Data dictionary:
  CHARACTER(2000)         :: info
  CHARACTER(*), PARAMETER :: Default_IRwaterCoeff_File   = 'Nalli.IRwater.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_IRlandCoeff_File    = 'NPOESS.IRland.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_IRsnowCoeff_File    = 'NPOESS.IRsnow.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_IRiceCoeff_File     = 'NPOESS.IRice.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_VISwaterCoeff_File  = 'NPOESS.VISwater.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_VISlandCoeff_File   = 'NPOESS.VISland.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_VISsnowCoeff_File   = 'NPOESS.VISsnow.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Default_VISiceCoeff_File    = 'NPOESS.VISice.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Optional_IRwaterCoeff_File  = 'Nalli2.IRwater.EmisCoeff.bin'
  CHARACTER(*), PARAMETER :: Optional_IRsnowCoeff_File   = 'Nalli.IRsnow.EmisCoeff.bin'
  LOGICAL,      PARAMETER :: netCDF = .FALSE.
  CHARACTER(*), PARAMETER :: File_Path = './testinput/'
  LOGICAL,      PARAMETER :: Quiet = .TRUE.
  INTEGER                 :: err_stat
  TYPE(UnitTest_type)     :: ioTest
  LOGICAL                 :: testPassed
  CHARACTER(*), PARAMETER :: Program_Name = 'Test_Emi_Coeff_io'

  ! Initialize Unit test:
  CALL UnitTest_Init(ioTest, .TRUE.)
  CALL UnitTest_Setup(ioTest, 'Emi_Coeff_IO_Test', Program_Name, .TRUE.)

  ! Greeting:
  WRITE(*,*) 'HELLO, THIS IS A TEST CODE TO INSPECT EmisCoeff files in binary format.'
  WRITE(*,*) 'test_emi_coeff_io'
  WRITE(*,*) 'The following default EmisCoeff files are investigated: '

  ! Load the default emissivity coefficient look-up table:
  WRITE(*,*) '...loading: ', Default_IRlandCoeff_File
  err_stat = 3
  err_stat = CRTM_IRlandCoeff_Load( &
               Default_IRlandCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRlandCoeff_Load' ,'Error loading IRlandCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_IRwaterCoeff_File
  err_stat = 3
  err_stat = CRTM_IRwaterCoeff_Load( &
               Default_IRwaterCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRwaterCoeff_Load' ,'Error loading IRwaterCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_IRsnowCoeff_File
  err_stat = 3
  err_stat = CRTM_IRsnowCoeff_Load( &
               Default_IRsnowCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRsnowCoeff_Load' ,'Error loading IRsnowCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_IRiceCoeff_File
  err_stat = 3
  err_stat = CRTM_IRiceCoeff_Load( &
               Default_IRiceCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRiceCoeff_Load' ,'Error loading IRiceCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_VISlandCoeff_File
  err_stat = 3
  err_stat = CRTM_VISlandCoeff_Load( &
               Default_IRiceCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_VISlandCoeff_Load' ,'Error loading VISlandCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_VISwaterCoeff_File
  err_stat = 3
  err_stat = CRTM_VISwaterCoeff_Load( &
               Default_VISwaterCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_VISwaterCoeff_Load' ,'Error loading VISwaterCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_VISsnowCoeff_File
  err_stat = 3
  err_stat = CRTM_VISsnowCoeff_Load( &
               Default_VISsnowCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_VISsnowCoeff_Load' ,'Error loading VISsnowCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Default_VISiceCoeff_File
  err_stat = 3
  err_stat = CRTM_VISiceCoeff_Load( &
               Default_VISiceCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_VISiceCoeff_Load' ,'Error loading VISiceCoeff data', err_stat )
    STOP 1
  END IF

  ! Greeting:
  WRITE(*,*) 'The following optional EmisCoeff files are investigated: '

  ! Load the optional emissivity coefficient look-up table:
  WRITE(*,*) '...loading: ', Optional_IRwaterCoeff_File
  err_stat = 3
  err_stat = CRTM_IRwaterCoeff_Load( &
               Optional_IRwaterCoeff_File, &
               netCDF     = netCDF, &
               Quiet      = Quiet, &
               File_Path  = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRwaterCoeff_Load' ,'Error loading IRwaterCoeff data', err_stat )
    STOP 1
  END IF

  WRITE(*,*) '...loading: ', Optional_IRsnowCoeff_File
  err_stat = 3
  err_stat = CRTM_IRsnowCoeff_Load( &
               Optional_IRsnowCoeff_File, &
               netCDF       = netCDF, &
               isSEcategory = .FALSE., &
               Quiet        = Quiet, &
               File_Path    = File_Path)
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_IRsnowCoeff_Load' ,'Error loading IRsnowCoeff data', err_stat )
    STOP 1
  END IF
  STOP 0


END PROGRAM test_emis_coeff_io
