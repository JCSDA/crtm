!-------------------------------------------------------
!
! test_taucoeff_io_nc.f90
!
!
! Description:
! ============
!
!	Simple test program to inspect the CRTM TauCoeff 
!	files using the netCDF interface.
!	
!	Date: 2018-08-14	Author: P. Stegmann
!
!
! MODIFICATION HISTORY:
! =====================
! 
! Author:           Date:          Description:
! =======           =====          ============
! Patrick Stegmann  2021-02-05     Refactored as a CRTM
!                                  unit test.
! Patrick Stegmann  2021-02-10     Switched from binary
!                                  to netCDF I/O.
! Patrick Stegmann  2021-07-26     Adapted Spc case to 
!                                  TauCoeff nc4 I/O.
!
!-------------------------------------------------------

PROGRAM test_taucoeff_io_nc
  
  ! ====================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !

  ! Module usage
  USE UnitTest_Define, ONLY: UnitTest_type,   &
                             UnitTest_Init,   &
                             UnitTest_Setup,  &
                             UnitTest_Assert, &
                             UnitTest_Passed
  USE TauCoeff_Define, ONLY: TauCoeff_type
  USE CRTM_TauCoeff,   ONLY: CRTM_Load_TauCoeff
  USE Message_Handler, ONLY: SUCCESS, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Data dictionary:
  TYPE(TauCoeff_type)                    :: sat_dat
  CHARACTER(2000)                        :: info
  CHARACTER(*), DIMENSION(1), PARAMETER :: Sensor_ID = 'amsua_aqua'
  CHARACTER(*),               PARAMETER :: File_Path = './testinput/'
  INTEGER,                    PARAMETER :: Quiet = 1
  INTEGER                                :: err_stat
  TYPE(UnitTest_type) :: ioTest
  LOGICAL :: testPassed
  CHARACTER(*), PARAMETER :: Program_Name = 'Test_TauCoeff_IO_NC'

  ! Initialize Unit test:
  CALL UnitTest_Init(ioTest, .TRUE.)
  CALL UnitTest_Setup(ioTest, 'TauCoeff_IO_NC_Test', Program_Name, .TRUE.)

  ! Greeting:
  WRITE(*,*) 'HELLO, THIS IS A TEST CODE TO INSPECT TauCoeff files.' 
  WRITE(*,*) ' test_taucoeff_io_nc ', 'The following instrument is investigated: ', Sensor_ID
  ! Load the transmittance model coefficients
  err_stat = 3
  err_stat = CRTM_Load_TauCoeff( &
                Sensor_ID         = Sensor_ID        , &
                File_Path         = File_Path        , &
                netCDF            = .TRUE.           , &
                Quiet             = Quiet          )
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)

  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_Load_TauCoeff' ,'Error loading TauCoeff data', err_stat )
    STOP 1
  END IF
  STOP 0

END PROGRAM test_taucoeff_io_nc
