!-------------------------------------------------------
!
! Description:
!	Simple test program to inspect the CRTM SpcCoeff 
!	files using the netCDF interface.
!	
!	Date: 2018-08-14	Author: P. Stegmann
!
! MODIFICATION HISTORY:
! =====================
! 
! Author:           Date:          Description:
! =======           =====          ============
! Patrick Stegmann  2021-02-05     Refactored as a CRTM
!                                  unit test.
! Patrick Stegmann  2021-02-10     Switched from binary
!                                  to netCDF I/O
!
!-------------------------------------------------------

PROGRAM test_spc_io
  
  ! ====================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !

  ! Module usage
  USE UnitTest_Define, ONLY: UnitTest_type,   &
                             UnitTest_Init,   &
                             UnitTest_Setup,  &
                             UnitTest_Assert, &
                             UnitTest_Passed
  USE SpcCoeff_Define, ONLY: SpcCoeff_type
  USE CRTM_SpcCoeff
  USE Message_Handler, ONLY: SUCCESS, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Data dictionary:
  TYPE(SpcCoeff_type)                    :: sat_dat
  CHARACTER(2000)                        :: info
  CHARACTER(*), DIMENSION(1), PARAMETER :: Sensor_ID = 'amsua_aqua'
  CHARACTER(*),               PARAMETER :: File_Path = './testinput/'
  LOGICAL,                    PARAMETER :: Quiet = .TRUE.
  INTEGER                                :: err_stat
  TYPE(UnitTest_type) :: ioTest
  LOGICAL :: testPassed
  CHARACTER(*), PARAMETER :: Program_Name = 'Test_Spc_IO'

  ! Initialize Unit test:
  CALL UnitTest_Init(ioTest, .TRUE.)
  CALL UnitTest_Setup(ioTest, 'Spc_IO_Test', Program_Name, .TRUE.)

  ! Greeting:
  WRITE(*,*) 'HELLO, THIS IS A TEST CODE TO INSPECT SpcCoeff files.' 
  WRITE(*,*) 'test_spc_io', 'The following instrument is investigated: ', Sensor_ID
  ! Load the transmittance model coefficients
  err_stat = 3
  err_stat = CRTM_SpcCoeff_Load( &
                Sensor_ID         = Sensor_ID        , &
                File_Path         = File_Path        , &
                netCDF            = .TRUE.           , &
                Quiet             = Quiet          )
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)

  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_Load_SpcCoeff' ,'Error loading SpcCoeff data', err_stat )
    STOP 1
  END IF
  STOP 0

END PROGRAM test_spc_io
