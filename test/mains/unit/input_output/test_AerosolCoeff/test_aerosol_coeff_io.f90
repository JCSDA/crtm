!-------------------------------------------------------
!
! Description:
!	Simple test program to inspect the CRTM AerosolCoeff 
!	files.
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
! Cheng Dang        2021-07-28     Modified for Aerosol 
!                                  Coeff look-up table
!-------------------------------------------------------

PROGRAM test_aerosol_coeff_io
  
  ! ====================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !

  ! Module usage
  USE UnitTest_Define, ONLY: UnitTest_type,   &
                             UnitTest_Init,   &
                             UnitTest_Setup,  &
                             UnitTest_Assert, &
                             UnitTest_Passed
  !USE AerosolCoeff_Define, ONLY: AerosolCoeff_type
  USE CRTM_AerosolCoeff
  USE Message_Handler, ONLY: SUCCESS, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Data dictionary:
  !TYPE(AerosolCoeff_type) :: aero_coeff
  CHARACTER(2000)         :: info
  CHARACTER(*), PARAMETER :: Aerosol_Model = 'CRTM'
  CHARACTER(*), PARAMETER :: AerosolCoeff_File = 'AerosolCoeff.bin'
  CHARACTER(*), PARAMETER :: File_Path = './testinput/'
  LOGICAL,      PARAMETER :: netCDF = .FALSE.
  LOGICAL,      PARAMETER :: Quiet = .TRUE.
  INTEGER                 :: err_stat
  TYPE(UnitTest_type)     :: ioTest
  LOGICAL                 :: testPassed
  CHARACTER(*), PARAMETER :: Program_Name = 'Test_Aerosol_Coeff_IO'

  ! Initialize Unit test:
  CALL UnitTest_Init(ioTest, .TRUE.)
  CALL UnitTest_Setup(ioTest, 'Aerosol_Coeff_IO_Test', Program_Name, .TRUE.)

  ! Greeting:
  WRITE(*,*) 'HELLO, THIS IS A TEST CODE TO INSPECT AerosolCoeff files.' 
  WRITE(*,*) 'test_aerosol_coeff_io', 'The following aerosol scheme is investigated: ', Aerosol_Model
  ! Load the aerosol coefficient look-up table:
  err_stat = 3
  err_stat = CRTM_AerosolCoeff_Load( &
                Aerosol_Model              , &
                AerosolCoeff_File          , &
                File_Path                  , &
                netCDF            = netCDF , &
                Quiet             = Quiet    ) 
  CALL UnitTest_Assert(ioTest, (err_stat==SUCCESS) )
  testPassed = UnitTest_Passed(ioTest)
  
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( 'CRTM_Load_Aerosol_Coeff' ,'Error loading AerosolCoeff data', err_stat )
    STOP 1
  END IF
  STOP 0

END PROGRAM test_aerosol_coeff_io
