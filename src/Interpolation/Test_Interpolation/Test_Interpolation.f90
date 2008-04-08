!
! Program to test the CRTM_Interpolation module subprograms
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Feb-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_Interpolation

  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Test_Interpolation_Index
  USE Test_Interpolation_LPoly
  USE Test_Interpolation_Functions
  USE Unit_Test
  USE CRTM_Interpolation
  ! Disable implicit typing
  IMPLICIT NONE
  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: $'  
  ! Variables
  TYPE(UTest_type) :: UTest


  ! Initialisation
  ! --------------
  CALL Init_AllTests(UTest)
  WRITE(*,'(5x,"==================================")')
  WRITE(*,'(5x,"Testing ",i0,"-pt interpolation...")') NPTS
  WRITE(*,'(5x,"==================================",/)')
  
  ! Index finding tests
  ! -------------------
  CALL Test_Index_Regular(UTest)
  CALL Test_Index_Random(UTest)

  ! Polynomial tests
  ! ----------------
  CALL Test_LPoly(UTest)
  CALL Test_LPoly_TLAD(UTest)
  
  ! Interpolation tests
  ! -------------------
  CALL Test_Hingepoint_Interpolation(UTest)
  CALL Test_Actual_Interpolation(UTest)
  CALL Test_TL_Interpolation(UTest)
  CALL Test_AD_Interpolation(UTest)
  CALL Test_FWDTL_Interpolation(UTest)
  CALL Test_TLAD_Interpolation(UTest)

  ! Test summary
  ! ------------
  CALL Report_AllTests(UTest)
  
END PROGRAM Test_Interpolation
