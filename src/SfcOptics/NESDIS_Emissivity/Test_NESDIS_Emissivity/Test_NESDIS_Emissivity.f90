!
! Test_NESDIS_Emissivity
!
! Program to test the CRTM NESDIS Emissivity modules
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Feb-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_NESDIS_Emissivity

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds              , ONLY: fp
  USE Message_Handler         , ONLY: Program_Message
  USE Compare_Float_Numbers   , ONLY: Compare_Float
                                       
  USE NESDIS_MHS_SICEEM_Module, ONLY: NESDIS_ICEEM_MHS
  USE NESDIS_MHS_SNOWEM_Module, ONLY: NESDIS_SNOWEM_MHS


  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_NESDIS_Emissivity'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'


  ! ---------
  ! Variables
  ! ---------
  REAL(fp) :: z_angle, u_angle
  REAL(fp) :: tbb(2), ts
  REAL(fp) :: f
  REAL(fp) :: e_expected
  REAL(fp) :: eh_expected, eh_actual
  REAL(fp) :: ev_expected, ev_actual

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM NESDIS Emissivity modules.', &
                        '$Revision$' )



  ! CASE 1
  ! ------
  WRITE( *,'(/5x,"CASE 1")' )
  z_angle = 40.34_fp
  u_angle = 40.34_fp
  tbb = (/ 212.144_fp, 235.451_fp /)
  ts  = 271.093_fp
  
  ! 89 GHz channel
  f = 89.0_fp
  ! MHS Sea Ice model
  e_expected = 0.72151_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.72053_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Snow', f, eh_expected, eh_actual, ev_expected, ev_actual )

  ! 156 GHz channel
  f = 156.0_fp
  ! MHS Sea Ice model
  e_expected = 0.77127_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.77623_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  
  
  ! CASE 2
  ! ------
  WRITE( *,'(/5x,"CASE 2")' )
  z_angle = 40.34_fp
  u_angle = 40.34_fp
  tbb = (/ 212.144_fp, 235.451_fp /)
  ts  = -999.0_fp
  
  ! 89 GHz channel
  f = 89.0_fp
  ! MHS Sea Ice model
  e_expected = 0.74155_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.76750_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Snow', f, eh_expected, eh_actual, ev_expected, ev_actual )

  ! 156 GHz channel
  f = 156.0_fp
  ! MHS Sea Ice model
  e_expected = 0.80198_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.84096_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  
  
  ! CASE 3
  ! ------
  WRITE( *,'(/5x,"CASE 3")' )
  z_angle = 40.34_fp
  u_angle = 40.34_fp
  tbb = (/ -999.0_fp, -999.0_fp /)
  ts  = -999.0_fp
  
  ! 89 GHz channel
  f = 89.0_fp
  ! MHS Sea Ice model
  e_expected = 0.85_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.85_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Snow', f, eh_expected, eh_actual, ev_expected, ev_actual )

  ! 156 GHz channel
  f = 156.0_fp
  ! MHS Sea Ice model
  e_expected = 0.85_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_ICEEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  ! MHS Snow model
  e_expected = 0.85_fp
  eh_expected = e_expected
  ev_expected = e_expected
  CALL NESDIS_SNOWEM_MHS( z_angle, u_angle, f, ts, tbb, eh_actual, ev_actual )
  CALL Output_Compare( 'MHS Sea Ice', f, eh_expected, eh_actual, ev_expected, ev_actual )
  
CONTAINS

  SUBROUTINE Output_Compare( model_type, f, eh_expected, eh_actual, ev_expected, ev_actual )
    CHARACTER(*), INTENT(IN) :: model_type
    REAL(fp),     INTENT(IN) :: f
    REAL(fp),     INTENT(IN) :: eh_expected, eh_actual
    REAL(fp),     INTENT(IN) :: ev_expected, ev_actual
    INTEGER, PARAMETER :: ULP = 1000000
    WRITE( *,'(7x,a,", f=",f6.2,"GHz")' ) TRIM(model_type), f
    WRITE( *,'(9x,"EH: ",3(1x,es15.8),5x,l1)' ) &
             eh_expected, eh_actual, eh_expected-eh_actual, &
             Compare_Float( eh_expected, eh_actual, ULP=ULP )
    WRITE( *,'(9x,"EH: ",3(1x,es15.8),5x,l1)' ) &
             ev_expected, ev_actual, ev_expected-ev_actual, &
             Compare_Float( ev_expected, ev_actual, ULP=ULP )
  END SUBROUTINE Output_Compare
END PROGRAM Test_NESDIS_Emissivity
