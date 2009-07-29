! Test_SRF
!
! Program to test the oSRF modules
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_oSRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Unit_Test
  USE oSRF_Define, ONLY: oSRF_type, &
                         Allocated_oSRF, &
                         Create_oSRF   , &
                         Destroy_oSRF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_oSRF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Single band number of points
  INTEGER, PARAMETER :: N1_POINTS(1) = (/ 20 /)
  ! Multi-band number of points
  INTEGER, PARAMETER :: N2_POINTS(4) = (/10, 12, 15, 11/)
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_status
  TYPE(oSRF_type) :: osrf1, osrf2


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the oSRF modules.', &
                        '$Revision$' )

    
  ! Test initial status
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  WRITE( *,'( 7x,"Single band creation...")' )
  err_status = Create_oSRF(N1_POINTS, osrf1)
  CALL Display_Message('Create_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 7x,"Multiple band creation...")' )
  err_status = Create_oSRF(N2_POINTS, osrf2)
  CALL Display_Message('Create_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_oSRF(osrf1)
  CALL Display_Message('Destroy_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_oSRF(osrf2)
  CALL Display_Message('Destroy_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))

END PROGRAM Test_oSRF
