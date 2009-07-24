! Test_PtrArr
!
! Program to test the PtrArr modules
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Jul-2009
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_PtrArr

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Unit_Test
  USE PtrArr_Define
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_PtrArr'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: $'

  INTEGER, PARAMETER :: N = 4
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: err_status
  TYPE(PtrArr_type) :: p


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the PtrArr modules.', &
                        '$Revision: $' )

  ! Specify what type of derived type
  WRITE( *,'(/5x,"PtrArr type component: ",a)' ) PTRARR_TYPE_COMPONENT
  
  
  ! Test initial status
#if defined ALLOC
  WRITE( *,'( 5x,"PtrArr allocation status: ",l5)' ) Allocated_PtrArr(p)
#else
  WRITE( *,'( 5x,"PtrArr association status: ",l5)' ) Associated_PtrArr(p)
#endif


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  err_status = Create_PtrArr(N, p)
  CALL Display_Message('Create_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
#if defined ALLOC
  WRITE( *,'( 5x,"PtrArr allocation status: ",l5)' ) Allocated_PtrArr(p)
#else
  WRITE( *,'( 5x,"PtrArr association status: ",l5)' ) Associated_PtrArr(p)
#endif


  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_PtrArr(p)
  CALL Display_Message('Destroy_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
#if defined ALLOC
  WRITE( *,'( 5x,"PtrArr allocation status: ",l5)' ) Allocated_PtrArr(p)
#else
  WRITE( *,'( 5x,"PtrArr association status: ",l5)' ) Associated_PtrArr(p)
#endif

END PROGRAM Test_PtrArr
