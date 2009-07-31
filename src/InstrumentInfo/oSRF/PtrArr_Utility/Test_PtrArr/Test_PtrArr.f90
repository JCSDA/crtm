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
  INTEGER :: err_status
  INTEGER :: i
  INTEGER :: n_Points
  REAL(fp), ALLOCATABLE :: Arr(:)
  TYPE(PtrArr_type) :: p, p_copy


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the PtrArr modules.', &
                        '$Revision: $' )


  ! Test initial status
  WRITE( *,'( 5x,"PtrArr allocation status: ",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  err_status = Create_PtrArr(p,N)
  CALL Display_Message('Create_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"PtrArr allocation status: ",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))


  ! Test set routines
  WRITE( *,'(/5x,"Testing set routines...")' )
  err_status = Set_PtrArr( p, Arr=(/(3.1415927_fp,i=1,N)/) )
  CALL Display_Message('Set_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"PtrArr allocation status: ",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))


  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting original PtrArr...")' )
  CALL Inspect_PtrArr( p )
  
  
  ! Test get routines
  WRITE( *,'(/5x,"Testing get routines...")' )
  err_status = Get_PtrArr( p, n_Points=n_Points )
  CALL Display_Message('Get_PtrArr','n_Points',err_status)
  IF ( err_status /= SUCCESS ) STOP
  ALLOCATE(Arr(n_Points))
  err_status = Get_PtrArr( p, Arr=Arr )
  CALL Display_Message('Get_PtrArr','Arr',err_status)
  IF ( err_status /= SUCCESS ) STOP
  PRINT *, 'n_Points : ', n_Points
  PRINT *, 'Arr      : ', Arr
  DEALLOCATE(Arr)
  WRITE( *,'( 5x,"PtrArr allocation status:",2(1x,l5))' ) Allocated_PtrArr((/p,p_copy/))
  
  
  ! Test assignment
  WRITE( *,'(/5x,"Testing assignment routines...")' )
  DO i = 1, 5
    err_status = Assign_PtrArr(p, p_copy)
    CALL Display_Message('Assign_PtrArr','',err_status)
  END DO
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"PtrArr allocation status:",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))


  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting copied PtrArr...")' )
  CALL Inspect_PtrArr( p_copy )
  
  
  ! Test equality
  WRITE( *,'(/5x,"Testing equality routines...")' )
  err_status = Equal_PtrArr(p, p_copy)
  CALL Display_Message('Equal_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"PtrArr structures are equal")' )
  WRITE( *,'( 5x,"PtrArr allocation status:",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))
  

  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_PtrArr(p)
  CALL Display_Message('Destroy_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_PtrArr(p_copy)
  CALL Display_Message('Destroy_PtrArr','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"PtrArr allocation status: ",2(1x,l5))' ) Allocated_PtrArr((/p, p_copy/))

END PROGRAM Test_PtrArr
