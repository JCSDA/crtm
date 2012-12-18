!
! COMMON_r3p3b_Module
!
! Module containing procedures for COMMON Record r3p3b.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 18-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p3b_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE File_Utility   , ONLY: File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  ! Line-by-line model parameters
  USE LBL_Parameters
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: COMMON_r3p3b_type
  ! Procedures
  PUBLIC :: COMMON_r3p3b_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P3B_FMT = '(8f10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p3b_type
    REAL(fp) :: bnd(MAX_IBMAX) = 0.0_fp  !  Altitudes/pressures of layer boundaries
  END TYPE COMMON_r3p3b_type


CONTAINS


  FUNCTION COMMON_r3p3b_Write(r3p3b,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r3p3b_type), INTENT(IN) :: r3p3b
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p3b_Write'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check unit is open
    IF ( .NOT. File_Open(fid) ) THEN
      msg = 'File unit is not connected'
      CALL Cleanup(); RETURN
    END IF

    ! Write the record
    WRITE( fid,FMT=COMMON_R3P3B_FMT,IOSTAT=io_stat) r3p3b
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing record. IOSTAT = ",i0)' ) io_stat
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p3b_Write

END MODULE COMMON_r3p3b_Module