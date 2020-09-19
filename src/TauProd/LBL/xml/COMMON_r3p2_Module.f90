!
! COMMON_r3p2_Module
!
! Module containing procedures for COMMON Record r3p2.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p2_Module

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
  PUBLIC :: COMMON_r3p2_type
  ! Procedures
  PUBLIC :: COMMON_r3p2_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P2_FMT = '(5f10.3,i5,5x,f10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p2_type
    REAL(fp) :: h1    = 0.0_fp  !  Observer altitude
    REAL(fp) :: h2    = 0.0_fp  !  Endpoint altitude, or tangent height
    REAL(fp) :: angle = 0.0_fp  !  Zenith angle at h1
    REAL(fp) :: range = 0.0_fp  !  Length of straight path from h1 to h2
    REAL(fp) :: beta  = 0.0_fp  !  Earth centred angle from h1 to h2
    INTEGER  :: len   = 0       !  Short or long path
    REAL(fp) :: hobs  = 0.0_fp  !  Height of oberver, for info only
  END TYPE COMMON_r3p2_type


CONTAINS


  FUNCTION COMMON_r3p2_Write(r3p2,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r3p2_type), INTENT(IN) :: r3p2
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p2_Write'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check unit is open
    IF ( .NOT. File_Open(fid) ) THEN
      msg = 'File unit is not connected'
      CALL Cleanup(); RETURN
    END IF

    ! Write the record
    WRITE( fid,FMT=COMMON_R3P2_FMT,IOSTAT=io_stat,IOMSG=io_msg) r3p2
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p2_Write

END MODULE COMMON_r3p2_Module
