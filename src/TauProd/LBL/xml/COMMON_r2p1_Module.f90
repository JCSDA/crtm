!
! COMMON_r2p1_Module
!
! Module containing procedures for COMMON Record r2p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r2p1_Module

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
  PUBLIC :: COMMON_r2p1_type
  ! Procedures
  PUBLIC :: COMMON_r2p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R2P1_FMT = '(1x,i1,i3,i5,f10.2,20x,f8.2,4x,f8.2,5x,f8.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r2p1_type
    INTEGER  :: iform  = 0       !  Column amount format flag. If == 1, read data es15.7 format
    INTEGER  :: nlayrs = 0       !  Number of layers (maximum of 200)
    INTEGER  :: nmol   = 7       !  Value of highest molecule number used (default = 7; maximum of 35)
    REAL(fp) :: secnt0 = 1.0_fp  !  Column amount scale factor. If +ve, looking up; if -ve looking down
    REAL(fp) :: zh1    = 0.0_fp  !  Observer altitude
    REAL(fp) :: zh2    = 0.0_fp  !  End point altitude
    REAL(fp) :: zangle = 0.0_fp  !  Zenith angle for path calculation
  END TYPE COMMON_r2p1_type


CONTAINS


  FUNCTION COMMON_r2p1_Write(r2p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r2p1_type), INTENT(IN) :: r2p1
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r2p1_Write'
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
    WRITE( fid,FMT=COMMON_R2P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r2p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r2p1_Write

END MODULE COMMON_r2p1_Module
