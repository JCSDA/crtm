!
! COMMON_r3p8p2_Module
!
! Module containing procedures for COMMON Record r3p8p2.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p8p2_Module

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
  PUBLIC :: COMMON_r3p8p2_type
  ! Procedures
  PUBLIC :: COMMON_r3p8p2_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P8P2_FMT = '(8es10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p8p2_type
    REAL(fp) :: denx(MAX_N_XSECT_ABSORBERS) = 0.0_fp  !  Xsection concentration
  END TYPE COMMON_r3p8p2_type


CONTAINS


  FUNCTION COMMON_r3p8p2_Write(r3p8p2,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r3p8p2_type), INTENT(IN) :: r3p8p2
    INTEGER                 , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p8p2_Write'
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
    WRITE( fid,FMT=COMMON_R3P8P2_FMT,IOSTAT=io_stat,IOMSG=io_msg) r3p8p2
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p8p2_Write

END MODULE COMMON_r3p8p2_Module
