!
! COMMON_r3p3a_Module
!
! Module containing procedures for COMMON Record r3p3a.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p3a_Module

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
  PUBLIC :: COMMON_r3p3a_type
  ! Procedures
  PUBLIC :: COMMON_r3p3a_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P3A_FMT = '(5f10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p3a_type
    REAL(fp) :: avtrat = 1.5_fp    !  Maximum Voigt width ratio across a layer
    REAL(fp) :: tdiff1 = 5.0_fp    !  Maximum layer temperature difference at altd1
    REAL(fp) :: tdiff2 = 8.0_fp    !  Maximum layer temperature difference at altd2
    REAL(fp) :: altd1  = 0.0_fp    !  Altitude of tdiff1
    REAL(fp) :: altd2  = 100.0_fp  !  Altitude of tdiff2
  END TYPE COMMON_r3p3a_type


CONTAINS


  FUNCTION COMMON_r3p3a_Write(r3p3a,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r3p3a_type), INTENT(IN) :: r3p3a
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p3a_Write'
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
    WRITE( fid,FMT=COMMON_R3P3A_FMT,IOSTAT=io_stat,IOMSG=io_msg) r3p3a
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p3a_Write

END MODULE COMMON_r3p3a_Module
