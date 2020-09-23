!
! COMMON_r1p2a_Module
!
! Module containing procedures for COMMON Record r1p2a.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r1p2a_Module

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
  PUBLIC :: COMMON_r1p2a_type
  ! Procedures
  PUBLIC :: COMMON_r1p2a_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R1P2A_FMT = '(7(1x,f9.6))'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r1p2a_type
    REAL(fp) :: xself = 0.0_fp  !  H2O self broadened continuum absorption multiplicative factor
    REAL(fp) :: xfrgn = 0.0_fp  !  H2O foreign broadened continuum absorption multiplicative factor
    REAL(fp) :: xco2c = 0.0_fp  !  CO2 continuum absorption multiplicative factor
    REAL(fp) :: xo3cn = 0.0_fp  !  O3 continuum absorption multiplicative factor
    REAL(fp) :: xo2cn = 0.0_fp  !  O2 continuum absorption multiplicative factor
    REAL(fp) :: xn2cn = 0.0_fp  !  N2 continuum absorption multiplicative factor
    REAL(fp) :: xrayl = 0.0_fp  !  Rayleigh extinction multiplicative factor
  END TYPE COMMON_r1p2a_type


CONTAINS


  FUNCTION COMMON_r1p2a_Write(r1p2a,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r1p2a_type), INTENT(IN) :: r1p2a
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r1p2a_Write'
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
    WRITE( fid,FMT=COMMON_R1P2A_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p2a
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r1p2a_Write

END MODULE COMMON_r1p2a_Module
