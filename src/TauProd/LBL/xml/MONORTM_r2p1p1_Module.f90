!
! MONORTM_r2p1p1_Module
!
! Module containing procedures for MONORTM Record r2p1p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE MONORTM_r2p1p1_Module

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
  PUBLIC :: MONORTM_r2p1p1_type
  ! Procedures
  PUBLIC :: MONORTM_r2p1p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: MONORTM_R2P1P1_FMT = '(f10.4,f10.4,15x,1x,f7.2,f8.3,f7.2,f7.2,f8.3,f7.2,es15.7)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: MONORTM_r2p1p1_type
    REAL(fp) :: pave  = 0.0_fp  !  Average pressure of layer
    REAL(fp) :: tave  = 0.0_fp  !  Average temperature of layer
    REAL(fp) :: altzb = 0.0_fp  !  Altitude of BOTTOM of current layer
    REAL(fp) :: pzb   = 0.0_fp  !  Pressure at BOTTOM of current layer
    REAL(fp) :: tzb   = 0.0_fp  !  Temperature at BOTTOM of current layer
    REAL(fp) :: altz  = 0.0_fp  !  Altitude of TOP of current layer
    REAL(fp) :: pz    = 0.0_fp  !  Pressure at TOP of current layer
    REAL(fp) :: tz    = 0.0_fp  !  Temperature at TOP of current layer
    REAL(fp) :: clw   = 0.0_fp  !  Cloud liquid water amount (mm) of current layer
  END TYPE MONORTM_r2p1p1_type


CONTAINS


  FUNCTION MONORTM_r2p1p1_Write(r2p1p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(MONORTM_r2p1p1_type), INTENT(IN) :: r2p1p1
    INTEGER                  , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MONORTM_r2p1p1_Write'
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
    WRITE( fid,FMT=MONORTM_R2P1P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r2p1p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION MONORTM_r2p1p1_Write

END MODULE MONORTM_r2p1p1_Module
