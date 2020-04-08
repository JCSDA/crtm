!
! MONORTM_r1p2_Module
!
! Module containing procedures for MONORTM Record r1p2.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE MONORTM_r1p2_Module

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
  PUBLIC :: MONORTM_r1p2_type
  ! Procedures
  PUBLIC :: MONORTM_r1p2_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: MONORTM_R1P2_FMT = '(4x,i1,11x,i1,34x,i1,19x,i1,16x,i4)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: MONORTM_r1p2_type
    INTEGER :: ihirac = 1  !  Voigt profile
    INTEGER :: icntnm = 1  !  All continua calculated, including Rayleigh extinction
    INTEGER :: iatm   = 1  !  Call LBLATM
    INTEGER :: ixsect = 0  !  No cross-sections included in calculation
    INTEGER :: ispd   = 0  !  0 == slow (accurate) version of MonoRTM
  END TYPE MONORTM_r1p2_type


CONTAINS


  FUNCTION MONORTM_r1p2_Write(r1p2,fid) RESULT(err_stat)

    ! Arguments
    TYPE(MONORTM_r1p2_type), INTENT(IN) :: r1p2
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MONORTM_r1p2_Write'
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
    WRITE( fid,FMT=MONORTM_R1P2_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p2
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION MONORTM_r1p2_Write

END MODULE MONORTM_r1p2_Module
