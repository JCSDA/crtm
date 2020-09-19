!
! LBLRTM_r2p2p1_Module
!
! Module containing procedures for LBLRTM Record r2p2p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r2p2p1_Module

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
  PUBLIC :: LBLRTM_r2p2p1_type
  ! Procedures
  PUBLIC :: LBLRTM_r2p2p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R2P2P1_FMT = '(7a10,(/,8a10))'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r2p2p1_type
    CHARACTER(10) :: xsname(MAX_N_XSECT_ABSORBERS) = ''  !  Name of the cross-section molecules
  END TYPE LBLRTM_r2p2p1_type


CONTAINS


  FUNCTION LBLRTM_r2p2p1_Write(r2p2p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r2p2p1_type), INTENT(IN) :: r2p2p1
    INTEGER                 , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r2p2p1_Write'
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
    WRITE( fid,FMT=LBLRTM_R2P2P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r2p2p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r2p2p1_Write

END MODULE LBLRTM_r2p2p1_Module
