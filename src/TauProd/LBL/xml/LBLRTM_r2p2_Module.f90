!
! LBLRTM_r2p2_Module
!
! Module containing procedures for LBLRTM Record r2p2.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r2p2_Module

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
  PUBLIC :: LBLRTM_r2p2_type
  ! Procedures
  PUBLIC :: LBLRTM_r2p2_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R2P2_FMT = '(i5,5x,i5)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r2p2_type
    INTEGER :: ixmols = 0  !  Number of cross-section molecules to be input
    INTEGER :: ixsbin = 0  !  Flag for pressure convolution of Xsect. 0==yes; 1==no
  END TYPE LBLRTM_r2p2_type


CONTAINS


  FUNCTION LBLRTM_r2p2_Write(r2p2,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r2p2_type), INTENT(IN) :: r2p2
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r2p2_Write'
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
    WRITE( fid,FMT=LBLRTM_R2P2_FMT,IOSTAT=io_stat,IOMSG=io_msg) r2p2
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r2p2_Write

END MODULE LBLRTM_r2p2_Module
