!
! LBLRTM_r1p6_Module
!
! Module containing procedures for LBLRTM Record r1p6.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r1p6_Module

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
  PUBLIC :: LBLRTM_r1p6_type
  ! Procedures
  PUBLIC :: LBLRTM_r1p6_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R1P6_FMT = '(a55,1x,i4)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r1p6_type
    CHARACTER(55) :: pthodl = ''  !  Pathname for precalculated optical depth files
    INTEGER       :: laytot = 0   !  Total number of layers used in radiative transfer
  END TYPE LBLRTM_r1p6_type


CONTAINS


  FUNCTION LBLRTM_r1p6_Write(r1p6,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r1p6_type), INTENT(IN) :: r1p6
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r1p6_Write'
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
    WRITE( fid,FMT=LBLRTM_R1P6_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p6
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r1p6_Write

END MODULE LBLRTM_r1p6_Module
