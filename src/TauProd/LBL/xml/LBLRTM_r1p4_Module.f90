!
! LBLRTM_r1p4_Module
!
! Module containing procedures for LBLRTM Record r1p4.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r1p4_Module

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
  PUBLIC :: LBLRTM_r1p4_type
  ! Procedures
  PUBLIC :: LBLRTM_r1p4_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R1P4_FMT = '(7(es10.3),4x,1a)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r1p4_type
    REAL(fp)     :: tbound    = 0.0_fp                    !  Temperature of end-of-path boundary (K)
    REAL(fp)     :: sremis(3) = (/1.0_fp,0.0_fp,0.0_fp/)  !  Frequency dependent boundary emissivity coefficients
    REAL(fp)     :: srrefl(3) = (/0.0_fp,0.0_fp,0.0_fp/)  !  Frequency dependent boundary reflectivity coefficients
    CHARACTER(1) :: surf_refl = ''                        !  Specifies the surface type (s,l) used in computing the reflected downward radiance
  END TYPE LBLRTM_r1p4_type


CONTAINS


  FUNCTION LBLRTM_r1p4_Write(r1p4,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r1p4_type), INTENT(IN) :: r1p4
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r1p4_Write'
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
    WRITE( fid,FMT=LBLRTM_R1P4_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p4
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r1p4_Write

END MODULE LBLRTM_r1p4_Module
