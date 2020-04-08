!
! LBLRTM_r1p3_Module
!
! Module containing procedures for LBLRTM Record r1p3.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r1p3_Module

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
  PUBLIC :: LBLRTM_r1p3_type
  ! Procedures
  PUBLIC :: LBLRTM_r1p3_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R1P3_FMT = '(8(es10.3),4x,i1,5x,es10.3,3x,i2)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r1p3_type
    REAL(fp) :: v1        = 0.0_fp   !  Beginning wavenumber value for the calculation
    REAL(fp) :: v2        = 0.0_fp   !  Ending wavenumber value for the calculation
    REAL(fp) :: sample    = 4.0_fp   !  number of sample points per mean halfwidth (between 1 and 4)
    REAL(fp) :: dvset     = 0.0_fp   !  Selected DV for the final monochromatic calculation
    REAL(fp) :: alfal0    = 0.04_fp  !  Average collision broadened halfwidth
    REAL(fp) :: avmass    = 36.0_fp  !  Average molecular mass (amu) for Doppler halfwidth
    REAL(fp) :: dptmin    = 0.0_fp   !  Minimum molecular optical depth below which lines will be rejected
    REAL(fp) :: dptfac    = 0.0_fp   !  Factor multiplying molecular continuum optical depth for rejection check
    INTEGER  :: ilnflg    = 0        !  Flag for binary record of line rejection information
    REAL(fp) :: dvout     = 0.0_fp   !  Selected DV grid for the optical depth 'monochromatic' output spacing
    INTEGER  :: nmol_scal = 0        !  Enables the scaling of the atmospheric profile for selected species
  END TYPE LBLRTM_r1p3_type


CONTAINS


  FUNCTION LBLRTM_r1p3_Write(r1p3,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r1p3_type), INTENT(IN) :: r1p3
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r1p3_Write'
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
    WRITE( fid,FMT=LBLRTM_R1P3_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p3
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r1p3_Write

END MODULE LBLRTM_r1p3_Module
