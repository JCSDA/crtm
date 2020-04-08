!
! LBLRTM_r9p1_Module
!
! Module containing procedures for LBLRTM Record r9p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r9p1_Module

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
  PUBLIC :: LBLRTM_r9p1_type
  ! Procedures
  PUBLIC :: LBLRTM_r9p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R9P1_FMT = '(3f10.3,2i5,15x,5i5)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r9p1_type
    REAL(fp) :: dvo    = 0.0_fp  !  Wavenumber spacing for interpolated result
    REAL(fp) :: v1     = 0.0_fp  !  Beginning wavenumber value for performing INTRPL
    REAL(fp) :: v2     = 0.0_fp  !  Ending wavenumber value for performing INTRPL
    INTEGER  :: jemit  = 0       !  Data selection flag. -1==absorption; 0==transmittance; 1==radiance
    INTEGER  :: i4pt   = 0       !  Interpolation type flag. 0==linear; 1==4-pt
    INTEGER  :: iunit  = 0       !  Unit designation of file to be interpolated
    INTEGER  :: ifilst = 0       !  Initial file from IUNIT to be interpolated
    INTEGER  :: nifils = 0       !  Number of files to be interpolated starting at IFILST
    INTEGER  :: junit  = 0       !  File containing interpolated results
    INTEGER  :: npts   = 0       !  No. of values printed for begin&end of each panel
  END TYPE LBLRTM_r9p1_type


CONTAINS


  FUNCTION LBLRTM_r9p1_Write(r9p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r9p1_type), INTENT(IN) :: r9p1
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r9p1_Write'
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
    WRITE( fid,FMT=LBLRTM_R9P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r9p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r9p1_Write

END MODULE LBLRTM_r9p1_Module
