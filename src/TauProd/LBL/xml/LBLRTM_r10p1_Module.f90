!
! LBLRTM_r10p1_Module
!
! Module containing procedures for LBLRTM Record r10p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r10p1_Module

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
  PUBLIC :: LBLRTM_r10p1_type
  ! Procedures
  PUBLIC :: LBLRTM_r10p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R10P1_FMT = '(3f10.3,3i5,f10.3,4i5,i3,i2)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r10p1_type
    REAL(fp) :: hwhm   = 0.0_fp  !  Half-width half maximum
    REAL(fp) :: v1     = 0.0_fp  !  Beginning wavenumber value for performing FFTSCN
    REAL(fp) :: v2     = 0.0_fp  !  Ending wavenumber value for performing FFTSCN
    INTEGER  :: jemit  = 0       !  Data selection flag. 0==transmittance; 1==radiance
    INTEGER  :: jfnin  = 0       !  Scanning function flag. 0 - 13
    INTEGER  :: mratin = 0       !  Ratio of HWHM of the scanning function to the halfwidth of the boxcar
    REAL(fp) :: dvout  = 0.0_fp  !  Output wavenumber grid spacing
    INTEGER  :: iunit  = 0       !  Unit designation of file to be scanned
    INTEGER  :: ifilst = 0       !  Initial file from IUNIT to be interpolated
    INTEGER  :: nifils = 0       !  Number of files to be interpolated starting at IFILST
    INTEGER  :: junit  = 0       !  File containing interpolated results
    INTEGER  :: ivx    = 0       !  Scanning fn calculation flag. -1==FFT of apodfn; 0==program determines; 1==analytic
    INTEGER  :: nofix  = 0       !  Deconvolution flag. 0==deconvolve with boxcar; 1==No deconvolution
  END TYPE LBLRTM_r10p1_type


CONTAINS


  FUNCTION LBLRTM_r10p1_Write(r10p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r10p1_type), INTENT(IN) :: r10p1
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r10p1_Write'
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
    WRITE( fid,FMT=LBLRTM_R10P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r10p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r10p1_Write

END MODULE LBLRTM_r10p1_Module
