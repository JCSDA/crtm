!
! LBLRTM_r8p1_Module
!
! Module containing procedures for LBLRTM Record r8p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r8p1_Module

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
  PUBLIC :: LBLRTM_r8p1_type
  ! Procedures
  PUBLIC :: LBLRTM_r8p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R8P1_FMT = '(3f10.3,3(3x,i2),f10.4,4(3x,i2),i5,f10.5)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r8p1_type
    REAL(fp) :: hwhm   = 0.0_fp  !  Half Width Half Maximum
    REAL(fp) :: v1     = 0.0_fp  !  Beginning wavenumber value for performing SCAN
    REAL(fp) :: v2     = 0.0_fp  !  Ending wavenumber value for performing SCAN
    INTEGER  :: jemit  = 0       !  Convolution flag. -1==absorption; 0==transmittance; 1==radiance
    INTEGER  :: jfn    = 0       !  Selects scanning function
    INTEGER  :: jvar   = 0       !  Flag for variable HWHM. 0==no variation; 1==HWHM(vi)=HWHM(v1)*(vi/v1)
    REAL(fp) :: sampl  = 0.0_fp  !  Number of sample points per half width
    INTEGER  :: iunit  = 0       !  Unit number of file to be scanned
    INTEGER  :: ifilst = 0       !  Initial file from IUNIT to be scanned
    INTEGER  :: nifils = 0       !  Number of files to be scanned starting at IFILST
    INTEGER  :: junit  = 0       !  File containing scanned results
    INTEGER  :: npts   = 0       !  No. of values printed for begin&end of each panel
  END TYPE LBLRTM_r8p1_type


CONTAINS


  FUNCTION LBLRTM_r8p1_Write(r8p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r8p1_type), INTENT(IN) :: r8p1
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r8p1_Write'
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
    WRITE( fid,FMT=LBLRTM_R8P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r8p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r8p1_Write

END MODULE LBLRTM_r8p1_Module
