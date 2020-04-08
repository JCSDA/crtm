!
! LBLRTM_r1p2_Module
!
! Module containing procedures for LBLRTM Record r1p2.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_r1p2_Module

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
  PUBLIC :: LBLRTM_r1p2_type
  ! Procedures
  PUBLIC :: LBLRTM_r1p2_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: LBLRTM_R1P2_FMT = '(10(4x,i1),3x,i2,3(4x,i1),2(1x,i4))'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_r1p2_type
    INTEGER :: ihirac = 1  !  Voigt profile
    INTEGER :: ilblf4 = 1  !  Line-by-line bounds is 25cm-1 for all layer pressures
    INTEGER :: icntnm = 1  !  All continua calculated, including Rayleigh extinction
    INTEGER :: iaersl = 0  !  No aerosols used
    INTEGER :: iemit  = 1  !  Radiance and transmittance calculated
    INTEGER :: iscan  = 0  !  No scanning function
    INTEGER :: ifiltr = 0  !  No filter function
    INTEGER :: iplot  = 0  !  No plotting output
    INTEGER :: itest  = 0  !  No test
    INTEGER :: iatm   = 1  !  Call LBLATM
    INTEGER :: imrg   = 0  !  Normal merge
    INTEGER :: ilas   = 0  !  Laser option (not currently available)
    INTEGER :: iod    = 0  !  Normal calculation for layering control in OD calcs.
    INTEGER :: ixsect = 0  !  No cross-sections included in calculation
    INTEGER :: mpts   = 0  !  Number of OD panel values output as a result of convolution
    INTEGER :: npts   = 0  !  Number of OD panel values output as a result of merge
  END TYPE LBLRTM_r1p2_type


CONTAINS


  FUNCTION LBLRTM_r1p2_Write(r1p2,fid) RESULT(err_stat)

    ! Arguments
    TYPE(LBLRTM_r1p2_type), INTENT(IN) :: r1p2
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_r1p2_Write'
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
    WRITE( fid,FMT=LBLRTM_R1P2_FMT,IOSTAT=io_stat,IOMSG=io_msg) r1p2
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION LBLRTM_r1p2_Write

END MODULE LBLRTM_r1p2_Module
