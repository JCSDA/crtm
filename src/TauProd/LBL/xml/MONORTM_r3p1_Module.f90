!
! MONORTM_r3p1_Module
!
! Module containing procedures for MONORTM Record r3p1.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE MONORTM_r3p1_Module

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
  PUBLIC :: MONORTM_r3p1_type
  ! Procedures
  PUBLIC :: MONORTM_r3p1_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: MONORTM_R3P1_FMT = '(7i5,i2,1x,i2,3f10.3,10x,f10.3)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: MONORTM_r3p1_type
    INTEGER  :: model   = 0       !  Selects atmospheric profile
    INTEGER  :: itype   = 0       !  Selects type of path
    INTEGER  :: ibmax   = 0       !  Selects layering
    INTEGER  :: nozero  = 1       !  Controls zeroing of small absorber amounts.
    INTEGER  :: noprnt  = 0       !  Controls printout length
    INTEGER  :: nmol    = 0       !  Number of molecular species
    INTEGER  :: ipunch  = 1       !  Controls output of layer data to TAPE7
    INTEGER  :: ifxtyp  = 0       !  Controls cross-layer DV field output
    INTEGER  :: munits  = 0       !  Controls molecular amounts units output
    REAL(fp) :: re      = 0.0_fp  !  Radius of earth (km). 0.0 == model defaults
    REAL(fp) :: hspace  = 0.0_fp  !  Altitude defintion for space. 100km == default
    REAL(fp) :: vbar    = 0.0_fp  !  Frequency for refractive geometry. Average == default
    REAL(fp) :: ref_lat = 0.0_fp  !  LAtitude of location of calculation. 0.0 == model defaults
  END TYPE MONORTM_r3p1_type


CONTAINS


  FUNCTION MONORTM_r3p1_Write(r3p1,fid) RESULT(err_stat)

    ! Arguments
    TYPE(MONORTM_r3p1_type), INTENT(IN) :: r3p1
    INTEGER                , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'MONORTM_r3p1_Write'
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
    WRITE( fid,FMT=MONORTM_R3P1_FMT,IOSTAT=io_stat,IOMSG=io_msg) r3p1
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION MONORTM_r3p1_Write

END MODULE MONORTM_r3p1_Module
