!
! COMMON_r3p5_Module
!
! Module containing procedures for COMMON Record r3p5.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 24-Dec-2012
!                     paul.vandelst@noaa.gov
!

MODULE COMMON_r3p5_Module

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
  PUBLIC :: COMMON_r3p5_type
  ! Procedures
  PUBLIC :: COMMON_r3p5_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  ! The record I/O format
  CHARACTER(*), PARAMETER :: COMMON_R3P5_FMT = '(3es10.3,5x,a1,a1,1x,a1,1x,39a1)'


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: COMMON_r3p5_type
    REAL(fp)     :: zm                     = 0.0_fp  !  Boundary altitude
    REAL(fp)     :: pm                     = 0.0_fp  !  Boundary pressure
    REAL(fp)     :: tm                     = 0.0_fp  !  Boundary temperature
    CHARACTER(1) :: jcharp                 = '+'     !  Number of input atmopsheric profile boundaries
    CHARACTER(1) :: jchart                 = '+'     !  Number of input atmopsheric profile boundaries
    CHARACTER(1) :: jlong                  = '+'     !  Number of input atmopsheric profile boundaries
    CHARACTER(1) :: jchar(MAX_N_ABSORBERS) = '+'     !  Flag for units for molecular input
  END TYPE COMMON_r3p5_type


CONTAINS


  FUNCTION COMMON_r3p5_Write(r3p5,fid) RESULT(err_stat)

    ! Arguments
    TYPE(COMMON_r3p5_type), INTENT(IN) :: r3p5
    INTEGER               , INTENT(IN) :: fid
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'COMMON_r3p5_Write'
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
    WRITE( fid,FMT=COMMON_R3P5_FMT,IOSTAT=io_stat,IOMSG=io_msg) r3p5
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing record - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION COMMON_r3p5_Write

END MODULE COMMON_r3p5_Module
