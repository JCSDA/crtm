!
!  FTS_SensorData_Define
!
!  Module defining the FTS_SensorData data structure and containing
!  routines to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE FTS_SensorData_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE SpcCoeff_Define, ONLY: INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID, &
                             UNPOLARIZED
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Public procedures to manipulate the FTS_SensorData structure
  PUBLIC :: Destroy_FTS_SensorData
  PUBLIC :: Allocate_FTS_SensorData
  PUBLIC :: Assign_FTS_SensorData
  PUBLIC :: Load_FTS_SensorData
  PUBLIC :: Print_FTS_SensorData
  PUBLIC :: Get_FTS_SensorData_Sensor_ID


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &

  ! The Sensor Id string length
  INTEGER, PARAMETER :: SL = 20
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PARAMETER ::  ONE   = 1.0_fp
  REAL(fp), PARAMETER :: POINT5 = 0.5_fp
  ! FTS_SensorData valid values
  INTEGER, PARAMETER :: INVALID = -1
  INTEGER, PARAMETER ::   VALID =  1
  ! Total number of points per channel for computing the
  ! channel frequencies and responses. Must be evenly
  ! divisible by 2 and 4.
  INTEGER, PARAMETER :: N_FREQUENCIES = 256
  INTEGER, PARAMETER :: N_HALFPOINTS  = N_FREQUENCIES/2


  ! -----------------------------------
  ! FTS_SensorData data type definition
  ! -----------------------------------
  TYPE, PUBLIC :: FTS_SensorData_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER :: n_Bands = 0  ! N
    ! Sensor IDs
    CHARACTER(SL) :: Sensor_ID        = ' '
    INTEGER       :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
    ! Sensor data
    REAL(fp) :: Laser_Wavelength   = ZERO  ! microns
    REAL(fp) :: Laser_Frequency    = ZERO  ! inverse centimetres
    REAL(fp) :: Nyquist_Frequency  = ZERO  ! inverse centimetres
    REAL(fp) :: Field_Angle        = ZERO  ! radians
    ! Apodisation Function (AF) parameters
    REAL(fp) :: AF_HWHM = ZERO
    ! Band parameters
    INTEGER , POINTER :: n_Band_Pts(:) => NULL()  ! N
    INTEGER , POINTER :: n_FFT_Pts(:)  => NULL()  ! N
    REAL(fp), POINTER :: Max_OPD(:)    => NULL()  ! N
    REAL(fp), POINTER :: Band_F1(:)    => NULL()  ! N
    REAL(fp), POINTER :: Band_F2(:)    => NULL()  ! N
    REAL(fp), POINTER :: FFT_F1(:)     => NULL()  ! N
    REAL(fp), POINTER :: FFT_F2(:)     => NULL()  ! N
  END TYPE FTS_SensorData_type
  
  
  
END MODULE FTS_SensorData_Define
