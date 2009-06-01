!
! Module defining the SatBiasAngle and SatBiasAirMass data structures. 
!
! DERIVED TYPES:
!   SatBiasAngle_type:  Definition of the public SatBiasAngle data structure.
!   -----------------   Fields are,
!     
!     nFOVs:           The number of FOV locations for the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     Channel_Index:   The index number of the current channel entry
!                      in the complete list of channels being processed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     Sensor_Id:       An unique string identifier for a satellite/sensor
!                      combination. It consists of a sensor and platform
!                      tag separated by "_". E.g. "hirs3_n16", "ssmis_f16", etc.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(SL)
!                      DIMENSION:  Scalar
!
!     Channel:         The instrument channel number of the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     LapseRate_Mean:  The integrated temperature lapse rate over the
!                      simulated weighting function for the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!
!     Bias_Mean:       The mean bias across the scan for the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!
!     Bias:            The scan angle bias at each scan angle position for
!                      the current channel entry.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1, MAX_SATBIAS_FOVS
!
!
!   SatBiasAirMass_type:  Definition of the public SatBiasAirMass data structure.
!   -------------------   Fields are,
!     
!     nPredictors:     The number of air mass bias predictors for the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     Channel_Index:   The index number of the current channel entry
!                      in the complete list of channels being processed.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     Sensor_Id:       An unique string identifier for a satellite/sensor
!                      combination. It consists of a sensor and platform
!                      tag separated by "_". E.g. "hirs3_n16", "ssmis_f16", etc.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(SL)
!                      DIMENSION:  Scalar
!
!     Channel:         The instrument channel number of the current
!                      channel entry.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!     c:               The air mass bias correction coefficients for
!                      the current channel entry.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1, MAX_SATBIAS_PREDICTORS
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 27-Dec-2005
!                   paul.vandelst@ssec.wisc.edu
!

MODULE SatBias_Define

  ! -----------
  ! Environment
  ! -----------
  ! Module usage
  USE Type_Kinds, ONLY: fp=>fp_kind
  ! Disable all implicit typing
  IMPLICIT NONE


  ! --------------------------
  ! Module entity visibilities
  ! --------------------------
  PRIVATE
  ! Parameters
  PUBLIC :: MAX_SATBIAS_FOVS
  PUBLIC :: MAX_SATBIAS_PREDICTORS
  ! Derived type definitions
  PUBLIC :: SatBiasAngle_type
  PUBLIC :: SatBiasAirMass_type
  ! Module procedures
  PUBLIC :: SatBias_Clear
  PUBLIC :: SatBias_Zero
  PUBLIC :: SatBias_CountSensors


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  INTEGER,  PARAMETER :: MAX_SATBIAS_FOVS       = 90
  INTEGER,  PARAMETER :: MAX_SATBIAS_PREDICTORS = 5
  INTEGER,  PARAMETER :: SL = 20


  ! ----------------
  ! Interface blocks
  ! ----------------
  INTERFACE SatBias_Clear
    MODULE PROCEDURE Angle_Clear
    MODULE PROCEDURE AirMass_Clear
  END INTERFACE SatBias_Clear

  INTERFACE SatBias_Zero
    MODULE PROCEDURE Angle_Zero
    MODULE PROCEDURE AirMass_Zero
  END INTERFACE SatBias_Zero

  INTERFACE SatBias_CountSensors
    MODULE PROCEDURE Angle_CountSensors
    MODULE PROCEDURE AirMass_CountSensors
  END INTERFACE SatBias_CountSensors

  ! ----------------
  ! Type definitions
  ! ----------------
  ! Scan angle bias structure
  TYPE :: SatBiasAngle_type
    INTEGER       :: nFOVs          = MAX_SATBIAS_FOVS      ! The FOV dimension
    INTEGER       :: Channel_Index  = -1                    ! The index of the channel in a list
    CHARACTER(SL) :: Sensor_Id      = ' '                   ! The sensor ID string, e.g. hirs3_n17
    INTEGER       :: Channel        = 0                     ! The sensor channel number
    REAL(fp)      :: LapseRate_Mean = ZERO                  ! The mean integrated lapse rate
    REAL(fp)      :: Bias_Mean      = ZERO                  ! The mean bias across the scan
    REAL(fp), DIMENSION( MAX_SATBIAS_FOVS ) :: Bias = ZERO  ! The angle bias data
  END TYPE SatBiasAngle_type

  ! Air mass bias structure
  TYPE :: SatBiasAirMass_type
    INTEGER       :: nPredictors   = MAX_SATBIAS_PREDICTORS    ! The predictor dimension
    INTEGER       :: Channel_Index = -1                        ! The index of the channel in a list 
    CHARACTER(SL) :: Sensor_Id     = ' '                       ! The sensor ID string, e.g. hirs3_n17
    INTEGER       :: Channel       = 0                         ! The sensor channel number          
    REAL(fp), DIMENSION( MAX_SATBIAS_PREDICTORS ) :: c = ZERO  ! The bias coefficients
  END TYPE SatBiasAirMass_type


CONTAINS


  ! --------------------------------------------
  ! Subroutine to clear SatBias Angle structures
  ! --------------------------------------------
  ELEMENTAL SUBROUTINE Angle_Clear( Angle )
    TYPE(SatBiasAngle_type), INTENT(IN OUT) :: Angle
    Angle%Channel_Index  = -1 
    Angle%Sensor_Id      = ' '
    Angle%Channel        = 0
    CALL Angle_Zero( Angle )
  END SUBROUTINE Angle_Clear


  ! ----------------------------------------------
  ! Subroutine to clear SatBias AirMass structures
  ! ----------------------------------------------
  ELEMENTAL SUBROUTINE AirMass_Clear( AirMass )
    TYPE( SatBiasAirMass_type ), INTENT(IN OUT) :: AirMass
    AirMass%Channel_Index  = -1 
    AirMass%Sensor_Id      = ' '
    AirMass%Channel        = 0
    CALL AirMass_Zero(AirMass)
  END SUBROUTINE AirMass_Clear


  ! ----------------------------------------------------
  ! Subroutine to zero out real SatBias Angle components
  ! ----------------------------------------------------
  ELEMENTAL SUBROUTINE Angle_Zero( Angle )
    TYPE(SatBiasAngle_type), INTENT(IN OUT) :: Angle
    Angle%LapseRate_Mean = ZERO
    Angle%Bias_Mean      = ZERO
    Angle%Bias           = ZERO
  END SUBROUTINE Angle_Zero


  ! ------------------------------------------------------
  ! Subroutine to zero out real SatBias AirMass components
  ! ------------------------------------------------------
  ELEMENTAL SUBROUTINE AirMass_Zero( AirMass )
    TYPE( SatBiasAirMass_type ), INTENT(IN OUT) :: AirMass
    AirMass%c = ZERO
  END SUBROUTINE AirMass_Zero


  ! ------------------------------------------------
  ! Function to count sensors in a SatBias structure
  ! ------------------------------------------------
  !
  ! Generic sensor counter
  !
  FUNCTION CountSensors(Sensor_Id) RESULT(nSensors)
    CHARACTER(*), DIMENSION(:), INTENT(IN) :: Sensor_Id
    INTEGER :: nSensors
    INTEGER :: i
    CHARACTER(SL) :: Id, Local_Id

    ! Initialisation
    nSensors = 0
    Local_Id = ' '

    ! Loop over Sensor IDs
    DO i = 1, SIZE( Sensor_Id )
      Id = ADJUSTL(Sensor_Id(i))
      IF ( LEN_TRIM(Id) > 0 ) THEN
        IF ( Local_Id /= Id ) THEN
          Local_Id = Id
          nSensors = nSensors + 1
        END IF
      END IF
    END DO
  END FUNCTION CountSensors
  !
  ! Sensor counter for SatBiasAngle
  !
  FUNCTION Angle_CountSensors(SatBias) RESULT(nSensors)
    TYPE( SatBiasAngle_type ), DIMENSION(:), INTENT(IN) :: SatBias
    INTEGER :: nSensors
    nSensors = CountSensors(SatBias%Sensor_Id)
  END FUNCTION Angle_CountSensors
  !
  ! Sensor counter for SatBiasAirMass
  !
  FUNCTION AirMass_CountSensors(SatBias) RESULT(nSensors)
    TYPE( SatBiasAirMass_type ), DIMENSION(:), INTENT(IN) :: SatBias
    INTEGER :: nSensors
    nSensors = CountSensors(SatBias%Sensor_Id)
  END FUNCTION AirMass_CountSensors

END MODULE SatBias_Define
