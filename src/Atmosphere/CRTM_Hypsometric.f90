!
! CRTM_Hypsometric.f90
!
! Module for hypsometric calculations.
!
!
! CREATION HISTORY:
!       Written by:     Patrick Stegmann, 06-Jul-2021
!                       
!

MODULE CRTM_Hypsometric
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, &
                                    FAILURE, &
                                    WARNING, &
                                    INFORMATION, &
                                    Display_Message
  USE Fundamental_Constants , ONLY: DRY_AIR_GAS_CONSTANT, &
                                    STANDARD_GRAVITY
  USE CRTM_Parameters       , ONLY: ZERO, ONE, POINT_5
  USE CRTM_Atmosphere_Define, ONLY: CRTM_Atmosphere_type    , &
                                    OPERATOR(==), &
                                    OPERATOR(+), &
                                    CRTM_Atmosphere_Associated, &
                                    CRTM_Atmosphere_Create, &
                                    CRTM_Atmosphere_AddLayerCopy, &
                                    CRTM_Atmosphere_Zero

                                    
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Module procedures
  PUBLIC :: HypsometricEq



  ! -----------------
  ! Module parameters
  ! -----------------
  ! Message string length
  INTEGER, PARAMETER :: ML = 256



CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!--------------------------------------------------------------------------------
!
!
! NAME:
!       HypsometricEq
!
! PURPOSE:
!       Function to compute the layer virtual temperature of an atmospheric
!       profile from the level pressure and level geometric altitude:
!
!                 g * ( z_2 - z_1 )
!         T_v = ----------------------
!                 R * ln( p_1 / p_2 )
!
!         T_v : Layer virtual temperatue [K]
!         g : Earth gravitational acceleration [m/s^2]
!         R : specific gas constant of dry air [J / (K * kg)]
!         z : Geometric height [m]
!         p : Level pressure [hPa]
!
!
! CALLING SEQUENCE:
!       Virtual_Temperature = HypsometricEq( Atm, height )
!
! INPUTS:
!       Atm:             Atmosphere structure.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Atmosphere_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       height:          Geometric height of the atmospheric profile in atm.
!                        Array starts at 0, like Level_Pressure.
!                        UNITS:      meters [m]
!                        TYPE:       REAL(KIND=fp)
!                        DIMENSION:  Array
!                        ATTRIBUTES: INTENT(IN)
!
!
! FUNCTION RESULT:
!       Virtual_Temperature: The function result is an array of the layer
!                             virtual temperature corresponding to the 
!                             atmospheric profile in the atm structure.
!                        UNITS:      Kelvin [K]
!                        TYPE:       REAL(KIND=fp)
!                        DIMENSION:  Array(atm%n_Layers)
!
!--------------------------------------------------------------------------------
  FUNCTION HypsometricEq( &
    atm, &
    height) &
  RESULT(Virtual_Temperature)
    ! Data Dictionary:
    ! Arguments
    TYPE(CRTM_Atmosphere_type), INTENT(IN) :: atm
    REAL(KIND=fp), DIMENSION(0:atm%n_Layers), INTENT(IN) :: height ! Geometric height of the                                                      
                                                                   ! atmospheric profile.
                                                                   ! 0:K
    ! Function result
    REAL(KIND=fp), DIMENSION(atm%n_Layers) :: Virtual_Temperature  ! 1:K
    ! Local variables
    INTEGER :: ii 
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'HypsometricEq'
    CHARACTER(ML) :: msg
    ! Instructions:
    IF ( atm%n_Layers > 0 .AND. atm%n_Layers == atm%n_Layers ) THEN
      ! Calculations:
      Layer_loop: DO ii = 1, atm%n_Layers
        Virtual_Temperature(ii) = STANDARD_GRAVITY &
                                  * ( height(ii) - height(ii-1) ) &
                                  / ( DRY_AIR_GAS_CONSTANT &
                                   *LOG( atm%Level_Pressure(ii-1) / atm%Level_Pressure(ii) ) )
      END DO Layer_loop
    ELSE
      msg = 'Atmosphere structure dimensions invalid'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      RETURN
    END IF 
    RETURN
  END FUNCTION HypsometricEq
    
END MODULE CRTM_Hypsometric
