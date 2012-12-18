!
! LBLRTM_Input_Module
!
! Module containing routines for creating LBLRTM input files.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Input_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE COMMON_r1p1_Module  , LBLRTM_r1p1_type  => COMMON_r1p1_type, &
                            LBLRTM_r1p1_Write => COMMON_r1p1_Write
  USE LBLRTM_r1p2_Module
  USE COMMON_r1p2a_Module , LBLRTM_r1p2a_type  => COMMON_r1p2a_type, &
                            LBLRTM_r1p2a_Write => COMMON_r1p2a_Write
  USE LBLRTM_r1p2p1_Module
  USE LBLRTM_r1p3_Module
  USE COMMON_r1p3a_Module , LBLRTM_r1p3a_type  => COMMON_r1p3a_type, &
                            LBLRTM_r1p3a_Write => COMMON_r1p3a_Write
  USE COMMON_r1p3b_Module , LBLRTM_r1p3b_type  => COMMON_r1p3b_type, &
                            LBLRTM_r1p3b_Write => COMMON_r1p3b_Write
  USE LBLRTM_r1p4_Module
  USE COMMON_r2p1_Module  , LBLRTM_r2p1_type  => COMMON_r2p1_type, &
                            LBLRTM_r2p1_Write => COMMON_r2p1_Write
  USE LBLRTM_r2p1p1_Module
  USE COMMON_r2p1p2_Module, LBLRTM_r2p1p2_type  => COMMON_r2p1p2_type, &
                            LBLRTM_r2p1p2_Write => COMMON_r2p1p2_Write
  USE LBLRTM_r2p2_Module
  USE LBLRTM_r2p2p1_Module
  USE LBLRTM_r2p2p2_Module
  USE LBLRTM_r3p1_Module
  USE COMMON_r3p2_Module  , LBLRTM_r3p2_type  => COMMON_r3p2_type, &
                            LBLRTM_r3p2_Write => COMMON_r3p2_Write
  USE COMMON_r3p3a_Module , LBLRTM_r3p3a_type  => COMMON_r3p3a_type, &
                            LBLRTM_r3p3a_Write => COMMON_r3p3a_Write
  USE COMMON_r3p3b_Module , LBLRTM_r3p3b_type  => COMMON_r3p3b_type, &
                            LBLRTM_r3p3b_Write => COMMON_r3p3b_Write
  USE COMMON_r3p4_Module  , LBLRTM_r3p4_type  => COMMON_r3p4_type, &
                            LBLRTM_r3p4_Write => COMMON_r3p4_Write
  USE COMMON_r3p5_Module  , LBLRTM_r3p5_type  => COMMON_r3p5_type, &
                            LBLRTM_r3p5_Write => COMMON_r3p5_Write
  USE COMMON_r3p6_Module  , LBLRTM_r3p6_type  => COMMON_r3p6_type, &
                            LBLRTM_r3p6_Write => COMMON_r3p6_Write
  USE COMMON_r3p7_Module  , LBLRTM_r3p7_type  => COMMON_r3p7_type, &
                            LBLRTM_r3p7_Write => COMMON_r3p7_Write
  USE COMMON_r3p7p1_Module, LBLRTM_r3p7p1_type  => COMMON_r3p7p1_type, &
                            LBLRTM_r3p7p1_Write => COMMON_r3p7p1_Write
  USE COMMON_r3p8_Module  , LBLRTM_r3p8_type  => COMMON_r3p8_type, &
                            LBLRTM_r3p8_Write => COMMON_r3p8_Write
  USE COMMON_r3p8p1_Module, LBLRTM_r3p8p1_type  => COMMON_r3p8p1_type, &
                            LBLRTM_r3p8p1_Write => COMMON_r3p8p1_Write
  USE COMMON_r3p8p2_Module, LBLRTM_r3p8p2_type  => COMMON_r3p8p2_type, &
                            LBLRTM_r3p8p2_Write => COMMON_r3p8p2_Write
  USE LBLRTM_r6_Module
  USE LBLRTM_r6p1_Module
  USE LBLRTM_r7p1_Module
  USE LBLRTM_r7p2_Module
  USE LBLRTM_r7p3_Module
  USE LBLRTM_r8p1_Module
  ! Disable implicit typing
  IMPLICIT NONE


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_Input_type
    TYPE(LBLRTM_r1p1_type)   :: r1p1
    TYPE(LBLRTM_r1p2_type)   :: r1p2
    TYPE(LBLRTM_r1p2a_type)  :: r1p2a
    TYPE(LBLRTM_r1p2p1_type) :: r1p2p1
    TYPE(LBLRTM_r1p3_type)   :: r1p3
    TYPE(LBLRTM_r1p3a_type)  :: r1p3a
    TYPE(LBLRTM_r1p3b_type)  :: r1p3b
    TYPE(LBLRTM_r1p4_type)   :: r1p4
    TYPE(LBLRTM_r2p1_type)   :: r2p1
    TYPE(LBLRTM_r2p1p1_type) :: r2p1p1
    TYPE(LBLRTM_r2p1p2_type) :: r2p1p2
    TYPE(LBLRTM_r2p2_type)   :: r2p2
    TYPE(LBLRTM_r2p2p1_type) :: r2p2p1
    TYPE(LBLRTM_r2p2p2_type) :: r2p2p2
    TYPE(LBLRTM_r3p1_type)   :: r3p1
    TYPE(LBLRTM_r3p2_type)   :: r3p2
    TYPE(LBLRTM_r3p3a_type)  :: r3p3a
    TYPE(LBLRTM_r3p3b_type)  :: r3p3b
    TYPE(LBLRTM_r3p4_type)   :: r3p4
    TYPE(LBLRTM_r3p5_type)   :: r3p5
    TYPE(LBLRTM_r3p6_type)   :: r3p6
    TYPE(LBLRTM_r3p7_type)   :: r3p7
    TYPE(LBLRTM_r3p7p1_type) :: r3p7p1
    TYPE(LBLRTM_r3p8_type)   :: r3p8
    TYPE(LBLRTM_r3p8p1_type) :: r3p8p1
    TYPE(LBLRTM_r3p8p2_type) :: r3p8p2
    TYPE(LBLRTM_r6_type)     :: r6
    TYPE(LBLRTM_r6p1_type)   :: r6p1
    TYPE(LBLRTM_r7p1_type)   :: r7p1
    TYPE(LBLRTM_r7p2_type)   :: r7p2
    TYPE(LBLRTM_r7p3_type)   :: r7p3
    TYPE(LBLRTM_r8p1_type)   :: r8p1
  END TYPE LBLRTM_Input_type
  

  ! ----------
  ! Visibility
  ! ----------
  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Input_type
  ! Procedures
  PUBLIC :: LBLRTM_Input_Write


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'

CONTAINS


  FUNCTION LBLRTM_Input_Write( &
    lblrtm_input, &
    filename    ) &
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Input_type), INTENT(IN) :: lblrtm_input
    CHARACTER(*)           , INTENT(IN) :: filename
  
  
  END FUNCTION LBLRTM_Input_Write

END MODULE LBLRTM_Input_Module
