!
! LBLRTM_Input
!
! Module containing routines for creating LBLRTM input files.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Input

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

  
END MODULE LBLRTM_Input
