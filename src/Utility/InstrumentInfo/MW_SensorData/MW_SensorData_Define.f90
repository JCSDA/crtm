
!  MW_SensorData_Define
!
!  Module defining the MW_SensorData data structure and containing
!  routines to manipulate it.
!
!
!  Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                  paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

MODULE MW_SensorData_Define


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: fp=>FP_Kind, Long
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE SpcCoeff_Define, ONLY: INVALID_NCEP_SENSOR_ID, &
                             INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID, &
                             INVALID_POLARIZATION, &
                             UNPOLARIZED, &
                             INTENSITY, &
                             FIRST_STOKES_COMPONENT, &
                             SECOND_STOKES_COMPONENT, &
                             THIRD_STOKES_COMPONENT, &
                             FOURTH_STOKES_COMPONENT, &
                             VL_POLARIZATION, &
                             HL_POLARIZATION, &
                             plus45L_POLARIZATION, &
                             minus45L_POLARIZATION, &
                             VL_MIXED_POLARIZATION, &
                             HL_MIXED_POLARIZATION, &
                             RC_POLARIZATION, &
                             LC_POLARIZATION, &
                             POLARIZATION_TYPE_NAME
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Public procedures to manipulate the MW_SensorData structure
  PUBLIC :: Destroy_MW_SensorData
  PUBLIC :: Allocate_MW_SensorData
  PUBLIC :: Assign_MW_SensorData
  PUBLIC :: Load_MW_SensorData
  PUBLIC :: Print_MW_SensorData
  PUBLIC :: Get_MW_SensorData_Sensor_ID


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: MW_SensorData_Define.f90,v 1.27 2006/07/18 21:05:40 wd20pd Exp $'

  ! MW_SensorData valid values
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1
  INTEGER, PRIVATE, PARAMETER ::   VALID =  1

  ! Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! Numerical constants
  REAL(fp), PRIVATE, PARAMETER :: ZERO   = 0.0_fp
  REAL(fp), PRIVATE, PARAMETER ::  ONE   = 1.0_fp
  REAL(fp), PRIVATE, PARAMETER :: POINT5 = 0.5_fp

  ! Total number of points per channel for computing the
  ! channel frequencies and responses. Must be evenly
  ! divisible by 2 and 4.
  INTEGER, PRIVATE, PARAMETER :: N_FREQUENCIES = 256
  INTEGER, PRIVATE, PARAMETER :: N_HALFPOINTS  = N_FREQUENCIES/2


  ! ---------------------------------
  ! MW_SensorData data type definition
  ! ---------------------------------
  TYPE, PUBLIC :: MW_SensorData_type
    INTEGER :: n_Allocates = 0
    ! Dimensions
    INTEGER(Long) :: n_Frequencies = 0  ! Lm
    INTEGER(Long) :: n_Channels    = 0  ! L
    ! Sensor ID information
    INTEGER(Long) :: NCEP_Sensor_ID   = INVALID_NCEP_SENSOR_ID  
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
    ! The sensor channels
    INTEGER(Long), POINTER, DIMENSION(:) :: Sensor_Channel   => NULL()  ! L
    ! Sensor data
    REAL(fp),      POINTER, DIMENSION(:)     :: Central_Frequency => NULL()  ! L
    INTEGER(Long), POINTER, DIMENSION(:)     :: Polarization      => NULL()  ! L
    INTEGER(Long), POINTER, DIMENSION(:)     :: n_Sidebands       => NULL()  ! L
    REAL(fp),      POINTER, DIMENSION(:,:,:) :: IF_Band           => NULL()  ! 2 x 2 x L
    ! MW channel frequency and response grid
    REAL(fp), POINTER, DIMENSION(:)   :: Delta_Frequency => NULL()  ! L
    REAL(fp), POINTER, DIMENSION(:,:) :: Frequency       => NULL()  ! Lm x L
    REAL(fp), POINTER, DIMENSION(:,:) :: Response        => NULL()  ! Lm x L
  END TYPE MW_SensorData_type


  ! ----------------
  ! Module variables
  ! ----------------
  INTEGER, PRIVATE :: i



  !#----------------------------------------------------------------------------#
  !#                         -- MICROWAVE SENSOR ID DATA --                     #
  !#----------------------------------------------------------------------------#

  ! --------------------------
  ! The valid sensor ID values
  ! --------------------------

  INTEGER, PRIVATE, PARAMETER :: N_VALID_SENSORS = 40

  INTEGER, PRIVATE, PARAMETER, &
           DIMENSION(  N_VALID_SENSORS ) :: VALID_NCEP_SENSOR_ID = &
    (/ 205, 206, 207, 208, 209, 210, 211, 212, 214, &  ! NOAA-05 to -14 MSU (no NOAA-13)
       315, 316, 317, &                                ! NOAA-15 to -17 AMSU-A
       415, 416, 417, &                                ! NOAA-15 to -17 AMSU-B
       713, 714, 715, &                                ! DMSP-13 to -15 SSM/I
       813, 814, 815, &                                ! DMSP-13 to -15 SSM/T-1
       113, 114, 115, &                                ! DMSP-13 to -15 SSM/T-2
       516, &                                          ! DMSP-16 SSMIS
       349, 449, 549, &                                ! AQUA AMSU-A, HSB, and AMSR-E
       318, 418, &                                     ! NOAA-18 AMSU-A and MHS
       900, &                                          ! Coriolis WindSat
       1012, &                                         ! NPOESS-C1 ATMS (Proposed)
       319, 419, &                                     ! NOAA-N' AMSU-A and MHS (Proposed)
       320, 420, &                                     ! MetOp-A AMSU-A and MHS (Proposed)
       321, 421, &                                     ! MetOp-B AMSU-A and MHS (Proposed)
       322, 422 /)                                     ! MetOp-C AMSU-A and MHS (Proposed)

  INTEGER, PRIVATE, PARAMETER, &
           DIMENSION(  N_VALID_SENSORS ) :: VALID_WMO_SATELLITE_ID = &
    (/ 708, 706, 707, 200, 201, 202, 203, 204, 205, &  ! NOAA-05 to -14 MSU (no NOAA-13)
       206, 207, 208, &                                ! NOAA-15 to -17 AMSU-A
       206, 207, 208, &                                ! NOAA-15 to -17 AMSU-B
       246, 247, 248, &                                ! DMSP-13 to -15 SSM/I
       246, 247, 248, &                                ! DMSP-13 to -15 SSM/T-1
       246, 247, 248, &                                ! DMSP-13 to -15 SSM/T-2
       249, &                                          ! DMSP-16 SSMIS
       784, 784, 784, &                                ! AQUA AMSU-A, HSB, and AMSR-E
       209, 209, &                                     ! NOAA-18 AMSU-A and MHS
       283, &                                          ! Coriolis WindSat (MISSING VALUE)
       INVALID_WMO_SATELLITE_ID, &                     ! NPOESS-C1 ATMS (MISSING VALUE)
       210, 210, &                                     ! NOAA-N' AMSU-A and MHS (GUESS. NOT LISTED IN C-5)
         4,   4, &                                     ! MetOp-A AMSU-A and MHS
         3,   3, &                                     ! MetOp-B AMSU-A and MHS
         5,   5 /)                                     ! MetOp-C AMSU-A and MHS

  INTEGER, PRIVATE, PARAMETER, &
           DIMENSION(  N_VALID_SENSORS ) :: VALID_WMO_SENSOR_ID = &
    (/ 623, 623, 623, 623, 623, 623, 623, 623, 623, &  ! NOAA-05 to -14 MSU (no NOAA-13)
       570, 570, 570, &                                ! NOAA-15 to -17 AMSU-A
       574, 574, 574, &                                ! NOAA-15 to -17 AMSU-B
       905, 905, 905, &                                ! DMSP-13 to -15 SSM/I
       906, 906, 906, &                                ! DMSP-13 to -15 SSM/T-1
       907, 907, 907, &                                ! DMSP-13 to -15 SSM/T-2
       908, &                                          ! DMSP-16 SSMIS
       570, 246, 479, &                                ! AQUA AMSU-A, HSB, and AMSR-E
       570, 203, &                                     ! NOAA-18 AMSU-A and MHS
       INVALID_WMO_SENSOR_ID, &                        ! Coriolis WindSat (MISSING VALUE)
       621, &                                          ! NPOESS-C1 ATMS
       570, 203, &                                     ! NOAA-N' AMSU-A and MHS
       570, 203, &                                     ! MetOp-A AMSU-A and MHS
       570, 203, &                                     ! MetOp-B AMSU-A and MHS
       570, 203 /)                                     ! MetOp-C AMSU-A and MHS

  CHARACTER(*), PRIVATE, PARAMETER, &
                DIMENSION(  N_VALID_SENSORS ) :: VALID_SENSOR_NAME = &
    (/ 'MSU    ', 'MSU    ', 'MSU    ', 'MSU    ', 'MSU    ', &
       'MSU    ', 'MSU    ', 'MSU    ', 'MSU    ', &             ! NOAA-05 to -14 MSU (no NOAA-13)
       'AMSU-A ', 'AMSU-A ', 'AMSU-A ', &                        ! NOAA-15 to -17 AMSU-A
       'AMSU-B ', 'AMSU-B ', 'AMSU-B ', &                        ! NOAA-15 to -17 AMSU-B
       'SSM/I  ', 'SSM/I  ', 'SSM/I  ', &                        ! DMSP-13 to -15 SSM/I
       'SSM/T-1', 'SSM/T-1', 'SSM/T-1', &                        ! DMSP-13 to -15 SSM/I
       'SSM/T-2', 'SSM/T-2', 'SSM/T-2', &                        ! DMSP-13 to -15 SSM/I
       'SSMIS  ', &                                              ! DMSP-16 SSMIS
       'AMSU-A ', 'HSB    ', 'AMSR-E ', &                        ! AQUA AMSU-A, HSB, and AMSR-E
       'AMSU-A ', 'MHS    ', &                                   ! NOAA-18 AMSU-A and MHS
       'WindSat', &                                              ! Coriolis WindSat 
       'ATMS   ', &                                              ! NPOESS-C1 ATMS
       'AMSU-A ', 'MHS    ', &                                   ! NOAA-N' AMSU-A and MHS
       'AMSU-A ', 'MHS    ', &                                   ! MetOp-A AMSU-A and MHS
       'AMSU-A ', 'MHS    ', &                                   ! MetOp-B AMSU-A and MHS
       'AMSU-A ', 'MHS    ' /)                                   ! MetOp-C AMSU-A and MHS

  CHARACTER(*), PRIVATE, PARAMETER, &
                DIMENSION(  N_VALID_SENSORS ) :: VALID_SATELLITE_NAME = &
    (/ 'TIROS-N  ', 'NOAA-06  ', 'NOAA-07  ', 'NOAA-08  ', 'NOAA-09  ', &
       'NOAA-10  ', 'NOAA-11  ', 'NOAA-12  ', 'NOAA-14  ', &              ! NOAA-05 to -14 MSU (no NOAA-13)
       'NOAA-15  ', 'NOAA-16  ', 'NOAA-17  ', &                           ! NOAA-15 to -17 AMSU-A
       'NOAA-15  ', 'NOAA-16  ', 'NOAA-17  ', &                           ! NOAA-15 to -17 AMSU-B
       'DMSP-13  ', 'DMSP-14  ', 'DMSP-15  ', &                           ! DMSP-13 to -15 SSM/I
       'DMSP-13  ', 'DMSP-14  ', 'DMSP-15  ', &                           ! DMSP-13 to -15 SSM/I
       'DMSP-13  ', 'DMSP-14  ', 'DMSP-15  ', &                           ! DMSP-13 to -15 SSM/I
       'DMSP-16  ', &                                                     ! DMSP-16 SSMIS
       'Aqua     ', 'Aqua     ', 'Aqua     ', &                           ! AQUA AMSU-A, HSB, and AMSR-E
       'NOAA-18  ', 'NOAA-18  ', &                                        ! NOAA-18 AMSU-A and MHS
       'Coriolis ', &                                                     ! Coriolis WindSat 
       'NPOESS-C1', &                                                     ! NPOESS-C1 ATMS
       'NOAA-Np  ', 'NOAA-Np  ', &                                        ! NOAA-N' AMSU-A and MHS
       'MetOp-A  ', 'MetOp-A  ', &                                        ! MetOp-A AMSU-A and MHS
       'MetOp-B  ', 'MetOp-B  ', &                                        ! MetOp-B AMSU-A and MHS
       'MetOp-C  ', 'MetOp-C  ' /)                                        ! MetOp-C AMSU-A and MHS



  !#----------------------------------------------------------------------------#
  !#                    -- MICROWAVE SENSOR CHANNEL DATA --                     #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------------
  ! The number of channels for each included instrument
  ! ---------------------------------------------------

  INTEGER, PRIVATE, PARAMETER :: N_MSU_CHANNELS     =  4
  INTEGER, PRIVATE, PARAMETER :: N_SSMI_CHANNELS    =  7
  INTEGER, PRIVATE, PARAMETER :: N_SSMT1_CHANNELS   =  7
  INTEGER, PRIVATE, PARAMETER :: N_SSMT2_CHANNELS   =  5
  INTEGER, PRIVATE, PARAMETER :: N_AMSUA_CHANNELS   = 15
  INTEGER, PRIVATE, PARAMETER :: N_AMSUB_CHANNELS   =  5
  INTEGER, PRIVATE, PARAMETER :: N_HSB_CHANNELS     =  4
  INTEGER, PRIVATE, PARAMETER :: N_SSMIS_CHANNELS   = 24
  INTEGER, PRIVATE, PARAMETER :: N_MHS_CHANNELS     =  5
  INTEGER, PRIVATE, PARAMETER :: N_AMSRE_CHANNELS   = 12
  INTEGER, PRIVATE, PARAMETER :: N_WINDSAT_CHANNELS = 16
  INTEGER, PRIVATE, PARAMETER :: N_ATMS_CHANNELS    = 22
 

  ! --------------------------------------------
  ! The number of channels for the valid sensors
  ! --------------------------------------------

  INTEGER, PRIVATE, PARAMETER, &
           DIMENSION(  N_VALID_SENSORS ) :: VALID_N_CHANNELS = &
    (/ N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, & ! NOAA-05 to -08 MSU
       N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, N_MSU_CHANNELS, & ! NOAA-09 to -12 MSU
       N_MSU_CHANNELS, &                                                 ! NOAA-14 MSU (no NOAA-13)
       N_AMSUA_CHANNELS, N_AMSUA_CHANNELS, N_AMSUA_CHANNELS, &           ! NOAA-15 to -17 AMSU-A
       N_AMSUB_CHANNELS, N_AMSUB_CHANNELS, N_AMSUB_CHANNELS, &           ! NOAA-15 to -17 AMSU-B
       N_SSMI_CHANNELS,  N_SSMI_CHANNELS,  N_SSMI_CHANNELS,  &           ! DMSP-13 to -15 SSM/I
       N_SSMT1_CHANNELS, N_SSMT1_CHANNELS, N_SSMT1_CHANNELS, &           ! DMSP-13 to -15 SSM/T-1
       N_SSMT2_CHANNELS, N_SSMT2_CHANNELS, N_SSMT2_CHANNELS, &           ! DMSP-13 to -15 SSM/T-2
       N_SSMIS_CHANNELS, &                                               ! DMSP-16 SSMIS
       N_AMSUA_CHANNELS, N_HSB_CHANNELS, N_AMSRE_CHANNELS, &             ! AQUA AMSU-A, HSB, and AMSR-E
       N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! NOAA-18 AMSU-A and MHS
       N_WINDSAT_CHANNELS, &                                             ! Coriolis WindSat 
       N_ATMS_CHANNELS, &                                                ! NPOESS-C1 ATMS
       N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! NOAA-N' AMSU-A and MHS
       N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! MetOp-A AMSU-A and MHS
       N_AMSUA_CHANNELS, N_MHS_CHANNELS, &                               ! MetOp-B AMSU-A and MHS
       N_AMSUA_CHANNELS, N_MHS_CHANNELS /)                               ! MetOp-C AMSU-A and MHS


  ! --------------------------
  ! The sensor channel numbers
  ! --------------------------

  ! MSU
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MSU_CHANNELS ) :: &
    MSU_SENSOR_CHANNEL = (/ ( i, i = 1, N_MSU_CHANNELS ) /)

  ! AMSU-A
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUA_CHANNELS ) :: &
    AMSUA_SENSOR_CHANNEL = (/ ( i, i = 1, N_AMSUA_CHANNELS ) /)

  ! AMSU-B
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUB_CHANNELS ) :: &
    AMSUB_SENSOR_CHANNEL = (/ ( i, i = 1, N_AMSUB_CHANNELS ) /)

  ! SSM/I
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMI_CHANNELS ) :: &
    SSMI_SENSOR_CHANNEL = (/ ( i, i = 1, N_SSMI_CHANNELS ) /)

  ! SSM/T-1
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT1_CHANNELS ) :: &
    SSMT1_SENSOR_CHANNEL = (/ ( i, i = 1, N_SSMT1_CHANNELS ) /)

  ! SSM/T-2
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT2_CHANNELS ) :: &
    SSMT2_SENSOR_CHANNEL = (/ ( i, i = 1, N_SSMT2_CHANNELS ) /)

  ! SSMIS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMIS_CHANNELS ) :: &
    SSMIS_SENSOR_CHANNEL = (/ ( i, i = 1, N_SSMIS_CHANNELS ) /)

  ! HSB
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_HSB_CHANNELS ) :: &
    HSB_SENSOR_CHANNEL = (/ ( i, i = 1, N_HSB_CHANNELS ) /)

  ! MHS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MHS_CHANNELS ) :: &
    MHS_SENSOR_CHANNEL = (/ ( i, i = 1, N_MHS_CHANNELS ) /)

  ! AMSR-E
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSRE_CHANNELS ) :: &
    AMSRE_SENSOR_CHANNEL = (/ ( i, i = 1, N_AMSRE_CHANNELS ) /)

  ! WindSat
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_WINDSAT_CHANNELS ) :: &
    WINDSAT_SENSOR_CHANNEL = (/ ( i, i = 1, N_WINDSAT_CHANNELS ) /)

  ! ATMS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ATMS_CHANNELS ) :: &
    ATMS_SENSOR_CHANNEL = (/ ( i, i = 1, N_ATMS_CHANNELS ) /)



  !#----------------------------------------------------------------------------#
  !#                   -- MICROWAVE SIDEBAND NUMBERING DATA --                  #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! The maximum number of sidebands
  ! -------------------------------

  INTEGER, PRIVATE, PARAMETER :: MAX_N_SIDEBANDS = 2


  ! ----------------------------
  ! The channel sideband numbers
  ! ----------------------------

  ! MSU
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MSU_CHANNELS ) :: &
    MSU_N_SIDEBANDS = 1

  ! AMSU-A
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUA_CHANNELS ) :: &
    AMSUA_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           2, 2, 2, 2, &
                           1 /)

  ! AMSU-B
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUB_CHANNELS ) :: &
    AMSUB_N_SIDEBANDS = 1

  ! SSM/I
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMI_CHANNELS ) :: &
    SSMI_N_SIDEBANDS = 1

  ! SSM/T-1
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT1_CHANNELS ) :: &
    SSMT1_N_SIDEBANDS = 1

  ! SSM/T-2
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT2_CHANNELS ) :: &
    SSMT2_N_SIDEBANDS = 1

  ! SSMIS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMIS_CHANNELS ) :: &
    SSMIS_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                           2, 2, 2, 2 /)

  ! HSB
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_HSB_CHANNELS ) :: &
    HSB_N_SIDEBANDS = 1

  ! MHS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MHS_CHANNELS ) :: &
    MHS_N_SIDEBANDS = 1

  ! AMSRE
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSRE_CHANNELS ) :: &
    AMSRE_N_SIDEBANDS = 1

  ! WindSat
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_WINDSAT_CHANNELS ) :: &
    WINDSAT_N_SIDEBANDS = 1

  ! ATMS
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ATMS_CHANNELS ) :: &
    ATMS_N_SIDEBANDS = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
                          2, 2, 2, 2, &
                          1, 1, 1, 1, 1, 1, 1 /)



  !#----------------------------------------------------------------------------#
  !#                    -- MICROWAVE SENSOR FREQUENCY DATA --                   #
  !#----------------------------------------------------------------------------#


  ! --------------
  ! NOAA-5->14 MSU
  ! --------------

  ! Central frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MSU_CHANNELS ) :: MSU_F0 = &
    (/ 50.35_fp, 53.79_fp, 55.02_fp, 58.01_fp /)


  ! MSU I/F band limits in GHz. All channel bandwidths are 220MHz
  ! with a 20MHz stopband at the local oscillator frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MSU_CHANNELS ) :: MSU_IF_BAND = &
    RESHAPE( (/ 0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch1
                0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch2
                0.01_fp, 0.1_fp, ZERO, ZERO, &    ! MSU ch3
                0.01_fp, 0.1_fp, ZERO, ZERO /), & ! MSU ch4
             (/ 2, MAX_N_SIDEBANDS, N_MSU_CHANNELS /) )


  ! --------------
  ! NOAA-15 AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N15_F0 = &
    (/ 23.800370_fp, 31.400420_fp, 50.299910_fp, &
       52.799390_fp, 53.595410_fp, 54.399530_fp, &
       54.940640_fp, 55.498700_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 88.997000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 103 and AMSU-A2
  ! S/N 102
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N15_IF_BAND = &
    RESHAPE( (/ 0.00850_fp,  0.13551_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.00897_fp,  0.08957_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.00895_fp,  0.08952_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.00890_fp,  0.19916_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03110_fp,  0.19930_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.00894_fp,  0.19921_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.00891_fp,  0.19919_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.00891_fp,  0.16408_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.00891_fp,  0.16412_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17916_fp,  0.25574_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.257435_fp, 0.292545_fp, 0.353570_fp, 0.387850_fp, &     ! AMSU-A1 ch11
                0.292745_fp, 0.308035_fp, 0.337050_fp, 0.352130_fp, &     ! AMSU-A1 ch12
                0.308315_fp, 0.316245_fp, 0.328320_fp, 0.336240_fp, &     ! AMSU-A1 ch13
                0.316200_fp, 0.319120_fp, 0.325370_fp, 0.328310_fp, &     ! AMSU-A1 ch14
                0.49871_fp,  1.49820_fp,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! --------------
  ! NOAA-15 AMSU-B
  ! --------------

  ! AMSU-B central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUB_CHANNELS ) :: AMSUB_N15_F0 = &
    (/  88.992_fp, 149.992_fp, 183.312_fp, &
       183.312_fp, 183.312_fp /)

  ! AMSU-B I/F band limits in GHz. Ch's 18-20 numbers were supplied by
  ! Nigel Atkinson of the UKMO. The on-line User's Guide sideband 1
  ! centre frequencies for these channels were nominal rather than
  ! actual (this is being corrected. The correct sideband 1 centre
  ! frequencies are 1.0025, 3.0015 and 6.9375GHz)
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) :: AMSUB_N15_IF_BAND = &
    RESHAPE( (/ 0.399_fp, 1.405_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.398_fp, 1.401_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.762_fp, 1.243_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.512_fp, 3.491_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.002_fp, 7.873_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! --------------
  ! NOAA-16 AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N16_F0 = &
    (/ 23.800110_fp, 31.400580_fp, 50.299700_fp, &
       52.800740_fp, 53.595470_fp, 54.399780_fp, &
       54.940770_fp, 55.499540_fp, 57.290324_fp, &
       57.290324_fp, 57.290324_fp, 57.290324_fp, &
       57.290324_fp, 57.290324_fp, 88.999800_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 102 and AMSU-A2
  ! S/N 103
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N16_IF_BAND = &
    RESHAPE( (/ 0.00858_fp,  0.13403_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.00887_fp,  0.08957_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.00898_fp,  0.08956_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.00891_fp,  0.19916_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03094_fp,  0.19920_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.00892_fp,  0.19922_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.00888_fp,  0.19919_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.00894_fp,  0.16402_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.00887_fp,  0.16417_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17876_fp,  0.25545_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.257140_fp, 0.291780_fp, 0.353095_fp, 0.387585_fp, &     ! AMSU-A1 ch11
                0.292715_fp, 0.308025_fp, 0.336865_fp, 0.351875_fp, &     ! AMSU-A1 ch12
                0.308195_fp, 0.316165_fp, 0.328350_fp, 0.336250_fp, &     ! AMSU-A1 ch13
                0.316325_fp, 0.319255_fp, 0.325340_fp, 0.328240_fp, &     ! AMSU-A1 ch14
                0.49943_fp,  1.49645_fp,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! --------------
  ! NOAA-16 AMSU-B
  ! --------------

  ! AMSU-B central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUB_CHANNELS ) :: AMSUB_N16_F0 = &
    (/  89.0056_fp, 150.0080_fp, 183.2990_fp, &
       183.2990_fp, 183.2990_fp /)

  ! AMSU-B I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) :: AMSUB_N16_IF_BAND = &
    RESHAPE( (/ 0.399_fp, 1.405_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.397_fp, 1.397_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.751_fp, 1.245_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.507_fp, 3.485_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.011_fp, 7.989_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! --------------
  ! NOAA-17 AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N17_F0 = &
    (/ 23.799210_fp, 31.399660_fp, 50.291801_fp, &
       52.800800_fp, 53.596001_fp, 54.400242_fp, &
       54.939819_fp, 55.499199_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 88.999802_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 104 and AMSU-A2
  ! S/N 104.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N17_IF_BAND = &
    RESHAPE( (/ 0.00845_fp,  0.13396_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.00893_fp,  0.08951_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.00894_fp,  0.08952_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.00894_fp,  0.19915_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03108_fp,  0.19923_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.00894_fp,  0.19920_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.00893_fp,  0.19923_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.00890_fp,  0.16400_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.00897_fp,  0.16388_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17901_fp,  0.25566_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.257115_fp, 0.291785_fp, 0.353100_fp, 0.387960_fp, &     ! AMSU-A1 ch11
                0.292805_fp, 0.308095_fp, 0.337015_fp, 0.352085_fp, &     ! AMSU-A1 ch12
                0.308190_fp, 0.316110_fp, 0.328305_fp, 0.336195_fp, &     ! AMSU-A1 ch13
                0.316300_fp, 0.319240_fp, 0.325260_fp, 0.328120_fp, &     ! AMSU-A1 ch14
                0.49978_fp,  1.49921_fp,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! --------------
  ! NOAA-17 AMSU-B
  ! --------------

  ! AMSU-B central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUB_CHANNELS ) :: AMSUB_N17_F0 = &
    (/  89.002_fp, 149.984_fp, 183.299_fp, &
       183.299_fp, 183.299_fp /)


  ! AMSU-B I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS ) :: AMSUB_N17_IF_BAND = &
    RESHAPE( (/ 0.399_fp, 1.406_fp, ZERO, ZERO, &    ! AMSU-B ch16
                0.398_fp, 1.402_fp, ZERO, ZERO, &    ! AMSU-B ch17
                0.751_fp, 1.248_fp, ZERO, ZERO, &    ! AMSU-B ch18
                2.511_fp, 3.267_fp, ZERO, ZERO, &    ! AMSU-B ch19
                6.016_fp, 7.971_fp, ZERO, ZERO /), & ! AMSU-B ch20
             (/ 2, MAX_N_SIDEBANDS, N_AMSUB_CHANNELS /) )


  ! ---------------------------------------------
  ! SSM/I
  !
  ! Frequency information taken from Table 2.1
  !   "SSM/I Antenna Beamwidths (S/N 002)"
  ! in the Special Sensor Microwave/Imager User's
  ! Guide, NRL, 14-Sep-1987
  !
  ! Note that the "repeated" frequencies are for
  ! different polarisations. That is, the SSM/I
  ! is a seven channel instrument, but only a 
  ! four frequency instrument.
  ! ---------------------------------------------

  ! Central frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_SSMI_CHANNELS ) :: SSMI_F0 = &
    (/ 19.35_fp, 19.35_fp, &
       22.235_fp, &
       37.0_fp,  37.0_fp, &
       85.5_fp,  85.5_fp /)

  ! SSM/I I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMI_CHANNELS ) :: SSMI_IF_BAND = &
    RESHAPE( (/ 0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch1
                0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch2
                0.010_fp, 0.250_fp, ZERO, ZERO, &    ! SSM/I ch3
                0.100_fp, 1.000_fp, ZERO, ZERO, &    ! SSM/I ch4
                0.100_fp, 1.000_fp, ZERO, ZERO, &    ! SSM/I ch5
                0.100_fp, 1.500_fp, ZERO, ZERO, &    ! SSM/I ch6
                0.100_fp, 1.500_fp, ZERO, ZERO /), & ! SSM/I ch7
             (/ 2, MAX_N_SIDEBANDS, N_SSMI_CHANNELS /) )


  ! -------------------------------------------------------
  ! SSM/T-1
  !
  ! Frequency information taken from
  !   http://www.saa.noaa.gov/help/detail/DMSP/README.ssmt1
  !
  ! The band WIDTHS are as stated in the above reference,
  ! but the I/F start values are simply set to 0.0 since
  ! there was no other info about this.
  ! -------------------------------------------------------

  ! Central frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_SSMT1_CHANNELS ) :: SSMT1_F0 = &
    (/ 50.5_fp, 53.2_fp, 54.35_fp, &
       54.9_fp, 58.4_fp, &
       58.825_fp, &
       59.4_fp /)

  ! SSM/T-1 I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMT1_CHANNELS ) :: SSMT1_IF_BAND = &
    RESHAPE( (/ 0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch1
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch2
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch3
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch4
                0.000_fp, 0.0575_fp, ZERO, ZERO, &    ! SSM/T-1 ch5
                0.000_fp, 0.2000_fp, ZERO, ZERO, &    ! SSM/T-1 ch6
                0.000_fp, 0.1250_fp, ZERO, ZERO /), & ! SSM/T-1 ch7
             (/ 2, MAX_N_SIDEBANDS, N_SSMT1_CHANNELS /) )


  ! -------
  ! SSM/T-2
  ! -------

  ! Central frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_SSMT2_CHANNELS ) :: SSMT2_F0 = &
    (/ 183.31000_fp, 183.31000_fp, 183.31000_fp, &
        91.66500_fp, 150.00000_fp /)

  ! SSM-T/2 I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMT2_CHANNELS ) :: SSMT2_IF_BAND = &
    RESHAPE( (/ 2.500_fp, 3.500_fp, ZERO, ZERO, &    ! SSM-T/2 ch1
                0.750_fp, 1.250_fp, ZERO, ZERO, &    ! SSM-T/2 ch2
                6.250_fp, 7.750_fp, ZERO, ZERO, &    ! SSM-T/2 ch3
                0.500_fp, 2.000_fp, ZERO, ZERO, &    ! SSM-T/2 ch4
                0.500_fp, 2.000_fp, ZERO, ZERO /), & ! SSM-T/2 ch5
             (/ 2, MAX_N_SIDEBANDS, N_SSMT2_CHANNELS /) )


  ! -------------
  ! DMSP-16 SSMIS
  ! -------------

  ! Central frequency
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_SSMIS_CHANNELS ) :: SSMIS_F16_F0 = &
    (/ 50.300000_fp,  52.800000_fp,  53.596000_fp,  54.400000_fp, &
       55.500000_fp,  57.290000_fp,  59.400000_fp, 150.000000_fp, &
      183.310000_fp, 183.310000_fp, 183.310000_fp,  19.350000_fp, &
       19.350000_fp,  22.235000_fp,  37.000000_fp,  37.000000_fp, &
       91.655000_fp,  91.655000_fp,  63.283248_fp,  60.792668_fp, &
       60.792668_fp,  60.792668_fp,  60.792668_fp,  60.792668_fp /)

  ! SSMIS I/F band limits in GHz.

  ! F16 parameters
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_IF_BAND = &
    RESHAPE( (/ 0.000000_fp,  0.190000_fp,  ZERO,              ZERO,            &      ! SSMIS ch1
                0.000000_fp,  0.194400_fp,  ZERO,              ZERO,            &      ! SSMIS ch2
                0.000000_fp,  0.190000_fp,  ZERO,              ZERO,            &      ! SSMIS ch3
                0.000000_fp,  0.191250_fp,  ZERO,              ZERO,            &      ! SSMIS ch4
                0.000000_fp,  0.195650_fp,  ZERO,              ZERO,            &      ! SSMIS ch5
                0.000000_fp,  0.165000_fp,  ZERO,              ZERO,            &      ! SSMIS ch6
                0.000000_fp,  0.119400_fp,  ZERO,              ZERO,            &      ! SSMIS ch7
                0.429000_fp,  2.071000_fp,  ZERO,              ZERO,            &      ! SSMIS ch8
                5.837000_fp,  7.363000_fp,  ZERO,              ZERO,            &      ! SSMIS ch9
                2.490500_fp,  3.509500_fp,  ZERO,              ZERO,            &      ! SSMIS ch10
                0.743750_fp,  1.256250_fp,  ZERO,              ZERO,            &      ! SSMIS ch11
                0.000000_fp,  0.177500_fp,  ZERO,              ZERO,            &      ! SSMIS ch12
                0.000000_fp,  0.178350_fp,  ZERO,              ZERO,            &      ! SSMIS ch13
                0.000000_fp,  0.203750_fp,  ZERO,              ZERO,            &      ! SSMIS ch14
                0.000000_fp,  0.807500_fp,  ZERO,              ZERO,            &      ! SSMIS ch15
                0.000000_fp,  0.772500_fp,  ZERO,              ZERO,            &      ! SSMIS ch16
                0.191000_fp,  1.609000_fp,  ZERO,              ZERO,            &      ! SSMIS ch17
                0.194500_fp,  1.605500_fp,  ZERO,              ZERO,            &      ! SSMIS ch18
                0.284591_fp,  0.285951_fp,  ZERO,              ZERO,            &      ! SSMIS ch19
                0.357217_fp,  0.358567_fp,  ZERO,              ZERO,            &      ! SSMIS ch20
                0.355247_fp,  0.356537_fp,  0.359247_fp,  0.360537_fp, &     ! SSMIS ch21
                0.351082_fp,  0.353702_fp,  0.362082_fp,  0.364702_fp, &     ! SSMIS ch22
                0.338232_fp,  0.345552_fp,  0.370232_fp,  0.377552_fp, &     ! SSMIS ch23
                0.294642_fp,  0.321142_fp,  0.394642_fp,  0.421142_fp /), &  ! SSMIS ch24
             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )

!!  ! Nominal instrument parameters
!!  REAL(fp), PRIVATE, PARAMETER, &
!!            DIMENSION( 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS ) :: SSMIS_F16_IF_BAND = &
!!    RESHAPE( (/ 0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch1
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch2
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch3
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch4
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch5
!!                0.000000_fp,  0.175000_fp,  ZERO,             ZERO,            &     ! SSMIS ch6
!!                0.000000_fp,  0.125000_fp,  ZERO,             ZERO,            &     ! SSMIS ch7
!!                0.500000_fp,  2.000000_fp,  ZERO,             ZERO,            &     ! SSMIS ch8
!!                5.850000_fp,  7.350000_fp,  ZERO,             ZERO,            &     ! SSMIS ch9
!!                2.500000_fp,  3.500000_fp,  ZERO,             ZERO,            &     ! SSMIS ch10
!!                0.750000_fp,  1.250000_fp,  ZERO,             ZERO,            &     ! SSMIS ch11
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch12
!!                0.000000_fp,  0.200000_fp,  ZERO,             ZERO,            &     ! SSMIS ch13
!!                0.000000_fp,  0.225000_fp,  ZERO,             ZERO,            &     ! SSMIS ch14
!!                0.000000_fp,  0.750000_fp,  ZERO,             ZERO,            &     ! SSMIS ch15
!!                0.000000_fp,  0.750000_fp,  ZERO,             ZERO,            &     ! SSMIS ch16
!!                0.150000_fp,  1.650000_fp,  ZERO,             ZERO,            &     ! SSMIS ch17
!!                0.150000_fp,  1.650000_fp,  ZERO,             ZERO,            &     ! SSMIS ch18
!!                0.284521_fp,  0.286021_fp,  ZERO,             ZERO,            &     ! SSMIS ch19
!!                0.357142_fp,  0.358642_fp,  ZERO,             ZERO,            &     ! SSMIS ch20
!!                0.355142_fp,  0.356642_fp,  0.359142_fp,  0.360642_fp, &     ! SSMIS ch21
!!                0.350892_fp,  0.353892_fp,  0.361892_fp,  0.364892_fp, &     ! SSMIS ch22
!!                0.337892_fp,  0.345892_fp,  0.369892_fp,  0.377892_fp, &     ! SSMIS ch23
!!                0.292892_fp,  0.322892_fp,  0.392892_fp,  0.422892_fp /), &  ! SSMIS ch24
!!             (/ 2, MAX_N_SIDEBANDS, N_SSMIS_CHANNELS /) )


  ! -------------------------
  ! Aqua AMSU-A (spec values)
  ! -------------------------

  ! AMSU-A central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_AQUA_F0 = &
    (/ 23.800000_fp, 31.400000_fp, 50.300000_fp, &
       52.800000_fp, 53.596000_fp, 54.400000_fp, &
       54.940000_fp, 55.500000_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 57.290344_fp, &
       57.290344_fp, 57.290344_fp, 89.000000_fp /)

  ! AMSU-A I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_AQUA_IF_BAND = &
    RESHAPE( (/ 0.01000_fp,  0.13500_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.01000_fp,  0.09000_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.01000_fp,  0.09000_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.01000_fp,  0.20000_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03000_fp,  0.20000_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.01000_fp,  0.20000_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.01000_fp,  0.20000_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.01000_fp,  0.16500_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.01000_fp,  0.16500_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17800_fp,  0.25600_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.256200_fp, 0.292200_fp, 0.352200_fp, 0.388200_fp, &     ! AMSU-A1 ch11
                0.292200_fp, 0.308200_fp, 0.336200_fp, 0.352200_fp, &     ! AMSU-A1 ch12
                0.308200_fp, 0.316200_fp, 0.328200_fp, 0.336200_fp, &     ! AMSU-A1 ch13
                0.316200_fp, 0.319200_fp, 0.325200_fp, 0.328200_fp, &     ! AMSU-A1 ch14
                0.50000_fp,  1.50000_fp,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------------------------
  ! Aqua HSB (AMSU-B spec values)
  ! -----------------------------

  ! HSB central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_HSB_CHANNELS ) :: HSB_AQUA_F0 = &
    (/ 150.000_fp, 183.310_fp, 183.310_fp, 183.310_fp /)

  ! HSB I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_HSB_CHANNELS ) :: HSB_AQUA_IF_BAND = &
    RESHAPE( (/ 0.400_fp, 1.400_fp, ZERO, ZERO, &    ! HSB ch1 == AMSU-B ch2
                0.750_fp, 1.250_fp, ZERO, ZERO, &    ! HSB ch2 == AMSU-B ch3
                2.500_fp, 3.500_fp, ZERO, ZERO, &    ! HSB ch3 == AMSU-B ch4
                6.000_fp, 8.000_fp, ZERO, ZERO /), & ! HSB ch4 == AMSU-B ch5
             (/ 2, MAX_N_SIDEBANDS, N_HSB_CHANNELS /) )



  ! -------------------------------------------------------
  ! Aqua AMSR-E
  !
  ! Frequency information taken from Table I from
  !
  ! Kawanishi, T. et al (2003), Advanced Microwave Scanning
  !   Radiometer for the Earth Observing System (AMSR-E),
  !   IEEE Transactions on Geoscience and Remote Sensing,
  !   41(2), pp184-194.
  !   doi 10.1109/TGRS.2002.808331
  !
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  ! 
  ! Note each frequency is listed twice. This is for separate
  ! channels with vertical and horizontal polarisations.
  ! -------------------------------------------------------

  ! AMSR-E central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSRE_CHANNELS ) :: AMSRE_AQUA_F0 = &
    (/ 6.925_fp,  6.925_fp, 10.650_fp, 10.650_fp, &
      18.700_fp, 18.700_fp, 23.800_fp, 23.800_fp, &
      36.500_fp, 36.500_fp, 89.000_fp, 89.000_fp /)

  ! AMSR-E I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSRE_CHANNELS ) :: AMSRE_AQUA_IF_BAND = &
    RESHAPE( (/ 0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz vertical pol.
                0.000_fp, 0.175_fp, ZERO, ZERO, &     ! 6.925GHz horizontal pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz vertical pol.
                0.000_fp, 0.050_fp, ZERO, ZERO, &     ! 10.65GHz horizontal pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz vertical pol.
                0.000_fp, 0.100_fp, ZERO, ZERO, &     ! 18.7GHz horizontal pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz vertical pol.
                0.000_fp, 0.200_fp, ZERO, ZERO, &     ! 23.8GHz horizontal pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz vertical pol.
                0.000_fp, 0.500_fp, ZERO, ZERO, &     ! 36.5GHz horizontal pol.
                0.000_fp, 1.500_fp, ZERO, ZERO, &     ! 89.0GHz vertical pol.
                0.000_fp, 1.500_fp, ZERO, ZERO /), &  ! 89.0GHz horizontal pol.
             (/ 2, MAX_N_SIDEBANDS, N_AMSRE_CHANNELS /) )


  ! --------------
  ! NOAA-18 AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290324GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N18_F0 = &
    (/ 23.800837_fp, 31.399518_fp, 50.299742_fp, &
       52.800072_fp, 53.595969_fp, 54.399819_fp, &
       54.940337_fp, 55.500255_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 89.009713_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 109 and AMSU-A2
  ! S/N 105. These are the 15C values.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N18_IF_BAND = &
    RESHAPE( (/ 0.00872_fp,  0.13423_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch1
                0.00877_fp,  0.08912_fp,  ZERO,             ZERO,             &     ! AMSU-A2 ch2
                0.00890_fp,  0.08900_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch3
                0.00912_fp,  0.19916_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch4
                0.03138_fp,  0.19886_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch5
                0.00915_fp,  0.19933_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch6
                0.00910_fp,  0.19919_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch7
                0.00914_fp,  0.16402_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch8
                0.00911_fp,  0.16415_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch9
                0.17901_fp,  0.25534_fp,  ZERO,             ZERO,             &     ! AMSU-A1 ch10
                0.256806_fp, 0.291663_fp, 0.352663_fp, 0.388032_fp, &     ! AMSU-A1 ch11
                0.292552_fp, 0.307980_fp, 0.336329_fp, 0.351829_fp, &     ! AMSU-A1 ch12
                0.308288_fp, 0.316136_fp, 0.328226_fp, 0.336086_fp, &     ! AMSU-A1 ch13
                0.316321_fp, 0.319253_fp, 0.325312_fp, 0.328254_fp, &     ! AMSU-A1 ch14
                0.49208_fp,  1.48898_fp,  ZERO,             ZERO              /), & ! AMSU-A1 ch15
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------
  ! NOAA-18 MHS
  ! -----------

  ! MHS central frequencies in GHz.
  ! These values are taken from the Table 3-1: Verification Report Matrix
  ! in the Matra Marconi Space document ref:3175-JA323-RPV, pg9.
  ! In PDF form, this is pg807 in the file MHS101_volume3.pdf.
  ! I couldn't find the actual measurement data so I consider these
  ! as provisional until I get confirmation they're final.
  ! NOTE: Channel 5 was implemented using the upper sideband option,
  !       rather than the typical 183.311+/-7GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MHS_CHANNELS ) :: MHS_N18_F0 = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! These were taken from the tables on pg10 and 11 in the Matra Marconi
  ! Space document ref:MHS.TR.JA247.MMP. In PDF form these were pp520-521
  ! of "MHS QRR_volume3.pdf" or pp362-363 of MHS101volume3.pdf.
  ! These are the 15C values for LO A.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) :: MHS_N18_IF_BAND = &
    RESHAPE( (/ 0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch17
                0.752_fp, 1.210_fp, ZERO, ZERO, &    ! MHS ch18
                2.524_fp, 3.434_fp, ZERO, ZERO, &    ! MHS ch19
                0.113_fp, 1.079_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )

! These are the LO B values
!    RESHAPE( (/ 0.111_fp, 1.207_fp, ZERO, ZERO, &    ! MHS ch16
!                0.113_fp, 1.209_fp, ZERO, ZERO, &    ! MHS ch17
!                0.750_fp, 1.210_fp, ZERO, ZERO, &    ! MHS ch18
!                2.526_fp, 3.430_fp, ZERO, ZERO, &    ! MHS ch19
!                0.113_fp, 1.079_fp, ZERO, ZERO /), & ! MHS ch20


  ! -------------------------------------------------------
  ! Coriolis WindSat
  !
  ! Frequency information taken from ppt presentation
  !
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  ! 
  ! Note frequencies are repeated. This is for separate
  ! channels with the same frequency but different
  ! polarisations (or combinations of polarisations).
  ! -------------------------------------------------------

  ! WindSat central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_WINDSAT_CHANNELS ) :: WINDSAT_CORIOLIS_F0 = &
    (/ 6.8_fp,  6.8_fp, &
      10.7_fp, 10.7_fp, 10.7_fp, 10.7_fp, &
      18.7_fp, 18.7_fp, 18.7_fp, 18.7_fp, &
      23.8_fp, 23.8_fp, &
      37.0_fp, 37.0_fp, 37.0_fp, 37.0_fp /)

  ! WindSat I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_WINDSAT_CHANNELS ) :: WINDSAT_CORIOLIS_IF_BAND = &
    RESHAPE( (/ 0.000_fp, 0.0625_fp, ZERO, ZERO, &    !  6.8GHz vertical pol.
                0.000_fp, 0.0625_fp, ZERO, ZERO, &    !  6.8GHz horizontal pol.
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(h) + R(v)
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(h) - R(v)    
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(+45) - R(-45)
                0.000_fp, 0.150_fp,  ZERO, ZERO, &    ! 10.7GHz R(rc) - R(lc)  
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(h) + R(v)
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(h) - R(v)    
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(+45) - R(-45)
                0.000_fp, 0.375_fp,  ZERO, ZERO, &    ! 18.7GHz R(rc) - R(lc)  
                0.000_fp, 0.250_fp,  ZERO, ZERO, &    ! 23.8GHz vertical pol.
                0.000_fp, 0.250_fp,  ZERO, ZERO, &    ! 23.8GHz horizontal pol.
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(h) + R(v)
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(h) - R(v)    
                0.000_fp, 1.000_fp,  ZERO, ZERO, &    ! 37.0GHz R(+45) - R(-45)
                0.000_fp, 1.000_fp,  ZERO, ZERO /), & ! 37.0GHz R(rc) - R(lc)  
             (/ 2, MAX_N_SIDEBANDS, N_WINDSAT_CHANNELS /) )


  ! -------------------------------------------------------
  ! NPOESS-C1 ATMS
  !
  ! Frequency information taken from ppt presentation
  !
  ! It is assumed there are no stopbands in the same manner
  ! as other microwave instruments with no sidebands so f1
  ! values are all 0.000.
  ! -------------------------------------------------------

  ! ATMS central frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_ATMS_CHANNELS ) :: ATMS_C1_F0 = &
    (/ 23.800000_fp,  31.400000_fp,  50.300000_fp,  51.760000_fp, &
       52.800000_fp,  53.596000_fp,  54.400000_fp,  54.940000_fp, &
       55.500000_fp,  57.290344_fp,  57.290344_fp,  57.290344_fp, &
       57.290344_fp,  57.290344_fp,  57.290344_fp,  88.200000_fp, &
      165.500000_fp, 183.310000_fp, 183.310000_fp, 183.310000_fp, &
      183.310000_fp, 183.310000_fp /)

  ! ATMS I/F band limits in GHz.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_ATMS_CHANNELS ) :: ATMS_C1_IF_BAND = &
    RESHAPE( (/ 0.000000_fp,  0.135000_fp,  ZERO,              ZERO,             &      ! ATMS ch1
                0.000000_fp,  0.090000_fp,  ZERO,              ZERO,             &      ! ATMS ch2
                0.000000_fp,  0.090000_fp,  ZERO,              ZERO,             &      ! ATMS ch3
                0.000000_fp,  0.200000_fp,  ZERO,              ZERO,             &      ! ATMS ch4
                0.000000_fp,  0.200000_fp,  ZERO,              ZERO,             &      ! ATMS ch5
                0.030000_fp,  0.200000_fp,  ZERO,              ZERO,             &      ! ATMS ch6
                0.000000_fp,  0.200000_fp,  ZERO,              ZERO,             &      ! ATMS ch7
                0.000000_fp,  0.200000_fp,  ZERO,              ZERO,             &      ! ATMS ch8
                0.000000_fp,  0.165000_fp,  ZERO,              ZERO,             &      ! ATMS ch9
                0.000000_fp,  0.165000_fp,  ZERO,              ZERO,             &      ! ATMS ch10
                0.178000_fp,  0.256000_fp,  ZERO,              ZERO,             &      ! ATMS ch11
                0.256200_fp,  0.292200_fp,  0.352200_fp,  0.388200_fp, &      ! ATMS ch12
                0.292200_fp,  0.308200_fp,  0.336200_fp,  0.352200_fp, &      ! ATMS ch13
                0.308200_fp,  0.316200_fp,  0.328200_fp,  0.336200_fp, &      ! ATMS ch14
                0.316200_fp,  0.319200_fp,  0.325200_fp,  0.328200_fp, &      ! ATMS ch15
                0.000000_fp,  1.000000_fp,  ZERO,              ZERO,             &      ! ATMS ch16
                0.000000_fp,  1.500000_fp,  ZERO,              ZERO,             &      ! ATMS ch17
                6.000000_fp,  8.000000_fp,  ZERO,              ZERO,             &      ! ATMS ch18
                3.500000_fp,  5.500000_fp,  ZERO,              ZERO,             &      ! ATMS ch19
                2.500000_fp,  3.500000_fp,  ZERO,              ZERO,             &      ! ATMS ch20
                1.300000_fp,  2.300000_fp,  ZERO,              ZERO,             &      ! ATMS ch21
                0.750000_fp,  1.250000_fp,  ZERO,              ZERO             /), &   ! ATMS ch22
             (/ 2, MAX_N_SIDEBANDS, N_ATMS_CHANNELS /) )


  ! --------------
  ! NOAA-N' AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290343GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_N19_F0 = &
    (/ 23.800904_fp, 31.400728_fp, 50.300271_fp, &
       52.800655_fp, 53.596130_fp, 54.400131_fp, &
       54.939489_fp, 55.498680_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 57.290329_fp, &
       57.290329_fp, 57.290329_fp, 89.010000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 107 (report #11326)
  ! and AMSU-A2 S/N 108 (report #11329). These are the 15C values.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_N19_IF_BAND = &
    RESHAPE( (/ 0.00867_fp,  0.13416_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch1 (pg.107)
                0.00893_fp,  0.08892_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch2 (pg.118)
                0.00884_fp,  0.08896_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch3 (pg.211)
                0.00904_fp,  0.19924_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch4 (pg.222)
                0.03140_fp,  0.19926_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch5 (pg.233)
                0.00914_fp,  0.19911_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch6 (pg.244)
                0.00913_fp,  0.19928_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch7 (pg.255)
                0.00919_fp,  0.16425_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch8 (pg.266)
                0.00916_fp,  0.16422_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch9 (pg.277)
                0.17934_fp,  0.25532_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch10 (pg.288)
                0.256937_fp, 0.291835_fp, 0.352592_fp, 0.387883_fp, &     ! AMSU-A1 ch11 (pp.302[LO],303[HI])
                0.292551_fp, 0.307988_fp, 0.336236_fp, 0.351764_fp, &     ! AMSU-A1 ch12 (pp.315[LO],316[HI])
                0.308247_fp, 0.316070_fp, 0.328234_fp, 0.336887_fp, &     ! AMSU-A1 ch13 (pp.328[LO],329[HI])
                0.316274_fp, 0.319223_fp, 0.325256_fp, 0.328212_fp, &     ! AMSU-A1 ch14 (pp.341[LO],342[HI])
                0.49157_fp,  1.48733_fp,  ZERO,             ZERO    /), & ! AMSU-A1 ch15 (pg.349)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------
  ! NOAA-N' MHS
  ! -----------

  ! MHS central frequencies in GHz.
  ! Instrument serial number: 102-MHS
  ! Data Source Reference: MHS.TR.JA249.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MHS_CHANNELS ) :: MHS_N19_F0 = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 102-MHS
  ! Data Source Reference: MHS.TR.JA249.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) :: MHS_N19_IF_BAND = &
    RESHAPE( (/ 0.118_fp, 1.217_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.211_fp, ZERO, ZERO, &    ! MHS ch17
                0.745_fp, 1.219_fp, ZERO, ZERO, &    ! MHS ch18
                2.520_fp, 3.367_fp, ZERO, ZERO, &    ! MHS ch19
                0.112_fp, 1.071_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! --------------
  ! MetOp-A AMSU-A
  ! --------------

! *** AMSU-A1 S/N 106 (was previously on MetOp-B)
! *** AMSU-A2 S/N 108 (was previously on NOAA-N')
  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290319GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_METOPA_F0 = &
    (/ 23.800904_fp, 31.400728_fp, 50.300069_fp, &
       52.799890_fp, 53.596155_fp, 54.400633_fp, &
       54.940002_fp, 55.499802_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 88.997000_fp /)


  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 106 (report #11324)
  ! and AMSU-A2 S/N 108 (report #11329). These are the 15C values.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_METOPA_IF_BAND = &
    RESHAPE( (/ 0.00867_fp,  0.13416_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch1 (pg.107)
                0.00893_fp,  0.08892_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch2 (pg.118)
                0.00885_fp,  0.08921_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch3 (pg.208)
                0.00905_fp,  0.19903_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch4 (pg.208)
                0.03134_fp,  0.19909_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch5 (pg.208)
                0.00910_fp,  0.19930_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch6 (pg.208)
                0.00915_fp,  0.19923_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch7 (pg.208)
                0.00906_fp,  0.16419_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch8 (pg.208)
                0.00913_fp,  0.16402_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch9 (pg.208)
                0.17921_fp,  0.25508_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch10 (pg.208)
                0.257043_fp, 0.291937_fp, 0.352609_fp, 0.387988_fp, &     ! AMSU-A1 ch11 (pp.302[LO],303[HI])
                0.292628_fp, 0.308060_fp, 0.336465_fp, 0.351940_fp, &     ! AMSU-A1 ch12 (pp.315[LO],316[HI])
                0.308343_fp, 0.316152_fp, 0.328359_fp, 0.335223_fp, &     ! AMSU-A1 ch13 (pp.328[LO],329[HI])
                0.316276_fp, 0.319209_fp, 0.325346_fp, 0.328300_fp, &     ! AMSU-A1 ch14 (pp.341[LO],342[HI])
                0.48978_fp,  1.48805_fp,  ZERO,             ZERO    /), & ! AMSU-A1 ch15 (pg.208)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )

! *** INSTRUMENTS BEFORE SWITCH. Jul 18, 2006 ***
! *** AMSU-A1 S/N 108 ***
! *** AMSU-A2 S/N 109 ***
!
!  ! AMSU-A central frequencies in GHz
!  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
!  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290319GHz
!  REAL(fp), PRIVATE, PARAMETER, &
!            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_METOPA_F0 = &
!    (/ 23.799744_fp, 31.402100_fp, 50.299733_fp, &
!       52.800575_fp, 53.596741_fp, 54.399710_fp, &
!       54.939924_fp, 55.500450_fp, 57.290313_fp, &
!       57.290313_fp, 57.290313_fp, 57.290313_fp, &
!       57.290313_fp, 57.290313_fp, 88.003000_fp /)
!
!
!  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
!  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
!  ! calibration logbooks for AMSU-A1 S/N 108 (report #11328)
!  ! and AMSU-A2 S/N 109 (report #11331). These are the 15C values.
!  REAL(fp), PRIVATE, PARAMETER, &
!            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_METOPA_IF_BAND = &
!    RESHAPE( (/ 0.00861_fp,  0.13415_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch1 (pg.103)
!                0.00875_fp,  0.08915_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch2 (pg.114)
!                0.00879_fp,  0.08909_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch3 (pg.205)
!                0.00907_fp,  0.19936_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch4 (pg.205)
!                0.03130_fp,  0.19900_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch5 (pg.205)
!                0.00907_fp,  0.19938_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch6 (pg.205)
!                0.00913_fp,  0.19932_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch7 (pg.205)
!                0.00914_fp,  0.16419_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch8 (pg.205)
!                0.00915_fp,  0.16414_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch9 (pg.205)
!                0.17914_fp,  0.25527_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch10 (pg.205)
!                0.256977_fp, 0.291873_fp, 0.352628_fp, 0.387892_fp, &     ! AMSU-A1 ch11 (pp.206)
!                0.292565_fp, 0.307998_fp, 0.336332_fp, 0.351781_fp, &     ! AMSU-A1 ch12 (pp.206)
!                0.308312_fp, 0.316124_fp, 0.328339_fp, 0.336215_fp, &     ! AMSU-A1 ch13 (pp.206)
!                0.316278_fp, 0.319208_fp, 0.325201_fp, 0.328140_fp, &     ! AMSU-A1 ch14 (pp.206)
!                0.49129_fp,  1.48691_fp,  ZERO,             ZERO    /), & ! AMSU-A1 ch15 (pg.205)
!             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------
  ! MetOp-A MHS
  ! -----------

  ! MHS central frequencies in GHz.
  ! Instrument serial number: 103-MHS
  ! Data Source Reference: MHS.TR.JA256.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MHS_CHANNELS ) :: MHS_METOPA_F0 = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 103-MHS
  ! Data Source Reference: MHS.TR.JA256.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) :: MHS_METOPA_IF_BAND = &
    RESHAPE( (/ 0.112_fp, 1.206_fp, ZERO, ZERO, &    ! MHS ch16
                0.114_fp, 1.211_fp, ZERO, ZERO, &    ! MHS ch17
                0.750_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch18
                2.500_fp, 3.330_fp, ZERO, ZERO, &    ! MHS ch19
                0.111_fp, 1.065_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! --------------
  ! MetOp-B AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290322GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_METOPB_F0 = &
    (/ 23.800335_fp, 31.401473_fp, 50.300069_fp, &
       52.799890_fp, 53.596155_fp, 54.400633_fp, &
       54.940002_fp, 55.499802_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 57.290327_fp, &
       57.290327_fp, 57.290327_fp, 88.997000_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 106 (report #11324)
  ! and AMSU-A2 S/N 106 (report #11325). These are the 15C values.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_METOPB_IF_BAND = &
    RESHAPE( (/ 0.00852_fp,  0.13416_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch1 (pg.103)
                0.00916_fp,  0.08938_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch2 (pg.114)
                0.00885_fp,  0.08921_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch3 (pg.208)
                0.00905_fp,  0.19903_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch4 (pg.208)
                0.03134_fp,  0.19909_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch5 (pg.208)
                0.00910_fp,  0.19930_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch6 (pg.208)
                0.00915_fp,  0.19923_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch7 (pg.208)
                0.00906_fp,  0.16419_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch8 (pg.208)
                0.00913_fp,  0.16402_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch9 (pg.208)
                0.17921_fp,  0.25508_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch10 (pg.208)
                0.257043_fp, 0.291937_fp, 0.352609_fp, 0.387988_fp, &     ! AMSU-A1 ch11 (pp.302[LO],303[HI])
                0.292628_fp, 0.308060_fp, 0.336465_fp, 0.351940_fp, &     ! AMSU-A1 ch12 (pp.315[LO],316[HI])
                0.308343_fp, 0.316152_fp, 0.328359_fp, 0.335223_fp, &     ! AMSU-A1 ch13 (pp.328[LO],329[HI])
                0.316276_fp, 0.319209_fp, 0.325346_fp, 0.328300_fp, &     ! AMSU-A1 ch14 (pp.341[LO],342[HI])
                0.48978_fp,  1.48805_fp,  ZERO,             ZERO    /), & ! AMSU-A1 ch15 (pg.208)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------
  ! MetOp-B MHS
  ! -----------

  ! MHS central frequencies in GHz.
  ! Instrument serial number: 104-MHS
  ! Data Source Reference: MHS.TR.JA267.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MHS_CHANNELS ) :: MHS_METOPB_F0 = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 104-MHS
  ! Data Source Reference: MHS.TR.JA267.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) :: MHS_METOPB_IF_BAND = &
    RESHAPE( (/ 0.113_fp, 1.202_fp, ZERO, ZERO, &    ! MHS ch16
                0.112_fp, 1.205_fp, ZERO, ZERO, &    ! MHS ch17
                0.748_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch18
                2.517_fp, 3.362_fp, ZERO, ZERO, &    ! MHS ch19
                0.112_fp, 1.077_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  ! --------------
  ! MetOp-C AMSU-A
  ! --------------

  ! AMSU-A central frequencies in GHz
  ! AMSU-A1 data (ch3-15) interpolated from measured data to 15C temperature
  ! AMSU-A1 ch9-14 values is for PLLO #1. PLLO #2 value is 57.290334GHz
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_AMSUA_CHANNELS ) :: AMSUA_METOPC_F0 = &
    (/ 23.800558_fp, 31.402120_fp, 50.300397_fp, &
       52.799710_fp, 53.596297_fp, 54.399751_fp, &
       54.940644_fp, 55.499839_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 57.290335_fp, &
       57.290335_fp, 57.290335_fp, 89.000800_fp /)

  ! AMSU-A I/F band limits in GHz. These are the 3dB bandpass
  ! filter frequencies from the AMSU-A2 and AMSU-A1 Aerojet
  ! calibration logbooks for AMSU-A1 S/N 105 (report #11322)
  ! and AMSU-A2 S/N 107 (report #11327). These are the 15C values.
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS ) :: AMSUA_METOPC_IF_BAND = &
    RESHAPE( (/ 0.00860_fp,  0.13412_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch1 (pg.103)
                0.00906_fp,  0.08922_fp,  ZERO,             ZERO,   &     ! AMSU-A2 ch2 (pg.114)
                0.00884_fp,  0.08928_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch3 (pg.195)
                0.00920_fp,  0.19904_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch4 (pg.105)
                0.03134_fp,  0.19905_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch5 (pg.195)
                0.00914_fp,  0.19919_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch6 (pg.195)
                0.00917_fp,  0.19926_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch7 (pg.195)
                0.00907_fp,  0.16406_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch8 (pg.195)
                0.00911_fp,  0.16415_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch9 (pg.195)
                0.17887_fp,  0.25511_fp,  ZERO,             ZERO,   &     ! AMSU-A1 ch10 (pg.195)
                0.257070_fp, 0.291921_fp, 0.352406_fp, 0.387740_fp, &     ! AMSU-A1 ch11 (pp.196)
                0.292527_fp, 0.307953_fp, 0.336374_fp, 0.351850_fp, &     ! AMSU-A1 ch12 (pp.196)
                0.308370_fp, 0.316193_fp, 0.328303_fp, 0.336182_fp, &     ! AMSU-A1 ch13 (pp.196)
                0.316245_fp, 0.319180_fp, 0.325187_fp, 0.328350_fp, &     ! AMSU-A1 ch14 (pp.196)
                0.49298_fp,  1.48667_fp,  ZERO,             ZERO    /), & ! AMSU-A1 ch15 (pg.195)
             (/ 2, MAX_N_SIDEBANDS, N_AMSUA_CHANNELS /) )


  ! -----------
  ! MetOp-C MHS
  ! -----------

  ! MHS central frequencies in GHz.
  ! Instrument serial number: 105-MHS
  ! Data Source Reference: MHS.TR.JA377.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( N_MHS_CHANNELS ) :: MHS_METOPC_F0 = &
    (/  89.000_fp, 157.000_fp, 183.311_fp, &
       183.311_fp, 190.311_fp /)


  ! MHS I/F band limits in GHz.
  ! Instrument serial number: 105-MHS
  ! Data Source Reference: MHS.TR.JA377.MMP
  REAL(fp), PRIVATE, PARAMETER, &
            DIMENSION( 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS ) :: MHS_METOPC_IF_BAND = &
    RESHAPE( (/ 0.115_fp, 1.209_fp, ZERO, ZERO, &    ! MHS ch16
                0.111_fp, 1.218_fp, ZERO, ZERO, &    ! MHS ch17
                0.743_fp, 1.215_fp, ZERO, ZERO, &    ! MHS ch18
                2.485_fp, 3.358_fp, ZERO, ZERO, &    ! MHS ch19
                0.114_fp, 1.078_fp, ZERO, ZERO /), & ! MHS ch20
             (/ 2, MAX_N_SIDEBANDS, N_MHS_CHANNELS /) )


  !#----------------------------------------------------------------------------#
  !#                   -- MICROWAVE SENSOR POLARIZATION DATA --                 #
  !#                                                                            #
  !#  Parameter definitions inherited from the SPCCOEFF_DEFINE module. Note     #
  !#  that UNPOLARIZED, INTENSITY, and FIRST_STOKES_COMPONENT all refer to the  #
  !#  same polarization state; that is, Stokes vector of [1,0,0,0]              #
  !#                                                                            #
  !#  Unpolarized                                   : UNPOLARIZED               #
  !#  Intensity                                     : INTENSITY                 #
  !#  First Stokes component (I; R(h) + R(v))       : FIRST_STOKES_COMPONENT    #
  !#  Second Stokes component (Q; R(h) - R(v))      : SECOND_STOKES_COMPONENT   #
  !#  Third Stokes component (U; R(+45) - R(-45))   : THIRD_STOKES_COMPONENT    #
  !#  Fourth Stokes component (V; R(rc) - R(lc))    : FOURTH_STOKES_COMPONENT   #
  !#  Vertical linear polarization                  : VL_POLARIZATION           #
  !#  Horizontal linear polarization                : HL_POLARIZATION           #
  !#  +45deg. linear polarization                   : plus45L_POLARIZATION      #
  !#  -45deg. linear polarization                   : minus45L_POLARIZATION     #
  !#  Vert. polarization at nadir; mixed off nadir  : VL_MIXED_POLARIZATION     #
  !#  Horiz. polarization at nadir; mixed off nadir : HL_MIXED_POLARIZATION     #
  !#  Right circular polarization                   : RC_POLARIZATION           #
  !#  Left circular polarization                    : LC_POLARIZATION           #
  !#----------------------------------------------------------------------------#

  ! --------------
  ! NOAA-5->14 MSU
  ! --------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MSU_CHANNELS ) :: &
    MSU_POLARIZATION = (/ VL_MIXED_POLARIZATION, &  ! MSU ch1
                          HL_MIXED_POLARIZATION, &  ! MSU ch2
                          VL_MIXED_POLARIZATION, &  ! MSU ch3
                          HL_MIXED_POLARIZATION /)  ! MSU ch4
                          

  ! ------
  ! AMSU-A
  ! ------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUA_CHANNELS ) :: &
    AMSUA_POLARIZATION = (/ VL_MIXED_POLARIZATION, &  ! AMSU-A2 ch1
                            VL_MIXED_POLARIZATION, &  ! AMSU-A2 ch2
                            VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch3
                            VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch4
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch5
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch6
                            VL_MIXED_POLARIZATION, &  ! AMSU-A1 ch7
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch8
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch9
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch10
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch11
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch12
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch13
                            HL_MIXED_POLARIZATION, &  ! AMSU-A1 ch14
                            VL_MIXED_POLARIZATION /)  ! AMSU-A1 ch15

  ! ------
  ! AMSU-B
  ! ------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSUB_CHANNELS ) :: &
    AMSUB_POLARIZATION = (/ VL_MIXED_POLARIZATION, &  ! AMSU-B ch16
                            VL_MIXED_POLARIZATION, &  ! AMSU-B ch17
                            VL_MIXED_POLARIZATION, &  ! AMSU-B ch18
                            VL_MIXED_POLARIZATION, &  ! AMSU-B ch19
                            VL_MIXED_POLARIZATION /)  ! AMSU-B ch20

  ! -----
  ! SSM/I
  ! -----

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMI_CHANNELS ) :: &
    SSMI_POLARIZATION = (/ VL_POLARIZATION, &  ! SSM/I ch1
                           HL_POLARIZATION, &  ! SSM/I ch2
                           VL_POLARIZATION, &  ! SSM/I ch3
                           VL_POLARIZATION, &  ! SSM/I ch4
                           HL_POLARIZATION, &  ! SSM/I ch5
                           VL_POLARIZATION, &  ! SSM/I ch6
                           HL_POLARIZATION /)  ! SSM/I ch7

  ! -------
  ! SSM/T-1
  ! -------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT1_CHANNELS ) :: &
    SSMT1_POLARIZATION = (/ VL_POLARIZATION, &  ! SSM/T-1 ch1
                            HL_POLARIZATION, &  ! SSM/T-1 ch2
                            VL_POLARIZATION, &  ! SSM/T-1 ch3
                            VL_POLARIZATION, &  ! SSM/T-1 ch4
                            HL_POLARIZATION, &  ! SSM/T-1 ch5
                            VL_POLARIZATION, &  ! SSM/T-1 ch6
                            HL_POLARIZATION /)  ! SSM/T-1 ch7

  ! -------
  ! SSM/T-2
  ! -------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMT2_CHANNELS ) :: &
    SSMT2_POLARIZATION = (/ HL_POLARIZATION, &  ! SSM/T-2 ch1
                            HL_POLARIZATION, &  ! SSM/T-2 ch2
                            HL_POLARIZATION, &  ! SSM/T-2 ch3
                            HL_POLARIZATION, &  ! SSM/T-2 ch4
                            HL_POLARIZATION /)  ! SSM/T-2 ch5

  ! -------------
  ! DMSP-16 SSMIS
  ! -------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_SSMIS_CHANNELS ) :: &
    SSMIS_POLARIZATION = (/ VL_POLARIZATION, &  ! SSMIS ch1
                            VL_POLARIZATION, &  ! SSMIS ch2
                            VL_POLARIZATION, &  ! SSMIS ch3
                            VL_POLARIZATION, &  ! SSMIS ch4
                            VL_POLARIZATION, &  ! SSMIS ch5
                            RC_POLARIZATION, &  ! SSMIS ch6
                            RC_POLARIZATION, &  ! SSMIS ch7
                            HL_POLARIZATION, &  ! SSMIS ch8
                            HL_POLARIZATION, &  ! SSMIS ch9
                            HL_POLARIZATION, &  ! SSMIS ch10
                            HL_POLARIZATION, &  ! SSMIS ch11
                            HL_POLARIZATION, &  ! SSMIS ch12
                            VL_POLARIZATION, &  ! SSMIS ch13
                            VL_POLARIZATION, &  ! SSMIS ch14
                            HL_POLARIZATION, &  ! SSMIS ch15
                            VL_POLARIZATION, &  ! SSMIS ch16
                            VL_POLARIZATION, &  ! SSMIS ch17
                            HL_POLARIZATION, &  ! SSMIS ch18
                            RC_POLARIZATION, &  ! SSMIS ch19
                            RC_POLARIZATION, &  ! SSMIS ch20
                            RC_POLARIZATION, &  ! SSMIS ch21
                            RC_POLARIZATION, &  ! SSMIS ch22
                            RC_POLARIZATION, &  ! SSMIS ch23
                            RC_POLARIZATION /)  ! SSMIS ch24


  ! -------------------------------
  ! Aqua HSB (assumed same as NOAA)
  ! -------------------------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_HSB_CHANNELS ) :: &
    HSB_AQUA_POLARIZATION =  (/ VL_MIXED_POLARIZATION, &  ! HSB ch1 == AMSU-B ch2
                                VL_MIXED_POLARIZATION, &  ! HSB ch2 == AMSU-B ch3
                                VL_MIXED_POLARIZATION, &  ! HSB ch3 == AMSU-B ch4
                                VL_MIXED_POLARIZATION /)  ! HSB ch4 == AMSU-B ch5


  ! -----------
  ! Aqua AMSR-E
  ! -----------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_AMSRE_CHANNELS ) :: &
    AMSRE_AQUA_POLARIZATION = (/ VL_POLARIZATION, &  ! AMSR-E ch1
                                 HL_POLARIZATION, &  ! AMSR-E ch2
                                 VL_POLARIZATION, &  ! AMSR-E ch3
                                 HL_POLARIZATION, &  ! AMSR-E ch4
                                 VL_POLARIZATION, &  ! AMSR-E ch5
                                 HL_POLARIZATION, &  ! AMSR-E ch6
                                 VL_POLARIZATION, &  ! AMSR-E ch7
                                 HL_POLARIZATION, &  ! AMSR-E ch8
                                 VL_POLARIZATION, &  ! AMSR-E ch9
                                 HL_POLARIZATION, &  ! AMSR-E ch10
                                 VL_POLARIZATION, &  ! AMSR-E ch11
                                 HL_POLARIZATION /)  ! AMSR-E ch12


  ! ------------------------
  ! MHS (needs verification)
  ! ------------------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_MHS_CHANNELS ) :: &
    MHS_POLARIZATION = (/ VL_MIXED_POLARIZATION, &  ! MHS ch16
                          VL_MIXED_POLARIZATION, &  ! MHS ch17
                          HL_MIXED_POLARIZATION, &  ! MHS ch18
                          HL_MIXED_POLARIZATION, &  ! MHS ch19
                          VL_MIXED_POLARIZATION /)  ! MHS ch20


  ! ----------------
  ! Coriolis WindSat
  ! ----------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_WINDSAT_CHANNELS ) :: &
    WINDSAT_CORIOLIS_POLARIZATION =  (/ VL_POLARIZATION,         &  ! WindSat ch1,   6.8GHz R(v)
                                        HL_POLARIZATION,         &  ! WindSat ch2,   6.8GHz R(h)

                                        VL_POLARIZATION,         &  ! WindSat ch3,  10.7GHz R(v)
                                        HL_POLARIZATION,         &  ! WindSat ch4,  10.7GHz R(h)
                                        THIRD_STOKES_COMPONENT,  &  ! WindSat ch5,  10.7GHz R(+45) - R(-45)
                                        FOURTH_STOKES_COMPONENT, &  ! WindSat ch6,  10.7GHz R(rc) - R(lc)

                                        VL_POLARIZATION,         &  ! WindSat ch7,  18.7GHz R(v)
                                        HL_POLARIZATION,         &  ! WindSat ch8,  18.7GHz R(h)
                                        THIRD_STOKES_COMPONENT,  &  ! WindSat ch9,  18.7GHz R(+45) - R(-45)
                                        FOURTH_STOKES_COMPONENT, &  ! WindSat ch10, 18.7GHz R(rc) - R(lc)

                                        VL_POLARIZATION,         &  ! WindSat ch11, 23.8GHz vertical pol.
                                        HL_POLARIZATION,         &  ! WindSat ch12, 23.8GHz horizontal pol.

                                        VL_POLARIZATION,         &  ! WindSat ch13, 37.0GHz R(v)
                                        HL_POLARIZATION,         &  ! WindSat ch14, 37.0GHz R(h)
                                        THIRD_STOKES_COMPONENT,  &  ! WindSat ch15, 37.0GHz R(+45) - R(-45)
                                        FOURTH_STOKES_COMPONENT /)  ! WindSat ch16, 37.0GHz R(rc) - R(lc)  

  ! --------------
  ! NPOESS-C1 ATMS
  ! --------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ATMS_CHANNELS ) :: &
    ATMS_C1_POLARIZATION = (/ VL_MIXED_POLARIZATION, &  ! ATMS ch1
                              VL_MIXED_POLARIZATION, &  ! ATMS ch2
                              HL_MIXED_POLARIZATION, &  ! ATMS ch3
                              HL_MIXED_POLARIZATION, &  ! ATMS ch4
                              HL_MIXED_POLARIZATION, &  ! ATMS ch5
                              HL_MIXED_POLARIZATION, &  ! ATMS ch6
                              HL_MIXED_POLARIZATION, &  ! ATMS ch7
                              HL_MIXED_POLARIZATION, &  ! ATMS ch8
                              HL_MIXED_POLARIZATION, &  ! ATMS ch9
                              HL_MIXED_POLARIZATION, &  ! ATMS ch10
                              HL_MIXED_POLARIZATION, &  ! ATMS ch11
                              HL_MIXED_POLARIZATION, &  ! ATMS ch12
                              HL_MIXED_POLARIZATION, &  ! ATMS ch13
                              HL_MIXED_POLARIZATION, &  ! ATMS ch14
                              HL_MIXED_POLARIZATION, &  ! ATMS ch15
                              VL_MIXED_POLARIZATION, &  ! ATMS ch16
                              HL_MIXED_POLARIZATION, &  ! ATMS ch17
                              HL_MIXED_POLARIZATION, &  ! ATMS ch18
                              HL_MIXED_POLARIZATION, &  ! ATMS ch19
                              HL_MIXED_POLARIZATION, &  ! ATMS ch20
                              HL_MIXED_POLARIZATION, &  ! ATMS ch21
                              HL_MIXED_POLARIZATION /)  ! ATMS ch22


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!
! Clear_MW_SensorData
!
! Subroutine to clear the scalar members of a MW_SensorData structure.
!
! Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                 paul.vandelst@ssec.wisc.edu

  SUBROUTINE Clear_MW_SensorData( MW_SensorData )
    TYPE(MW_SensorData_type), INTENT(IN OUT) :: MW_SensorData
    MW_SensorData%n_Frequencies = 0
    MW_SensorData%n_Channels    = 0
    MW_SensorData%NCEP_Sensor_ID   = INVALID_NCEP_SENSOR_ID  
    MW_SensorData%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    MW_SensorData%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
  END SUBROUTINE Clear_MW_SensorData




!
! Associated_MW_SensorData
!
! Function to test the association status of the pointer members of a
! MW_SensorData structure.
!
!
! Written by:     Paul van Delst, CIMSS/SSEC 20-Feb-2004
!                 paul.vandelst@ssec.wisc.edu

  FUNCTION Associated_MW_SensorData( MW_SensorData, & ! Input
                                     ANY_Test )     & ! Optional input
                                   RESULT( Association_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN) :: MW_SensorData
    INTEGER,          OPTIONAL, INTENT(IN) :: ANY_Test

    ! Function result
    LOGICAL :: Association_Status

    ! Local variables
    LOGICAL :: ALL_Test



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.

    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE POINTER MEMBER ASSOCIATION --            #
    !#--------------------------------------------------------------------------#

    Association_Status = .FALSE.

    IF ( ALL_Test ) THEN

      IF ( ASSOCIATED( MW_SensorData%Sensor_Channel    ) .AND. &
           ASSOCIATED( MW_SensorData%Central_Frequency ) .AND. &
           ASSOCIATED( MW_SensorData%Polarization      ) .AND. &
           ASSOCIATED( MW_SensorData%n_Sidebands       ) .AND. &
           ASSOCIATED( MW_SensorData%IF_Band           ) .AND. &
           ASSOCIATED( MW_SensorData%Delta_Frequency   ) .AND. &
           ASSOCIATED( MW_SensorData%Frequency         ) .AND. &
           ASSOCIATED( MW_SensorData%Response          )       ) THEN
        Association_Status = .TRUE.
      END IF

    ELSE

      IF ( ASSOCIATED( MW_SensorData%Sensor_Channel    ) .OR. &
           ASSOCIATED( MW_SensorData%Central_Frequency ) .OR. &
           ASSOCIATED( MW_SensorData%Polarization      ) .OR. &
           ASSOCIATED( MW_SensorData%n_Sidebands       ) .OR. &
           ASSOCIATED( MW_SensorData%IF_Band           ) .OR. &
           ASSOCIATED( MW_SensorData%Delta_Frequency   ) .OR. &
           ASSOCIATED( MW_SensorData%Frequency         ) .OR. &
           ASSOCIATED( MW_SensorData%Response          )      ) THEN
        Association_Status = .TRUE.
      END IF

    END IF

  END FUNCTION Associated_MW_SensorData





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!
! Destroy_MW_SensorData
! 
! Function to re-initialize the scalar and pointer members of
! MW_SensorData data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_MW_SensorData( MW_SensorData,            &  ! Output
!                                             RCS_Id = RCS_Id,          &  ! Optional output
!                                             Message_Log = Message_Log )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       MW_SensorData: Re-initialized MW_SensorData structure.
!                      UNITS:      N/A
!                      TYPE:       MW_SensorData_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the ERROR_HANDLER module.
!                      If == SUCCESS the structure re-initialisation was successful
!                         == FAILURE - an error occurred, or
!                                    - the structure internal allocation counter
!                                      is not equal to zero (0) upon exiting this
!                                      function. This value is incremented and
!                                      decremented for every structure allocation
!                                      and deallocation respectively.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!
! Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                 paul.vandelst@ssec.wisc.edu

  FUNCTION Destroy_MW_SensorData( MW_SensorData, &  ! Output
                                  No_Clear,      &  ! Optional input
                                  RCS_Id,        &  ! Optional output
                                  Message_Log )  &  ! Error messaging
                                RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN OUT) :: MW_SensorData
    INTEGER,        OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Destroy_MW_SensorData'

    ! Local variables
    CHARACTER( 256 ) :: message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == SET ) Clear = .FALSE.
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM INITIALISATION --                       #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the scalar members
    ! -----------------------------

    IF ( Clear ) CALL Clear_MW_SensorData( MW_SensorData )


    ! -----------------------------------------------------
    ! If ALL pointer members are NOT associated, do nothing
    ! -----------------------------------------------------

    IF ( .NOT. Associated_MW_SensorData( MW_SensorData ) ) RETURN


    ! ------------------------------
    ! Deallocate the pointer members
    ! ------------------------------

    ! Deallocate the sensor channel number array
    IF ( ASSOCIATED( MW_SensorData%Sensor_Channel ) ) THEN

      DEALLOCATE( MW_SensorData%Sensor_Channel, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData Sensor_Channel ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! Deallocate the central frequencies
    IF ( ASSOCIATED( MW_SensorData%Central_Frequency ) ) THEN

      DEALLOCATE( MW_SensorData%Central_Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData central frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! Deallocate the polarization type
    IF ( ASSOCIATED( MW_SensorData%Polarization ) ) THEN

      DEALLOCATE( MW_SensorData%Polarization, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData polarization ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the number of sidebands array
    IF ( ASSOCIATED( MW_SensorData%n_Sidebands ) ) THEN

      DEALLOCATE( MW_SensorData%n_Sidebands, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData n_Sidebands ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the intermediate frequency array
    IF ( ASSOCIATED( MW_SensorData%IF_Band ) ) THEN

      DEALLOCATE( MW_SensorData%IF_Band, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData IF_Band ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the delta frequency array
    IF ( ASSOCIATED( MW_SensorData%Delta_Frequency ) ) THEN

      DEALLOCATE( MW_SensorData%Delta_Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData Delta_Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the frequency grid array
    IF ( ASSOCIATED( MW_SensorData%Frequency ) ) THEN

      DEALLOCATE( MW_SensorData%Frequency, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData Frequency ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF

    ! Deallocate the response grid array
    IF ( ASSOCIATED( MW_SensorData%Response ) ) THEN

      DEALLOCATE( MW_SensorData%Response, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( message, '( "Error deallocating MW_SensorData Response ", &
                          &"member. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- DECREMENT AND TEST ALLOCATION COUNTER --                #
    !#--------------------------------------------------------------------------#

    MW_SensorData%n_Allocates = MW_SensorData%n_Allocates - 1

    IF ( MW_SensorData%n_Allocates /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      MW_SensorData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Destroy_MW_SensorData





!
! Allocate_MW_SensorData
! 
! Function to allocate the pointer members of the MW_SensorData
! data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_MW_SensorData( n_Channels,                &  ! Input
!                                              MW_SensorData,             &  ! Output
!                                              RCS_Id      = RCS_Id,      &  ! Optional output
!                                              Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       n_Channels:         Number of channels dimension.
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in
!                           which any messages will be logged. If not
!                           specified, or if an error occurs opening
!                           the log file, the default action is to
!                           output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       MW_SensorData:      MW_SensorData structure with allocated
!                           pointer members
!                           UNITS:      N/A
!                           TYPE:       MW_SensorData_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the ERROR_HANDLER module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                 paul.vandelst@ssec.wisc.edu

  FUNCTION Allocate_MW_SensorData( n_Channels,    &  ! Input
                                   MW_SensorData, &  ! Output
                                   RCS_Id,        &  ! Optional output
                                   Message_Log )  &  ! Error messaging
                                 RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    INTEGER,                  INTENT(IN)     :: n_Channels
    TYPE(MW_SensorData_type), INTENT(IN OUT) :: MW_SensorData
    CHARACTER(*),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Allocate_MW_SensorData'

    ! Local variables
    CHARACTER( 256 ) :: message
    INTEGER :: Allocate_Status



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( n_Channels  < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input MW_SensorData channel dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------------------------
    ! Check if ANY pointers are already associated. If so,
    ! deallocate the arrays, but leave the scalars alone.
    ! ----------------------------------------------------

    IF ( Associated_MW_SensorData( MW_SensorData, ANY_Test = SET ) ) THEN

      Error_Status = Destroy_MW_SensorData( MW_SensorData, &
                                            No_Clear = SET, &
                                            Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME,    &
                              'Error deallocating MW_SensorData pointer members.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ALLOCATION --                       #
    !#--------------------------------------------------------------------------#

    ALLOCATE( MW_SensorData%Sensor_Channel( n_Channels ), &
              MW_SensorData%Central_Frequency( n_Channels ), &
              MW_SensorData%Polarization( n_Channels ), &
              MW_SensorData%n_Sidebands( n_Channels ), &
              MW_SensorData%IF_Band( 2, MAX_N_SIDEBANDS, n_Channels ), &
              MW_SensorData%Delta_Frequency( n_Channels ), &
              MW_SensorData%Frequency( N_FREQUENCIES, n_Channels ), &
              MW_SensorData%Response( N_FREQUENCIES, n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( message, '( "Error allocating MW_SensorData data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE DIMENSION AND INITALISE ARRAYS --              #
    !#--------------------------------------------------------------------------#

    MW_SensorData%n_Frequencies = N_FREQUENCIES
    MW_SensorData%n_Channels    = n_Channels

    MW_SensorData%Sensor_Channel    = INVALID
    MW_SensorData%Central_Frequency = REAL( INVALID, fp )
    MW_SensorData%Polarization      = INVALID_POLARIZATION
    MW_SensorData%n_Sidebands       = INVALID
    MW_SensorData%IF_Band           = REAL( INVALID, fp )
    MW_SensorData%Delta_Frequency   = REAL( INVALID, fp )
    MW_SensorData%Frequency         = REAL( INVALID, fp )
    MW_SensorData%Response          = REAL( INVALID, fp )



    !#--------------------------------------------------------------------------#
    !#                -- INCREMENT AND TEST ALLOCATION COUNTER --               #
    !#--------------------------------------------------------------------------#

    MW_SensorData%n_Allocates = MW_SensorData%n_Allocates + 1

    IF ( MW_SensorData%n_Allocates /= 1 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      MW_SensorData%n_Allocates
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Allocate_MW_SensorData




!
! Assign_MW_SensorData
!
! Function to copy valid MW_SensorData structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_MW_SensorData( MW_SensorData_in,         &  ! Input
!                                            MW_SensorData_out,        &  ! Output
!                                            RCS_Id = RCS_Id,          &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       MW_SensorData_in:  MW_SensorData structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       MW_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       MW_SensorData_out: Copy of the input structure, MW_SensorData_in.
!                          UNITS:      N/A
!                          TYPE:       MW_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                 paul.vandelst@ssec.wisc.edu

  FUNCTION Assign_MW_SensorData( MW_SensorData_in,  &  ! Input
                                 MW_SensorData_out, &  ! Output
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN)     :: MW_SensorData_in
    TYPE(MW_SensorData_type), INTENT(IN OUT) :: MW_SensorData_out
    CHARACTER( * ), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER( * ), OPTIONAL, INTENT(IN)     :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Assign_MW_SensorData'



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
     END IF



    !#--------------------------------------------------------------------------#
    !#           -- TEST THE STRUCTURE ARGUMENT POINTER ASSOCIATION --          #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------
    ! ALL *input* pointers must be associated.
    ! If this test fails, the assumption is that
    ! ALL of the pointers are NOT associated,
    ! not just some of them. Thus, do nothing.
    ! ------------------------------------------

    IF ( .NOT. Associated_MW_SensorData( MW_SensorData_In ) ) RETURN



    !#--------------------------------------------------------------------------#
    !#                       -- PERFORM THE ASSIGNMENT --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_MW_SensorData( MW_SensorData_in%n_Channels, &
                                           MW_SensorData_out, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating output MW_SensorData arrays.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Assign scalar members
    ! ---------------------

    MW_SensorData_out%NCEP_Sensor_ID   = MW_SensorData_in%NCEP_Sensor_ID
    MW_SensorData_out%WMO_Satellite_ID = MW_SensorData_in%WMO_Satellite_ID
    MW_SensorData_out%WMO_Sensor_ID    = MW_SensorData_in%WMO_Sensor_ID


    ! -----------------
    ! Assign array data
    ! -----------------

    MW_SensorData_out%Sensor_Channel   = MW_SensorData_in%Sensor_Channel

    MW_SensorData_out%Central_Frequency = MW_SensorData_in%Central_Frequency
    MW_SensorData_out%Polarization      = MW_SensorData_in%Polarization

    MW_SensorData_out%n_Sidebands = MW_SensorData_in%n_Sidebands
    MW_SensorData_out%IF_Band     = MW_SensorData_in%IF_Band

    MW_SensorData_out%Delta_Frequency = MW_SensorData_in%Delta_Frequency
    MW_SensorData_out%Frequency       = MW_SensorData_in%Frequency
    MW_SensorData_out%Response        = MW_SensorData_in%Response 

  END FUNCTION Assign_MW_SensorData





!
! Function to allocate and load an MW_SensorData structure based on
! input sensor ID information.
!
! CALLING SEQUENCE:
!       Error_Status = Load_MW_SensorData( MW_SensorData,                       &  ! Output
!                                          NCEP_Sensor_ID   = NCEP_Sensor_ID,   &  ! Optional Input
!                                          WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional Input
!                                          WMO_Sensor_ID    = WMO_Sensor_ID,    &  ! Optional Input
!                                          RCS_Id = RCS_Id,                     &  ! Revision control
!                                          Message_Log = Message_Log )             ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       NCEP_Sensor_ID:    An "in-house" value used at NOAA/NCEP/EMC 
!                          to identify a satellite/sensor combination.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_ID:  The WMO code for identifying satellite
!                          platforms. Taken from the WMO common
!                          code tables at:
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          The Satellite ID is from Common Code
!                          table C-5, or code table 0 01 007 in BUFR.
!                          - This argument is ignored if the NCEP_Sensor_ID
!                            argument is passed.
!                          - If this argument is used, the WMO_Sensor_ID
!                            *must* be passed also.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_ID:     The WMO code for identifying a satelite
!                          sensor. Taken from the WMO common
!                          code tables at:
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          The Sensor ID is from Common Code
!                          table C-8, or code table 0 02 019 in BUFR
!                          - This argument is ignored if the NCEP_Sensor_ID
!                            argument is passed.
!                          - If this argument is used, the WMO_Satellite_ID
!                            *must* be passed also.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       MW_SensorData:     MW_SensorData structure containing the required
!                          sensor frequency data.
!                          UNITS:      N/A
!                          TYPE:       MW_SensorData_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the data load was successful
!                             == FAILURE an error occurred
!                             == WARNING nothing was done due to lack
!                                of user input
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!
! Written by:     Paul van Delst, CIMSS/SSEC 03-Jan-2003
!                 paul.vandelst@ssec.wisc.edu

  FUNCTION Load_MW_SensorData( MW_SensorData,    &  ! Output
                               NCEP_Sensor_ID,   &  ! Optional Input
                               WMO_Satellite_ID, &  ! Optional Input
                               WMO_Sensor_ID,    &  ! Optional Input
                               RCS_Id,           &  ! Revision control
                               Message_Log )     &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN OUT) :: MW_SensorData
    INTEGER,          OPTIONAL, INTENT(IN)     :: NCEP_Sensor_ID
    INTEGER,          OPTIONAL, INTENT(IN)     :: WMO_Satellite_ID
    INTEGER,          OPTIONAL, INTENT(IN)     :: WMO_Sensor_ID
    CHARACTER( * ),   OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER( * ),   OPTIONAL, INTENT(IN)     :: Message_Log

    ! Function result
    INTEGER :: Error_Status

    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Load_MW_SensorData'

    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: Sensor_ID
    INTEGER :: n_Channels
    INTEGER :: i, n
    INTEGER, DIMENSION( 1 ) :: idx
    INTEGER, DIMENSION( 2 ) :: n_Points
    INTEGER :: n_OffsetPoints
    INTEGER :: l, ln
    INTEGER :: i_Upper, i_Lower
    REAL(fp) :: df, f1, f



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK OPTIONAL INPUT ARGUMENTS --                   #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Were *any* passed?
    ! ------------------

    IF ( ( .NOT. PRESENT( NCEP_Sensor_ID   ) ) .AND. &
         ( .NOT. ( PRESENT( WMO_Satellite_ID ) .AND. &
                   PRESENT( WMO_Sensor_ID    )       ) ) ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'No sensor ID arguments passed. Nothing done.', &
                            Error_Status, &
                            MEssage_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- DETERMINE THE SENSOR ID --                       #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( NCEP_Sensor_ID ) ) THEN


      ! --------------------------------------
      ! Use the NCEP Sensor ID if available...
      ! --------------------------------------

      ! Check it's valid
      n = COUNT( VALID_NCEP_SENSOR_ID == NCEP_Sensor_ID )

      IF ( n == 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Specified NCEP sensor ID not found in valid list.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
 
      ! Determine the valid sensor ID index
      idx = PACK( (/ ( i, i = 1, N_VALID_SENSORS ) /), &
                  VALID_NCEP_SENSOR_ID == NCEP_Sensor_ID )

    ELSE


      ! ----------------------------
      ! ...otherwise use the WMO IDs
      ! ----------------------------

      ! Check if the IDs are valid
      n = COUNT( ( VALID_WMO_SATELLITE_ID == WMO_Satellite_ID ) .AND. &
                 ( VALID_WMO_SENSOR_ID    == WMO_Sensor_ID    )       )

      IF ( n == 0 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Specified WMO IDs not found in valid list.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! Determine the valid sensor ID index
      idx = PACK( (/ ( i, i = 1, N_VALID_SENSORS ) /), &
                  ( VALID_WMO_SATELLITE_ID == WMO_Satellite_ID ) .AND. &
                  ( VALID_WMO_SENSOR_ID    == WMO_Sensor_ID    )       )

    END IF


    ! -------------------------------------------
    ! Assign the sensor ID and number of channels
    ! -------------------------------------------

    Sensor_ID  = VALID_NCEP_SENSOR_ID( idx(1) )
    n_Channels = VALID_N_CHANNELS( idx(1) )



    !#--------------------------------------------------------------------------#
    !#                -- ALLOCATE THE MW_SensorData STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_MW_SensorData( n_Channels, &
                                           MW_SensorData, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating MW_SensorData structure..', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! Assign the sensor ID info
    ! -------------------------

    MW_SensorData%NCEP_Sensor_ID   = Sensor_ID
    MW_SensorData%WMO_Satellite_ID = VALID_WMO_SATELLITE_ID( idx(1) )
    MW_SensorData%WMO_Sensor_ID    = VALID_WMO_SENSOR_ID( idx(1) )

    


    !#--------------------------------------------------------------------------#
    !#      -- LOAD THE MW_SensorData STRUCTURE WITH THE FREQUENCY DATA --      #
    !#--------------------------------------------------------------------------#

    Load_Data: SELECT CASE ( Sensor_ID )


      ! ------------------------
      ! The NOAA MSU instruments
      ! ------------------------

      CASE ( 205:212,214 )

        MW_SensorData%Sensor_Channel = MSU_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MSU_F0
        MW_SensorData%Polarization      = MSU_POLARIZATION

        MW_SensorData%n_Sidebands = MSU_N_SIDEBANDS
        MW_SensorData%IF_Band     = MSU_IF_BAND


      ! --------------
      ! NOAA-15 AMSU-A
      ! --------------

      CASE ( 315 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_N15_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_N15_IF_BAND


      ! --------------
      ! NOAA-15 AMSU-B
      ! --------------

      CASE ( 415 )

        MW_SensorData%Sensor_Channel = AMSUB_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUB_N15_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUB_N15_IF_BAND


      ! --------------
      ! NOAA-16 AMSU-A
      ! --------------

      CASE ( 316 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_N16_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_N16_IF_BAND


      ! --------------
      ! NOAA-16 AMSU-B
      ! --------------

      CASE ( 416 )

        MW_SensorData%Sensor_Channel = AMSUB_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUB_N16_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUB_N16_IF_BAND


      ! --------------
      ! NOAA-17 AMSU-A
      ! --------------

      CASE ( 317 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_N17_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_N17_IF_BAND


      ! --------------
      ! NOAA-17 AMSU-B
      ! --------------

      CASE ( 417 )

        MW_SensorData%Sensor_Channel = AMSUB_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUB_N17_F0
        MW_SensorData%Polarization      = AMSUB_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUB_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUB_N17_IF_BAND


     ! --------------------
     ! DMSP-13 to -15 SSM/I
     ! --------------------

      CASE ( 713:715 )

        MW_SensorData%Sensor_Channel = SSMI_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = SSMI_F0
        MW_SensorData%Polarization      = SSMI_POLARIZATION

        MW_SensorData%n_Sidebands = SSMI_N_SIDEBANDS
        MW_SensorData%IF_Band     = SSMI_IF_BAND


     ! ----------------------
     ! DMSP-13 to -15 SSM/T-1
     ! ----------------------

      CASE ( 813:815 )

        MW_SensorData%Sensor_Channel = SSMT1_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = SSMT1_F0
        MW_SensorData%Polarization      = SSMT1_POLARIZATION

        MW_SensorData%n_Sidebands = SSMT1_N_SIDEBANDS
        MW_SensorData%IF_Band     = SSMT1_IF_BAND


     ! ----------------------
     ! DMSP-13 to -15 SSM/T-2
     ! ----------------------

      CASE ( 113:115 )

        MW_SensorData%Sensor_Channel = SSMT2_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = SSMT2_F0
        MW_SensorData%Polarization      = SSMT2_POLARIZATION

        MW_SensorData%n_Sidebands = SSMT2_N_SIDEBANDS
        MW_SensorData%IF_Band     = SSMT2_IF_BAND


     ! -------------
     ! DMSP-16 SSMIS
     ! -------------

      CASE ( 516 )

        MW_SensorData%Sensor_Channel = SSMIS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = SSMIS_F16_F0
        MW_SensorData%Polarization      = SSMIS_POLARIZATION

        MW_SensorData%n_Sidebands = SSMIS_N_SIDEBANDS
        MW_SensorData%IF_Band     = SSMIS_F16_IF_BAND


      ! -----------
      ! Aqua AMSU-A
      ! -----------

      CASE ( 349 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_AQUA_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_AQUA_IF_BAND


      ! --------
      ! Aqua HSB
      ! --------

      CASE ( 449 )

        MW_SensorData%Sensor_Channel = HSB_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = HSB_AQUA_F0
        MW_SensorData%Polarization      = HSB_AQUA_POLARIZATION

        MW_SensorData%n_Sidebands = HSB_N_SIDEBANDS
        MW_SensorData%IF_Band     = HSB_AQUA_IF_BAND


      ! -----------
      ! Aqua AMSR-E
      ! -----------

      CASE ( 549 )

        MW_SensorData%Sensor_Channel = AMSRE_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSRE_AQUA_F0
        MW_SensorData%Polarization      = AMSRE_AQUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSRE_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSRE_AQUA_IF_BAND


      ! --------------
      ! NOAA-18 AMSU-A
      ! --------------

      CASE ( 318 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_N18_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_N18_IF_BAND


      ! -----------
      ! NOAA-18 MHS
      ! -----------

      CASE ( 418 )

        MW_SensorData%Sensor_Channel = MHS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MHS_N18_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION

        MW_SensorData%n_Sidebands = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band     = MHS_N18_IF_BAND


      ! ----------------
      ! Coriolis WindSat
      ! ----------------

      CASE ( 900 )

        MW_SensorData%Sensor_Channel = WINDSAT_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = WINDSAT_CORIOLIS_F0
        MW_SensorData%Polarization      = WINDSAT_CORIOLIS_POLARIZATION

        MW_SensorData%n_Sidebands = WINDSAT_N_SIDEBANDS
        MW_SensorData%IF_Band     = WINDSAT_CORIOLIS_IF_BAND


      ! --------------
      ! NPOESS-C1 ATMS
      ! --------------

      CASE ( 1012 )

        MW_SensorData%Sensor_Channel = ATMS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = ATMS_C1_F0
        MW_SensorData%Polarization      = ATMS_C1_POLARIZATION

        MW_SensorData%n_Sidebands = ATMS_N_SIDEBANDS
        MW_SensorData%IF_Band     = ATMS_C1_IF_BAND


      ! --------------
      ! NOAA-N' AMSU-A
      ! --------------

      CASE ( 319 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_N19_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_N19_IF_BAND


      ! -----------
      ! NOAA-N' MHS
      ! -----------

      CASE ( 419 )

        MW_SensorData%Sensor_Channel = MHS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MHS_N19_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION

        MW_SensorData%n_Sidebands = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band     = MHS_N19_IF_BAND


      ! ----------------------------
      ! MetOp-A (was MetOp-2) AMSU-A
      ! ----------------------------

      CASE ( 320 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_METOPA_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_METOPA_IF_BAND


      ! -------------------------
      ! MetOp-A (was MetOp-2) MHS
      ! -------------------------

      CASE ( 420 )

        MW_SensorData%Sensor_Channel = MHS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MHS_METOPA_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION

        MW_SensorData%n_Sidebands = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band     = MHS_METOPA_IF_BAND


      ! ----------------------------
      ! MetOp-B (was MetOp-1) AMSU-A
      ! ----------------------------

      CASE ( 321 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_METOPB_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_METOPB_IF_BAND


      ! -------------------------
      ! MetOp-B (was MetOp-1) MHS
      ! -------------------------

      CASE ( 421 )

        MW_SensorData%Sensor_Channel = MHS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MHS_METOPB_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION

        MW_SensorData%n_Sidebands = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band     = MHS_METOPB_IF_BAND


      ! ----------------------------
      ! MetOp-C (was MetOp-3) AMSU-A
      ! ----------------------------

      CASE ( 322 )

        MW_SensorData%Sensor_Channel = AMSUA_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = AMSUA_METOPC_F0
        MW_SensorData%Polarization      = AMSUA_POLARIZATION

        MW_SensorData%n_Sidebands = AMSUA_N_SIDEBANDS
        MW_SensorData%IF_Band     = AMSUA_METOPC_IF_BAND


      ! -------------------------
      ! MetOp-C (was MetOp-3) MHS
      ! -------------------------

      CASE ( 422 )

        MW_SensorData%Sensor_Channel = MHS_SENSOR_CHANNEL

        MW_SensorData%Central_Frequency = MHS_METOPC_F0
        MW_SensorData%Polarization      = MHS_POLARIZATION

        MW_SensorData%n_Sidebands = MHS_N_SIDEBANDS
        MW_SensorData%IF_Band     = MHS_METOPC_IF_BAND


      ! --------------------------------
      ! No match! Should never get here!
      ! --------------------------------

      CASE DEFAULT

        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'No sensor ID match!!', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN

    END SELECT Load_Data



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE CHANNEL FREQUENCY RESPONSE GRID --            #
    !#--------------------------------------------------------------------------#

    Channel_Response_Loop: DO l = 1, MW_SensorData%n_Channels


      ! -----------------------------------------------
      ! Compute the number of points in the sideband(s)
      ! -----------------------------------------------

      ! First determine the total frequency range in the sidebands
      df = ZERO
      DO ln = 1, MW_SensorData%n_Sidebands( l )
        df = df + ( MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l) )
      END DO


      ! Now determine the frequency interval for this frequency
      ! range to provide the required number of points. Note that
      ! for > 1 sideband channels, the divisor is n-2, not n-1.
      ! This is to exclude the "space" between the sidebands in
      ! the frequency interval calculation. E.g.:
      ! --
      !       Sideband 1             Sideband 2
      !     |-----------|      |-------------------|
      !     x   x   x   x      x   x   x   x   x   x
      !     1   2   3   4      5   6   7   8   9  10   N_HALFPOINTS (n)
      ! --
      !       1   2   3          4   5   6   7   8     INTERVALS    (n-2)

      IF ( MW_SensorData%n_Sidebands( l ) == 1 ) THEN
        MW_SensorData%Delta_Frequency(l) = df / REAL( N_HALFPOINTS - 1, fp )
      ELSE
        MW_SensorData%Delta_Frequency(l) = df / REAL( N_HALFPOINTS - 2, fp )
      END IF


      ! Now determine the number of points for each sideband.
      ! Note that for single sideband channels, this will
      ! *always* be N_HALFPOINTS. For double sideband channels
      ! the points will be split up according to how broad the
      ! the sidebands are. E.g.: If they are the same width, then
      ! we'll get N_HALFPOINTS/2 points per sideband
      n_Points(:) = 0
      DO ln = 1, MW_SensorData%n_Sidebands( l )
        df = MW_SensorData%IF_Band(2,ln,l) - MW_SensorData%IF_Band(1,ln,l)
        n_Points(ln) = NINT( df / MW_SensorData%Delta_Frequency(l) ) + 1
      END DO


      ! Check the result
      IF ( SUM( n_Points ) /= n_HalfPoints ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error computing n_HalfPoints for channel ", i2, &
                          &" of ", a,1x,a, ". Computed value is ", i5 )' ) &
                        MW_SensorData%Sensor_Channel( l ), &
                        TRIM( VALID_SATELLITE_NAME( Idx(1) ) ), &
                        TRIM( VALID_SENSOR_NAME( Idx(1) ) ), &
                        SUM( n_Points )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        STOP
      END IF


      ! -----------------------------------------------------------
      ! Fill the frequency array. It's a bit convoluted as I
      ! want the frequencies to be in ascending order.
      !
      ! For the first sideband loop:
      !
      !   -Sideband2   -Sideband1    F0     +Sideband1   +Sideband2
      !                               ^
      !    |------|   |----------|    |    |----------|   |------|
      !                               |
      !               |<.........|         |.........>|
      !                  n_Lower             n_Upper
      !
      ! For the second sideband loop:
      !
      !                              F0      Sideband1    Sideband2
      !                               ^
      !    |------|   |----------|    |    |----------|   |------|
      !                               |
      !    |<.....|                                       |.....>|
      !     n_Lower                                        n_Upper
      !
      ! -----------------------------------------------------------

      ! Initialise the sideband point offset. This is the
      ! the number of points offset from the central frequency
      ! for a sideband.
      n_OffsetPoints = 0

      ! Loop over the number of sidebands
      DO ln = 1, MW_SensorData%n_Sidebands(l)

        ! Assign the start intermediate frequency for the sideband
        f1 = MW_SensorData%IF_Band( 1, ln, l )

        ! Loop over the number of points in the sideband
        DO i = 1, n_Points( ln )

          ! Determine the positions of the frequencies in the array
          i_Upper = N_HALFPOINTS + n_OffsetPoints + i
          i_Lower = N_HALFPOINTS - n_OffsetPoints - i + 1

          ! Compute the frequency offset
          f = f1 + ( REAL( i-1, fp ) * MW_SensorData%Delta_Frequency(l) )

          ! Apply the offset to the central frequency
          MW_SensorData%Frequency( i_Upper, l ) = MW_SensorData%Central_Frequency(l) + f
          MW_SensorData%Frequency( i_Lower, l ) = MW_SensorData%Central_Frequency(l) - f

        END DO

        ! Update the number of offset points
        n_OffsetPoints = n_OffsetPoints + n_Points( ln )

      END DO


      ! -----------------------------
      ! The response is assumed unity
      ! -----------------------------

      MW_SensorData%Response(:,l) = ONE

    END DO Channel_Response_Loop

  END FUNCTION Load_MW_SensorData




!  Print_MW_SensorData
!
!  Subroutine to print information contained in the MW_SensorData data
!  structure to stdout.
!
!
!  Written by:     Paul van Delst, CIMSS/SSEC 05-Apr-2004
!                  paul.vandelst@ssec.wisc.edu

  SUBROUTINE Print_MW_SensorData( MW_SensorData ) ! Input



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! Arguments
    TYPE(MW_SensorData_type), INTENT(IN)  :: MW_SensorData

    ! Local parameters
    INTEGER, PARAMETER :: N_HALFBANDS = MAX_N_SIDEBANDS * 2

    ! Local variables
    INTEGER :: Idx
    INTEGER :: i, l
    INTEGER :: iL1, iL2
    INTEGER :: iU1, iU2
    REAL(fp), DIMENSION( 1:N_HALFBANDS*2 ) :: Frequency



    !#--------------------------------------------------------------------------#
    !#                          -- PRINT THE DATA --                            #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Write a header
    ! --------------

    ! Get the satellite/sensor name index
    Idx = MINLOC( ABS( VALID_NCEP_SENSOR_ID - MW_SensorData%NCEP_Sensor_ID ), DIM = 1 )

    ! Output the table header
    WRITE( *, '( //5x, "SensorData for ", a, 1x, a, &
                 &/5x, "NCEP Sensor ID: ", i5, "; WMO Satellite/Sensor IDs: ", 2i5 )' ) &
              TRIM( VALID_SATELLITE_NAME(Idx)), &
              TRIM( VALID_SENSOR_NAME(Idx)), &
              MW_SensorData%NCEP_Sensor_ID, &
              MW_SensorData%WMO_Satellite_ID, &
              MW_SensorData%WMO_Sensor_ID
    WRITE( *, '( 1x, "Ch", 20x, "Polarization", 25x, "f0        f0-f4      f0-f3      f0-f2",&
                        &"      f0-f1          f0+f1      f0+f2      f0+f3      f0+f4" )' )


    ! ----------------------------------
    ! Print information for each channel
    ! ----------------------------------

    Channel_Loop : DO l = 1, MW_SensorData%n_Channels

      ! Fill frequency array with format string overflow values
      Frequency = -9999.0_fp

      ! Loop over sidebands
      Sideband_Loop : DO i = 1, MW_SensorData%n_Sidebands(l)

        ! Define the lower frequency indices
        iL2 = N_HALFBANDS - ( 2*i ) + 1
        iL1 = iL2 + 1

        ! Define the upper frequency indices
        iU1 = N_HALFBANDS + ( 2*i ) - 1
        iU2 = iU1 + 1

        ! Compute the frequencies
        Frequency( iL1:iL2:-1 ) = MW_SensorData%Central_Frequency(l) - MW_SensorData%IF_Band( :, i, l )
        Frequency( iU1:iU2 )    = MW_SensorData%Central_Frequency(l) + MW_SensorData%IF_Band( :, i, l )

      END DO Sideband_Loop

      ! Output the data
      WRITE( *, '( 1x, i2, 2x, a, 2x, f10.6, 4( 1x, f10.6 ), " -|-", 4( 1x, f10.6 ) )' ) &
                MW_SensorData%Sensor_Channel(l), &
                POLARIZATION_TYPE_NAME(MW_SensorData%Polarization(l) ), &
                MW_SensorData%Central_Frequency(l), &
                Frequency

    END DO Channel_Loop

  END SUBROUTINE Print_MW_SensorData


  FUNCTION Get_MW_SensorData_Sensor_ID( Reset ) RESULT( NCEP_Sensor_ID )
    LOGICAL, OPTIONAL, INTENT(IN) :: Reset
    INTEGER :: NCEP_Sensor_ID
    INTEGER, SAVE :: Idx = 0
    IF ( PRESENT( Reset ) ) THEN
      IF ( Reset ) Idx = 0
    END IF
    Idx = Idx + 1
    IF ( Idx > N_VALID_SENSORS ) THEN
      NCEP_Sensor_ID = -1
      RETURN
    END IF
    NCEP_Sensor_ID = VALID_NCEP_SENSOR_ID( Idx )
  END FUNCTION Get_MW_SensorData_Sensor_ID

END MODULE MW_SensorData_Define
