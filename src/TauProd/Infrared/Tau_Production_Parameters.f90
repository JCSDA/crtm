!------------------------------------------------------------------------------
!M+
! NAME:
!       Tau_Production_Parameters
!
! PURPOSE:
!       Module defining parameters used in the LBL transmittance
!       production runs
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Tau_Production_Parameters
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       LBLRTM_Parameters:  Module containing shared parameters required for
!                           LBLRTM format file IO.
!                           USEs: TYPE_KINDS module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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
!M-
!------------------------------------------------------------------------------

MODULE Tau_Production_Parameters


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE LBLRTM_Parameters, ONLY: LBLRTM_FILE_TYPE => LBLRTM_SINGLE_PANEL_TYPE



  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: LBLRTM_FILE_TYPE


  !#----------------------------------------------------------------------------#
  !#                           -- MODULE PARAMETERS --                          #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! Atmospheric profile information
  ! -------------------------------

  ! -- The number of recognised profile sets
  INTEGER, PUBLIC, PARAMETER :: N_PROFILE_SETS = 3

  ! -- The profile set ID tags
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_PROFILE_SETS ) :: &
    PROFILE_SET_ID_TAG = (/ 'UMBC ', &
                            'CIMSS', &
                            'ECMWF' /)

  ! -- The beginning profile is always 1 regardless
  ! -- of the selected profile set. It's an array
  ! -- for conformance only.
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_PROFILE_SETS ) :: &
    PROFILE_BEGIN = (/ 1, & ! For UMBC set
                       1, & ! For CIMSS set
                       1 /) ! For ECMWF set

  ! -- The end profile is set dependent
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_PROFILE_SETS ) :: &
    PROFILE_END = (/ 48, & ! For UMBC set
                     32, & ! For CIMSS set
                     52 /) ! For ECMWF set

  ! -- The number of profiles in each set.
  ! -- This parameter is superfluous (always the same
  ! -- as PROFILE_END), but more intuitive in code.
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_PROFILE_SETS ) :: &
    N_PROFILES = PROFILE_END - PROFILE_BEGIN + 1

  ! -- The number of calculation levels
  INTEGER, PUBLIC, PARAMETER :: N_LEVELS = 101
  INTEGER, PUBLIC, PARAMETER :: N_LAYERS = 100

  ! -- The standard AIRS pressure levels, SRC->TOA
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_LEVELS ) :: &
    LEVEL_PRESSURE = &
    (/ 1100.000000_fp_kind, 1070.916940_fp_kind, 1042.231940_fp_kind, 1013.947655_fp_kind, &
        986.066601_fp_kind,  958.591154_fp_kind,  931.523549_fp_kind,  904.865880_fp_kind, &
        878.620096_fp_kind,  852.788003_fp_kind,  827.371259_fp_kind,  802.371376_fp_kind, &
        777.789716_fp_kind,  753.627494_fp_kind,  729.885772_fp_kind,  706.565460_fp_kind, &
        683.667316_fp_kind,  661.191946_fp_kind,  639.139797_fp_kind,  617.511163_fp_kind, &
        596.306182_fp_kind,  575.524832_fp_kind,  555.166935_fp_kind,  535.232153_fp_kind, &
        515.719989_fp_kind,  496.629785_fp_kind,  477.960722_fp_kind,  459.711821_fp_kind, &
        441.881941_fp_kind,  424.469776_fp_kind,  407.473861_fp_kind,  390.892566_fp_kind, &
        374.724098_fp_kind,  358.966503_fp_kind,  343.617659_fp_kind,  328.675286_fp_kind, &
        314.136936_fp_kind,  300.000000_fp_kind,  286.261706_fp_kind,  272.919120_fp_kind, &
        259.969142_fp_kind,  247.408514_fp_kind,  235.233814_fp_kind,  223.441461_fp_kind, &
        212.027712_fp_kind,  200.988665_fp_kind,  190.320260_fp_kind,  180.018279_fp_kind, &
        170.078348_fp_kind,  160.495939_fp_kind,  151.266366_fp_kind,  142.384796_fp_kind, &
        133.846240_fp_kind,  125.645562_fp_kind,  117.777481_fp_kind,  110.236565_fp_kind, &
        103.017244_fp_kind,   96.113803_fp_kind,   89.520390_fp_kind,   83.231016_fp_kind, &
         77.239560_fp_kind,   71.539768_fp_kind,   66.125259_fp_kind,   60.989530_fp_kind, &
         56.125953_fp_kind,   51.527786_fp_kind,   47.188171_fp_kind,   43.100144_fp_kind, &
         39.256633_fp_kind,   35.650467_fp_kind,   32.274378_fp_kind,   29.121009_fp_kind, &
         26.182918_fp_kind,   23.452583_fp_kind,   20.922408_fp_kind,   18.584732_fp_kind, &
         16.431833_fp_kind,   14.455936_fp_kind,   12.649223_fp_kind,   11.003835_fp_kind, &
          9.511889_fp_kind,    8.165480_fp_kind,    6.956695_fp_kind,    5.877623_fp_kind, &
          4.920362_fp_kind,    4.077038_fp_kind,    3.339812_fp_kind,    2.700897_fp_kind, &
          2.152573_fp_kind,    1.687200_fp_kind,    1.297240_fp_kind,    0.975274_fp_kind, &
          0.714023_fp_kind,    0.506374_fp_kind,    0.345404_fp_kind,    0.224412_fp_kind, &
          0.136954_fp_kind,    0.076879_fp_kind,    0.038383_fp_kind,    0.016065_fp_kind, &
          0.005000_fp_kind /)

  ! -- Default CO2 mixing ratio in ppmv
  REAL( fp_kind ), PUBLIC, PARAMETER :: CO2_MIXING_RATIO = 380.0_fp_kind


  ! ------------------------------------
  ! The frequency calculation parameters
  ! ------------------------------------

  ! -- The begin and end frequencies
  REAL( fp_kind ), PUBLIC, PARAMETER :: FREQUENCY_BEGIN =  500.0_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: FREQUENCY_END   = 3500.0_fp_kind

  ! -- The number of recognised frequency intervals
  INTEGER, PUBLIC, PARAMETER :: N_FREQUENCY_INTERVALS = 2

  ! -- The allowed frequency intervals. These are the values
  ! -- of the frequency spacing of the line-by-line (LBL)
  ! -- band outputs. The lower resolution interval is for
  ! -- broadband instruments, the other for high resolution
  ! -- instruments.
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    FREQUENCY_INTERVAL = (/ 0.1000_fp_kind, &
                            0.0025_fp_kind /)

  ! -- The frequency deltas for a band.
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    FREQUENCY_DELTA = (/ 250.0_fp_kind, &  ! For 0.1cm^-1 spacing
                          25.0_fp_kind /)  ! For 0.0025cm^-1 spacing

  ! -- The frequency bandwidths. These are the bandwidths
  ! -- of the line-by-line (LBL) band outputs.
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    FREQUENCY_BANDWIDTH = FREQUENCY_DELTA - FREQUENCY_INTERVAL

  ! -- The number of points in each band.
  ! -- The values are determined using
  ! --   N_FREQUENCIES = INT( FREQUENCY_BANDWIDTH / FREQUENCY_INTERVAL ) + 1
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    N_FREQUENCIES = (/ 2500, & ! For 0.1cm^-1 spacing
                      10000 /) ! For 0.0025cm^-1 spacing


  ! ----------------------------------------------------
  ! The line-by-line (LBL) band values.
  ! An LBL "band" corresponds to the smallest contiguous
  ! spectral computation performed by the LBL code. The
  ! width of an LBL band is determined by the frequency
  ! bandwidth defined above.
  ! ----------------------------------------------------

  ! -- The beginning is always 1 regardless
  ! -- of the selected frequency interval.
  ! -- It's an array for conformance only.
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    LBLBAND_BEGIN = (/ 1, & ! For 0.1cm^-1 spacing
                       1 /) ! For 0.0025cm^-1 spacing

  ! -- The end is frequecy interval dependent
  ! -- The values are determined using
  ! --   LBLBAND_END = INT((FREQUENCY_BEGIN-FREQUENCY_END)/FREQUENCY_BANDWIDTH)+1
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    LBLBAND_END = (/ 13, & ! For 0.1cm^-1 spacing
                    121 /) ! For 0.0025cm^-1 spacing

  ! -- The number of LBL bands for each
  ! -- frequency interval
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    N_LBLBANDS = LBLBAND_END - LBLBAND_BEGIN + 1
  
  ! -- The maximum number of LBL bands than
  ! -- can be read in at one time
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_FREQUENCY_INTERVALS ) :: &
    MAX_N_LBLBANDS = (/ 6, & ! For 0.1cm^-1 spacing
                       10 /) ! For 0.0025cm^-1 spacing


  ! -------------------------------
  ! The molecular set specification
  ! -------------------------------

  ! -- The number of "molecular sets" recognised
  INTEGER, PUBLIC, PARAMETER :: N_MOLECULE_SETS = 19

  ! -- The names of the allowed molecular sets.
  ! -- These values are used in filenames and
  ! -- other transmittance production program inputs
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG = &
    (/ 'mol1            ', &  !   1
       'mol2            ', &  !   2
       'mol3            ', &  !   3
       'mol4            ', &  !   4
       'mol5            ', &  !   5
       'mol6            ', &  !   6
       'mol7            ', &  !   7
       'all_nocontinuum ', &  !   8
       'continua_only   ', &  !   9
       'all_withcontinua', &  !  10
       'wvo             ', &  !  11
       'wet             ', &  !  12
       'dry             ', &  !  13
       'ozo             ', &  !  14
       'wco             ', &  !  15
       'effective_mol1  ', &  ! 101
       'effective_wet   ', &  ! 112
       'effective_dry   ', &  ! 113
       'effective_ozo   ' /)  ! 114

  ! -- The ID values associated with the allowed moleculer sets
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_MOLECULE_SETS ) :: MOLECULE_SET_TAG_ID = &
    (/  1, &  !  mol1
        2, &  !  mol2
        3, &  !  mol3
        4, &  !  mol4
        5, &  !  mol5
        6, &  !  mol6
        7, &  !  mol7
        8, &  !  all_nocontinuum
        9, &  !  continua_only
       10, &  !  all_withcontinua
       11, &  !  wvo
       12, &  !  wet
       13, &  !  dry
       14, &  !  ozo
       15, &  !  wco
      101, &  !  effective_mol1
      112, &  !  effective_wet
      113, &  !  effective_dry
      114 /)  !  effective_ozo


  ! -----------------------------------
  ! The calculation angle specification
  ! -----------------------------------

  ! -- The begin and end zenith angle indices
  INTEGER, PUBLIC, PARAMETER :: ZENITH_ANGLE_BEGIN = 1
  INTEGER, PUBLIC, PARAMETER :: ZENITH_ANGLE_END   = 7
  
  ! -- The number of calculation zenith angles
  INTEGER, PUBLIC, PARAMETER :: N_ZENITH_ANGLES = ZENITH_ANGLE_END - ZENITH_ANGLE_BEGIN + 1

  ! -- The secant of the calculation angles
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_ZENITH_ANGLES ) :: &
    ZENITH_ANGLE_SECANT = (/ 1.00_fp_kind, &  ! Nadir
                             1.25_fp_kind, &  ! 36.870 deg.
                             1.50_fp_kind, &  ! 48.190 deg.
                             1.75_fp_kind, &  ! 55.150 deg.
                             2.00_fp_kind, &  ! 60.000 deg.
                             2.25_fp_kind, &  ! 63.612 deg.
                             3.00_fp_kind /)  ! 70.529 deg.

  ! -- The actual calculation angles
  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_ZENITH_ANGLES ) :: &
    ZENITH_ANGLE = (/  0.0_fp_kind,          &
                      36.8698976458_fp_kind, &
                      48.1896851042_fp_kind, &
                      55.1500954210_fp_kind, &
                      60.0_fp_kind,          &
                      63.6122000388_fp_kind, &
                      70.5287793655_fp_kind /)

END MODULE Tau_Production_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Tau_Production_Parameters.f90,v 1.16 2005/12/27 21:24:28 paulv Exp $
!
! $Date: 2005/12/27 21:24:28 $
!
! $Revision: 1.16 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Tau_Production_Parameters.f90,v $
! Revision 1.16  2005/12/27 21:24:28  paulv
! - Increased maximum number of LBL bands for 0.1cm-1 case from 5 to 6 to
!   handle SEVIRI.
!
! Revision 1.15  2005/12/19 19:32:34  paulv
! - Added definition of the default CO2 mixing ratio.
!
! Revision 1.14  2005/09/16 20:22:45  paulv
! - Angle parameters now all have the ZENITH_ prefix.
!
! Revision 1.13  2005/08/15 20:58:03  paulv
! - Corrected bug in specification of LBLBAND_END parameter.
!
! Revision 1.12  2005/05/08 15:28:14  paulv
! - Upgraded to Fortran-95
! - Added PROFILE_BEGIN and PROFILE_END parameter arrays for the various
!   profile sets.
! - Added capability to specify more than one frequency interval. Currently
!   two are supported: 0.1cm-1 and 0.0025cm-1. All the other frequency/band
!   parameters are based on the frequency intervals.
! - Added the actual calculation angles. Previously only the angle secants
!   were specified.
!
! Revision 1.11  2004/04/08 17:39:36  paulv
! - Changes made for calculation by angle.
!
! Revision 1.10  2004/01/23 17:17:51  paulv
! - Added molecule set index for effective water line absorption only,
!   effective_mol1.
! - Removed all references for the case where the band width is 100cm-1
!   rather than the current 25cm-1.
! - Increased the maximum number of LBL bands that can be read in at once
!   from 25 (for AVHRR) to 40 (for SEVIRI, which requires 36 for channel 5).
!   Hopefully this won't exceed memory limits.
!
! Revision 1.9  2004/01/22 18:26:17  paulv
! - Added moecule set index for water continuum only.
!
! Revision 1.8  2003/07/23 21:30:19  paulv
! - Level pressure array is now defined to 6 decimal places.
!
! Revision 1.7  2003/07/21 20:30:38  paulv
! - Changed number of ECMWF profiles from 80 to 52.
!
! Revision 1.6  2002/10/18 20:10:03  paulv
! - Rearranged and added extra molecule sets.
! - Added molecule set IDs for non-contiguous values.
!
! Revision 1.5  2002/07/21 20:21:54  paulv
! - Added definitions for multiple input profile sets.
! - Added default level pressure levels (AIRS levels).
!
! Revision 1.4  2002/06/11 21:59:18  paulv
! - Added an extra zenith angle (secant = 3.0)
! - Changed the bandwidth from 100cm-1 to 25cm-1.
!
! Revision 1.3  2002/06/07 18:23:51  paulv
! - Added definitions of molecule sets, angle limits, profile limits, and
!   zenith angle limits (for modifying nadir calcs.)
!
! Revision 1.2  2002/05/15 18:33:27  paulv
! - Added begin and end frequencies for production runs
! - Changed bandwidth from 100cm^-1 to 99.9975cm^-1.
!
! Revision 1.1  2002/04/26 22:56:23  paulv
! Initial checkin.
!
!
!
