!------------------------------------------------------------------------------
!M+
! NAME:
!       AIRS_ChannelProperties_Define
!
! PURPOSE:
!       Module defining the AIRS channel properties data structure.
!       
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AIRS_ChannelProperties_Define
!
! MODULES:
!       Type_Kinds:      Module containing definitions for kinds
!                        of variable types.
!
! CONTAINS:
!       None.
!
! DERIVED TYPES:
!       AIRS_ChannelProperties_type:  Definition of the AIRS_ChannelProperties
!                                     data structure. Fields are,
!
!         Channel_Number:            Channel ordering used by AIRS
!                                    PGE.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!         Frequency:                 The channel central frequency
!                                    as determined from the SRF fitting
!                                    of calibration data.
!                                    UNITS:      Inverse centimetres (cm^-1)
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Scalar
!
!         Module_Name:               Detector array name used in
!                                    http://www-airs.jpl.nasa.gov/airs_gui/airs_gui.html
!                                    UNITS:      N/A
!                                    TYPE:       CHARACTER
!                                    DIMENSION:  Scalar
!
!         Calibration_Channel_Index: This is the ordering of channels in
!                                    LMIRIS calibration files.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!         NEdT:                      Noise equivalent temperature or a 250K
!                                    black body provided by Calibration Team
!                                    in V.1. calibration properties file.
!                                    UNITS:      Kelvin (K)
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Scalar
!
!         FWHM:                      Full width at half maximum of the spectral 
!                                    response function.
!                                    UNITS:      Inverse centimetres (cm^-1)
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Scalar
!
!         Cij:                       Measure of spatial coregistration of
!                                    channels with respect to channel 2113
!                                    at 2391.880 cm-1. Quality control applied
!                                    to limit values to <= 1 and  >= -1.
!                                    UNITS:      N/A
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Scalar
!
!         Centroid:                  Centroid offset (x,y) in millidegrees
!                                    from boresight.
!                                    UNITS:      N/A
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Rank-1 (2)
!
!         RTA_Fitting_Error:         RTA fitting error, RMS error in Kelvin.
!                                    UNITS:      Kelvin (K)
!                                    TYPE:       REAL( fp_kind )
!                                    DIMENSION:  Scalar
!
!         AB_State:                  An integer 0-8, indicating Gain Type, Used in
!                                    L1B processing
!                                      0 - A and B are both used
!                                      1 - A is good and is used
!                                      2 - B is good and is used
!                                      3 - A and B are both used; both pop
!                                      4 - A is used, but is noisy
!                                      5 - B is used, but is noisy
!                                      6 - both detectors are dead
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!         Radiometric_Quality:       An integer 0-4, summarizing radiometric properties.
!                                    Qualities greater than 1 should not be used in
!                                    level 2, greater than 2 are considered bad.
!                                      0 - Channel is Good
!                                      1 - Channel may have anomalous noise
!                                      2 - Channel is noisy (NeDT > 2K))
!                                      3 - Channel pops
!                                      4 - Channel is dead
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!         Bad_Flag:                  A flag set to 1 to indicate that the detector
!                                    is not recomended for use in retrievals.  This
!                                    can be because the channel has poor radiometric
!                                    properties (Radiometric Quality  > 2, i.e. noisy
!                                    or popping channel), has a spatial centroid 
!                                    offset from the boresight by more than 0.25 degrees,
!                                    or has a poor SRF determination. The comment field
!                                    indicates why a channel is considered "bad"
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!         Comment:                   In order of decreasing precedence:
!                                      Dead    - detector is bad because it is dead
!                                      Noise   - detector is bad because both sides are
!                                                noisy or pop
!                                      Popping - detector is bad because both detectors
!                                                pop but are otherwise not noisy
!                                      Bad SRF - detector is bad because the SRF fit is
!                                                poor or unusable
!                                      Poor SRF- detector is bad because the SRF is poor.
!                                                This can occur if the fitted SRF width
!                                                is anomalous, such as from a shorted detector,
!                                      Spatial - detector is bad because the spatial centroid
!                                                is more than 0.25 degrees from the boresight.
!                                    UNITS:      N/A
!                                    TYPE:       CHARACTER
!                                    DIMENSION:  Scalar
!
!       Note that the AIRS_ChannelProperties_type is public, that is, the members can be
!       fully accessed outside the scope of this module.
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
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
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
!M-
!------------------------------------------------------------------------------

MODULE AIRS_ChannelProperties_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Data structure string lengths
  INTEGER, PRIVATE, PARAMETER :: ML = 5
  INTEGER, PRIVATE, PARAMETER :: CL = 8

  ! -- Initialization values
  INTEGER,         PRIVATE, PARAMETER :: I_INIT = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: R_INIT = -999.9_fp_kind
  CHARACTER( * ),  PRIVATE, PARAMETER :: C_INIT = ' ' 

  ! -------------------------------------
  ! AIRS_ChannelInfo data type definition
  ! -------------------------------------

  ! -- Structure for the channel info
  TYPE, PUBLIC :: AIRS_ChannelProperties_type
    INTEGER                         :: Channel_Number            = I_INIT
    REAL( fp_kind )                 :: Frequency                 = R_INIT
    CHARACTER( ML )                 :: Module_Name               = C_INIT
    INTEGER                         :: Calibration_Channel_Index = I_INIT
    REAL( fp_kind )                 :: NEdT                      = R_INIT
    REAL( fp_kind )                 :: FWHM                      = R_INIT
    REAL( fp_kind )                 :: Cij                       = R_INIT
    REAL( fp_kind ), DIMENSION( 2 ) :: Centroid                  = R_INIT
    REAL( fp_kind )                 :: RTA_Fitting_Error         = R_INIT
    INTEGER                         :: AB_State                  = I_INIT
    INTEGER                         :: Radiometric_Quality       = I_INIT
    INTEGER                         :: Bad_Flag                  = I_INIT
    CHARACTER( CL )                 :: Comment                   = C_INIT
  END TYPE AIRS_ChannelProperties_type

END MODULE AIRS_ChannelProperties_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/08/11 19:59:51 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AIRS_ChannelProperties_Define.f90,v $
! Revision 1.5  2004/08/11 19:59:51  paulv
! - Tested.
!
! Revision 1.4  2004/08/11 12:05:45  paulv
! - Reorganising and f90->f95 conversion. Incomplete.
!
! Revision 1.3  2004/03/09 17:14:58  paulv
! - Cosmetic changes to documentation.
!
! Revision 1.2  2003/01/10 15:42:17  paulv
! - Altered the definition of the AIRS_ChannelProperties_type from a structure
!   of arrays (with a channel dimension) to a simple scalar structure. Now, the
!   AIRS channel properties are represented by an array of structures.
!
! Revision 1.1  2003/01/09 21:23:00  paulv
! Initial checkin.
!
!
!
!
