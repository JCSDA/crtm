!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_SensorInfo
!
! PURPOSE:
!       Module of sensor information parameters definitions for the CRTM.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_SensorInfo
!
! MODULES:
!       None.
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!
!  Copyright (C) 2005 Paul van Delst
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

MODULE CRTM_SensorInfo


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! -----------------------------
  ! Default visibility is PRIVATE
  ! -----------------------------

  PRIVATE



  !#----------------------------------------------------------------------------#
  !#             -- WMO SENSOR CODES FROM COMMON CODE TABLE C-8 --              #
  !#----------------------------------------------------------------------------#

  ! -- Invalid code for those sensors not yet defined by WMO
  INTEGER, PUBLIC, PARAMETER :: WMO_INVALID_SENSOR = 2047

  INTEGER, PUBLIC, PARAMETER :: WMO_HIRS2   = 605
  INTEGER, PUBLIC, PARAMETER :: WMO_MSU     = 623
  INTEGER, PUBLIC, PARAMETER :: WMO_AVHRR2  = 590

  INTEGER, PUBLIC, PARAMETER :: WMO_HIRS3   = 606
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSUA   = 570
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSUB   = 574
  INTEGER, PUBLIC, PARAMETER :: WMO_AVHRR3  = 591
  INTEGER, PUBLIC, PARAMETER :: WMO_MHS     = 203

  INTEGER, PUBLIC, PARAMETER :: WMO_VAS     = 630

!*** error
  INTEGER, PUBLIC, PARAMETER :: WMO_SOUNDER = 626
!  INTEGER, PUBLIC, PARAMETER :: WMO_SOUNDER = 615

  INTEGER, PUBLIC, PARAMETER :: WMO_SSMI    = 905
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMT1   = 906
  INTEGER, PUBLIC, PARAMETER :: WMO_SSMT2   = 907

  INTEGER, PUBLIC, PARAMETER :: WMO_MODIS   = 389
  INTEGER, PUBLIC, PARAMETER :: WMO_HSB     = 246
  INTEGER, PUBLIC, PARAMETER :: WMO_AMSRE   = 345
  INTEGER, PUBLIC, PARAMETER :: WMO_AIRS    = 420

  INTEGER, PUBLIC, PARAMETER :: WMO_VISSR   = 489

  INTEGER, PUBLIC, PARAMETER :: WMO_MVIRI   = 205
  INTEGER, PUBLIC, PARAMETER :: WMO_SEVIRI  = 207

  INTEGER, PUBLIC, PARAMETER :: WMO_ABI     = WMO_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: WMO_WINDSAT = WMO_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: WMO_ATMS    = WMO_INVALID_SENSOR


  !#----------------------------------------------------------------------------#
  !#             -- WMO SATELITE CODES FROM COMMON CODE TABLE C-5 --            #
  !#----------------------------------------------------------------------------#

  ! -- Invalid code for those satellites not yet defined by WMO
  INTEGER, PUBLIC, PARAMETER :: WMO_INVALID_SATELLITE = 1023

  INTEGER, PUBLIC, PARAMETER :: WMO_TIROSN     = 708
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA05     = WMO_TIROSN
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA06     = 706
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA07     = 707
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA08     = 200
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA09     = 201
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA10     = 202
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA11     = 203
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA12     = 204
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA14     = 205
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA15     = 206
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA16     = 207
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA17     = 208
  INTEGER, PUBLIC, PARAMETER :: WMO_NOAA18     = 209

  INTEGER, PUBLIC, PARAMETER :: WMO_GOES04     = 734
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES05     = 735
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES06     = 250
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES07     = 251
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES08     = 252
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES09     = 253
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES10     = 254
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES11     = 255
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES12     = 256
  INTEGER, PUBLIC, PARAMETER :: WMO_GOES13     = 257

  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP13     = 246
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP14     = 247
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP15     = 248
  INTEGER, PUBLIC, PARAMETER :: WMO_DMSP16     = 249

  INTEGER, PUBLIC, PARAMETER :: WMO_TERRA      = 783
  INTEGER, PUBLIC, PARAMETER :: WMO_AQUA       = 784

  INTEGER, PUBLIC, PARAMETER :: WMO_GMS5       = 152

  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT03 = 50
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT04 = 51
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT05 = 52
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT06 = 53
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT07 = 54
  INTEGER, PUBLIC, PARAMETER :: WMO_METEOSAT08 = 71

  INTEGER, PUBLIC, PARAMETER :: WMO_GOESR      = WMO_INVALID_SATELLITE
  INTEGER, PUBLIC, PARAMETER :: WMO_CORIOLIS   = WMO_INVALID_SATELLITE
  INTEGER, PUBLIC, PARAMETER :: WMO_NPOESSC1   = WMO_INVALID_SATELLITE



  !#----------------------------------------------------------------------------#
  !#           -- COMBINED SENSOR/SATELLITE CODE USED AT NCEP/EMC --            #
  !#----------------------------------------------------------------------------#

  ! -- Invalid code for those sensors not used at EMC
  INTEGER, PUBLIC, PARAMETER :: NCEP_INVALID_SENSOR = -1

  INTEGER, PUBLIC, PARAMETER :: HIRS2_N05        =    5
  INTEGER, PUBLIC, PARAMETER :: MSU_N05          =  205
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N05       =  605
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N06        =    6
  INTEGER, PUBLIC, PARAMETER :: MSU_N06          =  206
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N06       =  606
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N07        =    7
  INTEGER, PUBLIC, PARAMETER :: MSU_N07          =  207
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N07       =  607
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N08        =    8
  INTEGER, PUBLIC, PARAMETER :: MSU_N08          =  208
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N08       =  608
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N09        =    9
  INTEGER, PUBLIC, PARAMETER :: MSU_N09          =  209
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N09       =  609
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N10        =   10
  INTEGER, PUBLIC, PARAMETER :: MSU_N10          =  210
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N10       =  610
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N11        =   11
  INTEGER, PUBLIC, PARAMETER :: MSU_N11          =  211
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N11       =  611
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N12        =   12
  INTEGER, PUBLIC, PARAMETER :: MSU_N12          =  212
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N12       =  612
  INTEGER, PUBLIC, PARAMETER :: HIRS2_N14        =   14
  INTEGER, PUBLIC, PARAMETER :: MSU_N14          =  214
  INTEGER, PUBLIC, PARAMETER :: AVHRR2_N14       =  614
  INTEGER, PUBLIC, PARAMETER :: HIRS3_N15        =   15
  INTEGER, PUBLIC, PARAMETER :: AMSUA_N15        =  315
  INTEGER, PUBLIC, PARAMETER :: AMSUB_N15        =  415
  INTEGER, PUBLIC, PARAMETER :: AVHRR3_N15       =  615
  INTEGER, PUBLIC, PARAMETER :: HIRS3_N16        =   16
  INTEGER, PUBLIC, PARAMETER :: AMSUA_N16        =  316
  INTEGER, PUBLIC, PARAMETER :: AMSUB_N16        =  416
  INTEGER, PUBLIC, PARAMETER :: AVHRR3_N16       =  616
  INTEGER, PUBLIC, PARAMETER :: HIRS3_N17        =   17
  INTEGER, PUBLIC, PARAMETER :: AMSUA_N17        =  317
  INTEGER, PUBLIC, PARAMETER :: AMSUB_N17        =  417
  INTEGER, PUBLIC, PARAMETER :: AVHRR3_N17       =  617
  INTEGER, PUBLIC, PARAMETER :: HIRS3_N18        =   18
  INTEGER, PUBLIC, PARAMETER :: AMSUA_N18        =  318
  INTEGER, PUBLIC, PARAMETER :: MHS_N18          =  418
  INTEGER, PUBLIC, PARAMETER :: AVHRR3_N18       =  618

  INTEGER, PUBLIC, PARAMETER :: VAS_G04          =   54
  INTEGER, PUBLIC, PARAMETER :: VAS_G05          =   55
  INTEGER, PUBLIC, PARAMETER :: VAS_G06          =   56
  INTEGER, PUBLIC, PARAMETER :: VAS_G07          =   57
  INTEGER, PUBLIC, PARAMETER :: SNDR_G08         =   58
  INTEGER, PUBLIC, PARAMETER :: IMGR_G08         =  258
  INTEGER, PUBLIC, PARAMETER :: SNDR_G09         =   59
  INTEGER, PUBLIC, PARAMETER :: IMGR_G09         =  259
  INTEGER, PUBLIC, PARAMETER :: SNDR_G10         =   60
  INTEGER, PUBLIC, PARAMETER :: IMGR_G10         =  260
  INTEGER, PUBLIC, PARAMETER :: SNDR_G11         =   61
  INTEGER, PUBLIC, PARAMETER :: IMGR_G11         =  261
  INTEGER, PUBLIC, PARAMETER :: SNDR_G12         =   62
  INTEGER, PUBLIC, PARAMETER :: IMGR_G12         =  262
  INTEGER, PUBLIC, PARAMETER :: SNDR_G13         =   63
  INTEGER, PUBLIC, PARAMETER :: IMGR_G13         =  263

  INTEGER, PUBLIC, PARAMETER :: SSMI_F13         =  713
  INTEGER, PUBLIC, PARAMETER :: SSMI_F14         =  714
  INTEGER, PUBLIC, PARAMETER :: SSMI_F15         =  715
  INTEGER, PUBLIC, PARAMETER :: SSMT1_F13        =  813
  INTEGER, PUBLIC, PARAMETER :: SSMT1_F15        =  815
  INTEGER, PUBLIC, PARAMETER :: SSMT2_F14        =  114
  INTEGER, PUBLIC, PARAMETER :: SSMT2_F15        =  115
  INTEGER, PUBLIC, PARAMETER :: SSMIS_F16        =  516

  INTEGER, PUBLIC, PARAMETER :: AMSUA_AQUA       =  349
  INTEGER, PUBLIC, PARAMETER :: HSB_AQUA         =  449
  INTEGER, PUBLIC, PARAMETER :: AMSRE_AQUA       =  549
  INTEGER, PUBLIC, PARAMETER :: AIRS_AQUA        =   49
  INTEGER, PUBLIC, PARAMETER :: MODIS_TERRA      =   47
  INTEGER, PUBLIC, PARAMETER :: MODIS_AQUA       =   48

  INTEGER, PUBLIC, PARAMETER :: VISSR_GMS5       = NCEP_INVALID_SENSOR

  INTEGER, PUBLIC, PARAMETER :: MVIRI_M03        = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: MVIRI_M04        = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: MVIRI_M05        = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: MVIRI_M06        = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: MVIRI_M07        = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: SEVIRI_M08       = NCEP_INVALID_SENSOR

  INTEGER, PUBLIC, PARAMETER :: ABI_GR           = NCEP_INVALID_SENSOR
  INTEGER, PUBLIC, PARAMETER :: WINDSAT_CORIOLIS =  900
  INTEGER, PUBLIC, PARAMETER :: ATMS_C1          = 1012


  !#----------------------------------------------------------------------------#
  !#                 -- COMBINED ID CODES IN ARRAY FORM --                      #
  !#----------------------------------------------------------------------------#

  INTEGER, PRIVATE, PARAMETER :: N_VALID_IDS = 83

  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_VALID_IDS ) :: NCEP_SENSOR_ID = (/ &
    HIRS2_N05,      MSU_N05,        AVHRR2_N05,     HIRS2_N06,        MSU_N06,        AVHRR2_N06,  &
    HIRS2_N07,      MSU_N07,        AVHRR2_N07,     HIRS2_N08,        MSU_N08,        AVHRR2_N08,  &
    HIRS2_N09,      MSU_N09,        AVHRR2_N09,     HIRS2_N10,        MSU_N10,        AVHRR2_N10,  &
    HIRS2_N11,      MSU_N11,        AVHRR2_N11,     HIRS2_N12,        MSU_N12,        AVHRR2_N12,  &
    HIRS2_N14,      MSU_N14,        AVHRR2_N14,     HIRS3_N15,        AMSUA_N15,      AMSUB_N15,   &
    AVHRR3_N15,     HIRS3_N16,      AMSUA_N16,      AMSUB_N16,        AVHRR3_N16,     HIRS3_N17,   &
    AMSUA_N17,      AMSUB_N17,      AVHRR3_N17,     HIRS3_N18,        AMSUA_N18,      MHS_N18,     &
    AVHRR3_N18,     VAS_G04,        VAS_G05,        VAS_G06,          VAS_G07,        SNDR_G08,    &
    IMGR_G08,       SNDR_G09,       IMGR_G09,       SNDR_G10,         IMGR_G10,       SNDR_G11,    &
    IMGR_G11,       SNDR_G12,       IMGR_G12,       SNDR_G13,         IMGR_G13,       SSMI_F13,    &
    SSMI_F14,       SSMI_F15,       SSMT1_F13,      SSMT1_F15,        SSMT2_F14,      SSMT2_F15,   &
    SSMIS_F16,      AMSUA_AQUA,     HSB_AQUA,       AMSRE_AQUA,       AIRS_AQUA,      MODIS_TERRA, &
    MODIS_AQUA,     VISSR_GMS5,     MVIRI_M03,      MVIRI_M04,        MVIRI_M05,      MVIRI_M06,   &
    MVIRI_M07,      SEVIRI_M08,     ABI_GR,         WINDSAT_CORIOLIS, ATMS_C1 /)      

END MODULE CRTM_SensorInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_SensorInfo.f90,v 1.1 2005/08/19 20:54:10 yhan Exp $
!
! $Date: 2005/08/19 20:54:10 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_SensorInfo.f90,v $
! Revision 1.1  2005/08/19 20:54:10  yhan
! -- Initial checkin
!
! Revision 1.1  2005/06/27 13:20:26  paulv
! Initial checkin.
!
!
!
!
