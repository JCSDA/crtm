!------------------------------------------------------------------------------
!M+
! NAME:
!       SensorInfo_Utility
!
! PURPOSE:
!       Module for SensorInfo utility routines.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE SensorInfo_Utility
!
! MODULES:
!       SensorInfo_Define:     Module defining the SensorInfo data structure
!                              and containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       SensorInfo_Get_Index:  Function to return the index in the SensorInfo
!                              structure for the required sensor.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP/EMC 04-Sep-2002
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

MODULE SensorInfo_Utility

  ! ------------
  ! Module usage
  ! ------------

  USE SensorInfo_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: SensorInfo_Get_Index


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE SensorInfo_Get_Index
    MODULE PROCEDURE Get_Index_By_Name
    MODULE PROCEDURE Get_Index_By_NCEP_ID
    MODULE PROCEDURE Get_Index_By_WMO_ID
  END INTERFACE ! SensorInfo_Get_Index


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC COUNT, &
            LEN, &
            LEN_TRIM, &
            PACK


CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       SensorInfo_Get_Index
!
! PURPOSE:
!       Function to return the index in the SensorInfo structure for
!       the required sensor.
!
! CATEGORY:
!       NCEP RTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!
!       Three interfaces are available:
!
!       1) Inquiring SensorInfo structure by sensor AND satellite name:
!       ---------------------------------------------------------------
!
!         result = SensorInfo_Get_Index( SensorInfo,    &  ! Input
!                                        Sensor_Name,   &  ! Input
!                                        Satellite_Name )  ! Input
!
!
!       2) Inquiring SensorInfo structure by NCEP Sensor ID:
!       ----------------------------------------------------
!
!         result = SensorInfo_Get_Index( SensorInfo,    &  ! Input
!                                        NCEP_Sensor_ID )  ! Input
!
!
!       3) Inquiring SensorInfo structure by WMO IDs:
!       ---------------------------------------------
!
!         result = SensorInfo_Get_Index( SensorInfo,      &  ! Input
!                                        WMO_Sensor_ID,   &  ! Input
!                                        WMO_Satellite_ID )  ! Input
!
!
! INPUT ARGUMENTS:
!
!       SensorInfo:          Structure containing the sensor information.
!                            UNITS:      N/A
!                            TYPE:       SensorInfo_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!
!       For inquiry by sensor AND satellite name:
!       -----------------------------------------
!
!         Sensor_Name:       A character string containing the sensor name.
!                            UNITS:      None
!                            TYPE:       CHARACTER( * )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         Satellite_Name:    A character string containing the satellite name.
!                            UNITS:      None
!                            TYPE:       CHARACTER( * )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!
!       For inquiry by NCEP ID:
!       -----------------------
!
!         NCEP_Sensor_ID:    The NCEP/EMC "in-house" value used to distinguish
!                            between different sensor/satellite combinations.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!       For inquiry by WMO IDs:
!       -----------------------
!
!         WMO_Sensor_ID:     The WMO Sensor ID number taken from Common
!                            Code Table C-8 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            A value of -1 indicates that no matching value
!                            was found.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
!         WMO_Satellite_ID:  The WMO Satellite ID number taken from Common
!                            Code Table C-5 in documentation at
!                              http://www.wmo.ch/web/ddbs/Code-tables.html
!                            A value of -1 indicates that no matching value
!                            was found.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       The return value is the index of the sensor within the
!       the SensorInfo structure that matched the input categories.
!
!       If the input is invalid or no match was found, -1 is returned.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 31-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Get_Index_By_Name( SensorInfo,      &  ! Input
                              Sensor_Name,     &  ! Input
                              Satellite_Name ) &  ! Input
                            RESULT( SensorInfo_Index  )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    TYPE( SensorInfo_type), INTENT( IN )  :: SensorInfo
    CHARACTER( * ),         INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),         INTENT( IN )  :: Satellite_Name


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: SensorInfo_Index


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( LEN( SensorInfo%Sensor_Name    ) ) :: Sensor
    CHARACTER( LEN( SensorInfo%Satellite_Name ) ) :: Satellite

    INTEGER, DIMENSION( SensorInfo%n_Sensors ) :: i
    INTEGER :: n, j



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALIZE THE RETURN VALUE --                     #
    !#--------------------------------------------------------------------------#

    SensorInfo_Index = -1



    !#--------------------------------------------------------------------------#
    !#                        -- CHECK INPUT STRINGS --                         #
    !#--------------------------------------------------------------------------#

    IF ( SensorInfo%n_Sensors       <= 0 .OR. &
         LEN_TRIM( Sensor_Name    ) == 0 .OR. &
         LEN_TRIM( Satellite_Name ) == 0      ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    ! -- Truncate, or pad, the input names to the
    ! -- same length as the SensorInfo strings
    Sensor    = Sensor_Name
    Satellite = Satellite_Name

    ! -- Count the matchups
    n = COUNT( SensorInfo%Sensor_Name    == Sensor    .AND. &
               SensorInfo%Satellite_Name == Satellite       )

    IF ( n == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE THE MATCHING SensorInfo INDEX --               #
    !#--------------------------------------------------------------------------#

    ! -- Get the matching indices (there may be more than one)
    i(1:n) = PACK( (/ ( j, j = 1, SensorInfo%n_Sensors ) /), &
                   ( SensorInfo%Sensor_Name    == Sensor    .AND. &
                     SensorInfo%Satellite_Name == Satellite       ) )

    ! -- Save the first occurrance in the result variable
    SensorInfo_Index = i(1)

  END FUNCTION Get_Index_By_Name

  FUNCTION Get_Index_By_NCEP_ID( SensorInfo,      &  ! Input
                                 NCEP_Sensor_ID ) &  ! Input
                               RESULT( SensorInfo_Index  )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    TYPE( SensorInfo_type), INTENT( IN )  :: SensorInfo
    INTEGER,                INTENT( IN )  :: NCEP_Sensor_ID


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: SensorInfo_Index


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( SensorInfo%n_Sensors ) :: i
    INTEGER :: n, j



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALIZE THE RETURN VALUE --                     #
    !#--------------------------------------------------------------------------#

    SensorInfo_Index = -1



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( SensorInfo%n_Sensors <= 0 .OR. &
         NCEP_Sensor_ID       <= 0      ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    n = COUNT( SensorInfo%NCEP_Sensor_ID == NCEP_Sensor_ID )

    IF ( n == 0 ) THEN
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE THE MATCHING SensorInfo INDEX --               #
    !#--------------------------------------------------------------------------#

    ! -- Get the matching indices (there may be more than one)
    i(1:n) = PACK( (/ ( j, j = 1, SensorInfo%n_Sensors ) /), &
                   SensorInfo%NCEP_Sensor_ID == NCEP_Sensor_ID )

    ! -- Save the first occurance in the result variable
    SensorInfo_Index = i(1)

  END FUNCTION Get_Index_By_NCEP_ID

  FUNCTION Get_Index_By_WMO_ID( SensorInfo,        &  ! Input
                                WMO_Sensor_ID,     &  ! Input
                                WMO_Satellite_ID ) &  ! Input
                              RESULT( SensorInfo_Index  )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    TYPE( SensorInfo_type), INTENT( IN )  :: SensorInfo
    INTEGER,                INTENT( IN )  :: WMO_Sensor_ID
    INTEGER,                INTENT( IN )  :: WMO_Satellite_ID


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: SensorInfo_Index


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( SensorInfo%n_Sensors ) :: i
    INTEGER :: n, j



    !#--------------------------------------------------------------------------#
    !#                    -- INITIALIZE THE RETURN VALUE --                     #
    !#--------------------------------------------------------------------------#

    SensorInfo_Index = -1



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( SensorInfo%n_Sensors <= 0 .OR. &
         WMO_Sensor_ID        <= 0 .OR. &
         WMO_Satellite_ID     <= 0      ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    n = COUNT( SensorInfo%WMO_Sensor_ID    == WMO_Sensor_ID .AND. &
               SensorInfo%WMO_Satellite_ID == WMO_Satellite_ID    )

    IF ( n == 0 ) THEN
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE THE MATCHING SensorInfo INDEX --               #
    !#--------------------------------------------------------------------------#

    ! -- Get the matching indices (there may be more than one)
    i(1:n) = PACK( (/ ( j, j = 1, SensorInfo%n_Sensors ) /), &
                   ( SensorInfo%WMO_Sensor_ID    == WMO_Sensor_ID .AND. &
                     SensorInfo%WMO_Satellite_ID == WMO_Satellite_ID    ) )

    ! -- Save the first occurance in the result variable
    SensorInfo_Index = i(1)

  END FUNCTION Get_Index_By_WMO_ID

END MODULE SensorInfo_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SensorInfo_Utility.f90,v 1.1 2002/09/05 16:52:21 paulv Exp $
!
! $Date: 2002/09/05 16:52:21 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SensorInfo_Utility.f90,v $
! Revision 1.1  2002/09/05 16:52:21  paulv
! Initial checkin.
!
!
!
!
