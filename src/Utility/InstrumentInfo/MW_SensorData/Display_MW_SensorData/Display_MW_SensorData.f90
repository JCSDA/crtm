!------------------------------------------------------------------------------
!P+
! NAME:
!       Display_MW_SensorData
!
! PURPOSE:
!       Program to display MW_SensorData structure information.
!
! CATEGORY:
!       Instrument_Information
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility routines
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       MW_SensorData_Define:       Module defining the MW_SensorData data structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Apr-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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
!P-
!------------------------------------------------------------------------------

PROGRAM Display_MW_SensorData


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE MW_SensorData_Define


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Display_MW_SensorData'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Display_MW_SensorData.f90,v 1.5 2006/05/02 16:58:02 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
 
  INTEGER :: NCEP_Sensor_ID

  TYPE( MW_SensorData_type ) :: MW_SensorData



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to display MW_SensorData structure information. ")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO



    !#--------------------------------------------------------------------------#
    !#                  -- GET AN NCEP_SENSOR_ID VALUE --                       #
    !#--------------------------------------------------------------------------#

    NCEP_Sensor_ID = Get_MW_SensorData_Sensor_ID()

    IF ( NCEP_Sensor_ID < 0 ) EXIT Sensor_Loop

 
    !#--------------------------------------------------------------------------#
    !#               -- LOAD THE CURRENT MICROWAVE SENSOR DATA --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Load_MW_SensorData( MW_SensorData, &
                                       NCEP_Sensor_ID = NCEP_Sensor_ID )

 
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error loading MW sensor data for NCEP Sensor ID ", i3 )' ) NCEP_Sensor_ID
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- PRINT THE MW_SensorData STRUCTURE INFORMATION --           #
    !#--------------------------------------------------------------------------#

    CALL Print_MW_SensorData( MW_SensorData )



    !#--------------------------------------------------------------------------#
    !#                 -- DESTROY THE MW_SensorData STRUCTURE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_MW_SensorData( MW_SensorData )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying MW_SensorData structure for NCEP Sensor ID ", i3 )' ) NCEP_Sensor_ID
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop

END PROGRAM Display_MW_SensorData


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Display_MW_SensorData.f90,v 1.5 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Display_MW_SensorData.f90,v $
! Revision 1.5  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.4  2005/09/14 21:04:10  paulv
! - Removed SET integer parameter.
!
! Revision 1.3  2004/08/13 16:11:38  paulv
! - Upgraded to f95 version of MW_SensorData module. Removed the call to the
!   MW_SensorData initialisation function.
!
! Revision 1.2  2004/07/20 14:41:56  paulv
! - Removed need for SensorInfo file. Sensor IDs are obtained directly from the
!   MW_SensorData structure.
!
! Revision 1.1  2004/04/05 16:15:13  paulv
! Initial checkin.
!
!
!
!
