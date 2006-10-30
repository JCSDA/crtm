!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Surface
!
! PURPOSE:
!       Program to test the CRTM Surface routines.
!
! CATEGORY:
!       CRTM : Surface
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
!       CRTM_Surface_Define:     Module defining the CRTM_Surface data
!                                structure and containing routines to
!                                manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!       CRTM_Surface_Binary_IO:  Module containing routines to read and write
!                                CRTM_Surface Binary format files.
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      ERROR_HANDLER module
!                                      BINARY_FILE_UTILITY module
!                                      CRTM_SURFACE_DEFINE module
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
! FILES ACCESSED:
!       Input/Output: Test file written and then read.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jul-2004
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

PROGRAM Test_Surface


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler

  USE CRTM_Surface_Define
  USE CRTM_Surface_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Surface'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Surface.f90,v 1.9 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: W1 = 10.0_fp_kind
  REAL( fp_kind ), PARAMETER :: W2 =  0.5_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  CHARACTER( 256 ) :: Input_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, n_Locations, n_Locations_Read

  TYPE( CRTM_Surface_type ), ALLOCATABLE, DIMENSION( : ) :: Surface, &
                                                            Surface_Copy



  !#----------------------------------------------------------------------------#
  !#                   -- GET AN INPUT Surface FILENAME --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a Surface data filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Input_Filename
  Input_Filename = ADJUSTL( Input_Filename )



  !#----------------------------------------------------------------------------#
  !#                -- ALLOCATE THE Surface STRUCTURE ARRAYS --                 #
  !#----------------------------------------------------------------------------#

  ! ----------------------
  ! Inquire the input file
  ! ----------------------

  WRITE( *, '( /5x, "Inquiring Surface data file ", a, "...." )' ) TRIM( Input_Filename )

  Error_Status = CRTM_Inquire_Surface_Binary( Input_Filename, &
                                              n_Locations = n_Locations )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring input Surface file '//&
                          TRIM( Input_Filename ), & 
                          Error_Status )
    STOP
  END IF

  WRITE( *, '( 10x, "Number of surface data locations: ", i5 )' ) n_Locations



  ! -------------------------------------
  ! Allocate the Surface structure arrays
  ! -------------------------------------

  ALLOCATE( Surface( n_Locations ), &
            Surface_Copy( n_Locations ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating Surface structure arrays. Stat = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), & 
                          Error_Status )
    STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#                       -- READ THE Surface DATA --                          #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Read_Surface_Binary( Input_Filename, &
                                           Surface, &
                                           n_Locations = n_Locations_Read )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading input Surface file '//&
                          TRIM( Input_Filename ), & 
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- TEST THE WEIGHTED SUM ROUTINE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing WeightedSum functions ..." )' )


  ! ------------------------
  ! Copy the structure array
  ! ------------------------

  Error_Status = CRTM_Assign_Surface( Surface, Surface_Copy )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Surface structure array for WeightedSum test', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------
  ! Compute the weighted sum
  ! ------------------------

  Error_Status = CRTM_WeightedSum_Surface( Surface, &
                                           Surface_Copy, &
                                           W1, &
                                           w2 = W2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing Surface weighted sum', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Print out some results
  ! ----------------------

  DO m = 1, n_Locations

    WRITE( *, '( /5x, "Location #", i3 )' ) m

    WRITE( *, * ) Surface(m)%Wind_Speed, Surface_Copy(m)%Wind_Speed

    WRITE( *, * ) LAND_TYPE_NAME( Surface(m)%Land_Type )
    WRITE( *, * ) Surface(m)%Land_Temperature,      Surface_Copy(m)%Land_Temperature
    WRITE( *, * ) Surface(m)%Soil_Moisture_Content, Surface_Copy(m)%Soil_Moisture_Content
    WRITE( *, * ) Surface(m)%Canopy_Water_Content,  Surface_Copy(m)%Canopy_Water_Content
    WRITE( *, * ) Surface(m)%Vegetation_Fraction,   Surface_Copy(m)%Vegetation_Fraction
    WRITE( *, * ) Surface(m)%Soil_Temperature,      Surface_Copy(m)%Soil_Temperature

    WRITE( *, * ) WATER_TYPE_NAME( Surface(m)%Water_Type )
    WRITE( *, * ) Surface(m)%Water_Temperature, Surface_Copy(m)%Water_Temperature
    WRITE( *, * ) Surface(m)%Wind_Direction,    Surface_Copy(m)%Wind_Direction
    WRITE( *, * ) Surface(m)%Salinity,          Surface_Copy(m)%Salinity

    WRITE( *, * ) SNOW_TYPE_NAME( Surface(m)%Snow_Type )
    WRITE( *, * ) Surface(m)%Snow_Temperature, Surface_Copy(m)%Snow_Temperature
    WRITE( *, * ) Surface(m)%Snow_Depth,       Surface_Copy(m)%Snow_Depth
    WRITE( *, * ) Surface(m)%Snow_Density,     Surface_Copy(m)%Snow_Density
    WRITE( *, * ) Surface(m)%Snow_Grain_Size,  Surface_Copy(m)%Snow_Grain_Size

    WRITE( *, * ) ICE_TYPE_NAME( Surface(m)%Ice_Type )
    WRITE( *, * ) Surface(m)%Ice_Temperature, Surface_Copy(m)%Ice_Temperature
    WRITE( *, * ) Surface(m)%Ice_Thickness,   Surface_Copy(m)%Ice_Thickness
    WRITE( *, * ) Surface(m)%Ice_Density,     Surface_Copy(m)%Ice_Density
    WRITE( *, * ) Surface(m)%Ice_Roughness,   Surface_Copy(m)%Ice_Roughness

  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- TEST THE ZERO ROUTINE --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing Zero subroutines ..." )' )


  ! ------------------------
  ! Zero the structure array
  ! ------------------------

  CALL CRTM_Zero_Surface( Surface )


  ! ----------------------
  ! Print out some results
  ! ----------------------

  DO m = 1, n_Locations

    WRITE( *, '( /5x, "Location #", i3 )' ) m

    WRITE( *, * ) Surface(m)%Wind_Speed

    WRITE( *, * ) LAND_TYPE_NAME( Surface(m)%Land_Type )
    WRITE( *, * ) Surface(m)%Land_Temperature
    WRITE( *, * ) Surface(m)%Soil_Moisture_Content
    WRITE( *, * ) Surface(m)%Canopy_Water_Content
    WRITE( *, * ) Surface(m)%Vegetation_Fraction
    WRITE( *, * ) Surface(m)%Soil_Temperature

    WRITE( *, * ) WATER_TYPE_NAME( Surface(m)%Water_Type )
    WRITE( *, * ) Surface(m)%Water_Temperature
    WRITE( *, * ) Surface(m)%Wind_Direction
    WRITE( *, * ) Surface(m)%Salinity

    WRITE( *, * ) SNOW_TYPE_NAME( Surface(m)%Snow_Type )
    WRITE( *, * ) Surface(m)%Snow_Temperature
    WRITE( *, * ) Surface(m)%Snow_Depth
    WRITE( *, * ) Surface(m)%Snow_Density
    WRITE( *, * ) Surface(m)%Snow_Grain_Size

    WRITE( *, * ) ICE_TYPE_NAME( Surface(m)%Ice_Type )
    WRITE( *, * ) Surface(m)%Ice_Temperature
    WRITE( *, * ) Surface(m)%Ice_Thickness
    WRITE( *, * ) Surface(m)%Ice_Density
    WRITE( *, * ) Surface(m)%Ice_Roughness

  END DO
  


  !#----------------------------------------------------------------------------#
  !#                       -- WRITE THE Surface DATA --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing test Surface data file ", a, "...." )' ) 'Test.'//TRIM( Input_Filename )

  Error_Status = CRTM_Write_Surface_Binary( 'Test.'//TRIM( Input_Filename ), &
                                            Surface )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test Surface file '//&
                          'Test.'//TRIM( Input_Filename ), & 
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- DESTROY AND DEALLOCATE THE Surface STRUCTURE ARRAY --        #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Destroy the structure array
  ! ---------------------------

  Error_Status = CRTM_Destroy_Surface( Surface )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying surface structure array.', & 
                          WARNING )
  END IF


  ! --------------------
  ! Deallocate the array
  ! --------------------

  DEALLOCATE( Surface, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating Surface structure array. Stat = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), & 
                          WARNING )
  END IF

END PROGRAM Test_Surface


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Surface.f90,v 1.9 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Surface.f90,v $
! Revision 1.9  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.8  2005/08/18 22:12:52  paulv
! - Added test of Surface Zero routines.
!
! Revision 1.7  2005/08/18 14:46:21  paulv
! - Removed references to Surface%Type component. Their is no longer any Type
!   member of the Surface structure.
!
! Revision 1.6  2005/06/16 16:55:47  paulv
! - Included test of the WeightedSum() functions.
!
! Revision 1.5  2005/06/16 15:07:10  paulv
! - Replaced initialisation of Surface structure with dummy data. Data is now
!   read from a user specified file. The file is inquired using the new
!   Inquire() functions and the Surface structure array is allocated.
! - Data read in is written to a test file.
!
! Revision 1.4  2004/11/05 16:00:07  paulv
! - Altered to use new and updated modules.
!
! Revision 1.3  2004/08/05 17:36:13  paulv
! - Changed to use updated Surface structure definition.
!
! Revision 1.2  2004/07/27 14:27:58  paulv
! - Updated to use new surface and sensor data definition modules.
!
! Revision 1.1  2004/07/22 19:49:02  paulv
! Initial checkin.
!
!
!
!
