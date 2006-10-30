!------------------------------------------------------------------------------
!P+
! NAME:
!       AtmProfile2Atmosphere
!
! PURPOSE:
!       Program to convert netCDF AtmProfile datafiles into Binary
!       Atmosphere datafiles.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:             Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       AtmProfile_Define:           Module defining the AtmProfile data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:        Module containing routines to read and write
!                                    AtmProfile netCDF format files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          ATMPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       CRTM_Atmosphere_Define:      Module defining the AtmProfile data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          CRTM_CLOUD_DEFINE module
!
!       CRTM_Atmosphere_Binary_IO:   Module containing routines to read and write
!                                    Atmosphere Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_ATMOSPHERE_DEFINE module
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
!       Input:  netCDF format AtmProfile file
!
!       Output: Binary format Atmosphere file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jul-2004
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

PROGRAM AtmProfile2Atmosphere


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE Binary_File_Utility

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'AtmProfile2Atmosphere'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: AtmProfile2Atmosphere.f90,v 1.7 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- No cloud data
  INTEGER, PARAMETER :: N_CLOUDS = 0

  ! -- No aerosol data
  INTEGER, PARAMETER :: N_AEROSOLS = 0


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: AtmProfile_Filename
  CHARACTER( 256 ) :: Atmosphere_Filename


  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER :: m, nc, na, in

  TYPE( AtmProfile_type ) :: AtmProfile

  TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), ALLOCATABLE :: Atmosphere
  TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), ALLOCATABLE :: Dummy_Atmosphere



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convert netCDF AtmProfile datafiles into",&
             &/5x, "   Binary Atmosphere datafiles.")' )
  WRITE( *, '(/5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE AtmProfile FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the AtmProfile filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter an AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) AtmProfile_fileNAME
  AtmProfile_fileNAME = ADJUSTL( AtmProfile_fileNAME )


  ! -------
  ! Read it
  ! -------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_fileNAME ), &
                                         AtmProfile, &
                                         Reverse = 1 )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_fileNAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- ALLOCATE THE Atmosphere STRUCTURE ARRAYS --              #
  !#----------------------------------------------------------------------------#

  ALLOCATE( Atmosphere( AtmProfile%n_Profiles ), &
            Dummy_Atmosphere(  AtmProfile%n_Profiles ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating the Atmosphere structure arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                         -- LOOP OVER PROFILES  --                          #
  !#----------------------------------------------------------------------------#

  Profile_Loop: DO m = 1, AtmProfile%n_Profiles


    ! -------------------------------------------------
    ! Allocate the current profile Atmosphere structure
    ! -------------------------------------------------

    Error_Status = CRTM_Allocate_Atmosphere( AtmProfile%n_Layers, &
                                             AtmProfile%n_Absorbers, &
                                             N_CLOUDS, &
                                             N_AEROSOLS, &
                                             Atmosphere(m) )

    IF ( Error_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Atmosphere structure for profile #", i5 )' ) m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    ! ------------------
    ! Copy over the data
    ! ------------------

    Atmosphere(m)%Climatology    = AtmProfile%Climatology_Model(m)
    Atmosphere(m)%Absorber_ID    = AtmProfile%Absorber_ID
    Atmosphere(m)%Absorber_Units = AtmProfile%Absorber_Units_ID

    Atmosphere(m)%Level_Pressure    = AtmProfile%Level_Pressure(:,m)
    Atmosphere(m)%Level_Temperature = AtmProfile%Level_Temperature(:,m)
    Atmosphere(m)%Pressure          = AtmProfile%Layer_Pressure(:,m)
    Atmosphere(m)%Temperature       = AtmProfile%Layer_Temperature(:,m)
    Atmosphere(m)%Absorber          = AtmProfile%Layer_Absorber(:,:,m)


    ! ------------------------------
    ! Set the level temperature flag
    ! ------------------------------

    Atmosphere(m)%Level_Temperature_Input = YES

  END DO Profile_Loop



  !#----------------------------------------------------------------------------#
  !#                  -- WRITE THE Atmosphere STRUCTURE FILE --                 #
  !#----------------------------------------------------------------------------#

  ! --------------
  ! Get a filename
  ! --------------

  WRITE( *, FMT     = '( /5x, "Enter an output Atmosphere filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Atmosphere_fileNAME
  Atmosphere_fileNAME = ADJUSTL( Atmosphere_fileNAME )


  ! --------------
  ! Write the file
  ! --------------

  WRITE( *, '( /5x, "Writing the Atmosphere data file...." )' )

  Error_Status = CRTM_Write_Atmosphere_Binary( TRIM( Atmosphere_Filename ), &
                                               Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Atmosphere file '//&
                          TRIM( Atmosphere_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- TEST READ THE Atmosphere STRUCTURE FILE --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test reading the Atmosphere data file...." )' )

  Error_Status = CRTM_Read_Atmosphere_Binary( TRIM( Atmosphere_Filename ), &
                                              Dummy_Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading Atmosphere file '//&
                          TRIM( Atmosphere_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- DESTROY ALL THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  ! ------------------------
  ! The AtmProfile structure
  ! ------------------------

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure.', &
                          WARNING )
  END IF


  ! -------------------------------
  ! The Atmosphere structure arrays
  ! -------------------------------

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere, Dummy_Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere structure arrays.', &
                          WARNING )
  END IF


  DEALLOCATE( Atmosphere, Dummy_Atmosphere, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating the Atmosphere structure arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  END IF

END PROGRAM AtmProfile2Atmosphere


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AtmProfile2Atmosphere.f90,v 1.7 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile2Atmosphere.f90,v $
! Revision 1.7  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.6  2005/06/15 17:49:53  paulv
! - Removed YES and NO flags.
!
! Revision 1.5  2005/03/28 16:30:00  paulv
! - Due to changes in the Atmosphere data structure:
!   o Level_Pressure assignment is now done for ALL levels, 0->K, not just for
!     the layer dimensions.
!   o Level_Temperature component of Atmosphere structure is now filled.
!   o Level_Temperature_Input flag component of ATmosphere structure is set.
!
! Revision 1.4  2005/02/25 17:47:54  paulv
! - Added dummy aerosol dimension (0) to Atmosphere structure allocation.
!
! Revision 1.3  2004/11/03 20:15:54  paulv
! - Using new CRTM and AtmProfile modules.
! - Added profile index to AtmProfile pressure array copy.
!
! Revision 1.2  2004/09/24 17:31:23  paulv
! - Now using Fortran-95 modules. Initialisation calls no longer required.
!
! Revision 1.1  2004/07/02 18:57:39  paulv
! Initial checkin.
!
!
!
!
