!------------------------------------------------------------------------------
!M+
! NAME:
!       Example_AtmProfile_Binary
!
! PURPOSE:
!       Example program to demonstrate how to read AtmProfile Binary format files.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       AtmProfile_Define:          Module defining the AtmProfile data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       AtmProfile_Binary_IO:       Module containing routines to read and
!                                   write AtmProfile Binary format files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         ATMPROFILE_DEFINE module
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
!       Input Binary AtmProfile data file
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
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

PROGRAM Example_AtmProfile_Binary


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_Binary_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Example_AtmProfile_Binary'


  ! ---------
  ! Variables
  ! ---------

  INTEGER                 :: Error_Status
  CHARACTER( 256 )        :: BIN_Filename
  TYPE( AtmProfile_type ) :: AtmProfile

  INTEGER :: m



  !#----------------------------------------------------------------------------#
  !#                     -- ENTER THE INPUT FILENAME --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the INPUT Binary AtmProfile file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_Filename )



  !#----------------------------------------------------------------------------#
  !#                     -- READ THE BINARY DATA FILE --                        #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_AtmProfile_Binary( TRIM( BIN_fileNAME ), &
                                         AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading Binary AtmProfile file '//&
                          TRIM( BIN_fileNAME ), &
                          Error_Status )
    STOP
  END IF


  ! -----------------------
  ! Output some information
  ! -----------------------

  WRITE( *, '( /5x, a, " file dimensions:", &
              &/10x, "n_Layers    = ", i5, &
              &/10x, "n_Absorbers = ", i5, &
              &/10x, "n_Profiles  = ", i5  )' ) &
            TRIM( BIN_Filename ), AtmProfile%n_Layers, &
                                  AtmProfile%n_Absorbers, &
                                  AtmProfile%n_Profiles

  WRITE( *, '( /5x, "LEVEL pressures: ",/, &
              &8(2x, f9.4) )' ) AtmProfile%Level_Pressure


  m = AtmProfile%n_Profiles / 2
  WRITE( *, '( /5x, "Profile #", i2, " LAYER temperatures: ",/, &
              &10(2x, f7.3) )' ) m, AtmProfile%Layer_Temperature( :, m )



  !#----------------------------------------------------------------------------#
  !#                   -- DESTROY THE AtmProfile STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status  = Destroy_AtmProfile(  AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure', &
                          WARNING )
  END IF

END PROGRAM Example_AtmProfile_Binary


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Example_AtmProfile_Binary.f90,v 1.2 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Example_AtmProfile_Binary.f90,v $
! Revision 1.2  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.1  2004/08/27 20:12:21  paulv
! Initial checkin.
!
!
!
