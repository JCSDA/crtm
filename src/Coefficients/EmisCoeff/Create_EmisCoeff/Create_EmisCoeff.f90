!------------------------------------------------------------------------------
!P+
! NAME:
!       Create_Spectral_EmisCoeff
!
! PURPOSE:
!       Program to create the EmisCoeff datafiles for the CRTM spectral
!       emissivity model.
!
! CATEGORY:
!       CRTM : Emissivity : Spectral Emissivity Model
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
!       Compare_Float_Numbers:      Module containing routines to perform equality
!                                   check comparisons on input floating point
!                                   numbers.
!                                   USEs: TYPE_KINDS module
!
!       Emissivity_Define:          Module defining the Emissivity data structures
!                                   and their manipulation routines.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       Emissivity_netCDF_IO:       Module containing routines to read and write
!                                   netCDF format Emissivity files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         EMISSIVITY_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       EmisCoeff_Define:           Module defining the EmisCoeff data structure
!                                   and its manipulation routines.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       EmisCoeff_netCDF_IO:        Module containing routines to read and write
!                                   netCDF format EmisCoeff files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         EMISCOEFF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
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
!       Input:
!         - Dependent netCDF format Spectral Emissivity data file
!
!       Output:
!         - netCDF format Spectral EmisCoeff data file
!
! SIDE EFFECTS:
!       If any of the output files exist, they are overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
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
!P-
!------------------------------------------------------------------------------

PROGRAM Create_Spectral_EmisCoeff


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE Emissivity_Define
  USE Emissivity_netCDF_IO

  USE EmisCoeff_Define
  USE EmisCoeff_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_Spectral_EmisCoeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_EmisCoeff.f90,v 2.2 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'



  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Spectral_Filename
  CHARACTER( 256 ) :: EmisCoeff_Filename

  INTEGER :: Error_Status

  CHARACTER( 2000 ) :: Reflectivity_Type
  CHARACTER( 2000 ) :: Title
  CHARACTER( 2000 ) :: History
  CHARACTER( 2000 ) :: Comment
  CHARACTER( 2000 ) :: Reference

  TYPE( Spectral_Emissivity_type ) :: Spectral
  TYPE( EmisCoeff_type )           :: EmisCoeff

integer :: j, k

  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create the EmisCoeff datafiles for the")' )
  WRITE( *, '( 5x, "    spectral emissivity model.")' )
  WRITE( *, '(/5x, " $Revision: 2.2 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                 -- READ THE SPECTRAL EMISSIVITY DATA FILE --               #
  !#----------------------------------------------------------------------------#

  ! -- Get the filename
  WRITE( *, FMT     = '( /5x, "Enter the INPUT Spectral Emissivity filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Spectral_Filename
  Spectral_Filename = ADJUSTL( Spectral_Filename )

  ! -- Read the data
  Error_Status = Read_Emissivity_netCDF( Spectral_Filename, &
                                         Spectral, &
                                         Reflectivity_Type = Reflectivity_Type, &
                                         Title             = Title, &
                                         History           = History, &
                                         Comment           = Comment, &
                                         Reference         = Reference )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Spectral Emissivity file '//&
                          TRIM( Spectral_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- ALLOCATE THE EmisCoeff STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_EmisCoeff( Spectral%n_View_Angles, &
                                     Spectral%n_Frequencies, &
                                     Spectral%n_Wind_Speeds, &
                                     EmisCoeff )
                                      
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred allocating EmisCoeff structure.', &
                          Error_Status )
    STOP
  END IF


  ! ------------------
  ! Copy over the data
  ! ------------------

  EmisCoeff%Frequency  = Spectral%Frequency
  EmisCoeff%Wind_Speed = Spectral%Wind_Speed
  EmisCoeff%Angle      = Spectral%View_Angle

  ! -- Copy over the Spectral_Emissivity
  ! --   n_Frequencies x n_Wind_Speeds x n_Angles
  ! -- emissivity data and reshape it into the
  ! --   n_Angles x n_Frequencies x n_Wind_Speeds
  ! -- EmisCoeff emissivity data array.
!  EmisCoeff%Emissivity = RESHAPE( Spectral%Emissivity, &
!                                  SHAPE( EmisCoeff%Emissivity ), &
!                                  ORDER=(/3,1,2/) )
  DO k = 1, EmisCoeff%n_Wind_Speeds
    DO j = 1, EmisCoeff%n_Frequencies
      EmisCoeff%Emissivity(:,j,k) = Spectral%Emissivity(j,k,:)
    END DO
  END DO



  !#----------------------------------------------------------------------------#
  !#                -- WRITE THE SPECTRAL EMISCOEFF DATA FILE --                #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Spectral EmisCoeff filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) EmisCoeff_Filename
  EmisCoeff_Filename = ADJUSTL( EmisCoeff_Filename )


  ! --------------
  ! Write the data
  ! --------------

  Error_Status = Write_EmisCoeff_netCDF( EmisCoeff_Filename, &
                                         EmisCoeff )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the Spectral EmisCoeff file '//&
                          TRIM( EmisCoeff_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                          -- DESTROY THE STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! Destroy the EmisCoeff structure
  ! -------------------------------

  Error_Status = Destroy_EmisCoeff( EmisCoeff )
                                      
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff structure.', &
                          Error_Status )
  END IF


  ! -----------------------------------------
  ! Destroy the spectral emissivity structure
  ! -----------------------------------------

  Error_Status = Destroy_Emissivity( Spectral )
                                      
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Spectral Emissivity structure.', &
                          Error_Status )
  END IF

END PROGRAM Create_Spectral_EmisCoeff


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_EmisCoeff.f90,v 2.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_EmisCoeff.f90,v $
! Revision 2.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.1  2005/07/29 16:29:04  paulv
! - Corrected bug in emissivity data reordering.
!
! Revision 2.0  2005/07/19 15:15:48  paulv
! - Updated to Release 2.0. Emissivity derivative is no longer computed and
!   output.
!
! Revision 1.3  2005/06/20 22:05:32  paulv
! - Major update to perform the interpolation as required in the CRTM.
!
! Revision 1.2  2005/06/16 00:07:51  paulv
! - Initial working version that performs the interpolation and outputs
!   differences. Number crunching currently handled by internal subprograms.
!
! Revision 1.1  2005/06/13 17:47:19  paulv
! Initial checkin.
!
!
!
