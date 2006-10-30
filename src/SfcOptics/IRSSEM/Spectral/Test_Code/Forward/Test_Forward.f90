!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Forward
!
! PURPOSE:
!       Program to test the forward CRTM IRSSEM.
!
! CATEGORY:
!       CRTM : SfcOptics : IRSSEM : Test
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility routines
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               and relational comparisons on floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
!       Emissivity_Define:      Module defining the Spectral_Emissivity and
!                               Sensor_Emissivity data structures and
!                               containing routines to manipulate them.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       Emissivity_netCDF_IO:   Module containing routines to read and write
!                               netCDF format emissivity files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     EMISSIVITY_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
!       CRTM_IRSSEM:            Module containing function to invoke the CRTM
!                               Spectral Infrared Sea Surface Emissivity
!                               Model (IRSSEM).


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
!       Input:
!       - netCDF Dependent Spectral Emissivity file
!       - netCDF Independent Spectral Emissivity file
!       - Binary EmisCoeff file
!
!       Output:
!       - netCDF Dependent Spectral Emissivity difference file.
!       - netCDF Independent Spectral Emissivity difference file.
!
! SIDE EFFECTS:
!       The output files are overwritten if they already exists.
!
! RESTRICTIONS:
!       The required input SensorEmissivity data files must be in the
!       directory in which this program is executed.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jul-2005
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

PROGRAM Test_Forward


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers

  USE Emissivity_Define
  USE Emissivity_netCDF_IO

  USE CRTM_IRSSEM


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_Forward'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_Forward.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  CHARACTER( 256 ) :: Spectral_Filename
  CHARACTER( 256 ) :: EmisCoeff_Filename

  CHARACTER( 5000 ) :: Title
  CHARACTER( 5000 ) :: Reflectivity_Type
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment
  CHARACTER( 5000 ) :: Reference

  TYPE( Spectral_Emissivity_type ) :: Spectral
  TYPE( Spectral_Emissivity_type ) :: Difference

  INTEGER :: i, j, k



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to test the Forward Spectral CRTM IRSSEM.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                   -- READ THE Spectral Emissivity DATA --                  #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT     = '( /5x, "Enter the Spectral Emissivity filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Spectral_Filename
  Spectral_Filename = ADJUSTL( Spectral_Filename )


  ! -------------
  ! Read the data
  ! -------------

  Error_Status = Read_Emissivity_netCDF( Spectral_Filename, &
                                         Spectral, &
                                         Reflectivity_Type = Reflectivity_Type, &
                                         History           = History, &
                                         Comment           = Comment, &
                                         Reference         = Reference )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Spectral Emissivity file '//&
                          TRIM( Spectral_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! ------------------------------
  ! Make a copy for the difference
  ! ------------------------------

  Error_Status = Assign_Emissivity( Spectral, Difference )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Spectral emissivity structure', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- LOAD THE CRTM IRSSEM COEFFICIENTS --                  #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Get the filename
  ! ----------------

  WRITE( *, FMT     = '( /5x, "Enter the Spectral EmisCoeff filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) EmisCoeff_Filename
  EmisCoeff_Filename = ADJUSTL( EmisCoeff_Filename )


  ! -------------
  ! Load the data
  ! -------------

  Error_Status = CRTM_Load_EmisCoeff( EmisCoeff_Filename )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error loading CRTM IRSSEM EmisCoeff data from '//&
                          TRIM( EmisCoeff_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#               -- LOOP OVER INPUT EMISSIVITIES AND COMPUTE --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Computing the emissivities and differences..." )' )

  DO k = 1, Spectral%n_Wind_Speeds
    DO j = 1, Spectral%n_Frequencies

      ! -- Compute the emissivity for all angles
      Error_Status = CRTM_Compute_IRSSEM( Spectral%Wind_Speed(k), &
                                          Spectral%Frequency(j),  &
                                          Spectral%View_Angle, &
                                          Difference%Emissivity(j,k,:) )

      ! -- Compute the emissivity difference
      Difference%Emissivity(j,k,:) = Spectral%Emissivity(j,k,:) - &
                                     Difference%Emissivity(j,k,:)

    END DO
  END DO

  DO j = 1, Spectral%n_Frequencies
    DO i = 1, Spectral%n_View_Angles

      Difference%Emissivity(j,:,i) = MAXVAL( ABS( Difference%Emissivity(j,:,i) ) )

    END DO
  END DO



  !#----------------------------------------------------------------------------#
  !#                   -- OUTPUT THE EMISSIVITY DIFFERENCE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing emissivity difference to output file..." )' )

  Title   = 'CRTM IRSSEM true-computed emissivity differences'
  History = PROGRAM_RCS_ID//'; '//TRIM( History )
  Comment = 'Spectral_Emissivity file: '//TRIM( Spectral_Filename )//', '//&
            'Spectral_EmisCoeff file: '//TRIM( EmisCoeff_Filename)//'; '//&
            TRIM( Comment )

  Error_Status = Write_Emissivity_netCDF( 'Difference.Spectral_Emissivity.nc', &
                                          Difference, &
                                          Title             = TRIM( Title ), &
                                          Reflectivity_Type = TRIM( Reflectivity_Type ), &
                                          History           = TRIM( History ), &
                                          Comment           = TRIM( Comment ), &
                                          Reference         = TRIM( Reference ) )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the Difference Spectral Emissivity file.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- DESTROY THE CRTM IRSSEM --                        #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_EmisCoeff()

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the CRTM IRSSEM EmisCoeff data.', &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- DESTROY THE DATA STRUCTURES --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_Emissivity( Difference )
  Error_Status = Destroy_Emissivity( Spectral )


END PROGRAM Test_Forward


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Forward.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Forward.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/07/21 16:32:56  paulv
! Initial checkin.
!
!
!
