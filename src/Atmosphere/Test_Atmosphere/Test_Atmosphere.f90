!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Atmosphere
!
! PURPOSE:
!       Program to test the CRTM Atmosphere structure manipulation and
!       I/O functions
!
! CATEGORY:
!       CRTM : Atmosphere : Test
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
!       CRTM_Atmosphere_Define:      Module defining the Atmosphere data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       CRTM_Atmosphere_Binary_IO:   Module containing routines to read and write
!                                    Atmosphere Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_ATMOSPHERE_DEFINE module
!                                          CRTM_CLOUD_BINARY_IO module
!                                          CRTM_AEROSOL_BINARY_IO module
!
!       CRTM_Cloud_Binary_IO:        Module containing routines to read and write
!                                    Cloud Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_CLOUD_DEFINE module
!
!       CRTM_Aerosol_Binary_IO:      Module containing routines to read and write
!                                    Aerosol Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_AEROSOL_DEFINE module
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
!       Output: Binary format Aerosol, Cloud, and Atmosphere file.
!
! SIDE EFFECTS:
!       If the output files already exists, they are overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Mar-2005
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

PROGRAM Test_Atmosphere


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO

  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Test_Atmosphere'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Atmosphere.f90,v 1.4 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER ::    AEROSOL_FILENAME = 'Test.Aerosol.bin'
  CHARACTER( * ), PARAMETER ::      CLOUD_FILENAME = 'Test.Cloud.bin'
  CHARACTER( * ), PARAMETER :: ATMOSPHERE_FILENAME = 'Test.Atmosphere.bin'

  INTEGER, PARAMETER :: MAX_N_LOOPS  = 10000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500

  INTEGER, PARAMETER :: N_PROFILES  = 6
  INTEGER, PARAMETER :: N_LAYERS    = 100
  INTEGER, PARAMETER :: N_ABSORBERS = 3
  INTEGER, PARAMETER :: N_CLOUDS    = 6
  INTEGER, PARAMETER :: N_AEROSOLS  = 7

  INTEGER, PARAMETER :: SET = 1

  REAL( fp_kind ), PARAMETER :: W1 = 10.0_fp_kind
  REAL( fp_kind ), PARAMETER :: W2 =  0.5_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER          :: pn_pos
  CHARACTER( 80 )  :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: j, m, mc, ma, n


  TYPE( CRTM_Cloud_type ),      DIMENSION( N_CLOUDS )   :: Cloud
  TYPE( CRTM_Aerosol_type ),    DIMENSION( N_AEROSOLS ) :: Aerosol
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere, &
                                                           Atmosphere_Copy
                                                           



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test all the CRTM Atmosphere structure",&
             &/5x, "   manipulation and I/O functions.")' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                -- ALLOCATE THE Atmosphere STRUCTURE ARRAY --               #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Allocate_Atmosphere( N_LAYERS,     &  ! Input
                                           N_ABSORBERS,  &  ! Input
                                           N_CLOUDS,     &  ! Input
                                           N_AEROSOLS,   &  ! Input
                                           Atmosphere    )  ! Output

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Atmosphere structure array.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#             -- FILL Atmosphere STRUCTURE ARRAY WITH FAKE DATA --           #
  !#----------------------------------------------------------------------------#

  DO m = 1, N_PROFILES

    Atmosphere(m)%Climatology    = m
    Atmosphere(m)%Absorber_ID    = (/ (j, j = 1, N_ABSORBERS ) /)
    Atmosphere(m)%Absorber_Units = SPECIFIC_AMOUNT_UNITS

    Atmosphere(m)%Level_Pressure    = 1.0_fp_kind
    Atmosphere(m)%Level_Temperature = 2.0_fp_kind
    Atmosphere(m)%Pressure          = 3.0_fp_kind
    Atmosphere(m)%Temperature       = 4.0_fp_kind
    Atmosphere(m)%Absorber          = 5.0_fp_kind

    DO mc = 1, N_CLOUDS
      Atmosphere(m)%Cloud(mc)%Type = mc
      Atmosphere(m)%Cloud(mc)%Effective_Radius   = 6.0_fp_kind
      Atmosphere(m)%Cloud(mc)%Effective_Variance = 7.0_fp_kind
      Atmosphere(m)%Cloud(mc)%Water_Content      = 8.0_fp_kind
    END DO
  
    DO ma = 1, N_AEROSOLS
      Atmosphere(m)%Aerosol(ma)%Type = ma
      Atmosphere(m)%Aerosol(ma)%Effective_Radius   =  9.0_fp_kind
      Atmosphere(m)%Aerosol(ma)%Effective_Variance = 10.0_fp_kind
      Atmosphere(m)%Aerosol(ma)%Concentration      = 11.0_fp_kind
    END DO

  END DO



  !#----------------------------------------------------------------------------#
  !#                      -- TEST THE WEIGHTED SUM ROUTINE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing WeightedSum functions ..." )' )


  ! ------------------------
  ! Copy the structure array
  ! ------------------------

  Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_Copy )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying Atmophere structure array for WeightedSum test', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------
  ! Compute the weighted sum
  ! ------------------------

  Error_Status = CRTM_WeightedSum_Atmosphere( Atmosphere, &
                                              Atmosphere_Copy, &
                                              W1, &
                                              w2 = W2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing Atmosphere weighted sum', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Print out some results
  ! ----------------------

  DO m = 1, N_PROFILES

    WRITE( *, '( 5x, "Profile #", i3 )' ) m

    WRITE( *, * ) Atmosphere(m)%Level_Pressure(1), &
                  Atmosphere(m)%Level_Temperature(1), &
                  Atmosphere(m)%Pressure(1), &
                  Atmosphere(m)%Temperature(1), &
                  Atmosphere(m)%Absorber(1,1)

    DO mc = 1, N_CLOUDS
      WRITE( *, '( 10x, "Cloud #", i3 )' ) mc
      WRITE( *, *) Atmosphere(m)%Cloud(mc)%Effective_Radius(1), &
                   Atmosphere(m)%Cloud(mc)%Effective_Variance(1), &
                   Atmosphere(m)%Cloud(mc)%Water_Content(1)
    END DO
  
    DO ma = 1, N_AEROSOLS
      WRITE( *, '( 10x, "Aerosol #", i3 )' ) ma
      WRITE( *, *) Atmosphere(m)%Aerosol(ma)%Effective_Radius(1,1), &
                   Atmosphere(m)%Aerosol(ma)%Effective_Variance(1,1), &
                   Atmosphere(m)%Aerosol(ma)%Concentration(1,1)
    END DO

  END DO
  


  !#----------------------------------------------------------------------------#
  !#                        -- TEST THE ZERO ROUTINE --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing Zero subroutines ..." )' )


  ! ------------------------
  ! Zero the structure array
  ! ------------------------

  CALL CRTM_Zero_Atmosphere( Atmosphere_Copy )


  ! ----------------------
  ! Print out some results
  ! ----------------------

  DO m = 1, N_PROFILES

    WRITE( *, '( 5x, "Profile #", i3 )' ) m

    WRITE( *, * ) Atmosphere_Copy(m)%Level_Pressure(1), &
                  Atmosphere_Copy(m)%Level_Temperature(1), &
                  Atmosphere_Copy(m)%Pressure(1), &
                  Atmosphere_Copy(m)%Temperature(1), &
                  Atmosphere_Copy(m)%Absorber(1,1)

    DO mc = 1, N_CLOUDS
      WRITE( *, '( 10x, "Cloud #", i3 )' ) mc
      WRITE( *, *) Atmosphere_Copy(m)%Cloud(mc)%Effective_Radius(1), &
                   Atmosphere_Copy(m)%Cloud(mc)%Effective_Variance(1), &
                   Atmosphere_Copy(m)%Cloud(mc)%Water_Content(1)
    END DO
  
    DO ma = 1, N_AEROSOLS
      WRITE( *, '( 10x, "Aerosol #", i3 )' ) ma
      WRITE( *, *) Atmosphere_Copy(m)%Aerosol(ma)%Effective_Radius(1,1), &
                   Atmosphere_Copy(m)%Aerosol(ma)%Effective_Variance(1,1), &
                   Atmosphere_Copy(m)%Aerosol(ma)%Concentration(1,1)
    END DO

  END DO
  


  !#----------------------------------------------------------------------------#
  !#                      -- TEST THE Cloud I/O FUNCTIONS --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing Cloud I/O functions ..." )' )


  ! -----------------------
  ! Write the test datafile
  ! -----------------------

  WRITE( *, '( 10x, "Writing test Cloud datafile ..." )' )

  Error_Status = CRTM_Write_Cloud_Binary( CLOUD_FILENAME, &
                                          Atmosphere(1)%Cloud )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Cloud data file.', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Test read the datafile
  ! ----------------------

  WRITE( *, '( 10x, "Looping for Cloud Binary read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = CRTM_Read_Cloud_Binary( CLOUD_FILENAME, &
                                           Cloud )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Cloud datafile on attempt # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                     -- TEST THE Aerosol I/O FUNCTIONS --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing Aerosol I/O functions ..." )' )


  ! -----------------------
  ! Write the test datafile
  ! -----------------------

  WRITE( *, '( 10x, "Writing test Aerosol datafile ..." )' )

  Error_Status = CRTM_Write_Aerosol_Binary( AEROSOL_FILENAME, &
                                            Atmosphere(1)%Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Aerosol data file.', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Test read the datafile
  ! ----------------------

  WRITE( *, '( 10x, "Looping for Aerosol Binary read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = CRTM_Read_Aerosol_Binary( AEROSOL_FILENAME, &
                                             Aerosol )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Aerosol datafile on attempt # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                   -- TEST THE Atmosphere I/O FUNCTIONS --                  #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Testing Atmosphere I/O functions ..." )' )


  ! -----------------------
  ! Write the test datafile
  ! -----------------------

  WRITE( *, '( 10x, "Writing test Atmosphere datafile ..." )' )

  Error_Status = CRTM_Write_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                               Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Atmosphere data file.', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Test read the datafile
  ! ----------------------

  WRITE( *, '( 10x, "Looping for Atmosphere Binary read memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                                Atmosphere, &
                                                Quiet = SET )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading Atmophere datafile on attempt # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                       -- LOOP FOR ASSIGN LEAK TEST --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Looping for Atmosphere structure copy memory leak test ..." )' )

  DO n = 1, MAX_N_LOOPS

    Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_Copy )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying Atmophere structure array on attempt # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO




  !#----------------------------------------------------------------------------#
  !#                     -- DESTROY THE STRUCTURE ARRAYS --                     #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere, Atmosphere_Copy )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = CRTM_Destroy_Cloud( Cloud )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Cloud structure array.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = CRTM_Destroy_Aerosol( Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Aerosol structure array.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_Atmosphere


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Atmosphere.f90,v 1.4 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Atmosphere.f90,v $
! Revision 1.4  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.3  2005/08/17 17:09:44  paulv
! - Added structure zero subroutine tests.
!
! Revision 1.2  2005/06/15 23:12:52  paulv
! - Added test for WeightedSum() functions.
!
! Revision 1.1  2005/03/24 15:11:29  paulv
! Initial checkin.
!
!
!
!
