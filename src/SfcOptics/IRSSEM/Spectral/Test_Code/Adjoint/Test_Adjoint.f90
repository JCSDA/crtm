!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Adjoint
!
! PURPOSE:
!       Program to test the Adjoint CRTM IRSSEM component with respect to the
!       Tangent-Linear component.
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
!       CRTM_IRSSEM:            Module containing function to invoke the CRTM
!                               Spectral Infrared Sea Surface Emissivity
!                               Model (IRSSEM).
!
!       TLADMtest_Define:       Module defining the TLADMtest data structures
!                               and containing routines to manipulate them.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       TLADMtest_netCDF_IO:    Module containing routines to read and write
!                               netCDF format TLADMtest files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     TLADMETEST_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
!
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
!       - Binary EmisCoeff file
!
!       Output:
!       - netCDF TLADMtest file.
!
! SIDE EFFECTS:
!       The output file is overwritten if they already exists.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2005
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

PROGRAM Test_Adjoint


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE CRTM_IRSSEM

  USE TLADMtest_Define
  USE TLADMtest_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_Adjoint'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_Adjoint.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  CHARACTER( * ), PARAMETER :: TLADMTEST_FILENAME = 'CRTM_IRSSEM.SurfaceTLADMtest.nc'
  CHARACTER( * ), PARAMETER :: EMISCOEFF_FILENAME = 'Independent.EmisCoeff.bin'

  REAL( fp_kind ), PARAMETER :: ANGLE_BEGIN =  0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ANGLE_END   = 60.0_fp_kind
  REAL( fp_kind ), PARAMETER :: D_ANGLE     =  1.0_fp_kind

  REAL( fp_kind ), PARAMETER :: FREQUENCY_BEGIN =  600.0_fp_kind
  REAL( fp_kind ), PARAMETER :: FREQUENCY_END   = 3000.0_fp_kind
  REAL( fp_kind ), PARAMETER :: D_FREQUENCY     =   10.0_fp_kind

  REAL( fp_kind ), PARAMETER :: WIND_SPEED_BEGIN =  0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: WIND_SPEED_END   = 15.0_fp_kind
  REAL( fp_kind ), PARAMETER :: D_WIND_SPEED     =  0.25_fp_kind

  INTEGER, PARAMETER :: N_VARIABLES = 1
  INTEGER, PARAMETER :: NV_WIND_SPEED = 1
  CHARACTER( * ), PARAMETER, DIMENSION( N_VARIABLES ) :: &
    VARIABLE_NAME = (/ 'Wind Speed' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_VARIABLES ) :: &
    VARIABLE_UNITS = (/ 'm.s^-1' /)

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 5000 ) :: Comment

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER :: i, n_Angles
  INTEGER :: j, n_Frequencies
  INTEGER :: k, n_Wind_Speeds
  INTEGER :: m,  n
  INTEGER :: nV

  INTEGER :: New_File

  ! -- Structure for test results
  TYPE( SurfaceTLADMtest_type ) :: TLADMtest

  ! -- Forward inputs
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: Angle
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: Frequency
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: Wind_Speed

  ! -- Tangent-linear inputs
  REAL( fp_kind ) :: Wind_Speed_TL

  ! -- Adjoint outputs
  REAL( fp_kind ) :: Wind_Speed_AD

  ! -- Tangent-linear outputs
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: Emissivity_TL

  ! -- Adjoint inputs
  REAL( fp_kind ), DIMENSION(:), ALLOCATABLE :: Emissivity_AD



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
  WRITE( *, '( 5x, " Program to test the Adjoint Spectral CRTM IRSSEM.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#              -- ENTER A COMMENT STRING FOR THE OUTPUT FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a comment attribute string for the output file:" )' )
  READ( *, FMT = '( a )' ) Comment



  !#----------------------------------------------------------------------------#
  !#                   -- LOAD THE CRTM IRSSEM COEFFICIENTS --                  #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Loading the CRTM IRSSEM coefficients..." )' )

  Error_Status = CRTM_Load_EmisCoeff( EMISCOEFF_FILENAME )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error loading CRTM IRSSEM EmisCoeff data from '//&
                          TRIM( EMISCOEFF_FILENAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                           -- ALLOCATE ARRAYS --                            #
  !#----------------------------------------------------------------------------#

  n_Angles = INT( 1.5_fp_kind + &
                  ( ( ANGLE_END - ANGLE_BEGIN ) / &
  !                 ---------------------------
                              D_ANGLE           ) )

  n_Frequencies = INT( 1.5_fp_kind + &
                       ( ( FREQUENCY_END - FREQUENCY_BEGIN ) / &
  !                      -----------------------------------
                                     D_FREQUENCY             ) )

  n_Wind_Speeds = INT( 1.5_fp_kind + &
                       ( ( WIND_SPEED_END - WIND_SPEED_BEGIN ) / &
  !                      -------------------------------------
                                     D_WIND_SPEED              ) )


  ALLOCATE( Angle( n_Angles ), &
            Frequency( n_Frequencies ), &
            Wind_Speed( n_Wind_Speeds ), &
            Emissivity_TL( n_Frequencies ), &
            Emissivity_AD( n_Frequencies ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                         -- COMPUTE INPUT DATA --                           #
  !#----------------------------------------------------------------------------#

  DO k = 1, n_Wind_Speeds
    Wind_Speed(k) = WIND_SPEED_BEGIN + ( REAL( k-1, fp_kind ) * D_WIND_SPEED )
  END DO
  DO j = 1, n_Frequencies
    Frequency(j) = FREQUENCY_BEGIN + ( REAL( j-1, fp_kind ) * D_FREQUENCY )
  END DO
  DO i = 1, n_Angles
    Angle(i) = ANGLE_BEGIN + ( REAL( i-1, fp_kind ) * D_ANGLE )
  END DO



  !#----------------------------------------------------------------------------#
  !#                     -- ALLOCATE TLADMtest STRUCTURE --                     #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_TLADMtest( n_Frequencies, &
                                     n_Variables, &
                                     TLADMtest )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating TLADMtest structure', &
                          Error_Status )
    STOP
  END IF  


  ! ----------------------------
  ! Initialise some data entries
  ! ----------------------------

  TLADMtest%DataType = TLADMTEST_SPECTRAL_TYPE

  TLADMtest%Output_Variable_Name  = 'Emissivity'
  TLADMtest%Output_Variable_Units = ' '

  TLADMtest%Frequency      = Frequency
  TLADMtest%Variable_Name  = VARIABLE_NAME



  !#----------------------------------------------------------------------------#
  !#                        -- BEGIN PROCESSING DATASETS --                     #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! Initialise the data set counter
  ! and output file status
  ! -------------------------------

  m = 0
  New_File = SET


  ! ---------------------
  ! Begin wind speed loop
  ! ---------------------

  Wind_Speed_Loop: DO k = 1, n_Wind_Speeds


    WRITE( *, '( 5x, "Processing wind speed: ", f5.2, " m.s^-1" )' ) Wind_Speed(k)


    ! ----------------
    ! Begin angle loop
    ! ----------------

    Angle_Loop: DO i = 1, n_Angles


      !#------------------------------------------------------------------------#
      !#               -- BEGIN THE TANGENT-LINEAR VARIABLE LOOP --             #
      !#------------------------------------------------------------------------#

      TL_Variable_Loop: DO nV = 1, N_VARIABLES



        !#----------------------------------------------------------------------#
        !#                     -- PERTURB THE VARIABLES --                      #
        !#----------------------------------------------------------------------#

        TL_Variable_Select: SELECT CASE ( nV )


          ! --------------
          ! The wind speed
          ! --------------

          CASE( NV_WIND_SPEED )

            ! -- Perturb the wind speed
            Wind_Speed_TL = ONE

            ! -- Run the TL model
            DO j = 1, n_Frequencies

              Error_Status = CRTM_Compute_IRSSEM_TL( Wind_Speed(k), &
                                                     Frequency(j),  &
                                                     Angle(i:i), &
                                                     Wind_Speed_TL, &
                                                     Emissivity_TL(j:j) )

              IF ( Error_Status /= SUCCESS ) THEN 
                CALL Display_Message( PROGRAM_NAME, &
                                      'Error in CRTM_Compute_IRSSEM_TL call', &
                                       Error_Status )                           
                STOP
              END IF

            END DO


            ! -- Save the data for this dataset
            TLADMtest%d_TL(:,nV) = Emissivity_TL


          ! ------------
          ! Default case
          ! ------------

          CASE DEFAULT

            CALL Display_Message( PROGRAM_NAME, &
                                  'Invalid TL variable. How did we get here!?!?!?', &
                                   FAILURE )
            STOP

        END SELECT TL_Variable_Select

      END DO TL_Variable_Loop



      !#------------------------------------------------------------------------#
      !#                  -- BEGIN THE ADJOINT VARIABLE LOOP --                 #
      !#------------------------------------------------------------------------#

      AD_Variable_Loop: DO nV = 1, N_VARIABLES



        !#----------------------------------------------------------------------#
        !#                     -- PERTURB THE VARIABLES --                      #
        !#----------------------------------------------------------------------#

        AD_Variable_Select: SELECT CASE ( nV )


          ! --------------
          ! The wind speed
          ! --------------

          CASE( NV_WIND_SPEED )

            ! -- Run the AD model
            DO j = 1, n_Frequencies

              ! -- Zero out the adjoint wind speed
              Wind_Speed_AD = ZERO

              ! -- Perturb the adjoint emissivity
              Emissivity_AD(j) = ONE


              Error_Status = CRTM_Compute_IRSSEM_AD( Wind_Speed(k), &
                                                     Frequency(j),  &
                                                     Angle(i:i), &
                                                     Emissivity_AD(j:j), &
                                                     Wind_Speed_AD )

              IF ( Error_Status /= SUCCESS ) THEN 
                CALL Display_Message( PROGRAM_NAME, &
                                      'Error in CRTM_Compute_IRSSEM_AD call', &
                                       Error_Status )                           
                STOP
              END IF


              ! -- Save the data for this dataset
              TLADMtest%d_AD(j,nV) = Wind_Speed_AD

            END DO


          ! ------------
          ! Default case
          ! ------------

          CASE DEFAULT

            CALL Display_Message( PROGRAM_NAME, &
                                  'Invalid AD variable. How did we get here!?!?!?', &
                                   FAILURE )
            STOP

        END SELECT AD_Variable_Select

      END DO AD_Variable_Loop



      !#----------------------------------------------------------------------#
      !#      -- WRITE THE CURRENT DATASET TLADMtest STRUCTURE TO FILE --     #
      !#----------------------------------------------------------------------#

      ! -------------------------
      ! Increment dataset counter
      ! -------------------------

      m = m + 1


      ! ---------------------
      ! Create a dataset name
      ! ---------------------

      WRITE( TLADMtest%nM_Name, '( "Wind Speed:",f6.2,"m.s^-1;  Angle:",f6.2,"deg." )' ) &
                                Wind_Speed(k), Angle(i)



      ! ------------------------------------------------------
      ! Set the current dataset number in the output structure
      ! ------------------------------------------------------

      TLADMtest%nM = m


      ! --------------
      ! Write the data
      ! --------------

      Error_Status = Write_TLADMtest_netCDF( TLADMtest_FILENAME, &
                                             TLADMtest, &
                                             New_File = New_File, &
                                             Title   = 'TL/AD Spectral CRTM IRSSEM test results', &
                                             History = PROGRAM_RCS_ID, &
                                             Comment = TRIM( Comment ), &
                                             ID_Tag = 'Spectral Test' )

      IF ( Error_Status /= SUCCESS ) THEN 
        WRITE( Message, '( "Error writing TLADMtest structure for angle #", i5, &
                          &", wind speed #", i5, " to ", a )' ) &
                        i, k, TLADMtest_FILENAME
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )                           
        STOP
      END IF


      ! -----------------------
      ! Unset the new file flag
      ! -----------------------

      New_File = UNSET

    END DO Angle_Loop
  END DO Wind_Speed_Loop



  !#----------------------------------------------------------------------------#
  !#                       -- DESTROY TLADMtest STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_TLADMtest( TLADMtest )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred destroying TLADMtest structure', &
                           WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                          -- DEALLOCATE ARRAYS --                           #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Angle, &
              Frequency, &
              Wind_Speed, &
              Emissivity_TL, &
              Emissivity_AD, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
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

END PROGRAM Test_Adjoint


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Adjoint.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Adjoint.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/07/28 20:47:28  paulv
! Initial checkin.
!
!
!
