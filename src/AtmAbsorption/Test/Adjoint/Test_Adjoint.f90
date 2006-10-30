!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Adjoint
!
! PURPOSE:
!       Program to test the CRTM AtmAbsorption Adjoint code.
!
! CATEGORY:
!       CRTM : AtmAbsorption : Test
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                Module containing definitions for kinds
!                                  of variable types.
!
!       Message_Handler:           Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       CRTM_Parameters:           Module of parameter definitions for the CRTM.
!                                  USEs: TYPE_KINDS module
!
!       CRTM_LifeCycle:            Module containing CRTM life cycle functions
!                                  to initialize and destroy the CRTM space.
!                                  USEs: ERROR_HANDLER module
!                                        CRTM_SPCCOEFF module
!                                        CRTM_TAUCOEFF module
!                                        CRTM_AEROSOLCOEFF module
!                                        CRTM_SCATTERCOEFF module
!                                        CRTM_CHANNELINFO_DEFINE module
!                                        CRTM_CHANNELINFO module
!  
!       CRTM_GeometryInfo_Define:  Module defining the CRTM GeometryInfo data
!                                  structure.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo_Define:   Module defining the CRTM ChannelInfo data
!                                  structure and containing routines to
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!
!       CRTM_Atmosphere_Define:    Module defining the CRTM Atmosphere
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_CLOUD_DEFINE module
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        BINARY_FILE_UTILITY module
!                                        CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_Atmosphere_Define:    Module defining the CRTM Atmosphere
!                                  structure and containing routines to 
!                                  manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_CLOUD_DEFINE module
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        BINARY_FILE_UTILITY module
!                                        CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_AtmAbsorption:        Module containing routines to compute the
!                                  optical depth profile due to gaseous
!                                  absorption.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        CRTM_PARAMETERS module
!                                        CRTM_TAUCOEFF module
!                                        CRTM_ATMOSPHERE_DEFINE module
!                                        CRTM_GEOMETRYINFO_DEFINE module
!                                        CRTM_ATMABSORPTION_DEFINE module
!                                        CRTM_ATMABSORPTION_INTABSORBER module
!                                        CRTM_ATMABSORPTION_PREDICTOR module
!
!       TLADMtest_Define:          Module defining the structure to hold CRTM
!                                  Tangent-Linear/Adjoint model test results
!                                  and containing routines to manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        ERROR_HANDLER module
!
!       TLADMtest_netCDF_IO:       Module containing routines to read and write
!                                  netCDF format TLADMtest files.
!                                  USEs: TYPE_KINDS module
!                                        FILE_UTILITY module
!                                        ERROR_HANDLER module
!                                        TLADMTEST_DEFINE module
!                                        NETCDF module
!                                        NETCDF_UTILITY module
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
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jan-2005
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

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_LifeCycle
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_AtmAbsorption

  USE TLADMtest_Define
  USE TLADMtest_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Adjoint'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Adjoint.f90,v 1.7 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin.Little_Endian'
  INTEGER,      PARAMETER :: N_PROFILES = 52

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  INTEGER, PARAMETER :: N_VARIABLES = 5
  INTEGER, PARAMETER :: NV_LEVEL_PRESSURE       = 1
  INTEGER, PARAMETER :: NV_LAYER_PRESSURE       = 2
  INTEGER, PARAMETER :: NV_LAYER_TEMPERATURE    = 3
  INTEGER, PARAMETER :: NV_LAYER_WATER_VAPOR    = 4
  INTEGER, PARAMETER :: NV_LAYER_OZONE          = 5
  CHARACTER( * ), PARAMETER, DIMENSION( N_VARIABLES ) :: &
    VARIABLE_NAME = (/ 'Level pressure   ', &
                       'Layer pressure   ', &
                       'Layer temperature', &
                       'Layer water vapor', &
                       'Layer ozone      ' /)

  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status,    Status_FWD   
  INTEGER :: Error_Status_TL, Status_TL
  INTEGER :: Error_Status_AD, Status_AD

  CHARACTER( 256 ) :: File_Prefix

  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: AerosolCoeff_File
  CHARACTER( 256 ) :: ScatterCoeff_File
  CHARACTER( 256 ) :: EmisCoeff_File

  CHARACTER( 256 ) :: TLADMtest_File
  INTEGER :: New_File

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER :: H2O_Idx
  INTEGER :: O3_Idx

  INTEGER :: j, k, l, m, nV

  TYPE( CRTM_ChannelInfo_type )   :: ChannelInfo
  TYPE( CRTM_GeometryInfo_type )  :: GeometryInfo
  TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption, &
                                     AtmAbsorption_TL, &
                                     AtmAbsorption_AD
  TYPE( CRTM_Atmosphere_type ), DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Atmosphere_type ) :: Atmosphere_TL, &
                                  Atmosphere_AD

  TYPE( LayerTLADMtest_type ) :: TLADMtest



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the CRTM AtmAbsorption Adjoint")' )
  WRITE( *, '( 5x, "   components with respect to the Tangent-Linear")' )
  WRITE( *, '( 5x, "   components.")' )
  WRITE( *, '(/5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                -- READ THE Atmosphere STRUCTURE DATA FILE --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading the Atmosphere structure file..." )' )

  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                              Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMOSPHERE_FILENAME, & 
                           Error_Status )
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#           -- ASSIGN DUMMY VALUES TO THE GeometryInfo STRUCTURE --          #
  !#----------------------------------------------------------------------------#

  GeometryInfo%Sensor_Scan_Angle   = 0.0_fp_kind   ! Nadir
  GeometryInfo%Sensor_Zenith_Angle = 0.0_fp_kind   ! Nadir

  Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error computing GeometryInfo values.', & 
                           Error_Status )
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- GET THE COEFFICIENT FILENAMES --                    #
  !#----------------------------------------------------------------------------#

  ! ------------------------------------------------
  ! Enter the instrument file prefix, e.g. hirs3_n16
  ! ------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL( File_Prefix )


  ! --------------------
  ! Create the filenames
  ! --------------------

  SpcCoeff_File     = TRIM( File_Prefix )//'.SpcCoeff.bin.Little_Endian'
  TauCoeff_File     = TRIM( File_Prefix )//'.TauCoeff.bin.Little_Endian'
  AerosolCoeff_File = 'AerosolCoeff.bin.Little_Endian'
  ScatterCoeff_File = 'ScatterCoeff.bin.Little_Endian'
  EmisCoeff_File    = 'EmisCoeff.bin.Little_Endian'

  TLADMtest_File = TRIM( File_Prefix )//'.CRTM_AtmAbsorption.TLADMtest.nc'

  New_File = SET



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALISE THE CRTM --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Initializing the CRTM..." )' )

  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            ScatterCoeff_File = ScatterCoeff_File, &
                            EmisCoeff_File    = EmisCoeff_File )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- ALLOCATE TLADMtest STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_TLADMtest( Atmosphere(1)%n_Layers, &
                                     ChannelInfo%n_Channels, &
                                     N_VARIABLES, &
                                     TLADMtest )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred allocating TLADMtest', &
                           Error_Status )                           
    STOP
  END IF


  ! -------------------------
  ! Assign data to TLADMtest
  ! -------------------------

  TLADMtest%Output_Variable_Name  = 'Optical Depth'
  TLADMtest%OUtput_Variable_Units = 'Unitless'

  TLADMtest%DataType      = TLADMTEST_SENSOR_TYPE
  TLADMtest%Pressure      = Atmosphere(1)%Pressure
  TLADMtest%Channel       = ChannelInfo%Sensor_Channel
  TLADMtest%Variable_Name = VARIABLE_NAME



  !#----------------------------------------------------------------------------#
  !#                   -- LOOP OVER ATMOSPHERIC PROFILES --                     #
  !#----------------------------------------------------------------------------#

  Profile_Loop: DO m = 1, N_PROFILES

    WRITE( *, '( 5x, "Processing profile # ", i3 )' ) m


    ! ------------------------------
    ! Determine the absorber indices
    ! ------------------------------

    ! -- Water vapour
    IF ( COUNT( Atmosphere(m)%Absorber_ID == H2O_ID ) /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'No water Vapor data in Atmosphere structure.', &
                            Error_Status )
      STOP
    END IF

    Idx = PACK( (/ (j, j = 1, Atmosphere(m)%n_Absorbers ) /), &
                Atmosphere(m)%Absorber_ID == H2O_ID )

    H2O_Idx = Idx(1)


    ! -- Ozone
    IF ( COUNT( Atmosphere(m)%Absorber_ID == O3_ID ) /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'No ozone data in Atmosphere structure.', &
                            Error_Status )
      STOP
    END IF

    Idx = PACK( (/ (j, j = 1, Atmosphere(m)%n_Absorbers ) /), &
                Atmosphere(m)%Absorber_ID == O3_ID )

    O3_Idx = Idx(1)


    ! -----------------------------------------------
    ! Allocate single TL and AD Atmosphere structures
    ! -----------------------------------------------

    ! -- The TL structure
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                           Atmosphere_TL  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error copying to Atmosphere_TL', &
                            Error_Status )
      STOP
    END IF

    ! -- The AD structure
    Error_Status = CRTM_Assign_Atmosphere( Atmosphere(m), &  ! Input
                                           Atmosphere_AD  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error copying to Atmosphere_AD', &
                            Error_Status )
      STOP
    END IF


    ! -------------------------------------
    ! Allocate the AtmAbsorption structures
    ! -------------------------------------

    ! -- Forward
    Status_FWD = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers, &  ! Input
                                              MAX_N_PREDICTORS,       &  ! Input
                                              MAX_N_ABSORBERS,        &  ! Input
                                              AtmAbsorption           )  ! Output

    ! -- Tangent-linear
    Status_TL = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers,   &  ! Input
                                             MAX_N_PREDICTORS,         &  ! Input
                                             MAX_N_ABSORBERS,          &  ! Input
                                             AtmAbsorption_TL          )  ! Output

    ! -- Adjoint
    Status_AD = CRTM_Allocate_AtmAbsorption( Atmosphere(m)%n_Layers,   &  ! Input
                                             MAX_N_PREDICTORS,         &  ! Input
                                             MAX_N_ABSORBERS,          &  ! Input
                                             AtmAbsorption_AD          )  ! Output
 
    ! -- Check for successful allocation
    IF ( Status_FWD /= SUCCESS .OR. Status_TL /= SUCCESS .OR. Status_AD /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( PROGRAM_NAME, &
                            'Error allocating AtmAbsorption structures', &
                            Error_Status )
      STOP
    END IF
    

    ! ---------------------------------
    ! Setup for the forward calculation
    ! ---------------------------------

    Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere(m), &  ! Input
                                             GeometryInfo,  &  ! Input
                                             AtmAbsorption  )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error setting up AtmAbsorption structure', &
                             Error_Status )                           
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- LOOP OVER SENSOR CHANNELS --                       #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels

      WRITE( *, '( 5x, "Channel: ", i5 )' ) ChannelInfo%Sensor_Channel(l)



      !#------------------------------------------------------------------------#
      !#            -- COMPUTE THE FORWARD MODEL AtmAbsorption --               #
      !#------------------------------------------------------------------------#

      Error_Status = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &  ! Input
                                                 AtmAbsorption                 )  ! In/Output

      IF ( Error_Status /= SUCCESS ) THEN
        CALL DIsplay_Message( PROGRAM_NAME, &
                              'Error computing AtmAbsorption_', &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#               -- BEGIN TANGENT-LINEAR VARIABLE LOOP --                 #
      !#------------------------------------------------------------------------#

      TL_Variable_Loop: DO nV = 1, N_VARIABLES

        WRITE( *, '( 10x, "Performing TL calculations for ", a, "...." )' ) TRIM( VARIABLE_NAME( nV ) )



        !#----------------------------------------------------------------------#
        !#                    -- BEGIN THE LAYER LOOP --                        #
        !#----------------------------------------------------------------------#

        TL_Layer_Loop: DO k = 1, Atmosphere(m)%n_Layers


          ! --------------------------
          ! Reinitialise all TL arrays
          ! --------------------------

          Atmosphere_TL%Level_Pressure = ZERO
          Atmosphere_TL%Pressure       = ZERO
          Atmosphere_TL%Temperature    = ZERO
          Atmosphere_TL%Absorber       = ZERO

          AtmAbsorption_TL%IntAbsorber   = ZERO
          AtmAbsorption_TL%Predictor     = ZERO
          AtmAbsorption_TL%Optical_depth = ZERO


          ! ------------------
          ! Perturb the inputs
          ! ------------------

          SELECT CASE ( nV )
            CASE ( NV_LEVEL_PRESSURE )
              Atmosphere_TL%Level_Pressure(k)   = ONE
            CASE ( NV_LAYER_PRESSURE )
              Atmosphere_TL%Pressure(k)         = ONE
            CASE ( NV_LAYER_TEMPERATURE )
              Atmosphere_TL%Temperature(k)      = ONE
            CASE ( NV_LAYER_WATER_VAPOR )
              Atmosphere_TL%Absorber(k,H2O_Idx) = ONE
            CASE ( NV_LAYER_OZONE )
              Atmosphere_TL%Absorber(k,O3_Idx)  = ONE
          END SELECT


          ! ------------------------------------------------
          ! Setup the tangent-linear AtmAbsorption structure
          ! ------------------------------------------------

          Error_Status = CRTM_SetUp_AtmAbsorption_TL( Atmosphere(m),   &  ! Input
                                                      AtmAbsorption,   &  ! Input   
                                                      Atmosphere_TL,   &  ! Input   
                                                      GeometryInfo,    &  ! Input   
                                                      AtmAbsorption_TL )  ! Output  

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error setting up AtmAbsorption TL structures', &
                                   Error_Status )                           
            STOP
          END IF


          ! ----------------------------------------
          ! Compute the tangent-linear AtmAbsorption
          ! ----------------------------------------

          Error_Status = CRTM_Compute_AtmAbsorption_TL( ChannelInfo%Channel_Index(l), &  ! Input
                                                        AtmAbsorption,                &  ! Input
                                                        AtmAbsorption_TL              )  ! In/Output

          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error computing AtmAbsorption TL', &
                                   Error_Status )                           
            STOP
          END IF

          ! ------------------------------
          ! Save the data for this profile
          ! ------------------------------

          TLADMtest%d_TL(k,l,nv) = AtmAbsorption_TL%Optical_Depth(k)

        END DO TL_Layer_Loop

      END DO TL_Variable_Loop



      !#------------------------------------------------------------------------#
      !#                  -- BEGIN ADJOINT VARIABLE LOOP --                     #
      !#------------------------------------------------------------------------#

      WRITE( *, '( 10x, "Performing AD calculations...." )' )

      AD_Variable_Loop: DO k = 1, Atmosphere(m)%n_Layers


        ! --------------------------
        ! Reinitialise all AD arrays
        ! --------------------------

        Atmosphere_AD%Level_Pressure = ZERO
        Atmosphere_AD%Pressure       = ZERO
        Atmosphere_AD%Temperature    = ZERO
        Atmosphere_AD%Absorber       = ZERO

        AtmAbsorption_AD%IntAbsorber   = ZERO
        AtmAbsorption_AD%Predictor     = ZERO
        AtmAbsorption_AD%Optical_Depth = ZERO


        ! ---------------------------------
        ! Perturb the adjoint optical depth
        ! ---------------------------------

        AtmAbsorption_AD%Optical_Depth(k) = ONE


        ! ---------------------------------
        ! Compute the adjoint AtmAbsorption
        ! ---------------------------------

        Error_Status = CRTM_Compute_AtmAbsorption_AD( ChannelInfo%Channel_Index(l), &  ! Input
                                                      AtmAbsorption,                &  ! Input
                                                      AtmAbsorption_AD              )  ! In/Output

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing AtmAbsorption AD', &
                                 Error_Status )                           
          STOP
        END IF


        ! ------------------------------
        ! Compute the adjoint Atmosphere
        ! ------------------------------

        Error_Status = CRTM_SetUp_AtmAbsorption_AD( Atmosphere(m),    &  ! Input
                                                    AtmAbsorption,    &  ! Input
                                                    AtmAbsorption_AD, &  ! Input
                                                    GeometryInfo,     &  ! Input
                                                    Atmosphere_AD     )  ! Output

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error computing AtmAbsorption AD setup', &
                                 Error_Status )                           
          STOP
        END IF


        ! ------------------------------
        ! Save the data for this profile
        ! ------------------------------

        TLADMtest%d_AD(k,l,NV_LEVEL_PRESSURE )    = Atmosphere_AD%Level_Pressure(k)
        TLADMtest%d_AD(k,l,NV_LAYER_PRESSURE )    = Atmosphere_AD%Pressure(k)
        TLADMtest%d_AD(k,l,NV_LAYER_TEMPERATURE ) = Atmosphere_AD%Temperature(k)
        TLADMtest%d_AD(k,l,NV_LAYER_WATER_VAPOR ) = Atmosphere_AD%Absorber(k,H2O_Idx)
        TLADMtest%d_AD(k,l,NV_LAYER_OZONE )       = Atmosphere_AD%Absorber(k,O3_Idx)

      END DO AD_Variable_Loop

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#        -- WRITE THE CURRENT DATASET TLADMtest STRUCTURE TO FILE --       #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( 10x, "Writing TL/AD data to output file...." )' )


    ! ------------------------------------------------------
    ! Set the current dataset number in the output structure
    ! ------------------------------------------------------

    TLADMtest%nM = m
    WRITE( TLADMtest%nM_Name, '( "Profile # ", i3 )' ) m


    ! --------------
    ! Write the data
    ! --------------

    Error_Status = Write_TLADMtest_netCDF( TRIM( TLADMtest_File ), &
                                           TLADMtest, &
                                           New_File      = New_File, &
                                           Title         = 'TL/AD CRTM AtmAbsorption test results for '//&
                                                           TRIM( File_Prefix ), &
                                           History       = PROGRAM_RCS_ID, &
                                           Sensor_Name   = TRIM( File_Prefix ), &
                                           Platform_Name = TRIM( File_Prefix ), &
                                           Comment       = 'TL and AD data are optical depths.', &
                                           ID_Tag        = 'ECMWF' )

    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error writing TLADMtest structure for profile #", i5, " to ", a )' ) &
                      m, TRIM( TLADMtest_File )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )                           
      STOP
    END IF


    ! -----------------------
    ! Reset the new file flag
    ! -----------------------

    IF ( New_File == SET ) New_File = UNSET



    !#--------------------------------------------------------------------------#
    !#              -- DEALLOCATE STRUCTURES FOR CURRENT PROFILE --             #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! The AtmAbsorption structures
    ! ----------------------------

    Error_Status    = CRTM_Destroy_AtmAbsorption( AtmAbsorption )
    Error_Status_TL = CRTM_Destroy_AtmAbsorption( AtmAbsorption_TL )
    Error_Status_AD = CRTM_Destroy_AtmAbsorption( AtmAbsorption_AD )

    IF ( Error_Status    /= SUCCESS .OR. &
         Error_Status_TL /= SUCCESS .OR. &
         Error_Status_AD /= SUCCESS      ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating AtmAbsorption structures', &
                            Error_Status )
      STOP
    END IF


    ! ------------------------------------
    ! The individual Atmosphere structures
    ! ------------------------------------

    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL, Atmosphere_AD )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating Atmosphere TL/AD structures', &
                            Error_Status )
      STOP
    END IF

  END DO Profile_Loop



  !#----------------------------------------------------------------------------#
  !#                       -- DESTROY TLADMtest STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_TLADMtest( TLADMtest )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error occurred destroying TLADMtest for '//&
                           TRIM( File_Prefix ), &
                            Error_Status )                           
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                           -- DESTROY THE CRTM --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Destroying the CRTM..." )' )

  Error_Status = CRTM_Destroy( ChannelInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- DEALLOCATE THE INPUT Atmosphere STRUCTURE ARRAY --           #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating input Atmosphere structure array', &
                          WARNING )
  END IF

END PROGRAM Test_Adjoint


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Adjoint.f90,v 1.7 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Adjoint.f90,v $
! Revision 1.7  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.6  2005/08/12 20:56:07  paulv
! - Now using new TLADMtest definition and I/O modules.
! - Only layer variables are output.
! - Added the CRTM IRSSE emissivity coefficient file to the initialisation list.
!
! Revision 1.5  2005/03/28 15:59:49  paulv
! - Updated calls to CRTM_AtmAbsorption routines from subroutines to functions.
!
! Revision 1.4  2005/02/16 22:41:54  paulv
! - Updated to use new AerosolCoeff modules.
!
! Revision 1.3  2005/02/16 15:49:07  paulv
! - Updated to use new TLADMtest modules.
!
! Revision 1.2  2005/02/01 15:57:32  paulv
! - Replaced CRTM_SetUp_AtmAbsorption function calls with subroutine calls.
!
! Revision 1.1  2005/01/27 21:25:39  paulv
! Initial checkin.
!
!
!
!
