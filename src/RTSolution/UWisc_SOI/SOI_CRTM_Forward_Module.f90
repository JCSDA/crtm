!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Forward_Module
!
! PURPOSE:
!       Module containing the CRTM forward model function.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_Forward_Module
!
! MODULES:
!       Type_Kinds:                 Module to define kind types for variable
!                                   declaration.
!
!       Message_Handler:            Module to define error codes and handle
!                                   error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_Atmosphere_Define:     Module defining the CRTM Atmosphere
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_CLOUD_DEFINE module
!
!       CRTM_Surface_Define:        Module defining the CRTM Surface data
!                                   structure and containing routines to 
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_GeometryInfo_Define:   Module defining the CRTM GeometryInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo_Define:    Module defining the CRTM ChannelInfo
!                                   data structure and containing routines
!                                   to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmAbsorption:         Module continaing routines to compute
!                                   the optical depth profile due to gaseous
!                                   absorption.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         CRTM_PARAMETERS module
!                                         CRTM_TAUCOEFF module
!                                         CRTM_ATMOSPHERE_DEFINE module
!                                         CRTM_GEOMETRYINFO_DEFINE module
!                                         CRTM_ATMABSORPTION_DEFINE module
!                                         CRTM_ATMABSORPTION_INTABSORBER module
!                                         CRTM_ATMABSORPTION_PREDICTOR module
!
!       CRTM_AerosolScatter:        Module containing routines to compute
!                                   aerosol absorption and scattering properties.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_CloudScatter:          Module containing routines to compute cloud
!                                   particle absorption and scattering properties.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_SfcOptics:             Module containing routines to compute 
!                                   surface emissivities and reflectivities.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_RTSolution:            Module containing the radiative transfer
!                                   solution routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!
! CONTAINS:
!       CRTM_Forward:    Function that calculates top-of-atmosphere (TOA)
!                        radiances and brightness temperatures for an input
!                        atmospheric profile or profile set and user
!                        specified satellites/channels.Function to solve
!                        the forward radiative transfer problem.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
!
!M-
!------------------------------------------------------------------------------

MODULE CRTM_Forward_Module


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds, ONLY : fp_kind
  USE Message_Handler

  ! -- CRTM "global" parameters
  USE CRTM_Parameters

  ! -- Definition modules
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define

  ! -- Application modules
  USE CRTM_AtmAbsorption
  USE CRTM_AerosolScatter
  USE CRTM_CloudScatter
  USE CRTM_SfcOptics
  USE CRTM_RTSolution


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures
  PUBLIC :: CRTM_Forward


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE CRTM_Forward
    MODULE PROCEDURE CRTM_Forward_scalar
    MODULE PROCEDURE CRTM_Forward_rank1
  END INTERFACE CRTM_Forward


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &


CONTAINS
















  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#                -- RANK-1 (N_PROFILES) SPECIFIC FUNCTION --                 #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Forward_rank1( Atmosphere,   &  ! Input, M
                               Surface,      &  ! Input, M    
                               GeometryInfo, &  ! Input, M    
                               ChannelInfo,  &  ! Input, Scalar    
                               RTSolution,   &  ! Output, L x M   
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),   DIMENSION( : ),    INTENT( IN )     :: Atmosphere    ! M
    TYPE( CRTM_Surface_type ),      DIMENSION( : ),    INTENT( IN )     :: Surface       ! M
    TYPE( CRTM_GeometryInfo_type ), DIMENSION( : ),    INTENT( IN )     :: GeometryInfo  ! M
    TYPE( CRTM_ChannelInfo_type ),                     INTENT( IN )     :: ChannelInfo   ! Scalar   

    ! -- Output
    TYPE( CRTM_RTSolution_type ),   DIMENSION( :, : ), INTENT( IN OUT ) :: RTSolution    ! L x M

    ! -- Revision control
    CHARACTER( * ),                 OPTIONAL,          INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),                 OPTIONAL,          INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Forward(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed

    LOGICAL :: Verbose
    INTEGER :: m, n_Profiles



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#          -- DETERMINE ARRAY DIMENSIONS AND CHECK THE VALUES --           #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------

    ! -- Number of atmospheric profiles.

    n_Profiles = SIZE( Atmosphere )



    ! -- Check that the number of profiles is not greater than
    ! -- MAX_N_PROFILES. This is simply a limit to restrict the
    ! -- size of the input arrays so they're not TOO big.

    IF ( n_Profiles > MAX_N_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Value_Input,   '( i5 )' ) n_Profiles
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of passed profiles ('// &
                            TRIM( ADJUSTL( Value_Input ) )// &
                            ') > maximum number of profiles allowed ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    END IF





    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Begin profile loop
    ! ------------------

    Profile_Loop: DO m = 1, n_Profiles

WRITE(*, '( 5x, "Processing profile ", i4 )' ) m

      ! ------------------------
      ! Call the scalar function
      ! ------------------------

      Verbose = .FALSE.
	  if (m == 32) Verbose = .TRUE.
      Error_Status = CRTM_Forward_scalar( Atmosphere(m),            &  ! Input, Scalar
                                          Surface(m),               &  ! Input, Scalar
                                          GeometryInfo(m),          &  ! Input, Scalar
                                          ChannelInfo,              &  ! Input, Scalar
                                          RTSolution(:,m),          &  ! Output, L   
                                          Verbose = Verbose,        &  ! verbose output
                                          Message_Log = Message_Log )  ! Error messaging

      ! -------------------------------
      ! Check for successful completion
      ! -------------------------------

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error occured in CRTM_Forward(Scalar) for profile #", i5 )' ) m
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END DO Profile_Loop

  END FUNCTION CRTM_Forward_rank1



  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#
  !#              -- SCALAR (SINGLE PROFILE) SPECIFIC FUNCTION --               #
  !#----------------------------------------------------------------------------#
  !#----------------------------------------------------------------------------#

  FUNCTION CRTM_Forward_scalar( Atmosphere,   &  ! Input, Scalar
                                Surface,      &  ! Input, Scalar
                                GeometryInfo, &  ! Input, Scalar
                                ChannelInfo,  &  ! Input, Scalar    
                                RTSolution,   &  ! Output, L   
                                RCS_Id,       &  ! Revision control
                                Message_Log,  &  ! Error messaging
                                Verbose )     &  ! Verbose Output
                              RESULT( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),                 INTENT( IN )     :: Atmosphere    ! Scalar
    TYPE( CRTM_Surface_type ),                    INTENT( IN )     :: Surface       ! Scalar
    TYPE( CRTM_GeometryInfo_type ),               INTENT( IN )     :: GeometryInfo  ! Scalar
    TYPE( CRTM_ChannelInfo_type ),                INTENT( IN )     :: ChannelInfo   ! Scalar   

    ! -- Output
    TYPE( CRTM_RTSolution_type ), DIMENSION( : ), INTENT( IN OUT ) :: RTSolution    ! L

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT )    :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )     :: Message_Log
    LOGICAL,                      OPTIONAL,       INTENT( IN )     :: Verbose



    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Forward(Scalar)'


    ! ** FOR TEST COMPILATION ONLY      **
    ! ** SfcOptics STRUCTURE DIMENSIONS **
    ! ** ASSUME 4 Angles                **
    ! ** ASSUME 2 Stokes PARAMETERS     **
    INTEGER, PARAMETER :: SFCOPTICS_N_ANGLES = 4
    INTEGER, PARAMETER :: SFCOPTICS_N_STOKES = 2


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Status
    INTEGER :: l
    TYPE( CRTM_AtmAbsorption_type ) :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type )    :: AerosolScatter
    TYPE( CRTM_AtmScatter_type )    :: CloudScatter
    TYPE( CRTM_SfcOPtics_type )     :: SfcOptics
	LOGICAL                         :: Verb


    ! ---------------
    ! Intrinsics
    ! ---------------
   
	INTRINSIC ACOS

	Verb = .FALSE.
	if (PRESENT(Verbose)) then
       if (Verbose) verb = .TRUE.
    endif


    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --              #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of channels
    ! ----------------------------

    ! -- If no channels, simply return
    IF ( ChannelInfo%n_Channels == 0 ) RETURN

    ! -- Output array too small
    IF ( SIZE( RTSolution ) < ChannelInfo%n_Channels ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Output RTSolution structure array too small (", i5, &
                        &") to hold results for the number of requested channels (", i5, ")" )' ) &
                      SIZE( RTSolution ), ChannelInfo%n_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF








    !#--------------------------------------------------------------------------#
    !#                     -- ALLOCATE ALL LOCAL STRUCTURES --                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! The AtmAbsorption structure
    ! ---------------------------

    Error_Status = CRTM_Allocate_AtmAbsorption( Atmosphere%n_Layers,      &  ! Input
                                                MAX_N_PREDICTORS,         &  ! Input
                                                MAX_N_ABSORBERS,          &  ! Input
                                                AtmAbsorption,            &  ! Output
                                                Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF







    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    Error_Status = CRTM_Allocate_SfcOptics( SFCOPTICS_N_ANGLES,       &  ! Input
                                            SFCOPTICS_N_STOKES,       &  ! Input
                                            SfcOptics,                &  ! Output
                                            Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error allocating SfcOptics structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF





    ! ---------------------------
    ! Assign some SfcOptics angles
    ! ---------------------------

      SfcOptics%Angle  = (/ ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS, &
                          54.7356103_fp_kind, 77.7999960_fp_kind, 37.9381274_fp_kind /)




    !#--------------------------------------------------------------------------#
    !#                -- SET UP FOR GASEOUS ABSORPTION CALCS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = CRTM_SetUp_AtmAbsorption( Atmosphere,               &  ! Input
                                             GeometryInfo,             &  ! Input
                                             AtmAbsorption,            &  ! Output
                                             Message_Log = Message_Log )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL DIsplay_Message( ROUTINE_NAME, &
                            'Error setting up AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, ChannelInfo%n_Channels


      ! --------------------------------
      ! Compute the layer optical depths
      ! due to gaseous absorption
      ! --------------------------------

      Error_Status = CRTM_Compute_AtmAbsorption( ChannelInfo%Channel_Index(l), &
                                                 AtmAbsorption, &
                                                 Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing AtmAbsorption for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ----------------------------------------------------
      ! Compute the aerosol absorption/scattering properties
      ! ----------------------------------------------------

      Error_Status = CRTM_Compute_AerosolScatter( Atmosphere, &
                                                  GeometryInfo, &
                                                  ChannelInfo%Channel_Index(l), &
                                                  AerosolScatter, &
                                                  Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing AerosolScatter for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! -----------------------------------------------------------
      ! Compute the cloud particle absorption/scattering properties
      ! -----------------------------------------------------------

      Error_Status = CRTM_Compute_CloudScatter( Atmosphere, &
                                                GeometryInfo, &
                                                ChannelInfo%Channel_Index(l), &
                                                CloudScatter, &
                                                Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing CloudScatter for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! --------------------------------------
      ! Compute the surface optical properties
      ! --------------------------------------

      Error_Status = CRTM_Compute_SfcOptics( Surface, &
                                             GeometryInfo, &
                                             ChannelInfo%Channel_Index(l), &
                                             SfcOptics, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing SfcOptics for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------------------------
      ! Solve the radiative transfer problem
      ! ------------------------------------

      Error_Status = CRTM_Compute_RTSolution( Atmosphere,                   &  ! Input
                                              Surface,                      &  ! Input
                                              AtmAbsorption,                &  ! Input
                                              AerosolScatter,               &  ! Input
                                              CloudScatter,                 &  ! Input
                                              GeometryInfo,                 &  ! Input
                                              ChannelInfo%Channel_Index(l), &  ! Input
                                              RTSolution(l),                &  ! Output
                                              SfcOptics   = SfcOptics,      &  ! Optional Input
                                              verbose = verb,               &  ! Verbose output
                                              Message_Log = Message_Log     )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error computing RTSolution for ", a, ", channel ", i4 )' ) &
                        TRIM( ChannelInfo%Sensor_Descriptor(l) ), &
                        ChannelInfo%Sensor_Channel(l)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                    -- DEALLOCATE LOCAL STRUCTURES --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The SfcOptics structure
    ! -----------------------

    Status = CRTM_Destroy_SfcOptics( SfcOptics )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating SfcOptics structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------------
    ! The CloudScatter structure
    ! --------------------------

    Status = CRTM_Destroy_AtmScatter( CloudScatter )

    IF ( Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating CloudScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------------
    ! The AerosolScatter structure
    ! ----------------------------

    Status = CRTM_Destroy_AtmScatter( AerosolScatter )

    IF ( Status /= SUCCESS ) THEN
      Error_status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AerosolScatter structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------------------------
    ! The AtmAbsorption structure
    ! ---------------------------

    Status = CRTM_Destroy_AtmAbsorption( AtmAbsorption )

    IF ( Status /= SUCCESS ) THEN
      Error_status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating AtmAbsorption structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Forward_scalar

END MODULE CRTM_Forward_Module


