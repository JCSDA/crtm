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





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################



!  *** USERS INSERT PRIVATE SUBPROGRAMS HERE AS NEEDED ***




!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Forward
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) radiances
!       and brightness temperatures for an input atmospheric profile or
!       profile set and user specified satellites/channels.
!
! CATEGORY:
!       CRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Forward( Atmosphere,                &  ! Input    
!                                    Surface,                   &  ! Input    
!                                    GeometryInfo,              &  ! Input    
!                                    ChannelInfo,               &  ! Input    
!                                    RTSolution,                &  ! Output   
!                                    RCS_Id       = RCS_Id,     &  ! Revision control
!                                    Message_Log  = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the Atmosphere data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                                     or
!                                   Rank-1 (M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the Surface data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:   Structure containing the view geometry
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_GeometryInfo_type )
!                       DIMENSION:  Same as input Atmosphere structure
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN )
!
!       ChannelInfo:    Structure returned from the CRTM_Init() function
!                       that contains the satellite/sesnor channel index
!                       information.
!                       UNITS:      N/A
!                       TYPE:       TYPE( CRTM_ChannelInfo_type )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Rank-1 (L)
!                                     or
!                                   Rank-2 (L x M)
!                                   See dimensionality table in COMMENTS below.
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!      CRTM_Allocate_AtmAbsorption:   Function to allocate AtmAbsorption
!                                     data structures.
!                                     SOURCE: CRTM_ATMABSORPTION_DEFINE module
!
!      CRTM_Allocate_AtmScatter:      Function to allocate AtmScatter data
!                                     structures.
!                                     SOURCE: CRTM_ATMSCATTER_DEFINE module
!
!      CRTM_Allocate_SfcOptics:       Function to allocate SfcOptics data
!                                     structures.
!                                     SOURCE: CRTM_SFCOPTICS_DEFINE module
!
!      CRTM_SetUp_AtmAbsorption:      Function to prepare the AtmAbsorption
!                                     structure for gaseous absorption calculations.
!                                     SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AtmAbsorption:    Function to compute optical depths due
!                                     to gaseuos absorption.
!                                     SOURCE: CRTM_ATMABSORPTION module
!
!      CRTM_Compute_AerosolScatter:   Function to compute aerosol absorption
!                                     and scattering properties.
!                                     SOURCE: CRTM_AEROSOLSCATTER module
!
!      CRTM_Compute_CloudScatter:     Function to compute cloud particle absorption
!                                     and scattering optical depths.
!                                     SOURCE: CRTM_CLOUDSCATTER module
!
!      CRTM_Compute_SfcOptics:        Function to compute surface emissivities
!                                     and reflectivities.
!                                     SOURCE: CRTM_SFCOPTICS module
!
!      CRTM_Compute_RTSolution:       Function to solve the radiative transfer
!                                     equation.
!                                     SOURCE: CRTM_RTSOLUTION module
!
!      Display_Message:               Subroutine to output messages
!                                     SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!      None.
!
! RESTRICTIONS:
!      None.
!
! COMMENTS:
!       - The folowing tables details the input/output argument dimensionality
!         association, where L == n_Channels, M == n_Profiles:
!
!                          INPUTS                   |     OUTPUTS
!                                                   |
!             Atmosphere   Surface   GeometryInfo   |    RTSolution
!          -----------------------------------------+------------------
!               Scalar      Scalar      Scalar      |        L
!                                                   |
!                 M           M           M         |      L x M
!
!         Thus one can process either a single profile or multiple profiles.
!         The routines for each specific case above have been overloaded to
!         the generic interface described in the header above.
!
!       - Note the INTENT on the output RTSolution argument is IN OUT rather
!         than just OUT. This is necessary because the argument may be defined
!         upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------


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

!    IF ( PRESENT( n_Input_Profiles ) ) THEN
!      IF ( n_Input_Profiles > 0 .AND. n_Input_Profiles <= n_Profiles ) THEN
!        n_Profiles = n_Input_Profiles
!      ELSE
!        WRITE( Message, '( "Invalid N_INPUT_PROFILES value: ", i5, &
!                          &". Using Atmosphere structure array dimension value of ", i5, "." )' ) &
!                        n_Input_Profiles, n_Profiles
!        CALL Display_Message( ROUTINE_NAME,    &
!                              TRIM( Message ), &
!                              WARNING,         &
!                              Message_Log = Message_Log )
!      END IF
!    END IF


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

!##### BEGIN: FOR TEST COMPILATION #####
WRITE(*, '( 5x, "Processing profile ", i4 )' ) m
!##### END: FOR TEST COMPILATION #####

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


!##### BEGIN: FOR TEST COMPILATION #####
    ! ** FOR TEST COMPILATION ONLY      **
    ! ** SfcOptics STRUCTURE DIMENSIONS **
    ! ** ASSUME 4 Angles                **
    ! ** ASSUME 2 Stokes PARAMETERS     **
    INTEGER, PARAMETER :: SFCOPTICS_N_ANGLES = 4
    INTEGER, PARAMETER :: SFCOPTICS_N_STOKES = 2
!##### END: FOR TEST COMPILATION #####


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


!##### CHECK INPUT DATA #####






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


!!    ! ----------------------------
!!    ! The AerosolScatter structure
!!    ! ----------------------------
!!
!!  *** HOW ARE THE AerosolScatter DIMENSIONS TO BE MADE AVAILABLE? ***
!!  *** OPTION 1: Make them parameters.
!!      OPTION 2: Compute them in the AerosolScatter routine.
!!
!!
!!    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
!!                                          ***n_Legendre_Terms,         &  ! Input
!!                                          ***n_Phase_Elements,         &  ! Input
!!                                             AerosolScatter,           &  ! Output
!!                                             Message_Log = Message_Log )  ! Error messaging
!!
!!    IF ( Error_Status /= SUCCESS ) THEN
!!      Error_Status = FAILURE
!!      CALL DIsplay_Message( ROUTINE_NAME, &
!!                            'Error allocating AerosolScatter structure', &
!!                            Error_Status, &
!!                            Message_Log = Message_Log )
!!      RETURN
!!    END IF



!!    ! --------------------------
!!    ! The CloudScatter structure
!!    ! --------------------------
!!
!!  *** HOW ARE THE CloudScatter DIMENSIONS TO BE MADE AVAILABLE? ***
!!  *** OPTION 1: Make them parameters.
!!      OPTION 2: Compute them in the CloudScatter routine.
!!
!!
!!    Error_Status = CRTM_Allocate_AtmScatter( Atmosphere%n_Layers,      &  ! Input
!!                                          ***n_Legendre_Terms,         &  ! Input
!!                                          ***n_Phase_Elements,         &  ! Input
!!                                             CloudScatter,             &  ! Output
!!                                             Message_Log = Message_Log )  ! Error messaging
!!
!!    IF ( Error_Status /= SUCCESS ) THEN
!!      Error_Status = FAILURE
!!      CALL DIsplay_Message( ROUTINE_NAME, &
!!                            'Error allocating CloudScatter structure', &
!!                            Error_Status, &
!!                            Message_Log = Message_Log )
!!      RETURN
!!    END IF


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





!##### BEGIN: FOR TEST COMPILATION #####
    ! ---------------------------
    ! Assign some SfcOptics angles
    ! ---------------------------

      SfcOptics%Angle  = (/ ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS, &
                          54.7356103_fp_kind, 77.7999960_fp_kind, 37.9381274_fp_kind /)

!##### END: FOR TEST COMPILATION #####



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
!          OR
!        CYCLE Channel_Loop
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
!          OR
!        CYCLE Channel_Loop
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
!          OR
!        CYCLE Channel_Loop
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
!          OR
!        CYCLE Channel_Loop
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
!          OR
!        CYCLE Channel_Loop
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


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/06/15 17:53:50 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SOI_CRTM_Forward_Module.f90,v $
! Revision 1.1  2006/06/15 17:53:50  wd20pd
! Renamed with SOI_ prefix to prevent confusion with "official" CRTM modules.
!
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/06/29 20:35:31  paulv
! - Initial checkin of UWisc SOI code.
!
! Revision 1.14  2005/02/25 17:53:05  paulv
! - Updated scattering application routines, CloudScatter and AerosolScatter,
!   now being used instead of AtmScatter and Aerosol respectively. Both the
!   cloud particle and aerosol scattering structures are now the same type,
!   CRTM_AtmScatter_type. All the argument to the various function have
!   been updated to reflect this change.
!
! Revision 1.13  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.12  2005/02/16 22:45:07  paulv
! - Added hooks for implementation of Aerosol absorption and scattering.
!   Right now, all of the Aerosol stuff is commented out awaiting a full
!   definition of the Aerosol structure.
!
! Revision 1.11  2005/02/16 15:29:23  paulv
! - Updated header documentation.
! - Started adding hooks for aerosol absorption and scattering function calls.
!   Not implemented yet as Aerosol structure definition is not yet mature enough.
!
! Revision 1.10  2005/02/01 15:53:44  paulv
! - Updated local structure allocation.
!
! Revision 1.9  2005/01/28 21:34:56  paulv
! - The module procedure interface block is now named in f95-style; with the
!   interface name also on the END INTERFACE line.
!
! Revision 1.8  2005/01/28 21:25:46  paulv
! - Added optional RCS_Id argument.
! - Updated header documentation.
! - Changed INTENT of RTSolution argument from OUT to IN OUT to prevent
!   memory leaks.
!
! Revision 1.7  2004/11/05 16:17:04  paulv
! - Removed all Init() routine calls.
!
! Revision 1.6  2004/08/06 18:59:33  paulv
! - Updated header documentation.
!
! Revision 1.5  2004/07/21 15:55:30  paulv
! - Added destroy function call for local AtmAbsorption structure in scalar
!   function.
!
! Revision 1.4  2004/07/02 21:12:06  paulv
! - Added some debug output statements.
!
! Revision 1.3  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.2  2004/07/01 14:55:03  paulv
! - Completed routines using new interface routines. Tested to compilation.
!
! Revision 1.1  2004/06/04 19:37:21  paulv
! Initial checkin. Incomplete.
!
!
!
!
