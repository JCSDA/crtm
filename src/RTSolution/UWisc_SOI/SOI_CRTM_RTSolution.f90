!--------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_RTSolution
!
! PURPOSE:
!       Module containing the raditive transfer solution routines.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CRTM_RTSolution
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       CRTM_Parameters:            Module of parameter definitions for the CRTM.
!                                   USEs: TYPE_KINDS module
!
!       CRTM_SpcCoeff:              Module containing the shared CRTM spectral
!                                   coefficients (SpcCoeff) and their
!                                   load/destruction routines. 
!                                   USEs TYPE_KINDS module
!                                        ERROR_HANDLER module
!                                        SPCCOEFF_DEFINE module
!                                        SPCCOEFF_BINARY_IO module
!                                        CRTM_PARAMETERS module
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
!       CRTM_AtmAbsorption_Define:  Module defining the CRTM AtmAbsorption
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_AtmScatter_Define:     Module defining the CRTM AtmScatter
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_SfcOptics_Define:      Module to compute the surface optical
!                                   properties required for determining
!                                   the surface contribution to the radiative
!                                   transfer.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       CRTM_RTSolution_Define:     Module defining the CRTM RTSolution
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
! CONTAINS:
!       PUBLIC subprograms
!       ------------------
!         CRTM_Compute_RTSolution:     Function to solve the radiative transfer 
!                                      problem.
!
!         CRTM_Compute_RTSolution_TL:  Function to solve the tangent-linear
!                                      radiative transfer problem.
!
!         CRTM_Compute_RTSolution_AD:  Function to solve the adjoint
!                                      radiative transfer problem.
!
!       PRIVATE subprograms
!       -------------------
!       
!         *** USERS ADD INFO HERE FOR ANY PRIVATE SUBPROGRAMS ***
!
!
!
!
!
! USE ASSOCIATED PUBLIC SUBPROGRAMS:
!       CRTM_Associated_RTSolution:  Function to test the association status
!                                    of the pointer members of a RTSolution
!                                    structure.
!
!       CRTM_Destroy_RTSolution:     Function to re-initialize an
!                                    CRTM_RTSolution structure.
!                                    SOURCE: CRTM_RTSOLUTION_DEFINE module
!
!       CRTM_Allocate_RTSolution:    Function to allocate the pointer
!                                    members of an CRTM_RTSolution
!                                    structure.
!                                    SOURCE: CRTM_RTSOLUTION_DEFINE module
!
!       CRTM_Assign_RTSolution:      Function to copy an CRTM_RTSolution
!                                    structure.
!                                    SOURCE: CRTM_RTSOLUTION_DEFINE module
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                       08-Jun-2004
!
!  Copyright (C) 2004 Yong Han, Quanhua Liu, Paul van Delst
!
!M-
!--------------------------------------------------------------------------------

MODULE CRTM_RTSolution


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM parameters and shared data
  USE CRTM_Parameters
  USE CRTM_SpcCoeff

  ! -- CRTM Input data structures
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! -- CRTM Internal structures
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type
  USE CRTM_AtmScatter_Define,    ONLY: CRTM_AtmScatter_type

  ! -- The surface optical properties module.
  ! -- Special case since the Compute_SfcOptics()
  ! -- function *may* be called in the Compute_RTSolution()
  ! -- function in this module for SOI schemes.
  USE CRTM_SfcOptics

  ! -- CRTM Output data structure
  ! -- The PUBLIC entities in CRTM_RTSOlution_Define
  ! -- are also explicitly defined as PUBLIC here so 
  ! -- a user need only USE CRTM_RTSolution.
  USE CRTM_RTSolution_Define


  ! -- The CRTM Plank Functions Module
  USE CRTM_Planck_Functions

  ! -- The SOI Radiative Transfer Module
  USE SOI_rt_model, ONLY : soi_rt, soi_rt_tl, soi_rt_ad

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibilities
  ! ------------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_RTSolution structure data type
  ! -- in the CRTM_RTSolution_Define module
  PUBLIC :: CRTM_RTSolution_type

  ! -- CRTM_RTSolution structure routines inherited
  ! -- from the CRTM_RTSolution_Define module
  PUBLIC :: CRTM_Associated_RTSolution
  PUBLIC :: CRTM_Destroy_RTSolution
  PUBLIC :: CRTM_Allocate_RTSolution
  PUBLIC :: CRTM_Assign_RTSolution

  ! -- Public procedures
  PUBLIC :: CRTM_Compute_RTSolution
  PUBLIC :: CRTM_Compute_RTSolution_TL
  PUBLIC :: CRTM_Compute_RTSolution_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ),  PARAMETER, PRIVATE :: MODULE_RCS_ID = &


CONTAINS














  FUNCTION CRTM_Compute_RTSolution( Atmosphere,    &  ! Input
                                    Surface,       &  ! Input
                                    AtmAbsorption, &  ! Input
                                    AerosolScatter, &  ! Input
                                    CloudScatter,  &  ! Input
                                    GeometryInfo,  &  ! Input
                                    Channel_Index, &  ! Input
                                    RTSolution,    &  ! Output
                                    SfcOptics,     &  ! Optional Input
                                    Message_Log,   &  ! Error messaging
									Verbose 	) &   ! Verbose Output
                                  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),          INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),             INTENT( IN )     :: Surface
    TYPE( CRTM_AtmAbsorption_type ),       INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: CloudScatter
    TYPE( CRTM_GeometryInfo_type ),        INTENT( IN )     :: GeometryInfo
    INTEGER,                               INTENT( IN )     :: Channel_Index

    ! -- Output 
    TYPE( CRTM_RTSolution_type ),          INTENT( IN OUT ) :: RTSolution

    ! -- Optional input
    TYPE( CRTM_SfcOptics_type ), OPTIONAL, INTENT( IN )     :: SfcOptics

    ! -- Error messaging
    CHARACTER( * ),              OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status

    ! -- Verbose output (optional), for debugging Only
    LOGICAL,                     OPTIONAL, INTENT( IN )     :: Verbose


    ! Extra Local Variables Needed for SOI Model
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Optical_Depth
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                             :: Single_Scatter_Albedo
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)							 :: Asymmetry_Factor
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                           :: Level_Temperature
	REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                           :: Level_Planck_Radiance
    REAL(fp_kind)                                                             :: Observation_Angle
    REAL(fp_kind)                                                             :: Surface_Temperature
    REAL(fp_kind)                                                             :: Surface_Planck_Radiance
	INTEGER 																  :: i, n_Layers, n_Stokes
	LOGICAL :: Verb, verb_soi


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    ! ---------------
    ! Intrinsics
    ! ---------------
   
	INTRINSIC ACOS

    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

	Verb = .FALSE.
	if (PRESENT(VERBOSE)) then
		if (VERBOSE) Verb = .TRUE.
	endif



	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
	! RTSolution%n_Stokes = n_Stokes - This is unnecessary. User sets this when they define RTSolution?
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS

	! Check some of the simple things before going on:

	Optical_Depth = AtmAbsorption%Optical_Depth ! + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth
	Single_Scatter_Albedo = ZERO ! + CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth / Optical_Depth
	Asymmetry_Factor = ZERO ! + CloudScatter%Asymmetry_Factor

	! Get the non-edge levels by simple averaging:
	Level_Temperature(2:n_Layers) = POINT_5 * &
                                  (Atmosphere%Temperature(1:N_layers-1) + Atmosphere%Temperature(2:))
	! Get the two edge levels by linear extrapolation:
	Level_Temperature(1) = POINT_5 * (THREE*Atmosphere%Temperature(1) - Atmosphere%Temperature(2))
	Level_Temperature(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere%Temperature(N_Layers) - Atmosphere%Temperature(N_Layers-1))

	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output
    enddo 

    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

    Optical_Depth = Optical_Depth(n_Layers:1:-1)
    Single_Scatter_Albedo = Single_Scatter_Albedo(n_Layers:1:-1)
    Asymmetry_Factor = Asymmetry_Factor(n_Layers:1:-1)
    Level_Planck_Radiance = Level_Planck_Radiance((n_Layers+1):1:-1)


  if (Verb) then
	print *, 'PERFORMING CHECKS IN CRTM_RT_SOLUTION: '
	print *
	print *, 'Surface_Temperature = ', Surface_Temperature
	print *, 'Sensor = ', SC%Sensor_Descriptor( Channel_Index )
	print *, 'Sensor Channel = ', SC%Sensor_Channel( Channel_Index )
	print *, 'Frequency  = ', SC%Frequency( Channel_Index ), ' [GHz]'
	print *, 'Observation Angle = ' , Observation_Angle
	print *, 'N_layers = ', n_Layers
	print *, 'N_Stokes = ', n_Stokes
	print *, 'Total Optical Depth = ' , sum(Optical_Depth)
	print *, 'Total Scat Opd Depth= ', sum(Optical_Depth * Single_Scatter_Albedo)
	print *, 'Surface Radiance = ', Surface_Planck_Radiance
	print *, 'CMB Radiance = ', SC%Cosmic_Background_Radiance( Channel_Index )
  endif


	verb_soi = .FALSE.
	if ((SC%Sensor_Channel( Channel_Index ) < 3) .AND. (verb)) verb_soi = .TRUE.

	CALL soi_rt(  Optical_Depth,                      &  ! Input
                  Single_Scatter_Albedo,              &  ! Input
                  Asymmetry_Factor,                   &  ! Input
                  Level_Planck_Radiance,              &  ! Input
                  Observation_Angle,                  &  ! Input
                  SfcOptics%Emissivity(:, 1),         &  ! Input
                  SfcOptics%Emissivity(:, 2),         &  ! Input
                  RTSolution%Radiance(1),             &  ! Output
                  RTSolution%Radiance(2),             &  ! Output
                  Surface_Planck_Radiance,            &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,1), &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,2), &  ! Input (Optional)
      SC%Cosmic_Background_Radiance( Channel_Index ), &  ! Input (Optional)
                  noscatter = .FALSE.,                &  ! Input (Optional)
 ! Delta_Scaling = CloudScatter%Delta_Truncation,      &  ! Input (Optional)
			      VERBOSE = verb)


    do i = 1, n_Stokes
	  CALL CRTM_Planck_Temperature( Channel_Index,       &                ! Input 
                                    RTSolution%Radiance(i), &                ! Input
                                    RTSolution%Brightness_Temperature(i) )	! Output
    enddo

	if (Verb) print *, 'Brightness Temps (V,H) = ', RTSolution%Brightness_Temperature



  END FUNCTION CRTM_Compute_RTSolution






  FUNCTION CRTM_Compute_RTSolution_TL( Atmosphere,       &  ! Input
                                       Surface,          &  ! Input
                                       AtmAbsorption,    &  ! Input
                                       AerosolScatter,   &  ! Input
                                       CloudScatter,     &  ! Input
                                       RTSolution,       &  ! Input
                                       Atmosphere_TL,    &  ! Input
                                       Surface_TL,       &  ! Input
                                       AtmAbsorption_TL, &  ! Input
                                       AerosolScatter_TL,&  ! Input
                                       CloudScatter_TL,  &  ! Input
                                       GeometryInfo,     &  ! Input
                                       Channel_Index,    &  ! Input
                                       RTSolution_TL,    &  ! Output
                                       SfcOptics,        &  ! Optional Input
                                       SfcOptics_TL,     &  ! Optional Input
                                       Message_Log ,     &  ! Error messaging
                                       Verbose )         &  ! Verbose output
                                     RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),          INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),             INTENT( IN )     :: Surface
    TYPE( CRTM_AtmAbsorption_type ),       INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: CloudScatter
    TYPE( CRTM_RTSolution_type ),          INTENT( IN )     :: RTSolution
    TYPE( CRTM_Atmosphere_type ),          INTENT( IN )     :: Atmosphere_TL
    TYPE( CRTM_Surface_type ),             INTENT( IN )     :: Surface_TL
    TYPE( CRTM_AtmAbsorption_type ),       INTENT( IN )     :: AtmAbsorption_TL
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: AerosolScatter_TL
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: CloudScatter_TL
    TYPE( CRTM_GeometryInfo_type ),        INTENT( IN )     :: GeometryInfo
    INTEGER,                               INTENT( IN )     :: Channel_Index

    ! -- Output 
    TYPE( CRTM_RTSolution_type ),          INTENT( IN OUT ) :: RTSolution_TL

    ! -- Optional input
    TYPE( CRTM_SfcOptics_type ), OPTIONAL, INTENT( IN )     :: SfcOptics
    TYPE( CRTM_SfcOptics_type ), OPTIONAL, INTENT( IN )     :: SfcOptics_TL

    ! -- Error messaging
    CHARACTER( * ),              OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    ! -- Verbose output (optional), for debugging Only
    LOGICAL,                     OPTIONAL, INTENT( IN )     :: Verbose


    ! Extra Local Variables Needed for SOI Model
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Optical_Depth
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Single_Scatter_Albedo
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)					      :: Asymmetry_Factor
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Temperature
	REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Planck_Radiance
    REAL(fp_kind)                                                             :: Observation_Angle
    REAL(fp_kind)                                                             :: Surface_Temperature
    REAL(fp_kind)                                                             :: Surface_Planck_Radiance
    REAL(fp_kind)                                                             :: Radiance_V, Radiance_H
	INTEGER 																  :: i, n_Layers, n_Stokes
	LOGICAL :: Verb

    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Optical_Depth_TL
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Single_Scatter_Albedo_TL
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)					      :: Asymmetry_Factor_TL
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Temperature_TL
	REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Planck_Radiance_TL
    REAL(fp_kind)                                                             :: Surface_Temperature_TL
    REAL(fp_kind)                                                             :: Surface_Planck_Radiance_TL


    ! ---------------
    ! Intrinsics
    ! ---------------
   
	INTRINSIC ACOS



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

	Verb = .FALSE.
	if (PRESENT(VERBOSE)) then
		if (VERBOSE) Verb = .TRUE.
	endif


	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
	! RTSolution%n_Stokes = n_Stokes - This is unnecessary. User sets this when they define RTSolution?
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS


	! Check some of the simple things before going on:

	Optical_Depth = AtmAbsorption%Optical_Depth ! + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth

	Single_Scatter_Albedo = ZERO
	Asymmetry_Factor = ZERO
	Single_Scatter_Albedo_TL = ZERO
	Asymmetry_Factor_TL = ZERO

	Optical_Depth_TL = AtmAbsorption_TL%Optical_Depth  ! + CloudScatter_TL%Optical_Depth + AerosolScatter_TL%Optical_Depth




	! Get the non-edge levels by simple averaging:
	Level_Temperature(2:n_Layers) = POINT_5 * &
                                  (Atmosphere%Temperature(1:N_layers-1) + Atmosphere%Temperature(2:))

	Level_Temperature_TL(2:n_Layers) = POINT_5* &
                                  (Atmosphere_TL%Temperature(1:N_layers-1) + Atmosphere_TL%Temperature(2:))

	! Get the two edge levels by linear extrapolation:
	Level_Temperature(1) = POINT_5 * (THREE*Atmosphere%Temperature(1) - Atmosphere%Temperature(2))
    Level_Temperature_TL(1) = POINT_5 * (THREE*Atmosphere_TL%Temperature(1) - Atmosphere_TL%Temperature(2))

	Level_Temperature(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere%Temperature(N_Layers) - Atmosphere%Temperature(N_Layers-1))

	Level_Temperature_TL(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere_TL%Temperature(N_Layers) - Atmosphere_TL%Temperature(N_Layers-1))


	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

	Surface_Temperature_TL = Surface_TL%Land_Coverage  * Surface%Land_Temperature  + &
                             Surface_TL%Water_Coverage * Surface%Water_Temperature + &
                             Surface_TL%Snow_Coverage  * Surface%Snow_Temperature  + &
                             Surface_TL%Ice_Coverage   * Surface%Ice_Temperature   + &

	                         Surface%Land_Coverage  * Surface_TL%Land_Temperature  + &
                             Surface%Water_Coverage * Surface_TL%Water_Temperature + &
                             Surface%Snow_Coverage  * Surface_TL%Snow_Temperature  + &
                             Surface%Ice_Coverage   * Surface_TL%Ice_Temperature 


    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output

      CALL CRTM_Planck_Radiance_TL( Channel_Index,      &    ! Input
                                 Level_Temperature(i)   , &  ! Input
                                 Level_Temperature_TL(i), &     ! Input
                                 Level_Planck_Radiance_TL(i) )  ! Output

    enddo 

    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    CALL CRTM_Planck_Radiance_TL( Channel_Index,       &    ! Input
					           Surface_Temperature   , &    ! Input
                               Surface_Temperature_TL, &    ! Input
                               Surface_Planck_Radiance_TL ) ! Output
    
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

	SfcOPtics_TL%Emissivity(:,1) = ZERO
	SfcOPtics_TL%Emissivity(:,2) = ZERO
    SfcOptics_TL%Direct_Reflectivity(:,1) = ZERO
    SfcOptics_TL%Direct_Reflectivity(:,2) = ZERO

    Optical_Depth = Optical_Depth(n_Layers:1:-1)
    Single_Scatter_Albedo = Single_Scatter_Albedo(n_Layers:1:-1)
    Asymmetry_Factor = Asymmetry_Factor(n_Layers:1:-1)
    Level_Planck_Radiance = Level_Planck_Radiance((n_Layers+1):1:-1)
    Optical_Depth_TL = Optical_Depth_TL(n_Layers:1:-1)
    Single_Scatter_Albedo_TL = Single_Scatter_Albedo_TL(n_Layers:1:-1)
    Asymmetry_Factor_TL = Asymmetry_Factor_TL(n_Layers:1:-1)
    Level_Planck_Radiance_TL = Level_Planck_Radiance_TL((n_Layers+1):1:-1)


  if (Verb) then
	print *, 'PERFORMING CHECKS IN CRTM_RT_SOLUTION (TL): '
	print *
	print *, 'Surface_Temperature = ', Surface_Temperature
	print *, 'Sensor = ', SC%Sensor_Descriptor( Channel_Index )
	print *, 'Sensor Channel = ', SC%Sensor_Channel( Channel_Index )
	print *, 'Frequency  = ', SC%Frequency( Channel_Index ), ' [GHz]'
	print *, 'Wavenumber = ', SC%Wavenumber( Channel_Index ), ' [cm^-1]'
	print *, 'Observation Angle = ' , Observation_Angle
	print *, 'N_layers = ', n_Layers
	print *, 'N_Stokes = ', n_Stokes
	print *, 'Total Optical Depth = ' , sum(Optical_Depth)
	print *, 'Layer Temperatures = ', Atmosphere%Temperature(n_Layers:1:-1)
	print *, 'Surface Radiance = ', Surface_Planck_Radiance
	print *, 'CMB Radiance = ', SC%Cosmic_Background_Radiance( Channel_Index )
	print *, 'V Emissivities = ', SfcOptics%Emissivity(:,1)
	print *, 'H Emissivities = ', SfcOptics%Emissivity(:,2)
	print *, 'V Reflectvities = ', SfcOptics%Direct_Reflectivity(:,1)
	print *, 'H Reflectvities = ', SfcOptics%Direct_Reflectivity(:,2)
	print *, 'RTSolution%n_Stokes = ', RTSolution%n_Stokes
	print *, 'Pre-SOI TBV = ', RTSolution%Radiance(1)
	print *, 'Pre-SOI TBH = ', RTSolution%Radiance(2)
	print *, 'ATM Radiances = ', Level_Planck_Radiance
  endif


	CALL soi_rt_TL(  Optical_Depth,                      &  ! Input
                  Single_Scatter_Albedo,                 &  ! Input
                  Asymmetry_Factor,                      &  ! Input
                  Level_Planck_Radiance,                 &  ! Input
                  Observation_Angle,                     &  ! Input
                  SfcOptics%Emissivity(:, 1),            &  ! Input
                  SfcOptics%Emissivity(:, 2),            &  ! Input
                  Radiance_V,                            &  ! Input
                  Radiance_H,                            &  ! Input
                  Optical_Depth_TL,                      &  ! Input
                  Single_Scatter_Albedo_TL,              &  ! Input
                  Asymmetry_Factor_TL,                   &  ! Input
                  Level_Planck_Radiance_TL,              &  ! Input
                  SfcOptics_TL%Emissivity(:,1),          &  ! Input
                  SfcOptics_TL%Emissivity(:,2),          &  ! Input
                  RTSolution_TL%Radiance(1),             &  ! Output
                  RTSolution_TL%Radiance(2),             &  ! Output
                  Surface_Planck_Radiance,               &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,1),    &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,2),    &  ! Input (Optional)
         SC%Cosmic_Background_Radiance( Channel_Index ), &  ! Input (Optional)
                  noscatter = .FALSE.,                   &  ! Input (Optional)
			      VERBOSE = .FALSE.,                     &  ! Input (Optional)
          g_r_v = SfcOptics_TL%Direct_Reflectivity(:,1), &  ! Input (Optional)
          g_r_h = SfcOptics_TL%Direct_Reflectivity(:,2), &  ! Input (Optional)
        g_tsurf = Surface_Planck_Radiance_TL            )   ! Input (Optional)               


    do i = 1, n_Stokes
	  CALL CRTM_Planck_Temperature_TL( Channel_Index,       &                ! Input 
									RTSolution%Radiance(i), &
                                    RTSolution_TL%Radiance(i), &                ! Input
                                    RTSolution_TL%Brightness_Temperature(i) )	! Output
    enddo

	if (Verb) print *, 'Brightness Temps TL (V,H) = ', RTSolution_TL%Brightness_Temperature




  END FUNCTION CRTM_Compute_RTSolution_TL






 FUNCTION CRTM_Compute_RTSolution_AD( Atmosphere,       &  ! Input
                                       Surface,          &  ! Input
                                       AtmAbsorption,    &  ! Input
                                       AerosolScatter,   &  ! Input
                                       CloudScatter,     &  ! Input
                                       RTSolution,       &  ! Input
                                       RTSolution_AD,    &  ! Input
                                       GeometryInfo,     &  ! Input
                                       Channel_Index,    &  ! Input
                                       Atmosphere_AD,    &  ! Output
                                       Surface_AD,       &  ! Output
                                       AtmAbsorption_AD, &  ! Output
                                       AerosolScatter_AD,&  ! Output
                                       CloudScatter_AD,  &  ! Output
                                       SfcOptics,        &  ! Optional input
                                       SfcOptics_AD,     &  ! Optional output
                                       Message_Log,      &  ! Error messaging
                                       Verbose     )     &  ! Enable Verbose Output
                                     RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),          INTENT( IN )     :: Atmosphere
    TYPE( CRTM_Surface_type ),             INTENT( IN )     :: Surface
    TYPE( CRTM_AtmAbsorption_type ),       INTENT( IN )     :: AtmAbsorption
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN )     :: CloudScatter
    TYPE( CRTM_RTSolution_type ),          INTENT( IN )     :: RTSolution
    TYPE( CRTM_RTSolution_type ),          INTENT( IN OUT ) :: RTSolution_AD
    TYPE( CRTM_GeometryInfo_type ),        INTENT( IN )     :: GeometryInfo
    INTEGER,                               INTENT( IN )     :: Channel_Index

    ! -- Output 
    TYPE( CRTM_Atmosphere_type ),          INTENT( IN OUT ) :: Atmosphere_AD
    TYPE( CRTM_Surface_type ),             INTENT( IN OUT ) :: Surface_AD
    TYPE( CRTM_AtmAbsorption_type ),       INTENT( IN OUT ) :: AtmAbsorption_AD
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN OUT ) :: AerosolScatter_AD
    TYPE( CRTM_AtmScatter_type ),          INTENT( IN OUT ) :: CloudScatter_AD

    ! -- Optional input
    TYPE( CRTM_SfcOptics_type ), OPTIONAL, INTENT( IN )     :: SfcOptics
    TYPE( CRTM_SfcOptics_type ), OPTIONAL, INTENT( IN OUT ) :: SfcOptics_AD

    ! -- Error messaging
    CHARACTER( * ),              OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_RTSolution_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message 

    ! -- Verbose output (optional), for debugging Only
    LOGICAL,                     OPTIONAL, INTENT( IN )     :: Verbose

    ! Extra Local Variables Needed for SOI Model
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Optical_Depth
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Single_Scatter_Albedo
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)					      :: Asymmetry_Factor
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Temperature
	REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Planck_Radiance
    REAL(fp_kind)                                                             :: Observation_Angle
    REAL(fp_kind)                                                             :: Surface_Temperature
    REAL(fp_kind)                                                             :: Surface_Planck_Radiance
	INTEGER 																  :: i, n_Layers, n_Stokes
	LOGICAL :: Verb

    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Optical_Depth_AD
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)                          :: Single_Scatter_Albedo_AD
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers)					      :: Asymmetry_Factor_AD
    REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Temperature_AD
	REAL(fp_kind), DIMENSION(AtmAbsorption%n_Layers+1)                        :: Level_Planck_Radiance_AD
    REAL(fp_kind)                                                             :: Surface_Temperature_AD
    REAL(fp_kind)                                                             :: Surface_Planck_Radiance_AD


    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



	Verb = .FALSE.
	if (PRESENT(VERBOSE)) then
		if (VERBOSE) Verb = .TRUE.
	endif


	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS

	Optical_Depth = AtmAbsorption%Optical_Depth + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth
	Single_Scatter_Albedo = CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth / Optical_Depth
	Asymmetry_Factor = CloudScatter%Asymmetry_Factor

	! Get the non-edge levels by simple averaging:
	Level_Temperature(2:n_Layers) = POINT_5 * &
                                  (Atmosphere%Temperature(1:N_layers-1) + Atmosphere%Temperature(2:))
	! Get the two edge levels by linear extrapolation:
	Level_Temperature(1) = POINT_5 * (THREE*Atmosphere%Temperature(1) - Atmosphere%Temperature(2))
	Level_Temperature(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere%Temperature(N_Layers) - Atmosphere%Temperature(N_Layers-1))

	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output
    enddo 

    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

    Optical_Depth = Optical_Depth(n_Layers:1:-1)
    Single_Scatter_Albedo = Single_Scatter_Albedo(n_Layers:1:-1)
    Asymmetry_Factor = Asymmetry_Factor(n_Layers:1:-1)
    Level_Planck_Radiance = Level_Planck_Radiance((n_Layers+1):1:-1)

    

    Optical_Depth_AD = ZERO
    Single_Scatter_Albedo_AD = ZERO
    Asymmetry_Factor_AD = ZERO
    Level_Planck_Radiance_AD = ZERO
	Surface_Planck_Radiance_AD = ZERO
	SfcOPtics_AD%Emissivity(:,1) = ONE
	SfcOPtics_AD%Emissivity(:,2) = ONE
    SfcOptics_AD%Direct_Reflectivity(:,1) = ZERO
    SfcOptics_AD%Direct_Reflectivity(:,2) = ZERO
	Surface_Temperature_AD = ZERO
	Level_Temperature_AD = ZERO


    do i = 1, n_Stokes
	  CALL CRTM_Planck_Temperature_AD( Channel_Index,                           & ! Input 
                                       RTSolution%Radiance(i),                  & ! Input
                                       RTSolution_AD%Brightness_Temperature(i), & ! Input
                                       RTSolution_AD%Radiance(i)              )   ! Input/Output
    enddo


	CALL soi_rt_AD(  Optical_Depth,                      &  ! Input
                  Single_Scatter_Albedo,                 &  ! Input
                  Asymmetry_Factor,                      &  ! Input
                  Level_Planck_Radiance,                 &  ! Input
                  Observation_Angle,                     &  ! Input
                  SfcOptics%Emissivity(:, 1),            &  ! Input
                  SfcOptics%Emissivity(:, 2),            &  ! Input
                  Optical_Depth_AD,                      &  ! Input
                  Single_Scatter_Albedo_AD,              &  ! Input
                  Asymmetry_Factor_AD,                   &  ! Input
                  Level_Planck_Radiance_AD,              &  ! Input
                  SfcOptics_AD%Emissivity(:,1),          &  ! Input
                  SfcOptics_AD%Emissivity(:,2),          &  ! Input
                  RTSolution_AD%Radiance(1),             &  ! Output
                  RTSolution_AD%Radiance(2),             &  ! Output
                  Surface_Planck_Radiance,               &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,1),    &  ! Input (Optional)
                  SfcOptics%Direct_Reflectivity(:,2),    &  ! Input (Optional)
        adtsurf = Surface_Planck_Radiance_AD,            &   ! Input (Optional)               
          adr_v = SfcOptics_AD%Direct_Reflectivity(:,1), &  ! Input (Optional)
          adr_h = SfcOptics_AD%Direct_Reflectivity(:,2), &  ! Input (Optional)	
                  noscatter = .FALSE.,                   &  ! Input (Optional)
			      VERBOSE = .FALSE.                      )  ! Input (Optional)

        
    Optical_Depth_AD = Optical_Depth_AD(n_Layers:1:-1)
    Single_Scatter_Albedo_AD = Single_Scatter_Albedo_AD(n_Layers:1:-1)
    Asymmetry_Factor_AD = Asymmetry_Factor_AD(n_Layers:1:-1)
    Level_Planck_Radiance_AD = Level_Planck_Radiance_AD((n_Layers+1):1:-1)
	Optical_Depth = Optical_Depth(n_Layers:1:-1)

	SfcOPtics_AD%Emissivity(:,1) = ZERO
	SfcOPtics_AD%Emissivity(:,2) = ZERO
    SfcOptics_AD%Direct_Reflectivity(:,1) = ZERO
    SfcOptics_AD%Direct_Reflectivity(:,2) = ZERO

    CALL CRTM_Planck_Radiance_AD( Channel_Index,              & ! Input
                                  Surface_Temperature,        & ! Input
                                  Surface_Planck_Radiance_AD, & ! Input
                                  Surface_Temperature_AD    )   ! Input/Output

   do i = 1, n_Layers+1 
      CALL CRTM_Planck_Radiance_AD( Channel_Index,               & ! Input
                                    Level_Temperature(i),        & ! Input
                                    Level_Planck_Radiance_AD(i), & ! Input
                                    Level_Temperature_AD(i)    )   ! Input/Output
   enddo

	Surface_AD%Land_Coverage = Surface_AD%Land_Coverage + Surface%Land_Temperature*Surface_Temperature_AD
	Surface_AD%Land_Temperature = Surface_AD%Land_Temperature + Surface%Land_Coverage*Surface_Temperature_AD
	Surface_AD%Water_Coverage = Surface_AD%Water_Coverage + Surface%Water_Temperature*Surface_Temperature_AD
	Surface_AD%Water_Temperature = Surface_AD%Water_Temperature + Surface%Water_Coverage*Surface_Temperature_AD
	Surface_AD%Snow_Coverage = Surface_AD%Snow_Coverage + Surface%Snow_Temperature*Surface_Temperature_AD
	Surface_AD%Snow_Temperature = Surface_AD%Snow_Temperature + Surface%Snow_Coverage*Surface_Temperature_AD
	Surface_AD%Ice_Coverage = Surface_AD%Ice_Coverage + Surface%Ice_Temperature*Surface_Temperature_AD
	Surface_AD%Ice_Temperature = Surface_AD%Ice_Temperature + Surface%Ice_Coverage*Surface_Temperature_AD

	Atmosphere_AD%Temperature(N_Layers) = Atmosphere_AD%Temperature(N_Layers) + POINT_5*THREE*Level_Temperature_AD(n_Layers+1)
	Atmosphere_AD%Temperature(N_Layers-1)= Atmosphere_AD%Temperature(N_Layers-1) - POINT_5*Level_Temperature_AD(n_Layers+1)
	Atmosphere_AD%Temperature(1) = Atmosphere_AD%Temperature(1) + POINT_5*THREE*Level_Temperature_AD(1)
	Atmosphere_AD%Temperature(2) = Atmosphere_AD%Temperature(2) - POINT_5*Level_Temperature_AD(1)
	Atmosphere_AD%Temperature(1:N_layers-1) = Atmosphere_AD%Temperature(1:N_layers-1) + POINT_5*Level_Temperature_AD(2:n_Layers)
	Atmosphere_AD%Temperature(2:) = Atmosphere_AD%Temperature(2:) + POINT_5*Level_Temperature_AD(2:n_Layers)




	! This is for the current case of no CloudScatter or AerosolScatter elements
	AtmAbsorption_AD%Optical_Depth = Optical_Depth_AD + AtmAbsorption_AD%Optical_Depth
	

  END FUNCTION CRTM_Compute_RTSolution_AD

END MODULE CRTM_RTSolution


