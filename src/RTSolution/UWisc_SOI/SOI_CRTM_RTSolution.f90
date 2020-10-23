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





!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################



!  *** USERS INSERT PRIVATE SUBPROGRAMS HERE ***




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
!       CRTM_Compute_RTSolution
!
! PURPOSE:
!       Function to solve the radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_RTSolution( Atmosphere,                &  ! Input
!                                               Surface,                   &  ! Input
!                                               AtmAbsorption,             &  ! Input
!                                               AerosolScatter,            &  ! Input
!                                               CloudScatter,              &  ! Input
!                                               GeometryInfo,              &  ! Input
!                                               Channel_Index,             &  ! Input
!                                               RTSolution,                &  ! Output
!                                               SfcOptics    = SfcOptics,  &  ! Optional input
!                                               Message_Log  = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:     Structure containing the atmospheric state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Atmosphere_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Surface:        Structure containing the surface state data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Surface_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:  Structure containing the atmospheric gas absorption
!                       data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmAbsorption_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter: Structure containing the aerosol absorption and scattering
!                       parameter data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:   Structure containing the cloud particle absorption and
!                       scattering parameter data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_AtmScatter_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:   Structure containing the view geometry data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_GeometryInfo_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:  Channel index id. This is a unique index associated
!                       with a (supported) sensor channel used to access the
!                       shared coefficient data.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       SfcOptics:      Structure containing the surface optical properties
!                       data. This is provided as an optional input so that
!                       the RTSolution code can call the Compute_SfcOptics()
!                       if it's not supplied. Specifically, this is for
!                       Successive Order of Interaction (SOI) schemes where
!                       the angles at which the surface optical properties
!                       are required are not known ahead of time.
!                       UNITS:      N/A
!                       TYPE:       CRTM_SfcOptics_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       RTSolution:     Structure containing the soluition to the RT equation
!                       for the given inputs.
!                       UNITS:      N/A
!                       TYPE:       CRTM_RTSolution_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
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
!
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output RTSolution argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

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


!  *** USERS INSERT CODE HERE ***

	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
	! RTSolution%n_Stokes = n_Stokes - This is unnecessary. User sets this when they define RTSolution?
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS

	! Check some of the simple things before going on:

!	Combine gas, aerosol, and cloud optical properties
	Optical_Depth = AtmAbsorption%Optical_Depth ! + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth
	Single_Scatter_Albedo = ZERO ! + CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth / Optical_Depth
	Asymmetry_Factor = ZERO ! + CloudScatter%Asymmetry_Factor

!   **** Calculate the Level Temperatures from the Layer Temperatures ***
	! Get the non-edge levels by simple averaging:
	Level_Temperature(2:n_Layers) = POINT_5 * &
                                  (Atmosphere%Temperature(1:N_layers-1) + Atmosphere%Temperature(2:))
	! Get the two edge levels by linear extrapolation:
	Level_Temperature(1) = POINT_5 * (THREE*Atmosphere%Temperature(1) - Atmosphere%Temperature(2))
	Level_Temperature(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere%Temperature(N_Layers) - Atmosphere%Temperature(N_Layers-1))

!  Calculate the Mean Surface Temperature, from all the surface types in this pixel
	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

!	Compute the Level Planck Radiances for this Channel, for all Levels.
    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output
    enddo 

!   Compute the Planck Radiance for the Surface 
    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    
! Set the emissivities (temporary kluge)
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

! Now reverse the order of the arrays so they are ordered surface->TOA.
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
!	print *, 'Wavenumber = ', SC%Wavenumber( Channel_Index ), ' [cm^-1]'
	print *, 'Observation Angle = ' , Observation_Angle
	print *, 'N_layers = ', n_Layers
	print *, 'N_Stokes = ', n_Stokes
	print *, 'Total Optical Depth = ' , sum(Optical_Depth)
	print *, 'Total Scat Opd Depth= ', sum(Optical_Depth * Single_Scatter_Albedo)
!	print *, 'Layer Temperatures = ', Atmosphere%Temperature(n_Layers:1:-1)
	print *, 'Surface Radiance = ', Surface_Planck_Radiance
	print *, 'CMB Radiance = ', SC%Cosmic_Background_Radiance( Channel_Index )
!	print *, 'V Emissivities = ', SfcOptics%Emissivity(:,1)
!	print *, 'H Emissivities = ', SfcOptics%Emissivity(:,2)
!	print *, 'V Reflectvities = ', SfcOptics%Direct_Reflectivity(:,1)
!	print *, 'H Reflectvities = ', SfcOptics%Direct_Reflectivity(:,2)
!	print *, 'RTSolution%n_Stokes = ', RTSolution%n_Stokes
!	print *, 'Pre-SOI TBV = ', RTSolution%Radiance(1)
!	print *, 'Pre-SOI TBH = ', RTSolution%Radiance(2)
!	print *, 'ATM Radiances = ', Level_Planck_Radiance
  endif


	verb_soi = .FALSE.
	if ((SC%Sensor_Channel( Channel_Index ) < 3) .AND. (verb)) verb_soi = .TRUE.

!   Call the SOI Radiative Transfer Model
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


!	Convert TOA Radiances to Brightness Temperatures
    do i = 1, n_Stokes
	  CALL CRTM_Planck_Temperature( Channel_Index,       &                ! Input 
                                    RTSolution%Radiance(i), &                ! Input
                                    RTSolution%Brightness_Temperature(i) )	! Output
    enddo

	if (Verb) print *, 'Brightness Temps (V,H) = ', RTSolution%Brightness_Temperature



  END FUNCTION CRTM_Compute_RTSolution





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_RTSolution_TL
!
! PURPOSE:
!       Function to solve the tangent-linear radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_RTSolution_TL( Atmosphere,                  &  ! Input
!                                                  Surface,                     &  ! Input
!                                                  AtmAbsorption,               &  ! Input
!                                                  AerosolScatter,              &  ! Input
!                                                  CloudScatter,                &  ! Input
!                                                  RTSolution,                  &  ! Input
!                                                  Atmosphere_TL,               &  ! Input
!                                                  Surface_TL,                  &  ! Input
!                                                  AtmAbsorption_TL,            &  ! Input
!                                                  AerosolScatter_TL,           &  ! Input
!                                                  CloudScatter_TL,             &  ! Input
!                                                  GeometryInfo,                &  ! Input
!                                                  Channel_Index,               &  ! Input
!                                                  RTSolution_TL,               &  ! Output
!                                                  SfcOptics    = SfcOptics,    &  ! Optional Input
!                                                  SfcOptics_TL = SfcOptics_TL, &  ! Optional Input
!                                                  Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:        Structure containing the atmospheric state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Surface:           Structure containing the surface state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Surface_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:     Structure containing the atmospheric gas absorption
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:    Structure containing the aerosol absorption and scattering
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:      Structure containing the cloud particle absorption and
!                          scattering data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       RTSolution:        Structure containing the solution to the RT equation
!                          for the given inputs.
!                          UNITS:      N/A
!                          TYPE:       CRTM_RTSolution_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:     Structure containing the tangent-linear atmospheric
!                          state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Surface_TL:        Structure containing the tangent-linear surface state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Surface_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption_TL:  Structure containing the tangent-linear atmospheric
!                          gas absorption data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter_TL: Structure containing the tangent-linear aerosol absorption
!                          and scattering data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter_TL:   Structure containing the tangent-linear cloud particle
!                          absorption and scattering data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:      Structure containing the view geometry data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_GeometryInfo_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:     Channel index id. This is a unique index associated
!                          with a (supported) sensor channel used to access the
!                          shared coefficient data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       SfcOptics:         Structure containing the surface optical properties
!                          data. This is provided as an optional input so that
!                          the RTSolution code can call the Compute_SfcOptics()
!                          if it's not supplied. Specifically, this is for
!                          Successive Order of Interaction (SOI) schemes where
!                          the angles at which the surface optical properties
!                          are required are not known ahead of time.
!                          UNITS:      N/A
!                          TYPE:       CRTM_SfcOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       SfcOptics_TL:      Structure containing the tangent-linear surface
!                          optical properties data. This is provided as an optional
!                          input so that the RTSolution code can call the
!                          Compute_SfcOptics_TL() if it's not supplied. Specifically,
!                          this is for Successive Order of Interaction (SOI) schemes
!                          where the angles at which the surface optical properties
!                          are required are not known ahead of time.
!                          UNITS:      N/A
!                          TYPE:       CRTM_SfcOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       RTSolution_TL:     Structure containing the solution to the tangent-linear
!                          RT equation for the given inputs.
!                          UNITS:      N/A
!                          TYPE:       CRTM_RTSolution_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the computation was sucessful
!                             == FAILURE an unrecoverable error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output RTSolution_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

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

!  *** USERS INSERT CODE HERE ***

	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
	! RTSolution%n_Stokes = n_Stokes - This is unnecessary. User sets this when they define RTSolution?
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS


	! Check some of the simple things before going on:

!	Combine gas, aerosol, and cloud optical properties
	Optical_Depth = AtmAbsorption%Optical_Depth ! + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth

!	Single_Scatter_Albedo = CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth / Optical_Depth
	Single_Scatter_Albedo = ZERO
!	Asymmetry_Factor = CloudScatter%Asymmetry_Factor
	Asymmetry_Factor = ZERO
	Single_Scatter_Albedo_TL = ZERO
	Asymmetry_Factor_TL = ZERO

	Optical_Depth_TL = AtmAbsorption_TL%Optical_Depth  ! + CloudScatter_TL%Optical_Depth + AerosolScatter_TL%Optical_Depth

!    Single_Scatter_Albedo_TL = ( Optical_Depth*( CloudScatter_TL%Single_Scatter_Albedo * CloudScatter%Optical_Depth + &
!                                 CloudScatter_TL%Single_Scatter_Albedo * CloudScatter%Optical_Depth) - &
!                                 CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth * Optical_Depth_TL ) / &
!                                 (Optical_Depth*Optical_Depth)

!	Asymmetry_Factor_TL = CloudScatter_TL%Asymmetry_Factor


!   **** Calculate the Level Temperatures from the Layer Temperatures ***
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


!  Calculate the Mean Surface Temperature, from all the surface types in this pixel
	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

!  Calculate the Mean Surface Temperature, from all the surface types in this pixel
	Surface_Temperature_TL = Surface_TL%Land_Coverage  * Surface%Land_Temperature  + &
                             Surface_TL%Water_Coverage * Surface%Water_Temperature + &
                             Surface_TL%Snow_Coverage  * Surface%Snow_Temperature  + &
                             Surface_TL%Ice_Coverage   * Surface%Ice_Temperature   + &

	                         Surface%Land_Coverage  * Surface_TL%Land_Temperature  + &
                             Surface%Water_Coverage * Surface_TL%Water_Temperature + &
                             Surface%Snow_Coverage  * Surface_TL%Snow_Temperature  + &
                             Surface%Ice_Coverage   * Surface_TL%Ice_Temperature 


!	Compute the Level Planck Radiances for this Channel, for all Levels.
    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output

      CALL CRTM_Planck_Radiance_TL( Channel_Index,      &    ! Input
                                 Level_Temperature(i)   , &  ! Input
                                 Level_Temperature_TL(i), &     ! Input
                                 Level_Planck_Radiance_TL(i) )  ! Output

    enddo 

!   Compute the Planck Radiance for the Surface
    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    CALL CRTM_Planck_Radiance_TL( Channel_Index,       &    ! Input
					           Surface_Temperature   , &    ! Input
                               Surface_Temperature_TL, &    ! Input
                               Surface_Planck_Radiance_TL ) ! Output
    
! Set the emissivities (temporary kluge)
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

	SfcOPtics_TL%Emissivity(:,1) = ZERO
	SfcOPtics_TL%Emissivity(:,2) = ZERO
    SfcOptics_TL%Direct_Reflectivity(:,1) = ZERO
    SfcOptics_TL%Direct_Reflectivity(:,2) = ZERO

! Now reverse the order of the arrays so they are ordered surface->TOA.
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


!   Call the Tangent-Linear SOI Radiative Transfer Model
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
!  Delta_Scaling = CloudScatter%Delta_Truncation,         &  ! Input (Optional)
!  g_Delta_Scaling = CloudScatter_TL%Delta_Truncation,    &  ! Input (Optional)
			      VERBOSE = .FALSE.,                     &  ! Input (Optional)
          g_r_v = SfcOptics_TL%Direct_Reflectivity(:,1), &  ! Input (Optional)
          g_r_h = SfcOptics_TL%Direct_Reflectivity(:,2), &  ! Input (Optional)
        g_tsurf = Surface_Planck_Radiance_TL            )   ! Input (Optional)               


!	Convert TOA Radiances to Brightness Temperatures
    do i = 1, n_Stokes
	  CALL CRTM_Planck_Temperature_TL( Channel_Index,       &                ! Input 
									RTSolution%Radiance(i), &
                                    RTSolution_TL%Radiance(i), &                ! Input
                                    RTSolution_TL%Brightness_Temperature(i) )	! Output
    enddo

	if (Verb) print *, 'Brightness Temps TL (V,H) = ', RTSolution_TL%Brightness_Temperature




  END FUNCTION CRTM_Compute_RTSolution_TL





!--------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_RTSolution_AD
!
! PURPOSE:
!       Function to solve the adjoint radiative transfer equation.
!
! CATEGORY:
!       CRTM : RT Solution
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_RTSolution_AD( Atmosphere,                  &  ! Input
!                                                  Surface,                     &  ! Input 
!                                                  AtmAbsorption,               &  ! Input 
!                                                  AerosolScatter,              &  ! Input 
!                                                  CloudScatter,                &  ! Input 
!                                                  RTSolution,                  &  ! Input 
!                                                  RTSolution_AD,               &  ! Input 
!                                                  GeometryInfo,                &  ! Input 
!                                                  Channel_Index,               &  ! Input 
!                                                  Atmosphere_AD,               &  ! Output
!                                                  Surface_AD,                  &  ! Output
!                                                  AtmAbsorption_AD,            &  ! Output
!                                                  AerosolScatter_AD,           &  ! Output
!                                                  CloudScatter_AD,             &  ! Output
!                                                  SfcOptics    = SfcOptics,    &  ! Optional input  
!                                                  SfcOptics_AD = SfcOptics_AD, &  ! Optional output 
!                                                  Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:        Structure containing the atmospheric state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Surface:           Structure containing the surface state data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_Surface_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AtmAbsorption:     Structure containing the atmospheric gas absorption
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:    Structure containing the aerosol absorption and scattering
!                          data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:      Structure containing the cloud particle absorption and
!                          scattering data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       RTSolution:        Structure containing the solution to the RT equation
!                          for the given inputs.
!                          UNITS:      N/A
!                          TYPE:       CRTM_RTSolution_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       RTSolution_AD:     Structure containing the RT solution adjoint inputs.
!                          **NOTE: On EXIT from this function, the contents of
!                                  this structure may be modified (e.g. set to
!                                  zero.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_RTSolution_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:      Structure containing the view geometry data.
!                          UNITS:      N/A
!                          TYPE:       CRTM_GeometryInfo_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:     Channel index id. This is a unique index associated
!                          with a (supported) sensor channel used to access the
!                          shared coefficient data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       SfcOptics:         Structure containing the surface optical properties
!                          data. This is provided as an optional input so that
!                          the RTSolution code can call the Compute_SfcOptics()
!                          if it's not supplied. Specifically, this is for
!                          Successive Order of Interaction (SOI) schemes where
!                          the angles at which the surface optical properties
!                          are required are not known ahead of time.
!                          UNITS:      N/A
!                          TYPE:       CRTM_SfcOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      None
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:     Structure containing the adjoint atmospheric
!                          state data.
!                          **NOTE: On ENTRY to this function, the contents of
!                                  this structure should be defined (e.g.
!                                  initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_Atmosphere_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_AD:        Structure containing the adjoint surface state data.
!                          **NOTE: On ENTRY to this function, the contents of
!                                  this structure should be defined (e.g.
!                                  initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_Surface_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       AtmAbsorption_AD:  Structure containing the adjoint atmospheric
!                          gas absorption data.
!                          **NOTE: On ENTRY to this function, the contents of
!                                  this structure should be defined (e.g.
!                                  initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmAbsorption_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       AerosolScatter_AD: Structure containing the adjoint aerosol absorption
!                          and scattering data.
!                          **NOTE: On ENTRY to this function, the contents of
!                                  this structure should be defined (e.g.
!                                  initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!       CloudScatter_AD:   Structure containing the adjoint cloud particle absorption
!                          and scattering data.
!                          **NOTE: On ENTRY to this function, the contents of
!                                  this structure should be defined (e.g.
!                                  initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_AtmScatter_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       SfcOptics_AD:      Structure containing the adjoint surface
!                          optical properties data. This is provided as an optional
!                          input so that the RTSolution code can call the
!                          Compute_SfcOptics_AD() if it's not supplied. Specifically,
!                          this is for Successive Order of Interaction (SOI) schemes
!                          where the angles at which the surface optical properties
!                          are required are not known ahead of time.
!                          **NOTE: If used, then on ENTRY to this function, the
!                                  contents of this structure should be defined
!                                  (e.g. initialized to some value based on the
!                                  position of this function in the call chain.)
!                          UNITS:      N/A
!                          TYPE:       CRTM_SfcOptics_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the computation was sucessful
!                             == FAILURE an unrecoverable error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!S-
!--------------------------------------------------------------------------------

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


!  *** USERS INSERT CODE HERE ***

	Verb = .FALSE.
	if (PRESENT(VERBOSE)) then
		if (VERBOSE) Verb = .TRUE.
	endif

! ******* Repeat Some Necessary Forward Calculations

	! Preliminary Stuff
    n_Layers = Atmosphere%n_Layers
	n_Stokes = SfcOptics%n_Stokes
    Observation_Angle = ACOS(GeometryInfo%Secant_View_Angle)/DEGREES_TO_RADIANS

!	Combine gas, aerosol, and cloud optical properties
	Optical_Depth = AtmAbsorption%Optical_Depth + CloudScatter%Optical_Depth + AerosolScatter%Optical_Depth
	Single_Scatter_Albedo = CloudScatter%Single_Scatter_Albedo * CloudScatter%Optical_Depth / Optical_Depth
	Asymmetry_Factor = CloudScatter%Asymmetry_Factor

!   **** Calculate the Level Temperatures from the Layer Temperatures ***
	! Get the non-edge levels by simple averaging:
	Level_Temperature(2:n_Layers) = POINT_5 * &
                                  (Atmosphere%Temperature(1:N_layers-1) + Atmosphere%Temperature(2:))
	! Get the two edge levels by linear extrapolation:
	Level_Temperature(1) = POINT_5 * (THREE*Atmosphere%Temperature(1) - Atmosphere%Temperature(2))
	Level_Temperature(n_Layers+1) = POINT_5 * &
                        (THREE*Atmosphere%Temperature(N_Layers) - Atmosphere%Temperature(N_Layers-1))

!  Calculate the Mean Surface Temperature, from all the surface types in this pixel
	Surface_Temperature = Surface%Land_Coverage  * Surface%Land_Temperature  + &
                          Surface%Water_Coverage * Surface%Water_Temperature + &
                          Surface%Snow_Coverage  * Surface%Snow_Temperature  + &
                          Surface%Ice_Coverage   * Surface%Ice_Temperature   

!	Compute the Level Planck Radiances for this Channel, for all Levels.
    do i = 1, n_Layers+1
      CALL CRTM_Planck_Radiance( Channel_Index,      &    ! Input
                                 Level_Temperature(i), &     ! Input
                                 Level_Planck_Radiance(i) )  ! Output
    enddo 

!   Compute the Planck Radiance for the Surface
    CALL CRTM_Planck_Radiance( Channel_Index,       &    ! Input
                               Surface_Temperature, &    ! Input
                               Surface_Planck_Radiance ) ! Output

    
! Set the emissivities (temporary kluge)
	SfcOPtics%Emissivity(:,1) = ONE
	SfcOPtics%Emissivity(:,2) = ONE
    SfcOptics%Direct_Reflectivity(:,1) = ZERO
    SfcOptics%Direct_Reflectivity(:,2) = ZERO

! Now reverse the order of the arrays so they are ordered surface->TOA.
    Optical_Depth = Optical_Depth(n_Layers:1:-1)
    Single_Scatter_Albedo = Single_Scatter_Albedo(n_Layers:1:-1)
    Asymmetry_Factor = Asymmetry_Factor(n_Layers:1:-1)
    Level_Planck_Radiance = Level_Planck_Radiance((n_Layers+1):1:-1)

    
! ************ Begin Adjoint Code ***********************************

!  Initialize Local Adjoint Variables 
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


!	Convert Brightness Temperatures Adjoints to TOA Radiance Adjoints
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
!  Delta_Scaling = CloudScatter%Delta_Truncation,         &  ! Input (Optional)
!  adDelta_Scaling = CloudScatter_AD%Delta_Truncation,    &  ! Input (Optional)
			      VERBOSE = .FALSE.                      )  ! Input (Optional)

        
! Now reverse the order of the adjoint arrays so they are ordered TOA->surface
    Optical_Depth_AD = Optical_Depth_AD(n_Layers:1:-1)
    Single_Scatter_Albedo_AD = Single_Scatter_Albedo_AD(n_Layers:1:-1)
    Asymmetry_Factor_AD = Asymmetry_Factor_AD(n_Layers:1:-1)
    Level_Planck_Radiance_AD = Level_Planck_Radiance_AD((n_Layers+1):1:-1)
	Optical_Depth = Optical_Depth(n_Layers:1:-1)

! Set the emissivities adjoints (temporary kluge)
	SfcOPtics_AD%Emissivity(:,1) = ZERO
	SfcOPtics_AD%Emissivity(:,2) = ZERO
    SfcOptics_AD%Direct_Reflectivity(:,1) = ZERO
    SfcOptics_AD%Direct_Reflectivity(:,2) = ZERO

!   Compute the adjoint surface temp from the adjoint surface radiance
    CALL CRTM_Planck_Radiance_AD( Channel_Index,              & ! Input
                                  Surface_Temperature,        & ! Input
                                  Surface_Planck_Radiance_AD, & ! Input
                                  Surface_Temperature_AD    )   ! Input/Output

!  Compute the adjoint level temps from the adjoint level radiances
   do i = 1, n_Layers+1 
      CALL CRTM_Planck_Radiance_AD( Channel_Index,               & ! Input
                                    Level_Temperature(i),        & ! Input
                                    Level_Planck_Radiance_AD(i), & ! Input
                                    Level_Temperature_AD(i)    )   ! Input/Output
   enddo

!  Calculate Surface Adjoint Crap
	Surface_AD%Land_Coverage = Surface_AD%Land_Coverage + Surface%Land_Temperature*Surface_Temperature_AD
	Surface_AD%Land_Temperature = Surface_AD%Land_Temperature + Surface%Land_Coverage*Surface_Temperature_AD
	Surface_AD%Water_Coverage = Surface_AD%Water_Coverage + Surface%Water_Temperature*Surface_Temperature_AD
	Surface_AD%Water_Temperature = Surface_AD%Water_Temperature + Surface%Water_Coverage*Surface_Temperature_AD
	Surface_AD%Snow_Coverage = Surface_AD%Snow_Coverage + Surface%Snow_Temperature*Surface_Temperature_AD
	Surface_AD%Snow_Temperature = Surface_AD%Snow_Temperature + Surface%Snow_Coverage*Surface_Temperature_AD
	Surface_AD%Ice_Coverage = Surface_AD%Ice_Coverage + Surface%Ice_Temperature*Surface_Temperature_AD
	Surface_AD%Ice_Temperature = Surface_AD%Ice_Temperature + Surface%Ice_Coverage*Surface_Temperature_AD

!   **** Calculate the Layer Temperature Adjoints from the Layer Temperature Adjoints ***
	Atmosphere_AD%Temperature(N_Layers) = Atmosphere_AD%Temperature(N_Layers) + POINT_5*THREE*Level_Temperature_AD(n_Layers+1)
	Atmosphere_AD%Temperature(N_Layers-1)= Atmosphere_AD%Temperature(N_Layers-1) - POINT_5*Level_Temperature_AD(n_Layers+1)
	Atmosphere_AD%Temperature(1) = Atmosphere_AD%Temperature(1) + POINT_5*THREE*Level_Temperature_AD(1)
	Atmosphere_AD%Temperature(2) = Atmosphere_AD%Temperature(2) - POINT_5*Level_Temperature_AD(1)
	Atmosphere_AD%Temperature(1:N_layers-1) = Atmosphere_AD%Temperature(1:N_layers-1) + POINT_5*Level_Temperature_AD(2:n_Layers)
	Atmosphere_AD%Temperature(2:) = Atmosphere_AD%Temperature(2:) + POINT_5*Level_Temperature_AD(2:n_Layers)

!   Break Out Adjoints for Cloud, Aerosol, and Gas
!	CloudScatter_AD%Asymmetry_Factor = Asymmetery_Factor_AD + CloudScatter_AD%Asymmetry_Factor

!	CloudScatter_AD%Single_Scatter_Albedo = CloudScatter%Optical_Depth/Optical_Depth * Single_Scatter_Albedo_AD + CloudScatter_AD%Single_Scatter_Albedo
!	CloudScatter_AD%Optical_Depth = CloudScatter%Single_Scatter_Albedo/Optical_Depth * Single_Scatter_Albedo_AD + CloudScatter_AD%Optical_Depth
!	Optical_Depth_AD = Optical_Depth_AD - &
!      CloudScatter%Single_Scatter_Albedo*CloudScatter%Optical_Depth/(Optical_Depth*Optical_Depth)*Single_Scatter_Albedo_AD

!	AerosolScatter_AD%Optical_Depth = Optical_Depth_AD + AerosolScatter_AD%Optical_Depth
!	CloudScatter_AD%Optical_Depth = Optical_Depth_AD + CloudScatter_AD%Optical_Depth
!	AtmAbsorption_AD%Optical_Depth = Optical_Depth_AD + AtmAbsorption_AD%Optical_Depth

	! This is for the current case of no CloudScatter or AerosolScatter elements
	AtmAbsorption_AD%Optical_Depth = Optical_Depth_AD + AtmAbsorption_AD%Optical_Depth
	

  END FUNCTION CRTM_Compute_RTSolution_AD

END MODULE CRTM_RTSolution


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
! $Log: SOI_CRTM_RTSolution.f90,v $
! Revision 1.1  2006/06/15 17:53:50  wd20pd
! Renamed with SOI_ prefix to prevent confusion with "official" CRTM modules.
!
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/06/29 20:35:31  paulv
! - Initial checkin of UWisc SOI code.
!
! Revision 1.11  2005/02/25 17:56:07  paulv
! - All the instances (in definitions and interfaces) of the following
!   structures
!     TYPE( CRTM_Aerosol_type )    -- Aerosol
!     TYPE( CRTM_AtmScatter_type ) -- AtmScatter
!   have been changed to
!     TYPE( CRTM_AtmScatter_type ) -- AerosolScatter
!     TYPE( CRTM_AtmScatter_type ) -- CloudScatter
!   to reflect the use of the same data type for both the cloud particle
!   and aerosol scattering codes.
!
! Revision 1.10  2005/02/18 23:17:56  paulv
! - Added Aerosol capability. RTSolution interfaces altered.
!
! Revision 1.9  2005/02/17 23:06:43  paulv
! - Corrected adjoint function documentation name.
!
! Revision 1.8  2005/02/01 16:02:47  paulv
! - Added adjoint shell function.
!
! Revision 1.7  2005/01/28 21:14:51  paulv
! - Added CRTM_Compute_RTSolution_TL() function shell.
! - Rearranged some of the RTSolution arguments to make their ordering
!   consistent in the CRTM source codes.
!
! Revision 1.6  2004/11/05 16:13:00  paulv
! - Replaced Init() with Associated() in PUBLIC list.
! - Changed output RTSolution structure intent from (OUT) to (IN OUT) to
!   prevent memory leaks.
!
! Revision 1.5  2004/08/06 18:45:53  paulv
! - Updated header documentation.
!
! Revision 1.4  2004/07/01 20:52:51  paulv
! - Resyncing repository with working versions. This version works with the
!   Test_Forward program of the same date.
!
! Revision 1.3  2004/06/29 16:58:59  paulv
! - Made SfcOptics argument in Compute_RTSolution() optional.
! - Updated documentation.
!
! Revision 1.2  2004/06/28 22:22:38  paulv
! - Removed RTSolution structure definition and methods to separate routine.
! - Added all required arguments.
! - Updated header documentation.
!
! Revision 1.1  2004/06/15 16:20:53  paulv
! Initial checkin.
!
!
!
