! 
! CRTM_AerosolScatter
!
! 
! Module to compute the aerosol absorption and scattering properties
! required for radiative transfer in an atmosphere with aerosols.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Feb-2005
!                       paul.vandelst@ssec.wisc.edu
!       Modified by     Quanhua Liu, 03-Oct-2006
!                       Quanhua.Liu@noaa.gov
                        


MODULE CRTM_AerosolScatter


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  ! -- CRTM modules
  USE CRTM_Parameters          
  USE CRTM_SpcCoeff,            ONLY: SC, INFRARED_SENSOR, VISIBLE_SENSOR
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_Interpolation,       ONLY: INTERP_NPTS=>NPTS, &
                                      find_index       , &
                                      interp_1D        , &
                                      interp_2D        , &
                                      interp_3D        , &
                                      interp_2D_TL     , &
                                      interp_3D_TL     , &
                                      interp_2D_AD     , &
                                      interp_3D_AD     , &
                                      dlpoly           , &
                                      lpoly

  ! -- The AtmScatter structure definition module
  ! -- The PUBLIC entities in CRTM_AtmScatter_Define
  ! -- are also explicitly defined as PUBLIC here
  ! -- (down below) so a user need only USE this
  ! -- module (CRTM_AerosolScatter).
  USE CRTM_AtmScatter_Define,  ONLY: CRTM_AtmScatter_type        , &
                                     CRTM_Associated_AtmScatter  , &
                                     CRTM_Destroy_AtmScatter     , &
                                     CRTM_Allocate_AtmScatter    , &
                                     CRTM_Assign_AtmScatter

  USE CRTM_Aerosol_Define
  USE CRTM_AerosolCoeff,   ONLY: AeroC 

  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- CRTM_AtmScatter structure data type
  ! -- in the CRTM_AtmScatter_Define module
  PUBLIC :: CRTM_AtmScatter_type

  ! -- CRTM_AtmScatter structure routines inherited
  ! -- from the CRTM_AtmScatter_Define module
  PUBLIC :: CRTM_Associated_AtmScatter
  PUBLIC :: CRTM_Destroy_AtmScatter
  PUBLIC :: CRTM_Allocate_AtmScatter
  PUBLIC :: CRTM_Assign_AtmScatter

  ! -- Science routines in this modules
  PUBLIC :: CRTM_Compute_AerosolScatter
  !PUBLIC :: CRTM_Compute_AerosolScatter_TL
  PUBLIC :: CRTM_Compute_AerosolScatter_AD


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  
  
  ! Number of stream angle definitions
  INTEGER, PARAMETER :: TWO_STREAMS       =  2
  INTEGER, PARAMETER :: FOUR_STREAMS      =  4
  INTEGER, PARAMETER :: SIX_STREAMS       =  6
  INTEGER, PARAMETER :: EIGHT_STREAMS     =  8
  INTEGER, PARAMETER :: SIXTEEN_STREAMS   = 16
  INTEGER, PARAMETER :: THIRTYTWO_STREAMS = 32
  
  
    
  
  
  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL and AD calls
  ! --------------------------------------
  TYPE, PUBLIC :: CRTM_ASVariables_type
    PRIVATE
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: ext = ZERO  !ke  ! Mass extinction coefficient
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: w0  = ZERO  !w   ! Single Scatter Albedo
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: g   = ZERO  !g   ! Asymmetry factor
    REAL(fp), DIMENSION(0:MAX_N_LEGENDRE_TERMS,&
                        MAX_N_PHASE_ELEMENTS,  &
                        MAX_N_LAYERS,          &
                        MAX_N_AEROSOLS         )  :: p_coef    !p      ! Phase coefficients
    REAL(fp), DIMENSION(MAX_N_LAYERS) :: Total_bs = ZERO  !bs  ! Volume scattering coefficient
  END TYPE CRTM_ASVariables_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AerosolScatter
!
! PURPOSE:
!       Function to compute the aerosol absorption and scattering properties
!       and populate the output AerosolScatter structure for a single channel.
!
! CATEGORY:
!       CRTM : AtmScatter
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter( Atmosphere,               &  ! Input
!                                                   GeometryInfo,             &  ! Input
!                                                   Channel_Index,            &  ! Input, scalar
!                                                   AerosolScatter,           &  ! Output        
!                                                   Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM_Atmosphere structure containing the atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_Atmosphere_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_GeometryInfo_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter: CRTM_AtmScatter structure containing the aerosol
!                        absorption and scattering properties required by
!                        the radiative transfer.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_AtmScatter_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! COMMENTS:
!       Note the INTENT on the output AerosolScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter( Atmosphere,     &  ! Input
                                        SensorIndex,    &  ! Input
                                        ChannelIndex,   &  ! Input
                                        AerosolScatter, &  ! Output
                                        ASV           , &  ! Internal variable output
                                        Message_Log )   &  ! Error messaging
                                      RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE(CRTM_Atmosphere_type),   INTENT( IN )     :: Atmosphere
    INTEGER,                      INTENT( IN )     :: ChannelIndex
    INTEGER,                      INTENT( IN )     :: SensorIndex
    
    ! -- Internal variable output
    TYPE(CRTM_ASVariables_type), INTENT(OUT) :: ASV

    ! -- Output 
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: AerosolScatter

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER(256) :: Message
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter'
    INTEGER :: k, ka, l, m, n
    REAL(fp) :: Wavenumber, Wavelength
    LOGICAL  :: Layer_Mask(Atmosphere%n_Layers)
    INTEGER  :: Layer_Index(Atmosphere%n_Layers)
    INTEGER  :: nAerosol_Layers
    INTEGER  :: Sensor_Type
    REAL(fp) :: bs
    
    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS
    ! Initialize and return if no aerosols
    AerosolScatter%Optical_Depth         = ZERO
    AerosolScatter%Single_Scatter_Albedo = ZERO
    AerosolScatter%Asymmetry_Factor      = ZERO
    IF (AeroC%n_Phase_Elements > 0) AerosolScatter%Phase_Coefficient = ZERO
    IF (Atmosphere%n_Aerosols == 0) RETURN
    ! Wavelength in Microns
    Wavelength = 10000.0_fp/SC(SensorIndex)%Wavenumber(ChannelIndex)
    Sensor_Type = SC(SensorIndex)%Sensor_Type(ChannelIndex)
    ! Determine offset for Legendre coefficients in the
    ! AeroC lookup table corresponding to the 
    ! number of streams
    SELECT CASE(AerosolScatter%n_Legendre_Terms)
      CASE (TWO_STREAMS)    ; AerosolScatter%lOffset = 0
      CASE (FOUR_STREAMS)   ; AerosolScatter%lOffset = 0
      CASE (SIX_STREAMS)    ; AerosolScatter%lOffset = 5
      CASE (EIGHT_STREAMS)  ; AerosolScatter%lOffset = 12
      CASE (SIXTEEN_STREAMS); AerosolScatter%lOffset = 21
      CASE DEFAULT
        AerosolScatter%lOffset = 0  
        ! Use two-stream model or HG and RAYLEIGH Phase function
        IF( HGPHASE ) THEN
          AerosolScatter%n_Legendre_Terms = 0
        ELSE
          Error_Status = FAILURE
          WRITE(Message,'("The n_Legendre_Terms in AerosolScatter, ",i0,", do not fit model")') &
                        AerosolScatter%n_Legendre_Terms
          CALL Display_Message(ROUTINE_NAME,           &
                               TRIM(Message),          &
                               Error_Status,           &
                               Message_Log=Message_Log )
          RETURN
        END IF
    END SELECT
    !PRINT *, AerosolScatter%n_Legendre_Terms
    ! -----------------------------------------------
    ! Loop over the different Aerosols in the profile
    ! -----------------------------------------------
    Aerosol_loop: DO n = 1, Atmosphere%n_Aerosols
    
    
      
                
      ! Only process aerosols with more
      ! than the threshold Aerosol amount
      Layer_Mask  = Atmosphere%Aerosol(n)%Concentration > AEROSOL_CONTENT_THRESHOLD
      nAerosol_Layers = COUNT(Layer_Mask)
      IF ( nAerosol_Layers == 0 ) CYCLE Aerosol_loop
      
        ! --------------------------------------
        ! Loop over the current Aerosol's Layers
        ! --------------------------------------
        Layer_Index(1:nAerosol_Layers) = PACK((/(k,k=1,Atmosphere%Aerosol(n)%n_Layers)/), Layer_Mask)
          Aerosol_Layer_loop: DO k = 1, nAerosol_Layers
            ka = Layer_Index(k)
        
            IF( Sensor_Type == INFRARED_SENSOR .OR. Sensor_Type == VISIBLE_SENSOR) THEN
              ! Forward AerosolScatter interpolation routine
              CALL Get_Aerosol_Opt(AerosolScatter                              , & ! Input
                                   Wavelength                                  , & ! Input
                                   Atmosphere%Aerosol(n)%Type                  , & ! Input
                                   Atmosphere%Aerosol(n)%Effective_Radius(ka)  , & ! Input                               
                                   ASV%ext(ka,n)                               , & ! Output
                                   ASV%w0(ka,n)                                , & ! Output
                                   ASV%g(ka,n)                                 , & ! Output
                                   ASV%p_coef(:,:,ka,n)                          )

              ! Compute the volume scattering coefficient for the current
              ! aerosol layer and accumulate it for the layer total for the
              ! profile (i.e. all aerosols)
              !   bs = rho.w.ke
              ! where
              !   bs  = volume scattering coefficient for a layer [dimensionless]
              !   rho = integrated aerosol concentration for a layer (g/m^2) [M.L^-2]
              !   w   = single scatter albedo [dimensionless]
              !   ke  = mass extintion coefficient (m^2/g) [L^2.M^-1]

              bs = Atmosphere%Aerosol(n)%Concentration(ka) * ASV%w0(ka,n) * ASV%ext(ka,n)
              ASV%Total_bs(ka) = ASV%Total_bs(ka) + bs 

              ! Compute the optical depth (absorption + scattering)
              !   tau = rho.ke
              ! where
              !   rho = Integrated Aerosol Concentration for a layer(g/m^2) [M.L^-2]
              !   ke  = mass extintion coefficient (m^2/g) [L^2.M^-1]
              ! Note that since all these computations are done for a given
              ! layer, the optical depth is the same as the volume extinction
              ! coefficient, be. Usually,
              !   tau = be.d(z)
              ! but we are working with height/thickness independent quantities
              ! so that
              !   tau = be
              ! This is why the optical depth is used in the denominator to
              ! compute the single scatter albedo in the Layer_loop below.
              AerosolScatter%Optical_Depth(ka) = AerosolScatter%Optical_Depth(ka) + &
                                               (ASV%ext(ka,n)*Atmosphere%Aerosol(n)%Concentration(ka))

              ! Compute and sum the asymmetry factor
              !   g = g + g(LUT)*bs
              ! where
              !   g(LUT) = the asymmetry factor from the LUT
              AerosolScatter%Asymmetry_Factor(ka) = AerosolScatter%Asymmetry_Factor(ka) + &
                                                    (ASV%g(ka,n) * bs)
            END IF
            ! Compute the phase matrix coefficients
            ! p = p + p(LUT)*bs
            ! where
            !   p(LUT) = the phase coefficient from the LUT
            IF( AerosolScatter%n_Phase_Elements > 0 ) THEN
              DO m = 1, AerosolScatter%n_Phase_Elements
                DO l = 0, AerosolScatter%n_Legendre_Terms
                  AerosolScatter%Phase_Coefficient(l,m,ka) = AerosolScatter%Phase_Coefficient(l,m,ka) + &
                                                           (ASV%p_coef(l,m,ka,n) * bs) 
                END DO
              END DO
            END IF
          END DO Aerosol_Layer_loop
    END DO Aerosol_loop                                      

    ! --------------------------------------------
    ! Accumulate optical properties for all Aerosols
    ! --------------------------------------------
    ! Some short names
    l = AerosolScatter%n_Legendre_Terms

    ! Begin full atmosphere layer loop                                                                                                                          
    Layer_Loop: DO k = 1, Atmosphere%n_Layers

      ! Only process layers that scatter
      IF (ASV%Total_bs(k) < BS_THRESHOLD) CYCLE Layer_loop

        ! Normalize the asymmetry factor with the total
        ! volume scattering coefficient, bs.
        AerosolScatter%Asymmetry_Factor(k) = AerosolScatter%Asymmetry_Factor(k) / ASV%Total_bs(k)

        IF ( AerosolScatter%n_Phase_Elements > 0 ) THEN
          IF (l > 2) THEN
            ! Normalize the phase matrix coefficients with
            ! the total volume scattering coefficient, bs.
            DO m = 1, AerosolScatter%n_Phase_Elements
              AerosolScatter%Phase_Coefficient(0:l,m,k) = AerosolScatter%Phase_Coefficient(0:l,m,k) / &
                                                          ASV%Total_bs(k)
            END DO
          ELSE
            ! Henyey-Greenstein phase function
            AerosolScatter%Phase_Coefficient(1,1,k) = ONEpointFIVE * AerosolScatter%Asymmetry_Factor(k)
            AerosolScatter%Phase_Coefficient(2,1,k) = ZERO
          END IF
          
          ! Normalization requirement
          AerosolScatter%Phase_Coefficient(0,1,k) = POINT_5
          AerosolScatter%Single_Scatter_Albedo(k) = ASV%Total_bs(k) / AerosolScatter%Optical_Depth(k)

          AerosolScatter%Delta_Truncation(k) = AerosolScatter%Phase_Coefficient(l,1,k)
        END IF
    END DO Layer_loop
                                                     
  END FUNCTION CRTM_Compute_AerosolScatter


  !
!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AerosolScatter_TL
!
! PURPOSE:
!       Function to compute the tangent-linear aerosol absorption and 
!       scattering properties and populate the output AerosolScatter_TL
!       structure for a single channel.
!
! CATEGORY:
!       CRTM : AtmScatter
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_TL( Atmosphere,               &  ! Input
!                                                      AerosolScatter,           &  ! Input
!                                                      Atmosphere_TL,            &  ! Input
!                                                      GeometryInfo,             &  ! Input
!                                                      Channel_Index,            &  ! Input, scalar
!                                                      AerosolScatter_TL,        &  ! Output  
!                                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmScatter_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:      CRTM Atmosphere structure containing the tangent-linear
!                           atmospheric state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:       CRTM_GeometryInfo structure containing the 
!                           view geometry information.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_GeometryInfo_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter_TL: CRTM_AtmScatter structure containing the tangent-linear
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmScatter_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! COMMENTS:
!       Note the INTENT on the output AerosolScatter_TL argument is IN OUT
!       rather than just OUT. This is necessary because the argument may be
!       defined upon input. To prevent memory leaks, the IN OUT INTENT is
!       a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_TL( Atmosphere             , & ! Input
                                           AerosolScatter         , & ! Input
                                           Atmosphere_TL          , & ! Input
                                           SensorIndex            , & ! Input
                                           ChannelIndex           , & ! Input
                                           AerosolScatter_TL      , & ! Input
                                           ASV                    , & ! Internal variable input
                                           Message_Log            ) & ! Error messaging
                                         RESULT( Error_Status )
    
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE(CRTM_Atmosphere_type) ,   INTENT(IN)     :: Atmosphere,Atmosphere_TL
    INTEGER                    ,   INTENT(IN)     :: SensorIndex
    INTEGER                    ,   INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AtmScatter_type) ,   INTENT(IN OUT) :: AerosolScatter
    TYPE(CRTM_AtmScatter_type) ,   INTENT(IN OUT) :: AerosolScatter_TL
    TYPE(CRTM_ASVariables_type),   INTENT(IN)     :: ASV
    CHARACTER(*),      OPTIONAL,   INTENT(IN)     :: Message_Log

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status

    ! ----------------
    ! Local parameters
    ! ----------------
     
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_TL'


    
    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


  END FUNCTION CRTM_Compute_AerosolScatter_TL

  


 

!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_AerosolScatter_AD
!
! PURPOSE:
!       Function to compute the adjoint aerosol absorption and scattering
!       properties for a single channel.
!
! CATEGORY:
!       CRTM : AtmScatter
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_AD(  Atmosphere,               &  ! Input
!                                                       AerosolScatter,           &  ! Input
!                                                       AerosolScatter_AD,        &  ! Input
!                                                       GeometryInfo,             &  ! Input
!                                                       Channel_Index,            &  ! Input
!                                                       Atmosphere_AD,            &  ! Output  
!                                                       Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmScatter_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       AerosolScatter_AD:  CRTM_AtmScatter structure containing the adjoint
!                           aerosol absorption and scattering properties.
!                           **NOTE: On EXIT from this function, the contents of
!                                   this structure may be modified (e.g. set to
!                                   zero.)
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_AtmScatter_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:       CRTM_GeometryInfo structure containing the 
!                           view geometry information.
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_GeometryInfo_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:      CRTM Atmosphere structure containing the adjoint
!                           atmospheric state data.
!                           **NOTE: On ENTRY to this function, the contents of
!                                   this structure should be defined (e.g.
!                                   initialized to some value based on the
!                                   position of this function in the call chain.)
!                           UNITS:      N/A
!                           TYPE:       TYPE( CRTM_Atmosphere_type )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTUPT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the computation was sucessful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!
! SIDE EFFECTS:
!
! RESTRICTIONS:
!
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_AD( Atmosphere,        &  ! Input
                                           AerosolScatter,    &  ! Input
                                           AerosolScatter_AD, &  ! Input
                                           GeometryInfo,      &  ! Input
                                           Channel_Index,     &  ! Input
                                           Atmosphere_AD,     &  ! Output
                                           Message_Log )      &  ! Error messaging
                                         RESULT ( Error_Status )               


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: AerosolScatter
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: AerosolScatter_AD
    TYPE( CRTM_GeometryInfo_type ), INTENT( IN )     :: GeometryInfo
    INTEGER,                        INTENT( IN )     :: Channel_Index

    ! -- Output 
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN OUT ) :: Atmosphere_AD

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_AD'


    ! ---------------
    ! Local variables
    ! ---------------




    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



!  *** USERS INSERT CODE HERE ***




  END FUNCTION CRTM_Compute_AerosolScatter_AD
  
!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
  
    ! ----------------------------------------
    ! Subroutine to obtain the bulk 
    ! optical properties of aerosol
    ! extinction coefficient (ext),
    ! scattering coefficient (w0)
    ! asymmetry factor (g), and
    ! spherical Legendre coefficients (p_coef)
    ! ----------------------------------------
    SUBROUTINE Get_Aerosol_Opt(AerosolScatter,  &  ! Input AerosolScatter structure
                               Wavelength    ,  &  ! Input in microns
                               Aerosol_Type  ,  &  ! Input see CRTM_Aerosol_Define.f90
                               Reff          ,  &  ! Input effective radius (mm)
                               ext           ,  &  ! Output optical depth
                               w0            ,  &  ! Output single scattering albedo
                               g             ,  &  ! Output asymmetry factor
                               p_coef           )  ! Output spherical Legendre coefficients
      ! Arguments
      TYPE(CRTM_AtmScatter_type), INTENT(IN)      :: AerosolScatter
      REAL(fp)                  , INTENT(IN)      :: Wavelength
      INTEGER                   , INTENT(IN)      :: Aerosol_Type
      REAL(fp)                  , INTENT(IN)      :: Reff
      REAL(fp)                  , INTENT(OUT)     :: ext
      REAL(fp)                  , INTENT(OUT)     :: w0
      REAL(fp)                  , INTENT(OUT)     :: g
      REAL(fp)                  , INTENT(IN OUT)  :: p_coef(0:,:)
      ! Local variables
      INTEGER  :: k
      INTEGER  :: l, lOffset
      INTEGER  :: i1, i2
      INTEGER  :: j1, j2
      REAL(fp) :: f_int, r_int
      REAL(fp), DIMENSION(INTERP_NPTS) :: f, r
      REAL(fp), DIMENSION(INTERP_NPTS) :: wlp, xlp

      ! Temporarily assign lOffset to ZERO
      lOffset = 0

      ! Assign a type index
      SELECT CASE (Aerosol_Type)
        CASE(DUST_AEROSOL)  ; k=1
        CASE(SEASALT_SSAM_AEROSOL)  ; k=2
        CASE(SEASALT_SSCM_AEROSOL)  ; k=3
        CASE(DRY_ORGANIC_CARBON_AEROSOL)  ; k=4
        CASE(WET_ORGANIC_CARBON_AEROSOL)  ; k=5
        CASE(DRY_BLACK_CARBON_AEROSOL)  ; k=6
        CASE(WET_BLACK_CARBON_AEROSOL)  ; k=7
        CASE(SULFATE_AEROSOL)  ; k=8
      END SELECT

      ! Find the wavelength and effective
      ! radius indices for interpolation
      f_int = MAX(MIN(AeroC%Wavelength(AeroC%n_Wavelength),Wavelength),AeroC%Wavelength(1))
      CALL find_index(AeroC%Wavelength(:),f_int,i1,i2)
      f = AeroC%Wavelength(i1:i2)

      r_int = MAX(MIN(AeroC%Aerosol_Reff(AeroC%n_Reff,Aerosol_Type),Reff),AeroC%Aerosol_Reff(1,Aerosol_Type)) 
      CALL find_index(AeroC%Aerosol_Reff(:,k), r_int, j1, j2)
      r = AeroC%Aerosol_Reff(j1:j2,k) 

      ! Calculate the Lagrange polynomials
      wlp = lpoly(f, f_int)
      xlp = lpoly(r, r_int)


      ! Perform Interpolation
      CALL interp_2D(AeroC%Mass_Extinction(j1:j2,k,i1:i2), xlp, wlp, ext)
      CALL interp_2D(AeroC%Scattering_Albedo(j1:j2,k,i1:i2), xlp, wlp, w0)
      CALL interp_2D(AeroC%Asymmetry_Factor(j1:j2,k,i1:i2), xlp, wlp, g)

      IF (AerosolScatter%n_Phase_Elements > 0 .AND.  &
          AerosolScatter%n_Legendre_Terms > 2        ) THEN
        DO l = 0, AerosolScatter%n_Legendre_Terms
          CALL interp_2D(AeroC%Phase_Coef(l+lOffset,j1:j2,k,i1:i2), xlp, wlp, p_coef(l,1))
        END DO
      END IF



    END SUBROUTINE Get_Aerosol_Opt

END MODULE CRTM_AerosolScatter


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id$
!
! $Date: 2006/05/25 19:27:59 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_AerosolScatter.f90,v $
! Revision 1.4  2006/05/25 19:27:59  wd20pd
! Removed redundant parameter definitions.
!
! Revision 1.3  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2005/02/25 17:49:48  paulv
! - Fixed incorrect function names.
!
! Revision 1.1  2005/02/25 00:13:14  paulv
! Initial checkin.
!
!
!

