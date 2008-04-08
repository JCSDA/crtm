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

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp
  USE Message_Handler,          ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, ONE, POINT_5, ONEpointFIVE, &
                                      MAX_N_LAYERS, &
                                      MAX_N_AEROSOLS, &
                                      AEROSOL_CONTENT_THRESHOLD, &
                                      BS_THRESHOLD, &
                                      MAX_N_LEGENDRE_TERMS, &
                                      MAX_N_PHASE_ELEMENTS, &
                                      HGPHASE  ! <<< NEED TO REMOVE THIS IN FUTURE
  USE CRTM_SpcCoeff,            ONLY: SC, &
                                      INFRARED_SENSOR, VISIBLE_SENSOR, MICROWAVE_SENSOR
  USE CRTM_AerosolCoeff,        ONLY: AeroC 
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type      , &
                                      DUST_AEROSOL              , &
                                      SEASALT_SSAM_AEROSOL      , &
                                      SEASALT_SSCM_AEROSOL      , &
                                      DRY_ORGANIC_CARBON_AEROSOL, &
                                      WET_ORGANIC_CARBON_AEROSOL, &
                                      DRY_BLACK_CARBON_AEROSOL  , &
                                      WET_BLACK_CARBON_AEROSOL  , &
                                      SULFATE_AEROSOL
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_Interpolation,       ONLY: NPTS        , &
                                      LPoly_type  , &
                                      find_index  , &
                                      interp_1D   , &
                                      interp_2D   , &
                                      interp_3D   , &
                                      interp_2D_TL, &
                                      interp_3D_TL, &
                                      interp_2D_AD, &
                                      interp_3D_AD, &
                                      Clear_LPoly , &
                                      LPoly       , &
                                      LPoly_TL    , &
                                      LPoly_AD
  USE CRTM_AtmScatter_Define,   ONLY: CRTM_AtmScatter_type      , &
                                      CRTM_Associated_AtmScatter, &
                                      CRTM_Destroy_AtmScatter   , &
                                      CRTM_Allocate_AtmScatter  , &
                                      CRTM_Assign_AtmScatter
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  
  ! Public types
  ! ------------
  ! Use associated
  PUBLIC :: CRTM_AtmScatter_type
  
  ! Public procedures
  ! -----------------
  PUBLIC :: CRTM_Compute_AerosolScatter
  PUBLIC :: CRTM_Compute_AerosolScatter_TL
  PUBLIC :: CRTM_Compute_AerosolScatter_AD
  ! Use associated
  PUBLIC :: CRTM_Associated_AtmScatter
  PUBLIC :: CRTM_Destroy_AtmScatter
  PUBLIC :: CRTM_Allocate_AtmScatter
  PUBLIC :: CRTM_Assign_AtmScatter


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'  
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
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
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: ke = ZERO  ! Mass extinction coefficient
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: w  = ZERO  ! Single Scatter Albedo
    REAL(fp), DIMENSION(MAX_N_LAYERS, MAX_N_AEROSOLS) :: g  = ZERO  ! Asymmetry factor
    REAL(fp), DIMENSION(0:MAX_N_LEGENDRE_TERMS,&
                        MAX_N_PHASE_ELEMENTS,  &
                        MAX_N_LAYERS,          &
                        MAX_N_AEROSOLS         )  :: pcoeff = ZERO   ! Phase coefficients
    REAL(fp), DIMENSION(MAX_N_LAYERS) :: Total_bs = ZERO             ! Volume scattering coefficient
  END TYPE CRTM_ASVariables_type


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter
!
! PURPOSE:
!       Function to compute the aerosol absorption and scattering properties
!       and populate the output AerosolScatter structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter( Atmosphere             , &  ! Input
!                                                   SensorIndex            , &  ! Input
!                                                   ChannelIndex           , &  ! Input
!                                                   AerosolScatter         , &  ! Output    
!                                                   ASV                    , &  ! Internal variable output
!                                                   Message_Log=Message_Log  )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:      CRTM_Atmosphere structure containing the atmospheric
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Atmosphere_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This ia a unique index associated
!                        with a sensor. 
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:       
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter: CRTM_AtmScatter structure containing the aerosol
!                        absorption and scattering properties required by
!                        the radiative transfer.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmScatter_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!        ASVariables:    Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AerosolScatter module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_ASVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
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
! COMMENTS:
!       Note the INTENT on the output AerosolScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter( Atm         , &  ! Input
                                        SensorIndex , &  ! Input
                                        ChannelIndex, &  ! Input
                                        AScat       , &  ! Output
                                        ASV         , &  ! Internal variable output
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )
    !Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)     :: Atm
    INTEGER,                     INTENT(IN)     :: ChannelIndex
    INTEGER,                     INTENT(IN)     :: SensorIndex
    TYPE(CRTM_AtmScatter_type),  INTENT(IN OUT) :: AScat
    TYPE(CRTM_ASVariables_type), INTENT(OUT)    :: ASV
    CHARACTER(*), OPTIONAL,      INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status    
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter'
    ! Local Variables
    CHARACTER(ML) :: Message
    INTEGER :: k, ka, l, m, n
    INTEGER  :: Sensor_Type
    REAL(fp) :: Wavelength
    LOGICAL  :: Layer_Mask(Atm%n_Layers)
    INTEGER  :: Layer_Index(Atm%n_Layers)
    INTEGER  :: nAerosol_Layers
    REAL(fp) :: bs
    
    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Sensor Type
    Sensor_Type = SC(SensorIndex)%Sensor_Type
    IF (Sensor_Type == MICROWAVE_SENSOR) RETURN 
    ! Initialize and return if no aerosols
    AScat%Optical_Depth         = ZERO
    AScat%Single_Scatter_Albedo = ZERO
    AScat%Asymmetry_Factor      = ZERO
    IF (AeroC%n_Phase_Elements > 0) AScat%Phase_Coefficient = ZERO
    IF (Atm%n_Aerosols == 0) RETURN
    ! Wavelength in microns
    Wavelength = 10000.0_fp/SC(SensorIndex)%Wavenumber(ChannelIndex)
    
    ! Determine offset for Legendre coefficients in the
    ! AeroC lookup table corresponding to the 
    ! number of streams        
    SELECT CASE(AScat%n_Legendre_Terms)
      CASE (TWO_STREAMS)    ; AScat%lOffset = 0
      CASE (FOUR_STREAMS)   ; AScat%lOffset = 0
      CASE (SIX_STREAMS)    ; AScat%lOffset = 5
      CASE (EIGHT_STREAMS)  ; AScat%lOffset = 12
      CASE (SIXTEEN_STREAMS); AScat%lOffset = 21
      CASE DEFAULT
        AScat%lOffset = 0  
        ! Use two-stream model or HG and RAYLEIGH Phase function
        IF( HGPHASE ) THEN
          AScat%n_Legendre_Terms = 0
        ELSE
          Error_Status = FAILURE
          WRITE(Message,'("The n_Legendre_Terms in AerosolScatter, ",i0,", do not fit model")') &
                        AScat%n_Legendre_Terms
          CALL Display_Message(ROUTINE_NAME,           &
                               TRIM(Message),          &
                               Error_Status,           &
                               Message_Log=Message_Log )
          RETURN
        END IF
    END SELECT
    
    
    ! -----------------------------------------------
    ! Loop over the different Aerosols in the profile
    ! -----------------------------------------------
    Aerosol_loop: DO n = 1, Atm%n_Aerosols
                
      ! Only process aerosols with more
      ! than the threshold Aerosol amount
      Layer_Mask  = Atm%Aerosol(n)%Concentration > AEROSOL_CONTENT_THRESHOLD
      nAerosol_Layers = COUNT(Layer_Mask)
      IF ( nAerosol_Layers == 0 ) CYCLE Aerosol_loop
      
      
      ! --------------------------------------
      ! Loop over the current Aerosol's Layers
      ! --------------------------------------
      Layer_Index(1:nAerosol_Layers) = PACK((/(k,k=1,Atm%Aerosol(n)%n_Layers)/), Layer_Mask)
      Aerosol_Layer_loop: DO k = 1, nAerosol_Layers
        ka = Layer_Index(k)
      
        ! Get the aerosol optical properties
        CALL Get_Aerosol_Opt( AScat                              , & ! Input
                              Wavelength                         , & ! Input
                              Atm%Aerosol(n)%Type                , & ! Input
                              Atm%Aerosol(n)%Effective_Radius(ka), & ! Input                               
                              ASV%ke(ka,n)                       , & ! Output
                              ASV%w(ka,n)                        , & ! Output
                              ASV%g(ka,n)                        , & ! Output
                              ASV%pcoeff(:,:,ka,n)                 )
          
        ! Compute the volume scattering coefficient for the current
        ! aerosol layer and accumulate it for the layer total for the
        ! profile (i.e. all aerosols)
        !   bs = rho.w.ke
        ! where
        !   bs  = volume scattering coefficient for a layer [dimensionless]
        !   rho = integrated aerosol concentration for a layer (kg/m^2) [M.L^-2]
        !   w   = single scatter albedo [dimensionless]
        !   ke  = mass extintion coefficient (m^2/kg) [L^2.M^-1]
        bs = Atm%Aerosol(n)%Concentration(ka) * ASV%w(ka,n) * ASV%ke(ka,n)
        ASV%Total_bs(ka) = ASV%Total_bs(ka) + bs 
        
        ! Compute the optical depth (absorption + scattering)
        !   tau = rho.ke
        ! where
        !   rho = Integrated Aerosol Concentration for a layer(kg/m^2) [M.L^-2]
        !   ke  = mass extintion coefficient (m^2/kg) [L^2.M^-1]
        ! Note that since all these computations are done for a given
        ! layer, the optical depth is the same as the volume extinction
        ! coefficient, be. Usually,
        !   tau = be.d(z)
        ! but we are working with height/thickness independent quantities
        ! so that
        !   tau = be
        ! This is why the optical depth is used in the denominator to
        ! compute the single scatter albedo in the Layer_loop below.
        AScat%Optical_Depth(ka) = AScat%Optical_Depth(ka) + &
                                  (ASV%ke(ka,n)*Atm%Aerosol(n)%Concentration(ka))

        ! Compute and sum the asymmetry factor
        !   g = g + g(LUT)*bs
        ! where
        !   g(LUT) = the asymmetry factor from the LUT
        AScat%Asymmetry_Factor(ka) = AScat%Asymmetry_Factor(ka) + &
                                     (ASV%g(ka,n) * bs)
        ! Compute the phase matrix coefficients
        ! p = p + p(LUT)*bs
        ! where
        !   p(LUT) = the phase coefficient from the LUT
        IF( AScat%n_Phase_Elements > 0 ) THEN
          DO m = 1, AScat%n_Phase_Elements
            DO l = 0, AScat%n_Legendre_Terms
              AScat%Phase_Coefficient(l,m,ka) = AScat%Phase_Coefficient(l,m,ka) + &
                                                (ASV%pcoeff(l,m,ka,n) * bs) 
            END DO
          END DO
        END IF
      END DO Aerosol_Layer_loop
    END DO Aerosol_loop                                      


    ! --------------------------------------------
    ! Accumulate optical properties for all Aerosols
    ! --------------------------------------------
    ! Some short names
    l = AScat%n_Legendre_Terms

    ! Begin full atmosphere layer loop
    Layer_Loop: DO k = 1, Atm%n_Layers
      ! Only process layers that scatter
      IF (ASV%Total_bs(k) < BS_THRESHOLD) CYCLE Layer_loop

        ! Normalize the asymmetry factor with the total
        ! volume scattering coefficient, bs.
        AScat%Asymmetry_Factor(k) = AScat%Asymmetry_Factor(k) / ASV%Total_bs(k)

        IF ( AScat%n_Phase_Elements > 0 ) THEN
          IF (l > 2) THEN
            ! Normalize the phase matrix coefficients with
            ! the total volume scattering coefficient, bs.
            DO m = 1, AScat%n_Phase_Elements
              AScat%Phase_Coefficient(0:l,m,k) = AScat%Phase_Coefficient(0:l,m,k) / &
                                                          ASV%Total_bs(k)
            END DO
          ELSE
            ! Henyey-Greenstein phase function
            AScat%Phase_Coefficient(1,1,k) = ONEpointFIVE * AScat%Asymmetry_Factor(k)
            AScat%Phase_Coefficient(2,1,k) = ZERO
          END IF
          
          ! Normalization requirement
          AScat%Phase_Coefficient(0,1,k) = POINT_5
          AScat%Single_Scatter_Albedo(k) = ASV%Total_bs(k) / AScat%Optical_Depth(k)

          AScat%Delta_Truncation(k) = AScat%Phase_Coefficient(l,1,k)
        END IF
        
    END DO Layer_loop
                                                    
  END FUNCTION CRTM_Compute_AerosolScatter


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter_TL
!
! PURPOSE:
!       Function to compute the tangent-linear aerosol absorption and 
!       scattering properties and populate the output AerosolScatter_TL
!       structure for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_TL( Atmosphere             , &  ! Input
!                                                      AerosolScatter         , &  ! Input
!                                                      Atmosphere_TL          , &  ! Input
!                                                      SensorIndex            , &  ! Input
!                                                      ChannelIndex           , &  ! Input
!                                                      AerosolScatter_TL      , &  ! Output  
!                                                      ASV                    , &  ! Internal Variable input
!                                                      Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Atmosphere_TL:      CRTM Atmosphere structure containing the tangent-linear
!                           atmospheric state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:        Sensor index id. This ia a unique index associated
!                           with a sensor. 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!        ASVariables:       Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AerosolScatter module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_ASVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        AerosolScatter_TL: CRTM_AtmScatter structure containing the tangent-linear
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the computation was sucessful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AerosolScatter_TL argument is IN OUT
!       rather than just OUT. This is necessary because the argument may be
!       defined upon input. To prevent memory leaks, the IN OUT INTENT is
!       a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_TL( Atm         , & ! FWD Input
                                           AScat       , & ! FWD Input
                                           Atm_TL      , & ! TL  Input
                                           SensorIndex , & ! Input
                                           ChannelIndex, & ! Input
                                           AScat_TL    , & ! TL  Input
                                           ASV         , & ! Internal variable input
                                           Message_Log ) & ! Error messaging
                                         RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atm
    TYPE(CRTM_AtmScatter_type) , INTENT(IN)     :: AScat
    TYPE(CRTM_Atmosphere_type) , INTENT(IN)     :: Atm_TL
    INTEGER                    , INTENT(IN)     :: SensorIndex
    INTEGER                    , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_AtmScatter_type) , INTENT(IN OUT) :: AScat_TL
    TYPE(CRTM_ASVariables_type), INTENT(IN)     :: ASV
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_TL'
    ! Local variables
    INTEGER  :: k, ka, l, m, n
    INTEGER  :: n_Legendre_Terms, n_Phase_Elements
    INTEGER  :: Sensor_Type
    REAL(fp) :: Wavelength
    LOGICAL  :: Layer_Mask(Atm%n_Layers)
    INTEGER  :: Layer_Index(Atm%n_Layers)
    INTEGER  :: nAerosol_Layers
    REAL(fp) :: ke_TL, w_TL, g_TL
    REAL(fp) :: pcoeff_TL(0:AScat%n_Legendre_Terms, AScat%n_Phase_Elements)
    REAL(fp) :: bs, bs_TL
    REAL(fp) :: Total_bs_TL(Atm%n_Layers)
    
    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Sensor Type
    Sensor_Type = SC(SensorIndex)%Sensor_Type
    IF (Sensor_Type == MICROWAVE_SENSOR) RETURN
    ! Initialize and return if no Aerosols
    AScat_TL%Optical_Depth         = ZERO
    AScat_TL%Single_Scatter_Albedo = ZERO
    AScat_TL%Asymmetry_Factor      = ZERO
    IF (AeroC%n_Phase_Elements > 0) AScat_TL%Phase_Coefficient = ZERO
    IF (Atm%n_Aerosols == 0) RETURN
    Total_bs_TL = ZERO
    ! Wavelength in Microns
    Wavelength = 10000.0_fp/SC(SensorIndex)%Wavenumber(ChannelIndex)
    ! Phase matrix dimensions
    n_Legendre_Terms = AScat_TL%n_Legendre_Terms
    n_Phase_Elements = AScat_TL%n_Phase_Elements
    AScat_TL%lOffset = AScat%lOffset

    
    ! -----------------------------------------------
    ! Loop over the different Aerosols in the profile
    ! -----------------------------------------------
    Aerosol_loop: DO n = 1, Atm%n_Aerosols
    
      ! Only process aerosols with more
      ! than the threshold aerosol amount
      Layer_Mask = Atm%Aerosol(n)%Concentration > AEROSOL_CONTENT_THRESHOLD
      nAerosol_Layers = COUNT(Layer_Mask)
      IF (nAerosol_Layers == 0) CYCLE Aerosol_loop
      
      
      ! --------------------------------------
      ! Loop over the current aerosol's layers
      ! --------------------------------------
      Layer_Index(1:nAerosol_Layers) = PACK((/(k,k=1,Atm%Aerosol(n)%n_Layers)/), Layer_Mask)
      Aerosol_Layer_loop: DO k = 1, nAerosol_Layers
        ka = Layer_Index(k)

        ! Obtain bulk aerosol optical properties
        CALL Get_Aerosol_Opt_TL(AScat_TL                              , & ! Input
                                Wavelength                            , & ! Input
                                Atm%Aerosol(n)%Type                   , & ! Input
                                Atm%Aerosol(n)%Effective_Radius(ka)   , & ! FWD Input
                                Atm_TL%Aerosol(n)%Effective_Radius(ka), & ! TL  Input
                                ke_TL                                 , & ! TL  Output
                                w_TL                                  , & ! TL  Output
                                g_TL                                  , & ! TL  Output
                                pcoeff_TL                               ) ! TL  Output

        ! Compute the volume scattering coefficient
        bs = Atm%Aerosol(n)%Concentration(ka) * ASV%w(ka,n) * ASV%ke(ka,n)                     
        bs_TL = (Atm_TL%Aerosol(n)%Concentration(ka) * ASV%w(ka,n) * ASV%ke(ka,n)) + &
                (Atm%Aerosol(n)%Concentration(ka)    * w_TL        * ASV%ke(ka,n)) + &
                (Atm%Aerosol(n)%Concentration(ka)    * ASV%w(ka,n) * ke_TL       )
        Total_bs_TL(ka) = Total_bs_TL(ka) + bs_TL

        ! Compute the optical depth (absorption + scattering)
        AScat_TL%Optical_Depth(ka) = AScat_TL%Optical_Depth(ka) + &
                                     (ke_TL        * Atm%Aerosol(n)%Concentration(ka)) + &
                                     (ASV%ke(ka,n) * Atm_TL%Aerosol(n)%Concentration(ka))

        ! Compute the asymmetry factor
        AScat_TL%Asymmetry_Factor(ka) = AScat_TL%Asymmetry_Factor(ka) + &
                                        (g_TL * bs) + &
                                        (ASV%g(ka,n) * bs_TL)

        ! Compute the phase matrix coefficients
        IF( n_Phase_Elements > 0) THEN
          DO m = 1, n_Phase_Elements
            DO l = 0, n_Legendre_Terms
              AScat_TL%Phase_Coefficient(l,m,ka) = AScat_TL%Phase_Coefficient(l,m,ka) + &
                                                   (pcoeff_TL(l,m)       * bs   ) + &
                                                   (ASV%pcoeff(l,m,ka,n) * bs_TL)
            END DO
          END DO
        END IF
      END DO Aerosol_Layer_loop
    END DO Aerosol_loop      


    ! ----------------------------------------------
    ! Accumulate optical properties for all aerosols
    ! ----------------------------------------------
    ! Some short names
    l = n_Legendre_Terms
    
    ! Begin full atmosphere layer loop
    Layer_loop: DO k = 1, Atm%n_Layers
    
      ! Only process layers that scatter
      IF (ASV%Total_bs(k) < BS_THRESHOLD) CYCLE Layer_loop
      
      ! Normalize the asymmetry factor with the total
      ! volume scattering coefficient, bs.
      ! NOTE: the second term is NOT divided by
      !       ASV%Total_bs(k)**2 because the forward
      !       model asymmetry factor for this layer
      !       has already been divided once by
      !       ASV%Total_bs(k).
      AScat_TL%Asymmetry_Factor(k) = &
        (AScat_TL%Asymmetry_Factor(k) - (AScat%Asymmetry_Factor(k)*Total_bs_TL(k))) / &
        ASV%Total_bs(k)
        
      IF (n_Phase_Elements > 0) THEN
        IF (l > 2) THEN
          ! Normalise the phase matrix coefficients with
          ! the total volume scattering coefficient, bs.
          ! NOTE: the second term is NOT divided by
          !       ASV%Total_bs(k)**2 because the forward
          !       model phase coefficients for this layer
          !       have already been divided once by
          !       ASV%Total_bs(k).
          DO m = 1, n_Phase_Elements
            AScat_TL%Phase_Coefficient(0:l,m,k) = &
              (AScat_TL%Phase_Coefficient(0:l,m,k) - (AScat%Phase_Coefficient(0:l,m,k)*Total_bs_TL(k))) / &
              ASV%Total_bs(k)
          END DO
        ELSE
          ! Henyey-Greenstein phase function
          AScat_TL%Phase_Coefficient(1,1,k) = ONEpointFIVE * AScat_TL%Asymmetry_Factor(k)
          AScat_TL%Phase_Coefficient(2,1,k) = ZERO
        END IF
        
        ! Normalization requirement
        ! NOTE: the second term of the single scatter
        !       albedo computation is NOT divided by
        !       AScat%Optical_Depth(k)**2 because
        !       the forward model single scatter albedo
        !       is used rather than recomputing it again
        !       here (i.e. the total scattering coefficient
        !       divided by the optical depth).
        AScat_TL%Phase_Coefficient(0,1,k) = ZERO
        AScat_TL%Single_Scatter_Albedo(k) = &
          (Total_bs_TL(k) - (AScat%Single_Scatter_Albedo(k)*AScat_TL%Optical_Depth(k))) / &
          AScat%Optical_Depth(k)
        AScat_TL%Delta_Truncation(k) = AScat_TL%Phase_Coefficient(l,1,k)
      END IF
    END DO Layer_loop

  END FUNCTION CRTM_Compute_AerosolScatter_TL

  
!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AerosolScatter_AD
!
! PURPOSE:
!       Function to compute the adjoint aerosol absorption and scattering
!       properties for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_AerosolScatter_AD(  Atmosphere             , &  ! Input
!                                                       AerosolScatter         , &  ! Input
!                                                       AerosolScatter_AD      , &  ! Input
!                                                       SensorIndex            , &  ! Input
!                                                       ChannelIndex           , &  ! Input
!                                                       Atmosphere_AD          , &  ! Output
!                                                       ASV                    , &  ! Internal Variable Output  
!                                                       Message_Log=Message_Log  )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:         CRTM_Atmosphere structure containing the atmospheric
!                           profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter:     CRTM_AtmScatter structure containing the forward model
!                           aerosol absorption and scattering properties required
!                           for radiative transfer.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AerosolScatter_AD:  CRTM_AtmScatter structure containing the adjoint
!                           aerosol absorption and scattering properties.
!                           **NOTE: On EXIT from this function, the contents of
!                                   this structure may be modified (e.g. set to
!                                   zero.)
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmScatter_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       SensorIndex:        Sensor index id. This ia a unique index associated
!                           with a sensor. 
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ASVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AerosolScatter module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_ASVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:      CRTM Atmosphere structure containing the adjoint
!                           atmospheric state data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Atmosphere_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
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
! COMMENTS:
!       Note the INTENT on all of the adjoint arguments (whether input or output)
!       is IN OUT rather than just OUT. This is necessary because the INPUT
!       adjoint arguments are modified, and the OUTPUT adjoint arguments must
!       be defined prior to entry to this routine. So, anytime a structure is
!       to be output, to prevent memory leaks the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_AerosolScatter_AD( Atm         , &  ! FWD Input
                                           AScat       , &  ! FWD Input
                                           AScat_AD    , &  ! AD  Input
                                           SensorIndex , &  ! Input
                                           ChannelIndex, &  ! Input
                                           Atm_AD      , &  ! AD  Output
                                           ASV         , &  ! Internal Variable input
                                           Message_Log ) &  ! Error messaging
                                         RESULT ( Error_Status )               
    ! Arguments
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)     :: Atm
    TYPE(CRTM_AtmScatter_type),  INTENT(IN)     :: AScat
    TYPE(CRTM_AtmScatter_type),  INTENT(IN OUT) :: AScat_AD
    INTEGER,                     INTENT(IN)     :: SensorIndex
    INTEGER,                     INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Atmosphere_type),  INTENT(IN OUT) :: Atm_AD
    TYPE(CRTM_ASVariables_type), INTENT(IN)     :: ASV
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AerosolScatter_AD'
    ! Local variables
    INTEGER   :: k, ka, l, m, n
    INTEGER   :: n_Legendre_Terms, n_Phase_Elements
    INTEGER   :: Sensor_Type
    REAL(fp)  :: Wavelength
    LOGICAL   :: Layer_Mask(Atm%n_Layers)
    INTEGER   :: Layer_Index(Atm%n_Layers)
    INTEGER   :: nAerosol_Layers
    REAL(fp)  :: ke_AD, w_AD, g_AD
    REAL(fp)  :: pcoeff_AD(0:AScat%n_Legendre_Terms, AScat%n_Phase_Elements)
    REAL(fp)  :: bs, bs_AD
    REAL(fp)  :: Total_bs_AD(Atm%n_Layers)
    
    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Sensor type
    Sensor_Type = SC(SensorIndex)%Sensor_Type
    IF (Sensor_Type == MICROWAVE_SENSOR) RETURN
    ! Return if no Aerosols
    IF (Atm%n_Aerosols == 0) RETURN
    ! Initialize local adjoint variables
    Total_bs_AD = ZERO
    ! Wavelength in Microns
    Wavelength = 10000.0_fp/SC(SensorIndex)%Wavenumber(ChannelIndex)
    ! Phase matrix dimensions
    n_Legendre_Terms = AScat_AD%n_Legendre_Terms
    n_Phase_Elements = AScat_AD%n_Phase_Elements
    AScat_AD%lOffset = AScat%lOffset

    
    ! ----------------------------------------------------------
    ! Adjoint of accumulated optical properties for all aerosols
    ! ----------------------------------------------------------
    ! Shorten variable name 
    l = n_Legendre_Terms
    
    ! Begin full atmosphere layer loop
    Layer_loop: DO k = 1, Atm%n_Layers
    
      ! Only process layers that scatter
      IF (ASV%Total_bs(k) < BS_THRESHOLD) CYCLE Layer_loop
      
      IF (n_Phase_Elements > 0) THEN
      
        ! Adjoint of the normalization requirements
        AScat_AD%Phase_Coefficient(0,1,k) = ZERO
        
        AScat_AD%Optical_Depth(k) = AScat_AD%Optical_Depth(k) - &
          (AScat_AD%Single_Scatter_Albedo(k)*AScat%Single_Scatter_Albedo(k) / &
           AScat%Optical_Depth(k))
        Total_bs_AD(k) = Total_bs_AD(k) + (AScat_AD%Single_Scatter_Albedo(k)/AScat%Optical_Depth(k))
        AScat_AD%Single_Scatter_Albedo(k) = ZERO
        
        AScat_AD%Phase_Coefficient(l,1,k) = AScat_AD%Phase_Coefficient(l,1,k) + &
                                            AScat_AD%Delta_Truncation(k)
        AScat_AD%Delta_Truncation(k) = ZERO
        
        ! Adjoint of phase matrix coefficients
        IF (l > 2) THEN
          ! Adjoint of the phase matrix coefficient normalization
          ! with the total volume scattering coefficient, bs.
          DO m = 1, n_Phase_Elements
            Total_bs_AD(k) = Total_bs_AD(k) - &
                             (SUM(AScat_AD%Phase_Coefficient(0:l,m,k) * &
                                  AScat%Phase_Coefficient(0:l,m,k)) / &
                              ASV%Total_bs(k))
            AScat_AD%Phase_Coefficient(0:l,m,k) = AScat_AD%Phase_Coefficient(0:l,m,k) / &
                                                  ASV%Total_bs(k)
          END DO
        ELSE
          ! Henyey-Greenstein phase function
          AScat_AD%Asymmetry_Factor(k) = AScat_AD%Asymmetry_Factor(k) + &
                                         (ONEpointFIVE * AScat_AD%Phase_Coefficient(1,1,k))
          AScat_AD%Phase_Coefficient(1,1,k) = ZERO
          AScat_AD%Phase_Coefficient(2,1,k) = ZERO
        END IF
        
      END IF
      
      ! Adjoint of the asymmetry factor normalization with
      ! the total volume scattering coefficient, bs.
      Total_bs_AD(k) = Total_bs_AD(k) - (AScat_AD%Asymmetry_Factor(k)*AScat%Asymmetry_Factor(k) / &
                                         ASV%Total_bs(k))
      AScat_AD%Asymmetry_Factor(k) = AScat_AD%Asymmetry_Factor(k)/ASV%Total_bs(k)
    END DO Layer_loop
      
      
    ! ------------------------------------------------
    ! Loop over different types of aerosols in profile
    ! ------------------------------------------------
    Aerosol_loop: DO n = 1, Atm%n_Aerosols
    
      ! Only process aerosols with more than
      ! the threshold aerosol concentration
      Layer_Mask = Atm%Aerosol(n)%Concentration > AEROSOL_CONTENT_THRESHOLD
      nAerosol_Layers = COUNT(Layer_Mask)
      IF ( nAerosol_Layers == 0 ) CYCLE Aerosol_loop
      
      ! --------------------------------------
      ! Loop over the current aerosol's layers
      ! --------------------------------------
      Layer_Index(1:nAerosol_Layers) = PACK((/(k,k=1,Atm%Aerosol(n)%n_Layers)/), Layer_Mask)
      Aerosol_Layer_loop: DO k = 1, nAerosol_Layers
        ka = Layer_Index(k)
        
        ! Initialize the individual
        ! Aerosol adjoint variables
        bs_AD = ZERO
        pcoeff_AD = ZERO
        g_AD      = ZERO
        ke_AD     = ZERO
        w_AD      = ZERO
        
        ! Recompute the forward model volume scattering
        ! coefficient for the current aerosol type ONLY
        bs = Atm%Aerosol(n)%Concentration(ka) * ASV%w(ka,n) * ASV%ke(ka,n)
        
        ! Compute the adjoint of the
        ! phase matrix coefficients
        IF( n_Phase_Elements > 0 ) THEN
          DO m = 1, n_Phase_Elements
            DO l = 0, n_Legendre_Terms
              bs_AD = bs_AD + (ASV%pcoeff(l,m,ka,n) * AScat_AD%Phase_Coefficient(l,m,ka))
              pcoeff_AD(l,m) = pcoeff_AD(l,m) + (bs * AScat_AD%Phase_Coefficient(l,m,ka))
            END DO
          END DO
        END IF
            
        ! Compute the adjoint of
        ! the asymmetry factor
        bs_AD = bs_AD + (ASV%g(ka,n) * AScat_AD%Asymmetry_Factor(ka))
        g_AD  = g_AD  + (bs          * AScat_AD%Asymmetry_Factor(ka))
        
        ! Compute the adjoint of the optical 
        ! depth (absorption + scattering)
        Atm_AD%Aerosol(n)%Concentration(ka) = Atm_AD%Aerosol(n)%Concentration(ka) + &
                                              (ASV%ke(ka,n) * AScat_AD%Optical_Depth(ka))
        ke_AD = ke_AD + (Atm%Aerosol(n)%Concentration(ka) * AScat_AD%Optical_Depth(ka))
        
        ! Compute the adjoint of the volume
        ! scattering coefficient.
        ! NOTE: bs_AD is not reinitialized after this
        !       point since it is reinitialized at the
        !       start of the Aerosol_Layer_loop
        bs_AD = bs_AD + Total_bs_AD(ka)
        
        ke_AD = ke_AD + (Atm%Aerosol(n)%Concentration(ka) * ASV%w(ka,n)  * bs_AD )
        w_AD  = w_AD  + (Atm%Aerosol(n)%Concentration(ka) * ASV%ke(ka,n) * bs_AD )
        Atm_AD%Aerosol(n)%Concentration(ka) = Atm_AD%Aerosol(n)%Concentration(ka) + &
                                              (ASV%w(ka,n) * ASV%ke(ka,n) * bs_AD)
                                                     
        ! Adjoint AScat interpolation routine
        CALL Get_Aerosol_Opt_AD(AScat_AD                              , & ! Input
                                Wavelength                            , & ! Input
                                Atm%Aerosol(n)%Type                   , & ! Input
                                Atm%Aerosol(n)%Effective_Radius(ka)   , & ! FWD Input
                                ke_AD                                 , & ! AD Input
                                w_AD                                  , & ! AD Input
                                g_AD                                  , & ! AD Input
                                pcoeff_AD                             , & ! AD Input
                                Atm_AD%Aerosol(n)%Effective_Radius(ka)  ) ! AD Input
                                
      END DO Aerosol_Layer_loop
    END DO Aerosol_loop           
        
  END FUNCTION CRTM_Compute_AerosolScatter_AD

  
!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################
  
  ! --------------------------------------------
  ! Determine the aerosol type index, k, for the
  ! aerosols based on AeroC LUT organisation
  ! --------------------------------------------
  FUNCTION Aerosol_Type_Index( Aerosol_Type ) RESULT( k )
    INTEGER, INTENT(IN) :: Aerosol_Type
    INTEGER :: k
    SELECT CASE (Aerosol_Type)
      CASE(DUST_AEROSOL)              ; k=1
      CASE(SEASALT_SSAM_AEROSOL)      ; k=2
      CASE(SEASALT_SSCM_AEROSOL)      ; k=3
      CASE(DRY_ORGANIC_CARBON_AEROSOL); k=4
      CASE(WET_ORGANIC_CARBON_AEROSOL); k=5
      CASE(DRY_BLACK_CARBON_AEROSOL)  ; k=6
      CASE(WET_BLACK_CARBON_AEROSOL)  ; k=7
      CASE(SULFATE_AEROSOL)           ; k=8
    END SELECT
  END FUNCTION Aerosol_Type_Index

  
  ! ----------------------------------------
  ! Subroutine to obtain the bulk 
  ! optical properties of aerosol
  ! extinction coefficient (ke),
  ! scattering coefficient (w)
  ! asymmetry factor (g), and
  ! spherical Legendre coefficients (pcoeff)
  ! ----------------------------------------
  SUBROUTINE Get_Aerosol_Opt( AerosolScatter, &  ! Input AerosolScatter structure
                              Wavelength    , &  ! Input in microns
                              Aerosol_Type  , &  ! Input see CRTM_Aerosol_Define.f90
                              Reff          , &  ! Input effective radius (mm)
                              ke            , &  ! Output extinction coefficient (=~ optical depth)
                              w             , &  ! Output single scattering albedo
                              g             , &  ! Output asymmetry factor
                              pcoeff          )  ! Output spherical Legendre coefficients
    ! Arguments
    TYPE(CRTM_AtmScatter_type), INTENT(IN)     :: AerosolScatter
    REAL(fp)                  , INTENT(IN)     :: Wavelength
    INTEGER                   , INTENT(IN)     :: Aerosol_Type
    REAL(fp)                  , INTENT(IN)     :: Reff
    REAL(fp)                  , INTENT(OUT)    :: ke
    REAL(fp)                  , INTENT(OUT)    :: w
    REAL(fp)                  , INTENT(OUT)    :: g
    REAL(fp)                  , INTENT(IN OUT) :: pcoeff(0:,:)
    ! Local variables
    INTEGER  :: k, l, m
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    REAL(fp) :: f_int, r_int
    REAL(fp), DIMENSION(NPTS) :: f, r
    TYPE(LPoly_type) :: wlp, xlp

    ! Get the aerosol type LUT index
    ! ------------------------------
    k = Aerosol_Type_Index( Aerosol_Type )

    ! Find the wavelength and effective
    ! radius indices for interpolation
    ! --------------------------------
    f_int = MAX(MIN(AeroC%Wavelength(AeroC%n_Wavelengths),Wavelength),AeroC%Wavelength(1))
    CALL find_index(AeroC%Wavelength(:),f_int,i1,i2)
    f = AeroC%Wavelength(i1:i2)

    r_int = MAX(MIN(AeroC%Reff(AeroC%n_Radii,k),Reff),AeroC%Reff(1,k)) 
    CALL find_index(AeroC%Reff(:,k), r_int, j1, j2)
    r = AeroC%Reff(j1:j2,k) 

    ! Calculate the interpolating polynomials
    ! ---------------------------------------
    ! Frequency term
    CALL LPoly( f, f_int, &  ! Input
                wlp       )  ! Output
    ! Effective radius term
    CALL LPoly( r, r_int, &  ! Input
                xlp       )  ! Output
 
    ! Perform Interpolation
    ! ---------------------
    CALL interp_2D( AeroC%ke(i1:i2,j1:j2,k), wlp, xlp, ke )
    CALL interp_2D( AeroC%w(i1:i2,j1:j2,k) , wlp, xlp, w  )
    CALL interp_2D( AeroC%g(i1:i2,j1:j2,k) , wlp, xlp, g  )
    IF (AerosolScatter%n_Phase_Elements > 0 .AND.  &
        AerosolScatter%n_Legendre_Terms > 2        ) THEN
      DO m = 1, AerosolScatter%n_Phase_Elements
        DO l = 0, AerosolScatter%n_Legendre_Terms
          CALL interp_2D( AeroC%pcoeff(i1:i2,j1:j2,k,l+AerosolScatter%lOffset,m), &
                          wlp, xlp, pcoeff(l,m) )
        END DO
      END DO
    END IF

  END SUBROUTINE Get_Aerosol_Opt


  ! ---------------------------------------------
  ! Subroutine to obtain the tangent-linear
  !  bulk optical properties of a aerosol:
  !   extinction coefficient (ke_TL),
  !   scattereing coefficient (w_TL)
  !   asymmetry factor (g_TL), and
  !   spherical Legendre coefficients (pcoeff_TL)
  ! ---------------------------------------------
  SUBROUTINE Get_Aerosol_Opt_TL(AerosolScatter_TL, &  ! Input  AerosolScatterTL structure
                                Wavelength       , &  ! Input  in microns
                                Aerosol_Type     , &  ! Input  see CRTM_Aerosol_Define.f90
                                Reff             , &  ! Input  FWD effective radius
                                Reff_TL          , &  ! Input  TL effective radius (mm)
                                ke_TL            , &  ! Output TL extinction coefficient (=~ optical depth)
                                w_TL             , &  ! Output TL single scattering albedo
                                g_TL             , &  ! Output TL asymmetry factor
                                pcoeff_TL          )  ! Output TL spherical Legendre coefficients

    ! Arguments
    TYPE(CRTM_AtmScatter_type), INTENT(IN)     :: AerosolScatter_TL
    REAL(fp),                   INTENT(IN)     :: Wavelength
    INTEGER ,                   INTENT(IN)     :: Aerosol_Type
    REAL(fp),                   INTENT(IN)     :: Reff
    REAL(fp),                   INTENT(IN)     :: Reff_TL
    REAL(fp),                   INTENT(OUT)    :: ke_TL
    REAL(fp),                   INTENT(OUT)    :: w_TL
    REAL(fp),                   INTENT(OUT)    :: g_TL
    REAL(fp),                   INTENT(IN OUT) :: pcoeff_TL(0:,:)
    ! Local variables
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k, l, m
    REAL(fp) :: f_int   , r_int
    REAL(fp) :: f_int_TL, r_int_TL
    REAL(fp), DIMENSION(NPTS) :: f   , r
    REAL(fp), DIMENSION(NPTS) :: f_TL, r_TL
    REAL(fp), DIMENSION(NPTS,NPTS) :: z, z_TL
    TYPE(LPoly_type) :: wlp, xlp
    TYPE(LPoly_type) :: wlp_TL, xlp_TL
        
    ! Setup
    ! -----
    ! Get the aerosol type LUT index
    k = Aerosol_Type_Index( Aerosol_Type )
    ! No TL output when effective radius
    ! is outside LUT bounds
    IF ( Reff < AeroC%Reff(1,k) .OR. &
         Reff > AeroC%Reff(AeroC%n_Radii,k)) THEN
      ke_TL     = ZERO
      w_TL      = ZERO
      g_TL      = ZERO
      pcoeff_TL = ZERO
      RETURN
    END IF
    ! The TL inputs
    f_int_TL = ZERO
    f_TL     = ZERO
    r_int_TL = Reff_TL
    r_TL     = ZERO
    z_TL = ZERO

    ! Find the wavelength and effective
    ! radius indices for interpolation
    ! ---------------------------------
    f_int = MAX(MIN(AeroC%Wavelength(AeroC%n_Wavelengths),Wavelength),AeroC%Wavelength(1))
    CALL find_index(AeroC%Wavelength, f_int, i1,i2)
    f = AeroC%Wavelength(i1:i2)
    
    r_int = MAX(MIN(AeroC%Reff(AeroC%n_Radii,k),Reff),AeroC%Reff(1,k))
    CALL find_index(AeroC%Reff(:,k), r_int, j1,j2)
    r = AeroC%Reff(j1:j2,k)

    ! Calculate the interpolating polynomials
    ! ---------------------------------------
    ! Frequency term
    CALL LPoly( f, f_int, &  ! Input
                wlp       )  ! Output
    ! Effective radius term
    CALL LPoly( r, r_int, &  ! Input
                xlp       )  ! Output
    
    ! Calculate the TL interpolating polynomials 
    ! ------------------------------------------
    ! Frequency term (always zero. This is a placeholder for testing)
    CALL LPoly_TL( f,    f_int,    & ! FWD Input
                   wlp,            & ! FWD Input
                   f_TL, f_int_TL, & ! TL  Input
                   wlp_TL          ) ! TL  Output
    ! Effective radius term
    CALL LPoly_TL( r, r_int,       & ! FWD Input
                   xlp,            & ! FWD Input
                   r_TL, r_int_TL, & ! TL  Input
                   xlp_TL          ) ! TL  Output
    
    
    ! Perform Interpolation
    ! ---------------------
    ! Extinction coefficient
    z = AeroC%ke(i1:i2,j1:j2,k)
    CALL interp_2D_TL( z   , wlp   , xlp   , &  ! FWD Input
                       z_TL, wlp_TL, xlp_TL, &  ! TL  Input
                       ke_TL                 )  ! TL  Output
    ! Single scatter albedo
    z = AeroC%w(i1:i2,j1:j2,k)
    CALL interp_2D_TL( z   , wlp   , xlp   , &  ! FWD Input
                       z_TL, wlp_TL, xlp_TL, &  ! TL  Input
                       w_TL                  )  ! TL  Output
    ! Asymmetry factor
    z = AeroC%g(i1:i2,j1:j2,k)
    CALL interp_2D_TL( z   , wlp   , xlp   , &  ! FWD Input
                       z_TL, wlp_TL, xlp_TL, &  ! TL  Input
                       g_TL                  )  ! TL  Output
    ! Phase matrix coefficients    
    IF (AerosolScatter_TL%n_Phase_Elements > 0 .AND. &
        AerosolScatter_TL%n_Legendre_Terms > 2       ) THEN
      DO m = 1, AerosolScatter_TL%n_Phase_Elements
        DO l = 0, AerosolScatter_TL%n_Legendre_Terms
          z = AeroC%pcoeff(i1:i2,j1:j2,k,l+AerosolScatter_TL%lOffset,m)
          CALL interp_2D_TL( z   , wlp   , xlp   , &  ! FWD Input
                             z_TL, wlp_TL, xlp_TL, &  ! TL  Input
                             pcoeff_TL(l,m)        )  ! TL  Output
        END DO
      END DO
    END IF
  
  END SUBROUTINE Get_Aerosol_Opt_TL


  ! ---------------------------------------------
  ! Subroutine to obtain the adjoint
  !  bulk optical properties of a aerosol:
  !   extinction coefficient (ke_AD),
  !   scattereing coefficient (w_AD)
  !   asymmetry factor (g_AD), and
  !   spherical Legendre coefficients (pcoeff_AD)
  ! ---------------------------------------------
  SUBROUTINE Get_Aerosol_Opt_AD( AerosolScatter_AD, & ! Input AerosolScatter AD structure
                                 Wavelength       , & ! Input Wavelength in microns
                                 Aerosol_Type     , & ! Input see CRTM_Aerosol_Define.f90
                                 Reff             , & ! FWD Input effective radius
                                 ke_AD            , & ! AD Input extinction cross section
                                 w_AD             , & ! AD Input single scatter albedo
                                 g_AD             , & ! AD Input asymmetry factor
                                 pcoeff_AD        , & ! AD Input spherical Legendre coefficients
                                 Reff_AD            ) ! AD Output effective radius
    ! Arguments
    TYPE(CRTM_AtmScatter_type), INTENT(IN)     :: AerosolScatter_AD
    REAL(fp),                   INTENT(IN)     :: Wavelength
    INTEGER ,                   INTENT(IN)     :: Aerosol_Type
    REAL(fp),                   INTENT(IN)     :: Reff  
    REAL(fp),                   INTENT(IN OUT) :: ke_AD            ! AD Input
    REAL(fp),                   INTENT(IN OUT) :: w_AD             ! AD Input
    REAL(fp),                   INTENT(IN OUT) :: g_AD             ! AD Input
    REAL(fp),                   INTENT(IN OUT) :: pcoeff_AD(0:,:)  ! AD Input
    REAL(fp),                   INTENT(IN OUT) :: Reff_AD          ! AD Output
    ! Local variables
    INTEGER  :: i1, i2
    INTEGER  :: j1, j2
    INTEGER  :: k, l, m
    REAL(fp) :: f_int   , r_int
    REAL(fp) :: f_int_AD, r_int_AD
    REAL(fp), DIMENSION(NPTS) :: f   , r
    REAL(fp), DIMENSION(NPTS) :: f_AD, r_AD
    REAL(fp), DIMENSION(NPTS,NPTS) :: z, z_AD
    TYPE(LPoly_type) :: wlp   , xlp
    TYPE(LPoly_type) :: wlp_AD, xlp_AD

    ! Setup
    ! -----
    ! Get the aerosol type LUT index
    k = Aerosol_Type_Index( Aerosol_Type )
    ! No AD output when effective radius
    ! is outside LUT bounds
    IF ( Reff < AeroC%Reff(1,k) .OR. &
         Reff > AeroC%Reff(AeroC%n_Radii,k)) THEN
      Reff_AD = ZERO
      ke_AD     = ZERO
      w_AD      = ZERO
      g_AD      = ZERO
      pcoeff_AD = ZERO
      RETURN
    END IF
    ! Initialise local adjoint variables
    f_int_AD = ZERO
    r_int_AD = ZERO
    f_AD = ZERO
    r_AD = ZERO
    z_AD = ZERO
    CALL Clear_LPoly(wlp_AD)
    CALL Clear_LPoly(xlp_AD)

    ! Find the wavelength and effective
    ! radius indices for interpolation
    ! ---------------------------------
    f_int = MAX(MIN(AeroC%Wavelength(AeroC%n_Wavelengths),Wavelength),AeroC%Wavelength(1))
    CALL find_index(AeroC%Wavelength, f_int, i1,i2)
    f = AeroC%Wavelength(i1:i2)

    r_int = MAX(MIN(AeroC%Reff(AeroC%n_Radii,k),Reff),AeroC%Reff(1,k))
    CALL find_index(AeroC%Reff(:,k), r_int, j1,j2)
    r = AeroC%Reff(j1:j2,k)

    ! Calculate the interpolating polynomials
    ! ---------------------------------------
    ! Frequency term
    CALL LPoly( f, f_int, &  ! Input
                wlp       )  ! Output
    ! Effective radius term
    CALL LPoly( r, r_int, &  ! Input
                xlp       )  ! Output

    ! Perform interpolation
    ! ---------------------
    ! Phase matrix coefficients
    IF (AerosolScatter_AD%n_Phase_Elements > 0 .AND. &
        AerosolScatter_AD%n_Legendre_Terms > 2       ) THEN
      DO m = 1, AerosolScatter_AD%n_Phase_Elements
        DO l = 0, AerosolScatter_AD%n_Legendre_Terms
          z = AeroC%pcoeff(i1:i2,j1:j2,k,l+AerosolScatter_AD%lOffset,m)
          CALL interp_2D_AD( z   , wlp   , xlp   , &  ! FWD Input
                             pcoeff_AD(l,m)      , &  ! AD  Input
                             z_AD, wlp_AD, xlp_AD  )  ! AD  Output
        END DO
      END DO
    END IF
    ! Asymmetry factor
    z = AeroC%g(i1:i2,j1:j2,k)
    CALL interp_2D_AD( z   , wlp   , xlp   , &  ! FWD Input
                       g_AD                , &  ! AD  Input
                       z_AD, wlp_AD, xlp_AD  )  ! AD  Output
    ! Single scatter albedo
    z = AeroC%w(i1:i2,j1:j2,k)
    CALL interp_2D_AD( z   , wlp   , xlp   , &  ! FWD Input
                       w_AD                , &  ! AD  Input
                       z_AD, wlp_AD, xlp_AD  )  ! AD  Output
    ! Extinction coefficient
    z = AeroC%ke(i1:i2,j1:j2,k)
    CALL interp_2D_AD( z   , wlp   , xlp   , &  ! FWD Input
                       ke_AD               , &  ! AD  Input
                       z_AD, wlp_AD, xlp_AD  )  ! AD  Output
                       
    ! Compute the AD of the interpolating polynomials
    ! -----------------------------------------------
    ! Efective radius term
    CALL LPoly_AD( r,    r_int,   & ! FWD Input
                   xlp,           & ! FWD Input
                   xlp_AD,        & ! AD  Input
                   r_AD, r_int_AD ) ! AD  Output
    ! Frequency term (always zero. This is a placeholder for testing)
    CALL LPoly_AD( f,    f_int,   & ! FWD Input
                   wlp,           & ! FWD Input
                   wlp_AD,        & ! AD  Input
                   f_AD, f_int_AD ) ! AD  Output
    
    ! The AD outputs
    ! --------------
    Reff_AD = Reff_AD + r_int_AD
    
  END SUBROUTINE Get_Aerosol_Opt_AD
 
END MODULE CRTM_AerosolScatter
