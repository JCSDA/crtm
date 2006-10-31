!
! CRTM_CloudScatter
!
! Module to compute the cloud particle absorption and scattering properties
! required for radiative transfer in a cloudy atmosphere.
!
! CREATION HISTORY  
!        Written by:     Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov 
!                        Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                        Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!                        02-July-2005
!

MODULE CRTM_CloudScatter


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM modules
  USE CRTM_Parameters
  USE CRTM_SpcCoeff
  USE CRTM_CloudCoeff,          ONLY: CloudC
  USE CRTM_Atmosphere_Define,   ONLY: CRTM_Atmosphere_type, &
                                      MAX_N_CLOUDS => N_VALID_CLOUD_TYPES, &
                                      WATER_CLOUD, &
                                      ICE_CLOUD, &
                                      RAIN_CLOUD, &
                                      SNOW_CLOUD, &
                                      GRAUPEL_CLOUD, &
                                      HAIL_CLOUD
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type

  ! -- The AtmScatter structure definition module
  ! -- The PUBLIC entities in CRTM_AtmScatter_Define
  ! -- are also explicitly defined as PUBLIC here
  ! -- (down below) so a user need only USE this
  ! -- module (CRTM_CloudScatter).
  USE CRTM_AtmScatter_Define


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
  PUBLIC :: CRTM_Compute_CloudScatter
  PUBLIC :: CRTM_Compute_CloudScatter_TL
  PUBLIC :: CRTM_Compute_CloudScatter_AD


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_CloudScatter.f90,v 1.6 2006/06/23 23:20:10 wd20pd Exp $'

  ! -- Number of stream angle definitions
  INTEGER, PARAMETER :: TWO_STREAMS       =  2
  INTEGER, PARAMETER :: FOUR_STREAMS      =  4
  INTEGER, PARAMETER :: SIX_STREAMS       =  6
  INTEGER, PARAMETER :: EIGHT_STREAMS     =  8
  INTEGER, PARAMETER :: SIXTEEN_STREAMS   = 16
  INTEGER, PARAMETER :: THIRTYTWO_STREAMS = 32


  ! ----------------
  ! Module variables (eventually remove)
  ! ----------------
  INTEGER :: Offset_LegTerm


  ! --------------------------------------
  ! Strucutre definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------

  TYPE, PUBLIC :: CRTM_CSVariables_type
    PRIVATE
    REAL( fp_kind ), DIMENSION(MAX_N_LAYERS, MAX_N_CLOUDS) :: ext  !be  ! Extinction coefficients
    REAL( fp_kind ), DIMENSION(MAX_N_LAYERS, MAX_N_CLOUDS) :: w0   !w   ! Single scatter albedos
    REAL( fp_kind ), DIMENSION(MAX_N_LAYERS, MAX_N_CLOUDS) :: g   ! Asymmetry factors
    REAL( fp_kind ), DIMENSION(0:MAX_N_LEGENDRE_TERMS,&
                               MAX_N_PHASE_ELEMENTS,  &
                               MAX_N_LAYERS,          &
                               MAX_N_CLOUDS           ) :: p_coef  !p      ! Phase coefficients
  END TYPE CRTM_CSVariables_type


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
!       CRTM_Compute_CloudScatter
!
! PURPOSE:
!       Function to compute the cloud particle absorption and scattering
!       properties and populate the output CloudScatter structure for a
!       single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_CloudScatter( Atmosphere,               &  ! Input
!                                                 GeometryInfo,             &  ! Input
!                                                 Channel_Index,            &  ! Input, scalar
!                                                 CloudScatter,             &  ! Output
!                                                 CSVariables,              &  ! Internal variable output
!                                                 Message_Log = Message_Log )  ! Error messaging 
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
!        CloudScatter:   CRTM_AtmScatter structure containing the cloud particle
!                        absorption and scattering properties required for
!                        radiative transfer.
!                        UNITS:      N/A
!                        TYPE:       TYPE( CRTM_AtmScatter_type )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!        CSVariables:    Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_CloudScatter module.
!                        UNITS:      N/A
!                        TYPE:       CRTM_CSVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
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
!       Note the INTENT on the output CloudScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_CloudScatter( Atmosphere,    &  ! Input
                                      Channel_Index, &  ! Input
                                      CloudScatter,  &  ! Output
                                      CSV,           &  ! Internal variable output
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    INTEGER,                        INTENT( IN )     :: Channel_Index
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: CloudScatter
    TYPE( CRTM_CSVariables_type ),  INTENT( OUT )    :: CSV
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local variables
    INTEGER :: i, j, k, n, L, kuse
    INTEGER :: Sensor_Type
    REAL( fp_kind ) :: Frequency, Wavenumber
    INTEGER, DIMENSION( Atmosphere%Max_Layers ) :: kidx
    REAL( fp_kind ) :: Water_Content,eff_radius,eff_v,Temperature
    INTEGER :: Cloud_Type, n_Legendre_Terms, n_Phase_Elements
    REAL( fp_kind ) :: Scattering_Coefficient
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_CloudScatter'


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! -------------------- !
    ! channel based        !
    ! -------------------- !
      Sensor_Type = SC%Sensor_Type(Channel_Index)
      Frequency   = SC%Frequency(Channel_Index)
      Wavenumber  = SC%Wavenumber(Channel_Index)

    ! -------------------- !
    ! OSS node based       !
    ! -------------------- !
    !  Sensor_Type = SC%Sensor_Type(SC%Channel_Node_Map(1,Channel_Index))
    !  Frequency   = SC%Node_Frequency(Channel_Index)
    !  Wavenumber  = SC%Node_Wavenumber(Channel_Index)
 
      n_Legendre_Terms = CloudScatter%n_Legendre_Terms
      n_Phase_Elements = CloudScatter%n_Phase_Elements

    ! ----------------------------------------------------
    ! Determining Offset place for Legendre coefficients
    ! corresponding to n_Streams.
    ! ----------------------------------------------------

     SELECT CASE( CloudScatter%n_Legendre_Terms )

       CASE ( Two_Streams )       ! 0 : Two_Streams, Asymmetry factor will be used.
        Offset_LegTerm = 0 

       CASE ( Four_Streams )       ! 0 : Four_Streams
        Offset_LegTerm = 0 

       CASE ( Six_Streams )        ! Four_Streams+1 : Six_Streams
        Offset_LegTerm = 5 

       CASE ( Eight_Streams )      ! Four_Streams+1+Six_Streams+1 : Eight_Streams
        Offset_LegTerm = 12

!       CASE ( Sixteen_Streams )    ! Four_Streams+1+Six_Streams+1+Eight_Streams+1 : Sixteen_Streams
!        Offset_LegTerm = 21 

       CASE DEFAULT

    ! ------------------------------------------------------------
    !    Using two-streams model or HG and RAYLEIGH Phase function
    ! ------------------------------------------------------------

      IF( HGphase ) THEN
      CloudScatter%n_Legendre_Terms = 0
      ELSE
      Error_Status = FAILURE
      print *,' the n_Legendre_Terms in cloudscatter do not fit this model ', &
         CloudScatter%n_Legendre_Terms
      ENDIF

      END SELECT
    !#--------------------------------------------------------------------------#
    !#                -- INITIALIZATION  --                                     #
    !#--------------------------------------------------------------------------#

         CloudScatter%Offset_LegTerm = Offset_LegTerm
         CloudScatter%Optical_Depth = ZERO
         CloudScatter%Single_Scatter_Albedo = ZERO
         CloudScatter%Asymmetry_Factor = ZERO

      IF(Atmosphere%n_Clouds == 0) RETURN
         IF( CloudC%n_Phase_Elements > 0 ) CloudScatter%Phase_Coefficient = ZERO
!
    !#--------------------------------------------------------------------------#
    !#                -- LOOP OVER CLOUD TYPE --                                #
    !#--------------------------------------------------------------------------#

  DO n = 1, Atmosphere%n_Clouds
     kuse = count(Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)

     IF(kuse > 0 ) THEN
       kidx(1:kuse) = PACK((/(k,k=1,Atmosphere%cloud(n)%n_layers)/), &
                           Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)
       Cloud_Type = Atmosphere%cloud(n)%Type
                                     
      !  LOOP OVER LAYERS
      DO i = 1, kuse
         j = kidx(i)

         Temperature=Atmosphere%Temperature(j)
         eff_radius=Atmosphere%cloud(n)%Effective_Radius(j)
         eff_v=Atmosphere%cloud(n)%Effective_Variance(j)
         Water_Content=Atmosphere%cloud(n)%Water_Content(j)

        !   MICROWAVE RANGE
        IF( Sensor_Type == MICROWAVE_SENSOR ) THEN
          call Get_Cloud_Opt_MW(n_Legendre_Terms,n_Phase_Elements,   & !INPUT
            Frequency,Cloud_Type,eff_radius,eff_v,Temperature,   & !INPUT
            CSV%ext(j,n),CSV%w0(j,n),CSV%g(j,n),CSV%p_coef(:,:,j,n))   !OUTPUT

        !   INFRARED RANGE
        ELSE IF( Sensor_Type == INFRARED_SENSOR ) THEN
          call Get_Cloud_Opt_IR(n_Legendre_Terms,n_Phase_Elements, & !INPUT
            Wavenumber,Cloud_Type,eff_radius,eff_v, &             !INPUT
            CSV%ext(j,n),CSV%w0(j,n),CSV%g(j,n),CSV%p_coef(:,:,j,n)) !OUTPUT

        !   UV AND VISIBLE RANGE
        ELSE IF( Sensor_Type == VISIBLE_SENSOR ) THEN
        !   under development
            CSV%ext(j,n) = ZERO
            CSV%w0(j,n)  = ZERO
            CSV%g(j,n)   = ZERO
            CSV%p_coef(:,:,j,n) = ZERO
        ELSE
            PRINT *,' WRONG SENSOR TYPE in CRTM_CloudScatter ', &
                      Channel_Index, Sensor_Type
        ENDIF

         Scattering_Coefficient = CSV%ext(j,n)*Water_Content*CSV%w0(j,n)
         CloudScatter%Optical_Depth(j) = CloudScatter%Optical_Depth(j)  &
                                       + CSV%ext(j,n)*Water_Content
         CloudScatter%Single_Scatter_Albedo(j) =  &
           CloudScatter%Single_Scatter_Albedo(j) + Scattering_Coefficient

         ! ---------------------------------------------------------- !
         !  Note: Single_Scatter_Albedo may be used for the           !
         ! intermediarys cattering coefficient, and finally be        !
         !  converted to single scattering albedo.                    !
         ! ---------------------------------------------------------- !
         CloudScatter%Asymmetry_Factor(j) = CloudScatter%Asymmetry_Factor(j) &
                                          + CSV%g(j,n)*Scattering_Coefficient

        IF( n_Phase_Elements > 0 ) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms
           CloudScatter%Phase_Coefficient(L, k, j) =   &
             CloudScatter%Phase_Coefficient(L, k, j)   &
             + CSV%p_coef(L,k,j,n)*Scattering_Coefficient
           ENDDO
          ENDDO
        ENDIF

      ENDDO     ! END of LOOP over layers (i)
                                 
     ENDIF      ! kuse
                                            
  ENDDO       ! END of LOOP over cloud type (n)

  DO i = 1, Atmosphere%n_Layers
    IF(CloudScatter%Single_Scatter_Albedo(i) > SCATTERING_ALBEDO_THRESHOLD) THEN
     CloudScatter%Asymmetry_Factor(i) = CloudScatter%Asymmetry_Factor(i)  &
                                      /CloudScatter%Single_Scatter_Albedo(i)

      IF( n_Phase_Elements > 0 ) THEN
        IF( n_Legendre_Terms > 2 ) THEN
          DO k = 1, n_Phase_Elements
            CloudScatter%Phase_Coefficient(0:n_Legendre_Terms, k, i) =  &
            CloudScatter%Phase_Coefficient(0:n_Legendre_Terms, k, i)/ &
                                    CloudScatter%Single_Scatter_Albedo(i)
          ENDDO
        ELSE
        ! For Henyey_Greenstein phase function
          CloudScatter%Phase_Coefficient(1,1,i)=1.5_fp_kind*CloudScatter%Asymmetry_Factor(i)
          CloudScatter%Phase_Coefficient(2,1,i) = ZERO
        ENDIF

         ! Normalization requirement
         CloudScatter%Phase_Coefficient(0,1,i) = POINT_5
         CloudScatter%Single_Scatter_Albedo(i)=  &
           CloudScatter%Single_Scatter_Albedo(i)/CloudScatter%Optical_Depth(i)
         CloudScatter%Delta_Truncation(i)=CloudScatter%Phase_Coefficient(n_Legendre_Terms,1,i)
      ENDIF
    ENDIF
  ENDDO 
!

  END FUNCTION CRTM_Compute_CloudScatter
!
!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_CloudScatter_TL
!
! PURPOSE:
!       Function to compute the tangent-linear cloud particle absorption and
!       scattering properties and populate the output CloudScatter_TL structure
!       for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_CloudScatter_TL( Atmosphere,               &  ! Input
!                                                    CloudScatter,             &  ! Input
!                                                    Atmosphere_TL,            &  ! Input
!                                                    GeometryInfo,             &  ! Input
!                                                    Channel_Index,            &  ! Input, scalar
!                                                    CloudScatter_TL,          &  ! Output        
!                                                    CSVariables,              &  ! Internal variable input
!                                                    Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:       CRTM_Atmosphere structure containing the atmospheric
!                         profile data.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_Atmosphere_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:     CRTM_AtmScatter structure containing the forward model
!                         cloud particle absorption and scattering properties
!                         required for radiative transfer.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_AtmScatter_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Atmosphere_TL:    CRTM Atmosphere structure containing the tangent-linear
!                         atmospheric state data.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_Atmosphere_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       GeometryInfo:     CRTM_GeometryInfo structure containing the 
!                         view geometry information.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_GeometryInfo_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:    Channel index id. This is a unique index associated
!                         with a (supported) sensor channel used to access the
!                         shared coefficient data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       CSVariables:      Structure containing internal variables required for
!                         subsequent tangent-linear or adjoint model calls.
!                         The contents of this structure are NOT accessible
!                         outside of the CRTM_CloudScatter module.
!                         UNITS:      N/A
!                         TYPE:       CRTM_CSVariables_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!        CloudScatter_TL: CRTM_AtmScatter structure containing the tangent-linear
!                         cloud particle absorption and scattering properties
!                         required for radiative transfer.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_AtmScatter_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
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
!       Note the INTENT on the output CloudScatter_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!S-
!------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_CloudScatter_TL( Atmosphere,      &  ! Input
                                         CloudScatter,    &  ! Input
                                         Atmosphere_TL,   &  ! Input
                                         Channel_Index,   &  ! Input
                                         CloudScatter_TL, &  ! Output
                                         CSV,             &  ! Internal variable input
                                         Message_Log )    &  ! Error messaging
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere,Atmosphere_TL
    INTEGER,                        INTENT( IN )     :: Channel_Index
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: CloudScatter
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: CloudScatter_TL
    TYPE( CRTM_CSVariables_type ),  INTENT( IN )     :: CSV
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_CloudScatter'
    ! Local variables 
    INTEGER :: i, j, k, n, L, kuse
    INTEGER :: Sensor_Type
    REAL( fp_kind ) :: Frequency, Wavenumber
    INTEGER, DIMENSION( Atmosphere%Max_Layers ) :: kidx
    REAL( fp_kind ) :: Water_Content,eff_radius,eff_v,Temperature
    REAL( fp_kind ) :: ext_TL,w0_TL,g_TL,Water_Content_TL,eff_radius_TL,eff_v_TL,Temperature_TL
    INTEGER :: Cloud_Type, n_Legendre_Terms, n_Phase_Elements
    REAL( fp_kind ), DIMENSION(0:CloudScatter%n_Legendre_Terms,CloudScatter%n_Phase_Elements) :: p_coef_TL
    REAL( fp_kind ) :: Scattering_Coefficient,Scattering_Coefficient_TL
    REAL( fp_kind ), DIMENSION( Atmosphere%Max_Layers ) :: T_Scattering,T_Scattering_TL


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! -------------------- !
    ! channel based        !
    ! -------------------- !
      Sensor_Type = SC%Sensor_Type(Channel_Index)
      Frequency   = SC%Frequency(Channel_Index)
      Wavenumber  = SC%Wavenumber(Channel_Index)

    ! -------------------- !
    ! OSS node based       !
    ! -------------------- !
    !  Sensor_Type = SC%Sensor_Type(SC%Channel_Node_Map(1,Channel_Index))
    !  Frequency   = SC%Node_Frequency(Channel_Index)
    !  Wavenumber  = SC%Node_Wavenumber(Channel_Index)

    n_Legendre_Terms = CloudScatter%n_Legendre_Terms
    n_Phase_Elements = CloudScatter%n_Phase_Elements
    Offset_LegTerm = CloudScatter%Offset_LegTerm

    T_Scattering = ZERO
    T_Scattering_TL = ZERO
    CloudScatter_TL%Optical_Depth = ZERO
    CloudScatter_TL%Single_Scatter_Albedo = ZERO
    CloudScatter_TL%Asymmetry_Factor = ZERO

  IF(Atmosphere%n_Clouds == 0) RETURN
!
    CloudScatter_TL%Phase_Coefficient = ZERO
    !#--------------------------------------------------------------------------#
    !#                -- LOOP OVER CLOUD TYPE --                                #
    !#--------------------------------------------------------------------------#
       
    DO n = 1, Atmosphere%n_Clouds
      kuse = count(Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)
      IF(kuse > 0 ) THEN
       kidx(1:kuse) = PACK((/(k,k=1,Atmosphere%cloud(n)%n_layers)/), &
                           Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)
        Cloud_Type = Atmosphere%cloud(n)%Type
                                     
     !  LOOP OVER LAYERS
                                            
       DO i = 1, kuse
         j = kidx(i)
         Temperature=Atmosphere%Temperature(j)
         eff_radius=Atmosphere%cloud(n)%Effective_Radius(j)
         eff_v=Atmosphere%cloud(n)%Effective_Variance(j)
         Water_Content=Atmosphere%cloud(n)%Water_Content(j)

         Temperature_TL=Atmosphere_TL%Temperature(j)
         eff_radius_TL=Atmosphere_TL%cloud(n)%Effective_Radius(j)
         eff_v_TL=Atmosphere_TL%cloud(n)%Effective_Variance(j)
         Water_Content_TL=Atmosphere_TL%cloud(n)%Water_Content(j)

         !  MICROWAVE RANGE
         IF( Sensor_Type == MICROWAVE_SENSOR ) THEN
          call Get_Cloud_Opt_MW_TL(n_Legendre_Terms,n_Phase_Elements, & !INPUT
            Frequency,Cloud_Type,eff_radius,eff_v,Temperature, & !INPUT
            eff_radius_TL,eff_v_TL,Temperature_TL, & !INPUT
            ext_TL,w0_TL,g_TL,p_coef_TL)     !OUTPUT

        !   INFRARED RANGE
        ELSE IF( Sensor_Type == INFRARED_SENSOR ) THEN
          call Get_Cloud_Opt_IR_TL(n_Legendre_Terms,n_Phase_Elements, & !INPUT
            Wavenumber,Cloud_Type,eff_radius,eff_v, &
            eff_radius_TL,eff_v_TL, & !INPUT
            ext_TL,w0_TL,g_TL,p_coef_TL)     !OUTPUT

        !   UV AND VISIBLE RANGE
        ELSE IF( Sensor_Type == VISIBLE_SENSOR ) THEN
            ext_TL = ZERO
            w0_TL  = ZERO
            g_TL   = ZERO
            p_coef_TL = ZERO
        ELSE
            PRINT *,' WRONG SENSOR TYPE in CRTM_CloudScatter ', &
                      Channel_Index, Sensor_Type
        ENDIF

        Scattering_Coefficient = CSV%ext(j,n)*Water_Content*CSV%w0(j,n)
        T_Scattering(j) = T_Scattering(j) + Scattering_Coefficient
        Scattering_Coefficient_TL = ext_TL*Water_Content*CSV%w0(j,n)  &
          +CSV%ext(j,n)*Water_Content_TL*CSV%w0(j,n)+CSV%ext(j,n)*Water_Content*w0_TL

        T_Scattering_TL(j) = T_Scattering_TL(j) + Scattering_Coefficient_TL

        CloudScatter_TL%Optical_Depth(j)=CloudScatter_TL%Optical_Depth(j)  &
          +ext_TL*Water_Content+CSV%ext(j,n)*Water_Content_TL
        CloudScatter_TL%Single_Scatter_Albedo(j)=  &
          CloudScatter_TL%Single_Scatter_Albedo(j)+Scattering_Coefficient_TL 

        CloudScatter_TL%Asymmetry_Factor(j)=CloudScatter_TL%Asymmetry_Factor(j) &
          +g_TL*Scattering_Coefficient+CSV%g(j,n)*Scattering_Coefficient_TL

        IF( n_Phase_Elements > 0 ) THEN
         DO k = 1, n_Phase_Elements
      !  L=0, phase_Coeff is the constant
          DO L = 1, n_Legendre_Terms
          CloudScatter_TL%Phase_Coefficient(L, k, j) =   &
            CloudScatter_TL%Phase_Coefficient(L, k, j)   &
            + p_coef_TL(L,k)*Scattering_Coefficient+CSV%p_coef(L,k,j,n)  &
            * Scattering_Coefficient_TL
          ENDDO
         ENDDO
        ENDIF

       ENDDO     ! END of LOOP over layers (i)
                                 
      ENDIF      ! kuse
                                            
   ENDDO       ! END of LOOP over cloud type (n)

   DO i = 1, Atmosphere%n_Layers
     IF(T_Scattering(i) > SCATTERING_ALBEDO_THRESHOLD) THEN

     CloudScatter_TL%Asymmetry_Factor(i) =   &
       CloudScatter_TL%Asymmetry_Factor(i)/T_Scattering(i) &
       -CloudScatter%Asymmetry_Factor(i)*T_Scattering_TL(i)/T_Scattering(i)

      IF( n_Phase_Elements > 0 ) THEN
        IF( n_Legendre_Terms > 2 ) THEN
         DO k = 1, n_Phase_Elements

      !  L=0, phase_Coeff is the constant
          DO j = 1, n_Legendre_Terms
           CloudScatter_TL%Phase_Coefficient(j, k, i)= &
           CloudScatter_TL%Phase_Coefficient(j, k, i)/T_Scattering(i)  &
           -CloudScatter%Phase_Coefficient(j, k, i)/T_Scattering(i)    &
           *T_Scattering_TL(i)
          ENDDO
         ENDDO
        ELSE

           CloudScatter_TL%Phase_Coefficient(1,1,i)  &
             =1.5_fp_kind*CloudScatter_TL%Asymmetry_Factor(i)
           CloudScatter_TL%Phase_Coefficient(2,1,i) = ZERO
        ENDIF
           CloudScatter_TL%Phase_Coefficient(0,1,i) = ZERO

        CloudScatter_TL%Single_Scatter_Albedo(i) =  &
          CloudScatter_TL%Single_Scatter_Albedo(i)  &
          /CloudScatter%Optical_Depth(i)  &
          -CloudScatter%Single_Scatter_Albedo(i)    &
          /CloudScatter%Optical_Depth(i)*CloudScatter_TL%Optical_Depth(i)
        CloudScatter_TL%Delta_Truncation(i) =   &
          CloudScatter_TL%Phase_Coefficient(n_Legendre_Terms,1,i)
      ENDIF
     ENDIF
   ENDDO 
!
    Error_Status = SUCCESS

  END FUNCTION CRTM_Compute_CloudScatter_TL
!
!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Compute_CloudScatter_AD
!
! PURPOSE:
!       Function to compute the adjoint of the cloud particle absorption and
!       scattering properties for a single channel.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_CloudScatter_AD(  Atmosphere,               &  ! Input   
!                                                     CloudScatter,             &  ! Input   
!                                                     CloudScatter_AD,          &  ! Input   
!                                                     GeometryInfo,             &  ! Input   
!                                                     Channel_Index,            &  ! Input   
!                                                     Atmosphere_AD,            &  ! Output  
!                                                     CSVariables,              &  ! Internal variable input
!                                                     Message_Log = Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Atmosphere:       CRTM_Atmosphere structure containing the atmospheric
!                         profile data.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_Atmosphere_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       CloudScatter:     CRTM_AtmScatter structure containing the forward model
!                         cloud particle absorption and scattering properties
!                         required for radiative transfer.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_AtmScatter_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!        CloudScatter_AD: CRTM_AtmScatter structure containing the adjoint
!                         of the cloud particle absorption and scattering
!                         properties required for radiative transfer.
!                         **NOTE: On EXIT from this function, the contents of
!                                 this structure may be modified (e.g. set to
!                                 zero.)
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_AtmScatter_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       GeometryInfo:     CRTM_GeometryInfo structure containing the 
!                         view geometry information.
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_GeometryInfo_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:    Channel index id. This is a unique index associated
!                         with a (supported) sensor channel used to access the
!                         shared coefficient data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       CSVariables:      Structure containing internal variables required for
!                         subsequent tangent-linear or adjoint model calls.
!                         The contents of this structure are NOT accessible
!                         outside of the CRTM_CloudScatter module.
!                         UNITS:      N/A
!                         TYPE:       CRTM_CSVariables_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere_AD:    CRTM Atmosphere structure containing the adjoint
!                         atmospheric state data.
!                         **NOTE: On ENTRY to this function, the contents of
!                                 this structure should be defined (e.g.
!                                 initialized to some value based on the
!                                 position of this function in the call chain.)
!                         UNITS:      N/A
!                         TYPE:       TYPE( CRTM_Atmosphere_type )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN OUT )
!
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the computation was sucessful
!                            == FAILURE an unrecoverable error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
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

  FUNCTION CRTM_Compute_CloudScatter_AD( Atmosphere,      &  ! Input
                                         CloudScatter,    &  ! Input
                                         CloudScatter_AD, &  ! Input
                                         Channel_Index,   &  ! Input
                                         Atmosphere_AD,   &  ! Output
                                         CSV,             &  ! Internal variable input
                                         Message_Log )    &  ! Error messaging
                                       RESULT ( Error_Status )
    ! Arguments
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN )     :: Atmosphere
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN )     :: CloudScatter
    TYPE( CRTM_AtmScatter_type ),   INTENT( IN OUT ) :: CloudScatter_AD
    INTEGER,                        INTENT( IN )     :: Channel_Index
    TYPE( CRTM_Atmosphere_type ),   INTENT( IN OUT ) :: Atmosphere_AD
    TYPE( CRTM_CSVariables_type ),  INTENT( IN )     :: CSV
    CHARACTER( * ), OPTIONAL,       INTENT( IN )     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_CloudScatter_AD'
    ! Local variables
    INTEGER :: i, j, k, n, L, kuse
    INTEGER :: Sensor_Type
    REAL( fp_kind ) :: Frequency, Wavenumber
    INTEGER, DIMENSION( Atmosphere%Max_Layers ) :: kidx
    REAL( fp_kind ) :: Water_Content,eff_radius,eff_v,Temperature
    REAL( fp_kind ) :: ext_AD,w0_AD,g_AD,Water_Content_AD,eff_radius_AD,eff_v_AD,Temperature_AD
    INTEGER :: Cloud_Type, n_Legendre_Terms, n_Phase_Elements
    REAL( fp_kind ), DIMENSION(0:CloudScatter%n_Legendre_Terms,CloudScatter%n_Phase_Elements) :: p_coef_AD
    REAL( fp_kind ) :: Scattering_Coefficient,Scattering_Coefficient_AD
    REAL( fp_kind ), DIMENSION( Atmosphere%Max_Layers ) :: T_Scattering,T_Scattering_AD


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! -------------------- !
    ! channel based        !
    ! -------------------- !
      Sensor_Type = SC%Sensor_Type(Channel_Index)
      Frequency   = SC%Frequency(Channel_Index)
      Wavenumber  = SC%Wavenumber(Channel_Index)

    ! -------------------- !
    ! OSS node based       !
    ! -------------------- !
    !  Sensor_Type = SC%Sensor_Type(SC%Channel_Node_Map(1,Channel_Index))
    !  Frequency   = SC%Node_Frequency(Channel_Index)
    !  Wavenumber  = SC%Node_Wavenumber(Channel_Index)

!
  IF(Atmosphere%n_Clouds == 0) RETURN
    n_Legendre_Terms = CloudScatter%n_Legendre_Terms
    n_Phase_Elements = CloudScatter%n_Phase_Elements
    Offset_LegTerm = CloudScatter%Offset_LegTerm
    T_Scattering_AD = ZERO

    DO i = 1, Atmosphere%n_Clouds
    Atmosphere_AD%cloud(i)%Water_Content = ZERO
    Atmosphere_AD%cloud(i)%Effective_Radius = ZERO
    ENDDO
!
    DO i = Atmosphere%n_Layers, 1, -1

     T_Scattering(i) = CloudScatter%Optical_Depth(i)*CloudScatter%Single_Scatter_Albedo(i)

     IF(T_Scattering(i) > SCATTERING_ALBEDO_THRESHOLD) THEN

      IF( n_Phase_Elements > 0 ) THEN
       CloudScatter_AD%Phase_Coefficient(n_Legendre_Terms,1,i) =  &
          CloudScatter_AD%Phase_Coefficient(n_Legendre_Terms,1,i) &
          + CloudScatter_AD%Delta_Truncation(i) 

       CloudScatter_AD%Optical_Depth(i) = CloudScatter_AD%Optical_Depth(i) &
          - CloudScatter%Single_Scatter_Albedo(i)/CloudScatter%Optical_Depth(i)  &
          * CloudScatter_AD%Single_Scatter_Albedo(i)

       CloudScatter_AD%Single_Scatter_Albedo(i) =  &
          CloudScatter_AD%Single_Scatter_Albedo(i)/CloudScatter%Optical_Depth(i) 

       CloudScatter_AD%Phase_Coefficient(0,1,i) = ZERO
       IF( n_Legendre_Terms > 2 ) THEN

       DO k = n_Phase_Elements, 1, -1
      !  L=0, phase_Coeff is the constant
           DO j = n_Legendre_Terms, 1, -1
        T_Scattering_AD(i) = T_Scattering_AD(i)  &
                           - CloudScatter%Phase_Coefficient(j,k,i) &
           * CloudScatter_AD%Phase_Coefficient(j,k,i)/T_Scattering(i)
        CloudScatter_AD%Phase_Coefficient(j, k,i)= &
           CloudScatter_AD%Phase_Coefficient(j, k,i)/T_Scattering(i)
           ENDDO
       ENDDO
       ELSE
      ! Henye-Greenstein phase function
       CloudScatter_AD%Phase_Coefficient(2,1,i) = ZERO
       CloudScatter_AD%Asymmetry_Factor(i) =  &
         CloudScatter_AD%Asymmetry_Factor(i)  &
         + 1.5_fp_kind*CloudScatter_AD%Phase_Coefficient(1,1,i)
       ENDIF

     ENDIF
       T_Scattering_AD(i) = T_Scattering_AD(i)  &
          - CloudScatter%Asymmetry_Factor(i)    &
          * CloudScatter_AD%Asymmetry_Factor(i)/T_Scattering(i)
       CloudScatter_AD%Asymmetry_Factor(i) =    &
          CloudScatter_AD%Asymmetry_Factor(i)/T_Scattering(i)

     ENDIF
    ENDDO 
!
!
    !#--------------------------------------------------------------------------#
    !#                -- LOOP OVER CLOUD TYPE --                                #
    !#--------------------------------------------------------------------------#
      
  DO n = Atmosphere%n_Clouds, 1, -1
    kuse = count(Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)
    IF(kuse > 0 ) THEN
       kidx(1:kuse) = PACK((/(k,k=1,Atmosphere%cloud(n)%n_layers)/), &
                           Atmosphere%cloud(n)%Water_Content > WATER_CONTENT_THRESHOLD)
      Cloud_Type = Atmosphere%cloud(n)%Type
                                     
     !  LOOP OVER LAYERS
                                            
     DO i = kuse, 1, -1
        j = kidx(i)
       Water_Content_AD = ZERO
       eff_v_AD = ZERO
       eff_radius_AD = ZERO
       ext_AD = ZERO
       w0_AD = ZERO
       g_AD = ZERO
       Scattering_Coefficient_AD = ZERO
       p_coef_AD = ZERO
       Temperature_AD = ZERO

       Temperature = Atmosphere%Temperature(j)
       eff_radius = Atmosphere%cloud(n)%Effective_Radius(j)
       eff_v = Atmosphere%cloud(n)%Effective_Variance(j)
       Water_Content = Atmosphere%cloud(n)%Water_Content(j)

      ! ---------------------------------------------------------- !
      !  Note: Single_Scatter_Albedo is scattering coefficient,    !
      !  it is converted to single scattering albedo as below.     !
      ! ---------------------------------------------------------- !

       Scattering_Coefficient = CSV%ext(j,n)*Water_Content*CSV%w0(j,n)

     IF( n_Phase_Elements > 0 ) THEN
      DO k = n_Phase_Elements, 1, -1
       DO L = n_Legendre_Terms, 1, -1
       Scattering_Coefficient_AD = Scattering_Coefficient_AD  &
         + CSV%p_coef(L,k,j,n)*CloudScatter_AD%Phase_Coefficient(L,k,j)
       p_coef_AD(L,k) = p_coef_AD(L,k)   &
         + CloudScatter_AD%Phase_Coefficient(L,k,j)*Scattering_Coefficient
       ENDDO
      ENDDO
     ENDIF

      Scattering_Coefficient_AD = Scattering_Coefficient_AD  &
         + CSV%g(j,n)*CloudScatter_AD%Asymmetry_Factor(j)
      g_AD = g_AD + CloudScatter_AD%Asymmetry_Factor(j)*Scattering_Coefficient

     Scattering_Coefficient_AD = Scattering_Coefficient_AD   &
         + CloudScatter_AD%Single_Scatter_Albedo(j)
     Water_Content_AD = Water_Content_AD   &
         + CloudScatter_AD%Optical_Depth(j)*CSV%ext(j,n)
     ext_AD = ext_AD + CloudScatter_AD%Optical_Depth(j)*Water_Content
     Scattering_Coefficient_AD=Scattering_Coefficient_AD+T_Scattering_AD(j)
     w0_AD=w0_AD+CSV%ext(j,n)*Water_Content*Scattering_Coefficient_AD
     Water_Content_AD = Water_Content_AD   &
         + CSV%ext(j,n)*Scattering_Coefficient_AD*CSV%w0(j,n)
     ext_AD=ext_AD+Scattering_Coefficient_AD*Water_Content*CSV%w0(j,n)
 
     !   MICROWAVE RANGE
     IF( Sensor_Type == MICROWAVE_SENSOR ) THEN
        
      call Get_Cloud_Opt_MW_AD(n_Legendre_Terms,n_Phase_Elements, & !INPUT
        Frequency,Cloud_Type,eff_radius,eff_v,Temperature,        & !INPUT
        ext_AD,w0_AD,g_AD,p_coef_AD,                              & !INPUT
        eff_radius_AD,eff_v_AD,Temperature_AD)                      !OUTPUT

     !   INFRARED RANGE
     ELSE IF( Sensor_Type == INFRARED_SENSOR ) THEN
      call Get_Cloud_Opt_IR_AD(n_Legendre_Terms,n_Phase_Elements, & !INPUT
        Wavenumber,Cloud_Type,eff_radius,eff_v,                   & !INPUT
        ext_AD,w0_AD,g_AD,p_coef_AD,                              & !INPUT
        eff_radius_AD,eff_v_AD)                                   !OUTPUT
                             
     !   UV AND VISIBLE RANGE
     ELSE IF( Sensor_Type == VISIBLE_SENSOR ) THEN
        eff_radius_AD = ZERO
        eff_v_AD = ZERO
        Temperature_AD = ZERO
    ENDIF
!
      Atmosphere_AD%cloud(n)%Water_Content(j) =   &
        Atmosphere_AD%cloud(n)%Water_Content(j)+Water_Content_AD
      Atmosphere_AD%cloud(n)%Effective_Variance(j) =   &
        Atmosphere_AD%cloud(n)%Effective_Variance(j)+eff_v_AD
      Atmosphere_AD%cloud(n)%Effective_Radius(j) =    &
        Atmosphere_AD%cloud(n)%Effective_Radius(j)+eff_radius_AD
      Atmosphere_AD%Temperature(j)=Atmosphere_AD%Temperature(j)+Temperature_AD

     ENDDO     ! END of LOOP over layers (i)
                                 
    ENDIF      ! kuse
                                            
  ENDDO       ! END of LOOP over cloud type (n)
                                   
  END FUNCTION CRTM_Compute_CloudScatter_AD
!
!
  SUBROUTINE Get_Cloud_Opt_IR(n_Legendre_Terms, & !INPUT  number of Legendre terms 
                              n_Phase_Elements, & !INPUT  number of phase elements
                                    Wavenumber, & !INPUT  wavenumber in 1/cm 
                                    cloud_type, & !INPUT  see CRTM_Cloud_Define.f90
                                    eff_radius, & !INPUT  effective radius (mm)
                                         eff_v, & !INPUT  effective variance of particles
                                           ext, & !OUTPUT optical depth for 1 mm water content
                                            w0, & !OUTPUT single scattering albedo
                                             g, & !OUTPUT asymmetry factor
                                         p_coef)  !OUTPUT spherical Legendre coefficients
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining extinction (ext), scattereing (w0) coefficients
!      asymmetry factor (g), and spherical Legendre coefficients (p_coef).
! ---------------------------------------------------------------------------------------
    REAL( fp_kind ) , INTENT( IN ) :: Wavenumber,eff_radius,eff_v
    INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
    REAL( fp_kind ) , INTENT( OUT ) :: ext,w0,g
    REAL( fp_kind ) , INTENT( INOUT ), DIMENSION(0:,:) :: p_coef

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
    REAL( fp_kind ) :: d1,d2
    INTEGER :: m,L,L1,L2
!
    ! eff_v is not used yet.
    d1 = eff_v

    p_coef = ZERO

    !  find index L1 and slope d1 on frequency interpolation
    L1 = ( wavenumber - 102 )/FOUR + 1
    d1 = wavenumber -(L1-1) * FOUR - 102.0
 
!  find index L2 and slope d2 on effective radius interpolation
    call find_idx(CloudC%n_Reff_IR,CloudC%Reff_IR,eff_radius,L2,d2)
    IF( Cloud_Type == WATER_CLOUD .OR. Cloud_Type == RAIN_CLOUD) THEN

     ext = (ONE-d1)*(ONE-d2)*CloudC%ext_L_IR(L1,L2)  &
         + (ONE-d1)*d2*CloudC%ext_L_IR(L1,L2+1)      &
         + (ONE-d2)*d1*CloudC%ext_L_IR(L1+1,L2)      &
         + d1*d2*CloudC%ext_L_IR(L1+1,L2+1)
     w0  = (ONE-d1)*(ONE-d2)*CloudC%w_L_IR(L1,L2)  &
         + (ONE-d1)*d2*CloudC%w_L_IR(L1,L2+1)      &
         + (ONE-d2)*d1*CloudC%w_L_IR(L1+1,L2)      &
         + d1*d2*CloudC%w_L_IR(L1+1,L2+1)
     g   = (ONE-d1)*(ONE-d2)*CloudC%g_L_IR(L1,L2)  &
         + (ONE-d1)*d2*CloudC%g_L_IR(L1,L2+1)      &
         + (ONE-d2)*d1*CloudC%g_L_IR(L1+1,L2)      &
         + d1*d2*CloudC%g_L_IR(L1+1,L2+1)

     IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
       DO L = 0, n_Legendre_Terms
        p_coef(L,1)=(ONE-d1)*(ONE-d2)*CloudC%phase_coeff_L_IR(L1,L2,L+Offset_LegTerm) &
        +(ONE-d1)*d2*CloudC%phase_coeff_L_IR(L1,L2+1,L+Offset_LegTerm) &
        +(ONE-d2)*d1*CloudC%phase_coeff_L_IR(L1+1,L2,L+Offset_LegTerm)   &
        +d1*d2*CloudC%phase_coeff_L_IR(L1+1,L2+1,L+Offset_LegTerm)
       ENDDO
     ENDIF
    ELSE IF(Cloud_Type==ICE_CLOUD .OR. Cloud_Type==SNOW_CLOUD .OR. &
       Cloud_Type==GRAUPEL_CLOUD .OR. Cloud_Type==HAIL_CLOUD) THEN
       IF(Cloud_Type == ICE_CLOUD) m = 3
       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3
       ext = (ONE-d1)*(ONE-d2)*CloudC%ext_S_IR(L1,L2,m)  &
           + (ONE-d1)*d2*CloudC%ext_S_IR(L1,L2+1,m)      &
           + (ONE-d2)*d1*CloudC%ext_S_IR(L1+1,L2,m)      &
           + d1*d2*CloudC%ext_S_IR(L1+1,L2+1,m)
                                                     
       w0  = (ONE-d1)*(ONE-d2)*CloudC%w_S_IR(L1,L2,m)    &
           + (ONE-d1)*d2*CloudC%w_S_IR(L1,L2+1,m)        &
           + (ONE-d2)*d1*CloudC%w_S_IR(L1+1,L2,m)        &
           + d1*d2*CloudC%w_S_IR(L1+1,L2+1,m)
                                            
       g   = (ONE-d1)*(ONE-d2)*CloudC%g_S_IR(L1,L2,m)    &
           + (ONE-d1)*d2*CloudC%g_S_IR(L1,L2+1,m)        &
           + (ONE-d2)*d1*CloudC%g_S_IR(L1+1,L2,m)        &
           + d1*d2*CloudC%g_S_IR(L1+1,L2+1,m)
         
     IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
      DO L = 0, n_Legendre_Terms
       p_coef(L,1)=(ONE-d1)*(ONE-d2)*CloudC%phase_coeff_S_IR(L1,L2,m,L+Offset_LegTerm)+ &
              (ONE-d1)*d2*CloudC%phase_coeff_S_IR(L1,L2+1,m,L+Offset_LegTerm) &
       +(ONE-d2)*d1*CloudC%phase_coeff_S_IR(L1+1,L2,m,L+Offset_LegTerm)      &
       +d1*d2*CloudC%phase_coeff_S_IR(L1+1,L2+1,m,L+Offset_LegTerm)
      ENDDO
     ENDIF
                                  
    ENDIF
            
   RETURN
  END SUBROUTINE Get_Cloud_Opt_IR
! 
!
  SUBROUTINE Get_Cloud_Opt_IR_TL(n_Legendre_Terms, &  !INPUT  number of Legendre terms 
                                 n_Phase_Elements, &  !INPUT  number of phase elements
                                       Wavenumber, &  !INPUT  wavenumber in unit 1/cm 
                                       cloud_type, &  !INPUT  see CRTM_Cloud_Define.f90
                                       eff_radius, &  !INPUT  effective radius (mm)
                                            eff_v, &  !INPUT  effective variance of particles
                                    eff_radius_TL, &  !INPUT  effective radius (mm)
                                         eff_v_TL, &  !INPUT  effective variance of particles
                                           ext_TL, &  !OUTPUT optical depth for 1 mm water content
                                            w0_TL, &  !OUTPUT single scattering albedo
                                             g_TL, &  !OUTPUT asymmetry factor
                                         p_coef_TL)   !OUTPUT spherical Legendre coefficients
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining tangent-linear extinction (ext_TL), scattereing (w0_TL) coefficient,
!      factor (g_TL), and Legendre coefficients (p_coef_TL).
! ---------------------------------------------------------------------------------------
       IMPLICIT NONE
       REAL( fp_kind ) , INTENT( IN ) :: Wavenumber,eff_radius,eff_v,eff_radius_TL,eff_v_TL
       INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
       REAL( fp_kind ) , INTENT( INOUT ) :: ext_TL,w0_TL,g_TL
       REAL( fp_kind ) , INTENT( INOUT ), DIMENSION(0:,:) :: p_coef_TL

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
       REAL( fp_kind ) :: d1,d2,d2_TL
       REAL( fp_kind ) :: a1_TL,a2_TL
       INTEGER :: m,L,L1,L2

    !  eff_v is not used yet.
       d1 = eff_v
       d1 = eff_v_TL
!
!  find index L1 and slope d1 on frequency interpolation
      L1 = ( wavenumber - 102 )/FOUR + 1
      d1 = wavenumber -(L1-1) * FOUR - 102.0
       
!  find index L2 and slope d2 on effective radius interpolation
      call find_idx(CloudC%n_Reff_IR,CloudC%Reff_IR,eff_radius,L2,d2)

      call find_idx_TL(CloudC%n_Reff_IR,CloudC%Reff_IR,eff_radius,L2,eff_radius_TL,d2_TL)

    IF( Cloud_Type == WATER_CLOUD .OR. Cloud_Type == RAIN_CLOUD) THEN
           
       a1_TL = -d2_TL*CloudC%ext_L_IR(L1,L2) + d2_TL*CloudC%ext_L_IR(L1,L2+1)
       a2_TL = -d2_TL*CloudC%ext_L_IR(L1+1,L2) + d2_TL*CloudC%ext_L_IR(L1+1,L2+1)
       ext_TL = (ONE-d1)*a1_TL + d1*a2_TL

       a1_TL = -d2_TL*CloudC%w_L_IR(L1,L2) + d2_TL*CloudC%w_L_IR(L1,L2+1)
       a2_TL = -d2_TL*CloudC%w_L_IR(L1+1,L2) + d2_TL*CloudC%w_L_IR(L1+1,L2+1)
       w0_TL = (ONE-d1)*a1_TL + d1*a2_TL

       a1_TL = -d2_TL*CloudC%g_L_IR(L1,L2) + d2_TL*CloudC%g_L_IR(L1,L2+1)
       a2_TL = -d2_TL*CloudC%g_L_IR(L1+1,L2) + d2_TL*CloudC%g_L_IR(L1+1,L2+1)
       g_TL = (ONE-d1)*a1_TL + d1*a2_TL

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
         DO L = 0, n_Legendre_Terms
          a1_TL=-d2_TL*CloudC%phase_coeff_L_IR(L1,L2,L+Offset_LegTerm) &
            +d2_TL*CloudC%phase_coeff_L_IR(L1,L2+1,L+Offset_LegTerm) 
          a2_TL=-d2_TL*CloudC%phase_coeff_L_IR(L1+1,L2,L+Offset_LegTerm) &
            +d2_TL*CloudC%phase_coeff_L_IR(L1+1,L2+1,L+Offset_LegTerm) 
          p_coef_TL(L,1)=(ONE-d1)*a1_TL+d1*a2_TL
         ENDDO
       ENDIF
    
    ELSE IF(Cloud_Type==ICE_CLOUD .OR. Cloud_Type==SNOW_CLOUD .OR. &
       Cloud_Type==GRAUPEL_CLOUD .OR. Cloud_Type==HAIL_CLOUD) THEN
       m = 1   ! Default
       IF(Cloud_Type == ICE_CLOUD) m = 3
       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3
           
       a1_TL = -d2_TL*CloudC%ext_S_IR(L1,L2,m) + d2_TL*CloudC%ext_S_IR(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%ext_S_IR(L1+1,L2,m)+ d2_TL*CloudC%ext_S_IR(L1+1,L2+1,m)
       ext_TL = (ONE-d1)*a1_TL + d1*a2_TL

       a1_TL = -d2_TL*CloudC%w_S_IR(L1,L2,m) + d2_TL*CloudC%w_S_IR(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%w_S_IR(L1+1,L2,m)+ d2_TL*CloudC%w_S_IR(L1+1,L2+1,m)
       w0_TL = (ONE-d1)*a1_TL + d1*a2_TL

       a1_TL = -d2_TL*CloudC%g_S_IR(L1,L2,m) + d2_TL*CloudC%g_S_IR(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%g_S_IR(L1+1,L2,m)+ d2_TL*CloudC%g_S_IR(L1+1,L2+1,m)
       g_TL = (ONE-d1)*a1_TL + d1*a2_TL

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
       DO L = 0, n_Legendre_Terms

       a1_TL = -d2_TL*CloudC%phase_coeff_S_IR(L1,L2,m,L+Offset_LegTerm) &
          + d2_TL*CloudC%phase_coeff_S_IR(L1,L2+1,m,L+Offset_LegTerm)   
       a2_TL = -d2_TL*CloudC%phase_coeff_S_IR(L1+1,L2,m,L+Offset_LegTerm) &
          + d2_TL*CloudC%phase_coeff_S_IR(L1+1,L2+1,m,L+Offset_LegTerm)   
       p_coef_TL(L,1)=(ONE-d1)*a1_TL + d1*a2_TL

       ENDDO
       ENDIF
                                  
    ENDIF
            
   RETURN
  END SUBROUTINE Get_Cloud_Opt_IR_TL
!
!
  SUBROUTINE Get_Cloud_Opt_IR_AD(n_Legendre_Terms, &  !INPUT  number of Legendre terms 
                                 n_Phase_Elements, &  !INPUT  number of phase elements
                                       Wavenumber, &  !INPUT  wavenumber 
                                       cloud_type, &  !INPUT  see CRTM_Cloud_Define.f90
                                       eff_radius, &  !INPUT  effective radius (mm)
                                            eff_v, &  !INPUT  effective variance of particles
                                           ext_AD, &  !INPUT optical depth for 1 mm water content
                                            w0_AD, &  !INPUT single scattering albedo
                                             g_AD, &  !INPUT asymmetry factor
                                        p_coef_AD, &  !INPUT spherical Legendre coefficients
                                    eff_radius_AD, &  !OUTPUT  effective radius (mm)
                                          eff_v_AD)   !OUTPUT  effective variance of particles
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining extinction (ext) and scattereing (w0) coefficients
!      as well as asymmetry factor (g).
! ---------------------------------------------------------------------------------------
       IMPLICIT NONE
       REAL( fp_kind ) , INTENT( IN ) :: Wavenumber,eff_radius,eff_v
       INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
       REAL( fp_kind ) , INTENT( IN ) :: ext_AD,w0_AD,g_AD
       REAL( fp_kind ) , INTENT( IN ), DIMENSION(0:,:) :: p_coef_AD
       REAL( fp_kind ) , INTENT( INOUT ) :: eff_radius_AD,eff_v_AD

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
       REAL( fp_kind ) :: d1,d2,d2_AD
       REAL( fp_kind ) :: a1_AD,a2_AD
       INTEGER :: m,L,L1,L2
!
    ! eff_v is not used yet.
       d2_AD = eff_v
       eff_v_AD = ZERO

       d2_AD = ZERO
!  find index L1 and slope d1 on frequency interpolation
       
       L1 = ( wavenumber - 102 )/FOUR + 1
       d1 = wavenumber -(L1-1) * FOUR - 102.0
!  find index L2 and slope d2 on effective radius interpolation
       call find_idx(CloudC%n_Reff_IR,CloudC%Reff_IR,eff_radius,L2,d2)

    IF( Cloud_Type == WATER_CLOUD .OR. Cloud_Type == RAIN_CLOUD) THEN
       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
           DO L = 0, n_Legendre_Terms
           a2_AD=d1*p_coef_AD(L,1)
           a1_AD=(ONE-d1)*p_coef_AD(L,1)
           d2_AD=d2_AD+a2_AD*CloudC%phase_coeff_L_IR(L1+1,L2+1,L+Offset_LegTerm)
           d2_AD=d2_AD-a2_AD*CloudC%phase_coeff_L_IR(L1+1,L2,L+Offset_LegTerm)
           d2_AD=d2_AD+a1_AD*CloudC%phase_coeff_L_IR(L1,L2+1,L+Offset_LegTerm)
           d2_AD=d2_AD-a1_AD*CloudC%phase_coeff_L_IR(L1,L2,L+Offset_LegTerm)
           ENDDO
       ENDIF
       a2_AD = d1*g_AD
       a1_AD = (ONE-d1)*g_AD
       d2_AD=d2_AD+a2_AD*CloudC%g_L_IR(L1+1,L2+1)
       d2_AD=d2_AD-a2_AD*CloudC%g_L_IR(L1+1,L2)
       d2_AD=d2_AD+a1_AD*CloudC%g_L_IR(L1,L2+1)
       d2_AD=d2_AD-a1_AD*CloudC%g_L_IR(L1,L2)
       a2_AD=d1*w0_AD
       a1_AD=(ONE-d1)*w0_AD
       d2_AD=d2_AD+a2_AD*CloudC%w_L_IR(L1+1,L2+1)
       d2_AD=d2_AD-a2_AD*CloudC%w_L_IR(L1+1,L2)
       d2_AD=d2_AD+a1_AD*CloudC%w_L_IR(L1,L2+1)
       d2_AD=d2_AD-a1_AD*CloudC%w_L_IR(L1,L2)
       a2_AD=d1*ext_AD
       a1_AD=(ONE-d1)*ext_AD
       d2_AD=d2_AD+a2_AD*CloudC%ext_L_IR(L1+1,L2+1)
       d2_AD=d2_AD-a2_AD*CloudC%ext_L_IR(L1+1,L2)
       d2_AD=d2_AD+a1_AD*CloudC%ext_L_IR(L1,L2+1)
       d2_AD=d2_AD-a1_AD*CloudC%ext_L_IR(L1,L2)
 
    ELSE IF(Cloud_Type==ICE_CLOUD .OR. Cloud_Type==SNOW_CLOUD .OR. &
       Cloud_Type==GRAUPEL_CLOUD .OR. Cloud_Type==HAIL_CLOUD) THEN

         m = 1   ! Default
       IF(Cloud_Type == ICE_CLOUD) m = 3
       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
       DO L = 0, n_Legendre_Terms
       a2_AD=d1*p_coef_AD(L,1)
       a1_AD=(ONE-d1)*p_coef_AD(L,1)
       d2_AD=d2_AD+a2_AD*CloudC%phase_coeff_S_IR(L1+1,L2+1,m,L+Offset_LegTerm)
       d2_AD=d2_AD-a2_AD*CloudC%phase_coeff_S_IR(L1+1,L2,m,L+Offset_LegTerm)
       d2_AD=d2_AD+a1_AD*CloudC%phase_coeff_S_IR(L1,L2+1,m,L+Offset_LegTerm)
       d2_AD=d2_AD-a1_AD*CloudC%phase_coeff_S_IR(L1,L2,m,L+Offset_LegTerm)
       ENDDO
       ENDIF

       a2_AD=d1*g_AD
       a1_AD=(ONE-d1)*g_AD
       d2_AD=d2_AD+a2_AD*CloudC%g_S_IR(L1+1,L2+1,m)
       d2_AD=d2_AD-a2_AD*CloudC%g_S_IR(L1+1,L2,m)
       d2_AD=d2_AD+a1_AD*CloudC%g_S_IR(L1,L2+1,m)
       d2_AD=d2_AD-a1_AD*CloudC%g_S_IR(L1,L2,m)
       a2_AD=d1*w0_AD
       a1_AD=(ONE-d1)*w0_AD
       d2_AD=d2_AD+a2_AD*CloudC%w_S_IR(L1+1,L2+1,m)
       d2_AD=d2_AD-a2_AD*CloudC%w_S_IR(L1+1,L2,m)
       d2_AD=d2_AD+a1_AD*CloudC%w_S_IR(L1,L2+1,m)
       d2_AD=d2_AD-a1_AD*CloudC%w_S_IR(L1,L2,m)
       a2_AD=d1*ext_AD
       a1_AD=(ONE-d1)*ext_AD
       d2_AD=d2_AD+a2_AD*CloudC%ext_S_IR(L1+1,L2+1,m)
       d2_AD=d2_AD-a2_AD*CloudC%ext_S_IR(L1+1,L2,m)
       d2_AD=d2_AD+a1_AD*CloudC%ext_S_IR(L1,L2+1,m)
       d2_AD=d2_AD-a1_AD*CloudC%ext_S_IR(L1,L2,m)     

    ENDIF
            
      call find_idx_AD(CloudC%n_Reff_IR,CloudC%Reff_IR,eff_radius,L2,d2_AD,eff_radius_AD)

   RETURN
  END subroutine get_cloud_opt_IR_AD
!
!
  SUBROUTINE Get_Cloud_Opt_MW(n_Legendre_Terms, &  !INPUT  number of Legendre terms
                              n_Phase_Elements, &  !INPUT  number of phase elements
                                     Frequency, &  !INPUT  Frequency in GHz 
                                    cloud_type, &  !INPUT  see CRTM_Cloud_Define.f90
                                    eff_radius, &  !INPUT  effective radius (mm)
                                         eff_v, &  !INPUT  effective variance of particles
                                   Temperature, &  !INPUT  cloudy temperature
                                           ext, &  !INPUT optical depth for 1 mm water content
                                            w0, &  !INPUT single scattering albedo
                                             g, &  !INPUT asymmetry factor
                                        p_coef)    !OUTPUT spherical Legendre coefficients
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining extinction (ext) and scattereing (w0) coefficients
!      as well as asymmetry factor (g).
! ---------------------------------------------------------------------------------------
       IMPLICIT NONE
       REAL( fp_kind ) , INTENT( IN ) :: Frequency,eff_radius,eff_v,Temperature
       INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
       REAL( fp_kind ) , INTENT( OUT ) :: ext,w0,g
       REAL( fp_kind ) , INTENT( INOUT ), DIMENSION(0:,:) :: p_coef

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
       REAL( fp_kind ) :: d1,d2,d3
       REAL( fp_kind ) :: a1,a2
       INTEGER :: k,m,L,L1,L2,L3
!
    ! eff_v is not used yet.
       g = eff_v

       p_coef = ZERO
       w0=ZERO
       g=ZERO
!  find index L1 and slope d1 on frequency interpolation
       call find_idx(CloudC%n_Frequencies,CloudC%frequency,Frequency,L1,d1)

!  find index L2 and slope d2 on temperature interpolation
    IF( Cloud_Type == WATER_CLOUD ) THEN
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L2,d2)
!  Rayleigh approximation for liquid water cloud (non-precipitation), 2-d interpolation

         a1 = (ONE-d2)*CloudC%ext_L_MW(L1,1,L2) + d2*CloudC%ext_L_MW(L1,1,L2+1)
         a2 = (ONE-d2)*CloudC%ext_L_MW(L1+1,1,L2)+ d2*CloudC%ext_L_MW(L1+1,1,L2+1)
         ext = (ONE-d1)*a1 + d1*a2

    ELSE IF( Cloud_Type == ICE_CLOUD ) THEN
!  Rayleigh approximation for fine ice cloud (small particles)
       ext=(ONE-d1)*CloudC%ext_S_MW(L1,1,3)+d1*CloudC%ext_S_MW(L1+1,1,3)

    ELSE IF( Cloud_Type == RAIN_CLOUD ) THEN
!  find index L2 and slope d2 on temperature interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L3,d3)
!  find index L3 and slope d3 on effective radius interpolation
     
       call interp2(d2,d3,CloudC%ext_L_MW(L1,L2,L3),CloudC%ext_L_MW(L1,L2,L3+1), &
                    CloudC%ext_L_MW(L1,L2+1,L3),CloudC%ext_L_MW(L1,L2+1,L3+1),a1)

       call interp2(d2,d3,CloudC%ext_L_MW(L1+1,L2,L3),CloudC%ext_L_MW(L1+1,L2,L3+1), &
                    CloudC%ext_L_MW(L1+1,L2+1,L3),CloudC%ext_L_MW(L1+1,L2+1,L3+1),a2)

       ext = (ONE-d1) * a1 + d1 * a2
                                                          
       call interp2(d2,d3,CloudC%w_L_MW(L1,L2,L3),CloudC%w_L_MW(L1,L2,L3+1), &
                    CloudC%w_L_MW(L1,L2+1,L3),CloudC%w_L_MW(L1,L2+1,L3+1),a1)

       call interp2(d2,d3,CloudC%w_L_MW(L1+1,L2,L3),CloudC%w_L_MW(L1+1,L2,L3+1), &
                    CloudC%w_L_MW(L1+1,L2+1,L3),CloudC%w_L_MW(L1+1,L2+1,L3+1),a2)

       w0 = (ONE-d1) * a1 + d1 * a2
                                   
       call interp2(d2,d3,CloudC%g_L_MW(L1,L2,L3),CloudC%g_L_MW(L1,L2,L3+1), &
                    CloudC%g_L_MW(L1,L2+1,L3),CloudC%g_L_MW(L1,L2+1,L3+1),a1)

       call interp2(d2,d3,CloudC%g_L_MW(L1+1,L2,L3),CloudC%g_L_MW(L1+1,L2,L3+1), &
                    CloudC%g_L_MW(L1+1,L2+1,L3),CloudC%g_L_MW(L1+1,L2+1,L3+1),a2)

       g = (ONE-d1) * a1 + d1 * a2
    
       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms

       call interp2(d2,d3,CloudC%phase_coeff_L_MW(L1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3+1,L+Offset_LegTerm,k),a1)

       call interp2(d2,d3,CloudC%phase_coeff_L_MW(L1+1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3+1,L+Offset_LegTerm,k),a2)

          p_coef(L,k)=(ONE-d1) * a1 + d1 * a2

           ENDDO
          ENDDO
       ENDIF
                                                       
    ELSE IF(Cloud_Type==SNOW_CLOUD.OR.Cloud_Type==GRAUPEL_CLOUD.OR.Cloud_Type==HAIL_CLOUD) THEN
!  find index L2 and slope d2 on effective radius interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)

       m = 1      ! Default
       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3
       a1 = (ONE-d2)*CloudC%ext_S_MW(L1,L2,m) + d2*CloudC%ext_S_MW(L1,L2+1,m)
       a2 = (ONE-d2)*CloudC%ext_S_MW(L1+1,L2,m)+ d2*CloudC%ext_S_MW(L1+1,L2+1,m)
       ext = (ONE-d1)*a1 + d1*a2
           
       a1 = (ONE-d2)*CloudC%w_S_MW(L1,L2,m) + d2*CloudC%w_S_MW(L1,L2+1,m)
       a2 = (ONE-d2)*CloudC%w_S_MW(L1+1,L2,m)+ d2*CloudC%w_S_MW(L1+1,L2+1,m)
       w0 = (ONE-d1)*a1 + d1*a2
           
       a1 = (ONE-d2)*CloudC%g_S_MW(L1,L2,m) + d2*CloudC%g_S_MW(L1,L2+1,m)
       a2 = (ONE-d2)*CloudC%g_S_MW(L1+1,L2,m)+ d2*CloudC%g_S_MW(L1+1,L2+1,m)
       g = (ONE-d1)*a1 + d1*a2
                                            
       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms

       a1 = (ONE-d2)*CloudC%phase_coeff_S_MW(L1,L2,m,L+Offset_LegTerm,k) &
          + d2*CloudC%phase_coeff_S_MW(L1,L2+1,m,L+Offset_LegTerm,k)   
       a2 = (ONE-d2)*CloudC%phase_coeff_S_MW(L1+1,L2,m,L+Offset_LegTerm,k) &
          + d2*CloudC%phase_coeff_S_MW(L1+1,L2+1,m,L+Offset_LegTerm,k)   
       p_coef(L,k)=(ONE-d1)*a1 + d1*a2

          ENDDO
         ENDDO
       ENDIF

    ENDIF
    RETURN
   END SUBROUTINE Get_Cloud_Opt_MW
! 
!
  SUBROUTINE Get_Cloud_Opt_MW_TL(n_Legendre_Terms, &  !INPUT  number of Legendre terms
                                 n_Phase_Elements, &  !INPUT  number of phase elements
                                        Frequency, &  !INPUT  frequency in GHz 
                                       cloud_type, &  !INPUT  see CRTM_Cloud_Define.f90
                                       eff_radius, &  !INPUT  effective radius (mm)
                                            eff_v, &  !INPUT  effective variance of particles
                                      Temperature, &  !INPUT  cloudy temperature
                                    eff_radius_TL, &  !INPUT  effective radius (mm)
                                         eff_v_TL, &  !INPUT  effective variance of particles
                                   Temperature_TL, &  !INPUT  cloudy temperature
                                           ext_TL, &  !OUTPUT optical depth for 1 mm water content
                                            w0_TL, &  !OUTPUT single scattering albedo
                                             g_TL, &  !OUTPUT asymmetry factor
                                         p_coef_TL)   !OUTPUT spherical Legendre coefficients
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining tangent-linear extinction (ext_TL), scattereing (w0_TL) coefficient,
!      factor (g_TL), and Legendre coefficients (p_coef_TL).
! ---------------------------------------------------------------------------------------
       IMPLICIT NONE
       REAL( fp_kind ) , INTENT( IN ) :: Frequency,eff_radius,eff_v,Temperature
       REAL( fp_kind ) , INTENT( IN ) :: eff_radius_TL,eff_v_TL,Temperature_TL
       INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
       REAL( fp_kind ) , INTENT( INOUT ) :: ext_TL,w0_TL,g_TL
       REAL( fp_kind ) , INTENT( INOUT ), DIMENSION(0:,:) :: p_coef_TL

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
       REAL( fp_kind ) :: d1,d2,d3
       REAL( fp_kind ) :: a1_TL,a2_TL,d2_TL,d3_TL
       INTEGER :: k,m,L,L1,L2,L3
!
    !  eff_TL is not used yet.
       g_TL = eff_v
       g_TL = eff_v_TL

       p_coef_TL = ZERO
       w0_TL=ZERO
       g_TL=ZERO
       ext_TL=ZERO
!  find index L1 and slope d1 on frequency interpolation
       call find_idx(CloudC%n_Frequencies,CloudC%frequency,Frequency,L1,d1)

!  find index L2 and slope d2 on temperature interpolation
    IF( Cloud_Type == WATER_CLOUD ) THEN
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L2,d2)
       call find_idx_TL(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L2,Temperature_TL,d2_TL)
!  Rayleigh approximation for liquid water cloud (non-precipitation), 2-d interpolation

         a1_TL = -d2_TL*CloudC%ext_L_MW(L1,1,L2) + d2_TL*CloudC%ext_L_MW(L1,1,L2+1)
         a2_TL = -d2_TL*CloudC%ext_L_MW(L1+1,1,L2)+ d2_TL*CloudC%ext_L_MW(L1+1,1,L2+1)
         ext_TL = (ONE-d1)*a1_TL + d1*a2_TL

    ELSE IF( Cloud_Type == ICE_CLOUD ) THEN
!  Rayleigh approximation for fine ice cloud (small particles)
                                                 
       ext_TL=ZERO

    ELSE IF( Cloud_Type == RAIN_CLOUD ) THEN
!  find index L2 and slope d2 on temperature interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)
       call find_idx_TL(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,eff_radius_TL,d2_TL)
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L3,d3)
       call find_idx_TL(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L3,Temperature_TL,d3_TL)

!  find index L3 and slope d3 on effective radius interpolation
     
       call interp2_TL(d2,d3,CloudC%ext_L_MW(L1,L2,L3),CloudC%ext_L_MW(L1,L2,L3+1), &
                    CloudC%ext_L_MW(L1,L2+1,L3),CloudC%ext_L_MW(L1,L2+1,L3+1),d2_TL,d3_TL,a1_TL)
                                                   
       call interp2_TL(d2,d3,CloudC%ext_L_MW(L1+1,L2,L3),CloudC%ext_L_MW(L1+1,L2,L3+1), &
                    CloudC%ext_L_MW(L1+1,L2+1,L3),CloudC%ext_L_MW(L1+1,L2+1,L3+1),d2_TL,d3_TL,a2_TL)

       ext_TL = (ONE-d1) * a1_TL + d1 * a2_TL

       call interp2_TL(d2,d3,CloudC%w_L_MW(L1,L2,L3),CloudC%w_L_MW(L1,L2,L3+1), &
                    CloudC%w_L_MW(L1,L2+1,L3),CloudC%w_L_MW(L1,L2+1,L3+1),d2_TL,d3_TL,a1_TL)
                                                   
       call interp2_TL(d2,d3,CloudC%w_L_MW(L1+1,L2,L3),CloudC%w_L_MW(L1+1,L2,L3+1), &
                    CloudC%w_L_MW(L1+1,L2+1,L3),CloudC%w_L_MW(L1+1,L2+1,L3+1),d2_TL,d3_TL,a2_TL)

       w0_TL = (ONE-d1) * a1_TL + d1 * a2_TL
                                   
       call interp2_TL(d2,d3,CloudC%g_L_MW(L1,L2,L3),CloudC%g_L_MW(L1,L2,L3+1), &
                    CloudC%g_L_MW(L1,L2+1,L3),CloudC%g_L_MW(L1,L2+1,L3+1),d2_TL,d3_TL,a1_TL)
                                                   
       call interp2_TL(d2,d3,CloudC%g_L_MW(L1+1,L2,L3),CloudC%g_L_MW(L1+1,L2,L3+1), &
                    CloudC%g_L_MW(L1+1,L2+1,L3),CloudC%g_L_MW(L1+1,L2+1,L3+1),d2_TL,d3_TL,a2_TL)

       g_TL = (ONE-d1) * a1_TL + d1 * a2_TL

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms

       call interp2_TL(d2,d3,CloudC%phase_coeff_L_MW(L1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3+1,L+Offset_LegTerm,k),d2_TL,d3_TL,a1_TL)

       call interp2_TL(d2,d3,CloudC%phase_coeff_L_MW(L1+1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3+1,L+Offset_LegTerm,k),d2_TL,d3_TL,a2_TL)

          p_coef_TL(L,k)=(ONE-d1) * a1_TL + d1 * a2_TL
           ENDDO
          ENDDO
       ENDIF
                                                       
    ELSE IF(Cloud_Type==SNOW_CLOUD.OR.Cloud_Type==GRAUPEL_CLOUD.OR.Cloud_Type==HAIL_CLOUD) THEN
!  find index L2 and slope d2 on effective radius interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)
       call find_idx_TL(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,eff_radius_TL,d2_TL)

       IF(eff_radius_TL == ZERO) RETURN

       m = 1   ! Default
       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3
           
       a1_TL = -d2_TL*CloudC%ext_S_MW(L1,L2,m) + d2_TL*CloudC%ext_S_MW(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%ext_S_MW(L1+1,L2,m)+ d2_TL*CloudC%ext_S_MW(L1+1,L2+1,m)
       ext_TL = (ONE-d1)*a1_TL + d1*a2_TL

       a1_TL = -d2_TL*CloudC%w_S_MW(L1,L2,m) + d2_TL*CloudC%w_S_MW(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%w_S_MW(L1+1,L2,m)+ d2_TL*CloudC%w_S_MW(L1+1,L2+1,m)
       w0_TL = (ONE-d1)*a1_TL + d1*a2_TL
           
       a1_TL = -d2_TL*CloudC%g_S_MW(L1,L2,m) + d2_TL*CloudC%g_S_MW(L1,L2+1,m)
       a2_TL = -d2_TL*CloudC%g_S_MW(L1+1,L2,m)+ d2_TL*CloudC%g_S_MW(L1+1,L2+1,m)
       g_TL = (ONE-d1)*a1_TL + d1*a2_TL
                                            
       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms

       a1_TL = -d2_TL*CloudC%phase_coeff_S_MW(L1,L2,m,L+Offset_LegTerm,k) &
          + d2_TL*CloudC%phase_coeff_S_MW(L1,L2+1,m,L+Offset_LegTerm,k)   
       a2_TL = -d2_TL*CloudC%phase_coeff_S_MW(L1+1,L2,m,L+Offset_LegTerm,k) &
          + d2_TL*CloudC%phase_coeff_S_MW(L1+1,L2+1,m,L+Offset_LegTerm,k)   
       p_coef_TL(L,k)=(ONE-d1)*a1_TL + d1*a2_TL

          ENDDO
         ENDDO
       ENDIF

    ENDIF
   RETURN
  END SUBROUTINE Get_Cloud_Opt_MW_TL
! 
!
  SUBROUTINE Get_Cloud_Opt_MW_AD(n_Legendre_Terms, &  !INPUT  number of Legendre terms
                                 n_Phase_Elements, &  !INPUT  number of phase elements
                                        Frequency, &  !INPUT  frequency in GHz 
                                       cloud_type, &  !INPUT  see CRTM_Cloud_Define.f90
                                       eff_radius, &  !INPUT  effective radius (mm)
                                            eff_v, &  !INPUT  effective variance of particles
                                      Temperature, &  !INPUT  cloudy temperature
                                           ext_AD, &  !INPUT optical depth for 1 mm water content
                                            w0_AD, &  !INPUT single scattering albedo
                                             g_AD, &  !INPUT asymmetry factor
                                        p_coef_AD, &  !INPUT spherical Legendre coefficients
                                    eff_radius_AD, &  !OUTPUT  effective radius (mm)
                                         eff_v_AD, &  !OUTPUT  effective variance of particles
                                   Temperature_AD)  !INPUT  cloudy temperature
! ---------------------------------------------------------------------------------------
!    Function:
!      obtaining extinction (ext) and scattereing (w0) coefficients
!      as well as asymmetry factor (g).
! ---------------------------------------------------------------------------------------
       IMPLICIT NONE
       REAL( fp_kind ) , INTENT( IN ) :: Frequency,eff_radius,eff_v,Temperature
       INTEGER, INTENT( IN ) :: Cloud_Type,n_Legendre_Terms,n_Phase_Elements
       REAL( fp_kind ) , INTENT( IN ) :: ext_AD,w0_AD,g_AD
       REAL( fp_kind ) , INTENT( IN ), DIMENSION(0:,:) :: p_coef_AD
       REAL( fp_kind ) , INTENT( INOUT ) :: eff_radius_AD,eff_v_AD,Temperature_AD

    ! ----------------------- !
    !  local variables        !
    ! ----------------------- !
       REAL( fp_kind ) :: d1,d2,d3
       REAL( fp_kind ) :: a1_AD,a2_AD,d2_AD,d3_AD
       INTEGER :: k,m,L,L1,L2,L3
!
    ! eff_v is not used yet.
       d2_AD = eff_v
       eff_v_AD = ZERO
       d2_AD = ZERO
       d3_AD = ZERO
!  find index L1 and slope d1 on frequency interpolation
       call find_idx(CloudC%n_Frequencies,CloudC%frequency,Frequency,L1,d1)

!  find index L2 and slope d2 on temperature interpolation
    IF( Cloud_Type == WATER_CLOUD ) THEN
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L2,d2)
!  Rayleigh approximation for liquid water cloud (non-precipitation), 2-d interpolation

         a2_AD=d1*ext_AD
         a1_AD=(ONE-d1)*ext_AD
         d2_AD=a2_AD*CloudC%ext_L_MW(L1+1,1,L2+1)
         d2_AD=d2_AD-a2_AD*CloudC%ext_L_MW(L1+1,1,L2)
         d2_AD=d2_AD+a1_AD*CloudC%ext_L_MW(L1,1,L2+1)
         d2_AD=d2_AD-a1_AD*CloudC%ext_L_MW(L1,1,L2)

       call find_idx_AD(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L2,d2_AD,Temperature_AD)
    ELSE IF( Cloud_Type == ICE_CLOUD ) THEN
!  Rayleigh approximation for fine ice cloud (small particles), constant optical parameters
!!       ext_AD=ZERO
                                                 
    ELSE IF( Cloud_Type == RAIN_CLOUD ) THEN
!  find index L2 and slope d2 on temperature interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)
       call find_idx(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L3,d3)
!  find index L3 and slope d3 on effective radius interpolation

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = 1, n_Phase_Elements
           DO L = 0, n_Legendre_Terms

         a2_AD=d1 * p_coef_AD(L,k)
         a1_AD=(ONE-d1) * p_coef_AD(L,k)
       
       call interp2_AD(d2,d3,CloudC%phase_coeff_L_MW(L1+1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1+1,L2+1,L3+1,L+Offset_LegTerm,k),a2_AD,d2_AD,d3_AD)

       call interp2_AD(d2,d3,CloudC%phase_coeff_L_MW(L1,L2,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2,L3+1,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3,L+Offset_LegTerm,k), &
           CloudC%phase_coeff_L_MW(L1,L2+1,L3+1,L+Offset_LegTerm,k),a1_AD,d2_AD,d3_AD)

           ENDDO
          ENDDO

       ENDIF
     
        a2_AD = d1 * g_AD
        a1_AD = (ONE-d1) * g_AD
       call interp2_AD(d2,d3,CloudC%g_L_MW(L1+1,L2,L3),CloudC%g_L_MW(L1+1,L2,L3+1), &
                    CloudC%g_L_MW(L1+1,L2+1,L3),CloudC%g_L_MW(L1+1,L2+1,L3+1),a2_AD,d2_AD,d3_AD)

       call interp2_AD(d2,d3,CloudC%g_L_MW(L1,L2,L3),CloudC%g_L_MW(L1,L2,L3+1), &
                    CloudC%g_L_MW(L1,L2+1,L3),CloudC%g_L_MW(L1,L2+1,L3+1),a1_AD,d2_AD,d3_AD)

       a2_AD = d1 * w0_AD
       a1_AD = (ONE-d1) * w0_AD

       call interp2_AD(d2,d3,CloudC%w_L_MW(L1+1,L2,L3),CloudC%w_L_MW(L1+1,L2,L3+1), &
                    CloudC%w_L_MW(L1+1,L2+1,L3),CloudC%w_L_MW(L1+1,L2+1,L3+1),a2_AD,d2_AD,d3_AD)

       call interp2_AD(d2,d3,CloudC%w_L_MW(L1,L2,L3),CloudC%w_L_MW(L1,L2,L3+1), &
                    CloudC%w_L_MW(L1,L2+1,L3),CloudC%w_L_MW(L1,L2+1,L3+1),a1_AD,d2_AD,d3_AD)

       a2_AD = d1 * ext_AD
       a1_AD = (ONE-d1) * ext_AD

       call interp2_AD(d2,d3,CloudC%ext_L_MW(L1+1,L2,L3),CloudC%ext_L_MW(L1+1,L2,L3+1), &
                    CloudC%ext_L_MW(L1+1,L2+1,L3),CloudC%ext_L_MW(L1+1,L2+1,L3+1),a2_AD,d2_AD,d3_AD)

       call interp2_AD(d2,d3,CloudC%ext_L_MW(L1,L2,L3),CloudC%ext_L_MW(L1,L2,L3+1), &
                    CloudC%ext_L_MW(L1,L2+1,L3),CloudC%ext_L_MW(L1,L2+1,L3+1),a1_AD,d2_AD,d3_AD)

     call find_idx_AD(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2_AD,eff_radius_AD)

     call find_idx_AD(CloudC%n_Temperatures,CloudC%Temperature,Temperature,L3,d3_AD,Temperature_AD)
                                       
    ELSE IF(Cloud_Type==SNOW_CLOUD.OR.Cloud_Type==GRAUPEL_CLOUD.OR.Cloud_Type==HAIL_CLOUD) THEN
!  find index L2 and slope d2 on effective radius interpolation
       call find_idx(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2)
       m = 1   ! Default

       IF(Cloud_Type == SNOW_CLOUD) m = 1
       IF(Cloud_Type == GRAUPEL_CLOUD) m = 2
       IF(Cloud_Type == HAIL_CLOUD) m = 3

       IF(n_Phase_Elements > 0 .AND. n_Legendre_Terms > 2) THEN
          DO k = n_Phase_Elements, 1, -1
           DO L = n_Legendre_Terms, 0, -1

       a2_AD = d1 * p_coef_AD(L,k)
       a1_AD = (ONE-d1) * p_coef_AD(L,k)
       d2_AD = d2_AD + a2_AD*CloudC%phase_coeff_S_MW(L1+1,L2+1,m,L+Offset_LegTerm,k)
       d2_AD =  d2_AD - a2_AD*CloudC%phase_coeff_S_MW(L1+1,L2,m,L+Offset_LegTerm,k)
       d2_AD = d2_AD + a1_AD*CloudC%phase_coeff_S_MW(L1,L2+1,m,L+Offset_LegTerm,k)
       d2_AD = d2_AD - a1_AD*CloudC%phase_coeff_S_MW(L1,L2,m,L+Offset_LegTerm,k)
          ENDDO
         ENDDO
       ENDIF

       a2_AD = d1 * g_AD
       a1_AD = (ONE-d1)*g_AD
       d2_AD = d2_AD + a2_AD * CloudC%g_S_MW(L1+1,L2+1,m)
       d2_AD = d2_AD - a2_AD*CloudC%g_S_MW(L1+1,L2,m)
       d2_AD = d2_AD + a1_AD*CloudC%g_S_MW(L1,L2+1,m)
       d2_AD = d2_AD - a1_AD*CloudC%g_S_MW(L1,L2,m)
       a2_AD = d1 * w0_AD
       a1_AD = (ONE-d1)* w0_AD
       d2_AD = d2_AD + a2_AD * CloudC%w_S_MW(L1+1,L2+1,m)
       d2_AD = d2_AD - a2_AD*CloudC%w_S_MW(L1+1,L2,m)
       d2_AD = d2_AD + a1_AD*CloudC%w_S_MW(L1,L2+1,m)
       d2_AD = d2_AD - a1_AD*CloudC%w_S_MW(L1,L2,m)
       a2_AD = d1 * ext_AD
       a1_AD = (ONE-d1)*ext_AD
       d2_AD = d2_AD + a2_AD*CloudC%ext_S_MW(L1+1,L2+1,m)
       d2_AD = d2_AD - a2_AD*CloudC%ext_S_MW(L1+1,L2,m)
       d2_AD = d2_AD + a1_AD*CloudC%ext_S_MW(L1,L2+1,m)
       d2_AD = d2_AD - a1_AD*CloudC%ext_S_MW(L1,L2,m)
       call find_idx_AD(CloudC%n_Reff_MW,CloudC%Reff_MW,eff_radius,L2,d2_AD,eff_radius_AD)

    ENDIF
   RETURN
  END SUBROUTINE Get_Cloud_Opt_MW_AD
! 
!
      SUBROUTINE find_idx(n, X, X0, idx, slope)
! ---------------------------------------------------------------------------
!   Find index and slope.
! ---------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: n
      REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: X
      REAL( fp_kind ), INTENT( IN ) :: X0
      INTEGER, INTENT( OUT ) :: idx
      REAL( fp_kind ), INTENT( OUT ) :: slope
!
      INTEGER :: k

         if(X0 <= X(1) ) then
         slope = ZERO
         idx = 1
         else if(X0 >= X(n) ) then
         slope = ONE
         idx = n - 1
         else
         do k = 1, n
         if(X0 <= X(k) ) go to 801
         enddo
 801     idx = k - 1 
         slope = (X0 - X(idx))/( X(idx+1)-X(idx) )
         endif
      RETURN
      END SUBROUTINE find_idx
! 
      SUBROUTINE find_idx_TL(n, X, X0, idx, X0_TL, slope_TL)
! ---------------------------------------------------------------------------
!   Find index and slope.
! ---------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: n
      REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: X
      REAL( fp_kind ), INTENT( IN ) :: X0, X0_TL
      INTEGER, INTENT( IN ) :: idx
      REAL( fp_kind ), INTENT( OUT ) :: slope_TL
!
         if( X0 <= X(1) ) then
         slope_TL = ZERO 
         else if(X0 >= X(n) ) then
         slope_TL = ZERO 
         else
         slope_TL = X0_TL/( X(idx+1)-X(idx) )
         endif
      RETURN
      END SUBROUTINE find_idx_TL
! 
      SUBROUTINE find_idx_AD(n, X, X0, idx, slope_AD, X0_AD)
! ---------------------------------------------------------------------------
!   Find index and slope.
! ---------------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER, INTENT( IN ) :: n
      REAL( fp_kind ), INTENT( IN ), DIMENSION( : ) :: X
      REAL( fp_kind ), INTENT( IN ) ::  X0
      REAL( fp_kind ), INTENT( INOUT ) :: slope_AD
      INTEGER, INTENT( IN ) :: idx
      REAL( fp_kind ), INTENT( OUT ) :: X0_AD
!
         X0_AD = ZERO
         if( X0 <= X(1) ) then
         slope_AD = ZERO 
         else if(X0 >= X(n) ) then
         slope_AD = ZERO 
         else
         X0_AD = slope_AD/( X(idx+1)-X(idx) )
         endif
      RETURN
      END SUBROUTINE find_idx_AD


  ! -----------------------------
  ! Two-dimensional interpolation
  ! -----------------------------
  SUBROUTINE interp2(x1,x2,y1,y2,y3,y4,y)

    REAL( fp_kind ), INTENT( IN ) :: x1,x2,y1,y2,y3,y4 
    REAL( fp_kind ), INTENT( OUT ) :: y 

    y=(ONE-x1)*(ONE-x2)*y1+(ONE-x1)*x2*y2+x1*(ONE-x2)*y3+x1*x2*y4

  END SUBROUTINE interp2


  ! -----------------------------------------------
  ! Tangent-linear of two-dimensional interpolation 
  ! -----------------------------------------------
  SUBROUTINE interp2_TL(x1,x2,y1,y2,y3,y4,x1_TL,x2_TL,y_TL)

    REAL( fp_kind ), INTENT( IN ) :: x1,x2,x1_TL,x2_TL,y1,y2,y3,y4 
    REAL( fp_kind ), INTENT( OUT ) :: y_TL

    y_TL= (-(ONE-x2)*y1-x2*y2+(ONE-x2)*y3+x2*y4)*x1_TL + &
          (-(ONE-x1)*y1+(ONE-x1)*y2-x1*y3+x1*y4)*x2_TL

  END SUBROUTINE interp2_TL


  ! ----------------------------------------
  ! Adjoint of two-dimensional interpolation 
  ! ----------------------------------------
  SUBROUTINE interp2_AD(x1,x2,y1,y2,y3,y4,y_AD,x1_AD,x2_AD)

    REAL( fp_kind ), INTENT( IN )     :: x1,x2,y1,y2,y3,y4 
    REAL( fp_kind ), INTENT( IN OUT ) :: y_AD
    REAL( fp_kind ), INTENT( IN OUT ) :: x1_AD,x2_AD

    x1_AD = x1_AD + (-(ONE-x2)*y1-x2*y2+(ONE-x2)*y3+x2*y4)*y_AD
    x2_AD = x2_AD + (-(ONE-x1)*y1+(ONE-x1)*y2-x1*y3+x1*y4)*y_AD
    y_AD = ZERO

  END SUBROUTINE interp2_AD

END MODULE CRTM_CloudScatter


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: CRTM_CloudScatter.f90,v 1.6 2006/06/23 23:20:10 wd20pd Exp $
!
! $Date: 2006/06/23 23:20:10 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_CloudScatter.f90,v $
! Revision 1.6  2006/06/23 23:20:10  wd20pd
! - Changed shared data structure name from ScatC to CloudC to relfect changes
!   in the CRTM_CloudCoeff module.
! - Changed some of the CloudC component names (dimension values) to relfect
!   changes in the CloudCoeff_Define module.
!
! Revision 1.5  2006/05/25 19:27:59  wd20pd
! Removed redundant parameter definitions.
!
! Revision 1.4  2006/05/18 23:18:31  dgroff
! The variable ZERO is use associated with the module (CRTM_Atmosphere_Define).
! The Fortran "ONLY" statement is used to remove any conflicts with the usage
! of variable 'ZERO' in this module.
!
! Revision 1.3  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2006/04/24 14:05:52  wd20pd
! - Merged CRTM_Sensor branch onto the main trunk.
!
! Revision 1.1.2.9  2006/04/20 20:34:53  paulv
! - Added Message_Log arguments to public procedures.
!
! Revision 1.1.2.8  2006/04/17 16:47:08  paulv
! - Added internal variable CSV to the CloudScatter procedure argumen list
!   to preserve the forward model variables required for the TL and AD
!   calculations.
! - Tidied up some other parameter definitions.
!
! Revision 1.1.2.7  2005/10/18 12:11:10  paulv
! - Corrected bug in interp2_AD() routine. The output adjoint variables were
!   declared with INTENT(IN). This has been changed to INTENT(IN OUT).
!
! Revision 1.1.2.6  2005/10/13 22:21:24  paulv
! - Corrected bug in use of kidx and PACK intrinsic in CRTM_Compute_CloudScatter_TL
!   and CRTM_Compute_CloudScatter_AD. The LHS and RHS of the offending lines were
!   made conformable.
!
! Revision 1.1.2.5  2005/10/13 15:12:51  paulv
! - Corrected bug in use of kidx and PACK intrinsic in CRTM_Compute_CloudScatter.
!   The LHS and RHS of the offending line were made conformable.
!
! Revision 1.1.2.4  2005/09/23 19:36:38  yhan
! --- Replaced Cloud_Type = n with Cloud_Type = Atmosphere%cloud(n)%Type
!
! Revision 1.1.2.3  2005/08/23 22:21:51  qliu
! -- Deleted unused variables.
!
! Revision 1.1.2.2  2005/08/19 20:14:32  qliu
! -- change "ScatterCoeff" to CloudCoeff.
!
! Revision 1.1.2.1  2005/08/16 17:47:51  qliu
! - First working version of cloud scattering and absorption.
!
! Revision 1.1  2005/02/25 00:13:14  paulv
! Initial checkin.
!
!
!
!


!

