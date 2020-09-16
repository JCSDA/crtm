!
! CRTM_MW_Snow_SfcOptics
!
! Module to compute the surface optical properties for SNOW surfaces at
! microwave frequencies required for determining the SNOW surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Jun-2005
!                       paul.vandelst@noaa.gov
!
!       Modified by:    Banghua Yan, 03-Oct-2007
!                       Banghua.Yan@noaa.gov
!
!       Modified by:    Yong Chen. 09-Jul-212
!                       Yong.Chen@noaa.gov

MODULE CRTM_MW_Snow_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS
  USE CRTM_Parameters,            ONLY: ZERO, ONE
  USE CRTM_SpcCoeff,              ONLY: SC
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type, &
                                        CRTM_GeometryInfo_GetValue
  USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type
  USE CRTM_SensorInfo,            ONLY: WMO_AMSUA, &
                                        WMO_AMSUB, &
                                        WMO_AMSRE, &
                                        WMO_SSMI , &
                                        WMO_MSU  , &
                                        WMO_MHS  , &
                                        WMO_SSMIS, &
                                        WMO_ATMS
  USE NESDIS_LandEM_Module,       ONLY: NESDIS_LandEM
  USE NESDIS_AMSU_SNOWEM_Module,  ONLY: NESDIS_AMSU_SNOWEM
  USE NESDIS_SSMI_SNOWEM_Module,  ONLY: NESDIS_SSMI_SnowEM
  USE NESDIS_AMSRE_SNOWEM_Module, ONLY: NESDIS_AMSRE_SNOW
  USE NESDIS_MHS_SNOWEM_Module,   ONLY: NESDIS_SNOWEM_MHS
  USE NESDIS_SSMIS_SnowEM_Module, ONLY: NESDIS_SSMIS_SnowEM
  USE NESDIS_ATMS_SnowEM_Module,  ONLY: NESDIS_ATMS_SNOWEM
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_MW_Snow_SfcOptics
  PUBLIC :: Compute_MW_Snow_SfcOptics_TL
  PUBLIC :: Compute_MW_Snow_SfcOptics_AD
  PUBLIC :: Populate_Subset

  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE iVar_type


CONTAINS


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Snow_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at microwave
!       frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Snow_SfcOptics( &
!                        Surface     , &
!                        GeometryInfo, &
!                        SensorIndex , &
!                        ChannelIndex, &
!                        SfcOptics     )
!
! INPUTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Snow_SfcOptics( &
    Surface     , &  ! Input
    GeometryInfo, &  ! Input
    SensorIndex , &  ! Input
    ChannelIndex, &  ! Input
    SfcOptics   ) &  ! Output
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Compute_MW_Snow_SfcOptics'
    REAL(fp), PARAMETER :: MSU_SNOW_TEMPERATURE_THRESHOLD = 100.0_fp  ! K
    REAL(fp), PARAMETER :: MSU_TB_THRESHOLD               =  50.0_fp  ! K
    REAL(fp), PARAMETER :: MSU_ALPHA_C                    =   0.35_fp
    REAL(fp), PARAMETER :: MSU_EMISSIVITY_THRESHOLD       =   0.6_fp
    REAL(fp), PARAMETER :: MSU_DEFAULT_EMISSIVITY         =   0.855_fp
    REAL(fp), PARAMETER :: FREQUENCY_THRESHOLD            =  80.0_fp  ! GHz
    REAL(fp), PARAMETER :: DEFAULT_EMISSIVITY             =   0.90_fp
    REAL(fp), PARAMETER :: NOT_USED(4)                    = -99.9_fp
    INTEGER,  PARAMETER :: AMSRE_V_INDEX(6) = (/1, 3, 5, 7, 9, 11/)  ! AMSRE channels with V pol.
    INTEGER,  PARAMETER :: AMSRE_H_INDEX(6) = (/2, 4, 6, 8, 10, 12/) ! AMSRE channels with H pol.
    INTEGER,  PARAMETER :: AMSUA_INDEX(4)   = (/1, 2, 3, 15/)
    INTEGER,  PARAMETER :: AMSUB_INDEX(2)   = (/1, 2/)
    INTEGER,  PARAMETER :: SSMIS_INDEX(8)   = (/13,12,14,16,15,17,18,8/)  ! With swapped polarisations
    INTEGER,  PARAMETER :: SSMI_INDEX(7)   =  (/1,2,3,4,5,6,7/) 
    INTEGER,  PARAMETER :: ATMS_INDEX(5)    = (/1, 2, 3, 16,17/)          ! With mixed polarisations
    INTEGER,  PARAMETER :: MaxChan = 8 
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Sensor_Zenith_Angle
    REAL(fp) :: Alpha
    REAL(fp) :: TBs_In(MaxChan)
    REAL(fp) :: TBs_In_V(MaxChan)
    REAL(fp) :: TBs_In_H(MaxChan)


    ! Set up
    Error_Status = SUCCESS
    CALL CRTM_GeometryInfo_GetValue( GeometryInfo, Sensor_Zenith_Angle = Sensor_Zenith_Angle )


    ! Compute the surface emissivities
    Sensor_Type: SELECT CASE( Surface%SensorData%WMO_Sensor_ID )

      ! ATMSemissivity model
      CASE( WMO_ATMS )    
         TBs_In(1:5) = Populate_Subset( Surface, 5, ATMS_INDEX)

         DO i = 1, SfcOptics%n_Angles
          CALL NESDIS_ATMS_SNOWEM( Sensor_Zenith_Angle,                     &  ! Input, Degree           
                                   SfcOptics%Angle(i),                      &  ! Input, Degree           
                                   SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz                  
                                   TBs_In(1:5),                             &  ! Input, ATMS           
                                   Surface%Snow_Temperature,                &  ! Input, K                
                                   Surface%Snow_Depth,                      &  ! Input, mm               
                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component      
                                   SfcOptics%Emissivity(i,1)   )               ! Output, V component 
         END DO                                                                                           


      ! AMSU-A emissivity model
      CASE( WMO_AMSUA )
         TBs_In(1:4) = Populate_Subset( Surface, 4, AMSUA_INDEX)

         DO i = 1, SfcOptics%n_Angles
           CALL NESDIS_AMSU_SNOWEM( Sensor_Zenith_Angle,                    &  ! Input, Degree
                                   SfcOptics%Angle(i),                      &  ! Input, Degree
                                   SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                   Surface%Snow_Depth,                      &  ! Input, mm
                                   Surface%Snow_Temperature,                &  ! Input, K
                                   TBs_In(1:4),                             &  ! Input, AMSUA
                                   NOT_USED(1:2),                           &  ! Input, AMSUB  *** NO AMSU-B DATA ***
                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                   SfcOptics%Emissivity(i,1)                )  ! Output, V component
         END DO

      ! AMSU-B emissivity model
      CASE( WMO_AMSUB)
         TBs_In(1:2) = Populate_Subset( Surface, 2, AMSUB_INDEX)

         DO i = 1, SfcOptics%n_Angles
           CALL NESDIS_AMSU_SNOWEM( Sensor_Zenith_Angle,                    &  ! Input, Degree
                                   SfcOptics%Angle(i),                      &  ! Input, Degree
                                   SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                   Surface%Snow_Depth,                      &  ! Input, mm
                                   Surface%Snow_Temperature,                &  ! Input, K
                                   NOT_USED(1:4),                           &  ! Input  AMSUA  *** NO AMSU-A DATA ***
                                   TBs_In(1:2),                             &  ! Input, AMSUB
                                   SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                   SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! MHS emissivity model
      CASE (WMO_MHS)
         TBs_In(1:2) = Populate_Subset( Surface, 2, AMSUB_INDEX)

         DO i = 1, SfcOptics%n_Angles
            CALL NESDIS_SNOWEM_MHS( Sensor_Zenith_Angle,                   &  ! Input, Degree
                                 SfcOptics%Angle(i),                      &  ! Input, Degree
                                 SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                 Surface%Snow_Temperature,                &  ! Input, K
                                 Tbs_In(1:2),                             &  ! Input, AMSUB
                                 SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                 SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! AMSR-E emissivity model
      CASE( WMO_AMSRE )
         TBs_In_V(1:6) = Populate_Subset( Surface, 6, AMSRE_V_INDEX)
         TBs_In_H(1:6) = Populate_Subset( Surface, 6, AMSRE_H_INDEX)
         DO i = 1, SfcOptics%n_Angles
            CALL NESDIS_AMSRE_SNOW(SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                 SfcOptics%Angle(i),                      &  ! Input, Degree
                                 TBs_In_V(1:6),                           &  ! Input, Tb_V, K
                                 TBs_In_H(1:6),                           &  ! Input, Tb_H, K
                                 Surface%Snow_Temperature,                &  ! Input, Ts, K
                                 Surface%Snow_Temperature,                &  ! Input, Tsnow, K
                                 SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                 SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! SSM/I emissivity model
      CASE( WMO_SSMI )
         TBs_In(1:7) = Populate_Subset( Surface, 7, SSMI_INDEX)
         DO i = 1, SfcOptics%n_Angles
            CALL NESDIS_SSMI_SnowEM(SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                    SfcOptics%Angle(i),                      &  ! Input, Degree
                                    Surface%Snow_Temperature,                &  ! Input, K
                                    TBs_In(1:7),                             &  ! Input, K
                                    Surface%Snow_Depth,                      &  ! Input, mm
                                    SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                    SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! SSMIS emissivity model
      CASE( WMO_SSMIS )
         TBs_In(1:8) = Populate_Subset( Surface, 8, SSMIS_INDEX)
         DO i = 1, SfcOptics%n_Angles
            CALL NESDIS_SSMIS_SnowEM(SC(SensorIndex)%Frequency(ChannelIndex), &  ! Input, GHz
                                     SfcOptics%Angle(i),                      &  ! Input, Degree
                                     Surface%Snow_Temperature,                &  ! Input, K
                                     TBs_In(1:8),                             &  ! Input, K
                                     Surface%Snow_Depth,                      &  ! Input, mm
                                     SfcOptics%Emissivity(i,2),               &  ! Output, H component
                                     SfcOptics%Emissivity(i,1)                )  ! Output, V component
        END DO

      ! MSU emissivity model
      CASE( WMO_MSU )
        DO i = 1, SfcOptics%n_Angles
          IF( Surface%Snow_Temperature > MSU_SNOW_TEMPERATURE_THRESHOLD .AND. &
              Surface%SensorData%Tb(1) > MSU_TB_THRESHOLD                     ) THEN
            Alpha = MSU_ALPHA_C * Surface%Snow_Temperature
            SfcOptics%Emissivity(i,1) = (Surface%SensorData%Tb(1)-Alpha)/&
                                        (Surface%Snow_Temperature-Alpha)
            IF( SfcOptics%Emissivity(i,1) > ONE ) &
              SfcOptics%Emissivity(i,1) = ONE
            IF( SfcOptics%Emissivity(i,1) < MSU_EMISSIVITY_THRESHOLD ) &
              SfcOptics%Emissivity(i,1) = MSU_EMISSIVITY_THRESHOLD
          ELSE
            SfcOptics%Emissivity(i,1) = MSU_DEFAULT_EMISSIVITY
          END IF
          SfcOptics%Emissivity(i,2) = SfcOptics%Emissivity(i,1)
        END DO

      ! Default physical model
      CASE DEFAULT
        IF ( SC(SensorIndex)%Frequency(ChannelIndex) < FREQUENCY_THRESHOLD ) THEN
          DO i = 1, SfcOptics%n_Angles
            CALL NESDIS_LandEM( SfcOptics%Angle(i),                      & ! Input, Degree
                                SC(SensorIndex)%Frequency(ChannelIndex), & ! Input, GHz
                                NOT_USED(1),                             & ! Input, Soil_Moisture_Content, g.cm^-3
                                NOT_USED(1),                             & ! Input, Vegetation_Fraction
                                Surface%Snow_Temperature,                & ! Input, K
                                Surface%Snow_Temperature,                & ! Input, K
                                Surface%Lai,                             & ! Input, Leaf Area Index
                                Surface%Soil_Type,                       & ! Input, Soil Type (1 -  9)
                                Surface%Vegetation_Type,                 & ! Input, Vegetation Type (1 - 13)
                                Surface%Snow_Depth,                      & ! Input, mm
                                SfcOptics%Emissivity(i,2),               & ! Output, H component
                                SfcOptics%Emissivity(i,1)                ) ! Output, V component
          END DO
        ELSE
          SfcOptics%Emissivity(1:SfcOptics%n_Angles,1:2) = DEFAULT_EMISSIVITY
        END IF

    END SELECT Sensor_Type
    
    ! assuming a specular surface
    SfcOptics%Reflectivity = ZERO
    DO i = 1, SfcOptics%n_Angles
      SfcOptics%Reflectivity(i,1,i,1) = ONE-SfcOptics%Emissivity(i,1)
      SfcOptics%Reflectivity(i,2,i,2) = ONE-SfcOptics%Emissivity(i,2)
    END DO

  END FUNCTION Compute_MW_Snow_SfcOptics


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Snow_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at microwave frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO TL
!           COMPONENTS IN THE MW SNOW SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Snow_SfcOptics_TL( SfcOptics_TL )
!
! OUTPUTS:
!       SfcOptics_TL:    Structure containing the tangent-linear surface
!                        optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input.
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Snow_SfcOptics_TL( &
    SfcOptics_TL) &  ! TL  Output
  RESULT ( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type), INTENT(IN OUT) :: SfcOptics_TL
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Snow_SfcOptics_TL'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the tangent-linear surface optical parameters
    ! ***No TL models yet, so default TL output is zero***
    SfcOptics_TL%Reflectivity = ZERO
    SfcOptics_TL%Emissivity   = ZERO

  END FUNCTION Compute_MW_Snow_SfcOptics_TL



!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Compute_MW_Snow_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at microwave frequencies over a snow surface.
!
!       This function is a wrapper for third party code.
!
!       NB: CURRENTLY THIS IS A STUB FUNCTION AS THERE ARE NO AD
!           COMPONENTS IN THE MW SNOW SFCOPTICS COMPUTATIONS.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_MW_Snow_SfcOptics_AD( SfcOptics_AD )
!
! INPUTS:
!       SfcOptics_AD:    Structure containing the adjoint surface optical
!                        properties required for the adjoint radiative
!                        transfer calculation.
!                        *** COMPONENTS MODIFIED UPON OUTPUT ***
!                        UNITS:      N/A
!                        TYPE:       CRTM_SfcOptics_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input adjoint arguments are IN OUT regardless
!       of their specification as "input" or "output". This is because these
!       arguments may contain information on input, or need to be zeroed on
!       output (or both).
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Compute_MW_Snow_SfcOptics_AD( &
    SfcOptics_AD) &  ! AD  Input
  RESULT( err_stat )
    ! Arguments
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_MW_Snow_SfcOptics_AD'
    ! Local variables


    ! Set up
    err_stat = SUCCESS


    ! Compute the adjoint surface optical parameters
    ! ***No AD models yet, so there is no impact on AD result***
    SfcOptics_AD%Reflectivity = ZERO
    SfcOptics_AD%Emissivity   = ZERO

  END FUNCTION Compute_MW_Snow_SfcOptics_AD

!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Populate_Subset
!
! PURPOSE:
!       Repackages brightness temperatures into a subset of channels required by
!       lower NESDIS emissivity routines.
!
! CALLING SEQUENCE:
!       Output_TBs = Populate_Subset( Surface, Num_Output_Channels, Required_Channels )
!
! INPUTS:
!       Surface:         CRTM Surface Structure
!                        UNITS:      N/A
!                        TYPE:       CRTM_Surface_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Num_Output Channels:   The number of channels required
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Required_Channels:   The required channels 
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Num_Output Channels
!                        ATTRIBUTES: INTENT(IN)
! FUNCTION RESULT:
!       Output_TBs:    An array of the required brightness temperatures
!                        UNITS:      K
!                        TYPE:       REAL
!                        DIMENSION:  Num_Output Channels
!
! COMMENTS:
!
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Populate_Subset( &
        Surface, &
        Num_Output_Channels, &
        Required_Channels) &
  RESULT ( Output_TBs )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    INTEGER, INTENT(IN) :: Num_Output_Channels
    INTEGER, INTENT(IN) :: Required_Channels(Num_Output_Channels)
    ! Function Result
    REAL(fp) :: Output_TBs(Num_Output_Channels) 
    ! Local Variables
    INTEGER :: I, J

    Output_TBs(:) = -99.9_fp

    DO I = 1, Num_Output_Channels
      DO J = 1,Surface%SensorData%n_Channels
        IF (Required_Channels(I) == Surface%SensorData%Sensor_Channel(J)) &
            Output_TBs(I) = Surface%SensorData%Tb(J)
      END DO
    END DO

  END FUNCTION Populate_Subset

END MODULE CRTM_MW_Snow_SfcOptics
