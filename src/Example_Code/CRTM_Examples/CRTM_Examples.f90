!
! CRTM_Examples
!
! Program to provide examples of CRTM function usage for CRTM v1.
!
!    Examples: running Forward(FW), Tangent_Linear(TL), Adjoint(AD) or K-Matrix(KM) 
!              model for one of the following cases,  
!      Ocean, Clear Sky
!      Mixed Surface types  :  Mixed surfaces partially over ocean and partially over land                                                                    
!      Land, single Cloud                                                                  
!      Land, single Aerosol                                                                
!      Land, two Clouds and two Aerosols                                                   
!      Clear sky, two Prifles (one over ocean & the other over land) : Call CRTM with multiple profiles                                                                  
!      Optional Inputs/Outputs : user emissivity input and optical depth profile output           
!
! Inputs: command line parameter inputs for sensor IDs, coefficient directory, model 
!         index and test case index
! Outputs: results are contained in an output file with an internally created
!         filename, tagged with the sesnor ID, model type and test case. 

! CREATION HISTORY:
!       Yong Han, May, 2009
!       Based on Example_Forward.f90 & Example_K_Matrix.f90 written by
!       Paul van Delst, 01-Feb-2008, paul.vandelst@noaa.gov      
!       

PROGRAM CRTM_Examples

!  Running CRTM involves the following steps:
!    (1) Add the line USE CRTM_Module in user's program to include CRTM modules
!    (2) Define CRTM interface structures
!    (3) Initialize CRTM for a set of sensors.(The initialization will load coefficient data)
!    (4) Allocate memory for the interface structure arrays and their pointer members
!    (5) Fill in the interface structures with data
!    (6) Call the FW, TL, AD or KM model
!        Note: the TL, AD or KM model includes the FW model calculation
!    (7) Deallocate memory for the interface structure arrays and their pointer members
!    (8) Repeat steps (4) - (7) over sensors and state profiles. Some of the memory
!        allocations and deallocations may be omitted through reuses of the interface
!        structures if their dimensions are kept the same over the loops.         
!    (9) Destroy CRTM
!
  ! ============================================================================
  ! **** ENVIRONMENT SETUP FOR RTM USAGE ****
  !
  ! Module usage
  USE CRTM_Module
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'CRTM_Examples'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! CRTM Model Type
  INTEGER, PARAMETER      :: N_MODEL_TYPES = 4
  INTEGER, PARAMETER      :: FORWARD = 1, TANGENT_LINEAR = 2, ADJOINT = 3, K_MATRIX = 4
  CHARACTER(*), PARAMETER :: MODEL_DESCRIPTION(N_MODEL_TYPES) = (/'FW', 'TL', 'AD', 'KM'/)

  ! Cases of atmospheric and surface states
  INTEGER, PARAMETER :: N_CASES                 = 7
  INTEGER, PARAMETER :: Ocean_ClearSky          = 1,   &
                        Mixed_Surfaces          = 2,   &
                        Land_1Cloud             = 3,   &
                        Land_1Aerosol           = 4,   &
                        Land_2Cloud_2Aerosol    = 5,   &
                        ClearSky_2Prifles       = 6,   &
                        Optional_Inputs_Outputs = 7
  CHARACTER(23), PARAMETER :: CASE_DESCRIPTION(N_CASES) = (/ &
                       "Ocean_ClearSky         ",   &
                       "Mixed_Surfaces         ",   &
                       "Land_1Cloud            ",   &
                       "Land_1Aerosol          ",   &
                       "Land_2Cloud_2Aerosol   ",   &
                       "ClearSky_2Prifles      ",   &
                       "Optional_Inputs_Outputs"/)
              
  INTEGER, PARAMETER  :: FOUT = 12
  
  ! ---------
  ! Variables
  ! ---------

  ! ============================================================================
  !  **** DEFINE THE CRTM INTERFACE STRUCTURES ****
  !
  TYPE(CRTM_ChannelInfo_type) , DIMENSION(:),   ALLOCATABLE       :: ChannelInfo
  TYPE(CRTM_GeometryInfo_type), DIMENSION(:),   ALLOCATABLE       :: GeoInfo

  ! Define the FORWARD variables, needed for running all models
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:),   ALLOCATABLE       :: Atm
  TYPE(CRTM_Surface_type)     , DIMENSION(:),   ALLOCATABLE       :: Sfc
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE       :: RTSolution

  ! Define the Tangent Linear variables, needed only  for running CRTM Tangent Linear model
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:),   ALLOCATABLE       :: Atm_TL
  TYPE(CRTM_Surface_type)     , DIMENSION(:),   ALLOCATABLE       :: Sfc_TL
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE       :: RTSolution_TL

  ! Define the Adjoint variables, needed only  for running CRTM Adjoint model
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:),   ALLOCATABLE       :: Atm_AD
  TYPE(CRTM_Surface_type)     , DIMENSION(:),   ALLOCATABLE       :: Sfc_AD
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE       :: RTSolution_AD

  ! Define the K-MATRIX variables, needed only for running CRTM K-MATRIX model
  TYPE(CRTM_Atmosphere_type)  , DIMENSION(:,:), ALLOCATABLE       :: Atm_K
  TYPE(CRTM_Surface_type)     , DIMENSION(:,:), ALLOCATABLE       :: Sfc_K
  TYPE(CRTM_RTSolution_type)  , DIMENSION(:,:), ALLOCATABLE       :: RTSolution_K

  ! Define the optional variable, which can be used with any model
  TYPE(CRTM_Options_type)     , DIMENSION(:),   ALLOCATABLE       :: Options(:) 
  ! ============================================================================

  CHARACTER(256) :: Message, Sensor_Info, Coeffdata_dir
  CHARACTER(64)  :: Sensor_Ids(10) 
  INTEGER        :: Error_Status, Error_Status_arr(3)
  INTEGER        :: Allocate_Status, Allocate_Status_arr(3)
  INTEGER        :: n_Channels
  INTEGER        :: i, k, l, m, n, s
  INTEGER        :: Model_Type, Case_ID

  INTEGER        :: n_Profiles
  INTEGER        :: n_Layers, n_Levels
  INTEGER        :: n_Absorbers 
  INTEGER        :: n_Clouds    
  INTEGER        :: n_Aerosols  
  INTEGER        :: n_Sensors

  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to provide examples of CRTM usage.', &
                        '$Revision$' )

  ! Get user parameters

  ! (1) Get sensor id from user
  WRITE( *,'(/5x,"Enter 1 or more sensor ids separated by comma [e.g. hirs4_n18,amsua_n18]: ")' )
  READ( *,'(a)' ) Sensor_Info
  ! (2) Coefficient data directory
  WRITE( *,'(/5x,"Enter coeff data directory[e.g. ../Coefficient_Data/ ]: ")' )
  READ( *,'(a)' ) Coeffdata_dir
  Coeffdata_dir = ADJUSTL(Coeffdata_dir)
  ! (3) CRTM type index
  WRITE( *,'(/5x,"Enter CRTM model type [1 - FW, 2 - TL, 3 - AD, 4 - KM]: ")')
  READ( *,'(i5)' ) Model_Type
  ! (4) Test cases
  WRITE( *,'(/5x,"Enter Test Case ID: ",10(/7x,i3, " - ", a))') &
            (i, TRIM(CASE_DESCRIPTION(i)), i=1, N_CASES)
  READ( *,'(i5)' ) Case_ID

  ! Parse sensor IDs and put them into an array

  n = LEN(TRIM(Sensor_Info))
  l = 1
  n_Sensors = 0
  DO i = 1, n
    IF(Sensor_Info(i:i) == ",")THEN
      n_Sensors = n_Sensors + 1
      Sensor_Ids(n_Sensors) = ADJUSTL(Sensor_Info(l:i-1))
      l = i+1
    END IF
  END DO 
  n_Sensors = n_Sensors + 1
  Sensor_Ids(n_Sensors) = ADJUSTL(Sensor_Info(l:n))

  ! ============================================================================
  !                     **** INITIALIZE CRTM ****
  !
  !     This initializes the CRTM for the sensors 
  !     defined in the example SENSOR_IDS string array.
  ! ============================================================================
 
  ! Allocate array ChannelInfo, before calling the CRTM initialization routine

  ALLOCATE( ChannelInfo(n_Sensors), &
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN 
    Error_Status = FAILURE
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating structure ChannelInfo', & 
                           Error_Status)  
    STOP
  END IF

  ! Call the CRTM initialization routine. If the optional File_Path argument does not present, 
  ! the current directory is assumed for coefficient files.

  WRITE( *,'(/5x,"Initializing the CRTM...")' )
  Error_Status = CRTM_Init( ChannelInfo                  ,     &  ! This is an OUTPUT
                            Sensor_Id=Sensor_Ids(1:n_Sensors), &  ! ID array for 1 or more sensors
                            File_Path=TRIM(Coeffdata_dir)  )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error initializing CRTM', & 
                          Error_Status)  
    STOP
  END IF

  ! ============================================================================================
  !
  !   *** (1) Allocate memory for the interface strucutres Atm and Sfc (contain state variables) 
  !       (2) Assign data for their member variables, given a user selected case ***
  ! 
  ! ============================================================================================

  SELECT CASE( Case_ID )

    CASE( Ocean_ClearSky, Mixed_Surfaces, Optional_Inputs_Outputs )

    ! ***  (1) Clear sky over ocean surface, or
    !      (2) Clear sky partially over ocean and partially over land, or 
    !      (3) Optional Inputs/outputs ***
      
      ! allocate interface structures arrays Atm and Sfc

      n_Profiles  = 1
      n_Layers    = 65
      n_Absorbers = 2    ! H2O & O3
      n_Clouds    = 0
      n_Aerosols  = 0

      ALLOCATE( Atm(n_Profiles), Sfc(n_Profiles), STAT=Allocate_Status ) 
        
      ! Allocate structure array pointer members   
                                                    
      Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, n_Aerosols, Atm )

      IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                       
        Error_Status = FAILURE                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                
                              'Error allocating structure arrays or structure  array pointer members', &                         
                               Error_Status)                                                 
        STOP                                                                                 
      END IF
                                                                                 
      ! Assignment for the Atmosphere structure Atm.

        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_65L(&
                 Atm(1)%Level_Pressure,                &  ! Level Pressure & layer Pressure    
                 Atm(1)%Pressure, Atm(1)%Temperature,  &  ! Layer pressure & layer water vapor 
                 Atm(1)%Absorber(:, 1), Atm(1)%Absorber(:, 2)) ! layer Ozone                   

        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used. 
 
      Atm(1)%Climatology    = TROPICAL 

        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.
                                                                                          
      Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      ! Assignment for the Surface structure Sfc.
   
      IF( Case_ID == Mixed_Surfaces )THEN  ! Mixed surfaces

        ! *** For mixed surfaces over land and ocean, surface coverage must adds up to 1 ***
     
        Sfc(1)%Water_Coverage    = 0.6_fp 
        Sfc(1)%Water_Type        = SEA_WATER                                                                             
        Sfc(1)%Water_Temperature = Atm(1)%Temperature(n_layers) + 0.1_fp ! (K)
        Sfc(1)%Wind_Speed        = 10.0_fp  ! (m/s)
                                                                                
        Sfc(1)%Land_Type        = SCRUB                                                                                   
        Sfc(1)%Land_Temperature = Atm(1)%Temperature(n_layers) - 0.2_fp  ! (K) 
        Sfc(1)%Land_Coverage     = 0.4_fp                                                                                 

      ELSE

        Sfc(1)%Water_Coverage    = 1.0_fp 
        Sfc(1)%Water_Type        = SEA_WATER                                                                              
        Sfc(1)%Water_Temperature = Atm(1)%Temperature(n_layers) + 0.1_fp ! (K)
        Sfc(1)%Wind_Speed        = 10.0_fp  ! (m/s)
       
      END IF                                              

    CASE( Land_1Cloud )

    ! ***  Single Cloud over land ***

      ! allocate interface structures arrays Atm and Sfc

      n_Profiles  = 1
      n_Layers    = 100
      n_Absorbers = 2    ! H2O & O3
      n_Clouds    = 1
      n_Aerosols  = 0

      ! allocate interface structures arrays Atm and Sfc

      ALLOCATE( Atm(n_Profiles), Sfc(n_Profiles), STAT=Allocate_Status ) 
             
      ! Allocate structure array pointer members
                                                 
      Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, n_Aerosols, Atm )

      IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                       
        Error_Status = FAILURE                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                
                              'Error allocating structure arrays or structure  array pointer members', &                         
                               Error_Status)                                                 
        STOP                                                                                 
      END IF

      ! Assignment for the Atmosphere structure Atm.
                                                                                 
        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_100L( &
                    Atm(1)%Level_Pressure,                &  ! Level Pressure & layer Pressure   
                    Atm(1)%Pressure, Atm(1)%Temperature,  &  ! Layer pressure & layer water vapor
                    Atm(1)%Absorber(:, 1), Atm(1)%Absorber(:, 2)) ! layer Ozone                      


        ! Get cloud profile and type (see definition of this routine for units)
 
      CALL Get_Cloud_1( Atm(1)%Cloud(1) )

        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used. 

      Atm(1)%Climatology    = US_STANDARD_ATMOSPHERE  
                                                      
        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.

      Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      ! Assignment for the Surface structure Sfc.

      Sfc(1)%Land_Coverage    = 1.0_fp                                                                                  
      Sfc(1)%Land_Type        = SAND                                                                                   
      Sfc(1)%Land_Temperature = Atm(1)%Temperature(n_layers) + 0.5_fp  ! (K)                                                
  
    CASE( Land_1Aerosol )

    ! ***  Single Aerosol over land ***

      ! allocate interface structures arrays Atm and Sfc

      n_Profiles  = 1
      n_Layers    = 100
      n_Absorbers = 2    ! H2O & O3
      n_Clouds    = 0
      n_Aerosols  = 1

        ! allocate interface structures arrays Atm and Sfc

      ALLOCATE( Atm(n_Profiles), Sfc(n_Profiles), STAT=Allocate_Status )

        ! Allocate structure array pointer members  
                                                     
      Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, n_Aerosols, Atm )

      IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                       
        Error_Status = FAILURE                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                
                              'Error allocating structure arrays or structure  array pointer members', &                         
                               Error_Status)                                                 
        STOP                                                                                 
      END IF
                                                                                 
      ! Assignment for the Atmosphere structure Atm.
                                                                                 
        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_100L( &
                   Atm(1)%Level_Pressure,                &  ! Level Pressure & layer Pressure   
                   Atm(1)%Pressure, Atm(1)%Temperature,  &  ! Layer pressure & layer water vapor
                   Atm(1)%Absorber(:, 1), Atm(1)%Absorber(:, 2)) ! layer Ozone                  

        ! Get aerosol profile and type (see definition of this routine for units)
 
      CALL Get_Aerosol_1( Atm(1)%Aerosol(1) )

        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used.
 
      Atm(1)%Climatology    = US_STANDARD_ATMOSPHERE  

        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.

      Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      ! Assignment for the Surface structure Sfc.

      Sfc(1)%Land_Coverage    = 1.0_fp                                                                                  
      Sfc(1)%Land_Type        = SAND                                                                                   
      Sfc(1)%Land_Temperature = Atm(1)%Temperature(n_layers) + 0.5_fp  ! (K)                                                

    CASE( Land_2Cloud_2Aerosol )
 
    ! ***  Two clouds and two aerosols over land ***

      ! allocate interface structures arrays Atm and Sfc

      n_Profiles  = 1
      n_Layers    = 100
      n_Absorbers = 2    ! H2O & O3
      n_Clouds    = 2
      n_Aerosols  = 2

      ! allocate interface structures arrays Atm and Sfc

      ALLOCATE( Atm(n_Profiles), Sfc(n_Profiles), STAT=Allocate_Status )              
                                                                       
      ! Allocate structure array pointer members  
                                                    
      Error_Status_arr(1) = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, n_Aerosols, Atm )
      IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                       
        Error_Status = FAILURE                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                
                              'Error allocating structure arrays or structure  array pointer members', &                         
                               Error_Status)                                                 
        STOP                                                                                 
      END IF
                                                                                 
        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_100L( &
                    Atm(1)%Level_Pressure,                &  ! Level Pressure & layer Pressure   
                    Atm(1)%Pressure, Atm(1)%Temperature,  &  ! Layer pressure & layer water vapor
                    Atm(1)%Absorber(:, 1), Atm(1)%Absorber(:, 2)) ! layer Ozone                  

        ! Get cloud and aerosol profiles and types (see definition of this routine for units) 

      CALL Get_Cloud_1(Atm(1)%Cloud(1))
      CALL Get_Aerosol_1(Atm(1)%Aerosol(1))
      CALL Get_Cloud_2(Atm(1)%Cloud(2))
      CALL Get_Aerosol_2(Atm(1)%Aerosol(2))

        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used.
 
      Atm(1)%Climatology    = US_STANDARD_ATMOSPHERE  
                                                      
        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.

      Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      ! Assignment for the Surface structure Sfc.

      Sfc(1)%Land_Coverage    = 1.0_fp                                                                                  
      Sfc(1)%Land_Type        = SAND                                                                                   
      Sfc(1)%Land_Temperature = Atm(1)%Temperature(n_layers) + 0.5_fp  ! (K)                                                

    CASE( ClearSky_2Prifles )

    ! ***  Two profiles, both clear-sky, one over ocean and the other over land ***
    ! ***  The same results can also be obtained if the user call CRTM twice, each only passing in 
    !      one profile.

      ! allocate interface structures arrays Atm and Sfc

      n_Profiles  = 2
      n_Layers    = 100
      n_Absorbers = 2    ! H2O & O3
      n_Clouds    = 0
      n_Aerosols  = 0

      ! allocate interface structures arrays Atm and Sfc

      ALLOCATE( Atm(n_Profiles), Sfc(n_Profiles), STAT=Allocate_Status )

      ! Allocate structure array pointer members  

      Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, n_Aerosols, Atm )

      IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                
                              'Error allocating structure arrays or structure  array pointer members', &                         
                               Error_Status)                                                 
        STOP                                                                                 
      END IF

      ! *** Profile 1  
                                                                               
        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_100L( &
                  Atm(1)%Level_Pressure,                &  ! Level Pressure & layer Pressure   
                  Atm(1)%Pressure, Atm(1)%Temperature,  &  ! Layer pressure & layer water vapor
                  Atm(1)%Absorber(:, 1), Atm(1)%Absorber(:, 2)) ! layer Ozone                  
  
        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used.

      Atm(1)%Climatology    = US_STANDARD_ATMOSPHERE  
                                                      
        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.

      Atm(1)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(1)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      Sfc(1)%Water_Coverage    = 1.0_fp                                                                                 
      Sfc(1)%Water_Type        = SEA_WATER                                                                              
      Sfc(1)%Water_Temperature = Atm(1)%Temperature(n_layers) + 0.1_fp  ! (K)

      ! *** Profile 2 
                                                                                
        ! Get pressure(mb), temperature(K), H2O (mass mixing ratio g/kg) and O3 (ppmv) profiles.
        ! See this routine for level and layer definition and array indexing

      CALL Get_p_t_absorbers_100L( &
                 Atm(2)%Level_Pressure, Atm(2)%Pressure,    &  ! Level Pressure and layer Pressure       
                 Atm(2)%Temperature, Atm(2)%Absorber(:, 1), &  ! Layer Temperature and layer water vapor 
                                     Atm(2)%Absorber(:, 2))    ! layer Ozone                             
  

        ! Flag used for extrapolating user profile to the CRTM top level. 
        ! If not set, the defaut will be used.

      Atm(2)%Climatology    = US_STANDARD_ATMOSPHERE  
                                                      
        ! Absorber IDs and units assignment, for the current CRTM these assigments should be fixed.

      Atm(2)%Absorber_Id    = (/ H2O_ID                 , O3_ID /)                                                      
      Atm(2)%Absorber_Units = (/ MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS /)                                  

      ! Assignment for the Surface structure Sfc.

      Sfc(2)%Land_Coverage    = 1.0_fp                                                                                  
      Sfc(2)%Land_Type        = SAND                                                                                   
      Sfc(2)%Land_Temperature = Atm(1)%Temperature(n_layers) + 0.5_fp  ! (K)                                                

    CASE DEFAULT
      Error_Status = FAILURE                                                                 
      WRITE( Message,'("Error can not find a match for the input case #: ",i0)' ) Case_id    
      CALL Display_Message( PROGRAM_NAME, &                                                    
                            TRIM(Message), &                                                   
                            Error_Status)                                                      
      STOP
  END SELECT

  Sensor_Loop: DO s = 1, n_Sensors

    ! print a message on screen
    WRITE( *,'(/5x,"*** Sensor ID: ",a, "; Test Case: ",a, "; Model Type: ",a, " ***")' )&
     TRIM(Sensor_Ids(s)), TRIM(CASE_DESCRIPTION(Case_ID)), TRIM(MODEL_DESCRIPTION(Model_Type))

    ! =====================================================================================
    !
    !    *** (1) Allocate memory for interface structures RTSolution and GeoInfo ***
    !
    !    *** (2) Allocate memory for the optional interface structure Options (if selected) 
    !            and the optional RTSolution pointer variables that hold additional
    !            model results (if selected) *** 
    !
    !    *** (3) Set the content of the Options structure if the user has chosen the options
    !
    ! =====================================================================================

    ! Allocate memory for the RTSolution & GeoInfo arrays (needed for all models)

    n_Channels = ChannelInfo(s)%n_Channels                                   
    ALLOCATE(RTSolution( n_Channels, n_Profiles ), GeoInfo(n_Profiles), &
             STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN                                                         
      Error_Status = FAILURE                                                                   
      CALL Display_Message( PROGRAM_NAME, &                                                    
               'Error allocating the RTSolution and GeoInfo arrays', &    
                Error_Status)                                                               
      STOP                                                                                     
    END IF                                                                                  


    ! Set GeoInfo input (for simplicity, the same seetings are used for all profiles        
    ! and sensors)                                                                                                    

    GeoInfo(:)%Sensor_Zenith_Angle = 30.0_fp  ! zensor zenith agnle at the surface (degree)                               

    ! The scan angle needed only for MW sensors with variable polarization (e.g. AMSUA).     
    ! If not set, it will be calculated internally for a default satellite height.           

    GeoInfo(:)%Sensor_Scan_Angle   = 26.37293341421_fp  ! (degree)  

    ! Solar zenith angle. Set this variable (0 <= Source_Zenith_Angle < 90) to include
    ! the solar radiation. If the following assignment is omitted, a default value 100.0
    ! will be internally assigned, which signals no solar component will be in the calculation.

    GeoInfo(:)%Source_Zenith_Angle  = 100.0_fp ! (degree)                        

    ! (1) Allocate memery for the Options structure, if the user choose to pass data such 
    !     as the surface emissivity to CRTM through the Options structure.
    ! (2) Allocate memory for the RTsolution pointer members, if the user choose to obtain 
    !     additional results such as the optical depth profile. (The allocation will be
    !     used to signal the output of the additional results.)   
    ! (3) Set the user surface emissivity

    IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                     

      ALLOCATE(Options(n_Profiles), STAT=Allocate_Status ) 
      Error_Status_arr(1) = CRTM_Allocate_RTSolution( n_Layers, RTSolution ) 
      Error_Status_arr(2) = CRTM_Allocate_Options( n_Channels, Options)
      IF ( Allocate_Status /= 0 .OR. ANY(Error_Status_arr(1:2) /= SUCCESS) ) THEN
        Error_Status = FAILURE                                                                 
        CALL Display_Message( PROGRAM_NAME, &                                                  
                 'Error allocating the RTSolution or Options structures', &  
                  Error_Status)                                                             
        STOP                                                                                   
      END IF                                                                                

      ! Set the user emissivity through the options structure. 

      DO m = 1, n_Profiles
        Options(m)%Emissivity(:)     = 0.98_fp
        Options(m)%Emissivity_Switch = SET
      END DO

    END IF

    ! =====================================================================================
    !
    !    *** (1) Allocate memory for model dependent interace structures ***
    !
    !    *** (2) Call CRTM FW, TL, AD or KM model ***                          
    !      Note: The same settings completed in the previous steps for the state variables Atm 
    !            and Sfc as well as the variable Geoinfo are applied below to all sensors.
    !            In reality, different sensors may require different settings.
    !
    !    *** (3) Print out results ***
    !
    !    *** (4) Decallocate memory for model dependent interface structures
    ! =====================================================================================

    ! Open output file
    OPEN(FOUT, FILE = TRIM(Sensor_Ids(s))//"."//TRIM(CASE_DESCRIPTION(Case_ID))//"."//&
                      MODEL_DESCRIPTION(Model_Type)//".result", STATUS = 'REPLACE')

    SELECT CASE (Model_Type)
      CASE( FORWARD )

        IF ( Case_ID /= Optional_Inputs_Outputs ) THEN                                    
          Error_Status = CRTM_Forward( Atm              , &                               
                                       Sfc              , &                               
                                       GeoInfo          , &                               
                                       ChannelInfo(s:s) , &                               
                                       RTSolution )                                       
        ELSE                                                                              
          Error_Status = CRTM_Forward( Atm              , &                               
                                       Sfc              , &                               
                                       GeoInfo          , &                               
                                       ChannelInfo(s:s) , &                               
                                       RTSolution  , &                                    
                                       Options = Options )                                
        END IF                                                                            
        IF ( Error_Status /= SUCCESS ) THEN                                               
          CALL Display_Message( PROGRAM_NAME, &                                           
                                'Error in CRTM Forward Model', &                          
                                Error_Status)                                             
          STOP                                                                            
        END IF                                                                            

        ! Print out FW results 
                                                        
        CALL Print_FW_Result(FOUT, ChannelInfo(s), RTSolution)                            

        ! Print out optical depth and layer upwelling radiance if uer has selected them.
  
        IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                    
          CALL  Print_Option_Result(FOUT, ChannelInfo(s), Atm, RTSolution)                
        END IF                                                                            

      CASE( TANGENT_LINEAR )

        ! Allocate memory for interface structures specific to the model

        ALLOCATE( Atm_TL( n_Profiles ), &
                  Sfc_TL( n_Profiles ), &
                  RTSolution_TL( n_Channels, n_Profiles ), &
                  STAT = Allocate_Status )

        Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, &
                                                 n_Aerosols, Atm_TL )

        IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                     
          Error_Status = FAILURE                                                               
          CALL Display_Message( PROGRAM_NAME, &                                                
                   'Error allocating structure arrays or structure array pointer members', &
                    Error_Status)                                                           
          STOP                                                                                 
        END IF                                                                              

        ! Initialize all the increments variables contained in Atm_TL & Sfc_TL to zero. 
                                           
        CALL CRTM_Zero_Atmosphere( Atm_TL )                                               
        CALL CRTM_Zero_Surface( Sfc_TL )                                                  

        ! Set the increments
        ! here only set a few increment variables, as a demonstration
                        
        DO m = 1, n_Profiles                                                              
          Atm_TL(m)%Temperature = 0.5_fp  ! (K)
          Sfc_TL(m)%Wind_Speed  = 5.0_fp  ! (m/s)                                           
        END DO                                                                            

        ! call the TL model                                                               
        IF ( Case_ID /= Optional_Inputs_Outputs ) THEN                                    
          Error_Status = CRTM_Tangent_Linear( Atm              , &                                           
                                              Sfc              , &                        
                                              Atm_TL           , &                        
                                              Sfc_TL           , &                                 
                                              GeoInfo          , &                                    
                                              ChannelInfo(s:s) , &                                         
                                              RTSolution       , &                             
                                              RTSolution_TL )                             
        ELSE                                                                              
          Error_Status = CRTM_Tangent_Linear( Atm               , &                                           
                                              Sfc               , &                       
                                              Atm_TL            , &                       
                                              Sfc_TL            , &                                 
                                              GeoInfo           , &                                    
                                              ChannelInfo(s:s)  , &                                         
                                              RTSolution        , &                       
                                              RTSolution_TL     , &                       
                                              Options = Options )                         
        END IF                                                                            

        IF ( Error_Status /= SUCCESS ) THEN                                               
          CALL Display_Message( PROGRAM_NAME, &                                           
                                'Error in CRTM Tangent Linear Model', &                   
                                Error_Status)                                             
          STOP                                                                            
        END IF                                                                            

        ! Print out FW part of results   
                                                 
        CALL Print_FW_Result(FOUT, ChannelInfo(s), RTSolution)                            

        ! Print out TL part of results  
                                                  
        CALL Print_TL_Result(FOUT, ChannelInfo(s), RTSolution_TL)                         

        ! Print out optical depth and layer upwelling radiance if uer has selected them.  

        IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                    
          CALL  Print_Option_Result(FOUT, ChannelInfo(s), Atm, RTSolution)                
        END IF                                                                            

        ! Deallocate memory

        Error_Status = CRTM_Destroy_Atmosphere(Atm_TL)
        DEALLOCATE(RTSolution_TL, Sfc_TL, Atm_TL,STAT = Allocate_Status)                                     
                                                                      
      CASE( ADJOINT )

        ! Allocate memory for interface structures specific to the model

        ALLOCATE( Atm_AD( n_Profiles ), &
                  Sfc_AD( n_Profiles ), &
                  RTSolution_AD( n_Channels, n_Profiles ), &
                  STAT = Allocate_Status )

        Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, &
                                                 n_Aerosols, Atm_AD )

        IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS ) THEN                                                     
          Error_Status = FAILURE                                                               
          CALL Display_Message( PROGRAM_NAME, &                                                
                   'Error allocating structure arrays or structure array pointer members', &
                    Error_Status)                                                           
          STOP                                                                                 
        END IF                                                                              

        ! Here, initialize the AD structure variables to zero (user may set them to
        ! other values, such as those in the AD chain.  
                       
        CALL CRTM_Zero_Atmosphere( Atm_AD )                                             
        CALL CRTM_Zero_Surface( Sfc_AD )                                                

        ! Inintialize the AD INPUT so that all the results are Adjoint to the                                           
        ! brightness temperature. To have Adjoint to the Radiance, set          
        !      RTSolution_AD%radiance = ONE             
        !      RTSolution_AD%Brightness_Temperature = ZERO                               
                                        
        RTSolution_AD%Brightness_Temperature = ONE                                      

        ! Call the AD model                                                             
        IF ( Case_ID /= Optional_Inputs_Outputs ) THEN                                  
          Error_Status = CRTM_Adjoint( Atm               , &                                           
                                       Sfc               , &                            
                                       RTSolution_AD     , &                            
                                       GeoInfo           , &                                           
                                       ChannelInfo(s:s)  , &                                                
                                       Atm_AD            , &                            
                                       Sfc_AD            , &                                        
                                       RTSolution )                                     
        ELSE                                                                            
          Error_Status = CRTM_Adjoint( Atm               , &                                           
                                       Sfc               , &                            
                                       RTSolution_AD     , &                            
                                       GeoInfo           , &                                           
                                       ChannelInfo(s:s)  , &                                                
                                       Atm_AD            , &                            
                                       Sfc_AD            , &                                        
                                       RTSolution        , &                            
                                       Options = Options )                              
        END IF                                                                          

        IF ( Error_Status /= SUCCESS ) THEN                                             
          CALL Display_Message( PROGRAM_NAME, &                                         
                                'Error in CRTM Adjoint Model', &                        
                                Error_Status)                                           
          STOP                                                                          
        END IF                                                                          

        ! Print out FW part of results  
                                                
        CALL Print_FW_Result(FOUT, ChannelInfo(s), RTSolution)                          

        ! Print out AD part of results 
                                                 
        CALL Print_AD_Result(FOUT, ChannelInfo(s), Atm, Sfc_AD, Atm_AD)                 

        ! Print out optical depth and layer upwelling radiance if uer has selected them.

        IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                  
          CALL  Print_Option_Result(FOUT, ChannelInfo(s), Atm, RTSolution)              
        END IF                                                                          

        ! Deallocate memory

        Error_Status = CRTM_Destroy_Atmosphere(Atm_AD)
        DEALLOCATE(RTSolution_AD, Sfc_AD, Atm_AD, STAT = Allocate_Status)                                           
                                                                     
      CASE( K_MATRIX )

        ! Allocate memory for structure arrays and their pointer members specific to the model

        ALLOCATE( Atm_K( n_Channels, n_Profiles ), &
                  Sfc_K( n_Channels, n_Profiles ), &
                  RTSolution_K( n_Channels, n_Profiles ), &
                  STAT = Allocate_Status )
        IF ( Allocate_Status /= 0 ) THEN                                                       
          Error_Status = FAILURE                                                               
          CALL Display_Message( PROGRAM_NAME, &                                                
                                'Error allocating structure arrays Atm_K, Sfc_K & RTSolution_K', &                         
                                 Error_Status)                                                 
          STOP                                                                                 
        END IF

        DO m = 1, n_Profiles
          Error_Status = CRTM_Allocate_Atmosphere( n_Layers, n_Absorbers, n_Clouds, &
                                                   n_Aerosols, Atm_K(:,m) )
          IF ( Error_Status /= SUCCESS ) THEN                                                   
            Error_Status = FAILURE                                                              
            CALL Display_Message( PROGRAM_NAME, &                                               
                                  'Error allocating structure Atm_K pointer members', &                     
                                   Error_Status)                                                
            STOP                                                                                
          END IF
        END DO

        ! Zero the K-matrix OUTPUT structures                      

        CALL CRTM_Zero_Atmosphere( Atm_K )                         
        CALL CRTM_Zero_Surface( Sfc_K )                            

        ! Inintialize the K-matrix INPUT so that all the results are dTb/dx                       
        ! To have dR/dx, set RTSolution_K%radiance = ONE           
        ! and RTSolution_K%Brightness_Temperature = ZERO           

        RTSolution_K%Brightness_Temperature = ONE                  

        ! Call KM model                                            
        IF ( Case_ID /= Optional_Inputs_Outputs ) THEN             
          Error_Status = CRTM_K_Matrix( Atm              , &       
                                        Sfc              , &       
                                        RTSolution_K     , &       
                                        GeoInfo          , &       
                                        ChannelInfo(s:s) , &            
                                        Atm_K            , &       
                                        Sfc_K            , &       
                                        RTSolution    )            
        ELSE                                                       
          Error_Status = CRTM_K_Matrix( Atm              , &       
                                        Sfc              , &       
                                        RTSolution_K     , &       
                                        GeoInfo          , &       
                                        ChannelInfo(s:s) , &            
                                        Atm_K            , &       
                                        Sfc_K            , &       
                                        RTSolution       , &       
                                        Options = Options )        
        END IF                                                     
        IF ( Error_Status /= SUCCESS ) THEN                        
          CALL Display_Message( PROGRAM_NAME, &                    
                                'Error in CRTM K_Matrix Model', &  
                                Error_Status)                      
          STOP                                                     
        END IF                                                     

        ! Print out FW part of results 
                                                   
        CALL Print_FW_Result(FOUT, ChannelInfo(s), RTSolution)                            

        ! Print out KM part of results
                                                    
        CALL Print_KM_Result(FOUT, ChannelInfo(s), Atm, Sfc_K, Atm_K)                     

        ! Print out optical depth and layer upwelling radiance if uer has selected them. 
 
        IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                    
          CALL  Print_Option_Result(FOUT, ChannelInfo(s), Atm, RTSolution)                
        END IF                                                                            

        ! Deallocate memory

        Error_Status = CRTM_Destroy_Atmosphere(Atm_K)
        DEALLOCATE(RTSolution_K, Sfc_K, Atm_K, STAT = Allocate_Status)                                          
                                                                      
      CASE DEFAULT                                                                        
        Error_Status = FAILURE                                                               
        WRITE( Message,'("Error can not find a match for Model ID #: ",i0)' ) Model_Type   
        CALL Display_Message( PROGRAM_NAME, &                                                  
                              TRIM(Message), &                                                 
                              Error_Status)                                                    
        STOP                                                                              

    END SELECT

    ! Check the memory deallocation status

    IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS) THEN                       
      Error_Status = FAILURE                                                                   
      CALL Display_Message( PROGRAM_NAME, 'Error deallocating TL, AD or KM structure arrays', &                        
              Error_Status)                                                                  
      STOP                                                                                     
    END IF                                                                                   

    CLOSE( FOUT )

    ! Deallocate memory for sensor dependent interface structures
     
    Error_Status_arr = SUCCESS
    Allocate_Status_arr  = 0
    IF ( Case_ID == Optional_Inputs_Outputs ) THEN                                             
      Error_Status_arr(1) = CRTM_Destroy_Options(Options)                                      
      Error_Status_arr(2) = CRTM_Destroy_RTSolution(RTSolution)                                
      DEALLOCATE(Options, STAT = Allocate_Status_arr(1))                                              
    END IF                                                                                     
    DEALLOCATE(RTSolution, GeoInfo, STAT = Allocate_Status_arr(2))                                     

    IF ( ANY(Allocate_Status_arr(1:2) /= 0) .OR. ANY(Error_Status_arr(1:2) /= SUCCESS)) THEN                 
      Error_Status = FAILURE                                                                     
      CALL Display_Message( PROGRAM_NAME, &                                                      
              'Error deallocating structures RTSolution, GeoInfo and Options', &    
              Error_Status)                                                                    
      STOP                                                                                       
    END IF                                                                                     


  END DO Sensor_Loop

  ! Deallocate memory for the sensor/model independent interface structures

  Error_Status = CRTM_Destroy_Atmosphere(Atm)                                                  
  DEALLOCATE(Atm, Sfc, STAT = Allocate_Status)                                              
  IF ( Allocate_Status /= 0 .OR. Error_Status /= SUCCESS) THEN                                 
    Error_Status = FAILURE                                                                       
    CALL Display_Message( PROGRAM_NAME, 'Error deallocating structure Atm and Sfc.', &                          
            Error_Status)                                                                      
    STOP                                                                                         
  END IF                                                                                       

  !  **** DESTROY THE CRTM ****
  !
  WRITE( *, '( /5x, "Destroying the CRTM..." )' )
  Error_Status = CRTM_Destroy( ChannelInfo )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                          Error_Status )
  END IF

CONTAINS

  SUBROUTINE Print_FW_Result(fid, ChannelInfo, RTSolution)
    INTEGER,                     INTENT( IN )  :: fid
    TYPE(CRTM_ChannelInfo_type), INTENT( IN )  :: ChannelInfo
    TYPE(CRTM_RTSolution_type),  INTENT( IN )  :: RTSolution(:,:)

    ! Local
    INTEGER :: l, m, n_Profiles
    
    n_Channels = ChannelInfo%n_Channels
    n_Profiles = SIZE(RTSolution, DIM=2)

    DO m = 1, n_Profiles                                                                    

      WRITE( fid,'(/7x,"Profile ",i0," FWD output",&                                       
                &/7x,"Sensor_Id",3x,"Channel",2x,"R(mW/(m^2.sr.cm^-1))",2x,"Tb(K)")' ) m                        
      DO l = 1, n_Channels                                                                  
        WRITE( fid,'(7x,a9,5x,i5,5x,es13.6,5x,f7.3)') TRIM(ChannelInfo%Sensor_ID), &             
                                                    ChannelInfo%Sensor_Channel(l), &        
                                                    RTSolution(l,m)%Radiance, &             
                                                    RTSolution(l,m)%Brightness_Temperature  
      END DO                                                                                
    END DO                                                                                  

  END SUBROUTINE Print_FW_Result 

  SUBROUTINE Print_TL_Result(fid, ChannelInfo, RTSolution_TL)
    INTEGER,                     INTENT( IN )  :: fid
    TYPE(CRTM_ChannelInfo_type), INTENT( IN )  :: ChannelInfo
    TYPE(CRTM_RTSolution_type),  INTENT( IN )  :: RTSolution_TL(:,:)

    ! Local
    INTEGER :: l, m, n_Profiles
    
    n_Channels = ChannelInfo%n_Channels
    n_Profiles = SIZE(RTSolution_TL, DIM=2)

    DO m = 1, n_Profiles                                                                       

      WRITE( FOUT,'(/7x,"Profile ",i0," Tangent Linear output",&                               
                &/7x,"Sensor_Id",3x,"Channel",1x,"dR(mW/(m^2.sr.cm^-1))",2x,"dTb(K)")' ) m     
      DO l = 1, n_Channels                                                                     
        WRITE( FOUT,'(7x,a9,5x,i5,5x,es13.6,5x,f7.3)') ChannelInfo%Sensor_ID, &    
                                                    ChannelInfo%Sensor_Channel(l), &           
                                                    RTSolution_TL(l,m)%Radiance, &             
                                                    RTSolution_TL(l,m)%Brightness_Temperature  
      END DO                                                                                   
    END DO                                                                                     
  END SUBROUTINE Print_TL_Result

  SUBROUTINE Print_AD_Result(fid, ChannelInfo, Atm, Sfc_AD, Atm_AD)
    INTEGER,                     INTENT( IN )  :: fid
    TYPE(CRTM_ChannelInfo_type), INTENT( IN )  :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  INTENT( IN )  :: Atm(:)
    TYPE(CRTM_Surface_type),     INTENT( IN )  :: Sfc_AD(:)
    TYPE(CRTM_Atmosphere_type),  INTENT( IN )  :: Atm_AD(:)
    ! Local
    INTEGER :: l, k, m, n, n_Profiles
    REAL(fp)       :: data_out(100)
    CHARACTER(256) :: str_out
    
    n_Channels = ChannelInfo%n_Channels
    n_Profiles = SIZE(Atm)

    ! -------------------------------------------------------------------                         
    ! Here only output a few AD variables as a demonstration.                                     

    ! Sfc AD output                                                                               
    DO m = 1, n_Profiles                                                                          
      WRITE( FOUT,'(/7x,"Profile ",i0," Surface AD output",&                                    
                  &/7x,"Sensor_Id",3x,"*dTb/dTland",2x,"*dTb/dTwater")' ) m
      WRITE( FOUT,'(7x,a9,5x,f7.3,6x,f7.3)') ChannelInfo%Sensor_ID, &                       
                                                Sfc_AD(m)%Land_Temperature, &                     
                                                Sfc_AD(m)%Water_Temperature                       
    END DO                                                                                        
 
    ! Atm AD output                                                                               
    DO m = 1, n_Profiles                                                                          
      WRITE( FOUT, '(/7x,"Profile ",i0," ATM AD output for ",a, ", n_Layers = ", i0)') &                
               m, TRIM(ChannelInfo%Sensor_ID), Atm(m)%n_Layers
      str_out = " Pressure(mb)  *dTb/dT      *dTb/dH2O       *dTb/dO3"                            

      DO i = 1, n_Clouds                                                                          
        str_out = TRIM(str_out)//"    *dTb/dRe_cl    *dTb/dCon_cl"                                 
      END DO                                                                                      
      DO i = 1, n_Aerosols                                                                        
        str_out = TRIM(str_out)//"    *dTb/dRe_ae    *dTb/dCon_ae"                                 
      END DO                                                                                      
      WRITE( FOUT,'(/7x, a)')TRIM(str_out)                                                        

   
      DO k = 1, Atm(m)%n_Layers                                                                   
        data_out(1:4) = (/Atm(m)%Pressure(k), Atm_AD(m)%Temperature(k), &                         
                          Atm_AD(m)%Absorber(k,1), Atm_AD(m)%Absorber(k,2)/)                      
        n = 4                                                                                     
        DO i = 1, n_Clouds                                                                        
          n = n + 1                                                                               
          data_out(n) = Atm_AD(m)%Cloud(i)%Effective_Radius(k)                                    
          n = n + 1                                                                               
          data_out(n) = Atm_AD(m)%Cloud(i)%Water_Content(k)                                       
        END DO                                                                                    
        DO i = 1, n_Aerosols                                                                      
          n = n + 1                                                                               
          data_out(n) = Atm_AD(m)%Aerosol(i)%Effective_Radius(k)                                  
          n = n + 1                                                                               
          data_out(n) = Atm_AD(m)%Aerosol(i)%Concentration(k)                                     
        END DO                                                                                    
   
        WRITE( FOUT,'(7x,f8.3,2x,es13.6,2x,es13.6,2x,es13.6, 10(2x, es13.6))' ) data_out(1:n)     
      END DO 
    END DO
                                                                                     
  END SUBROUTINE Print_AD_Result

  SUBROUTINE Print_KM_Result(fid, ChannelInfo, Atm, Sfc_K, Atm_K)
    INTEGER,                     INTENT( IN )  :: fid
    TYPE(CRTM_ChannelInfo_type), INTENT( IN )  :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  INTENT( IN )  :: Atm(:)
    TYPE(CRTM_Surface_type),     INTENT( IN )  :: Sfc_K(:,:)
    TYPE(CRTM_Atmosphere_type),  INTENT( IN )  :: Atm_K(:,:)
    ! Local
    INTEGER :: l, k, m, n, n_Profiles
    REAL(fp)       :: data_out(100)
    CHARACTER(256) :: str_out
    
    n_Channels = ChannelInfo%n_Channels
    n_Profiles = SIZE(Atm)

    ! Sfc K-Matrix output                                                                       
    ! -----------------------                                                                   
    DO m = 1, n_Profiles                                                                        
      WRITE( FOUT,'(/7x,"Profile ",i0," SFC K-Matrix output",&                                
                &/7x,"Sensor_Id",3x,"Channel",2x,"dTb/dTland",2x,"dTb/dTwater")' ) m            
      DO l = 1, n_Channels                                                                      
        WRITE( FOUT,'(7x,a9,5x,i2,5x,f7.3,5x,f7.3)') ChannelInfo%Sensor_ID, &       
                                                  ChannelInfo%Sensor_Channel(l), &  
                                                  Sfc_K(l,m)%Land_Temperature, &                
                                                  Sfc_K(l,m)%Water_Temperature                  
      END DO                                                                                    
    END DO                                                                                      
  
    ! Atm K-Matrix output                                                                       
    ! -----------------------                                                                   
    DO m = 1, n_Profiles                                                                        
      DO l = 1, n_Channels                                                                      
                                                                                                
        WRITE( FOUT, '(/7x,"Profile ",i0," ATM K-Matrix output for ",a," channel ",i0, &
                       &", n_Layers = ", i0)') m, & 
                 TRIM(ChannelInfo%Sensor_ID), &                                     
                 ChannelInfo%Sensor_Channel(l), &
                 Atm(m)%n_Layers                                     
        str_out = " Pressure(mb)  dTb/dT       dTb/dH2O         dTb/dO3"                        

        DO i = 1, n_Clouds                                                                      
          str_out = TRIM(str_out)//"     dTb/dRe_cl    dTb/dCon_cl"                             
        END DO                                                                                  
        DO i = 1, n_Aerosols                                                                    
          str_out = TRIM(str_out)//"     dTb/dRe_ae    dTb/dCon_ae"                             
        END DO                                                                                  
        WRITE( FOUT,'(/7x, a)')TRIM(str_out)                                                    
   
        DO k = 1, Atm(m)%n_Layers                                                               
          data_out(1:4) = (/Atm(m)%Pressure(k), Atm_K(l,m)%Temperature(k), &                    
                            Atm_K(l,m)%Absorber(k,1), Atm_K(l,m)%Absorber(k,2)/)                
          n = 4                                                                                 
          DO i = 1, n_Clouds                                                                    
            n = n + 1                                                                           
            data_out(n) = Atm_K(l,m)%Cloud(i)%Effective_Radius(k)                               
            n = n + 1                                                                           
            data_out(n) = Atm_K(l,m)%Cloud(i)%Water_Content(k)                                  
          END DO                                                                                
          DO i = 1, n_Aerosols                                                                  
            n = n + 1                                                                           
            data_out(n) = Atm_K(l,m)%Aerosol(i)%Effective_Radius(k)                             
            n = n + 1                                                                           
            data_out(n) = Atm_K(l,m)%Aerosol(i)%Concentration(k)                                
          END DO                                                                                
   
          WRITE( FOUT,'(7x,f8.3,2x,es13.6,2x,es13.6,2x,es13.6, 10(2x, es13.6))' )data_out(1:n)  
        END DO                                                                                  
      END DO                                                                                    
    END DO                                                                                      

  END SUBROUTINE Print_KM_Result

  SUBROUTINE Print_Option_Result(fid, ChannelInfo, Atm, RTSolution)
    INTEGER,                     INTENT( IN )  :: fid
    TYPE(CRTM_ChannelInfo_type), INTENT( IN )  :: ChannelInfo
    TYPE(CRTM_Atmosphere_type),  INTENT( IN )  :: Atm(:)
    TYPE(CRTM_RTSolution_type),  INTENT( IN )  :: RTSolution(:,:)
    ! Local
    INTEGER  :: l, k, m, n_Profiles
    REAL(fp) :: data_out(100)
    
    n_Channels = SIZE(RTSolution, DIM=1)
    n_Profiles = SIZE(RTSolution, DIM=2)

    ! Optical depth and layer upwelling radiance outputs                                                    
    ! -----------------------                                                                               
    DO m = 1, n_Profiles                                                                                    
      DO l = 1, n_Channels                                                                                  
                                                                                                            
        WRITE( fid, '(/7x,"Profile ",i0," Optical Depth & Upwelling Radiance for ",a,&
                    &" channel ",i0, ", n_Layers = ", i0)') m, TRIM(ChannelInfo%Sensor_ID),& 
                     ChannelInfo%Sensor_Channel(l), Atm(m)%n_Layers

        WRITE( fid,'(/6x, a, i0)')" Pressure(mb)    OD      UR(mW/(m^2.sr.cm^-1)"
   
        DO k = 1, Atm(m)%n_Layers                                                                           
          data_out(1:3) = (/Atm(m)%Pressure(k), RTSolution(l, m)%Layer_Optical_Depth(k), &                  
                            RTSolution(l, m)%Upwelling_Radiance(k)/)                                        
          WRITE( fid,'(7x,f8.3,2x,es13.6,2x,es13.6)' ) data_out(1:3)                                        
        END DO                                                                                              
      END DO                                                                                                
    END DO                                                                                                  
  END SUBROUTINE Print_Option_Result

  SUBROUTINE Get_p_t_absorbers_100L(Level_Pressure, Layer_Pressure, Layer_T, Layer_W, Layer_O3)

    REAL(fp), INTENT( OUT ) ::    Level_Pressure(0:),&  ! level pressure  0:n_Layers   (mb)
                                  Layer_Pressure(:), &  ! layer mean pressure  1:n_layers (mb)
                                  Layer_T(:),        &  ! layer mean temperature 1:n_layers (K)
                                  Layer_W(:),        &  ! Layer mean water vapor mxing ratio (g/kg)
                                  Layer_O3(:)           ! layer mean ozone 1:n_layers (ppmv)
    ! Local
    INTEGER :: n_Layers

    n_Layers = SIZE(Layer_Pressure)

      Level_Pressure(0: ) = &                                                                                 
      (/0.005_fp,   0.016_fp,   0.038_fp,   0.077_fp,   0.137_fp,   0.224_fp,   0.345_fp,   0.506_fp, &  
        0.714_fp,   0.975_fp,   1.297_fp,   1.687_fp,   2.153_fp,   2.701_fp,   3.340_fp,   4.077_fp, &  
        4.920_fp,   5.878_fp,   6.957_fp,   8.165_fp,   9.512_fp,  11.004_fp,  12.649_fp,  14.456_fp, &  
       16.432_fp,  18.585_fp,  20.922_fp,  23.453_fp,  26.183_fp,  29.121_fp,  32.274_fp,  35.650_fp, &  
       39.257_fp,  43.100_fp,  47.188_fp,  51.528_fp,  56.126_fp,  60.990_fp,  66.125_fp,  71.540_fp, &  
       77.240_fp,  83.231_fp,  89.520_fp,  96.114_fp, 103.017_fp, 110.237_fp, 117.777_fp, 125.646_fp, &  
      133.846_fp, 142.385_fp, 151.266_fp, 160.496_fp, 170.078_fp, 180.018_fp, 190.320_fp, 200.989_fp, &  
      212.028_fp, 223.441_fp, 235.234_fp, 247.409_fp, 259.969_fp, 272.919_fp, 286.262_fp, 300.000_fp, &  
      314.137_fp, 328.675_fp, 343.618_fp, 358.967_fp, 374.724_fp, 390.893_fp, 407.474_fp, 424.470_fp, &  
      441.882_fp, 459.712_fp, 477.961_fp, 496.630_fp, 515.720_fp, 535.232_fp, 555.167_fp, 575.525_fp, &  
      596.306_fp, 617.511_fp, 639.140_fp, 661.192_fp, 683.667_fp, 706.565_fp, 729.886_fp, 753.627_fp, &  
      777.790_fp, 802.371_fp, 827.371_fp, 852.788_fp, 878.620_fp, 904.866_fp, 931.524_fp, 958.591_fp, &  
      986.067_fp,1013.948_fp,1042.232_fp,1070.917_fp,1100.000_fp/)                                       
  
    Layer_T = &
    (/175.859_fp, 182.237_fp, 203.251_fp, 222.895_fp, 233.669_fp, 239.987_fp, 248.220_fp, 255.085_fp, &
      256.186_fp, 252.608_fp, 247.762_fp, 243.314_fp, 239.018_fp, 235.282_fp, 233.777_fp, 234.909_fp, &
      237.889_fp, 241.238_fp, 243.194_fp, 243.304_fp, 242.977_fp, 243.133_fp, 242.920_fp, 242.026_fp, &
      240.695_fp, 239.379_fp, 238.252_fp, 236.928_fp, 235.452_fp, 234.561_fp, 234.192_fp, 233.774_fp, &
      233.305_fp, 233.053_fp, 233.103_fp, 233.307_fp, 233.702_fp, 234.219_fp, 234.959_fp, 235.940_fp, &
      236.744_fp, 237.155_fp, 237.374_fp, 238.244_fp, 239.736_fp, 240.672_fp, 240.688_fp, 240.318_fp, &
      239.888_fp, 239.411_fp, 238.512_fp, 237.048_fp, 235.388_fp, 233.551_fp, 231.620_fp, 230.418_fp, &
      229.927_fp, 229.511_fp, 229.197_fp, 228.947_fp, 228.772_fp, 228.649_fp, 228.567_fp, 228.517_fp, &
      228.614_fp, 228.861_fp, 229.376_fp, 230.223_fp, 231.291_fp, 232.591_fp, 234.013_fp, 235.508_fp, &
      237.041_fp, 238.589_fp, 240.165_fp, 241.781_fp, 243.399_fp, 244.985_fp, 246.495_fp, 247.918_fp, &
      249.073_fp, 250.026_fp, 251.113_fp, 252.321_fp, 253.550_fp, 254.741_fp, 256.089_fp, 257.692_fp, &
      259.358_fp, 261.010_fp, 262.779_fp, 264.702_fp, 266.711_fp, 268.863_fp, 271.103_fp, 272.793_fp, &
      273.356_fp, 273.356_fp, 273.356_fp, 273.356_fp/)

    Layer_W = &
    (/1.612E-03_fp,2.746E-03_fp,3.688E-03_fp,3.914E-03_fp,3.940E-03_fp,4.837E-03_fp,5.271E-03_fp,4.548E-03_fp, &
      4.187E-03_fp,4.401E-03_fp,4.250E-03_fp,3.688E-03_fp,3.516E-03_fp,3.739E-03_fp,3.694E-03_fp,3.449E-03_fp, &
      3.228E-03_fp,3.212E-03_fp,3.245E-03_fp,3.067E-03_fp,2.886E-03_fp,2.796E-03_fp,2.704E-03_fp,2.617E-03_fp, &
      2.568E-03_fp,2.536E-03_fp,2.506E-03_fp,2.468E-03_fp,2.427E-03_fp,2.438E-03_fp,2.493E-03_fp,2.543E-03_fp, &
      2.586E-03_fp,2.632E-03_fp,2.681E-03_fp,2.703E-03_fp,2.636E-03_fp,2.512E-03_fp,2.453E-03_fp,2.463E-03_fp, &
      2.480E-03_fp,2.499E-03_fp,2.526E-03_fp,2.881E-03_fp,3.547E-03_fp,4.023E-03_fp,4.188E-03_fp,4.223E-03_fp, &
      4.252E-03_fp,4.275E-03_fp,4.105E-03_fp,3.675E-03_fp,3.196E-03_fp,2.753E-03_fp,2.338E-03_fp,2.347E-03_fp, &
      2.768E-03_fp,3.299E-03_fp,3.988E-03_fp,4.531E-03_fp,4.625E-03_fp,4.488E-03_fp,4.493E-03_fp,4.614E-03_fp, &
      7.523E-03_fp,1.329E-02_fp,2.468E-02_fp,4.302E-02_fp,6.688E-02_fp,9.692E-02_fp,1.318E-01_fp,1.714E-01_fp, &
      2.149E-01_fp,2.622E-01_fp,3.145E-01_fp,3.726E-01_fp,4.351E-01_fp,5.002E-01_fp,5.719E-01_fp,6.507E-01_fp, &
      7.110E-01_fp,7.552E-01_fp,8.127E-01_fp,8.854E-01_fp,9.663E-01_fp,1.050E+00_fp,1.162E+00_fp,1.316E+00_fp, &
      1.494E+00_fp,1.690E+00_fp,1.931E+00_fp,2.226E+00_fp,2.574E+00_fp,2.939E+00_fp,3.187E+00_fp,3.331E+00_fp, &
      3.352E+00_fp,3.260E+00_fp,3.172E+00_fp,3.087E+00_fp/)

    Layer_O3 = &
    (/3.513E-01_fp,4.097E-01_fp,5.161E-01_fp,7.225E-01_fp,1.016E+00_fp,1.354E+00_fp,1.767E+00_fp,2.301E+00_fp, &
      3.035E+00_fp,3.943E+00_fp,4.889E+00_fp,5.812E+00_fp,6.654E+00_fp,7.308E+00_fp,7.660E+00_fp,7.745E+00_fp, &
      7.696E+00_fp,7.573E+00_fp,7.413E+00_fp,7.246E+00_fp,7.097E+00_fp,6.959E+00_fp,6.797E+00_fp,6.593E+00_fp, &
      6.359E+00_fp,6.110E+00_fp,5.860E+00_fp,5.573E+00_fp,5.253E+00_fp,4.937E+00_fp,4.625E+00_fp,4.308E+00_fp, &
      3.986E+00_fp,3.642E+00_fp,3.261E+00_fp,2.874E+00_fp,2.486E+00_fp,2.102E+00_fp,1.755E+00_fp,1.450E+00_fp, &
      1.208E+00_fp,1.087E+00_fp,1.030E+00_fp,1.005E+00_fp,1.010E+00_fp,1.028E+00_fp,1.068E+00_fp,1.109E+00_fp, &
      1.108E+00_fp,1.071E+00_fp,9.928E-01_fp,8.595E-01_fp,7.155E-01_fp,5.778E-01_fp,4.452E-01_fp,3.372E-01_fp, &
      2.532E-01_fp,1.833E-01_fp,1.328E-01_fp,9.394E-02_fp,6.803E-02_fp,5.152E-02_fp,4.569E-02_fp,4.855E-02_fp, &
      5.461E-02_fp,6.398E-02_fp,7.205E-02_fp,7.839E-02_fp,8.256E-02_fp,8.401E-02_fp,8.412E-02_fp,8.353E-02_fp, &
      8.269E-02_fp,8.196E-02_fp,8.103E-02_fp,7.963E-02_fp,7.741E-02_fp,7.425E-02_fp,7.067E-02_fp,6.702E-02_fp, &
      6.368E-02_fp,6.070E-02_fp,5.778E-02_fp,5.481E-02_fp,5.181E-02_fp,4.920E-02_fp,4.700E-02_fp,4.478E-02_fp, &
      4.207E-02_fp,3.771E-02_fp,3.012E-02_fp,1.941E-02_fp,9.076E-03_fp,2.980E-03_fp,5.117E-03_fp,1.160E-02_fp, &
      1.428E-02_fp,1.428E-02_fp,1.428E-02_fp,1.428E-02_fp/)

    ! This is one of the ways to compute layer mean pressure from the level pressure    
    Layer_Pressure = ( Level_Pressure(1:N_LAYERS) - Level_Pressure(0:N_LAYERS-1) ) / &  
                      LOG( Level_Pressure(1:N_LAYERS) / Level_Pressure(0:N_LAYERS-1) )

  END SUBROUTINE Get_p_t_absorbers_100L

  SUBROUTINE Get_p_t_absorbers_65L(Level_Pressure, Layer_Pressure, Layer_T, Layer_W, Layer_O3)

    REAL(fp), INTENT( OUT ) ::    Level_Pressure(0:),&  ! level pressure  0:n_Layers   (mb)
                                  Layer_Pressure(:), &  ! layer mean pressure  1:n_layers (mb)
                                  Layer_T(:),        &  ! layer mean temperature 1:n_layers (K)
                                  Layer_W(:),        &  ! Layer mean water vapor mxing ratio (g/kg)
                                  Layer_O3(:)           ! layer mean ozone 1:n_layers (ppmv)
    ! Local
    INTEGER :: n_Layers

    n_Layers = SIZE(Layer_Pressure)

      Level_Pressure(0: ) = &
        (/0.005_fp,    0.100_fp,    0.267_fp,    0.994_fp,    1.787_fp,    2.691_fp,    3.724_fp,    4.905_fp,&
          6.255_fp,    7.797_fp,    9.560_fp,   11.572_fp,   13.869_fp,   16.490_fp,   19.477_fp,   22.879_fp,&
         26.750_fp,   31.151_fp,   36.149_fp,   41.817_fp,   48.235_fp,   55.489_fp,   63.674_fp,   72.890_fp,&
         83.242_fp,   94.841_fp,  107.801_fp,  122.236_fp,  138.252_fp,  155.950_fp,  175.415_fp,  196.717_fp,&
        219.899_fp,  244.972_fp,  271.916_fp,  300.664_fp,  331.107_fp,  363.089_fp,  396.408_fp,  430.820_fp,&
        466.045_fp,  501.772_fp,  537.676_fp,  573.421_fp,  608.683_fp,  643.149_fp,  676.540_fp,  708.610_fp,&
        739.156_fp,  768.019_fp,  795.093_fp,  820.306_fp,  843.636_fp,  865.091_fp,  884.718_fp,  902.568_fp,&
        918.737_fp,  933.322_fp,  946.426_fp,  958.157_fp,  968.631_fp,  977.940_fp,  986.234_fp,  993.568_fp,&
       1000.065_fp, 1001.000_fp/)

    Layer_T = &
       (/185.230_fp,  243.084_fp,  262.150_fp,  266.332_fp,  259.465_fp,  253.393_fp,  248.497_fp,  244.372_fp,&
         240.765_fp,  237.517_fp,  234.536_fp,  231.778_fp,  229.208_fp,  226.741_fp,  224.324_fp,  221.979_fp,&
         219.746_fp,  217.582_fp,  215.222_fp,  212.260_fp,  208.861_fp,  205.468_fp,  202.208_fp,  199.088_fp,&
         196.411_fp,  195.944_fp,  198.674_fp,  203.128_fp,  207.937_fp,  212.872_fp,  217.831_fp,  222.632_fp,&
         227.372_fp,  232.183_fp,  236.990_fp,  241.633_fp,  246.133_fp,  250.522_fp,  254.777_fp,  258.859_fp,&
         262.741_fp,  266.446_fp,  269.966_fp,  273.281_fp,  276.382_fp,  279.267_fp,  281.913_fp,  283.994_fp,&
         285.467_fp,  286.698_fp,  287.987_fp,  289.408_fp,  290.775_fp,  292.005_fp,  293.102_fp,  294.086_fp,&
         294.967_fp,  295.750_fp,  296.442_fp,  297.053_fp,  297.592_fp,  298.066_fp,  298.484_fp,  298.851_fp,&
         299.047_fp/)

    Layer_W = &
       (/1.256e-03_fp,3.456e-03_fp,3.705e-03_fp,3.600e-03_fp,3.403e-03_fp,3.214e-03_fp,3.047e-03_fp,2.905e-03_fp,&
         2.780e-03_fp,2.668e-03_fp,2.564e-03_fp,2.457e-03_fp,2.343e-03_fp,2.233e-03_fp,2.135e-03_fp,2.051e-03_fp,&
         1.974e-03_fp,1.863e-03_fp,1.758e-03_fp,1.689e-03_fp,1.636e-03_fp,1.619e-03_fp,1.642e-03_fp,1.703e-03_fp,&
         1.773e-03_fp,1.831e-03_fp,2.034e-03_fp,2.541e-03_fp,3.416e-03_fp,4.786e-03_fp,8.828e-03_fp,1.808e-02_fp,&
         3.422e-02_fp,6.897e-02_fp,1.305e-01_fp,2.165e-01_fp,3.384e-01_fp,5.013e-01_fp,6.992e-01_fp,9.517e-01_fp,&
         1.261e+00_fp,1.636e+00_fp,2.033e+00_fp,2.384e+00_fp,2.824e+00_fp,3.637e+00_fp,4.680e+00_fp,5.854e+00_fp,&
         7.201e+00_fp,8.491e+00_fp,9.526e+00_fp,1.027e+01_fp,1.086e+01_fp,1.139e+01_fp,1.187e+01_fp,1.239e+01_fp,&
         1.297e+01_fp,1.349e+01_fp,1.395e+01_fp,1.436e+01_fp,1.472e+01_fp,1.504e+01_fp,1.532e+01_fp,1.556e+01_fp,&
         1.569e+01_fp/)                                                                                          

    Layer_O3 = &
       (/3.701e-01_fp,9.020e-01_fp,2.183e+00_fp,4.067e+00_fp,5.945e+00_fp,7.582e+00_fp,8.723e+00_fp,9.429e+00_fp,&
         9.751e+00_fp,9.743e+00_fp,9.511e+00_fp,9.063e+00_fp,8.423e+00_fp,7.617e+00_fp,6.653e+00_fp,5.628e+00_fp,&
         4.604e+00_fp,3.643e+00_fp,2.772e+00_fp,2.078e+00_fp,1.624e+00_fp,1.259e+00_fp,8.916e-01_fp,5.662e-01_fp,&
         3.358e-01_fp,2.061e-01_fp,1.483e-01_fp,1.269e-01_fp,1.128e-01_fp,1.007e-01_fp,9.069e-02_fp,8.059e-02_fp,&
         7.120e-02_fp,6.314e-02_fp,5.664e-02_fp,5.179e-02_fp,4.800e-02_fp,4.504e-02_fp,4.306e-02_fp,4.158e-02_fp,&
         4.021e-02_fp,3.895e-02_fp,3.780e-02_fp,3.675e-02_fp,3.590e-02_fp,3.542e-02_fp,3.518e-02_fp,3.483e-02_fp,&
         3.432e-02_fp,3.383e-02_fp,3.335e-02_fp,3.287e-02_fp,3.244e-02_fp,3.204e-02_fp,3.169e-02_fp,3.131e-02_fp,&
         3.091e-02_fp,3.054e-02_fp,3.022e-02_fp,2.993e-02_fp,2.968e-02_fp,2.946e-02_fp,2.926e-02_fp,2.909e-02_fp,&
         2.900e-02_fp/)

    ! This is one of the ways to compute layer mean pressure from the level pressure    
    Layer_Pressure = ( Level_Pressure(1:N_LAYERS) - Level_Pressure(0:N_LAYERS-1) ) / &  
                      LOG( Level_Pressure(1:N_LAYERS) / Level_Pressure(0:N_LAYERS-1) )

  END SUBROUTINE Get_p_t_absorbers_65L

  ! Cloud profile 1: WATER_CLOUD 
  ! This is only a demonstration showing how to set cloud profile.  The values in the setting 
  ! may not reflect the reality.
  !
  SUBROUTINE Get_Cloud_1(Cloud)
    TYPE(CRTM_Cloud_type), INTENT ( IN OUT ) :: Cloud

    ! A water cloud located between layers 83 and 87, inclusive.
    Cloud%Type = WATER_CLOUD
    Cloud%Effective_Radius = &  ! units = microns
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,  20.0_fp,  20.0_fp,  20.0_fp,  20.0_fp,  20.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   
    Cloud%Water_Content = & ! units = kg/m^2
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   

  END SUBROUTINE Get_Cloud_1

  ! Cloud profile 2 : ICE_CLOUD
  ! This is only a demonstration showing how to set cloud profile.  The values in the setting 
  ! may not reflect the reality.
  !
  SUBROUTINE Get_Cloud_2(Cloud)
    TYPE(CRTM_Cloud_type), INTENT ( IN OUT ) :: Cloud

    ! A ice cloud located between layers 66 and 67, inclusive.
    Cloud%Type = ICE_CLOUD
    Cloud%Effective_Radius = &  ! units = microns
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,  42.0_fp,  42.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   
    Cloud%Water_Content = & ! units = kg/m^2
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.1_fp,   0.1_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   

  END SUBROUTINE Get_Cloud_2

  ! Cloud profile 3 : RAIN_CLOUD
  ! This is only a demonstration showing how to set cloud profile.  The values in the setting 
  ! may not reflect the reality.
  !
  SUBROUTINE Get_Cloud_3(Cloud)
    TYPE(CRTM_Cloud_type), INTENT ( IN OUT ) :: Cloud

    ! A water cloud located between layers 81 and 98, inclusive.
    Cloud%Type = RAIN_CLOUD
    Cloud%Effective_Radius = &  ! units = microns
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
   1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp, &  
   1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,1000.0_fp,   0.0_fp,   0.0_fp/)   
    Cloud%Water_Content = & ! units = kg/m^2
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp, &  
      5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   5.0_fp,   0.0_fp,   0.0_fp/)   

  END SUBROUTINE Get_Cloud_3

  ! Aerosol profile 1 : DUST_AEROSOL
  ! This is only a demonstration showing how to set the Aerosol profile.  The values in the setting 
  ! may not reflect the reality.
  !
  SUBROUTINE Get_Aerosol_1(Aerosol)
    TYPE(CRTM_Aerosol_type), INTENT ( IN OUT ) :: Aerosol

    ! A dust aerosol layer located between layers 91 and 93, inclusive.
    Aerosol%Type = DUST_AEROSOL
    Aerosol%Effective_Radius = &  ! units = microns
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      2.0_fp,   2.0_fp,   2.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   
    Aerosol%Concentration = & ! units = kg/m^2
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      5.0_fp,   5.0_fp,   5.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   


  END SUBROUTINE Get_Aerosol_1

  ! Aerosol profile 2 : DRY_ORGANIC_CARBON_AEROSOL
  ! This is only a demonstration showing how to set the Aerosol profile.  The values in the setting 
  ! may not reflect the reality.
  !
  SUBROUTINE Get_Aerosol_2(Aerosol)
    TYPE(CRTM_Aerosol_type), INTENT ( IN OUT ) :: Aerosol

    ! Two DRY_ORGANIC_CARBON_AEROSOL aerosol layers located between layers 56 and 63 and between 86 and 94, 
    ! respectively.
    Aerosol%Type = DRY_ORGANIC_CARBON_AEROSOL
    Aerosol%Effective_Radius = &  ! units = microns
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, 0.009_fp, 0.009_fp, 0.009_fp, 0.009_fp, 0.009_fp, &  
    0.009_fp, 0.009_fp, 0.009_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,  0.15_fp,  0.15_fp,  0.15_fp,  0.15_fp,  0.15_fp, &  
     0.15_fp,  0.15_fp,  0.15_fp,  0.15_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   
    Aerosol%Concentration = & ! units = kg/m^2
    (/0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, 0.003_fp, 0.003_fp, 0.003_fp, 0.003_fp, 0.003_fp, &  
    0.003_fp, 0.003_fp, 0.003_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp, &  
      0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,  0.06_fp,  0.06_fp,  0.06_fp,  0.06_fp,  0.06_fp, &  
     0.06_fp,  0.06_fp,  0.06_fp,  0.06_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp,   0.0_fp/)   


  END SUBROUTINE Get_Aerosol_2


END PROGRAM CRTM_Examples
