!
! ECMWF5K_Profile_Set
!
! Module containing the ECMWF5K atmospheric profile set data 
! definitions and access routines
!
!
! CREATION HISTORY:
!       Written by:     David N. Groff, 29-Jul2009
!                       david.groff@noaa.gov
!
MODULE ECMWF5K_Profile_Set

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE File_Utility         , ONLY: Get_Lun
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message
  USE Fundamental_Constants, ONLY: STANDARD_GRAVITY
  USE Units_Conversion     , ONLY: SA_to_MR, &
                                   MR_to_PPMV
  USE ECMWF5K_Parameters   , ONLY: ECMWF5K_BINARY_FILE, &
                                   ECMWF5K_RECLEN     , &
                                   N_ECMWF5K_LEVELS   , &
                                   N_ECMWF5K_LAYERS   , &
                                   N_ECMWF5K_ABSORBERS, &
                                   N_ECMWF5K_PROFILES , &
                                   ECMWF5K_ABSORBER_ID  , &
                                   ECMWF5K_ABSORBER_UNITS_ID, &
                                   H2O_ID, O3_ID
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: N_ECMWF5K_LEVELS   
  PUBLIC :: N_ECMWF5K_LAYERS   
  PUBLIC :: N_ECMWF5K_ABSORBERS
  PUBLIC :: N_ECMWF5K_PROFILES 
  ! Procedures
  PUBLIC :: Load_ECMWF5K_Profile

  ! ----------
  ! Parameters
  ! ----------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Message string length
  INTEGER,  PARAMETER :: ML = 512
  ! The ECMWF data file name
  CHARACTER(*), PARAMETER :: ECMWF_DATA_FILE = './ECMWF5K_Profile_Set/'//ECMWF5K_BINARY_FILE


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Load_ECMWF5K_Profile
!
! PURPOSE:
!       Function to return the requested profile from the ECMWF5K set of
!       atmospheric profiles.
!
! CALLING SEQUENCE:
!       Error_Status = Load_ECMWF5K_Profile( Profile,                               &  ! Input
!                                            Level_Pressure,                        &  ! Output
!                                            Level_Temperature,                     &  ! Output
!                                            Level_Absorber,                        &  ! Output
!                                            Absorber_ID       = Absorber_ID,       &  ! Optional output
!                                            Absorber_Units_ID = Absorber_Units_ID, &  ! Optional output
!                                            Description       = Description,       &  ! Optional output
!                                            Climatology_Model = Climatology_Model, &  ! Optional output
!                                            Year              = Year,              &  ! Optional output
!                                            Month             = Month,             &  ! Optional output
!                                            Day               = Day,               &  ! Optional output
!                                            Hour              = Hour,              &  ! Optional output
!                                            Latitude          = Latitude,          &  ! Optional output
!                                            Longitude         = Longitude,         &  ! Optional output
!                                            Surface_Altitude  = Surface_Altitude,  &  ! Optional output
!                                            RCS_Id            = RCS_Id,            &  ! Revision control
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Profile:            The requested atmospheric profile number from the
!                           ECMWF set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Level_Pressure:     Pressure profile for the requested
!                           atmospheric profile.
!                           UNITS:      hectoPascals, hPa
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Temperature:  Temperature profile for the requested
!                           atmospheric profile.
!                           UNITS:      Kelvin, K
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Absorber:     Absorber profiles for the requested atmospheric
!                           profile.
!                           UNITS:      Variable. See ABSORBER_UNITS_ID argument
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2, n_levels x n_absorbers
!                           ATTRIBUTES: POINTER
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
! OPTIONAL OUTPUT ARGUMENTS:
!       Absorber_ID:        The list of the HITRAN absorber numbers for the 
!                           molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Absorber_Units_ID:  The list of the absorber units ID numbers for
!                           the molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Description:        Description of the requested profile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Climatology_Model:  Climatology model for the requested profile.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Year:               Year in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Month:              Month in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Day:                Day on which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Hour:               Hour in which the requested profile sonde was launched.
!                           UNITS:      UTC
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Latitude:           Latitude for the requested profile.
!                           UNITS:      Degrees North (-90 -> +90).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Longitude:          Longitude for the requested profile.
!                           UNITS:      Degrees East (0 -> 360).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Surface_Altitude:   Surface altitude for the requested profile.
!                           UNITS:      Metres, m.
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the profile data load was successful.
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Load_ECMWF5K_Profile( Profile,           &  ! Input
                                 Level_Pressure,    &  ! Output           
                                 Level_Temperature, &  ! Output           
                                 Level_Absorber,    &  ! Output           
                                 Absorber_ID,       &  ! Optional output  
                                 Absorber_Units_ID, &  ! Optional output  
                                 Description,       &  ! Optional output  
                                 Climatology_Model, &  ! Optional output  
                                 Year,              &  ! Optional output  
                                 Month,             &  ! Optional output  
                                 Day,               &  ! Optional output  
                                 Hour,              &  ! Optional output  
                                 Latitude,          &  ! Optional output  
                                 Longitude,         &  ! Optional output  
                                 Surface_Altitude,  &  ! Optional output  
                                 RCS_Id,            &  ! Revision control 
                                 Message_Log )      &  ! Error messaging  
                               RESULT( Error_Status )
                               
    ! Arguments
    INTEGER     ,            INTENT(IN)  :: Profile
    REAL(fp)    ,            POINTER     :: Level_Pressure(:)
    REAL(fp)    ,            POINTER     :: Level_Temperature(:)
    REAL(fp)    ,            POINTER     :: Level_Absorber(:,:)
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Absorber_ID(:)
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Absorber_Units_ID(:)
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: Description
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Climatology_Model
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Year
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Month
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Day
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Hour
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Latitude
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Longitude
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Surface_Altitude
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Load_ECMWF5K_Profile'
    
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    INTEGER :: IO_Status
    CHARACTER(ML) :: io_msg
    INTEGER :: FileID
    INTEGER :: m
    INTEGER :: H2O_Idx
    INTEGER :: O3_Idx
    
    REAL(fp) :: ECMWF_Latitude
    REAL(fp) :: ECMWF_Longitude
    INTEGER  :: ECMWF_Year
    INTEGER  :: ECMWF_Month
    INTEGER  :: ECMWF_Day
    
    REAL(fp), DIMENSION(N_ECMWF5K_LEVELS) :: hum, ozo, ozomr, cc, clw, ciw
    REAL(fp), DIMENSION(N_ECMWF5K_LEVELS) :: rain, snow, w
    REAL(fp) :: lsm, t2m, td2m, hum2m, u10, v10
    REAL(fp) :: stratrsrf, convrsrf, snowsurf, NLog_Surface_Pressure, Surface_Pressure
    REAL(fp) :: tsurf, z0
    INTEGER  :: Grid_Point, Ind, Step
    REAL(fp) :: Half_Level_Pressure(N_ECMWF5K_LEVELS+1) 

       
    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID 

    ! Is requested profile valid?
    IF ( Profile < 1 .OR. Profile > N_ECMWF5K_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Invalid model profile number ",i0," specified.")' ) Profile
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Check output pointers
    IF ( ASSOCIATED(Level_Pressure) ) THEN
      DEALLOCATE( Level_Pressure, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Pressure output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( ASSOCIATED(Level_Temperature) ) THEN
      DEALLOCATE( Level_Temperature, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Temperature output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( ASSOCIATED(Level_Absorber) ) THEN
      DEALLOCATE( Level_Absorber, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Absorber output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Check output array sizes
    IF ( PRESENT(Absorber_ID) ) THEN
      IF ( SIZE(Absorber_ID) /= N_ECMWF5K_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Size of output Absorber_ID array must be ",i0," elements.")' ) &
                       N_ECMWF5K_ABSORBERS
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( PRESENT(Absorber_Units_ID) ) THEN
      IF ( SIZE(Absorber_Units_ID) /= N_ECMWF5K_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Size of output Absorber_Units_ID array must be ",i0," elements.")' ) &
                        N_ECMWF5K_ABSORBERS
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF                       


    ! Allocate output pointer arrays
    ALLOCATE( Level_Pressure( N_ECMWF5K_LEVELS ), &
              Level_Temperature( N_ECMWF5K_LEVELS ), &
              Level_Absorber( N_ECMWF5K_LEVELS, N_ECMWF5K_ABSORBERS ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating output arrays. STATs = ",i0)' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Determine the absorber array indices for H2O and O3
    H2O_Idx = Get_Absorber_Index( ECMWF5K_ABSORBER_ID, H2O_ID )
    IF ( H2O_Idx < 0 ) THEN
      Error_Status = FAILURE
      Message = 'No H2O in absorber list'
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF
    O3_Idx = Get_Absorber_Index( ECMWF5K_ABSORBER_ID, O3_ID )
    IF ( O3_Idx < 0 ) THEN
      Error_Status = FAILURE
      Message = 'No O3 in absorber list'
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Open the data file
    FileID = Get_Lun()
    OPEN( FileID, FILE   = ECMWF_DATA_FILE, &
                  STATUS = 'OLD'          , &
                  ACCESS = 'DIRECT'       , &
                  FORM   = 'UNFORMATTED'  , &
                  ACTION = 'READ'         , &
                  RECL   = ECMWF5K_RECLEN , &
                  IOSTAT = io_status      , &
                  IOMSG  = io_msg           )
    IF ( io_status /= 0 ) THEN
      Error_Status = FAILURE
      Message = 'Error opening '//ECMWF_DATA_FILE//' - '//TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status, Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    
    
    ! Read the requested profile
    READ( FileID, REC    = Profile  , &
                  IOSTAT = io_status, &
                  IOMSG  = io_msg   ) &
      Level_Temperature,     &! 1) Temperature [K]                          (1-91)
      hum,                   &! 2) Humidity [kg/kg]                         (92-182)
      ozo,                   &! 3) Ozone [kg/kg]                            (183-273)
      cc,                    &! 4) Cloud Cover [0-1]                        (274-364)
      clw,                   &! 5) C Liquid W [kg/kg]                       (365-455)
      ciw,                   &! 6) C Ice W [kg/kg]                          (456-546)
      rain,                  &! 7) Rain [kg/(m2 *s)]                        (547-637)
      snow,                  &! 8) Snow [kg/(m2 *s)]                        (638-728)
      w,                     &! 9) Vertical Velocity [Pa/s]                 (729-819)
      NLog_Surface_Pressure, &!10) Ln of Surf Pressure in [Pa]              (820)
      z0,                    &!11) Surface geopotential [m2/s2]             (821) 
      tsurf,                 &!12) Surface Skin Temperature [K]             (822)
      t2m,                   &!13) 2m Temperature [K]                       (823)
      td2m,                  &!14) 2m Dew point temperature [K]             (824)
      hum2m,                 &!15) 2m Specific Humidity [kg/kg]             (825)
      u10,                   &!16) 10m wind speed U component [m/s]         (826)
      v10,                   &!17) 10m wind speed V component [m/s]         (827)
      stratrsrf,             &!18) Stratiform rain at surface [kg/(m2 *s)]  (828)
      convrsrf,              &!19) Convective rain at surface [kg/(m2 *s)]  (829)
      snowsurf,              &!20) Snow at surface [kg/(m2 *s)]             (830)
      lsm,                   &!21) Land/sea Mask [0-1]                      (831)
      ECMWF_Latitude,        &!22) Latitude [deg]                           (832)
      ECMWF_Longitude,       &!23) Longitude [deg]                          (833)
      ECMWF_Year,            &!24) Year                                     (834)      
      ECMWF_Month,           &!25) Month                                    (835)      
      ECMWF_Day,             &!26) Day                                      (836)      
      step,                  &!27) Step                                     (837)
      Grid_Point,            &!28) Grid point [1-843490]                    (838)
      Ind                     !29) Index (rank-sorted)                      (839)      
    IF ( io_status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error reading profile # ",i0," - ",a)' ) m, TRIM(io_msg)
      CALL Display_Message( ROUTINE_NAME, TRIM(Message), Error_Status, Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    CLOSE( FileId )


    ! Create pressure profile
    ! ...Compute surface pressure [Pa]
    Surface_Pressure = EXP(NLog_Surface_Pressure)
    ! ...Compute full-level pressure and half-level pressures [Pa]
    call ec_p91l(Surface_Pressure,Level_Pressure,Half_Level_Pressure)


    ! Put all arrays in required order
    CALL Reverse( Level_Pressure )
    CALL Reverse( Level_Temperature )   
    CALL Reverse( hum )
    CALL Reverse( ozo )
    

    ! Perform units conversions
    ! ...Convert Level_Pressure from Pa to hPa
    CALL Convert_Pa_to_hPA( Level_Pressure )
    ! ...Convert specific amounts from g/g to g/Kg
    CALL Convert_GpG_TO_GpKG( hum )
    CALL Convert_GpG_TO_GpKG( ozo )
    ! ...Convert the specific amount of water vapor to mixing ratio
    CALL SA_to_MR( hum, Level_Absorber(:,H2O_Idx) )
    
    ! ...Convert the specific amount of ozone to mixing ratio
    CALL SA_to_MR( ozo, ozomr, Water_Vapor=Level_Absorber(:,H2O_Idx) )                           
    ! ...and now convert ozone from mixing ratio to ppmv
    CALL MR_to_PPMV( ozomr, Level_Absorber(:,O3_Idx), Molecule_ID=O3_ID )
    

    ! Set optional arguments
    IF ( PRESENT(Absorber_ID      ) ) Absorber_ID       = ECMWF5K_ABSORBER_ID
    IF ( PRESENT(Absorber_Units_ID) ) Absorber_Units_ID = ECMWF5K_ABSORBER_UNITS_ID
    IF ( PRESENT(Description      ) ) WRITE( Description,'("ECMWF5K profiles on 91L. Profile #",i0)' ) Profile
    IF ( PRESENT(Climatology_Model) ) Climatology_Model = Get_Climatology( ECMWF_Month, ECMWF_Latitude )
    IF ( PRESENT(Year             ) ) Year  = ECMWF_Year
    IF ( PRESENT(Month            ) ) Month = ECMWF_Month
    IF ( PRESENT(Day              ) ) Day   = ECMWF_Day
    IF ( PRESENT(Hour             ) ) Hour  = 0
    IF ( PRESENT(Latitude         ) ) Longitude        = ECMWF_Longitude
    IF ( PRESENT(Longitude        ) ) Latitude         = ECMWF_Latitude
    IF ( PRESENT(Surface_Altitude ) ) Surface_Altitude = Geopotential_to_Altitude( z0 )

  END FUNCTION Load_ECMWF5K_Profile



  FUNCTION Get_Absorber_Index(Absorber_Id_List, Absorber_Id) RESULT(Absorber_Index)
    INTEGER, INTENT(IN) :: Absorber_Id_List(:)
    INTEGER, INTENT(IN) :: Absorber_Id
    ! Function result
    INTEGER :: Absorber_Index
    ! Local variables
    INTEGER :: j, Idx(1)
    ! Initialise result to "not found"
    Absorber_Index = -1
    ! Return if absorber not present
    IF ( COUNT(Absorber_Id == Absorber_Id_List) /= 1 ) RETURN
    ! Find the location
    Idx = PACK( (/(j,j=1,SIZE(Absorber_Id_List))/), &
                Absorber_Id == Absorber_Id_List )
    Absorber_Index = Idx(1)
  END FUNCTION Get_Absorber_Index


  FUNCTION Get_Climatology( Month, Latitude ) RESULT( Climatology )
    INTEGER , INTENT(IN) :: Month
    REAL(fp), INTENT(IN) :: Latitude
    INTEGER              :: Climatology
    ! Local parameters
    ! ...The climatologies
    INTEGER, PARAMETER :: TROPICAL           = 1
    INTEGER, PARAMETER :: MIDLATITUDE_SUMMER = 2
    INTEGER, PARAMETER :: MIDLATITUDE_WINTER = 3
    INTEGER, PARAMETER :: SUBARCTIC_SUMMER   = 4
    INTEGER, PARAMETER :: SUBARCTIC_WINTER   = 5
    INTEGER, PARAMETER :: US_STD_ATM         = 6
    ! ...The months of the year
    INTEGER, PARAMETER :: JANUARY   =  1
    INTEGER, PARAMETER :: FEBRUARY  =  2
    INTEGER, PARAMETER :: MARCH     =  3
    INTEGER, PARAMETER :: APRIL     =  4
    INTEGER, PARAMETER :: MAY       =  5
    INTEGER, PARAMETER :: JUNE      =  6
    INTEGER, PARAMETER :: JULY      =  7
    INTEGER, PARAMETER :: AUGUST    =  8
    INTEGER, PARAMETER :: SEPTEMBER =  9
    INTEGER, PARAMETER :: OCTOBER   = 10
    INTEGER, PARAMETER :: NOVEMBER  = 11
    INTEGER, PARAMETER :: DECEMBER  = 12
    ! Local variables
    LOGICAL :: Region, Winter, Summer
    
    ! Topical
    IF ( ABS(Latitude) < 30.0_fp ) THEN
      Climatology = TROPICAL
      RETURN
    END IF


    ! Midlatitude - NORTHERN HEMISPHERE
    Region = ( Latitude >= 30.0_fp .AND. Latitude <= 60.0_fp )
    Summer = ( Month >= JUNE     .AND. Month <= AUGUST   )
    Winter = ( Month >= DECEMBER .AND. Month <= FEBRUARY )
    ! ...Northern hemisphere default to US STD ATM for spring/autumn
    IF ( Region ) Climatology = US_STD_ATM
    ! ...SUMMER
    IF ( Region .AND. Summer ) Climatology = MIDLATITUDE_SUMMER
    ! ...WINTER
    IF ( Region .AND. Winter ) Climatology = MIDLATITUDE_WINTER


    ! Midlatitude - SOUTHERN HEMISPHERE
    Region = ( Latitude <= -30.0_fp .AND. Latitude >= -60.0_fp )
    Winter = ( Month >= JUNE     .AND. Month <= AUGUST   )
    Summer = ( Month >= DECEMBER .AND. Month <= FEBRUARY )
    ! ...Southern hemisphere default to US STD ATM for spring/autumn
    IF ( Region ) Climatology = US_STD_ATM
    ! ...SUMMER
    IF ( Region .AND. Summer ) Climatology = MIDLATITUDE_SUMMER
    ! ...WINTER
    IF ( Region .AND. Winter ) Climatology = MIDLATITUDE_WINTER


    ! SUBARCTIC - NORTHERN HEMISPHERE
    Region = ( Latitude > 60.0_fp )
    Summer = ( Month >= MAY .AND. Month <= OCTOBER )
    ! ...Northern hemisphere default to SUBARCTIC_WINTER
    IF ( Region ) Climatology = SUBARCTIC_WINTER
    ! ...SUMMER
    IF ( Region .AND. Summer ) Climatology = SUBARCTIC_SUMMER


    ! SUBARCTIC - SOUTHERN HEMISPHERE
    Region = ( Latitude < -60.0_fp )
    Winter = ( Month >= MAY .AND. Month <= OCTOBER )
    ! ...Southern hemisphere default to SUBARCTIC_SUMMER
    IF ( Region ) Climatology = SUBARCTIC_SUMMER
    ! ...WINTER
    IF ( Region .AND. Winter ) Climatology = SUBARCTIC_WINTER

  END FUNCTION Get_Climatology
  
  
  SUBROUTINE Reverse( array )
    REAL(fp), INTENT(IN OUT) :: array(:)
    INTEGER :: n
    n = SIZE(array)
    array = array(n:1:-1)
  END SUBROUTINE Reverse

  
  SUBROUTINE Convert_Pa_to_hPa( Pressure )
    REAL(fp), INTENT(IN OUT) :: Pressure(:)
    REAL(fp), PARAMETER :: PA_TO_HPA  = 1.0e-02_fp
    Pressure = Pressure * PA_TO_HPA
  END SUBROUTINE Convert_Pa_to_hPa


  SUBROUTINE Convert_GpG_TO_GpKG( Absorber )
    REAL(fp), INTENT(IN OUT) :: Absorber(:)
    REAL(fp), PARAMETER :: GpG_TO_GpKG = 1000.0_fp
    Absorber = Absorber * GpG_TO_GpKG
  END SUBROUTINE Convert_GpG_TO_GpKG

  
  FUNCTION Geopotential_to_Altitude( Geopotential ) RESULT( Altitude )
    REAL(fp), INTENT(IN) :: Geopotential
    REAL(fp)             :: Altitude
    Altitude = Geopotential/STANDARD_GRAVITY
  END FUNCTION Geopotential_to_Altitude
    
  
  SUBROUTINE EC_P91l(spres,pap,paph)
  !
  !    This software was developed within the context of
  !    the EUMETSAT Satellite Application Facility on
  !    Numerical Weather Prediction (NWP SAF), under the
  !    Cooperation Agreement dated 25 November 1998, between
  !    EUMETSAT and the Met Office, UK, by one or more partners
  !    within the NWP SAF. The partners in the NWP SAF are
  !    the Met Office, ECMWF, KNMI and MeteoFrance.
  !
  !    Copyright 2006, EUMETSAT, All Rights Reserved.
  !
  !     Description:
  !     Computes the 91-level vertical pressure grid
  !       associated to the input surface pressure
  !     All pressures are in Pa

    implicit none
    integer, parameter    :: nlev=91
    integer               :: jk
    real(fp)      :: spres
    real(fp)      :: aam(nlev+1), bbm(nlev+1)
    real(fp)      :: pap(nlev), paph(nlev+1)

    data aam / &
       0.000000_fp, &
       2.000040_fp, &
       3.980832_fp, &
       7.387186_fp, &
      12.908319_fp, &
      21.413612_fp, &
      33.952858_fp, &
      51.746601_fp, &
      76.167656_fp, &
     108.715561_fp, &
     150.986023_fp, &
     204.637451_fp, &
     271.356506_fp, &
     352.824493_fp, &
     450.685791_fp, &
     566.519226_fp, &
     701.813354_fp, &
     857.945801_fp, &
    1036.166504_fp, &
    1237.585449_fp, &
    1463.163940_fp, &
    1713.709595_fp, &
    1989.874390_fp, &
    2292.155518_fp, &
    2620.898438_fp, &
    2976.302246_fp, &
    3358.425781_fp, &
    3767.196045_fp, &
    4202.416504_fp, &
    4663.776367_fp, &
    5150.859863_fp, &
    5663.156250_fp, &
    6199.839355_fp, &
    6759.727051_fp, &
    7341.469727_fp, &
    7942.926270_fp, &
    8564.624023_fp, &
    9208.305664_fp, &
    9873.560547_fp, &
   10558.881836_fp, &
   11262.484375_fp, &
   11982.662109_fp, &
   12713.897461_fp, &
   13453.225586_fp, &
   14192.009766_fp, &
   14922.685547_fp, &
   15638.053711_fp, &
   16329.560547_fp, &
   16990.623047_fp, &
   17613.281250_fp, &
   18191.029297_fp, &
   18716.968750_fp, &
   19184.544922_fp, &
   19587.513672_fp, &
   19919.796875_fp, &
   20175.394531_fp, &
   20348.916016_fp, &
   20434.158203_fp, &
   20426.218750_fp, &
   20319.011719_fp, &
   20107.031250_fp, &
   19785.357422_fp, &
   19348.775391_fp, &
   18798.822266_fp, &
   18141.296875_fp, &
   17385.595703_fp, &
   16544.585938_fp, &
   15633.566406_fp, &
   14665.645508_fp, &
   13653.219727_fp, &
   12608.383789_fp, &
   11543.166992_fp, &
   10471.310547_fp, &
    9405.222656_fp, &
    8356.252930_fp, &
    7335.164551_fp, &
    6353.920898_fp, &
    5422.802734_fp, &
    4550.215820_fp, &
    3743.464355_fp, &
    3010.146973_fp, &
    2356.202637_fp, &
    1784.854614_fp, &
    1297.656128_fp, &
     895.193542_fp, &
     576.314148_fp, &
     336.772369_fp, &
     162.043427_fp, &
      54.208336_fp, &
       6.575628_fp, &
       0.003160_fp, &
       0.000000_fp /

    data bbm / &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000000_fp, &
   0.000014_fp, &
   0.000055_fp, &
   0.000131_fp, &
   0.000279_fp, &
   0.000548_fp, &
   0.001000_fp, &
   0.001701_fp, &
   0.002765_fp, &
   0.004267_fp, &
   0.006322_fp, &
   0.009035_fp, &
   0.012508_fp, &
   0.016860_fp, &
   0.022189_fp, &
   0.028610_fp, &
   0.036227_fp, &
   0.045146_fp, &
   0.055474_fp, &
   0.067316_fp, &
   0.080777_fp, &
   0.095964_fp, &
   0.112979_fp, &
   0.131935_fp, &
   0.152934_fp, &
   0.176091_fp, &
   0.201520_fp, &
   0.229315_fp, &
   0.259554_fp, &
   0.291993_fp, &
   0.326329_fp, &
   0.362203_fp, &
   0.399205_fp, &
   0.436906_fp, &
   0.475016_fp, &
   0.513280_fp, &
   0.551458_fp, &
   0.589317_fp, &
   0.626559_fp, &
   0.662934_fp, &
   0.698224_fp, &
   0.732224_fp, &
   0.764679_fp, &
   0.795385_fp, &
   0.824185_fp, &
   0.850950_fp, &
   0.875518_fp, &
   0.897767_fp, &
   0.917651_fp, &
   0.935157_fp, &
   0.950274_fp, &
   0.963007_fp, &
   0.973466_fp, &
   0.982238_fp, &
   0.989153_fp, &
   0.994204_fp, &
   0.997630_fp, &
   1.000000_fp /

   do jk=1,nlev+1
     paph(jk)=aam(jk)+bbm(jk)*spres
   end do
   do jk=1,nlev
     pap(jk)=0.5*(paph(jk)+paph(jk+1))
   end do

   return
   end subroutine ec_p91l

END MODULE ECMWF5K_Profile_Set
