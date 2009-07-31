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
  USE Type_Kinds      , ONLY: fp
  USE File_Utility    , ONLY: Get_Lun
  USE Message_Handler , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                              Display_Message
  USE Units_Conversion, ONLY: SA_to_MR, &
                              MR_to_PPMV
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
  '$Id:$'
  ! Message string length
  INTEGER,  PARAMETER :: ML = 512
  ! Literal constants
  REAL(fp), PARAMETER :: THOUSAND = 1000.0_fp
  REAL(fp), PARAMETER :: HUNDRED  = 100.0_fp
  ! The ECMWF data file name
  CHARACTER(*), PARAMETER :: ECMWF_DATA_FILE = './ECMWF5K_Profile_Set/nwp_saf_t_sampled.atm'  
  ! The number of levels, layers, absorbers, and profiles
  INTEGER, PARAMETER :: N_ECMWF5K_LEVELS    = 91
  INTEGER, PARAMETER :: N_ECMWF5K_LAYERS    = N_ECMWF5K_LEVELS - 1
  INTEGER, PARAMETER :: N_ECMWF5K_ABSORBERS = 6
  INTEGER, PARAMETER :: N_ECMWF5K_PROFILES  = 5000

  ! Absorber info
  INTEGER, PARAMETER :: ECMWF_ABSORBER_ID(N_ECMWF5K_ABSORBERS) = (/ 1,2,3,4,5,6 /)
  INTEGER, PARAMETER :: ECMWF_ABSORBER_UNITS_ID(N_ECMWF5K_ABSORBERS) = (/ 3,1,1,1,1,1 /)

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
    INTEGER :: FileID
    INTEGER :: m
    
    REAL(fp) :: ECMWF_ppmv_H2O(N_ECMWF5K_LEVELS)    
    REAL(fp) :: ECMWF_Latitude
    REAL(fp) :: ECMWF_Longitude
    REAL(fp) :: ECMWF_Surface_Pressure
    REAL(fp) :: ECMWF_Surface_Altitude
    INTEGER  :: ECMWF_Year
    INTEGER  :: ECMWF_Month
    INTEGER  :: ECMWF_Day
    INTEGER  :: ECMWF_Hour 
    
    REAL(fp), dimension(N_ECMWF5K_LEVELS) :: temp, hum, ozo, cc, clw, ciw
    REAL(fp), dimension(N_ECMWF5K_LEVELS) :: rain, snow, w, Pressure
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
    ! ------------------------------
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

    ! Open the data file
    FileID = Get_Lun()
    OPEN(FileID,file=ECMWF_DATA_FILE,status='OLD',ACCESS='SEQUENTIAL',FORM='formatted',ACTION='read',IOSTAT=IO_Status)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error opening ",a," file in comment skip. IOSTAT = ",i0)' ) &
                      ECMWF_DATA_FILE, IO_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    
    ! Read through data file
    Profile_Read_Loop: DO m = 1, Profile

      READ(FileID,'(833(e13.6,x),i5,3i3,i7,i5)') &
           temp(:),                  &! 1) Temperature [K]                          (1-91)
           hum(:),                   &! 2) Humidity [kg/kg]                         (92-182)
           ozo(:),                   &! 3) Ozone [kg/kg]                            (183-273)
           cc(:),                    &! 4) Cloud Cover [0-1]                        (274-364)
           clw(:),                   &! 5) C Liquid W [kg/kg]                       (365-455)
           ciw(:),                   &! 6) C Ice W [kg/kg]                          (456-546)
           rain(:),                  &! 7) Rain [kg/(m2 *s)]                        (547-637)
           snow(:),                  &! 8) Snow [kg/(m2 *s)]                        (638-728)
           w(:),                     &! 9) Vertical Velocity [Pa/s]                 (729-819)
           NLog_Surface_Pressure,    &!10) Ln of Surf Pressure in [Pa]              (820)
           z0,                       &!11) Surface geopotential [m2/s2]             (821) 
           tsurf,                    &!12) Surface Skin Temperature [K]             (822)
           t2m,                      &!13) 2m Temperature [K]                       (823)
           td2m,                     &!14) 2m Dew point temperature [K]             (824)
           hum2m,                    &!15) 2m Specific Humidity [kg/kg]             (825)
           u10,                      &!16) 10m wind speed U component [m/s]         (826)
           v10,                      &!17) 10m wind speed V component [m/s]         (827)
           stratrsrf,                &!18) Stratiform rain at surface [kg/(m2 *s)]  (828)
           convrsrf,                 &!19) Convective rain at surface [kg/(m2 *s)]  (829)
           snowsurf,                 &!20) Snow at surface [kg/(m2 *s)]             (830)
           lsm,                      &!21) Land/sea Mask [0-1]                      (831)
           ECMWF_Latitude,           &!22) Latitude [deg]                           (832)
           ECMWF_Longitude,          &!23) Longitude [deg]                          (833)
           ECMWF_Year,               &!24) Year                                     (834)      
           ECMWF_Month,              &!25) Month                                    (835)      
           ECMWF_Day,                &!26) Day                                      (836)      
           step,                     &!27) Step                                     (837)
           Grid_Point,               &!28) Grid point [1-843490]                    (838)
           Ind                        !29) Index (rank-sorted)                      (839)      
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading ",a," file in comment skip. IOSTAT = ",i0)' ) &
                        ECMWF_DATA_FILE, IO_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! compute surface pressure [Pa]
      Surface_Pressure = exp(NLog_Surface_Pressure)
      
      ! compute full-level pressure and half-level pressures [Pa]
      ! note that all profile variables above are given at full levels
      call ec_p91l(Surface_Pressure,Level_Pressure,Half_Level_Pressure)
       
    END DO Profile_Read_Loop
    CLOSE( FileID )
    
    ! convert Level_Pressure from Pa to hPa
    Level_Pressure=Level_Pressure/HUNDRED   
    
    ! Reverse level pressure array
    Level_Pressure=Level_Pressure(N_ECMWF5K_LEVELS:1:-1)
    
    ! Convert specific amounts to g/Kg
    hum = hum(N_ECMWF5K_LEVELS:1:-1)*THOUSAND
    ozo = ozo(N_ECMWF5K_LEVELS:1:-1)*THOUSAND
    
    ! Convert the specific amount
    ! of water vapor to mixing ratio
    Level_Absorber(:,1) = SA_to_MR( hum )
    
    ! Convert the specific amount 
    ! of ozone to mixing ratio
    ozo = SA_to_MR( ozo , &
                    Water_Vapor=Level_Absorber(:,1) )                           
    
    ! Convert ozone as 
    ! mixing ratio to ppmv
    Level_Absorber(:,3) = MR_to_PPMV( ozo,        &
                                      Molecule_ID=3)
    
    ! compute the surface altitude
    Surface_Altitude = z0 / 9.80665
    
    ! Reverse temp and assign to Level_Temperature
    Level_Temperature = temp(N_ECMWF5K_LEVELS:1:-1)
    
    ! Set Climatology model to invalid
    Climatology_Model = 0
    
    ! Set the profile description
    IF ( PRESENT(Description) ) THEN
      Description = ' '
      WRITE( Description,'("ECMWF 5000 profiles on 91L. Profile #",i0)' ) Profile
    END IF
    
    ! Set the absorber id's and absorber units
    IF (PRESENT(Absorber_ID)) Absorber_ID = ECMWF_ABSORBER_ID
    IF (PRESENT(Absorber_Units_ID)) Absorber_Units_ID = ECMWF_ABSORBER_UNITS_ID
    
    ! Set Locations and Times for the profile
    Longitude = ECMWF_Longitude
    Latitude = ECMWF_Latitude
    Year  = ECMWF_Year
    Month = ECMWF_Month
    Day   = ECMWF_Day
    Hour  = 0

  END FUNCTION Load_ECMWF5K_Profile
  
  ! ----------------------------------------
  !
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
       0.000000, &
       2.000040, &
       3.980832, &
       7.387186, &
      12.908319, &
      21.413612, &
      33.952858, &
      51.746601, &
      76.167656, &
     108.715561, &
     150.986023, &
     204.637451, &
     271.356506, &
     352.824493, &
     450.685791, &
     566.519226, &
     701.813354, &
     857.945801, &
    1036.166504, &
    1237.585449, &
    1463.163940, &
    1713.709595, &
    1989.874390, &
    2292.155518, &
    2620.898438, &
    2976.302246, &
    3358.425781, &
    3767.196045, &
    4202.416504, &
    4663.776367, &
    5150.859863, &
    5663.156250, &
    6199.839355, &
    6759.727051, &
    7341.469727, &
    7942.926270, &
    8564.624023, &
    9208.305664, &
    9873.560547, &
   10558.881836, &
   11262.484375, &
   11982.662109, &
   12713.897461, &
   13453.225586, &
   14192.009766, &
   14922.685547, &
   15638.053711, &
   16329.560547, &
   16990.623047, &
   17613.281250, &
   18191.029297, &
   18716.968750, &
   19184.544922, &
   19587.513672, &
   19919.796875, &
   20175.394531, &
   20348.916016, &
   20434.158203, &
   20426.218750, &
   20319.011719, &
   20107.031250, &
   19785.357422, &
   19348.775391, &
   18798.822266, &
   18141.296875, &
   17385.595703, &
   16544.585938, &
   15633.566406, &
   14665.645508, &
   13653.219727, &
   12608.383789, &
   11543.166992, &
   10471.310547, &
    9405.222656, &
    8356.252930, &
    7335.164551, &
    6353.920898, &
    5422.802734, &
    4550.215820, &
    3743.464355, &
    3010.146973, &
    2356.202637, &
    1784.854614, &
    1297.656128, &
     895.193542, &
     576.314148, &
     336.772369, &
     162.043427, &
      54.208336, &
       6.575628, &
       0.003160, &
       0.000000 /

    data bbm / &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000000, &
   0.000014, &
   0.000055, &
   0.000131, &
   0.000279, &
   0.000548, &
   0.001000, &
   0.001701, &
   0.002765, &
   0.004267, &
   0.006322, &
   0.009035, &
   0.012508, &
   0.016860, &
   0.022189, &
   0.028610, &
   0.036227, &
   0.045146, &
   0.055474, &
   0.067316, &
   0.080777, &
   0.095964, &
   0.112979, &
   0.131935, &
   0.152934, &
   0.176091, &
   0.201520, &
   0.229315, &
   0.259554, &
   0.291993, &
   0.326329, &
   0.362203, &
   0.399205, &
   0.436906, &
   0.475016, &
   0.513280, &
   0.551458, &
   0.589317, &
   0.626559, &
   0.662934, &
   0.698224, &
   0.732224, &
   0.764679, &
   0.795385, &
   0.824185, &
   0.850950, &
   0.875518, &
   0.897767, &
   0.917651, &
   0.935157, &
   0.950274, &
   0.963007, &
   0.973466, &
   0.982238, &
   0.989153, &
   0.994204, &
   0.997630, &
   1.000000 /

   do jk=1,nlev+1
     paph(jk)=aam(jk)+bbm(jk)*spres
   end do
   do jk=1,nlev
     pap(jk)=0.5*(paph(jk)+paph(jk+1))
   end do

   return
   end subroutine ec_p91l

END MODULE ECMWF5K_Profile_Set
