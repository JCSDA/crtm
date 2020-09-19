PROGRAM ECMWF5K_ASC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE ECMWF5K_Parameters
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'ECMWF5K_ASC2BIN'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg  
  INTEGER :: ASC_FileId
  INTEGER :: BIN_FileId
  INTEGER :: io_status
  CHARACTER(256) :: io_msg
  INTEGER :: m
  
  REAL(fp), DIMENSION(N_ECMWF5K_LEVELS) :: temp, hum, ozo, cc, clw, ciw
  REAL(fp), DIMENSION(N_ECMWF5K_LEVELS) :: rain, snow, w
  REAL(fp) :: lsm, t2m, td2m, hum2m, u10, v10
  REAL(fp) :: stratrsrf, convrsrf, snowsurf, NLog_Surface_Pressure
  REAL(fp) :: tsurf, z0
  INTEGER  :: Grid_Point, Ind, Step
  REAL(fp) :: ECMWF_Latitude
  REAL(fp) :: ECMWF_Longitude
  INTEGER  :: ECMWF_Year
  INTEGER  :: ECMWF_Month
  INTEGER  :: ECMWF_Day


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert ECMWF5K data file from ASCII to '//&
                        'direct access unformatted format.', &
                        '$Revision$' )
                        
  
  ! Open the input ASCII data file
  ASC_FileID = Get_Lun()
  OPEN( ASC_FileID, FILE   = ECMWF5K_ASCII_FILE, &
                    STATUS ='OLD'        , &
                    ACCESS ='SEQUENTIAL' , &
                    FORM   ='FORMATTED'  , &
                    ACTION ='READ'       , &
                    IOSTAT = io_status   , &
                    IOMSG  = io_msg        )
  IF ( io_status /= 0 ) THEN
    msg = 'Error opening '//ECMWF5K_ASCII_FILE//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
    CLOSE( ASC_FileID )
  END IF
  

  ! Open the output Binary data file
  BIN_FileID = Get_Lun()
  OPEN( BIN_FileID, FILE   = ECMWF5K_BINARY_FILE, &
                    STATUS ='REPLACE'      , &
                    ACCESS ='DIRECT'       , &
                    FORM   ='UNFORMATTED'  , &
                    ACTION ='WRITE'        , &
                    RECL   = ECMWF5K_RECLEN, &
                    IOSTAT = io_status     , &
                    IOMSG  = io_msg          )
  IF ( io_status /= 0 ) THEN
    msg = 'Error opening '//ECMWF5K_BINARY_FILE//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
    CLOSE( BIN_FileID )
  END IF
  
  
  ! Begin profile loop
  DO m = 1, N_ECMWF5K_PROFILES
  
    IF ( MOD(m,100) == 0 ) WRITE( *, '(5x,"Converting profile #",i0)' ) m
  
    ! Read a single profile
    READ( ASC_FileID, FMT    = ECMWF5K_FMT_STRING, &
                      IOSTAT = io_status         , &
                      IOMSG  = io_msg            ) &
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
    IF ( io_status /= 0 ) THEN
      WRITE( msg,'("Error reading profile # ",i0," - ",a)' ) m, TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
      GOTO 100
    END IF
  
    ! And write it straight back out
    WRITE( BIN_FileID, REC    = m        , &
                       IOSTAT = io_status, &
                       IOMSG  = io_msg   ) &
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
    IF ( io_status /= 0 ) THEN
      WRITE( msg,'("Error writing profile # ",i0," - ",a)' ) m, TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
      GOTO 100
    END IF
  
  END DO

  ! Done
  100 CONTINUE
  CLOSE( ASC_FileId )
  CLOSE( BIN_FileId )

END PROGRAM ECMWF5K_ASC2BIN
