!
! Create_SensorInfo_Code
!
! Program to create the code for the CRTM_Get_SensorAttributes function
! in the CRTM_SensorInfo module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Sep-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Create_SensorInfo_Code

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_SensorInfo_Code'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: SENSORINFO_FILENAME='SensorInfo'
  INTEGER, PARAMETER :: SL = 20

  ! -------------
  ! Derived types
  ! -------------
  TYPE :: SensorMap
    INTEGER       :: SensorID
    CHARACTER(20) :: SensorIDvarname
    INTEGER       :: nFOVs
  END TYPE SensorMap

  TYPE :: SatelliteMap
    INTEGER       :: SatelliteID
    CHARACTER(20) :: SatelliteIDvarname
  END TYPE SatelliteMap

  ! ----------
  ! Parameters
  ! ----------
  ! WMO SENSOR codes from COMMON CODE TABLE C-8
  INTEGER, PARAMETER :: INVALID_WMO_SENSORID = 2047
  INTEGER, PARAMETER :: N_WMO_SENSORS=28
  INTEGER, PARAMETER :: WMO_HIRS2     = 605
  INTEGER, PARAMETER :: WMO_MSU       = 623
  INTEGER, PARAMETER :: WMO_AVHRR2    = 590
  INTEGER, PARAMETER :: WMO_SSU       = 627
  INTEGER, PARAMETER :: WMO_HIRS3     = 606
  INTEGER, PARAMETER :: WMO_HIRS4     = 607
  INTEGER, PARAMETER :: WMO_AMSUA     = 570
  INTEGER, PARAMETER :: WMO_AMSUB     = 574
  INTEGER, PARAMETER :: WMO_AVHRR3    = 591
  INTEGER, PARAMETER :: WMO_MHS       = 203
  INTEGER, PARAMETER :: WMO_VAS       = 630
  INTEGER, PARAMETER :: WMO_SNDR_NOAA = 626  ! GOES
  INTEGER, PARAMETER :: WMO_IMGR_NOAA = 615  ! GOES
  INTEGER, PARAMETER :: WMO_SSMI      = 905
  INTEGER, PARAMETER :: WMO_SSMT1     = 906
  INTEGER, PARAMETER :: WMO_SSMT2     = 907
  INTEGER, PARAMETER :: WMO_MODIS     = 389
  INTEGER, PARAMETER :: WMO_HSB       = 246
  INTEGER, PARAMETER :: WMO_AMSRE     = 345
  INTEGER, PARAMETER :: WMO_AIRS      = 420
  INTEGER, PARAMETER :: WMO_VISSR     = 489
  INTEGER, PARAMETER :: WMO_MVIRI     = 205
  INTEGER, PARAMETER :: WMO_SEVIRI    = 207
  INTEGER, PARAMETER :: WMO_ABI       = INVALID_WMO_SENSORID
  INTEGER, PARAMETER :: WMO_WINDSAT   = INVALID_WMO_SENSORID
  INTEGER, PARAMETER :: WMO_ATMS      = 621
  INTEGER, PARAMETER :: WMO_IMGR_JMA  = 294  ! MTSAT-1R
  INTEGER, PARAMETER :: WMO_SSMIS     = 908

  TYPE(SensorMap), PARAMETER, DIMENSION(N_WMO_SENSORS) :: WMO_SensorMap = &
  (/ SensorMap(WMO_HIRS2    , 'WMO_HIRS2    ',56), &
     SensorMap(WMO_MSU      , 'WMO_MSU      ',10), &
     SensorMap(WMO_AVHRR2   , 'WMO_AVHRR2   ',90), &
     SensorMap(WMO_SSU      , 'WMO_SSU      ',-1), &
     SensorMap(WMO_HIRS3    , 'WMO_HIRS3    ',56), &
     SensorMap(WMO_HIRS4    , 'WMO_HIRS4    ',56), &
     SensorMap(WMO_AMSUA    , 'WMO_AMSUA    ',30), &
     SensorMap(WMO_AMSUB    , 'WMO_AMSUB    ',90), &
     SensorMap(WMO_AVHRR3   , 'WMO_AVHRR3   ',90), &
     SensorMap(WMO_MHS      , 'WMO_MHS      ',90), &
     SensorMap(WMO_VAS      , 'WMO_VAS      ',-1), &
     SensorMap(WMO_SNDR_NOAA, 'WMO_SNDR_NOAA',90), &
     SensorMap(WMO_IMGR_NOAA, 'WMO_IMGR_NOAA',90), &
     SensorMap(WMO_SSMI     , 'WMO_SSMI     ',64), &
     SensorMap(WMO_SSMT1    , 'WMO_SSMT1    ',-1), &
     SensorMap(WMO_SSMT2    , 'WMO_SSMT2    ',-1), &
     SensorMap(WMO_MODIS    , 'WMO_MODIS    ',-1), &
     SensorMap(WMO_HSB      , 'WMO_HSB      ',90), &
     SensorMap(WMO_AMSRE    , 'WMO_AMSRE    ',90), &
     SensorMap(WMO_AIRS     , 'WMO_AIRS     ',90), &
     SensorMap(WMO_VISSR    , 'WMO_VISSR    ',-1), &
     SensorMap(WMO_MVIRI    , 'WMO_MVIRI    ',-1), &
     SensorMap(WMO_SEVIRI   , 'WMO_SEVIRI   ',-1), &
     SensorMap(WMO_ABI      , 'WMO_ABI      ',-1), &
     SensorMap(WMO_WINDSAT  , 'WMO_WINDSAT  ',-1), &
     SensorMap(WMO_IMGR_JMA , 'WMO_IMGR_JMA ',-1), &
     SensorMap(WMO_ATMS     , 'WMO_ATMS     ',-1), &
     SensorMap(WMO_SSMIS    , 'WMO_SSMIS    ',-1) /)

  ! WMO SATELLITE codes from COMMON CODE TABLE C-5
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITEID = 1023
  INTEGER, PARAMETER :: N_WMO_SATELLITES=50
  INTEGER, PARAMETER :: WMO_TIROSN     = 708
  INTEGER, PARAMETER :: WMO_NOAA05     = WMO_TIROSN
  INTEGER, PARAMETER :: WMO_NOAA06     = 706
  INTEGER, PARAMETER :: WMO_NOAA07     = 707
  INTEGER, PARAMETER :: WMO_NOAA08     = 200
  INTEGER, PARAMETER :: WMO_NOAA09     = 201
  INTEGER, PARAMETER :: WMO_NOAA10     = 202
  INTEGER, PARAMETER :: WMO_NOAA11     = 203
  INTEGER, PARAMETER :: WMO_NOAA12     = 204
  INTEGER, PARAMETER :: WMO_NOAA14     = 205
  INTEGER, PARAMETER :: WMO_NOAA15     = 206
  INTEGER, PARAMETER :: WMO_NOAA16     = 207
  INTEGER, PARAMETER :: WMO_NOAA17     = 208
  INTEGER, PARAMETER :: WMO_NOAA18     = 209
  INTEGER, PARAMETER :: WMO_NOAA19     = INVALID_WMO_SATELLITEID
  INTEGER, PARAMETER :: WMO_GOES04     = 734
  INTEGER, PARAMETER :: WMO_GOES05     = 735
  INTEGER, PARAMETER :: WMO_GOES06     = 250
  INTEGER, PARAMETER :: WMO_GOES07     = 251
  INTEGER, PARAMETER :: WMO_GOES08     = 252
  INTEGER, PARAMETER :: WMO_GOES09     = 253
  INTEGER, PARAMETER :: WMO_GOES10     = 254
  INTEGER, PARAMETER :: WMO_GOES11     = 255
  INTEGER, PARAMETER :: WMO_GOES12     = 256
  INTEGER, PARAMETER :: WMO_GOES13     = 257
  INTEGER, PARAMETER :: WMO_DMSP13     = 246
  INTEGER, PARAMETER :: WMO_DMSP14     = 247
  INTEGER, PARAMETER :: WMO_DMSP15     = 248
  INTEGER, PARAMETER :: WMO_DMSP16     = 249
  INTEGER, PARAMETER :: WMO_DMSP17     = 285
  INTEGER, PARAMETER :: WMO_TERRA      = 783
  INTEGER, PARAMETER :: WMO_AQUA       = 784
  INTEGER, PARAMETER :: WMO_METEOSAT03 = 50
  INTEGER, PARAMETER :: WMO_METEOSAT04 = 51
  INTEGER, PARAMETER :: WMO_METEOSAT05 = 52
  INTEGER, PARAMETER :: WMO_METEOSAT06 = 53
  INTEGER, PARAMETER :: WMO_METEOSAT07 = 54
  INTEGER, PARAMETER :: WMO_METEOSAT08 = 71
  INTEGER, PARAMETER :: WMO_METEOSAT09 = 56
  INTEGER, PARAMETER :: WMO_METEOSAT10 = 57
  INTEGER, PARAMETER :: WMO_METEOSAT11 = 70
  INTEGER, PARAMETER :: WMO_GOESR      = INVALID_WMO_SATELLITEID
  INTEGER, PARAMETER :: WMO_CORIOLIS   = 283
  INTEGER, PARAMETER :: WMO_NPOESSC1   = INVALID_WMO_SATELLITEID
  INTEGER, PARAMETER :: WMO_METOPA     = 3
  INTEGER, PARAMETER :: WMO_METOPB     = 4
  INTEGER, PARAMETER :: WMO_METOPC     = 5
  INTEGER, PARAMETER :: WMO_MTSAT1R    = 171
  INTEGER, PARAMETER :: WMO_GMS3       = 150
  INTEGER, PARAMETER :: WMO_GMS4       = 151
  INTEGER, PARAMETER :: WMO_GMS5       = 152

  TYPE(SatelliteMap), PARAMETER, DIMENSION(N_WMO_SATELLITES) :: WMO_SatelliteMap = &
  (/SatelliteMap(WMO_NOAA05, 'WMO_NOAA05    '), SatelliteMap(WMO_GOES11    , 'WMO_GOES11    '), &
    SatelliteMap(WMO_NOAA06, 'WMO_NOAA06    '), SatelliteMap(WMO_GOES12    , 'WMO_GOES12    '), &
    SatelliteMap(WMO_NOAA07, 'WMO_NOAA07    '), SatelliteMap(WMO_GOES13    , 'WMO_GOES13    '), &
    SatelliteMap(WMO_NOAA08, 'WMO_NOAA08    '), SatelliteMap(WMO_DMSP13    , 'WMO_DMSP13    '), &
    SatelliteMap(WMO_NOAA09, 'WMO_NOAA09    '), SatelliteMap(WMO_DMSP14    , 'WMO_DMSP14    '), &
    SatelliteMap(WMO_NOAA10, 'WMO_NOAA10    '), SatelliteMap(WMO_DMSP15    , 'WMO_DMSP15    '), &
    SatelliteMap(WMO_NOAA11, 'WMO_NOAA11    '), SatelliteMap(WMO_DMSP16    , 'WMO_DMSP16    '), &
    SatelliteMap(WMO_NOAA12, 'WMO_NOAA12    '), SatelliteMap(WMO_DMSP17    , 'WMO_DMSP17    '), &
    SatelliteMap(WMO_NOAA14, 'WMO_NOAA14    '), SatelliteMap(WMO_TERRA     , 'WMO_TERRA     '), &
    SatelliteMap(WMO_NOAA15, 'WMO_NOAA15    '), SatelliteMap(WMO_AQUA      , 'WMO_AQUA      '), &
    SatelliteMap(WMO_NOAA16, 'WMO_NOAA16    '), SatelliteMap(WMO_GMS5      , 'WMO_GMS5      '), &
    SatelliteMap(WMO_NOAA17, 'WMO_NOAA17    '), SatelliteMap(WMO_METEOSAT03, 'WMO_METEOSAT03'), &
    SatelliteMap(WMO_NOAA18, 'WMO_NOAA18    '), SatelliteMap(WMO_METEOSAT04, 'WMO_METEOSAT04'), &
    SatelliteMap(WMO_GOES04, 'WMO_GOES04    '), SatelliteMap(WMO_METEOSAT05, 'WMO_METEOSAT05'), &
    SatelliteMap(WMO_GOES05, 'WMO_GOES05    '), SatelliteMap(WMO_METEOSAT06, 'WMO_METEOSAT06'), &
    SatelliteMap(WMO_GOES06, 'WMO_GOES06    '), SatelliteMap(WMO_METEOSAT07, 'WMO_METEOSAT07'), &
    SatelliteMap(WMO_GOES07, 'WMO_GOES07    '), SatelliteMap(WMO_METEOSAT08, 'WMO_METEOSAT08'), &
    SatelliteMap(WMO_GOES08, 'WMO_GOES08    '), SatelliteMap(WMO_GOESR     , 'WMO_GOESR     '), &
    SatelliteMap(WMO_GOES09, 'WMO_GOES09    '), SatelliteMap(WMO_CORIOLIS  , 'WMO_CORIOLIS  '), &
    SatelliteMap(WMO_GOES10, 'WMO_GOES10    '), SatelliteMap(WMO_NPOESSC1  , 'WMO_NPOESSC1  '), &
    SatelliteMap(WMO_METEOSAT09, 'WMO_METEOSAT09'), SatelliteMap(WMO_METEOSAT10, 'WMO_METEOSAT10'), &
    SatelliteMap(WMO_METEOSAT11, 'WMO_METEOSAT11'), SatelliteMap(WMO_METOPA,     'WMO_METOPA    '), &
    SatelliteMap(WMO_METOPB,     'WMO_METOPB    '), SatelliteMap(WMO_METOPC,     'WMO_METOPC    '), &
    SatelliteMap(WMO_MTSAT1R,    'WMO_MTSAT1R   '), SatelliteMap(WMO_GMS3,       'WMO_GMS3      '), &
    SatelliteMap(WMO_GMS4,       'WMO_GMS4      '), SatelliteMap(WMO_NOAA19,     'WMO_NOAA19    ') /)

  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Sensors, n
  INTEGER :: i, Idx(1)
  LOGICAL :: SensorMatch(N_WMO_SENSORS), SatelliteMatch(N_WMO_SATELLITES)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to create the code for the '//&
                       'SensorAttributes function in the '//&
                       'CRTM_SensorInfo module.', &
                       '$Revision: 1.1.2.1 $' )

  ! Read the sensorinfo file
  Error_Status = Read_SensorInfo( SENSORINFO_FILENAME, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//SENSORINFO_FILENAME, &
                          FAILURE )
    STOP
  END IF

  ! Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *, '( 5x, "Number of sensors in list: ", i5 )' ) n_Sensors

  ! Loop over all sensors in the list
  Sensor_Loop: DO n = 1, n_Sensors

    ! Get the current SensorInfo node from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

    ! Construct the SELECT CASE item
    WRITE(*,'(6x,"CASE(''",a,"'')")') TRIM(SensorInfo%File_Prefix)
    WRITE(*,'(8x,"local_nChannels       = ",i0)') SensorInfo%n_Channels
    WRITE(*,'(8x,"local_Detector        = -1")')
    IF ( SensorInfo%WMO_Sensor_ID == INVALID_WMO_SENSORID ) THEN
      WRITE(*,'(8x,"local_nFOVs           = -1")')
      WRITE(*,'(8x,"local_WMO_SensorID    = INVALID_WMO_SENSORID")')
    ELSE
      SensorMatch = WMO_SensorMap%SensorID==SensorInfo%WMO_Sensor_ID
      IF ( COUNT(SensorMatch) /= 1 ) THEN
        WRITE(*,'(//,"** ERROR: No WMO_Sensor_ID match for ",a,1x,a," **" )') &
           TRIM(SensorInfo%Sensor_Name), TRIM(SensorInfo%Satellite_Name)
        STOP
      END IF
      idx = PACK( (/(i,i=1,N_WMO_SENSORS)/), SensorMatch )
      WRITE(*,'(8x,"local_nFOVs           = ",i0)') WMO_SensorMap(Idx(1))%nFOVs
      WRITE(*,'(8x,"local_WMO_SensorID    = ",a)') WMO_SensorMap(Idx(1))%SensorIDvarname
    END IF
    IF ( SensorInfo%WMO_Satellite_ID == INVALID_WMO_SATELLITEID ) THEN
      WRITE(*,'(8x,"local_WMO_SatelliteID = INVALID_WMO_SATELLITEID")')
    ELSE
      SatelliteMatch = WMO_SatelliteMap%SatelliteID==SensorInfo%WMO_Satellite_ID
      IF ( COUNT(SatelliteMatch) /= 1 ) THEN
        WRITE(*,'(//,"** ERROR: No WMO_Satellite_ID match for ",a,1x,a," **" )') &
           TRIM(SensorInfo%Sensor_Name), TRIM(SensorInfo%Satellite_Name)
        STOP
      END IF
      idx = PACK( (/(i,i=1,N_WMO_SATELLITES)/), satelliteMatch )
      WRITE(*,'(8x,"local_WMO_SatelliteID = ",a)') WMO_SatelliteMap(Idx(1))%SatelliteIDvarname
    END IF
    WRITE(*,'(8x,"local_SensorName      = ''",a,"''")') TRIM(SensorInfo%Sensor_Name)
    WRITE(*,'(8x,"local_SatelliteName   = ''",a,"''")') TRIM(SensorInfo%Satellite_Name)

    ! Destroy the current SensorInfo node in
    ! preparation for the next node retrieval
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF

  END DO Sensor_Loop

  ! Destroy the linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          Error_Status )
  END IF

END PROGRAM Create_SEnsorInfo_Code
