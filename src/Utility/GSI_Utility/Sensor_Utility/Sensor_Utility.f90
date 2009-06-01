MODULE Sensor_Utility

  IMPLICIT NONE

  ! Define explciit visibilities
  PRIVATE
  PUBLIC :: INVALID_NCEP_ID
  PUBLIC :: INVALID_NFOVS
  PUBLIC :: Get_SensorAttributes
  PUBLIC :: Get_NCEP_Id
  PUBLIC :: Get_nFOVs
  PUBLIC :: Get_Detector
  PUBLIC :: Get_Sensor_Name
  PUBLIC :: Get_Platform_Name
  PUBLIC :: Get_Attributes_by_NCEP_id

  ! Module parameters
  INTEGER, PARAMETER :: INVALID_NCEP_ID = -1
  INTEGER, PARAMETER :: INVALID_NFOVS   = -1
  INTEGER, PARAMETER :: MAX_SENSORS = 58
  INTEGER, PARAMETER :: MAX_STRLEN  = 20

  ! Sensor attribute map type definition
  TYPE :: Map
    CHARACTER(MAX_STRLEN) :: Sensor_Id  ! This is the unique identifier. The "key"
    INTEGER               :: NCEP_Id
    INTEGER               :: nFOVs
    INTEGER               :: Detector
    CHARACTER(MAX_STRLEN) :: Sensor_Name
    CHARACTER(MAX_STRLEN) :: Platform_Name
  END TYPE Map

  ! The sensor attribute map array
  TYPE( Map ), PARAMETER, DIMENSION( MAX_SENSORS ) :: SensorAttribute_Map = &
(/ Map('hirs2_n14'    , 14,56,0,'HIRS/2' ,'NOAA-14'), Map('msu_n14'      ,214,10,0,'MSU'    ,'NOAA-14'), &
   Map('avhrr2_n14'   ,614,90,0,'AVHRR/2','NOAA-14'), Map('hirs3_n15'    , 15,56,0,'HIRS/3' ,'NOAA-15'), &
   Map('amsua_n15'    ,315,30,0,'AMSU-A' ,'NOAA-15'), Map('amsub_n15'    ,415,90,0,'AMSU-B' ,'NOAA-15'), &
   Map('avhrr3_n15'   ,615,90,0,'AVHRR/3','NOAA-15'), Map('hirs3_n16'    , 16,56,0,'HIRS/3' ,'NOAA-16'), &
   Map('amsua_n16'    ,316,30,0,'AMSU-A' ,'NOAA-16'), Map('amsub_n16'    ,416,90,0,'AMSU-B' ,'NOAA-16'), &
   Map('avhrr3_n16'   ,616,90,0,'AVHRR/3','NOAA-16'), Map('hirs3_n17'    , 17,56,0,'HIRS/3' ,'NOAA-17'), &
   Map('amsua_n17'    ,317,30,0,'AMSU-A' ,'NOAA-17'), Map('amsub_n17'    ,417,90,0,'AMSU-B' ,'NOAA-17'), &
   Map('avhrr3_n17'   ,617,90,0,'AVHRR/3','NOAA-17'), Map('hirs4_n18'    , 18,56,0,'HIRS/4' ,'NOAA-18'), &
   Map('amsua_n18'    ,318,30,0,'AMSU-A' ,'NOAA-18'), Map('mhs_n18'      ,418,90,0,'MHS'    ,'NOAA-18'), &
   Map('avhrr3_n18'   ,618,90,0,'AVHRR/3','NOAA-18'), Map('airs281SUBSET_aqua',49,90,0,'AIRS','Aqua'  ), &
   Map('amsua_aqua'   ,349,30,0,'AMSU-A' ,'Aqua'   ), Map('hsb_aqua'     ,449,90,0,'HSB'    ,'Aqua'   ), &
   Map('amsre_aqua'   ,549,90,0,'AMSR-E' ,'Aqua'   ), Map('sndr_g08'     , 58,62,0,'Sounder','GOES-08'), &
   Map('sndr_g09'     , 59,90,0,'Sounder','GOES-09'), Map('sndr_g10'     , 60,63,0,'Sounder','GOES-10'), &
   Map('sndr_g11'     , 61,90,0,'Sounder','GOES-11'), Map('sndr_g12'     , 62,63,0,'Sounder','GOES-12'), &
   Map('sndr_g13'     , 63,63,0,'Sounder','GOES-13'), Map('sndrD1_g10'   , 60,63,1,'Sounder','GOES-10'), &
   Map('sndrD2_g10'   , 60,63,2,'Sounder','GOES-10'), Map('sndrD3_g10'   , 60,63,3,'Sounder','GOES-10'), &
   Map('sndrD4_g10'   , 60,63,4,'Sounder','GOES-10'), Map('sndrD1_g11'   , 61,63,1,'Sounder','GOES-11'), &
   Map('sndrD2_g11'   , 61,63,2,'Sounder','GOES-11'), Map('sndrD3_g11'   , 61,63,3,'Sounder','GOES-11'), &
   Map('sndrD4_g11'   , 61,63,4,'Sounder','GOES-11'), Map('sndrD1_g12'   , 62,63,1,'Sounder','GOES-12'), &
   Map('sndrD2_g12'   , 62,63,2,'Sounder','GOES-12'), Map('sndrD3_g12'   , 62,63,3,'Sounder','GOES-12'), &
   Map('sndrD4_g12'   , 62,63,4,'Sounder','GOES-12'), Map('sndrD1_g13'   , 63,63,1,'Sounder','GOES-13'), &
   Map('sndrD2_g13'   , 63,63,2,'Sounder','GOES-13'), Map('sndrD3_g13'   , 63,63,3,'Sounder','GOES-13'), &
   Map('sndrD4_g13'   , 63,63,4,'Sounder','GOES-13'), Map('imgr_g08'     ,258,90,0,'Imager' ,'GOES-08'), &
   Map('imgr_g09'     ,259,90,0,'Imager' ,'GOES-09'), Map('imgr_g10'     ,260,90,0,'Imager' ,'GOES-10'), &
   Map('imgr_g11'     ,261,90,0,'Imager' ,'GOES-11'), Map('imgr_g12'     ,262,90,0,'Imager' ,'GOES-12'), &
   Map('imgr_g13'     ,263,90,0,'Imager' ,'GOES-13'), Map('ssmi_f13'     ,713,64,0,'SSM/I'  ,'DMSP-13'), &
   Map('ssmi_f14'     ,714,64,0,'SSM/I'  ,'DMSP-14'), Map('ssmi_f15'     ,715,64,0,'SSM/I'  ,'DMSP-15'), &
   Map('ssmis_f16'    ,516,90,0,'SSMIS'  ,'DMSP-16'), Map('hirs4_metop-a', -1,56,0,'HIRS/4' ,'MetOp-A'), &
   Map('amsua_metop-a', -1,30,0,'AMSU-A' ,'MetOp-A'), Map('mhs_metop-a'  , -1,90,0,'MHS'    ,'MetOp-A') /)


CONTAINS


  ! -------------------------------------------
  ! Subroutine to return the sensor information
  ! for a supplied name Id
  ! -------------------------------------------
  SUBROUTINE Get_SensorAttributes( Sensor_Id,    &  ! Input
                                   NCEP_Id,      &  ! Optional output
                                   nFOVs,        &  ! Optional output       
                                   Detector,     &  ! Optional output    
                                   Sensor_Name,  &  ! Optional output
                                   Platform_Name )  ! Optional output
    ! Arguments
    CHARACTER(*), INTENT(IN)            :: Sensor_Id
    INTEGER,      INTENT(OUT), OPTIONAL :: NCEP_Id
    INTEGER,      INTENT(OUT), OPTIONAL :: nFOVs
    INTEGER,      INTENT(OUT), OPTIONAL :: Detector
    CHARACTER(*), INTENT(OUT), OPTIONAL :: Sensor_Name
    CHARACTER(*), INTENT(OUT), OPTIONAL :: Platform_Name
    ! Local variables
    CHARACTER(MAX_STRLEN) :: Local_Sensor_Id
    INTEGER :: i,n
    LOGICAL, DIMENSION(MAX_SENSORS) :: Match_Test
    INTEGER, DIMENSION(1) :: Idx

    ! Search for name id match
    Local_Sensor_Id = ADJUSTL(Sensor_Id)
    Match_Test = ( SensorAttribute_Map%Sensor_Id == Local_Sensor_Id )
    n = COUNT( Match_Test )
    IF ( n == 1 ) Idx = PACK( (/(i,i=1,MAX_SENSORS)/), Match_Test )

    ! Get NCEP_Id if required
    IF ( PRESENT( NCEP_Id ) ) THEN
      NCEP_Id = INVALID_NCEP_ID
      IF ( n == 1 ) NCEP_Id = SensorAttribute_Map(Idx(1))%NCEP_Id
    END IF

    ! Get nFOVs if required
    IF ( PRESENT( nFOVs ) ) THEN
      nFOVs = INVALID_NFOVS
      IF ( n == 1 ) nFOVs = SensorAttribute_Map(Idx(1))%nFOVs
    END IF

    ! Get detector number if required
    IF ( PRESENT( Detector ) ) THEN
      Detector = 0
      IF ( n == 1 ) Detector = SensorAttribute_Map(Idx(1))%Detector
    END IF

    ! Get sensor name if required
    IF ( PRESENT( Sensor_Name ) ) THEN
      Sensor_Name = ' '
      IF ( n == 1 ) Sensor_Name = SensorAttribute_Map(Idx(1))%Sensor_Name
    END IF

    ! Get platform name if required
    IF ( PRESENT( Platform_Name ) ) THEN
      Platform_Name = ' '
      IF ( n == 1 ) Platform_Name = SensorAttribute_Map(Idx(1))%Platform_Name
    END IF

  END SUBROUTINE Get_SensorAttributes


  ! -------------------------------------
  ! Function to return the sensor NCEP Id
  ! for a supplied name Id
  ! -------------------------------------
  FUNCTION Get_NCEP_Id( Sensor_Id ) RESULT( NCEP_ID )
    CHARACTER(*), INTENT(IN) :: Sensor_Id
    INTEGER :: NCEP_Id
    CALL Get_SensorAttributes( Sensor_Id, NCEP_Id=NCEP_Id )
  END FUNCTION Get_NCEP_ID


  ! ----------------------------------
  ! Functions to return the number of
  ! sensor FOVs for a supplied name Id
  ! ----------------------------------
  FUNCTION Get_nFOVs( Sensor_Id ) RESULT( nFOVs )
    CHARACTER(*), INTENT(IN) :: Sensor_Id
    INTEGER :: nFOVs
    CALL Get_SensorAttributes( Sensor_Id, nFOVs=nFOVs )
  END FUNCTION Get_nFOVs


  ! --------------------------------
  ! Functions to return the detector
  ! number for a supplied name Id.
  ! Detector averages == Detcotr 0.
  ! --------------------------------
  FUNCTION Get_Detector( Sensor_Id ) RESULT( Detector )
    CHARACTER(*), INTENT(IN) :: Sensor_Id
    INTEGER :: Detector
    CALL Get_SensorAttributes( Sensor_Id, Detector=Detector )
  END FUNCTION Get_Detector


  ! -----------------------------------
  ! Functions to return the sensor name
  ! for a supplied name Id
  ! -----------------------------------
  FUNCTION Get_Sensor_Name( Sensor_Id ) RESULT( Sensor_Name )
    CHARACTER(*), INTENT(IN) :: Sensor_Id
    CHARACTER(MAX_STRLEN) :: Sensor_Name
    CALL Get_SensorAttributes( Sensor_Id, Sensor_Name=Sensor_Name )
  END FUNCTION Get_Sensor_Name


  ! -------------------------------------
  ! Functions to return the platform name
  ! for a supplied name Id
  ! -------------------------------------
  FUNCTION Get_Platform_Name( Sensor_Id ) RESULT( Platform_Name )
    CHARACTER(*), INTENT(IN) :: Sensor_Id
    CHARACTER(MAX_STRLEN) :: Platform_Name
    CALL Get_SensorAttributes( Sensor_Id, Platform_Name=Platform_Name )
  END FUNCTION Get_Platform_Name


  ! -------------------------------------------
  ! Subroutine to return the sensor information
  ! for a supplied NCEP Id.
  !
  ! This approach is obselete and only used for
  ! intefacing with heritage code. Note that the
  ! FIRST match encountered determines the 
  ! attributes returned.
  ! -------------------------------------------
  SUBROUTINE Get_Attributes_by_NCEP_Id( NCEP_Id,      &  ! Input
                                        Sensor_Id,    &  ! Optional output
                                        nFOVs,        &  ! Optional output       
                                        Detector,     &  ! Optional output    
                                        Sensor_Name,  &  ! Optional output
                                        Platform_Name )  ! Optional output
    ! Arguments
    INTEGER,      INTENT(IN)            :: NCEP_Id
    CHARACTER(*), INTENT(OUT), OPTIONAL :: Sensor_Id
    INTEGER,      INTENT(OUT), OPTIONAL :: nFOVs
    INTEGER,      INTENT(OUT), OPTIONAL :: Detector
    CHARACTER(*), INTENT(OUT), OPTIONAL :: Sensor_Name
    CHARACTER(*), INTENT(OUT), OPTIONAL :: Platform_Name
    ! Local variables
    INTEGER :: Local_NCEP_Id
    INTEGER :: i,n
    LOGICAL, DIMENSION(MAX_SENSORS) :: Match_Test
    INTEGER, DIMENSION(MAX_SENSORS) :: Idx

    ! Search for name id match
    Match_Test = ( SensorAttribute_Map%NCEP_Id == NCEP_Id )
    n = COUNT( Match_Test )
    IF ( n > 0 ) Idx(1:n) = PACK( (/(i,i=1,MAX_SENSORS)/), Match_Test )

    ! Get Sensor_Id if required
    IF ( PRESENT( Sensor_Id ) ) THEN
      Sensor_Id = ' '
      IF ( n > 0 ) Sensor_Id = SensorAttribute_Map(Idx(1))%Sensor_Id
    END IF

    ! Get nFOVs if required
    IF ( PRESENT( nFOVs ) ) THEN
      nFOVs = INVALID_NFOVS
      IF ( n > 0 ) nFOVs = SensorAttribute_Map(Idx(1))%nFOVs
    END IF

    ! Get detector number if required
    IF ( PRESENT( Detector ) ) THEN
      Detector = 0
      IF ( n > 0 ) Detector = SensorAttribute_Map(Idx(1))%Detector
    END IF

    ! Get sensor name if required
    IF ( PRESENT( Sensor_Name ) ) THEN
      Sensor_Name = ' '
      IF ( n > 0 ) Sensor_Name = SensorAttribute_Map(Idx(1))%Sensor_Name
    END IF

    ! Get platform name if required
    IF ( PRESENT( Platform_Name ) ) THEN
      Platform_Name = ' '
      IF ( n > 0 ) Platform_Name = SensorAttribute_Map(Idx(1))%Platform_Name
    END IF

  END SUBROUTINE Get_Attributes_by_NCEP_Id

END MODULE Sensor_Utility
