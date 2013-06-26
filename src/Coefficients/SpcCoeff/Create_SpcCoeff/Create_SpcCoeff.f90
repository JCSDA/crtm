!
! Create_SpcCoeff
!
! Program to create spectral coefficient (SpcCoeff) files
!
!
! CREATION HISTORY:
!       Written by:     David Groff 13-Feb-2009 
!                       david.groff@noaa.gov
!
!       Revised by:     David Groff 8-June-2009
!                       david.groff@noaa.gov
!
!       Revised by:     David Groff 25-October-2010
!                       david.groff@noaa.gov

PROGRAM Create_SpcCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Compare_Float_Numbers    , ONLY: OPERATOR(.EqualTo.)
  USE SensorInfo_Parameters    , ONLY: MICROWAVE_SENSOR, &
                                       INFRARED_SENSOR , &
                                       VISIBLE_SENSOR  , &
                                       UNPOLARIZED     
  USE Planck_Functions         , ONLY: Planck_Radiance
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_GHz, &
                                       GHz_to_inverse_cm
  USE MW_SensorData_Define     , ONLY: MW_SensorData_type, &
                                       MW_SensorData_Destroy, &
                                       MW_SensorData_Load   , &
                                       MW_SensorData_DefineVersion
  USE Solar_Define             , ONLY: Solar_type, &
                                       Solar_Destroy,solar_inspect
  USE Solar_IO                 , ONLY: Solar_ReadFile
  USE SpcCoeff_Define          , ONLY: SpcCoeff_type      , &
                                       SpcCoeff_Associated, &
                                       SpcCoeff_Create    , &
                                       SpcCoeff_Destroy   , &
                                       SpcCoeff_IsSolar   , &
                                       SpcCoeff_IsMicrowaveSensor, &
                                       SpcCoeff_SetSolar  , &
                                       SpcCoeff_SetZeeman
  USE SpcCoeff_netCDF_IO       , ONLY: SpcCoeff_netCDF_WriteFile
  USE PtrArr_Define            , ONLY: PtrArr_type, &
                                       PtrArr_Associated, &
                                       PtrArr_Destroy   , &
                                       PtrArr_Create
  USE oSRF_Define              , ONLY: oSRF_type, &
                                       oSRF_Destroy , &
                                       oSRF_GetValue, &
                                       oSRF_Convolve, &
                                       oSRF_IsFrequencyGHz
  USE oSRF_File_Define         , ONLY: oSRF_File_type, &
                                       oSRF_File_GetValue, &
                                       oSRF_File_GetFrom , &
                                       oSRF_File_Read    , &
                                       oSRF_File_Destroy
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_SpcCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Cosmic background temperature
  REAL(fp), PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.7253_fp
  ! Coefficient dimensions
  INTEGER, PARAMETER :: N_PLANCK_COEFFS = 2
  INTEGER, PARAMETER :: N_POLYCHROMATIC_COEFFS = 2


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: osrf_filename
  CHARACTER(256) :: solar_filename = ''
  CHARACTER(256) :: spccoeff_filename
  CHARACTER(20)  :: sensor_id
  CHARACTER(2000) :: comment               = ''
  CHARACTER(2000) :: osrf_history          = ''
  CHARACTER(2000) :: osrf_comment          = ''
  CHARACTER(2000) :: solar_history         = ''
  CHARACTER(2000) :: solar_comment         = ''
  CHARACTER(2000) :: mw_sensordata_history = ''
  INTEGER :: err_stat
  INTEGER :: sensor_type     
  INTEGER :: wmo_satellite_id, wmo_sensor_id
  INTEGER :: n_channels, n_points
  INTEGER :: channel      
  INTEGER :: idx_f1, idx_f2
  INTEGER :: l
  INTEGER :: n_coeffs
  INTEGER :: SpcCoeff_File_Version
  REAL(fp) :: f0, f1, f2
  REAL(fp) :: df_osrf, df_solar
  REAL(fp) :: planck_coeffs(N_PLANCK_COEFFS)
  REAL(fp) :: polychromatic_coeffs(N_POLYCHROMATIC_COEFFS)
  TYPE(PtrArr_type)        :: solar_irradiance(1)
  TYPE(oSRF_File_type)     :: osrf_file
  TYPE(oSRF_type)          :: osrf
  TYPE(Solar_type)         :: solar
  TYPE(SpcCoeff_type)      :: spccoeff
  TYPE(MW_SensorData_type) :: mw_sensordata

  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to create the SpcCoeff '//&
                       'files from the sensor oSRF data files.', &
                       '$Revision$' )


  ! Get user inputs
  ! ...The spectral response function data filename
  WRITE( *,FMT='(/5x,"Enter the oSRF netCDF filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) oSRF_Filename
  oSRF_Filename = ADJUSTL(oSRF_Filename)
  IF ( .NOT. File_Exists( oSRF_Filename ) ) THEN
    msg = 'oSRF file '//TRIM(oSRF_Filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...The SpcCoeff version
  WRITE( *,FMT='(/5x,"Default SpcCoeff version is: ",i0,". Enter value: ")',ADVANCE='NO' ) SpcCoeff%Version
  READ( *,* ) SpcCoeff_File_Version
  IF ( SpcCoeff_File_Version < SpcCoeff%Version ) THEN
    msg = 'Invalid version number specified. Using default.'
    CALL Display_Message( PROGRAM_NAME, msg, INFORMATION )
    SpcCoeff_File_Version = SpcCoeff%Version
  END IF


  ! Read the spectral response function data file
  err_stat = oSRF_File_Read( oSRF_File, oSRF_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading oSRF data from '//TRIM(oSRF_Filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Retrieve the oSRF file attributes
  err_stat = oSRF_File_GetValue( &
    oSRF_File                          , &
    n_Channels       = n_channels      , &
    Sensor_Id        = sensor_id       , &
    WMO_Satellite_Id = wmo_satellite_id, &
    WMO_Sensor_Id    = wmo_sensor_id   , &
    Sensor_Type      = sensor_type     , &
    History          = osrf_history    , &
    Comment          = osrf_comment      )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error retrieving attributes from'//TRIM(oSRF_Filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate the SpcCoeff structure                                                   
  CALL SpcCoeff_Create( SpcCoeff, n_channels )
  IF ( .NOT. SpcCoeff_Associated( SpcCoeff ) ) THEN
    msg = 'Error allocating SpcCoeff data structure.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP                      
  END IF
  ! ...Copy over the version and sensor id information
  SpcCoeff%Version          = spccoeff_file_version
  SpcCoeff%Sensor_Id        = sensor_id
  SpcCoeff%Sensor_Type      = sensor_type
  SpcCoeff%WMO_Satellite_ID = wmo_satellite_id
  SpcCoeff%WMO_Sensor_ID    = wmo_sensor_id


  ! Set some sensor dependent stuff
  SELECT CASE( sensor_type )
    CASE( MICROWAVE_SENSOR )
      err_stat = MW_SensorData_Load( MW_SensorData, Sensor_Id )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error loading MW sensor data for '//TRIM(sensor_id)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      SpcCoeff%Polarization = mw_sensordata%Polarization
      CALL MW_SensorData_DefineVersion( MW_SensorData_History )
      MW_SensorData_History = '; '//TRIM(MW_SensorData_History)
       
    CASE( INFRARED_SENSOR )
      SpcCoeff%Polarization = UNPOLARIZED
      CALL SpcCoeff_SetSolar( SpcCoeff )
      IF ( sensor_id(1:4) == 'airs' ) THEN
        solar_filename = 'dF_0.0025.Solar.nc'
      ELSE
        solar_filename = 'dF_0.1000.Solar.nc'
      END IF

    CASE( VISIBLE_SENSOR )
      SpcCoeff%Polarization = UNPOLARIZED
      CALL SpcCoeff_SetSolar( SpcCoeff )
      solar_filename = 'dF_0.1000.Solar.nc'

    CASE DEFAULT
      msg = 'Can only handle MW, IR, and VIS sensors so far.'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END SELECT
    
    
  ! Read the solar irradiance data if required
  IF ( SpcCoeff_IsSolar( SpcCoeff ) ) THEN
    err_stat = Solar_ReadFile( &
      solar_filename         , &
      solar                  , &
      Comment = solar_comment, &
      History = solar_history  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading solar data file '//TRIM(solar_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  END IF


  ! Begin loop over channels
  Channel_Loop: DO l = 1, SpcCoeff%n_Channels


    ! Get the current oSRF object from the file container
    err_stat = oSRF_File_GetFrom( &
      osrf_file, &
      osrf     , &
      pos = l    )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error retrieving oSRF #",i0," from oSRF_File container")') l
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Retrieve information from the oSRF object
    err_stat = oSRF_GetValue( &
      oSRF, &
      Sensor_Id              = sensor_id       , &
      WMO_Satellite_Id       = wmo_satellite_id, &
      WMO_Sensor_Id          = wmo_sensor_id   , &
      Sensor_Type            = sensor_type     , &
      Channel                = channel         , &
      f0                     = f0              , &
      Planck_Coeffs          = planck_coeffs   , &
      n_Polychromatic_Coeffs = n_coeffs          )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error retrieving oSRF #",i0," attributes")') l
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Check that we can handle the number of polychromatic coefficients
    IF ( n_coeffs /= N_POLYCHROMATIC_COEFFS ) THEN
      WRITE( msg,'("The number of polychromatic correction coefficients for oSRF #",i0,&
                  &", ",i0,", is different from that for the SpcCoeff file, ",i0)' ) &
                  l, n_coeffs, N_POLYCHROMATIC_COEFFS
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Get the polychromatic coefficients
    err_stat = oSRF_GetValue( &
      oSRF, &
      Polychromatic_Coeffs = polychromatic_coeffs )    
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error retrieving oSRF #",i0," polychromatic correction coefficients")') l
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Double-check the sensor information is consistent
    IF ( TRIM(SpcCoeff%Sensor_Id)  /= TRIM(sensor_id)  .OR. &
         SpcCoeff%Sensor_Type      /= sensor_type      .OR. &
         SpcCoeff%WMO_Satellite_ID /= wmo_satellite_id .OR. &
         SpcCoeff%WMO_Sensor_ID    /= wmo_sensor_id         ) THEN
      WRITE( msg,'("oSRF #",i0," sensor information different from oSRF_File object")') l
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Assign (ostensibly) sensor independent data
    SpcCoeff%Sensor_Channel(l) = channel
    ! ...Frequency information
    IF ( oSRF_IsFrequencyGHz( oSRF ) ) THEN
      SpcCoeff%Frequency(l)  = f0 
      SpcCoeff%Wavenumber(l) = GHz_to_inverse_cm( f0 )
    ELSE
      SpcCoeff%Frequency(l)  = Inverse_cm_to_GHz( f0 )
      SpcCoeff%Wavenumber(l) = f0
    END IF
    ! ...Planck coefficients
    SpcCoeff%Planck_C1(l) = planck_coeffs(1)
    SpcCoeff%Planck_C2(l) = planck_coeffs(2)
    ! ...Polychromatic correction coefficients
    SpcCoeff%Band_C1(l) = polychromatic_coeffs(1)
    SpcCoeff%Band_C2(l) = polychromatic_coeffs(2)
    ! ...Set the microwave specific items
    IF ( SpcCoeff_IsMicrowaveSensor( SpcCoeff ) ) THEN
      ! The Zeeman flag
      IF ( mw_sensordata%Zeeman(l) == SET ) CALL SpcCoeff_SetZeeman(SpcCoeff, ChannelIndex=l)
      ! The CBR
      CALL Planck_Radiance( &
        SpcCoeff%Wavenumber(l)                , &
        COSMIC_BACKGROUND_TEMPERATURE         , &
        SpcCoeff%Cosmic_Background_Radiance(l)  )
    END IF
    
    
    ! Add the solar information to the SpcCoeff if necessary
    Add_Solar: IF ( SpcCoeff_IsSolar( SpcCoeff ) ) THEN
      ! ...Get the number of oSRF points, the begin and end frequencies
      err_stat = oSRF_GetValue( oSRF, n_Points = n_points, f1 = f1, f2 = f2 )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving oSRF #",i0," n_Points, f1, and f2")') l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...Compute the frequency intervals
      df_osrf  = (f2-f1)/REAL(n_points-1,fp)
      df_solar = (solar%f2-solar%f1)/REAL(solar%n_frequencies-1,fp)
      IF ( ABS(df_osrf-df_solar) > 1.0e-06_fp ) THEN
        WRITE( msg,'("df values for oSRF(",es23.16,") ",&
                             &"and Solar(",es23.16,") are different")' ) df_osrf, df_solar
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...Find the indices of the solar spectrum corresponding to the oSRF begin and end frequencies
      idx_f1 = NINT((f1 - solar%f1)/df_solar) + 1
      idx_f2 = NINT((f2 - solar%f1)/df_solar) + 1
      IF ( (idx_f1 < 1 .OR. idx_f1 > solar%n_Frequencies) .OR. &
           (idx_f2 < 1 .OR. idx_f2 > solar%n_Frequencies) ) THEN
        msg = 'Solar indices for oSRF end points are invalid'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      IF ( (idx_f2 - idx_f1 + 1) /= n_points ) THEN
        msg = 'No. of solar points corresponding to oSRF are different'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...Allocate the solar irradiance pointer array
      CALL PtrArr_Create( solar_irradiance, n_points )
      IF ( .NOT. ALL(PtrArr_Associated( solar_irradiance )) ) THEN
        WRITE( msg,'("Error allocating oSRF #",i0," solar irradiance PtrArr")' ) l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...Convolve the solar spectrum with the oSRF
      solar_irradiance(1)%Arr = solar%irradiance(idx_f1:idx_f2)
      err_stat = oSRF_Convolve( &
        osrf                        , &
        solar_irradiance            , &
        spccoeff%Solar_Irradiance(l)  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error occurred convolving solar irradiance'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...Cleanup
      CALL PtrArr_Destroy( solar_irradiance )
    END IF Add_Solar


    ! Destroy current oSRF object
    CALL oSRF_Destroy( osrf )
      
  END DO Channel_Loop


  ! Write the SpcCoeff data file
  spccoeff_filename = TRIM(sensor_id)//'.SpcCoeff.nc'
  err_stat = SpcCoeff_netCDF_WriteFile( &
    spccoeff_filename, &
    spccoeff, &
    Title   = 'Spectral coefficients for '//TRIM(sensor_id)//&
              ' derived from SRF data file '//TRIM(osrf_filename)//&
              ' and solar data file '//TRIM(solar_filename), &
    History = PROGRAM_VERSION_ID//'; '//TRIM(osrf_history)//'; '//TRIM(solar_history), &
    Comment = TRIM(comment)//&
              '; oSRF Information: '//TRIM(osrf_comment)//&
              '; Solar Information: '//TRIM(solar_comment) )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing netCDF SpcCoeff data file '//TRIM(spccoeff_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Clean up
  CALL SpcCoeff_Destroy( spccoeff )
  CALL oSRF_File_Destroy( osrf_file )
  CALL Solar_Destroy( solar )
  CALL MW_SensorData_Destroy( mw_sensordata )

END PROGRAM Create_SpcCoeff
