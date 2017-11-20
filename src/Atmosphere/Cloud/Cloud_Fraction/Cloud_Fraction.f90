!
! Cloud_Fraction
!
! Program to play around with and test cloud fraction computation schemes
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Sep-2015
!                       paul.vandelst@noaa.gov
!

PROGRAM Cloud_Fraction

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module
  USE CRTM_CloudCover_Define
  ! ...For the rad->tb conversion
  USE CRTM_Planck_Functions
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME = 'Cloud_Fraction'
  CHARACTER(*),  PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id$'
  ! Generic
  LOGICAL , PARAMETER :: QUIET = .TRUE.
  ! Thresholds
  REAL(fp), PARAMETER :: WC_THRESHOLD = 1.0e-06_fp
  REAL(fp), PARAMETER :: F_THRESHOLD  = 1.0e-06_fp

  ! The argument defaults
  ! ...The test sensors
  CHARACTER(*), PARAMETER :: AVAILABLE_SENSOR_ID(*) = &
    [ 'atms_npp     ', 'cris399_npp  ', 'amsr2_gcom-w1']
  CHARACTER(*), PARAMETER :: DEFAULT_SENSOR_ID = AVAILABLE_SENSOR_ID(1)
  ! ...Profile skip
  INTEGER,      PARAMETER :: DEFAULT_N_PROFILES_STEP = 1
  ! ...The test inputs
  CHARACTER(*), PARAMETER :: AVAILABLE_DATASET_ID(*) = [ 'ecmwf', 'model', 'gsi  ' ]
  CHARACTER(*), PARAMETER :: DEFAULT_DATASET_ID = AVAILABLE_DATASET_ID(1)
  CHARACTER(*), PARAMETER :: ECMWF5K_ATM_FILENAME = 'ecmwf_5k_atmosphereccol.bin'
  CHARACTER(*), PARAMETER :: ECMWF5K_SFC_FILENAME = 'ecmwf_5k_surfaceccol.bin'
  CHARACTER(*), PARAMETER :: ECMWF5K_GEO_FILENAME = 'ecmwf_5k_geometryccol.bin'
  CHARACTER(*), PARAMETER :: MODEL6_ATM_FILENAME = 'Model6.Atmosphere.bin'
  CHARACTER(*), PARAMETER :: MODEL6_SFC_FILENAME = 'Model6.Surface.bin'
  CHARACTER(*), PARAMETER :: MODEL6_GEO_FILENAME = 'Model6.Geometry.bin'
  CHARACTER(*), PARAMETER :: GSI_ATM_FILENAME = 'gsi58_atm.bin'
  CHARACTER(*), PARAMETER :: GSI_SFC_FILENAME = 'gsi58_sfc.bin'
  CHARACTER(*), PARAMETER :: GSI_GEO_FILENAME = 'gsi58_geo.bin'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: err_msg, alloc_msg, info_msg, io_msg
  CHARACTER(256) :: atm_filename, sfc_filename, geo_filename
  CHARACTER(256) :: subset_filename, output_filename
  CHARACTER(256) :: sensor_id(1)
  CHARACTER(256) :: dataset_id
  CHARACTER(256) :: arg_switch, switch_value
  INTEGER :: n_args, arg_count
  INTEGER :: err_stat, alloc_stat, io_stat
  INTEGER :: fid
  INTEGER :: n_channels, l
  INTEGER :: n_profiles, m
  INTEGER :: n_profiles_total
  INTEGER :: n_profiles_step, m_step
  INTEGER :: n
  REAL(fp) :: rad_cloud, rad_clear
  REAL(fp) :: rad_maximum, rad_random, rad_maxran, rad_average
  REAL(fp), ALLOCATABLE :: tb_maximum(:) , tb_random(:) , tb_maxran(:) , tb_average(:)
  TYPE(CRTM_ChannelInfo_type)              :: chinfo(1)
  TYPE(CRTM_Atmosphere_type) , ALLOCATABLE :: atm(:)
  TYPE(CRTM_Surface_type)    , ALLOCATABLE :: sfc(:)
  TYPE(CRTM_Geometry_type)   , ALLOCATABLE :: geo(:)
  TYPE(CRTM_RTSolution_type) , ALLOCATABLE :: rts_cloud(:,:)  , rts_clear(:,:) , &
                                              rts_maximum(:,:), rts_random(:,:), &
                                              rts_maxran(:,:) , rts_average(:,:)
  TYPE(CRTM_Options_type)                  :: opt(1)
  TYPE(CRTM_CloudCover_type) :: scc_maximum, scc_random, scc_maxran, scc_average


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to play around with and test cloud fraction computation schemes.', &
                        '$Revision$' )

  ! Set the defaults for the command line args
  n_profiles_step = DEFAULT_N_PROFILES_STEP
  dataset_id      = DEFAULT_DATASET_ID
  sensor_id(1)    = DEFAULT_SENSOR_ID


  ! Get the arguments
  n_args = COMMAND_ARGUMENT_COUNT()
  IF ( n_args > 0 ) THEN
    arg_count = 0
    arg_parse_loop: DO
    
      ! In crement argument count and exit if all processed
      arg_count = arg_count + 1
      IF ( arg_count > n_args ) EXIT arg_parse_loop

      ! Get the switch
      CALL GET_COMMAND_ARGUMENT(arg_count, VALUE=arg_switch, STATUS=err_stat)
      IF ( err_stat /= 0 ) THEN
        err_msg = 'Command line switch retrieval failed'
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      arg_switch = ADJUSTL(arg_switch)
      
      ! Work through the argument switches
      SELECT CASE(TRIM(arg_switch))
      
        ! This profile-step switch has an argument
        CASE('-p','--profile-step')
        
          ! Check that a value was passed
          arg_count = arg_count + 1
          IF ( arg_count > n_args ) THEN
            err_msg = 'The command line switch '//TRIM(arg_switch)//' requires an argument'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          
          ! Get the switch value
          CALL GET_COMMAND_ARGUMENT(arg_count, VALUE=switch_value, STATUS=err_stat)
          IF ( err_stat /= 0 ) THEN
            err_msg = 'Retrieval of the '//TRIM(arg_switch)//' value failed'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          switch_value = ADJUSTL(switch_value)
          
          ! Convert switch value character to its required format
          READ(switch_value,FMT=*,IOSTAT=io_stat,IOMSG=io_msg) n_profiles_step
          IF ( io_stat /= 0 ) THEN
            err_msg = 'Error reading '//TRIM(arg_switch)//' value - '//TRIM(io_msg)
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF

          
        ! This sensor-id switch has an argument
        CASE('-s','--sensor-id')
        
          ! Check that a value was passed
          arg_count = arg_count + 1
          IF ( arg_count > n_args ) THEN
            err_msg = 'The command line switch '//TRIM(arg_switch)//' requires an argument'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          
          ! Get the switch value
          CALL GET_COMMAND_ARGUMENT(arg_count, VALUE=switch_value, STATUS=err_stat)
          IF ( err_stat /= 0 ) THEN
            err_msg = 'Retrieval of the '//TRIM(arg_switch)//' value failed'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          switch_value = ADJUSTL(switch_value)
          
          ! Check and assign the switch value
          IF ( .NOT. ANY(AVAILABLE_SENSOR_ID == switch_value) ) THEN
            err_msg = 'Specified sensor id, '//TRIM(switch_value)//' is not available'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          sensor_id(1) = switch_value
          
          
        ! This input data selection switch has an argument
        CASE('-d','--dataset-id')
        
          ! Check that a value was passed
          arg_count = arg_count + 1
          IF ( arg_count > n_args ) THEN
            err_msg = 'The command line switch '//TRIM(arg_switch)//' requires an argument'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          
          ! Get the switch value
          CALL GET_COMMAND_ARGUMENT(arg_count, VALUE=switch_value, STATUS=err_stat)
          IF ( err_stat /= 0 ) THEN
            err_msg = 'Retrieval of the '//TRIM(arg_switch)//' value failed'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          switch_value = ADJUSTL(switch_value)
          
          ! Check and assign the switch value
          IF ( .NOT. ANY(AVAILABLE_DATASET_ID == switch_value) ) THEN
            err_msg = 'Specified data id, '//TRIM(switch_value)//' is not available'
            CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
          END IF
          dataset_id = switch_value
          

        ! The help switch has NO argument
        CASE('-h','--help')

          WRITE(*,'(/," Usage: ForwardFD_vs_Kmatrix [-hm] [-d id] [-p M] [-s id]")')
          WRITE(*,*)
          WRITE(*,'(  " Options:")')
          WRITE(*,'(  "   -h, --help")')
          WRITE(*,'(  "         Print this message and exit.")')
          WRITE(*,*)
          WRITE(*,'(  "   -d id, --dataset-id id")')
          WRITE(*,'(  "         Specify the input data set to use. The default is ",a,".")') TRIM(DEFAULT_DATASET_ID)
          WRITE(*,'(  "         The available dataset ids are:")')
          DO n = 1, SIZE(AVAILABLE_DATASET_ID)
            WRITE(*,'("           * ",a)') TRIM(AVAILABLE_DATASET_ID(n))
          END DO
          WRITE(*,*)
          WRITE(*,'(  "   -p M, --profile-step M")')
          WRITE(*,'(  "         Specify how many profiles to skip in the profile processing loop.")')
          WRITE(*,'(  "         The default profile step is ",i0," profiles.")') DEFAULT_N_PROFILES_STEP
          WRITE(*,*)
          WRITE(*,'(  "   -s id, --sensor-id id")')
          WRITE(*,'(  "         Specify the sensor id to process. The default sensor is ",a)') DEFAULT_SENSOR_ID
          WRITE(*,'(  "         The available sensor ids are:")')
          DO n = 1, SIZE(AVAILABLE_SENSOR_ID)
            WRITE(*,'("           * ",a)') AVAILABLE_SENSOR_ID(n)
          END DO
          WRITE(*,*)
          STOP


        ! Error
        CASE DEFAULT
          err_msg = 'Unrecognised command line switch: '//TRIM(arg_switch)
          CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END SELECT
    END DO arg_parse_loop
  END IF


  ! Assign the input datafiles
  SELECT CASE (TRIM(dataset_id))
    CASE('ecmwf')
      atm_filename = ECMWF5K_ATM_FILENAME
      sfc_filename = ECMWF5K_SFC_FILENAME
      geo_filename = ECMWF5K_GEO_FILENAME
    CASE('model')
      atm_filename = MODEL6_ATM_FILENAME
      sfc_filename = MODEL6_SFC_FILENAME
      geo_filename = MODEL6_GEO_FILENAME
    CASE('gsi')
      atm_filename = GSI_ATM_FILENAME
      sfc_filename = GSI_SFC_FILENAME
      geo_filename = GSI_GEO_FILENAME
    CASE DEFAULT
      err_msg = 'Invalid selector, >'//TRIM(switch_value)//'<, for input dataset.'
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END SELECT
                     

  ! Output argument dependent info
  WRITE(info_msg,'("Profile step increment   : ",i0)') n_profiles_step
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  info_msg = 'Input atmosphere filename: '//TRIM(atm_filename)
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  info_msg = 'Input surface filename   : '//TRIM(sfc_filename)
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  info_msg = 'Input geometry filename  : '//TRIM(geo_filename)
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  info_msg = 'Sensor id to process     : '//TRIM(sensor_id(1))
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  WRITE(*,*)
  

  ! Read the input datafiles
  ! ...Atmosphere
  err_stat = CRTM_Atmosphere_ReadFile( atm_filename, atm, Quiet=QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error reading Atmosphere datafile '//TRIM(atm_filename)
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF
  ! ...Surface
  err_stat = CRTM_Surface_ReadFile( sfc_filename, sfc, Quiet=QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error reading Surface datafile '//TRIM(sfc_filename)
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF
  ! ...Geometry
  err_stat = CRTM_Geometry_ReadFile( geo_filename, geo, Quiet=QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error reading Geometry datafile '//TRIM(geo_filename)
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF


  ! Set the profile count/step information
  n_profiles_total = SIZE(atm)
  n_profiles       = SIZE(atm(::n_profiles_step))


  ! Write the profile subsets for easier accounting if necessary
  IF ( n_profiles_step > 1 ) THEN
    ! ...Atmosphere
    subset_filename = 'subset.'//TRIM(atm_filename)
    err_stat = CRTM_Atmosphere_WriteFile( subset_filename, atm(::n_profiles_step), Quiet=QUIET )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error writing subset Atmosphere datafile '//TRIM(subset_filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat )
    END IF
    ! ...Surface
    subset_filename = 'subset.'//TRIM(sfc_filename)
    err_stat = CRTM_Surface_WriteFile( subset_filename, sfc(::n_profiles_step), Quiet=QUIET )  
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading subset Surface datafile '//TRIM(subset_filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat )
    END IF
    ! ...Geometry
    subset_filename = 'subset.'//TRIM(geo_filename)
    err_stat = CRTM_Geometry_WriteFile( subset_filename, geo(::n_profiles_step), Quiet=QUIET )  
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading subset Geometry datafile '//TRIM(subset_filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, err_stat )
    END IF
  END IF


  ! Initialise the CRTM
  info_msg = 'Initializing CRTM for '//TRIM(sensor_id(1))//'...'
  CALL Display_Message( PROGRAM_NAME, info_msg, INFORMATION )
  
  err_stat = CRTM_Init( sensor_id, chinfo, &
                        Load_AerosolCoeff = .FALSE., &
                        Quiet=QUIET )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error initializing CRTM for '//TRIM(sensor_id(1))
    CALL Display_Message( PROGRAM_NAME, err_msg, err_stat ); STOP
  END IF


  ! Allocate various output arrays
  n_channels = CRTM_ChannelInfo_n_Channels(chinfo(1))
  ALLOCATE( rts_cloud(  n_channels, 1), &
            rts_clear(  n_channels, 1), &
            rts_maximum(n_channels, 1), &
            rts_random( n_channels, 1), &
            rts_maxran( n_channels, 1), &
            rts_average(n_channels, 1), &
            tb_maximum(n_channels), &
            tb_random( n_channels), &
            tb_maxran( n_channels), &
            tb_average(n_channels), &
            STAT   = alloc_stat, &
            ERRMSG = alloc_msg   )
  IF ( alloc_stat /= 0 ) THEN
    err_msg = 'Error allocating output arrays - '//TRIM(alloc_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  ! ...Allocate the options internals
  CALL CRTM_Options_Create(opt, n_channels)
  IF ( .NOT. CRTM_Options_Associated(opt(1)) ) THEN
    err_msg = 'Error allocating options structure'
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF

  
  ! Define a fixed emissivity for the tests
  CALL CRTM_Options_SetEmissivity(opt(1), 0.75_fp)


  ! Open the output data file
  output_filename = TRIM(sensor_id(1))//'.Cloud_Fraction_results.dat'
  OPEN( FILE    = output_filename, &
        NEWUNIT = fid, &
        FORM    = 'UNFORMATTED', &
        ACCESS  = 'SEQUENTIAL', &
        STATUS  = 'REPLACE', &
        IOSTAT  = io_stat, &
        IOMSG   = io_msg )
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error opening output filename '//TRIM(output_filename)//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF
  ! ...Write the channel and profile dimensions
  WRITE(fid, IOSTAT = io_stat, IOMSG = io_msg) n_channels, n_profiles
  IF ( io_stat /= 0 ) THEN
    err_msg = 'Error writing channel and profile dimension to '//TRIM(output_filename)//' - '//TRIM(io_msg)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! Perform the cloud overlap calculations
  ! ...Initialise RNG for maximum cloud fraction
  CALL RANDOM_SEED()
  ! ...Loop over selected profiles
  m_step = 0
  Profile_Loop: DO m = 1, n_profiles_total, n_profiles_step

    m_step = m_step + 1
    IF ( m_step == 1 .OR. MOD(m_step,100) == 0 ) &
      PRINT *, 'Profile #',m_step,'... number of clouds: ',atm(m)%n_Clouds


    ! Compute the different cloud covers
    err_stat = scc_maximum%Compute_CloudCover(atm(m), Overlap = CloudCover_Maximum_Overlap())
    err_stat =  scc_random%Compute_CloudCover(atm(m), Overlap = CloudCover_Random_Overlap() )
    err_stat =  scc_maxran%Compute_CloudCover(atm(m), Overlap = CloudCover_MaxRan_Overlap() )
    err_stat = scc_average%Compute_CloudCover(atm(m), Overlap = CloudCover_Average_Overlap())


    ! Write profile results to the test output file
    ! ...The profile dimensions
    WRITE(fid, IOSTAT = io_stat, IOMSG = io_msg) atm(m)%n_Layers, atm(m)%n_Clouds
    IF ( io_stat /= 0 ) THEN
      WRITE(err_msg,'("Error writing profile #",i0," dimensions to ",a," - ",a)') &
                    m_step, TRIM(output_filename), TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...The profile information
    WRITE(fid, IOSTAT = io_stat, IOMSG = io_msg) &
      m                      , & ! The original profile number
      m_step                 , & ! The subset profile index
      atm(m)%Pressure        , &
      atm(m)%Temperature     , &
      atm(m)%Cloud_Fraction  , & ! The constructed cloud fraction
      scc_maximum%cloud_cover, &
      scc_random%cloud_cover , &
      scc_maxran%cloud_cover , &
      scc_average%cloud_cover
    IF ( io_stat /= 0 ) THEN
      WRITE(err_msg,'("Error writing profile #",i0," results to ",a," - ",a)') &
                    m_step, TRIM(output_filename), TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF


    ! Run the CRTM
    ! ...Fractional cloudy, maximum overlap
    CALL CRTM_Options_SetValue(opt, Set_Maximum_Overlap = .TRUE.)
    err_stat = CRTM_Forward( [atm(m)]   , &
                             [sfc(m)]   , &
                             [geo(m)]   , &
                             chinfo     , &
                             rts_maximum, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; fractional cloudy, maximum overlap'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Fractional cloudy, random overlap
    CALL CRTM_Options_SetValue(opt, Set_Random_Overlap = .TRUE.)
    err_stat = CRTM_Forward( [atm(m)]  , &
                             [sfc(m)]  , &
                             [geo(m)]  , &
                             chinfo    , &
                             rts_random, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; fractional cloudy, random overlap'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Fractional cloudy, maximum-random overlap
    CALL CRTM_Options_SetValue(opt, Set_MaxRan_Overlap = .TRUE.)
    err_stat = CRTM_Forward( [atm(m)]  , &
                             [sfc(m)]  , &
                             [geo(m)]  , &
                             chinfo    , &
                             rts_maxran, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; fractional cloudy, maximum-random overlap'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...Fractional cloudy, average overlap
    CALL CRTM_Options_SetValue(opt, Set_Average_Overlap = .TRUE.)
    err_stat = CRTM_Forward( [atm(m)]   , &
                             [sfc(m)]   , &
                             [geo(m)]   , &
                             chinfo     , &
                             rts_average, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; fractional cloudy, average overlap'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...100% Cloudy
    atm(m)%Cloud_Fraction = 1.0_fp
    err_stat = CRTM_Forward( [atm(m)] , &
                             [sfc(m)] , &
                             [geo(m)] , &
                             chinfo   , &
                             rts_cloud, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; overcast'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF
    ! ...100% Clear
    atm(m)%Cloud_Fraction = 0.0_fp  ! Not necessary, but what the hey...
    atm(m)%n_Clouds = 0
    err_stat = CRTM_Forward( [atm(m)] , &
                             [sfc(m)] , &
                             [geo(m)] , &
                             chinfo   , &
                             rts_clear, &
                             Options = opt )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error when calling CRTM_Forward; clear'
      CALL Display_Message(PROGRAM_NAME, err_msg, err_stat ); STOP
    END IF


    ! Compute TCC averaged brightness temperature results from 100% clear/cloudy calculations
    Channel_Loop: DO l = 1, n_channels
      rad_cloud = rts_cloud(l,1)%Radiance
      rad_clear = rts_clear(l,1)%Radiance
      rad_maximum = (ONE - scc_maximum%total_cloud_cover)*rad_clear + (scc_maximum%total_cloud_cover*rad_cloud)
      rad_random  = (ONE - scc_random%total_cloud_cover )*rad_clear + (scc_random%total_cloud_cover *rad_cloud)
      rad_maxran  = (ONE - scc_maxran%total_cloud_cover )*rad_clear + (scc_maxran%total_cloud_cover *rad_cloud)
      rad_average = (ONE - scc_average%total_cloud_cover)*rad_clear + (scc_average%total_cloud_cover*rad_cloud)
      ! Convert rad->tb explicitly here
      CALL CRTM_Planck_Temperature(1, l, rad_maximum, tb_maximum(l))
      CALL CRTM_Planck_Temperature(1, l, rad_random , tb_random(l) )
      CALL CRTM_Planck_Temperature(1, l, rad_maxran , tb_maxran(l) )
      CALL CRTM_Planck_Temperature(1, l, rad_average, tb_average(l))
    END DO Channel_Loop


    ! Write radiance/brightness temperature results to the test output file
    ! ...The "internal" TCC averaged brightness temperatures
    WRITE(fid, IOSTAT = io_stat, IOMSG = io_msg) &
      rts_maximum(:,1)%Brightness_Temperature, &
      rts_random (:,1)%Brightness_Temperature, &
      rts_maxran (:,1)%Brightness_Temperature, &
      rts_average(:,1)%Brightness_Temperature
    IF ( io_stat /= 0 ) THEN
      WRITE(err_msg,'("Error writing profile #",i0," internal Tb results to ",a," - ",a)') &
                    m_step, TRIM(output_filename), TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...The "external" TCC averaged brightness temperatures
    WRITE(fid, IOSTAT = io_stat, IOMSG = io_msg) &
      tb_maximum, tb_random, tb_maxran, tb_average
    IF ( io_stat /= 0 ) THEN
      WRITE(err_msg,'("Error writing profile #",i0," external Tb results to ",a," - ",a)') &
                    m_step, TRIM(output_filename), TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF


    ! Clean up profile-based entities
    CALL scc_maximum%Destroy()
    CALL scc_random%Destroy()
    CALL scc_maxran%Destroy()
    CALL scc_average%Destroy()

  END DO Profile_Loop


  ! Close the test output file
  CLOSE(fid)


  ! Clean up everything else
  err_stat = CRTM_Destroy(chinfo)
  DEALLOCATE( atm, sfc, geo, &
              rts_cloud, rts_clear, &
              tb_maximum, tb_random, tb_maxran, tb_average, &
              rts_maximum, &
              rts_random , &
              rts_maxran , &
              rts_average  )

END PROGRAM Cloud_Fraction
