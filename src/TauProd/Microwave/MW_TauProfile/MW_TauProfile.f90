!
! NAME:
!       MW_TauProfile
!
! PURPOSE:
!       Program to general microwave TauProfile datafiles based
!       on oSRF inputs.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Sep-2013
!                       paul.vandelst@noaa.gov
!

PROGRAM MW_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds                , ONLY: fp
  USE Message_Handler           , ONLY: SUCCESS, FAILURE, WARNING, &
                                        Display_Message, Program_Message
  USE Profile_Utility           , ONLY: ID_H2O    , &
                                        PPMV_UNITS, &
                                        ND_UNITS  , &
                                        MR_UNITS  , &
                                        MD_UNITS  , &
                                        PP_UNITS  , &
                                        PPMV_to_PP, & 
                                        ND_to_PP  , & 
                                        MR_to_PP  , & 
                                        MD_to_PP   
  USE Timing_Utility            , ONLY: Timing_type , &
                                        Timing_Begin, &
                                        Timing_End  , &
                                        Timing_Display
  USE Spectral_Units_Conversion , ONLY: inverse_cm_to_GHz
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE PtrArr_Define             , ONLY: PtrArr_type, &
                                        PtrArr_FromVector, &
                                        PtrArr_ToVector
  USE oSRF_Define               , ONLY: oSRF_type, &
                                        oSRF_Destroy , &
                                        oSRF_GetValue, &
                                        oSRF_Convolve
  USE oSRF_File_Define          , ONLY: oSRF_File_type, &
                                        oSRF_File_GetValue, &
                                        oSRF_File_GetFrom , &
                                        oSRF_File_Read    , &
                                        oSRF_File_Destroy
  USE MWLBL_Transmittance
  USE CRTM_GeometryInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'MW_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! Keyword argument set value
  INTEGER, PARAMETER :: SET = 1
  ! direction names
  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(N_DIRECTIONS) = [ 'upwelling  ', &
                                                              'downwelling'  ]

  ! Model names
  INTEGER, PARAMETER :: N_MODELS = 2
  INTEGER, PARAMETER :: MODEL_LIEBE      = 1
  INTEGER, PARAMETER :: MODEL_ROSENKRANZ = 2
  INTEGER, PARAMETER :: MODEL_INDEX(N_MODELS) = [ MODEL_LIEBE, &
                                                  MODEL_ROSENKRANZ ]
  CHARACTER(*), PARAMETER :: MODEL_NAME(N_MODELS) = [ 'Liebe89/93  ', &
                                                      'Rosenkranz03'  ]
  ! Define the secant of the zenith angles to be used
  INTEGER, PARAMETER :: N_ANGLES = 7
  REAL(fp), PARAMETER :: ANGLE_SECANT(N_ANGLES) = &
    [ 1.00_fp, 1.25_fp, 1.50_fp, &
      1.75_fp, 2.00_fp, 2.25_fp, &
      3.00_fp ]
  ! The molecular set IDs.
  !   1 == WLO; Wet lines only
  !  10 == ALL; All absorbers with continua
  !  12 == WET; Wet (and the wet continua)
  !  13 == DRY; Dry (and the dry continua)
  !  15 == WCO; Wet continua only
  ! 101 == EffWLO; Effective wet lines (wet/wco)
  ! 113 == EffDRY; Effective dry (all/wet)
  INTEGER, PARAMETER :: N_MOLECULE_SETS = 7
  INTEGER, PARAMETER :: MOLECULE_SET_LIST( N_MOLECULE_SETS ) = &
    [ 1, 10, 12, 13, 15, 101, 113 ]
  CHARACTER(*), PARAMETER :: MOLECULE_SET_NAME( N_MOLECULE_SETS ) = & 
    [ 'wlo   ', 'all   ', 'wet   ', 'dry   ', 'wco   ', 'effwlo', 'effdry' ]
  INTEGER, PARAMETER :: WLO_IDX = 1
  INTEGER, PARAMETER :: ALL_IDX = 2
  INTEGER, PARAMETER :: WET_IDX = 3
  INTEGER, PARAMETER :: DRY_IDX = 4
  INTEGER, PARAMETER :: WCO_IDX = 5
  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = 6
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = 7
  ! Numeric literal
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ONE)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: err_stat
  INTEGER :: alloc_stat
  INTEGER :: IO_Status
  INTEGER :: direction
  LOGICAL :: downwelling
  INTEGER :: Model
  LOGICAL :: rosenkranz
  CHARACTER(256)             :: sinfo_filename
  TYPE(SensorInfo_type)      :: sinfo
  TYPE(Sensorinfo_List_type) :: sinfo_list
  CHARACTER(256)        :: atmprofile_filename
  TYPE(AtmProfile_type), ALLOCATABLE :: atmprofile(:)
  CHARACTER(256)        :: tauprofile_filename
  TYPE(TauProfile_type) :: tauprofile
  CHARACTER(256)        :: osrf_filename
  TYPE(oSRF_File_type)  :: osrf_file
  TYPE(oSRF_type)       :: osrf
  CHARACTER(256)        :: osrf_history, osrf_comment
  CHARACTER(256)        :: Comment
  INTEGER :: i ! N_ANGLES
  INTEGER :: j ! N_MOLECULE_SETS
  INTEGER :: k, n_layers
  INTEGER :: l, n_channels
  INTEGER :: n_frequencies
  INTEGER :: m, n_profiles
  INTEGER :: n, n_sensors
  INTEGER :: n_bands
  INTEGER, ALLOCATABLE :: n_points(:)
  CHARACTER(256) :: profile_set = ''
  CHARACTER(512) :: mwlbl_version_id
  INTEGER :: j_idx(1)
  INTEGER :: index_h2o
  REAL(fp), ALLOCATABLE :: h2o_pressure(:)
  REAL(fp), ALLOCATABLE :: frequency(:), wavenumber(:)
  REAL(fp), ALLOCATABLE, TARGET  :: tauall(:,:,:)     ! L x K x I
  REAL(fp), ALLOCATABLE, TARGET  :: tauwlo(:,:,:)     ! L x K x I
  REAL(fp), ALLOCATABLE, TARGET  :: tauwco(:,:,:)     ! L x K x I
  REAL(fp), ALLOCATABLE, TARGET  :: tauwet(:,:,:)     ! L x K x I
  REAL(fp), ALLOCATABLE, TARGET  :: taudry(:,:,:)     ! L x K x I
  REAL(fp),              POINTER :: tau(:,:,:) => NULL()
  REAL(fp) :: integrated_srf
  REAL(fp) :: zenith_radian
  TYPE(Timing_type) :: sensor_timing, total_timing
  TYPE(PtrArr_type), ALLOCATABLE :: passband_frequency(:)
  TYPE(PtrArr_type), ALLOCATABLE :: tau_bands(:)
  

  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to compute transmittance profiles for user-defined '//&
                        'microwave instruments.', &
                        '$Revision$' )

  ! Get the calculation direction
  WRITE( *,FMT='(/5x,"Select atmospheric path")' )
  DO i = 1, N_DIRECTIONS
    WRITE( *,FMT='(10x,i1,") ",a)' ) i, DIRECTION_NAME(i)
  END DO
  WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
  READ( *,FMT='(i5)',IOSTAT=IO_Status ) direction
  IF ( IO_Status /= 0 ) THEN
    msg = 'Invalid ATMOSPHERIC PATH identifier input.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  IF ( direction < 1 .OR. direction > N_DIRECTIONS ) THEN
    msg = 'Invalid ATMOSPERIC PATH identifier value.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  ENDIF
  downwelling = .FALSE.
  IF ( TRIM(DIRECTION_NAME(direction)) == 'downwelling' ) THEN
    downwelling = .TRUE.
  END IF
  ! ...The absorption model
  model      = 2       ! Index
  rosenkranz = .TRUE.  ! Flag


  ! Read the SensorInfo file
  WRITE( *,FMT='(/5x,"Enter the SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) sinfo_filename
  sinfo_filename = ADJUSTL(sinfo_filename)
  err_stat = Read_SensorInfo( sinfo_filename, sinfo_list, Quiet = SET )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading SensorInfo file '//TRIM(sinfo_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Count the number of sensors
  n_sensors = Count_SensorInfo_Nodes( sinfo_list )
  IF ( n_sensors < 1 ) THEN
    msg = 'SensorInfo list is empty.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  WRITE( *,'(7x,"Number of sensor entries read: ",i0)' ) n_sensors


  ! Read the AtmProfile file
  WRITE( *,FMT='(/5x,"Enter the netCDF AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) atmprofile_filename
  atmprofile_filename = ADJUSTL(atmprofile_filename)
  ! ...Inquire the file for dimensions
  err_stat = AtmProfile_netCDF_InquireFile( atmprofile_filename, n_Profiles = n_profiles )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring AtmProfile input file '//TRIM(atmprofile_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Allocate the AtmProfile array
  ALLOCATE( atmprofile( n_profiles ), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    msg = 'Error allocating AtmProfile array'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Read the data
  err_stat = AtmProfile_netCDF_ReadFile( &
    atmprofile, &
    atmprofile_filename, &
    Profile_Set = profile_set, &
    Quiet = .TRUE. )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading AtmProfile file '//TRIM(atmprofile_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  WRITE( *,'(7x,"Number of profiles read: ",i0)' ) n_profiles
  ! ...Assign layer dimension to local variable
  n_layers = atmprofile(1)%n_Layers 



  ! Convert water vapour to partial pressure
  H2O_Convert_Loop: DO m = 1, n_profiles

    ! Determine the H2O index in the absorber array
    n = COUNT( atmprofile(m)%Absorber_ID == ID_H2O )
    IF ( n == 0 ) THEN
      WRITE( msg,'("No H2O in absorber set for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    IF ( n > 1 ) THEN
      WRITE( msg,'("More than one H2O identifier in absorber set for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    j_idx = PACK((/(j,j=1,atmprofile(m)%n_Absorbers)/), atmprofile(m)%Absorber_ID == ID_H2O )
    index_h2o = j_idx(1)
    
    ! Allocate the H2O partial pressure array for units conversion
    ALLOCATE( h2o_pressure( atmprofile(m)%n_Layers ), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error allocating H2O partial pressure array for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF

    ! Convert the water vapor amounts
    SELECT CASE ( atmprofile(m)%Absorber_Units_ID( index_h2o ) )
      ! Convert from ppmv
      CASE ( PPMV_UNITS )
        CALL PPMV_to_PP( atmprofile(m)%Layer_Pressure, &
                         atmprofile(m)%Layer_Absorber(:,index_h2o), &
                         h2o_pressure )
      ! Convert from number density
      CASE ( ND_UNITS )
        CALL ND_to_PP( atmprofile(m)%Layer_Absorber(:,index_h2o), &
                       atmprofile(m)%Layer_Temperature, &
                       h2o_pressure )
      ! Convert from mixing ratio
      CASE ( MR_UNITS )
        CALL MR_to_PP( atmprofile(m)%Layer_Pressure, &
                       atmprofile(m)%Layer_Absorber(:,index_h2o), &
                       h2o_pressure )
      ! Convert from mass density
      CASE ( MD_UNITS )
        CALL MD_to_PP( atmprofile(m)%Layer_Absorber(:,index_h2o), &
                       atmprofile(m)%Layer_Temperature, &
                       h2o_pressure )
      ! Partial pressure input
      CASE ( PP_UNITS )
        h2o_pressure = atmprofile(m)%Layer_Absorber(:,index_h2o)
      ! Any other input
      CASE DEFAULT
        WRITE( msg,'("Unrecognised water vapour units for AtmProfile #",i0)' ) m
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END SELECT

    ! Check the result
    IF ( ANY(h2o_pressure < ZERO) ) THEN
      WRITE( msg,'("Error converting water vapor units to hPa for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF

    ! Save the partial pressure in the structure array
    atmprofile(m)%Layer_Absorber(:,index_h2o) = h2o_pressure
    ! ...Update the absorber units ID
    atmprofile(m)%Absorber_Units_ID(index_h2o) = PP_UNITS
    ! ...Deallocate the H2O partial pressure array
    DEALLOCATE( h2o_pressure, STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) THEN
      WRITE( msg,'("Error deallocating H2O partial pressure array for AtmProfile #",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    
  END DO H2O_Convert_Loop


  ! Get the LBL module id
  CALL MWLBL_Transmittance_Version(mwlbl_version_id)
  
  
  ! Begin the total timer
  CALL Timing_Begin(total_timing)


  ! Begin the loop over sensors
  Sensor_Loop: DO n = 1, n_sensors

    ! Begin the sensor timer
    CALL Timing_Begin(sensor_timing)

    ! Get the current sensor
    err_stat = GetFrom_Sensorinfo_List( sinfo_list, n, sinfo )
    IF ( err_stat /= SUCCESS ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF

    ! Only process microwave sensors
    IF ( sinfo%Sensor_Type /= MICROWAVE_SENSOR ) CYCLE Sensor_Loop

    ! Output an info message
    WRITE( *,'(//2x,"Calculating ",a," transmittances for ",a,/)' ) &
             TRIM(DIRECTION_NAME(direction)), TRIM(sinfo%Sensor_id)

    ! Construct sensor filenames
    osrf_filename = TRIM(sinfo%Sensor_Id)//'.osrf.nc'


    ! Read the spectral response function data file
    err_stat = oSRF_File_Read( osrf_file, osrf_filename )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading oSRF data from '//TRIM(osrf_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Retrieve the oSRF file attributes
    err_stat = oSRF_File_GetValue( &
      osrf_file                , &
      n_Channels = n_channels  , &
      History    = osrf_history, &
      Comment    = osrf_comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error retrieving attributes from'//TRIM(osrf_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Allocate the TauProfile structure
    err_stat = Allocate_TauProfile( n_layers       , &
                                    n_channels     , &
                                    N_ANGLES       , &
                                    n_profiles     , &
                                    N_MOLECULE_SETS, &
                                    tauprofile       )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error allocating TauProfile structure for '//TRIM(sinfo%Sensor_Id)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Begin loop over channels
    Channel_Loop: DO l = 1, n_channels

      WRITE( *,'(/4x,"Processing channel #",i0,"....")' ) sinfo%Sensor_Channel(l)


      ! Get the current oSRF object from the file container
      err_stat = oSRF_File_GetFrom( &
        osrf_file, &
        osrf     , &
        pos = l )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving oSRF #",i0," from ",a)') l, TRIM(osrf_filename)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF


      ! Get the band and frequency dimensions, and the SRF integral
      err_stat = oSRF_GetValue( &
        osrf, &
        n_Bands            = n_bands, &
        n_Points           = n_points, &
        Integral           = integrated_srf, &
        Passband_Frequency = passband_frequency )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving oSRF #",i0," attributes")') l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      ! ...The total number of points for all bands
      n_frequencies = SUM(n_points)


      ! Allocate the LBL and PtrArr transmittance arrays
      ALLOCATE( tauall( n_frequencies, n_layers, N_ANGLES ), &
                tauwlo( n_frequencies, n_layers, N_ANGLES ), &
                tauwco( n_frequencies, n_layers, N_ANGLES ), &
                tauwet( n_frequencies, n_layers, N_ANGLES ), &
                taudry( n_frequencies, n_layers, N_ANGLES ), &
                tau_bands( n_bands ), &
                STAT=alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg,'("Error allocating frequency and transmittance arrays for ",a," oSRF #",i0)') &
                   TRIM(sinfo%Sensor_Id), l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      
      
      ! Transfer frequencies to contiguous vector
      err_stat = PtrArr_ToVector( passband_frequency, wavenumber )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error transferring passband frequencies to array for oSRF #",i0)') l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      frequency = inverse_cm_to_GHz(wavenumber)

      

      ! Begin profile loop
      Profile_Loop: DO m = 1, n_profiles

        WRITE( *,'(6x,"Processing profile #",i0,"....")' ) m
        n_layers = atmprofile(m)%n_Layers

   
        ! Call the LBL transmittance function
        err_stat = MWLBL_Compute_Tau( atmprofile(m)%Layer_Pressure,              &  ! Input
                                      atmprofile(m)%Layer_Temperature,           &  ! Input
                                      atmprofile(m)%Layer_Absorber(:,index_h2o), &  ! Input
                                      atmprofile(m)%Layer_Delta_Z,               &  ! Input
                                      ANGLE_SECANT,                              &  ! Input
                                      frequency,                                 &  ! Input
                                      tauall,                                    &  ! Output
                                      tauwlo,                                    &  ! Output
                                      tauwco,                                    &  ! Output
                                      tauwet,                                    &  ! Output
                                      taudry,                                    &  ! Output
                                      Downwelling=downwelling,                   &  ! Optional input
                                      Rosenkranz =rosenkranz,                    &  ! Optional input
                                      Quiet      =.TRUE.                         )  ! Optional input
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error computing MW LBL transmittance for profile #",i0,&
                      &", channel ",i0," of ",a)' ) &
                      m, sinfo%Sensor_Channel(l), TRIM(sinfo%Sensor_Id)
          CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
        END IF

        ! Average the data over the frequencies, but not
        ! for the effective transmittances
        DO j = 1, N_MOLECULE_SETS - 2  ! Last two are effective WLO and DRY

          ! Use a pointer to the particular transmittances
          SELECT CASE (j)
            CASE (WLO_IDX)
              tau => tauwlo
            CASE (ALL_IDX)
              tau => tauall
            CASE (WET_IDX)
              tau => tauwet
            CASE (DRY_IDX)
              tau => taudry
            CASE (WCO_IDX)
              tau => tauwco
            CASE DEFAULT
              WRITE( msg,'("Invalid molecule list index found at profile #",i0,&
                          &", channel ",i0," of ",a)' ) &
                          m, sinfo%Sensor_Channel(l), TRIM(sinfo%Sensor_Id)
              CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
          END SELECT


          ! Loop over the other transmittance dimensions.
          DO i = 1, N_ANGLES
            DO k = 1, n_layers

              ! Construct a PtrArr of the transmittance for convolution
              err_stat = PtrArr_FromVector( tau_bands, n_points, tau(:,k,i) )
              IF ( err_stat /= SUCCESS ) THEN
                WRITE( msg,'("Error extracting transmittance vector for layer# ",i0,&
                            &", angle# ",i0,", and molecule set ",a)' ) &
                            k, i, MOLECULE_SET_NAME(j)
                CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
              END IF
           
              ! Convolve the LBL transmittance with the SRF data
              err_stat = oSRF_Convolve( osrf, tau_bands, tauprofile%Tau(k,l,i,m,j) )
              IF ( err_stat /= SUCCESS ) THEN
                WRITE( msg,'("Error convolving transmittance for layer# ",i0,&
                            &", angle# ",i0,", and molecule set ",a)' ) &
                            k, i, MOLECULE_SET_NAME(j)
                CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
              END IF

            END DO  ! k : 1, n_layers
          END DO  ! i : 1, N_ANGLES
        END DO  ! j : 1, N_MOLECULE_SETS-2
        
        ! Nullify the transmittance array pointer
        NULLIFY( Tau )

        ! Compute the EFFECTIVE transmittances.
        DO i = 1, N_ANGLES
          DO k = 1, n_layers
            ! The EFFECTIVE WLO transmittances
            IF( tauprofile%Tau( k,l,i,m,WCO_IDX ) > TOLERANCE ) THEN
              tauprofile%Tau( k,l,i,m,EFFECTIVE_WLO_IDX ) = tauprofile%Tau( k,l,i,m,WET_IDX ) / &
                                                            tauprofile%Tau( k,l,i,m,WCO_IDX )
            ELSE
              tauprofile%Tau( k,l,i,m,EFFECTIVE_WLO_IDX ) = -ONE
            END IF
            
            ! The EFFECTIVE DRY transmittances
            IF( tauprofile%Tau( k,l,i,m,WET_IDX ) > TOLERANCE ) THEN
              tauprofile%Tau( k,l,i,m,EFFECTIVE_DRY_IDX ) = tauprofile%Tau( k,l,i,m,ALL_IDX ) / &
                                                            tauprofile%Tau( k,l,i,m,WET_IDX )

            ELSE
              tauprofile%Tau( k,l,i,m,EFFECTIVE_DRY_IDX ) = -ONE
            END IF
            
          END DO  ! k : 1, n_layers
        END DO  ! i : 1, N_ANGLES
      
      
        ! Compute the geometric angle profiles
        DO i = 1, N_ANGLES
          zenith_radian = ACOS(ONE/ANGLE_SECANT(i))
          CALL SACONV( zenith_radian, &
                       atmprofile(m)%Level_Altitude(1:atmprofile(m)%n_Layers), &
                       tauprofile%Geometric_Angle(:,i,m) )
        END DO  ! i : 1, N_ANGLES
        
      END DO Profile_Loop
      
      
      ! Deallocate the LBL arrays
      DEALLOCATE( wavenumber, frequency, &
                  tauall, tauwlo, tauwco, tauwet, taudry, &
                  tau_bands, &
                  STAT=alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg,'("Error deallocating frequency and transmittance arrays for ",a," oSRF #",i0)') &
                   TRIM(sinfo%Sensor_Id), l
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF

    END DO Channel_Loop


    ! Output the TauProfile data
    WRITE( Comment,'("Number of points used per channel in LBL calculation: ",i0,&
                    &". Absorption model used: ",a)' ) &
                    n_frequencies, MODEL_NAME(model)
    ! ...Create the output data file
    tauprofile_filename = TRIM(DIRECTION_NAME(direction))//'.'//&
                          TRIM(sinfo%Sensor_Id)//&
                          '.TauProfile.nc'
    err_stat = Create_TauProfile_netCDF( tauprofile_filename, &
                                         atmprofile(1)%Level_Pressure, &
                                         sinfo%Sensor_Channel, &
                                         ANGLE_SECANT, &
                                         (/(m, m=1,n_Profiles)/), &
                                         MOLECULE_SET_LIST, &
                                         Release         =tauprofile%Release, &
                                         Version         =tauprofile%Version, &
                                         Sensor_ID       =sinfo%Sensor_ID, &
                                         WMO_Satellite_ID=sinfo%WMO_Satellite_ID, &
                                         WMO_Sensor_ID   =sinfo%WMO_Sensor_ID, &
                                         ID_Tag = TRIM(profile_set), &
                                         Title = TRIM(sinfo%Sensor_Name)//' '//&
                                                 TRIM(DIRECTION_NAME(direction))//&
                                                 ' transmittances for '//&
                                                 TRIM(sinfo%Satellite_Name), &
                                         History = PROGRAM_VERSION_ID//'; '//&
                                                   TRIM(mwlbl_version_id),  &
                                         Comment = TRIM(Comment) )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error creating netCDF TauProfile file '//TRIM(tauprofile_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...Write the data to file
    err_stat = Write_TauProfile_netCDF( tauprofile_filename, tauprofile, profile_angle=1 )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing TauProfile structure for '//&
            TRIM(sinfo%Sensor_Id)//' to '//TRIM(tauprofile_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Display the sensor timing results
    CALL Timing_End(sensor_timing)
    WRITE( *,'(7x,"Timing for ",a,":")') TRIM(sinfo%Sensor_id)
    CALL Timing_Display(sensor_timing)


    ! Clean up sensor dependent data structures
    ! ...TauProfile
    err_stat = Destroy_TauProfile( tauprofile )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error destroying TauProfile structure for '//TRIM(sinfo%Sensor_Id)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    ! ...oSRF
    CALL oSRF_File_Destroy( osrf_file )
    ! ...SensorInfo
    err_stat = Destroy_SensorInfo( sinfo )
    IF ( err_stat /= SUCCESS ) THEN
      WRITE( msg,'("Error destroying SensorInfo structure for sensor #",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF

  END DO Sensor_Loop


  ! Display the total timing results
  CALL Timing_End(total_timing)
  WRITE( *,'(/5x,"Total time for all sensors:")')
  CALL Timing_Display(total_timing)


  ! Clean up sensor independent data structures
  ! ...The AtmProfile structure array
  DEALLOCATE( atmprofile, STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    msg = 'Error deallocating AtmProfile structure array'
    CALL Display_Message( PROGRAM_NAME, msg, WARNING )
  END IF
  ! ...The linked list
  err_stat = Destroy_Sensorinfo_List( sinfo_list, Quiet=1 )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error destroying SensorInfo linked list.'
    CALL Display_Message( PROGRAM_NAME, msg, WARNING )
  END IF

END PROGRAM MW_TauProfile
