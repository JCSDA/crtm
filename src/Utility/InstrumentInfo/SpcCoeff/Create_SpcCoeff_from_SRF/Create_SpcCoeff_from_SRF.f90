!
! Create_SpcCoeff_from_SRF
!
! Program to create spectral coefficient (SpcCoeff) data files
! from the sensor SRF data files.
!
!
! CREATION HISTORY:
!       Written by:     David Groff 13-Feb-2009 (Friday 13th!)
!                       david.groff@noaa.gov
!

PROGRAM Create_SpcCoeff_from_SRF


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Fundamental_Constants    , ONLY: C_1, C_2
  USE Planck_Functions         , ONLY: Planck_Radiance, Planck_Temperature
  USE Interpolate_Utility      , ONLY: Spline_Initialize, &
                                       Spline_Interpolate
  USE Integrate_Utility        , ONLY: Simpsons_Integral
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_GHz, &
                                       GHz_to_inverse_cm
  USE MW_SensorData_Define     , ONLY: MW_SensorData_type, &
                                       Load_MW_SensorData, &
                                       Destroy_MW_SensorData
  USE SensorInfo_Define        , ONLY: N_SENSOR_TYPES          , &
                                       INVALID_SENSOR          , &
                                       MICROWAVE_SENSOR        , &
                                       INFRARED_SENSOR         , &
                                       VISIBLE_SENSOR          , &
                                       ULTRAVIOLET_SENSOR      , &
                                       SENSOR_TYPE_NAME        , &
                                       UNPOLARIZED             , &
                                       SensorInfo_type         , &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type, &
                                       Count_SensorInfo_Nodes, &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
  USE SRF_Define               , ONLY: SRF_type, &
                                       Destroy_SRF
  USE SRF_netCDF_IO            , ONLY: Inquire_SRF_netCDF, &
                                       Read_SRF_netCDF
  USE Solar_Define             , ONLY: Solar_type, &
                                       Destroy_Solar
  USE Solar_netCDF_IO          , ONLY: Inquire_Solar_netCDF, &
                                       Read_Solar_netCDF
  USE AntCorr_Define           , ONLY: AntCorr_type, &
                                       Assign_AntCorr, &
                                       Destroy_AntCorr
  USE AntCorr_netCDF_IO        , ONLY: Read_AntCorr_netCDF
  USE SpcCoeff_Define          , ONLY: SOLAR_FLAG, &
                                       ZEEMAN_FLAG, &
                                       SpcCoeff_type, &
                                       SetFlag_SpcCoeff, &
                                       ClearFlag_SpcCoeff, &
                                       Allocate_SpcCoeff, &
                                       Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO       , ONLY: Write_SpcCoeff_netCDF

  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_SpcCoeff_from_SRF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp

  ! The following scaling factors are applied to produce radiances in units
  ! of mW/(m^2.sr.cm^-1) when they are used.
  !
  ! First Planck function constant (C_1) scale factors. Units of C_1 are W.m^2.
  ! Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  !                 which is 100, to the fourth power, which is 1.0e+08.
  ! Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  REAL(fp), PARAMETER :: C_1_LENGTH_SCALE_FACTOR = 1.0e+08_fp
  REAL(fp), PARAMETER :: C_1_POWER_SCALE_FACTOR  = 1.0e+03_fp
  REAL(fp), PARAMETER :: C_1_SCALE_FACTOR = C_1_LENGTH_SCALE_FACTOR * C_1_POWER_SCALE_FACTOR
  ! Second Planck function constant (C_2) scale factor. Units of C_2 are K.m,
  ! So to convert to K.cm, a scaling of 100 is applied.
  REAL(fp), PARAMETER :: C_2_SCALE_FACTOR = 100.0_fp

  ! The number and range of temperatures used in determining the 
  ! polychromatic correction coefficients
  INTEGER,  PARAMETER :: N_TEMPERATURES = 17
  REAL(fp), PARAMETER :: MIN_TEMPERATURE = 180.0_fp
  REAL(fp), PARAMETER :: MAX_TEMPERATURE = 340.0_fp

  ! Solar channel cut-off frequency
  REAL(fp), PARAMETER :: SOLAR_CUTOFF_WAVENUMBER = 500.0_fp
  
  ! Cosmic background temperature
  REAL(fp), PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.7253

  ! Integration methods
  INTEGER, PARAMETER :: N_INTEGRATION_METHODS = 2
  INTEGER, PARAMETER :: SUMMATION_METHOD = 1
  INTEGER, PARAMETER :: INTEGRATE_METHOD = 2
  INTEGER, PARAMETER :: INTEGRATION_METHOD(N_INTEGRATION_METHODS) = &
    (/ SUMMATION_METHOD, &
       INTEGRATE_METHOD /)
  CHARACTER(*), PARAMETER :: INTEGRATION_METHOD_NAME(N_INTEGRATION_METHODS) = &
    (/ 'Summation  ', &
       'Integration' /)

  ! Interpolation order
  INTEGER, PARAMETER :: LINEAR_ORDER = 1
  INTEGER, PARAMETER ::  CUBIC_ORDER = 3


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: SRF_Filename
  CHARACTER(256) :: Solar_Filename
  CHARACTER(256) :: SpcCoeff_Filename
  CHARACTER(256) :: ASCII_Filename
  CHARACTER(256) :: AntCorr_Filename
  CHARACTER(256) :: MW_SensorData_History
  INTEGER :: ASCII_FileID
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: l
  INTEGER :: ls1, ls2
  INTEGER :: i, n
  INTEGER :: n_FOVs
  INTEGER :: SRF_Integration_Method
  INTEGER :: SpcCoeff_File_Version
  CHARACTER( 256) :: Title
  CHARACTER(2000) :: SRF_History, Solar_History
  CHARACTER(2000) :: AntCorr_Comment, AntCorr_History
  CHARACTER(2000) :: Comment
  REAL(fp) :: dFrequency
  REAL(fp) :: Integrated_MW_Response
  REAL(fp) :: SRF_Integral
  REAL(fp) :: SRF_First_Moment
  REAL(fp) :: x_Temperature(N_TEMPERATURES)
  REAL(fp) :: y_Effective_Temperature(N_TEMPERATURES)
  REAL(fp) :: yFit_Effective_Temperature(N_TEMPERATURES)
  REAL(fp) :: var_Effective_Temperature
  REAL(fp), ALLOCATABLE :: Solar_Derivative(:)
  REAL(fp), ALLOCATABLE, TARGET :: Spectrum(:)
  REAL(fp), POINTER             :: Radiance(:)   => NULL()
  REAL(fp), POINTER             :: Irradiance(:) => NULL()
  REAL(fp) :: Convolved_Radiance
  REAL(fp) :: v1, v2
  INTEGER :: n_Sensors
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(SRF_type)             :: SRF
  TYPE(Solar_type)           :: Solar
  TYPE(SpcCoeff_type)        :: SpcCoeff
  TYPE(MW_SensorData_type)   :: MW_SensorData
  TYPE(AntCorr_type)         :: AntCorr


  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to create the SpcCoeff '//&
                       'files from the sensor SRF data files.', &
                       '$Revision$' )

  ! Get user inputs
  ! ---------------
  ! The SensorInfo filename and data
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL(SensorInfo_Filename)

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List    , &
                                  Quiet=SET            )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM(SensorInfo_Filename), &
                          FAILURE )
    STOP
  END IF

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF

  ! Integration method
  WRITE( *,FMT='(/5x,"Select SRF integration method")' )
  DO i = 1, N_INTEGRATION_METHODS
    WRITE( *,FMT='(10x,i1,") ",a)' ) i, INTEGRATION_METHOD_NAME(i)
  END DO
  WRITE( *,FMT='(5x,"Enter choice : ")',ADVANCE='NO' )
  READ( *,* ) SRF_Integration_Method
  IF ( SRF_Integration_Method < 1                     .OR. &
       SRF_Integration_Method > N_INTEGRATION_METHODS      ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid integration method. Using summation.', &
                          INFORMATION )
    SRF_Integration_Method = SUMMATION_METHOD
  END IF

  ! The SpcCoeff version
  WRITE( *,FMT='(/5x,"Default SpcCoeff file version is: ",i0, &
               &".  Enter value: ")', &
           ADVANCE='NO' ) SpcCoeff%Version
  READ( *,* ) SpcCoeff_File_Version
  IF ( SpcCoeff_File_Version < SpcCoeff%Version ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    SpcCoeff_File_Version = SpcCoeff%Version
  END IF


  ! Read the netCDF Solar file
  ! --------------------------
  Solar_Filename = 'solar.nc'
  WRITE( *,'(/5x,"Reading the solar irradiance data file ",a,"...")' ) &
            TRIM(Solar_Filename)

  ! Inquire the file to get the history attribute
  Error_Status = Inquire_Solar_netCDF( Solar_Filename, History=Solar_History )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF Solar file '//TRIM(Solar_Filename), &
                          Error_Status )
    STOP
  END IF

  ! Read the file
  Error_Status = Read_Solar_netCDF( Solar_Filename, Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF Solar file '//TRIM(Solar_Filename), &
                          Error_Status )
    STOP
  END IF

  ! Compute the derivatives for
  ! solar spectrum interpolation
  ! ----------------------------
  ! Allocate the solar derivative array
  ALLOCATE( Solar_Derivative( Solar%n_Frequencies ),STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating solar array for interpolation. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF

  ! Initialise the spline
  Error_Status = Spline_Initialize( Solar%Frequency , &
                                    Solar%Irradiance, &
                                    Solar_Derivative  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing solar irradiance derivatives.', &
                          Error_Status )
    STOP
  END IF


  ! Generate the monochromatic temperatures
  ! ---------------------------------------
  x_Temperature = (/(REAL(i-1,fp),i=1,N_TEMPERATURES)/) / REAL(N_TEMPERATURES-1,fp)
  x_Temperature = (x_Temperature * ( MAX_TEMPERATURE-MIN_TEMPERATURE )) + MIN_TEMPERATURE


  ! Begin the main sensor loop
  ! ---------------------------
  Sensor_Loop: DO n = 1, n_Sensors

    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


    ! Only operate on sensors with SRF file present
    ! ---------------------------------------------
    SRF_Filename = TRIM(SensorInfo%Sensor_ID)//'.srf.nc'
    IF ( .NOT. File_Exists( SRF_Filename ) ) CYCLE Sensor_Loop


    ! Output an info message
    ! ----------------------
    WRITE( *,'(//5x,"Creating the SpcCoeff data file for ",a)' ) &
              TRIM(SensorInfo%Sensor_Id)

    ! Read MW_SENSORDATA AND ANTCORR data for microwave
    ! -------------------------------------------------
    IF(SensorInfo%Sensor_Type == MICROWAVE_SENSOR) THEN
    
      ! Load the current microwave sensor data
      ! --------------------------------------
      Error_Status = Load_MW_SensorData( MW_SensorData, &
                                         Sensor_ID=SensorInfo%Sensor_ID, &
                                         RCS_Id   =MW_SensorData_History )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error loading MW sensor data for '//&
                              TRIM(SensorInfo%Sensor_Id), &
                              FAILURE )
        STOP
      END IF
             
      ! Modify GAtts for concatenation
      MW_SensorData_History = '; '//TRIM(MW_SensorData_History)

      ! Load antenna correction data if required
      ! ----------------------------------------
      AntCorr_Filename = TRIM(SensorInfo%Sensor_ID)//'.AntCorr.nc'
      AntCorr_History  = ' '
      AntCorr_Comment  = ' '
      n_FOVs = 0
      IF ( File_Exists(AntCorr_Filename) ) THEN
        
        ! Read the antenna correction data
        Error_Status = Read_AntCorr_netCDF( AntCorr_Filename, &
                                            AntCorr, &
                                            History=AntCorr_History, &
                                            Comment=AntCorr_Comment )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error loading antenna correction data for '//&
                                TRIM(SensorInfo%Sensor_Id), &
                                FAILURE )
          STOP
        END IF
        
        ! Modify GAtts for concatenation
        AntCorr_History = '; AntCorr: '//TRIM(AntCorr_History)
        AntCorr_Comment = '; AntCorr: '//TRIM(AntCorr_Comment)
        
        ! Set the FOV dimension for the SpcCoeff allocation
        n_FOVs = AntCorr%n_FOVs
      END IF
      
    END IF
    ! Inquire the SRF file for its GAtts
    ! ----------------------------------
    Error_Status = Inquire_SRF_netCDF( SRF_Filename         , &  ! Input
                                       Title   = Title      , & ! Optional output
                                       History = SRF_History, & ! Optional output
                                       Comment = Comment      ) ! Optional output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring the netCDF SRF file '//TRIM(SRF_Filename), &
                            Error_Status )
      STOP
    END IF
    
    ! Allocate the SpcCoeff structure
    ! -------------------------------
    Error_Status = Allocate_SpcCoeff( SensorInfo%n_Channels, SpcCoeff, n_FOVs=n_FOVs )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SpcCoeff data structure.', &
                            FAILURE )
      STOP
    END IF
      
    ! Assign the sensor-independent data components
    ! ---------------------------------------------
    SpcCoeff%Version          = SpcCoeff_File_Version
    SpcCoeff%Sensor_Id        = SensorInfo%Sensor_Id
    SpcCoeff%Sensor_Type      = SensorInfo%Sensor_Type
    SpcCoeff%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
    SpcCoeff%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID
    SpcCoeff%Sensor_Channel   = SensorInfo%Sensor_Channel
    ! Initialise
    SpcCoeff%Solar_Irradiance = ZERO
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,SOLAR_FLAG)
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,ZEEMAN_FLAG)

    ! Assign the simple sensor-dependent data components
    ! --------------------------------------------------
    SELECT CASE (SpcCoeff%Sensor_Type)
      CASE (MICROWAVE_SENSOR)
        SpcCoeff%Frequency    = MW_SensorData%Central_Frequency
        SpcCoeff%Polarization = MW_SensorData%Polarization
        ! The antenna correction data
        IF ( SpcCoeff%AC_Present ) THEN
          ! Copy it
          Error_Status = Assign_AntCorr( AntCorr, SpcCoeff%AC )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error copying antenna correction data into SpcCoeff structure for '//&
                                  TRIM(SensorInfo%Sensor_Id), &
                                  FAILURE )
            STOP
          END IF
          ! Destroy it
          Error_Status = Destroy_AntCorr( AntCorr )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error destroying AntCorr structure for '//&
                                  TRIM(SensorInfo%Sensor_Id), &
                                  FAILURE )
            STOP
          END IF
        END IF
        
      CASE (INFRARED_SENSOR)
        SpcCoeff%Polarization = UNPOLARIZED

      CASE (VISIBLE_SENSOR)
        SpcCoeff%Polarization = UNPOLARIZED

      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME,'Unsupported sensor type!',FAILURE )
        STOP
    END SELECT
    


    ! Open file for band correction fit output
    ! ----------------------------------------
    ASCII_Filename = TRIM(SensorInfo%Sensor_Id)//'.SpcCoeff.asc'
    ASCII_FileID = Get_Lun()
    IF ( ASCII_FileID < 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error obtaining file unit number for output to '//&
                            TRIM(ASCII_Filename), &
                            FAILURE )
      STOP
    END IF
    OPEN( ASCII_FileID, FILE  =TRIM(ASCII_Filename), &
                        ACCESS='SEQUENTIAL', &
                        FORM  ='FORMATTED', &
                        STATUS='REPLACE', &
                        ACTION='WRITE', &
                        IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error opening statistics output file ",a,". STAT = ",i0)' ) &
                     TRIM(ASCII_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    WRITE( ASCII_FileID,'(5x,"SRF data from file: ",a)' ) TRIM(SRF_Filename)




    ! Begin loop over channels
    ! ------------------------
    WRITE( *,'(/,"    CH        V0            ",&
                &"FK1            FK2            BC1            BC2          ",&
                &"CBR            F(Solar)       POLARIZATION")' )

    Channel_Loop: DO l = 1, SpcCoeff%n_Channels
        
      WRITE( *,FMT='(2x,i4)',ADVANCE='NO') SpcCoeff%Sensor_Channel(l)

      ! Set the Zeeman flag if necessary
      ! --------------------------------
      IF ( SpcCoeff%Sensor_Type == MICROWAVE_SENSOR ) THEN
        ! Compute the MW response
        Integrated_MW_Response = SUM(MW_SensorData%Response(:,l)) * MW_SensorData%Delta_Frequency(l)
        IF ( MW_SensorData%Zeeman(l) == SET ) &
          CALL SetFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),ZEEMAN_FLAG)
      END IF
      

      ! Read the current SRF data
      ! -------------------------
      Error_Status = Read_SRF_netCDF( SRF_Filename              , &  ! Input
                                      SpcCoeff%Sensor_Channel(l), &  ! Input
                                      SRF                       , &  ! Output
                                      Quiet=SET                   )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error reading channel #",i0," SRF from ",a)' ) &
                       SpcCoeff%Sensor_Channel(l), TRIM(SRF_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF


      ! Define the integrated SRF and first moment
      ! ------------------------------------------
      IF ( SRF_Integration_Method == SUMMATION_METHOD ) THEN
        ! via Summation
        SRF_Integral = SRF%Summation_SRF
        dFrequency   = SRF%Frequency(2)-SRF%Frequency(1)
        SRF_First_Moment = SUM(SRF%Response*SRF%Frequency)*dFrequency
      ELSE
        ! via Integration
        SRF_Integral = SRF%Integrated_SRF
        Error_Status = Simpsons_Integral( SRF%Frequency, &
                                          SRF%Response * SRF%Frequency, &
                                          SRF_First_Moment, &
                                          Order = CUBIC_ORDER )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error occurred calculating SRF first moment by integration.', &
                                FAILURE )
          STOP
        END IF
      END IF

      ! Determine the SRF centroid and Planck coefficients
      ! --------------------------------------------------
      SELECT CASE (SpcCoeff%Sensor_Type)
        CASE (MICROWAVE_SENSOR)
          SpcCoeff%Wavenumber(l) = GHz_to_inverse_cm( SpcCoeff%Frequency(l) )

        CASE (INFRARED_SENSOR, VISIBLE_SENSOR)        
          SpcCoeff%Wavenumber(l) = SRF_First_Moment / SRF_Integral
          SpcCoeff%Frequency(l)  = Inverse_cm_to_GHz( SpcCoeff%Wavenumber(l) )
      END SELECT
      
      ! Compute the coefficients
      SpcCoeff%Planck_C1(l) = C_1_SCALE_FACTOR * C_1 * ( SpcCoeff%Wavenumber(l)**3 )
      SpcCoeff%Planck_C2(l) = C_2_SCALE_FACTOR * C_2 *   SpcCoeff%Wavenumber(l)
      
      WRITE( *,FMT='(3(2x,es13.6))',ADVANCE='NO' ) SpcCoeff%Wavenumber(l), &
                                                   SpcCoeff%Planck_C1(l), &
                                                   SpcCoeff%Planck_C2(l)


      ! Allocate the radiance/irradiance spectrum workarray
      ! ---------------------------------------------------
      ALLOCATE( Spectrum( SRF%n_Points ),STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating spectrum array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      

      ! Compute the polychromatic correction coefficients
      ! -------------------------------------------------
      Radiance => Spectrum
      
      ! Generate the "polychromatic" temperatures
      Temperature_Loop: DO i = 1, N_TEMPERATURES
      
        ! Calculate monochromatic radiances
        Error_Status = Planck_Radiance( SRF%Frequency, &
                                        x_Temperature(i), &
                                        Radiance )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error calculating radiance at T = ",f5.1," K.")' ) &
                          x_Temperature(i)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
        
        ! Convolved radiance calculations
        ! -------------------------------         
        IF (SpcCoeff%Sensor_Type == MICROWAVE_SENSOR) THEN
          ! Convolve the radiance spectrum with the MW response
          Convolved_Radiance = SUM(Radiance)*MW_SensorData%Delta_Frequency(l)/Integrated_MW_Response
        ELSE
         ! Convolve the radiance spectrum with the SRF
          Error_Status = Simpsons_Integral( SRF%Frequency, &
                                            Radiance*SRF%Response, &
                                            Convolved_Radiance, &
                                            Order=CUBIC_ORDER )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error convolving radiance at T = ",f5.1," K.")' ) &
                           x_Temperature(i)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  FAILURE )
            STOP
          END IF       
          ! Normalise the convolved radiance
          Convolved_Radiance = Convolved_Radiance/SRF_Integral
        END IF
        
        ! Convert the convolved radiance back into a temperature
        Error_Status = Planck_Temperature( SpcCoeff%Wavenumber(l),    &
                                           Convolved_Radiance,        &
                                           y_Effective_Temperature(i) )
        IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error calculating polychromatic temperature at T = ",f5.1," K.")' ) &
                        x_Temperature(i)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE)
          STOP
        END IF
      END DO Temperature_Loop
      NULLIFY( Radiance )

      ! Fit the mono- and polychromatic temperatures
      Error_Status = Least_Squares_Linear_Fit( x_Temperature, &
                                               y_Effective_Temperature, &
                                               SpcCoeff%Band_C1(l), &
                                               SpcCoeff%Band_C2(l), &
                                               yFit=yFit_Effective_Temperature, &
                                               MSE =var_Effective_Temperature )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error calculating band correction coefficients', &
                              FAILURE )
        STOP
      END IF
      
      ! Output fit coefficients to screen
      WRITE( *,FMT='(2(2x,es13.6))',ADVANCE='NO' ) SpcCoeff%Band_C1(l), &
                                                   SpcCoeff%Band_C2(l)

      ! Output fit statistics to file
      WRITE( ASCII_FileID,FMT='(/5x,"CHANNEL ",i4)' ) SpcCoeff%Sensor_Channel(l)
      WRITE( ASCII_FileID,FMT='(2x,"Fit equation : Teff = ",es13.6," + ",es13.6," T")' ) &
                              SpcCoeff%Band_C1(l), SpcCoeff%Band_C2(l)
      WRITE( ASCII_FileID,FMT='(2x,"MSE : ",es13.6,";  Sigma : ",es13.6)' ) &
                              var_Effective_Temperature, SQRT(var_Effective_Temperature)
      WRITE( ASCII_FileID,'(7x,"T      Teff(true)   Teff(fit)  dTeff(true-fit)")' )
      WRITE( ASCII_FileID,'(2x,49("-"))' )
      DO i = 1, N_TEMPERATURES
        WRITE( ASCII_FileID,'(3(2x,f10.6),2x,es13.6)' ) &
                             x_Temperature(i), &
                             y_Effective_Temperature(i),  yFit_Effective_Temperature(i), &
                             y_Effective_Temperature(i) - yFit_Effective_Temperature(i)
      END DO
        
      IF(SpcCoeff%Sensor_Type == MICROWAVE_SENSOR) THEN      
        ! Compute the cosmic background radiance
        ! --------------------------------------
        Error_Status = Planck_Radiance( SpcCoeff%Wavenumber(l), &
                                        COSMIC_BACKGROUND_TEMPERATURE, &
                                        SpcCoeff%Cosmic_Background_Radiance(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( *,* )
          WRITE( Message,'("Error computing cosmic background radiance for ",a,&
                          &" channel ", i0,".")' ) &
                          TRIM(SensorInfo%Sensor_Id ), &
                          SpcCoeff%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
      ELSE
        ! Fill the cosmic background field
        ! --------------------------------
        SpcCoeff%Cosmic_Background_Radiance( l ) = ZERO
      END IF
        
      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Cosmic_Background_Radiance(l)
        
      ! Set the solar fields for visible and infrared
      ! ---------------------------------------------
      IF(SpcCoeff%Sensor_Type == INFRARED_SENSOR .OR. &
      SpcCoeff%Sensor_Type == VISIBLE_SENSOR) THEN
        
        ! Interpolate the solar spectrum to the SRF spacing
        Irradiance => Spectrum
        Error_Status = Spline_Interpolate( Solar%Frequency    , &  ! X
                                           Solar%Irradiance   , &  ! Y
                                           SRF%Frequency      , &  ! Xint
                                           Irradiance         , &  ! Yint
                                           y2=Solar_Derivative  )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error interpolating solar spectrum for SRF channel ",i0)' ) &
                         SRF%Channel
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
        print *, maxval(Solar%Frequency), 'Solar Irradiance'
        ! Convolve the irradiance witrh the SRF
        IF ( SRF_Integration_Method == SUMMATION_METHOD ) THEN
          ! via summation
          SpcCoeff%Solar_Irradiance(l) = SUM(SRF%Response*Irradiance) * &
                                         Solar%Frequency_Interval/SRF_Integral
        ELSE
          ! via integration
          Error_Status = Simpsons_Integral( SRF%Frequency, &
                                            SRF%Response * Irradiance, &
                                            SpcCoeff%Solar_Irradiance(l), &
                                            Order=CUBIC_ORDER )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error occurred convoling solar irradiance with SRF.', &
                                  FAILURE )
            STOP
          END IF
          SpcCoeff%Solar_Irradiance(l) = SpcCoeff%Solar_Irradiance(l)/SRF_Integral
        END IF
        NULLIFY( Irradiance )
        
      ENDIF

      ! Set the solar channel flag
      IF ( SpcCoeff%Wavenumber(l) > SOLAR_CUTOFF_WAVENUMBER ) THEN
        CALL SetFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),SOLAR_FLAG)
      ELSE
        CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),SOLAR_FLAG)
      END IF
      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Solar_Irradiance(l)


      ! Deallocate the radiance/irradiance spectrum workarray
      ! -----------------------------------------------------
      DEALLOCATE( Spectrum, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating spectrum array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF


      ! Destroy the current channel SRF structure
      ! -----------------------------------------
      Error_Status = Destroy_SRF( SRF )
      IF ( Error_Status/= SUCCESS ) THEN
        WRITE( Message,'("Error destroying SRF structure for channel ",i0)' ) &
                        SpcCoeff%Sensor_Channel(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE  )
        STOP
      END IF
      
    END DO Channel_Loop


    ! Close the ASCII output file
    ! ---------------------------
    CLOSE( ASCII_FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error closing ASCII statistics file '//&
                            TRIM(ASCII_Filename), &
                            WARNING )
    END IF


    ! Write the SpcCoeff data file
    ! ----------------------------
    SpcCoeff_Filename = TRIM(SensorInfo%Sensor_ID)//'.SpcCoeff.nc'
    
    Error_Status = Write_SpcCoeff_netCDF( TRIM(SpcCoeff_Filename), &
                                          SpcCoeff, &
                                          Title = 'Spectral coefficients for '//&
                                                  TRIM(SensorInfo%Sensor_Id)//&
                                                  ' derived from SRF data file '//&
                                                  TRIM(SRF_Filename)//&
                                                  ' and solar data file '//&
                                                  TRIM(Solar_Filename), &
                                          History = PROGRAM_RCS_ID//&
                                                    '; '//TRIM(SRF_History)//&
                                                    '; '//TRIM(Solar_History), &
                                          Comment = TRIM(Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing netCDF SpcCoeff data file '//&
                            TRIM(SpcCoeff_Filename), &
                            FAILURE )
      STOP
    END IF


    ! Destroy the current sensor data structures
    ! ------------------------------------------
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff data structure for '//&
                            TRIM(SRF_Filename)//' processing.', &
                            FAILURE )
      STOP
    END IF

    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            FAILURE )
      STOP
    END IF
    
  END DO Sensor_loop


  ! Destroy the sensor independent data arrays/structures
  ! -----------------------------------------------------
  DEALLOCATE( Solar_Derivative, STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating solar array for interpolation. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF

  Error_Status = Destroy_Solar(Solar)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar data structure.', &
                          WARNING )
  END IF

  ! Destroy the SensorInfo linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       Least_Squares_Linear_Fit
!
! PURPOSE:
!       Function to perform a least squares linear fit on the input 
!       polychromatic and monochromatic temperature data.
!
! CALLING SEQUENCE:
!       Error_Status = Least_Squares_Linear_Fit( x, y,        &  ! Input
!                                                a, b,        &  ! Output
!                                                yFit = yFit, &  ! Optional output
!                                                SSE  = SSE,  &  ! Optional output
!                                                MSE  = MSE,  &  ! Optional output
!                                                Message_Log = Message_Log ) !  Error messaging
!
! INPUT ARGUMENTS:
!       x:               Input ordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the true temperature.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
!       y:               Input coordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the effective temperature due to
!                        polychromaticity.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (Same size as x)
!                        ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       a:               Offset coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!       b:               Slope coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin/Kelvin (K/K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       yFit:            Predicted coordinate (effective temperature) data,
!                          yFit = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (Same size as y)
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       SSE:             The residual sum of the squares of the fit to the
!                        input data,
!                                 __  N
!                                \                    2
!                          SSE =  > ( Y(i) - YFit(i) )
!                                /__
!                                    i=1
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       MSE:             The residual mean square of the fit to the
!                        input data,
!                                
!                                 SSE
!                          MSE = -----
!                                 N-2
!
!                        where N == number of input data points
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the ERROR_HANDLER module.
!                        If == SUCCESS the regression fit was successful.
!                           == FAILURE  an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Least_Squares_Linear_Fit( x, y,         &  ! Input
                                     a, b,         &  ! Output
                                     yFit,         &  ! Optional output
                                     SSE,          &  ! Optional output
                                     MSE,          &  ! Optional output
                                     Message_Log ) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    REAL(fp),                INTENT(IN)  :: x(:)
    REAL(fp),                INTENT(IN)  :: y(:)
    REAL(fp),                INTENT(OUT) :: a
    REAL(fp),                INTENT(OUT) :: b
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: yFit(:)
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: SSE
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: MSE
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Least_Squares_Linear_Fit'
    REAL(fp),     PARAMETER :: TOLERANCE = EPSILON(1.0_fp)
    ! Local variables
    INTEGER :: n
    REAL(fp) :: xAverage
    REAL(fp) :: yAverage
    REAL(fp) :: sum_dx2
    REAL(fp) :: yCalculated(SIZE(y))
    REAL(fp) :: Residual_Sum_of_Squares
    REAL(fp) :: Residual_Mean_Square   


    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    n = SIZE(x)
    IF ( n < 3 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input data must be at least 3 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( SIZE(y) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sizes of input X,Y arguments are inconsistent', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( PRESENT(yFit) ) THEN
      IF ( SIZE(yFit) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Sizes of output YFIT argument is inconsistent', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! Calculate averages
    ! ------------------
    xAverage = SUM(x) / REAL(n,fp)
    yAverage = SUM(y) / REAL(n,fp)


    ! Calculate the sums of the square of the mean difference for X
    ! -------------------------------------------------------------
    sum_dx2 = SUM(( x-xAverage )**2)
    IF ( sum_dx2 < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sum of the squares of mean difference for X is zero.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Calculate coefficients
    ! ----------------------
    b = SUM(( x-xAverage )*( y-yAverage )) / sum_dx2
    a = yAverage - ( b*xAverage )


    ! Calculate the regression Y values
    ! ---------------------------------
    yCalculated = a + ( b*x )
    Residual_Sum_of_Squares = SUM(( y-yCalculated )**2)
    Residual_Mean_Square    = Residual_Sum_of_Squares / REAL(n-2,fp)


    ! Assign optional arguments
    ! -------------------------
    IF ( PRESENT(yFit) ) yFit = yCalculated
    IF ( PRESENT(SSE)  ) SSE  = Residual_Sum_of_Squares
    IF ( PRESENT(MSE)  ) MSE  = Residual_Mean_Square

  END FUNCTION Least_Squares_Linear_Fit


!--------------------------------------------------------------------------------
!
! NAME:
!       Compute_Frequency_Index
!
! PURPOSE:
!       Function to determine the frequency array element index for
!       an input frequency value.
!
! CALLING SEQUENCE:
!       Frequency_Index = Compute_Frequency_Index( Begin_Frequency,    &  ! Input
!                                                  Frequency_Interval, &  ! Input
!                                                  Frequency           )  ! Input
!
! INPUT ARGUMENTS:
!       Begin_Frequency:     Begin frequency corresponding to the first
!                            value in the array.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       Frequency_Interval:  Frequency interval between adjacent frequency
!                            array values.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       Frequency:           Frequency value for which the index is required.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Frequency_Index:     The return value is an integer corresponding to
!                            the location of the input frequency value in an
!                            array defined by the begin frequency and
!                            frequency interval.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Compute_Frequency_Index( Begin_Frequency, &
                                    Frequency_Interval, &
                                    Frequency ) &
                                  RESULT( Frequency_Index )
    ! Arguments
    REAL(fp), INTENT(IN) :: Begin_Frequency
    REAL(fp), INTENT(IN) :: Frequency_Interval
    REAL(fp), INTENT(IN) :: Frequency
    ! Function result
    INTEGER :: Frequency_Index
    ! Local parameters
    REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp 

    ! Compute the frequency index
    Frequency_Index = INT(((Frequency-Begin_Frequency)/Frequency_Interval) + ONEpointFIVE)

  END FUNCTION Compute_Frequency_Index

END PROGRAM Create_SpcCoeff_from_SRF
