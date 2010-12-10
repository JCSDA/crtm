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
  USE CRTM_Parameters          , ONLY: ZERO
  USE Fundamental_Constants    , ONLY: C_1, C_2
  USE Planck_Functions         , ONLY: Planck_Radiance
  USE Interpolate_Utility      , ONLY: Spline_Initialize, &
                                       Spline_Interpolate
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_GHz, &
                                       GHz_to_inverse_cm
  USE MW_SensorData_Define     , ONLY: MW_SensorData_type, &
                                       Load_MW_SensorData, &
                                       Destroy_MW_SensorData
  USE SensorInfo_Define        , ONLY: MICROWAVE_SENSOR        , &
                                       INFRARED_SENSOR         , &
                                       VISIBLE_SENSOR          , &
                                       UNPOLARIZED             , &
                                       SensorInfo_type         , &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type,    &
                                       Count_SensorInfo_Nodes,  &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
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
  USE oSRF_File_Define         , ONLY: oSRF_File_type, &
                                       oSRF_File_GetValue, &
                                       oSRF_File_Read, &
                                       oSRF_File_Destroy
  USE Integrate_Utility        , ONLY: Integral

  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_SpcCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  
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
  
  ! Cosmic background temperature
  REAL(fp), PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.7253

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: oSRF_Filename
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
  INTEGER :: n, b
  INTEGER :: n_FOVs
  INTEGER :: SpcCoeff_File_Version
  CHARACTER( 256) :: Title
  CHARACTER(2000) :: oSRF_History, Solar_History, Solar_Comment
  CHARACTER(2000) :: AntCorr_Comment, AntCorr_History
  CHARACTER(2000) :: Comment
  REAL(fp), ALLOCATABLE :: Solar_Derivative(:)
  REAL(fp), ALLOCATABLE, TARGET :: Spectrum(:)
  REAL(fp), POINTER             :: Irradiance(:) => NULL()
  INTEGER :: n_Sensors
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(oSRF_File_type)       :: oSRF_File
  TYPE(Solar_type)           :: Solar
  TYPE(SpcCoeff_type)        :: SpcCoeff
  TYPE(MW_SensorData_type)   :: MW_SensorData
  TYPE(AntCorr_type)         :: AntCorr

  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to create the SpcCoeff '//&
                       'files from the sensor oSRF data files.', &
                       '$Revision$' )

  ! Get user inputs
  ! ---------------
  ! The solar filename
  WRITE( *,FMT='(/5x,"Enter a Solar filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) Solar_Filename
  Solar_Filename = ADJUSTL(Solar_Filename)
  
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
  
  Solar_Comment = ' '
  Solar_History = ' '

  ! Read the file
  Error_Status = Read_Solar_netCDF( Solar_Filename         , &
                                    Solar                  , &
                                    Comment = Solar_Comment, &
                                    History = Solar_History  )
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
    CALL Display_Message( PROGRAM_NAME,  &
                          TRIM(Message), &
                          FAILURE        )
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
  
  ! Begin the main sensor loop
  ! ---------------------------
  Sensor_Loop: DO n = 1, n_Sensors
    
    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n,               &
                                            SensorInfo       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME,  &
                            TRIM(Message), &
                            FAILURE        )
      STOP
    END IF
    
    ! Only operate on sensors with SRF file present
    ! ---------------------------------------------
    oSRF_Filename = TRIM(SensorInfo%Sensor_ID)//'.osrf.nc'
    IF ( .NOT. File_Exists( oSRF_Filename ) ) CYCLE Sensor_Loop
    
    ! Output an info message
    ! ----------------------
    WRITE( *,'(//5x,"Creating the SpcCoeff data file for ",a)' ) &
              TRIM(SensorInfo%Sensor_Id)

    ! Read MW_SENSORDATA AND ANTCORR data for microwave
    ! -------------------------------------------------
    IF(SensorInfo%Sensor_Type == MICROWAVE_SENSOR) THEN
      
      ! Load the current microwave sensor data
      ! --------------------------------------
      Error_Status = Load_MW_SensorData( MW_SensorData,                     &
                                         Sensor_ID = SensorInfo%Sensor_ID,  &
                                         RCS_Id    = MW_SensorData_History  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME,                        &
                              'Error loading MW sensor data for '//&
                              TRIM(SensorInfo%Sensor_Id),          &
                              FAILURE                              )
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
        Error_Status = Read_AntCorr_netCDF( AntCorr_Filename,        &
                                            AntCorr,                 &
                                            History=AntCorr_History, &
                                            Comment=AntCorr_Comment  )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME,                                  &
                                'Error loading antenna correction data for '// &
                                TRIM(SensorInfo%Sensor_Id),                    &
                                FAILURE                                        )
          STOP
        END IF
        
        ! Modify GAtts for concatenation
        AntCorr_History = '; AntCorr: '//TRIM(AntCorr_History)
        AntCorr_Comment = '; AntCorr: '//TRIM(AntCorr_Comment)
        
        ! Set the FOV dimension for the SpcCoeff allocation
        n_FOVs = AntCorr%n_FOVs
                
      END IF      
      
      ! Allocate the SpcCoeff structure                                                   
      ! -------------------------------                                                   
      Error_Status = Allocate_SpcCoeff( SensorInfo%n_Channels, SpcCoeff, n_FOVs=n_FOVs )  
      IF ( Error_Status /= SUCCESS ) THEN                                                 
        CALL Display_Message( PROGRAM_NAME,                                &              
                              'Error allocating SpcCoeff data structure.', &              
                              FAILURE                                      )              
        STOP                                                                              
      END IF 
      
      ! The antenna correction data
      IF ( SpcCoeff%AC_Present ) THEN
        ! Copy it
        Error_Status = Assign_AntCorr( AntCorr, SpcCoeff%AC )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME,                                                         & 
                                'Error copying antenna correction data into SpcCoeff structure for '//&
                                TRIM(SensorInfo%Sensor_Id),                                           &
                                FAILURE                                                               )
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
                                                                                   
    ELSE 
          
      ! Allocate the SpcCoeff structure                                      
      ! -------------------------------                                      
      Error_Status = Allocate_SpcCoeff( SensorInfo%n_Channels, SpcCoeff)     
      IF ( Error_Status /= SUCCESS ) THEN                                    
        CALL Display_Message( PROGRAM_NAME,                                & 
                              'Error allocating SpcCoeff data structure.', & 
                              FAILURE                                      ) 
        STOP                                                                   
      END IF
    END IF
      
    ! Read the current oSRF data
    ! --------------------------
    Error_Status = oSRF_File_Read( oSRF_File   , &  ! Output
                                   oSRF_Filename )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error occured when reading the oSRF_File', &
                            FAILURE )
      STOP
    END IF
    
    ! Get the title history and comment from oSRF file
    ! ------------------------------------------------
    Error_Status = oSRF_File_GetValue( oSRF_File             , & ! Input
                                       Title   = Title       , & ! Optional output
                                       History = oSRF_History, & ! Optional output
                                       Comment = Comment       ) ! Optional output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error getting title, history and/or '//&
                            'comment from '//TRIM(oSRF_Filename), &
                            Error_Status )
      STOP
    END IF
              
    ! Assign fields that are independent of sensor-type 
    ! -------------------------------------------------
    SpcCoeff%Version          = SpcCoeff_File_Version
    SpcCoeff%Sensor_Id        = SensorInfo%Sensor_Id
    SpcCoeff%Sensor_Type      = SensorInfo%Sensor_Type
    SpcCoeff%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
    SpcCoeff%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID
    SpcCoeff%Sensor_Channel   = SensorInfo%Sensor_Channel
    
    ! Initialise
    SpcCoeff%Solar_Irradiance = ZERO
    SpcCoeff%Cosmic_Background_Radiance = ZERO
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,SOLAR_FLAG)
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,ZEEMAN_FLAG)
    
    ! Set Polarization Status
    IF (SpcCoeff%Sensor_Type==MICROWAVE_SENSOR) THEN 
      SpcCoeff%Polarization = MW_SensorData%Polarization
    ELSE
      SpcCoeff%Polarization = UNPOLARIZED
    ENDIF 
     
    ! Open file for band correction fit output
    ! ----------------------------------------
    ASCII_Filename = TRIM(SensorInfo%Sensor_Id)//'.SpcCoeff.asc'
    ASCII_FileID = Get_Lun()
    IF ( ASCII_FileID < 0 ) THEN
      CALL Display_Message( PROGRAM_NAME,                                      &
                            'Error obtaining file unit number for output to '//&
                            TRIM(ASCII_Filename),                              &
                            FAILURE                                            )
      STOP
    END IF
    OPEN( ASCII_FileID, FILE  =TRIM(ASCII_Filename), &
                        ACCESS='SEQUENTIAL',         &
                        FORM  ='FORMATTED',          &
                        STATUS='REPLACE',            &
                        ACTION='WRITE',              &
                        IOSTAT=IO_Status             )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error opening statistics output file ",a,". STAT = ",i0)' ) &
                     TRIM(ASCII_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    WRITE( ASCII_FileID,'(5x,"SRF data from file: ",a)' ) TRIM(oSRF_Filename)

    ! Begin loop over channels
    ! ------------------------
    WRITE( *,'(/,"    CH        V0            ",                              &
                &"FK1            FK2            BC1            BC2          ",&
                &"CBR            F(Solar)       POLARIZATION")'               )

    Channel_Loop: DO l = 1, SpcCoeff%n_Channels
        
      WRITE( *,FMT='(2x,i4)',ADVANCE='NO') SpcCoeff%Sensor_Channel(l) 
         
      ! Set the SRF centroid and determine the Planck coefficients
      ! ----------------------------------------------------------
      SELECT CASE (SpcCoeff%Sensor_Type)
        CASE (MICROWAVE_SENSOR)
          SpcCoeff%Frequency(l)  = oSRF_File%oSRF(l)%f0 
          SpcCoeff%Wavenumber(l) = GHz_to_inverse_cm( SpcCoeff%Frequency(l) )
          DO b = 1, oSRF_File%oSRF(l)%n_Bands
            oSRF_File%oSRF(l)%Frequency(b)%Arr = GHz_to_inverse_cm( oSRF_File%oSRF(l)%Frequency(b)%Arr )
          END DO         
        CASE (INFRARED_SENSOR, VISIBLE_SENSOR)        
          SpcCoeff%Wavenumber(l) = oSRF_File%oSRF(l)%f0
          SpcCoeff%Frequency(l)  = Inverse_cm_to_GHz( SpcCoeff%Wavenumber(l) )
      END SELECT
      
      ! Assign the coefficients
      ! -----------------------
      
      ! Planck Coefficients
      SpcCoeff%Planck_C1(l) = oSRF_File%oSRF(l)%Planck_Coeffs(1)
      SpcCoeff%Planck_C2(l) = oSRF_File%oSRF(l)%Planck_Coeffs(2)
      
      WRITE( *,FMT='(3(2x,es13.6))',ADVANCE='NO' ) SpcCoeff%Wavenumber(l), &
                                                   SpcCoeff%Planck_C1(l), &
                                                   SpcCoeff%Planck_C2(l)
      ! Polychromatic correction coefficients
      SpcCoeff%Band_C1(l) = oSRF_File%oSRF(l)%Polychromatic_Coeffs(1)
      SpcCoeff%Band_C2(l) = oSRF_File%oSRF(l)%Polychromatic_Coeffs(2)
      
      ! Output fit coefficients to screen
      WRITE( *,FMT='(2(2x,es13.6))',ADVANCE='NO' ) SpcCoeff%Band_C1(l), &
                                                   SpcCoeff%Band_C2(l)
        
      IF(SpcCoeff%Sensor_Type == MICROWAVE_SENSOR) THEN
      
        ! Set the zeeman flag based on MW_SensorData
        IF ( MW_SensorData%Zeeman(l) == SET ) &
        CALL SetFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),ZEEMAN_FLAG)      
        
        ! Compute the cosmic background radiance
        ! --------------------------------------
        Error_Status = Planck_Radiance( SpcCoeff%Wavenumber(l),                &
                                        COSMIC_BACKGROUND_TEMPERATURE,         &
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
        
        ! Set the solar channel flag
        CALL SetFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),SOLAR_FLAG)

        ! Allocate the radiance/irradiance spectrum workarray
        ! ---------------------------------------------------
        ALLOCATE( Spectrum( oSRF_File%oSRF(l)%n_Points(1) ),STAT=Allocate_Status )
        IF ( Allocate_Status /= 0 ) THEN
          WRITE( Message,'("Error allocating spectrum array. STAT = ",i0)' ) &
                         Allocate_Status
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
            
        ! Interpolate the solar spectrum to the SRF spacing
        Irradiance => Spectrum
        Error_Status = Spline_Interpolate( Solar%Frequency                    , &  ! X
                                           Solar%Irradiance                   , &  ! Y
                                           oSRF_File%oSRF(l)%Frequency(1)%Arr , &  ! Xint
                                           Irradiance                         , &  ! Yint
                                           y2=Solar_Derivative                  )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error interpolating solar spectrum for SRF channel ",i0)' ) &
                         oSRF_File%oSRF(l)%Channel
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
             
        ! via integration
        Error_Status = Integral( oSRF_File%oSRF(l)%Frequency(1)%Arr             , &
                                 oSRF_File%oSRF(l)%Response(1)%Arr * Irradiance , &
                                 SpcCoeff%Solar_Irradiance(l)                     )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error occurred convoling solar irradiance with SRF.', &
                                FAILURE )
          STOP
        END IF
        
        SpcCoeff%Solar_Irradiance(l) = SpcCoeff%Solar_Irradiance(l)/oSRF_File%oSRF(l)%Integral
        NULLIFY( Irradiance )
        
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
          
      ENDIF
      
      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Cosmic_Background_Radiance(l)
      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Solar_Irradiance(l)
      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Solar_Irradiance(l)
      
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
                                                  TRIM(oSRF_Filename)//&
                                                  ' and solar data file '//&
                                                  TRIM(Solar_Filename), &
                                          History = PROGRAM_RCS_ID//&
                                                    '; '//TRIM(oSRF_History)//&
                                                    '; '//TRIM(Solar_History), &
                                          Comment = TRIM(Comment)//&
                                                    '; Solar Information'//&
                                                    ': '//TRIM(Solar_Comment) )
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
                            TRIM(oSRF_Filename)//' processing.', &
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
    
    ! For microwave sensors destroy MW_SensorData structure
    IF(SpcCoeff%Sensor_Type == MICROWAVE_SENSOR) THEN
      Error_Status = Destroy_MW_SensorData( MW_SensorData )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying MW_SensorData data structure.', &
                              WARNING )
      END IF 
    END IF 
    
    ! Destroy current oSRF_File
    ! -------------------------
    CALL oSRF_File_Destroy(oSRF_File)
    
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

END PROGRAM Create_SpcCoeff
