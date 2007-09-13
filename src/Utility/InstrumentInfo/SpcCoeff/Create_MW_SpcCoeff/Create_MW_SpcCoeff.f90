!
! Create_MW_SpcCoeff
!
! Program to create the microwave spectral coefficient (SpcCoeff)
! data files for use with the CRTM.
!
! FILES ACCESSED:
!       Input:
!         - ASCII SensorInfo data file
!
!       Output:
!         - netCDF SpcCoeff data file containing all the required data.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Create_MW_SpcCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Fundamental_Constants    , ONLY: C_1, C_2
  USE Planck_Functions         , ONLY: Planck_Radiance, Planck_Temperature
  USE Spectral_Units_Conversion, ONLY: GHz_to_inverse_cm
  USE MW_SensorData_Define     , ONLY: MW_SensorData_type, &
                                       Load_MW_SensorData, &
                                       Destroy_MW_SensorData
  USE SensorInfo_Define        , ONLY: SENSORINFO_MICROWAVE=>MICROWAVE_SENSOR_TYPE, &
                                       SensorInfo_type, &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type, &
                                       Count_SensorInfo_Nodes, &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
  USE AntCorr_Define           , ONLY: AntCorr_type, &
                                       Assign_AntCorr, &
                                       Destroy_AntCorr
  USE AntCorr_netCDF_IO        , ONLY: Read_AntCorr_netCDF
  USE SpcCoeff_Define          , ONLY: SPCCOEFF_MICROWAVE=>MICROWAVE_SENSOR, &
                                       POLARIZATION_TYPE_NAME, &
                                       SOLAR_FLAG, ZEEMAN_FLAG, &
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
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_MW_SpcCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Keyword/flag set value
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
  INTEGER,  PARAMETER :: N_TEMPERATURES  = 17
  REAL(fp), PARAMETER :: MIN_TEMPERATURE = 180.0_fp
  REAL(fp), PARAMETER :: MAX_TEMPERATURE = 340.0_fp

  ! The default cosmic background temperature
  REAL(fp),     PARAMETER :: COSMIC_BACKGROUND_TEMPERATURE = 2.7253_fp
  CHARACTER(*), PARAMETER :: CMB_REFERENCE = &
    'CMB value from J.C. Mather, et. al., 1999, "Calibrator Design for the '//&
    'COBE Far-Infrared Absolute Spectrophotometer (FIRAS)," Astrophysical '//&
    'Journal, vol 512, pp 511-520'

  ! A list of the instruments for which we have antenna correction data
  CHARACTER(*), PARAMETER :: AC_SENSOR_ID_LIST = &
  'amsua_n15:amsua_n16:amsua_n17:amsua_n18:amsua_metop-a:amsub_n15:amsub_n16:'//&
  'amsub_n17:mhs_n18:mhs_metop-a'

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: SpcCoeff_Filename
  CHARACTER(256) :: ASCII_Filename
  CHARACTER(256) :: MW_SensorData_History
  INTEGER :: ASCII_FileID
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: i, l, n, n_Sensors
  INTEGER :: SpcCoeff_File_Version
  REAL(fp) :: Integrated_MW_Response
  REAL(fp) :: Convolved_Radiance
  REAL(fp) :: x_Temperature(N_TEMPERATURES)
  REAL(fp) :: y_Effective_Temperature(N_TEMPERATURES)
  REAL(fp) :: yFit_Effective_Temperature(N_TEMPERATURES)
  REAL(fp) :: var_Effective_Temperature
  REAL(fp), ALLOCATABLE :: Wavenumber(:)
  REAL(fp), ALLOCATABLE :: Radiance(:)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(MW_SensorData_type)   :: MW_SensorData
  TYPE(SpcCoeff_type)        :: SpcCoeff
  ! Antenna correction variables
  CHARACTER(2000) :: AntCorr_History
  CHARACTER(2000) :: AntCorr_Comment
  CHARACTER(256)  :: AntCorr_Filename
  INTEGER :: n_FOVs
  TYPE(AntCorr_type) :: AntCorr
    

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the microwave SpcCoeff files.', &
                        '$Revision$' )


  ! Get user inputs
  ! ---------------
  ! The SensorInfo filename and data
  WRITE( *,FMT    ='(/5x,"Enter a SensorInfo filename: ")', &
           ADVANCE='NO' )
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

  
  ! Generate mononchromatic temperatures
  ! ------------------------------------
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


    ! Only operate on microwave sensors
    ! ---------------------------------
    IF ( SensorInfo%Microwave_Flag /= SENSORINFO_MICROWAVE ) CYCLE Sensor_Loop


    ! Output an info message
    ! ----------------------
    WRITE( *,'(//5x,"Creating the SpcCoeff data file for ",a)' ) &
              TRIM(SensorInfo%Sensor_Id)


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

    ! Modify GAtss for concatenation
    MW_SensorData_History = '; '//TRIM(MW_SensorData_History)


    ! Load antenna correction data if required
    ! ----------------------------------------
    AntCorr_Filename = TRIM(SensorInfo%Sensor_ID)//'.AntCorr.nc'
    AntCorr_History  = ' '
    AntCorr_Comment  = ' '
    n_FOVs = 0
    IF ( INDEX(AC_SENSOR_ID_LIST,TRIM(SensorInfo%Sensor_ID)) /= 0 .AND. &
         File_Exists(AntCorr_Filename) ) THEN
      
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
      
      ! Modify GAtss for concatenation
      AntCorr_History = '; AntCorr: '//TRIM(AntCorr_History)
      AntCorr_Comment = '; AntCorr: '//TRIM(AntCorr_Comment)

      ! Set the FOV dimension for the SpcCoeff allocation
      n_FOVs = AntCorr%n_FOVs
    END IF


    ! Double check the WMO IDs
    ! ------------------------
    IF ( MW_SensorData%WMO_Satellite_ID /= SensorInfo%WMO_Satellite_ID .OR. &
         MW_SensorData%WMO_Sensor_ID    /= SensorInfo%WMO_Sensor_ID         ) THEN
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'MW_SensorData and SensorInfo WMO IDs are different for '//&
                              TRIM(SensorInfo%Sensor_Id), &
                              FAILURE )
        STOP
      END IF
    END IF


    ! Allocate the SpcCoeff structure
    ! -------------------------------
    Error_Status = Allocate_SpcCoeff( MW_SensorData%n_Channels, &
                                      SpcCoeff, &
                                      n_FOVs=n_FOVs )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SpcCoeff data structure.', &
                            FAILURE )
      STOP
    END IF


    ! Assign various data components
    ! ------------------------------
    ! Set the version value
    SpcCoeff%Version   = SpcCoeff_File_Version

    ! Assign the sensor ID values
    SpcCoeff%Sensor_Id        = SensorInfo%Sensor_Id
    SpcCoeff%Sensor_Type      = SPCCOEFF_MICROWAVE
    SpcCoeff%WMO_Satellite_ID = MW_SensorData%WMO_Satellite_ID
    SpcCoeff%WMO_Sensor_ID    = MW_SensorData%WMO_Sensor_ID
    SpcCoeff%Sensor_Channel   = MW_SensorData%Sensor_Channel

    ! Assign the central frequencies
    SpcCoeff%Frequency = MW_SensorData%Central_Frequency

    ! Assign the polarisation type
    SpcCoeff%Polarization = MW_SensorData%Polarization

    ! Clear the solar channel flag and solar irradiance array
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,SOLAR_FLAG)
    SpcCoeff%Solar_Irradiance = ZERO

    ! Clear the Zeeman flag
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,ZEEMAN_FLAG)
    
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
    
    
    ! Open file for band correction fit output
    ! ----------------------------------------
    ASCII_Filename = TRIM(SensorInfo%Sensor_Id)//'.SpcCoeff.asc'

    ASCII_FileID = Get_Lun()
    IF ( ASCII_FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error obtaining file unit number for output to '//&
                            TRIM( ASCII_Filename ), &
                            Error_Status )
      STOP
    END IF

    OPEN( ASCII_FileID, FILE  =TRIM(ASCII_Filename), &
                        ACCESS='SEQUENTIAL', &
                        FORM  ='FORMATTED', &
                        STATUS='REPLACE', &
                        ACTION='WRITE', &
                        IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error opening statistics output file ", a, ". STAT = ",i0)' ) &
                      TRIM( ASCII_Filename ), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


    ! Begin loop over channels
    ! ------------------------
    WRITE( *, '( /, "    CH        F0             V0            ", &
                   &"FK1            FK2", 25x, "POLARIZATION", 26x, "BC1",12x,"BC2",12x,"CBR" )' )

    Channel_Loop: DO l = 1, SpcCoeff%n_Channels

      WRITE( *,FMT='(2x,i4)',ADVANCE='NO') SpcCoeff%Sensor_Channel(l)


      ! Set the Zeeman flag if necessary
      ! --------------------------------
      IF ( MW_SensorData%Zeeman(l) == SET ) &
        CALL SetFlag_SpcCoeff(SpcCoeff%Channel_Flag(l),ZEEMAN_FLAG)


      ! Compute the Planck coefficients
      ! -------------------------------
      ! Convert the central frequency to units of inverse centimetres
      SpcCoeff%Wavenumber(l) = GHz_to_inverse_cm( SpcCoeff%Frequency(l) )

      ! Compute the coefficients
      SpcCoeff%Planck_C1(l) = C_1_SCALE_FACTOR * C_1 * ( SpcCoeff%Wavenumber(l)**3 )
      SpcCoeff%Planck_C2(l) = C_2_SCALE_FACTOR * C_2 *   SpcCoeff%Wavenumber(l)

      WRITE( *,FMT    ='(4(2x,es13.6),2x,a)', &
               ADVANCE='NO' ) SpcCoeff%Frequency(l), &
                              SpcCoeff%Wavenumber(l), &
                              SpcCoeff%Planck_C1(l), &
                              SpcCoeff%Planck_C2(l), &
                              POLARIZATION_TYPE_NAME(MW_SensorData%Polarization(l))


      ! Compute the polychromatic correction coefficients
      ! -------------------------------------------------
      ! Allocate the wavenumber/radiance arrays
      ALLOCATE( Wavenumber(MW_SensorData%n_Frequencies), &
                Radiance(MW_SensorData%n_Frequencies), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating wavenumber and radiance arrays. STAT = ",i0)' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Convert the MW frequency grid from GHz to cm^-1 units
      Wavenumber = GHz_to_inverse_cm( MW_SensorData%Frequency(:,l) )

      ! Compute the MW response
      Integrated_MW_Response = SUM(MW_SensorData%Response(:,l)) * MW_SensorData%Delta_Frequency(l)

      ! Generate the "polychromatic" temperatures
      Temperature_Loop: DO i = 1, N_TEMPERATURES

        ! Calculate monochromatic radiances
        Error_Status = Planck_Radiance( Wavenumber, &
                                        x_Temperature(i), &
                                        Radiance )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'( "Error calculating radiance at T = ",f5.1," K.")' ) &
                          x_Temperature(i)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF

        ! Convolve the radiance spectrum with the MW response
        Convolved_Radiance = SUM(Radiance)*MW_SensorData%Delta_Frequency(l)/Integrated_MW_Response

        ! Convert the convolved radiance back into a temperature
        Error_Status = Planck_Temperature( SpcCoeff%Wavenumber(l), &
                                           Convolved_Radiance, &
                                           y_Effective_Temperature(i) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error calculating polychromatic temperature at T = ",f5.1," K.")' ) &
                          x_Temperature(i)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
      END DO Temperature_Loop

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

      ! Output fit information to file
      WRITE( ASCII_FileID,FMT='(/5x,"CHANNEL ",i4)' ) SpcCoeff%Sensor_Channel(l)
      WRITE( ASCII_FileID,FMT='(2x,"Fit equation : Teff = ",es13.6," + ",es13.6," T")' ) &
                              SpcCoeff%Band_C1(l), SpcCoeff%Band_C2(l)
      WRITE( ASCII_FileID,FMT='(2x,"MSE : ",es13.6,";  Sigma : ",es13.6)' )&
                              var_Effective_Temperature, SQRT(var_Effective_Temperature)
      WRITE( ASCII_FileID,'(7x,"T      Teff(true)   Teff(fit)  dTeff(true-fit)")' )
      WRITE( ASCII_FileID,'(2x,49("-"))' )
      DO i = 1, N_TEMPERATURES
        WRITE( ASCII_FileID,'(3(2x,f10.6),2x,es13.6)' ) &
                             x_Temperature(i), &
                             y_Effective_Temperature(i),  yFit_Effective_Temperature(i), &
                             y_Effective_Temperature(i) - yFit_Effective_Temperature(i)
      END DO

      ! Deallocate arrays
      DEALLOCATE( Wavenumber, Radiance, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating wavenumber and radiance arrays. STAT = ",i0)' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF


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

      WRITE( *,FMT='(2x,es13.6)' ) SpcCoeff%Cosmic_Background_Radiance(l)

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
    SpcCoeff_Filename = TRIM(SensorInfo%Sensor_Id)//'.SpcCoeff.nc'

    Error_Status = Write_SpcCoeff_netCDF( TRIM(SpcCoeff_Filename), &
                                          SpcCoeff, &
                                          Title = 'Spectral coefficients for '//&
                                                  TRIM(SensorInfo%Sensor_Id)//'.', &
                                          History = PROGRAM_RCS_ID//&
                                                    TRIM(MW_SensorData_History)//&
                                                    TRIM(AntCorr_History), &
                                          Comment = 'Boxcar spectral response assumed; '//&
                                                    CMB_REFERENCE//&
                                                    TRIM(AntCorr_Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing SpcCoeff data structure to '//&
                            TRIM( SpcCoeff_Filename ), &
                            FAILURE )
      STOP
    END IF


    ! Destroy the current sensor data structures
    ! ------------------------------------------
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff structure for '//&
                            TRIM(SensorInfo%Sensor_Id), &
                            FAILURE )
      STOP
    END IF

    Error_Status = Destroy_MW_SensorData( MW_SensorData )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying MW_SensorData structure for '//&
                            TRIM(SensorInfo%Sensor_Id), &
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

  END DO Sensor_Loop


  ! Destroy the SensorInfo linked list
  ! ----------------------------------
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

END PROGRAM Create_MW_SpcCoeff
