!
! AVHRRJM_SRF_ASCII2NC
!
! Program to convert the Jon Mittaz's AVHRR SRF ASCII files to netCDF format
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Sep-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM AVHRRJM_SRF_ASCII2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                   Display_Message, Program_Message
  USE File_Utility         , ONLY: Get_Lun, File_Exists
  USE Interpolate_Utility  , ONLY: Polynomial_Interpolate
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Allocate_SensorInfo, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   New_SensorInfo_List, &
                                   Destroy_SensorInfo_List, &
                                   AddTo_SensorInfo_List, &
                                   GetFrom_SensorInfo_List, &
                                   Count_SensorInfo_Nodes 
  USE SensorInfo_IO        , ONLY: Read_SensorInfo, Write_SensorInfo
  USE SRF_Define           , ONLY: SRF_type, &
                                   Allocate_SRF, &
                                   Destroy_SRF, &
                                   Frequency_SRF, &
                                   Integrate_SRF
  USE SRF_netCDF_IO        , ONLY: Inquire_SRF_netCDF, &
                                   Create_SRF_netCDF, &
                                   Write_SRF_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'AVHRRJM_SRF_ASCII2NC'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  
  REAL(fp), PARAMETER :: ZERO         = 0.0_fp
  REAL(fp), PARAMETER :: ONE          = 1.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp
  INTEGER , PARAMETER :: SET = 1
  ! The index for the required frequency interval = 1
  ! This corresponds to the 0.1cm^-1 frequency interval
  ! from the Tau_Production_Parameters module.
  REAL(fp), PARAMETER :: SCALE_FACTOR = 10.0_fp
  REAL(fp), PARAMETER :: DF = 0.1_fp
  ! Interpolation order
  ! If 1 == linear
  !    3 == cubic
  INTEGER, PARAMETER :: ORDER = 3
  
  INTEGER,      PARAMETER :: N_AVHRR_CHANNELS = 3
  CHARACTER(*), PARAMETER :: CHANNEL_ID(N_AVHRR_CHANNELS) = &
    (/ '3.7', '11 ', '12 ' /)
       

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: ASCII_Filename
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: Title
  CHARACTER(256) :: Comment
  CHARACTER(20) :: Sensor_Id
  INTEGER :: Sat_Id
  INTEGER :: Error_Status, Allocate_Status, IO_Status
  INTEGER :: FileID
  INTEGER :: i, l, n
  INTEGER :: n_Sensors
  INTEGER :: n_Pts
  INTEGER :: n_iPts
  REAL(fp) :: f1, f2
  REAL(fp), DIMENSION(:), ALLOCATABLE :: f_Raw
  REAL(fp), DIMENSION(:), ALLOCATABLE :: SRF_Raw
  TYPE(SRF_type) :: SRF
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read ASCII column format AVHRR SRF data files '//&
                        'received from Jon Mittaz and write netCDF format SRF data files.', &
                        '$Revision$' )

  ! Load the SensorInfo information
  ! -------------------------------
  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter a SensorInfo filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )
  
  ! Read the file
  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM(SensorInfo_Filename), &
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
  WRITE( *,'(5x,"Number of sensors in list: ",i0)' ) n_Sensors


  ! Create the sensor ID
  ! --------------------
  WRITE(*,'(/5x,"Enter the NOAA satellite number [17/18]: ")',ADVANCE='NO')
  READ(*,'(i5)') Sat_ID
  WRITE(Sensor_Id,'("avhrr3JM_n",i2)') Sat_Id
  
  NC_Filename = TRIM(Sensor_Id)//'.srf.nc'
  
  
  ! Begin loop over SensorInfo sensors
  ! ----------------------------------
  Sensor_Loop: DO n = 1, n_Sensors
  
  
    ! Get the current SensorInfo node from the list
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
    
    
    ! Cycle loop if not the current sensor
    ! ------------------------------------
    IF ( Sensor_Id /= SensorInfo%Sensor_Id) CYCLE Sensor_Loop


    ! Create the output interpolated SRF data file
    ! --------------------------------------------
    ! Create the attributes
    Title   = 'Shifted AVHRR/3 SRFs'
    Comment = 'ASCII SRF data provided by J.Mittaz; Interpolated SRFs for transmittance production'

    ! Create the netCDF file
    Error_Status = Create_SRF_netCDF( NC_Filename, &
                                      SensorInfo%Sensor_Channel, &
                                      WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                      WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &
                                      Title            = TRIM(Title), &
                                      History          = PROGRAM_RCS_ID, &
                                      Sensor_Name      = TRIM(SensorInfo%Sensor_Name), &
                                      Platform_Name    = TRIM(SensorInfo%Satellite_Name), &
                                      Comment          = TRIM(Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating '//TRIM(NC_Filename), &
                            Error_Status )
      STOP
    END IF


    ! Loop over channels files for read
    ! ---------------------------------
    Channel_Loop: DO l = 1, N_AVHRR_CHANNELS

      ! Create filename
      WRITE(ASCII_Filename,'("noaa-",i2,"-",a,".srf")' ) Sat_Id, TRIM(CHANNEL_ID(l))
      
      ! Open the file
      FileID = Get_Lun()
      OPEN( FileID, FILE   = ASCII_Filename, &
                    STATUS = 'OLD', &
                    FORM   = 'FORMATTED', &
                    ACCESS = 'SEQUENTIAL', &
                    ACTION = 'READ', &
                    IOSTAT = IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error opening ",a,". IOSTAT=",i0)' ) TRIM(ASCII_Filename), IO_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    
      ! Read the number of SRF points
      READ(FileID,*,IOSTAT=IO_Status) n_Pts
      IF ( IO_Status /= 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading n_Pts from file '//TRIM(ASCII_Filename), &
                              FAILURE )
        STOP
      END IF
    
      ! Allocate the read arrays
      ALLOCATE( f_Raw(n_Pts), SRF_Raw(n_Pts), STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating read arrays for '//TRIM(ASCII_Filename), &
                              FAILURE )
        STOP
      END IF

      ! Read the data
      DO i = 1, n_Pts
        READ(FileID,*,IOSTAT=IO_Status) f_Raw(i), SRF_Raw(i)
        IF ( IO_Status /= 0 ) THEN
          WRITE(Message,'("Error reading line #",i0," from ",a)') i, TRIM(ASCII_Filename)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
      END DO
      CLOSE( FileID )

      ! Ensure data is monotonically increasing in frequency
      IF ( f_Raw(2) < f_Raw(1) ) THEN
        f_Raw   = f_Raw(n_Pts:1:-1)
        SRF_Raw = SRF_Raw(n_Pts:1:-1)
      END IF

      ! Compute the number of interpolated frequency points
      f1 = REAL(INT(f_Raw(1)    *SCALE_FACTOR) + 1, fp)/SCALE_FACTOR
      f2 = REAL(INT(f_Raw(n_Pts)*SCALE_FACTOR) - 1, fp)/SCALE_FACTOR
      n_iPts = INT((f2-f1)/DF + ONEpointFIVE)
      
      ! Allocate the SRF
      Error_Status = Allocate_SRF( n_iPts, SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error allocating channel ",i0," SRF for ",a)' ) &
                       SensorInfo%Sensor_Channel(l), TRIM(SensorInfo%Sensor_Id)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
      
      ! Assign stuff
      SRF%Sensor_Name      = SensorInfo%Sensor_Name
      SRF%Platform_Name    = SensorInfo%Satellite_Name
      SRF%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
      SRF%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
      SRF%Begin_Frequency  = f1
      SRF%End_Frequency    = f2
      SRF%Channel          = SensorInfo%Sensor_Channel(l)
      
      ! Compute the interpolation frequency grid
      Error_Status = Frequency_SRF( SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error computing the interpolation frequency grid ",&
                        &"for channel #",i0)') &
                        SensorInfo%Sensor_Channel(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF

      ! Interpolate the input SRF
      Error_Status = Polynomial_Interpolate( f_Raw        , &  ! Input
                                             SRF_Raw      , &  ! Input
                                             SRF%Frequency, &  ! Input
                                             SRF%Response , &  ! Output
                                             Order=ORDER    )  ! Optional input
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error interpolating SRF for channel #",i0)') &
                        SensorInfo%Sensor_Channel(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF

      ! Any -ve bits due to the interpolation are set to zero
      WHERE( SRF%Response < ZERO ) SRF%Response = ZERO

      ! Normalise it
      SRF%Response = SRF%Response / MAXVAL(SRF%Response)

      ! Integrate the interpolated SRF
      Error_Status = Integrate_SRF( SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error integrating channel #",i0," interpolated SRF.")') &
                        SensorInfo%Sensor_Channel(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF

      ! Output the integration comparisons
      WRITE(*,'(10x,"Integrated SRF: ",es13.6,5x,"Summed SRF: ",es13.6,5x,"% difference: ",es13.6)') &
              SRF%Integrated_SRF, SRF%Summation_SRF, &
              100.0_fp*(SRF%Integrated_SRF-SRF%Summation_SRF)/SRF%Integrated_SRF

      ! Write the channel SRF to the netCDF file
      Error_Status = Write_SRF_netCDF( NC_Filename, SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error occurred writing channel #",i0,&
                        &" interpolated SRF to ",a,".")') &
                        SRF%Channel, TRIM(NC_Filename)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF
      
      ! Destroy SRF structure for next channel
      Error_Status = Destroy_SRF( SRF )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error occurred destroying channel #",i0,&
                       &" SRF structure.")') &
                       SensorInfo%Sensor_Channel(l)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF
      
      ! Deallocate the read arrays
      DEALLOCATE( f_Raw, SRF_Raw, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error deallocating read arrays for '//TRIM(ASCII_Filename), &
                              FAILURE )
        STOP
      END IF
      
    END DO Channel_Loop

    
    ! Destroy the current SensorInfo node in
    ! preparation for the next node retrieval
    ! ---------------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF

  END DO Sensor_Loop
  
  
  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo_List.', &
                          FAILURE )
  END IF

END PROGRAM AVHRRJM_SRF_ASCII2NC
