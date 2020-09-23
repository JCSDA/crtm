!
! Interpolate_SRFs
!
! Program to read the netCDF SRF data files and interpolate the SRFs to
! to the same frequency grid used in the line-by-line transmittance data
! files.
!
!
! FILES ACCESSED:
!       Input: - User specified SensorInfo file.
!              - Original netCDF format SRF files
!
!       Output: Interpolated netCDF format SRF files
!
! SIDE EFFECTS:
!       If the interpolated output netCDF format SRF file already
!       exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Interpolate_SRFs


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Program_Message, Display_Message
  USE Interpolate_Utility      , ONLY: Polynomial_Interpolate
  USE SensorInfo_Define        , ONLY: SensorInfo_type, &
                                       Allocate_SensorInfo, &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type, &
                                       New_SensorInfo_List, &
                                       Count_SensorInfo_Nodes, &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
  USE SRF_Define               , ONLY: SRF_type, &
                                       Allocate_SRF, &
                                       Destroy_SRF, &
                                       Frequency_SRF, &
                                       Integrate_SRF
  USE SRF_netCDF_IO            , ONLY: Inquire_SRF_netCDF, &
                                       Create_SRF_netCDF, &
                                       Read_SRF_netCDF, &
                                       Write_SRF_netCDF
  USE Tau_Production_Parameters, ONLY: FREQUENCY_INTERVAL, &
                                       FREQUENCY_BEGIN
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME = 'Interpolate_SRFs'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  ! Numeric literals
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp
  ! The index for the required frequency interval = 1
  ! This corresponds to the 0.1cm^-1 frequency interval
  ! from the Tau_Production_Parameters module.
  INTEGER,  PARAMETER :: INTERPOLATION_INDEX = 1
  ! Interpolation order
  ! If 1 == linear
  !    3 == cubic
  INTEGER, PARAMETER :: ORDER = 3
  ! Keyword set (TRUE/ON) value
  INTEGER, PARAMETER :: SET = 1


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) ::  SRF_Filename
  CHARACTER(256) :: iSRF_Filename
  INTEGER :: Error_Status
  INTEGER :: n_Channels, l
  INTEGER :: WMO_Satellite_ID
  INTEGER :: WMO_Sensor_ID
  CHARACTER(256)  :: Title
  CHARACTER(5000) :: History
  CHARACTER(256)  :: Sensor_Name
  CHARACTER(256)  :: Platform_Name
  CHARACTER(5000) :: Comment
  INTEGER :: n_iPoints, n1, n2
  INTEGER :: n_Sensors, n
  TYPE(SRF_type) ::  SRF
  TYPE(SRF_type) :: iSRF
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

  ! Setup
  ! -----
  ! Initialise linked list structure
  SensorInfo_List = New_SensorInfo_List()

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, & 
                        'Program to read the netCDF SRF data files and interpolate '//&
                        'the SRFs to the same frequency grid used in the line-by-line '//&
                        'transmittance data files', &
                        '$Revision$' )


  ! Read the SensorInfo file
  ! ------------------------
  WRITE(*, FMT='(/5x,"Enter a SensorInfo filename: ")', ADVANCE='NO')
  READ(*, FMT='(a)') SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet=SET )
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


  ! Begin loop over sensors
  ! -----------------------
  Sensor_Loop: DO n = 1, n_Sensors

    ! Get the current SensorInfo data from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error retrieving SensorInfo data for sensor # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF

    ! Construct the SRF filenames
     SRF_Filename = TRIM(SensorInfo%Sensor_Id)//'.srf.nc'
    iSRF_Filename = TRIM(SensorInfo%Sensor_Id)//'.interpolated_srf.nc'


    ! Operate only on files that exist
    ! --------------------------------
    Available_Sensors: IF ( File_Exists(TRIM(SRF_Filename)) ) THEN

      WRITE(*,'(/5x,"Processing SRF file ",a,"...")') TRIM(SRF_Filename)


      ! Inquire the input SRF file and check the results
      ! ------------------------------------------------
      Error_Status = Inquire_SRF_netCDF( TRIM(SRF_Filename),                  &
                                         n_Channels       = n_Channels,       &
                                         WMO_Satellite_ID = WMO_Satellite_ID, &
                                         WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                         Title            = Title,            &
                                         History          = History,          &
                                         Sensor_Name      = Sensor_Name,      &
                                         Platform_Name    = Platform_Name,    &
                                         Comment          = Comment           )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring input SRF file '//TRIM(SRF_Filename), &
                              FAILURE )
        STOP
      END IF

      ! Check the number of channels
      IF ( n_Channels /= SensorInfo%n_Channels ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Number of channels in ",a,", ",i0,&
                        &", is different from SensorInfo entry, ",i0,".")' ) &
                       TRIM(SRF_Filename), n_Channels, SensorInfo%n_Channels
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF

      ! Check the sensor IDs
      IF ( WMO_Satellite_ID /= SensorInfo%WMO_Satellite_ID .OR. &
           WMO_Sensor_ID    /= SensorInfo%WMO_Sensor_ID         ) THEN
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Sensor Ids in '//TRIM(SRF_Filename)//&
                              ' are different from SensorInfo entry.', &
                              Error_Status )
        STOP
      END IF


      ! Create the output interpolated SRF data file
      ! --------------------------------------------
      ! Modify the comment attribute
      IF ( LEN_TRIM(Comment) == 0 .OR. TRIM(ADJUSTL(Comment)) == 'None' ) THEN
        Comment = 'Interpolated SRFs for transmittance production'
      ELSE
        Comment = 'Interpolated SRFs for transmittance production; '//&
                  TRIM(ADJUSTL(Comment))
      END IF

      ! Create the netCDF file
      Error_Status = Create_SRF_netCDF( TRIM( iSRF_Filename ), &
                                        SensorInfo%Sensor_Channel, &
                                        WMO_Satellite_ID = WMO_Satellite_ID, &
                                        WMO_Sensor_ID    = WMO_Sensor_ID, &
                                        Title            = TRIM(Title), &
                                        History          = PROGRAM_RCS_ID//'; '//&
                                                           TRIM(History), &
                                        Sensor_Name      = TRIM(Sensor_Name), &
                                        Platform_Name    = TRIM(Platform_Name), &
                                        Comment          = TRIM(Comment) )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating '//TRIM(iSRF_Filename), &
                              Error_Status )
        STOP
      END IF


      ! Loop over the channels
      ! ----------------------
      Channel_Loop: DO l = 1, n_Channels

        WRITE(*,'(5x,"Interpolating ",a," channel ",i0," SRF...")') &
                  TRIM(SRF_Filename), SensorInfo%Sensor_Channel(l)


        ! Read the current channel input SRF data
        ! ---------------------------------------
        Error_Status = Read_SRF_netCDF( TRIM(SRF_Filename), &
                                        SensorInfo%Sensor_Channel(l), &
                                        SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading channel #", i5, " SRF from ", a )' ) &
                          SensorInfo%Sensor_Channel(l), TRIM(SRF_Filename)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF

        ! Assign the channel number and sensor Ids
        ! to the interpolation structure
        iSRF%Channel          = SRF%Channel
        iSRF%WMO_Satellite_ID = SRF%WMO_Satellite_ID
        iSRF%WMO_Sensor_ID    = SRF%WMO_Sensor_ID


        ! Calculate the begin and end interpolated frequencies
        ! and the number of interpolated points
        ! ----------------------------------------------------
        ! The begin point and frequency
        n1 = iCompute(SRF%Begin_Frequency)
        iSRF%Begin_Frequency = fCompute(n1)

        ! The end point and frequency
        n2 = iCompute(SRF%End_Frequency)
        iSRF%End_Frequency = fCompute(n2)

        ! Total number of interpolate points
        n_iPoints = n2 - n1 + 1


        ! Allocate the interpolated SRF structure arrays
        ! ----------------------------------------------
        Error_Status = Allocate_SRF( n_iPoints, iSRF )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error allocating iSRF arrays for channel #",i0,".")') &
                          SensorInfo%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF


        ! Compute the interpolation frequency grid
        ! ----------------------------------------
        Error_Status = Frequency_SRF( iSRF )
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
        ! -------------------------
        Error_Status = Polynomial_Interpolate(  SRF%Frequency, &  ! Input
                                                SRF%Response , &  ! Input
                                               iSRF%Frequency, &  ! Input
                                               iSRF%Response , &  ! Output
                                               Order=ORDER     )  ! Optional input
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error interpolating SRF for channel #",i0)') &
                          SensorInfo%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF

        ! Any -ve bits due to the interpolation are set to zero
        WHERE( iSRF%Response < ZERO ) iSRF%Response = ZERO

        ! Normalise it
        iSRF%Response = iSRF%Response / MAXVAL( iSRF%Response )


        ! Integrate the interpolated SRF
        ! ------------------------------
        Error_Status = Integrate_SRF( iSRF )
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
                iSRF%Integrated_SRF, iSRF%Summation_SRF, &
                100.0_fp*(iSRF%Integrated_SRF-iSRF%Summation_SRF)/iSRF%Integrated_SRF


        ! Write the channel SRF to the netCDF file
        ! ----------------------------------------
        Error_Status = Write_SRF_netCDF( TRIM(iSRF_Filename), iSRF )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error occurred writing channel #",i0,&
                          &" interpolated SRF to ",a,".")') &
                          iSRF%Channel, TRIM(iSRF_Filename)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF


        ! Destroy SRF structures for next channel
        ! ---------------------------------------
        ! The interpolated SRF
        Error_Status = Destroy_SRF( iSRF )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error occurred destroying channel #",i0,&
                          &" iSRF structure.")') &
                         SensorInfo%Sensor_Channel(l)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF

        ! The input SRF
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

      END DO Channel_Loop

    END IF Available_Sensors


    ! Destroy the current SensorInfo structure
    ! ----------------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error destroying SensorInfo data for sensor # ",i0)') n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
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

  ! Function to compute the index of a frequency
  FUNCTION iCompute( f ) RESULT( i )
    REAL(fp), INTENT(IN) :: f
    INTEGER :: i
    i = INT( ONEpointFIVE + ((f - FREQUENCY_BEGIN)/FREQUENCY_INTERVAL(INTERPOLATION_INDEX)) )
  END FUNCTION iCompute
  
  ! Function to compute a frequency from an index
  FUNCTION fCompute( i ) RESULT( f )
    INTEGER, INTENT(IN) :: i
    REAL(fp) :: f
    f = FREQUENCY_BEGIN + (REAL(i-1,fp)*FREQUENCY_INTERVAL(INTERPOLATION_INDEX))
  END FUNCTION fCompute
  
END PROGRAM Interpolate_SRFs
