!
! Extract_IASI_TauCoeff_Subset
!
! Program to extract the IASI channel subset from the individual
! IASI band netCDF format TauCoeff data files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Extract_IASI_TauCoeff_Subset

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE List_File_Utility,     ONLY: Integer_List_File_type, &
                                   Read_List_File, &
                                   Get_List_Size, Get_List_Entry
  USE TauCoeff_Define,       ONLY: TauCoeff_type, &
                                   Allocate_TauCoeff, Destroy_TauCoeff
  USE TauCoeff_netCDF_IO,    ONLY: Inquire_TauCoeff_netCDF, &
                                   Read_TauCoeff_netCDF, &
                                   Write_TauCoeff_netCDF
  USE Channel_Subset_Define, ONLY: Channel_Subset_type, &
                                   Destroy_Channel_Subset
  USE IASI_Define,           ONLY: N_IASI_BANDS, &
                                   N_IASI_CHANNELS, &
                                   IASI_BAND
  USE IASI_Subset,           ONLY: N_IASI_SUBSET_300, IASI_SUBSET_300, IASI_SUBSET_300_COMMENT, &
                                   N_IASI_SUBSET_316, IASI_SUBSET_316, IASI_SUBSET_316_COMMENT, &
                                   N_IASI_SUBSET_616, IASI_SUBSET_616, IASI_SUBSET_616_COMMENT, &
                                   Index_IASI_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Extract_IASI_TauCoeff_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  
  INTEGER,      PARAMETER :: N_VALID_SETS = 5
  CHARACTER(*), PARAMETER :: VALID_SET_NAME(N_VALID_SETS) = &
    (/ 'EUMETSAT 300 channel set                ', &
       'NESDIS 316 channel set                  ', &
       'Combined EUMETSAT/NESDIS 616 channel set', &
       'All channels                            ', &
       'User specified                          ' /)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: List_Filename
  CHARACTER(256)  :: In_Filename
  CHARACTER(256)  :: Out_Filename
  CHARACTER(256)  :: Sensor_Name
  CHARACTER(256)  :: Platform_Name
  CHARACTER(256)  :: ID_Tag
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: Subset_Comment
  CHARACTER(20)   :: Sensor_ID
  INTEGER :: i, Set
  LOGICAL :: First_Band
  INTEGER :: n_Orders
  INTEGER :: n_Predictors     
  INTEGER :: n_Absorbers      
  INTEGER :: Version
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: j, l, l1, l2
  INTEGER :: n_Subset_Channels
  INTEGER, ALLOCATABLE :: Subset_List(:)
  TYPE(Integer_List_File_type) :: User_Subset_List
  TYPE(TauCoeff_type) :: In_TauCoeff, Out_TauCoeff
  TYPE(Channel_Subset_type) :: Subset

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to extract the IASI channel SUBSET transmittance '//&
                       'coefficient data from the individual band netCDF '//&
                       'TauCoeff files and write them to a separate netCDF '//&
                       'datafile.', &
                       '$Revision$' )

  ! Select a subset set
  ! -------------------
  Select_Loop: DO

    ! Prompt user to select a subset set 
    WRITE( *,'(/5x,"Select an IASI channel subset")' )
    DO i = 1, N_VALID_SETS
      WRITE( *,'(10x,i1,") ",a)' ) i, VALID_SET_NAME(i)
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,FMT='(i5)',IOSTAT=IO_Status ) Set

    ! Check for I/O errors
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Invalid input', &
                            FAILURE )
      STOP
    END IF
    
    ! Check the input
    IF ( Set < 1 .OR. Set > N_VALID_SETS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Invalid selection', &
                            FAILURE )
      STOP
    ELSE
      EXIT Select_Loop
    END IF

  END DO Select_Loop


  ! Get the required channels list
  ! ------------------------------
  SELECT CASE ( Set )

    ! The 300 channel subset
    ! ----------------------
    CASE (1)
    
      ! Assign values
      n_Subset_Channels = N_IASI_SUBSET_300
      Subset_Comment    = IASI_SUBSET_300_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = IASI_SUBSET_300
      Sensor_ID   = 'iasi300_metop-a'


    ! The 316 channel subset
    ! ----------------------
    CASE (2)
    
      ! Assign values
      n_Subset_Channels = N_IASI_SUBSET_316
      Subset_Comment    = IASI_SUBSET_316_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = IASI_SUBSET_316
      Sensor_ID   = 'iasi316_metop-a'


    ! The combine channel subset
    ! --------------------------
    CASE (3)
    
      ! Assign values
      n_Subset_Channels = N_IASI_SUBSET_616
      Subset_Comment    = IASI_SUBSET_616_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = IASI_SUBSET_616
      Sensor_ID   = 'iasi616_metop-a'


    ! All the channels
    ! ----------------
    CASE(4)
    
      ! Assign values
      n_Subset_Channels = N_IASI_CHANNELS
      Subset_Comment    = 'IASI full channel set'

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = (/(l,l=1,N_IASI_CHANNELS)/)
      WRITE( Sensor_ID,'("iasi",i0,"_metop-a")' ) n_Subset_Channels


    ! A user specified channel subset
    ! -------------------------------
    CASE (5)
    
      ! Get a channel subset list filename
      WRITE( *, FMT='(/5x,"Enter an IASI channel subset list filename : ")', &
                ADVANCE='NO' )
      READ( *,FMT='(a)' ) List_Filename
      List_Filename = ADJUSTL(List_Filename)

      ! Read the channel subset list file
      Error_Status = Read_List_File( List_Filename, &
                                     User_Subset_List )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading list file '//TRIM(List_Filename), &
                              Error_Status )
        STOP
      END IF

      ! Retrieve the number of subset channels
      n_Subset_Channels = Get_List_Size( User_Subset_List )
      IF ( n_Subset_Channels < 1 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'No channels listed in '//TRIM(List_Filename), &
                              Error_Status )
        STOP
      END IF

      ! Check the number of channels
      IF ( n_Subset_Channels < 1 .OR. n_Subset_Channels > N_IASI_CHANNELS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Number of channels listed in '//&
                              TRIM( List_Filename )//' outside of valid range.', &
                              Error_Status )
        STOP
      END IF

      ! Allocate the subset list to use
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill the subset list
      DO l = 1, n_Subset_Channels
        Error_Status = Get_List_Entry( User_Subset_List, l, Subset_List(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error retrieving user subset channel list entry ",i4)' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF
      END DO

      ! Create the subset comment and sensor id
      WRITE( Subset_Comment,'("User specified IASI ",i0," channel SUBSET")' ) n_Subset_Channels
      WRITE( Sensor_ID,'("iasi",i0,"_metop-a")' ) n_Subset_Channels

  END SELECT


  ! Initialise the start index for output
  ! and the "initial band" flag
  ! -------------------------------------
  l1 = 1
  First_Band = .TRUE.


  ! Loop over bands to extract subset channels
  ! ------------------------------------------
  Band_Loop: DO l = 1, N_IASI_BANDS


    ! Current band channel subset
    ! ---------------------------
    ! Determine the subset channel indices
    ! for the current band
    Error_Status = Index_IASI_Subset( l, Subset_List, Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for band '//&
                            TRIM(IASI_BAND(l)), &
                            Error_Status )
      STOP
    END IF

    ! Output the number of channels to extract
    WRITE( *,'(/10x,"There are ",i0," channels to be extracted from band ",a,":")' ) &
             Subset%n_Channels, TRIM(IASI_BAND(l))


    ! Read the input TauCoeff file if required
    ! ----------------------------------------
    Non_Zero_n_Channels: IF ( Subset%n_Channels > 0 ) THEN

      ! Output the list of channel numbers to extract
      WRITE( *,'(10x,10i5)' ) Subset%Channel_Number

      ! Define the filename
      In_Filename = 'iasi'//TRIM(IASI_BAND(l))//'_metop-a.TauCoeff.nc'


      ! Get the file release/version info and
      ! global attributes for the initial band
      ! --------------------------------------
      IF ( First_Band ) THEN

        ! **NOTE: No Update of First_Band here. It's used later**

        ! Inquire the file
        Error_Status = Inquire_TauCoeff_netCDF( TRIM(In_Filename), &
                                                n_Orders      = n_Orders, &
                                                n_Predictors  = n_Predictors, &
                                                n_Absorbers   = n_Absorbers, &
                                                Release       = Out_TauCoeff%Release, &
                                                Version       = Out_TauCoeff%Version, &
                                                History       = History, &
                                                Sensor_Name   = Sensor_Name, &
                                                Platform_Name = Platform_Name, &
                                                Comment       = Comment, &
                                                ID_Tag        = ID_Tag )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error inquiring the input netCDF TauCoeff file '//&
                                TRIM(In_Filename), &
                                Error_Status )
          STOP
        END IF

        ! Append onto the comment attribute.
        Comment = TRIM(Subset_Comment)//'; '//TRIM(Comment)

        ! Allocate the output structure
        Error_Status = Allocate_TauCoeff( n_Orders, &
                                          n_Predictors, &
                                          n_Absorbers, &
                                          n_Subset_Channels, &
                                          Out_TauCoeff )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error allocating output TauCoeff data structure.', &
                                Error_Status )
          STOP
        END IF

      END IF


      ! Get the Version info and compare
      ! --------------------------------
      Error_Status = Inquire_TauCoeff_netCDF( TRIM(In_Filename), &
                                              Version = Version )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the input netCDF TauCoeff file '//&
                              TRIM(In_Filename)//' for Release/Version info.', &
                              Error_Status )
        STOP
      END IF

      ! Check the Version value. If different - issue warning and continue, 
      ! but modify the Comment global attribute field for output
      IF ( Out_TauCoeff%Version /= Version ) THEN
        WRITE( Message,'("Input file ",a," Version, ",i0, &
                        &", is different from previous file value, ",i0,".")' ) &
                        TRIM(In_Filename), Version, Out_TauCoeff%Version
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              WARNING )
        Comment = TRIM(Message)//'; '//TRIM(Comment)
      END IF


      ! Read the data
      ! -------------
      Error_Status = Read_TauCoeff_netCDF( In_Filename, &
                                           In_TauCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF IASI TauCoeff file '//&
                              TRIM(In_Filename), &
                              Error_Status )
        STOP
      END IF


      ! Copy or check the absorber id and alpha data
      ! --------------------------------------------
      IF ( First_Band ) THEN

        ! Now update the First_Band flag
        First_Band = .FALSE.

        ! Simply copy these for the first band
        Out_TauCoeff%Absorber_ID = In_TauCoeff%Absorber_ID
        Out_TauCoeff%Alpha       = In_TauCoeff%Alpha
        Out_TauCoeff%Alpha_C1    = In_TauCoeff%Alpha_C1
        Out_TauCoeff%Alpha_C2    = In_TauCoeff%Alpha_C2

      ELSE

        DO j = 1, In_TauCoeff%n_Absorbers

          ! Check Absorber IDs
          IF ( In_TauCoeff%Absorber_ID(j) /= Out_TauCoeff%Absorber_ID(j) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," ID values are different for band ",a)' ) &
                            j, TRIM(IASI_Band(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha(j), Out_TauCoeff%Alpha(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," Alpha values are different for band ",a)' ) &
                            j, TRIM(IASI_Band(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha_C1 value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha_C1(j), Out_TauCoeff%Alpha_C1(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," Alpha_C1 values are different for band ",a)' ) &
                            j, TRIM(IASI_Band(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check Alpha_C2 value
          IF ( .NOT. Compare_Float( In_TauCoeff%Alpha_C2(j), Out_TauCoeff%Alpha_C2(j) ) ) THEN
            Error_Status = FAILURE
            WRITE( Message, '( "Absorber #",i2," ALpha_C2 values are different for band ",a)' ) &
                            j, TRIM(IASI_Band(l))
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

        END DO

      END IF

      ! Copy the required channel's data
      ! --------------------------------
      l2 = l1 + Subset%n_Channels - 1
      Out_TauCoeff%Sensor_Descriptor(l1:l2)   = TRIM(Sensor_ID)
      Out_TauCoeff%NCEP_Sensor_ID(l1:l2)      = In_TauCoeff%NCEP_Sensor_ID(Subset%Channel_Index)
      Out_TauCoeff%WMO_Satellite_ID(l1:l2)    = In_TauCoeff%WMO_Satellite_ID(Subset%Channel_Index)
      Out_TauCoeff%WMO_Sensor_ID(l1:l2)       = In_TauCoeff%WMO_Sensor_ID(Subset%Channel_Index)
      Out_TauCoeff%Sensor_Channel(l1:l2)      = In_TauCoeff%Sensor_Channel(Subset%Channel_Index)
      Out_TauCoeff%Order_Index(:,:,l1:l2)     = In_TauCoeff%Order_Index(:,:,Subset%Channel_Index)
      Out_TauCoeff%Predictor_Index(:,:,l1:l2) = In_TauCoeff%Predictor_Index(:,:,Subset%Channel_Index)
      Out_TauCoeff%C(:,:,:,l1:l2)             = In_TauCoeff%C(:,:,:,Subset%Channel_Index)
      l1 = l2 + 1


      ! Destroy the input TauCoeff structure
      ! for the next band read
      ! ------------------------------------
      Error_Status = Destroy_TauCoeff( In_TauCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying TauCoeff structure for input from '//&
                              TRIM(In_Filename), &
                              Error_Status )
        STOP
      END IF

    END IF Non_Zero_n_Channels


    ! Destroy the IASI channel subset structure
    ! -----------------------------------------
    Error_Status = Destroy_Channel_Subset( Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying IASI Channel Subset structure for input from '//&
                            TRIM(In_Filename), &
                            Error_Status )
      STOP
    END IF

  END DO Band_Loop

  ! Write the output SpcCoeff file
  ! ------------------------------
  ! The output filename
  Out_Filename = TRIM(Sensor_ID)//'.TauCoeff.nc'

  ! Set the number of sensors
  Out_TauCoeff%n_Sensors = 1

  ! Write the data
  WRITE( *,'(/10x,"Creating the output file...")' )
  Error_Status = Write_TauCoeff_netCDF( TRIM(Out_Filename), &
                                        Out_TauCoeff, &
                                        Title         = 'Optical depth coefficients for '//&
                                                        TRIM(Sensor_ID), &
                                        History       = PROGRAM_RCS_ID//'; '//&
                                                        TRIM(History), &
                                        Sensor_Name   = TRIM(Sensor_Name), &
                                        Platform_Name = TRIM(Platform_Name), &
                                        Comment       = 'Data extracted from the individual '//&
                                                        'IASI band TauCoeff datafiles.; '//&
                                                        TRIM(Comment), &
                                        ID_Tag        = TRIM(ID_Tag) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the IASI TauCoeff file '//&
                          TRIM(Out_Filename), &
                          FAILURE )
    STOP
  END IF


  ! Destroy the output TauCoeff structure
  ! -------------------------------------
  Error_Status = Destroy_TauCoeff( Out_TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure for output to '//&
                          TRIM(Out_Filename), &
                          Error_Status )
  END IF

END PROGRAM Extract_IASI_TauCoeff_Subset
