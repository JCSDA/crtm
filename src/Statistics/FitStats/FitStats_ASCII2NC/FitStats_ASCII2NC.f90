!
! FitStats_ASCII2NC
!
! Program to test the FitStats structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jan-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM FitStats_ASCII2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: fp
  USE File_Utility         , ONLY: Get_Lun, File_Exists
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                   Display_Message, Program_Message
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type, &
                                   Count_SensorInfo_Nodes, &
                                   GetFrom_SensorInfo_List, &
                                   Destroy_SensorInfo_List
  USE SensorInfo_IO        , ONLY: Read_SensorInfo
  USE SpcCoeff_Define      , ONLY: SpcCoeff_type   , &
                                   Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO   , ONLY: Read_SpcCoeff_netCDF
  USE FitStats_Define      , ONLY: FitStats_type, &
                                   Destroy_FitStats, &
                                   Allocate_FitStats
  USE FitStats_netCDF_IO   , ONLY: Write_FitStats_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'FitStats_ASCII2NC'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1

  ! The allowed absorber components
  INTEGER,      PARAMETER :: MAX_N_MOL_SETS = 3
  CHARACTER(*), PARAMETER :: MOL_SET_NAME(MAX_N_MOL_SETS) = (/'wet','dry','ozo'/)

  ! The dependent profile set ID
  CHARACTER(*), PARAMETER :: ID_TAG = 'UMBC'

  ! The ASCII file read format  
  CHARACTER(*), PARAMETER :: READ_FMT = &
  '(i4,3x,i2,1x,i2,1x,6(1x,i2),1x,f7.4,3x,4(f8.5),3x,4(f9.6),3x,3(f9.6),3x,f7.3)'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: SpcCoeff_Filename
  CHARACTER(256) :: ASCII_Filename, NC_Filename
  CHARACTER(256) :: Message
  CHARACTER(1000) :: Comment
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  INTEGER :: j, l, n, n_Sensors, n_Mol_Sets
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(SpcCoeff_type) :: SpcCoeff
  TYPE(FitStats_type) :: FitStats

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert ASCII FitStats files to netCDF format.', &
                        '$Revision$' )


  ! Get user input
  ! --------------
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


    ! Set the number of molecule sets to use
    ! --------------------------------------
    n_Mol_Sets = MAX_N_MOL_SETS
    IF ( SensorInfo%Microwave_Flag == SET ) n_Mol_Sets = 2

    
    ! Only operate on sensors for which ALL data is available
    ! -------------------------------------------------------
    SpcCoeff_Filename = TRIM(SensorInfo%Sensor_ID)//'.SpcCoeff.nc'
    IF ( .NOT. File_Exists( SpcCoeff_Filename ) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(SpcCoeff_Filename)//' not present. Skipping '//&
                            TRIM(SensorInfo%Sensor_ID)//'...', &
                            INFORMATION )
      CYCLE Sensor_Loop
    END IF
    DO j = 1, n_Mol_Sets
      ASCII_Filename = 'stat.'//TRIM(SensorInfo%Sensor_ID)//'.'//&
                       MOL_SET_NAME(j)//'.varyingOrder.umbc48101.txt'
      IF ( .NOT. File_Exists( ASCII_Filename ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(ASCII_Filename)//' not present. Skipping '//&
                              TRIM(SensorInfo%Sensor_ID)//'...', &
                              INFORMATION )
        CYCLE Sensor_Loop
      END IF
    END DO


    ! Read the SpcCoeff data
    ! ----------------------
    Error_Status = Read_SpcCoeff_netCDF( SpcCoeff_Filename, SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading netCDF SpcCoeff file '//&
                            TRIM(SpcCoeff_Filename)//'.', &
                            Error_Status )
      STOP
    END IF


    ! Begin the main molecule/absorber set loop
    ! -----------------------------------------
    Molecule_Loop: DO j = 1, n_Mol_Sets
    
    
      ! Construct filenames
      ! -------------------
      ASCII_Filename = 'stat.'//TRIM(SensorInfo%Sensor_ID)//'.'//&
                       MOL_SET_NAME(j)//'.varyingOrder.umbc48101.txt'
      NC_Filename = MOL_SET_NAME(j)//'.'//TRIM(SensorInfo%Sensor_ID)//'.FitStats.nc'

      WRITE( *,'(/5x,"Converting ASCII ",a," FitStats to netCDF format for ",a,"...")' ) &
               MOL_SET_NAME(j), TRIM(SensorInfo%Sensor_ID)

  
      ! Allocate the FitStats structure
      ! ---------------------------------
      Error_Status = Allocate_FitStats( SensorInfo%n_Channels, FitStats )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating FitStats structure.', &
                              FAILURE )
        STOP
      END IF


      ! Fill the external data fields
      ! -----------------------------
      FitStats%Sensor_Id        = TRIM(SensorInfo%Sensor_Id)
      FitStats%WMO_Satellite_Id = SensorInfo%WMO_Satellite_Id
      FitStats%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_Id
      FitStats%Frequency        = SpcCoeff%Wavenumber
      
      
      ! Read the ASCII file
      ! -------------------
      ! Open the file
      FileID = Get_Lun()
      IF ( FileID < 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error getting ASCII file id.', &
                              FAILURE )
        STOP
      END IF
      OPEN( FileID,FILE  =ASCII_Filename, &
                   FORM  ='FORMATTED'   , &
                   STATUS='OLD'         , &
                   ACCESS='SEQUENTIAL'  , &
                   ACTION='READ'        , &
                   IOSTAT=IO_Status       )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error opening file ",a,". IOSTAT=",i0)') &
                       TRIM(ASCII_Filename), IO_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
      ! Read data channel by channel
      Channel_Loop: DO l = 1, SensorInfo%n_Channels
        READ( FileID,FMT=READ_FMT,IOSTAT=IO_Status ) FitStats%Sensor_Channel(l) , &
                                                     FitStats%Order(l)          , &
                                                     FitStats%n_Predictors(l)   , &
                                                     FitStats%Predictor_Idx(:,l), &
                                                     FitStats%Fit_Residual(l)   , &
                                                     FitStats%Tb_BIAS(l)        , &
                                                     FitStats%Tb_SDEV(l)        , &
                                                     FitStats%Tb_RMS(l)         , &
                                                     FitStats%Tb_MAX(l)         , &
                                                     FitStats%Tau_BIAS(l)       , &
                                                     FitStats%Tau_SDEV(l)       , &
                                                     FitStats%Tau_RMS(l)        , &
                                                     FitStats%Tau_MAX(l)        , &
                                                     FitStats%Tau_Max_BIAS(l)   , &
                                                     FitStats%Tau_Max_SDEV(l)   , &
                                                     FitStats%Tau_Max_RMS(l)    , &
                                                     FitStats%Max_Pred_Term(l) 
        IF ( IO_Status /= 0 ) THEN
          WRITE( Message,'("Error reading channel ",i0," data from ",a,". IOSTAT=",i0)') &
                         SensorInfo%Sensor_Channel(l), TRIM(ASCII_Filename), IO_Status
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                FAILURE )
          STOP
        END IF
      END DO Channel_Loop
      ! Done
      CLOSE( FileID )


      ! Write the netCDF file
      ! ---------------------
      Error_Status = Write_FitStats_netCDF( NC_Filename, FitStats, &
                                            ID_Tag  = ID_TAG, &
                                            Title   = 'CompactOPTRAN '//MOL_SET_NAME(j)//&
                                                      'fit statistics'  , &
                                            History = PROGRAM_RCS_ID     , &
                                            Comment = 'This is a comment'  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing FitStats to '//TRIM(NC_Filename), &
                              FAILURE )
        STOP
      END IF


      ! Deallocate the current FitStats structure
      ! -----------------------------------------
      Error_Status = Destroy_FitStats( FitStats )      
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error deallocating '//MOL_SET_NAME(j)//' FitStats', &
                              FAILURE )
        STOP
      END IF

    END DO Molecule_Loop      
      
      
    ! Destroy the current SpcCoeff structure
    ! --------------------------------------
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff data structure.', &
                            FAILURE )
      STOP
    END IF

    
    ! Destroy the current SensorInfo structure
    ! ----------------------------------------
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
  
END PROGRAM FitStats_ASCII2NC
