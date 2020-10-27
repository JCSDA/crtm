!
! TauProfile_OLD2NEW
!
! Program to convert previous format TauProfile netCDF datafiles to 
! the latest format.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 10-Oct-2007
!                     paul.vandelst@noaa.gov
!
!

PROGRAM TauProfile_OLD2NEW

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds              , ONLY: fp
  USE File_Utility            , ONLY: Get_Lun, File_Exists
  USE Message_Handler         , ONLY: SUCCESS, FAILURE, WARNING, Display_Message, Program_Message
  USE TauProfile_Define       , ONLY: TauProfile_type
  USE TauProfile_netCDF_IO    , ONLY: Create_TauProfile_netCDF, &
                                      Write_TauProfile_netCDF
  USE SensorInfo_Define       , ONLY: SensorInfo_type, &
                                      Destroy_SensorInfo
  USE SensorInfo_LinkedList   , ONLY: SensorInfo_List_type, &
                                      Count_SensorInfo_Nodes, &
                                      GetFrom_SensorInfo_List, &
                                      Destroy_SensorInfo_List
  USE SensorInfo_IO           , ONLY: Read_SensorInfo
  USE TauProfile_Define_old   , ONLY: TauProfile_type_old => TauProfile_type
  USE TauProfile_netCDF_IO_old, ONLY: Inquire_TauProfile_netCDF_old => Inquire_TauProfile_netCDF, &
                                      Read_TauProfile_netCDF_old => Read_TauProfile_netCDF
  ! Disable implicit typing
  IMPLICIT NONE
  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauProfile_OLD2NEW'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Direction information
  INTEGER     , PARAMETER :: N_DIRECTIONS = 2
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(N_DIRECTIONS) = (/ 'upwelling  ', &
                                                               'downwelling' /)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: Oldfile, Newfile
  CHARACTER(2000) :: ID_Tag, Title, History, Comment
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n, n_Sensors
  INTEGER :: iDir
  INTEGER :: n_k, n_l, n_i
  INTEGER :: m, n_m
  INTEGER :: j, n_j
  REAL(fp), ALLOCATABLE :: Pressure(:)
  INTEGER , ALLOCATABLE :: Channel(:)
  REAL(fp), ALLOCATABLE :: Angle(:)
  INTEGER , ALLOCATABLE :: Profile(:)
  INTEGER , ALLOCATABLE :: Molecule(:)
  REAL(fp), ALLOCATABLE :: Tau(:,:,:)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(TauProfile_type) :: TauProfile
    
  
  ! Output program header
  ! ---------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert the previous format TauProfile netCDF '//&
                        'datafiles to the latest format.', &
                        '$Revision$' )


  ! Read the SensorInfo file
  ! ------------------------
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


    ! Begin the direction loop
    ! ------------------------
    Direction_Loop: DO iDir = 1, N_DIRECTIONS
    

      ! Only operate on files that exist
      ! --------------------------------
      Oldfile = TRIM(DIRECTION_NAME(iDir))//'.'//TRIM(SensorInfo%Sensor_ID)//'.TauProfile.nc'
      Newfile = TRIM(Oldfile)//'.new'
      IF ( .NOT. File_Exists( Oldfile ) ) CYCLE Sensor_Loop

      ! Output an info message
      WRITE( *,'(/5x,"Converting TauProfile data for ",a)' ) TRIM(SensorInfo%Sensor_Id)


      ! Inquire the old format TauProfile file for
      ! its dimensions and global attributes
      ! ------------------------------------------
      Error_Status = Inquire_TauProfile_netCDF_old( OldFile                 , &  ! Input
                                                    n_Layers        =n_k    , &  ! Optional output
                                                    n_Channels      =n_l    , &  ! Optional output
                                                    n_Angles        =n_i    , &  ! Optional output
                                                    n_Profiles      =n_m    , &  ! Optional output
                                                    n_Molecule_Sets =n_j    , &  ! Optional output
                                                    ID_Tag          =ID_Tag , &  ! Optional output
                                                    Title           =Title  , &  ! Optional output
                                                    History         =History, &  ! Optional output
                                                    Comment         =Comment  )  ! Optional output
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring old format netCDF TauProfile file '//&
                              TRIM(Oldfile)//&
                              ' for dimensions and global attributes', &
                              Error_Status )
        STOP
      END IF


      ! Allocate arrays for this sensor
      ! -------------------------------
      ALLOCATE( Pressure(0:n_k) , &
                Channel(n_l)    , &
                Angle(n_i)      , &
                Profile(n_m)    , &
                Molecule(n_j)   , &
                Tau(n_k,n_l,n_i),  &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating arrays for ",a,". STAT=",i0)' ) &
                       TRIM(SensorInfo%Sensor_Id), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

  
      ! Get old format TauProfile file dimension lists
      ! ----------------------------------------------
      Error_Status = Inquire_TauProfile_netCDF_old( OldFile, &
                                                    Level_Pressure   =Pressure, &
                                                    Channel_List     =Channel , &
                                                    Angle_List       =Angle   , &
                                                    Profile_List     =Profile , &
                                                    Molecule_Set_List=Molecule  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring old format netCDF TauProfile file '//&
                              TRIM(Oldfile)//&
                              ' for dimension list data', &
                              Error_Status )
        STOP
      END IF
    
    
      ! Create the new TauProfile file
      ! ------------------------------
      Error_Status = Create_TauProfile_netCDF( Newfile , &
                                               Pressure, &
                                               Channel , &
                                               Angle   , &
                                               Profile , &
                                               Molecule, &
                                               Release         =TauProfile%Release         , &
                                               Version         =TauProfile%Version         , &
                                               Sensor_Id       =SensorInfo%Sensor_Id       , &
                                               WMO_Satellite_Id=SensorInfo%WMO_Satellite_Id, &
                                               WMO_Sensor_Id   =SensorInfo%WMO_Sensor_Id   , &
                                               ID_Tag          =TRIM(ID_Tag) , &
                                               Title           =TRIM(Title)  , &
                                               History         =PROGRAM_RCS_ID//'; '//&
                                                                TRIM(History), &
                                               Comment         =TRIM(Comment)  )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating the new for netCDF TauProfile file '//&
                              TRIM(Newfile), &
                              FAILURE )
        STOP
      END IF


      ! Transfer data by profile and molecule set
      ! -----------------------------------------
      ! Loop over dimensions
      Molecule_Loop: DO j = 1, n_j
        Profile_Loop: DO m = 1, n_m

          ! Read data from old format file
          Error_Status = Read_TauProfile_netCDF_old( Oldfile, &
                                                     Profile(m), Molecule(j), &
                                                     Tau, &
                                                     Quiet=SET )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading transmittance from ",a," for profile #",i0,&
                            &" and molecule #",i0)' ) &
                            TRIM(Oldfile), Profile(m), Molecule(j)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  FAILURE )
            STOP
          END IF

          ! Write data from old format file
          Error_Status = Write_TauProfile_netCDF( Newfile, &
                                                  Tau, &
                                                  Profile(m), Molecule(j), &
                                                  Quiet=SET )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error writing transmittance to ",a," for profile #",i0,&
                            &" and molecule #",i0)' ) &
                            TRIM(Newfile), Profile(m), Molecule(j)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  FAILURE )
            STOP
          END IF

        END DO Profile_Loop
      END DO Molecule_Loop


      ! Deallocate all file dependent arrays
      ! ------------------------------------
      DEALLOCATE( Pressure, &
                  Channel , &
                  Angle   , &
                  Profile , &
                  Molecule, &
                  Tau     , &
                  STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating arrays for ",a,". STAT=",i0)' ) &
                       TRIM(SensorInfo%Sensor_Id), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

    END DO Direction_Loop
    
    
    ! Destroy file independent structures
    ! -----------------------------------
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            FAILURE )
      STOP
    END IF
    
  END DO Sensor_loop


  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM TauProfile_OLD2NEW
