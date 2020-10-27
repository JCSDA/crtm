!
! Effective_TauProfile
!
! Program to compute the effective molecular transmittances profiles
! from the molecular combinations determined from the output of the
! Select_TauProfile program.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Effective_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: File_Exists
  USE Binary_File_Utility      , ONLY: Open_Binary_File
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Display_Message, Program_Message
  USE SensorInfo_Define        , ONLY: SensorInfo_type, &
                                       Destroy_SensorInfo
  USE SensorInfo_LinkedList    , ONLY: SensorInfo_List_type, &
                                       Count_SensorInfo_Nodes, &
                                       GetFrom_SensorInfo_List, &
                                       Destroy_SensorInfo_List
  USE SensorInfo_IO            , ONLY: Read_SensorInfo
  USE TauProfile_Define        , ONLY: TauProfile_type
  USE TauProfile_netCDF_IO     , ONLY: Modify_TauProfile_GAtts, &
                                       Create_TauProfile_netCDF, &
                                       Inquire_TauProfile_netCDF, &
                                       Read_TauProfile_netCDF   , &
                                       Write_TauPRofile_netCDF
  USE Tau_Production_Parameters, ONLY: N_LAYERS             , &
                                       N_DIRECTIONS         , &
                                       UPWELLING_DIRECTION  , &
                                       DOWNWELLING_DIRECTION, &
                                       DIRECTION_NAME       , &
                                       N_PROFILE_SETS       , &
                                       PROFILE_SET_ID_TAG   , &
                                       LEVEL_PRESSURE
  USE Tau_Production_Utility   , ONLY: Create_Signal_File
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Effective_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp

  INTEGER, PARAMETER :: WLO_IDX =  1  ! H2O lines only, no continua
  INTEGER, PARAMETER :: ALL_IDX = 10  ! First 7 molecules with continua
  INTEGER, PARAMETER :: WVO_IDX = 11  ! H2O and O3 only with continua
  INTEGER, PARAMETER :: WET_IDX = 12  ! H2O lines and continua
  INTEGER, PARAMETER :: DRY_IDX = 13  ! Dry gases (no H2O and O3) and continua
  INTEGER, PARAMETER :: OZO_IDX = 14  ! O3 lines and continua
  INTEGER, PARAMETER :: WCO_IDX = 15  ! H2O continua only, no line absorption
  INTEGER, PARAMETER :: DOZ_IDX = 16  ! Dry and O3 only with continua
  INTEGER, PARAMETER :: WVD_IDX = 17  ! H2O and Dry with continua
  ! "effective" transmittances
  INTEGER, PARAMETER :: EFF_WET_IDX = WET_IDX + 100
  INTEGER, PARAMETER :: EFF_DRY_IDX = DRY_IDX + 100
  INTEGER, PARAMETER :: EFF_OZO_IDX = OZO_IDX + 100
  
  ! The number of individual absorbers (wet, dry, ozo)
  INTEGER,PARAMETER :: N_TAU = 3
  
  ! Definition of derived transmittance "sets" (the j1 value)
  INTEGER     , PARAMETER :: N_DERIVED_SETS = 3
  INTEGER     , PARAMETER :: WVO_DERIVED = 1
  INTEGER     , PARAMETER :: DOZ_DERIVED = 2
  INTEGER     , PARAMETER :: WVD_DERIVED = 3
  CHARACTER(*), PARAMETER :: DERIVED_SET_NAME(N_DERIVED_SETS) = (/'WVO', 'DOZ', 'WVD'/)
  
  ! Definition of derived tau "subsets" (the j2 value)
  INTEGER     , PARAMETER :: N_DERIVED_SUBSETS = 2
  CHARACTER(*), PARAMETER :: DERIVED_SUBSET_NAME(N_DERIVED_SUBSETS) = (/'1', '2'/)
  
  ! The TauProfile indices to read for each transmittance combination
  INTEGER, PARAMETER :: TAU_IDX(N_TAU,N_DERIVED_SUBSETS,N_DERIVED_SETS) = &
    RESHAPE((/ ALL_IDX,WVO_IDX,WET_IDX, &    ! WVO1
               ALL_IDX,WVO_IDX,OZO_IDX, &    ! WVO2
               ALL_IDX,DOZ_IDX,DRY_IDX, &    ! DOZ1
               ALL_IDX,DOZ_IDX,OZO_IDX, &    ! DOZ2
               ALL_IDX,WVD_IDX,WET_IDX, &    ! WVD1
               ALL_IDX,WVD_IDX,DRY_IDX /),&  ! WVD2
            (/N_TAU,N_DERIVED_SUBSETS,N_DERIVED_SETS/) )

  ! The TauProfile indices for the generated effective transmittances
  ! Note that for each set, the last effective transmittance is actually
  ! the LBL-generated transmittance.
  INTEGER, PARAMETER :: EFF_TAU_IDX(N_TAU,N_DERIVED_SUBSETS,N_DERIVED_SETS) = &
    RESHAPE((/ EFF_DRY_IDX,EFF_OZO_IDX,EFF_WET_IDX, &    ! WVO1
               EFF_DRY_IDX,EFF_WET_IDX,EFF_OZO_IDX, &    ! WVO2
               EFF_WET_IDX,EFF_OZO_IDX,EFF_DRY_IDX, &    ! DOZ1
               EFF_WET_IDX,EFF_DRY_IDX,EFF_OZO_IDX, &    ! DOZ2
               EFF_OZO_IDX,EFF_DRY_IDX,EFF_WET_IDX, &    ! WVD1
               EFF_OZO_IDX,EFF_WET_IDX,EFF_DRY_IDX /),&  ! WVD2
            (/N_TAU,N_DERIVED_SUBSETS,N_DERIVED_SETS/) )


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: ID_Tag
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: TauProfile_Filename, Out_TauProfile_Filename
  CHARACTER(256)  :: Signal_Filename
  CHARACTER(256)  :: dTb_Filename
  INTEGER :: dTb_FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: iDir
  INTEGER :: Release, Version
  INTEGER :: l, n_l, n_Channels
  INTEGER :: i, n_i, n_Angles
  INTEGER :: m, n_m, n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: j1, n_j1
  INTEGER :: j2, n_j2
  INTEGER :: n, n_Sensors
  INTEGER :: Read_Molecule(N_TAU), Write_Molecule(N_TAU)
  INTEGER , ALLOCATABLE :: Channels(:)
  REAL(fp), ALLOCATABLE :: Angles(:)
  INTEGER , ALLOCATABLE :: Profiles(:)
  REAL(fp), ALLOCATABLE :: Frequency(:)
  REAL(fp), ALLOCATABLE :: dTb(:,:,:,:,:)
  REAL(fp), ALLOCATABLE :: RMS_dTb(:,:,:)
  INTEGER , ALLOCATABLE :: SET_dTb(:,:)
  REAL(fp) :: Tau(N_LAYERS,N_TAU)
  REAL(fp), ALLOCATABLE :: Tau_ALL(:,:,:,:)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to compute the effective molecular transmittances profiles '//&
                        'from the molecular combinations determined from the output of the '//&
                        'Select_TauProfile program.', &
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

  ! The calculation direction
  WRITE( *, FMT='(/5x,"Select atmospheric path")' )
  DO i = 1, N_DIRECTIONS
    WRITE( *,FMT='(10x,i1,") ",a)' ) i, TRIM(DIRECTION_NAME(i))
  END DO
  WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
  READ( *,FMT='(i1)',IOSTAT=IO_Status ) iDir
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF
  IF ( iDir /= UPWELLING_DIRECTION   .AND. &
       iDir /= DOWNWELLING_DIRECTION       ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF


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


    ! Only operate on sensors for which data is available
    ! ---------------------------------------------------
    TauProfile_Filename = TRIM(DIRECTION_NAME(iDir))//'.'//&
                          TRIM(SensorInfo%Sensor_ID)//'.TauProfile.nc'
    dTb_Filename = TRIM(SensorInfo%Sensor_ID)//'.dTb_Stats.bin'
    
    ! Cycle loop if no data
    IF ( (.NOT. File_Exists( TauProfile_Filename )) .OR. &
         (.NOT. File_Exists( dTb_Filename        ))      ) CYCLE Sensor_Loop

    WRITE( *,'(/5x,"Computing effective transmittances for ",a,"...")' ) &
             TRIM(SensorInfo%Sensor_ID)
    
    
    ! Read the dTb data
    ! -----------------
    Error_Status = Open_Binary_File( dTb_Filename, &
                                     dTb_FileID    )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error opening dTb file '//TRIM(dTb_Filename), &
                            FAILURE )
      STOP
    END IF
    ! Read the dimensions
    READ( dTb_FileID,IOSTAT=IO_Status ) n_Angles  , &
                                        n_Profiles, &
                                        n_j2      , &
                                        n_j1      , &
                                        n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading dimensions from dTb file ",a,". IOSTAT = ",i0)' ) &
                     TRIM(dTb_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    ! Check the j-values
    IF ( n_j2 /= N_DERIVED_SUBSETS .OR. n_j1 /= N_DERIVED_SETS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'j-values from file inconsistent with parameters', &
                            FAILURE )
      STOP
    END IF
    ! Allocate the data arrays
    ALLOCATE( Frequency( n_Channels ), &
              dTb( n_Angles         , &
                   n_Profiles       , &
                   N_DERIVED_SUBSETS, &
                   N_DERIVED_SETS   , &
                   n_Channels         ), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating dTb data arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    ! Read the channel frequency data
    READ( dTb_FileID,IOSTAT=IO_Status ) Frequency
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message,'("Error reading frequency data from dTb file ",a,". IOSTAT = ",i0)' ) &
                     TRIM(dTb_Filename), IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    ! Read the brightness temperature residual data
    DO l = 1, n_Channels
      READ( dTb_FileID,IOSTAT=IO_Status ) dTb(:,:,:,:,l)
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message,'("Error reading Tb residual data from dTb file ",a,". IOSTAT = ",i0)' ) &
                       TRIM(dTb_Filename), IO_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    END DO


    ! Compute the residual RMS for all angles/profiles
    ! ------------------------------------------------
    ! Allocate the statistics arrays
    ALLOCATE( RMS_dTb( N_DERIVED_SUBSETS, &
                       N_DERIVED_SETS   , &
                       n_Channels         ), &
              SET_dTb( 2, n_Channels ), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating dTb statistics arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    ! Loop over transmittance data sets
    DO l = 1, n_Channels
      DO j1 = 1, N_DERIVED_SETS
        DO j2 = 1, N_DERIVED_SUBSETS
          RMS_dTb(j2,j1,l) = SQRT(SUM(dTb(:,:,j2,j1,l)**2)/REAL(n_Angles*n_Profiles,fp))
        END DO
      END DO
      ! Determine the transmittance dataset with the lowest Tb RMS
      SET_dTb(:,l) = MINLOC(RMS_dTb(:,:,l))
    END DO


    ! Inquire the TauProfile file to get
    ! dimensions and global attributes
    ! ----------------------------------
    Error_Status = Inquire_TauProfile_netCDF( TauProfile_Filename, &
                                              n_Channels     =n_l, &
                                              n_Angles       =n_i, &
                                              n_Profiles     =n_m, &
                                              n_Molecule_Sets=n_j, &
                                              Release        =Release, &
                                              Version        =Version, &
                                              ID_Tag         =ID_Tag , &
                                              History        =History, &
                                              Comment        =Comment  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM(TauProfile_Filename)//' dimensions.', &
                            Error_Status )
      STOP
    END IF
    
    
    ! Inquire the file to get the dimension lists
    ! -------------------------------------------
    ! Allocate the array
    ALLOCATE( Channels(n_l), Angles(n_i), Profiles(n_m), STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating dimension list arrays. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    
    ! Read the dimension lists
    Error_Status = Inquire_TauProfile_netCDF( TauProfile_Filename  , &
                                              Channel_List=Channels, &
                                              Angle_List  =Angles  , &
                                              Profile_List=Profiles  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM(TauProfile_Filename)//' dimension lists.', &
                            Error_Status )
      STOP
    END IF


    ! Create the output file
    ! ----------------------
    Out_TauProfile_Filename = TRIM(DIRECTION_NAME(iDir))//'.'//&
                              TRIM(SensorInfo%Sensor_ID)//'.'//&
                              'Effective.TauProfile.nc'
    Error_Status = Create_TauProfile_netCDF( Out_TauProfile_Filename, &
                                             LEVEL_PRESSURE, &
                                             Channels, &
                                             Angles, &
                                             Profiles, &
                                             (/EFF_WET_IDX,EFF_DRY_IDX,EFF_OZO_IDX/), &
                                             Release         =Release, &
                                             Version         =Version, &
                                             Sensor_ID       =TRIM(SensorInfo%Sensor_ID), &
                                             WMO_Satellite_ID=SensorInfo%WMO_Satellite_ID, &
                                             WMO_Sensor_ID   =SensorInfo%WMO_Sensor_ID, &
                                             ID_Tag          =TRIM(ID_Tag), &
                                             Title           =TRIM(DIRECTION_NAME(iDir))//' '//&
                                                              ' effective transmittances for '//&
                                                              TRIM(SensorInfo%Sensor_ID), &
                                             History         =PROGRAM_RCS_ID//'; '//TRIM(History), &
                                             Comment         =TRIM(Comment) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating netCDF TauProfile file '//&
                            TRIM(Out_TauProfile_Filename), &
                            Error_Status )
      STOP
    END IF
    
    
    ! Begin CHANNEL loop
    ! ------------------
    Channel_Loop: DO l = 1, n_l


      ! Set the transmittance indices to read and write
      ! -----------------------------------------------
      Read_Molecule  = TAU_IDX(:,SET_dTb(1,l),SET_dTb(2,l))
      Write_Molecule = EFF_TAU_IDX(:,SET_dTb(1,l),SET_dTb(2,l))


      ! Begin the PROFILE and ANGLE loops
      ! ---------------------------------
      Profile_Loop: DO m = 1, n_m
        Angle_Loop:   DO i = 1, n_i

          ! Read the transmittance data
          Molecule_Read_Loop: DO j = 1, N_TAU
            Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                   Channels(l)     , &
                                                   Angles(i)       , &
                                                   Profiles(m)     , &
                                                   Read_Molecule(j), &
                                                   Tau(:,j)        , &
                                                   Quiet=SET         )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", profile ",i0,&
                              &", and molid ",i0," from ", a)' ) &
                              Channels(l), i, Profiles(m), Read_Molecule(j), TRIM(TauProfile_Filename)
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM(Message), &
                                    Error_Status )
              STOP
            END IF
          END DO Molecule_Read_Loop


          ! Compute the effective transmittances
          EffTau_Loop: DO j = 1, N_TAU-1
            CALL Compute_EffTau(Tau(:,j),Tau(:,j+1))
          END DO EffTau_Loop

          ! Write the transmittance data
          Molecule_Write_Loop: DO j = 1, N_TAU
            Error_Status = Write_TauProfile_netCDF( Out_TauProfile_Filename, &
                                                    Tau(:,j)         , &
                                                    Channels(l)      , &
                                                    Angles(i)        , &
                                                    Profiles(m)      , &
                                                    Write_Molecule(j), &
                                                    Quiet=SET          )
            IF ( Error_Status /= SUCCESS ) THEN
              WRITE( Message,'("Error writing channel ",i0,", angle ",i0,", profile ",i0,&
                              &", and molid ",i0," to ", a)' ) &
                              Channels(l), i, Profiles(m), Write_Molecule(j), TRIM(Out_TauProfile_Filename)
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM(Message), &
                                    Error_Status )
              STOP
            END IF
        
          END DO Molecule_Write_Loop

        END DO Angle_Loop
      END DO Profile_Loop
      WRITE( *,'(5x,"Channel ", i0,"(",f7.2,") ",a,a)' ) &
               Channels(l), Frequency(l), &
               DERIVED_SET_NAME(SET_dTb(2,l)), &
               DERIVED_SUBSET_NAME(SET_dTb(1,l))
    END DO Channel_Loop


    ! Deallocate arrays for current sensor
    ! ------------------------------------
    DEALLOCATE( Frequency, dTb, &
                RMS_dTb, SET_dTb, &
                Channels, Angles, Profiles, &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating arrays for ",a,". STAT = ",i0)' ) &
                      TRIM(SensorInfo%Sensor_Id), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


    ! Add the total transmittance to the output file
    ! ----------------------------------------------
    ! Allocate monster array for read
    ALLOCATE( Tau_ALL( N_LAYERS,n_Channels,n_Angles,n_Profiles), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating Tau_ALL data array. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    ! Read everything
    Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                           ALL_IDX  , &
                                           Tau_ALL  , &
                                           Quiet=SET  )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading total transmittance from ", a)' ) TRIM(TauProfile_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF
    ! Write everything
    Error_Status = Write_TauProfile_netCDF( Out_TauProfile_Filename, &
                                            Tau_ALL  , &
                                            ALL_IDX  , &
                                            Quiet=SET  )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error writing total transmittance to ", a)' ) TRIM(Out_TauProfile_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF
    ! Deallocate monster array
    DEALLOCATE( Tau_ALL,STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating Tau_ALL data array. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
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


  ! Create a signal file
  ! --------------------
  Error_Status = Create_Signal_File( TRIM(DIRECTION_NAME(iDir))//'.'//PROGRAM_NAME )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM(DIRECTION_NAME(iDir))//' signal file', &
                          Error_Status )
    STOP
  END IF


  ! Destroy the SensorInfo linked list
  ! ----------------------------------
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF
  
  
CONTAINS


  ! Subroutine to compute the effective transmittances
  ! --------------------------------------------------
  SUBROUTINE Compute_EffTau( Numerator, Denominator )
    ! Arguments
    REAL(fp), INTENT(IN OUT) :: Numerator(:)
    REAL(fp), INTENT(IN)     :: Denominator(:)
    ! Local parameters
    REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ONE)
    ! Only compute effective transmittance if the denominator
    ! is greater than numerical precision.
    WHERE( ABS(Denominator) > TOLERANCE )
      Numerator = Numerator/Denominator
    ELSEWHERE
      Numerator = ZERO
    END WHERE
    ! Correct the effective transmittances
    CALL Correct_EffTau( Numerator )
  END SUBROUTINE Compute_EffTau


  ! Subroutine to "correct" the effective transmittances
  ! For any channel, the corrections applied to the
  ! values at layer k are:
  !   i)   If tau(k) < 0,        then tau(k) = 0
  !   ii)  If tau(k) > 1,        then tau(k) = 1
  !   iii) If tau(k) > tau(k-1), then tau(k) = tau(k-1)
  ! ----------------------------------------------------
  SUBROUTINE Correct_EffTau( EffTau )
    ! Arguments
    REAL(fp), INTENT(IN OUT) :: EffTau(:)
    ! Local variables
    INTEGER :: k
    ! Correct for transmittances < 0 and > 1
    WHERE( EffTau < ZERO ) EffTau = ZERO
    WHERE( EffTau > ONE  ) EffTau = ONE
    ! Correct for negative optical depths
    DO k = 2, SIZE(EffTau,DIM=1)
      IF ( EffTau(k) > EffTau(k-1) ) EffTau(k) = EffTau(k-1)
    END DO
  END SUBROUTINE Correct_EffTau

END PROGRAM Effective_TauProfile
