!
! Effective_TauProfile
!
! Program to compute the effective molecular transmittances profiles
! from the various available molecular combinations.
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
                                       Inquire_TauProfile_netCDF, &
                                       Read_TauProfile_netCDF   , &
                                       Write_TauPRofile_netCDF
  USE Tau_Production_Parameters, ONLY: N_LAYERS             , &
                                       N_DIRECTIONS         , &
                                       UPWELLING_DIRECTION  , &
                                       DOWNWELLING_DIRECTION, &
                                       DIRECTION_NAME
  USE Tau_Production_Utility   , ONLY: Create_Signal_File
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Effective_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
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
  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = WLO_IDX + 100  ! WET/WCO
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = DRY_IDX + 100  ! ALL/WVO
  INTEGER, PARAMETER :: EFFECTIVE_OZO_IDX = OZO_IDX + 100  ! WVO/WET


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(5000) :: History
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: TauProfile_Filename
  CHARACTER(256)  :: Signal_Filename
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: iDir
  INTEGER :: l, n_l ! n_Channels
  INTEGER :: i, n_i ! n_Angles
  INTEGER :: m, n_m ! n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: n, n_Sensors
  INTEGER , ALLOCATABLE :: Channels(:)
  REAL(fp), ALLOCATABLE :: Angles(:)
  INTEGER , ALLOCATABLE :: Profiles(:)
  REAL(fp) :: Tau_ALL(N_LAYERS)
  REAL(fp) :: Tau_WVO(N_LAYERS)
  REAL(fp) :: Tau_WET(N_LAYERS)
  REAL(fp) :: Tau_WCO(N_LAYERS)
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List

  ! Output header
  ! -------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to compute the effective molecular transmittances profiles'//&
                       'from the various available molecular combinations.', &
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
                          
    ! Cycle loop if no data
    IF ( .NOT. File_Exists( TauProfile_Filename ) ) CYCLE Sensor_Loop

    WRITE( *,'(/10x,"Computing effective transmittances for ",a,"...")' ) &
             TRIM(SensorInfo%Sensor_ID)
    
    ! Inquire the file to get dimensions
    ! and global attributes
    ! ----------------------------------
    Error_Status = Inquire_TauProfile_netCDF( TauProfile_Filename, &
                                              n_Channels     =n_l, &
                                              n_Angles       =n_i, &
                                              n_Profiles     =n_m, &
                                              n_Molecule_Sets=n_j, &
                                              History=History )
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


    ! Modify the TauProfile HISTORY global attribute
    ! ----------------------------------------------
    Error_Status = Modify_TauProfile_GAtts( TauProfile_Filename, &
                                            History=PROGRAM_RCS_ID//'; '//TRIM(History) )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error modifying HISTORY global attribute in '//&
                            TRIM(TauProfile_Filename), &
                            Error_Status )
      STOP
    END IF


    ! Begin dimension loops
    ! ---------------------
    Profile_Loop: DO m = 1, n_m
      Angle_Loop:   DO i = 1, n_i
        Channel_Loop: DO l = 1, n_l
        
          ! Read the transmittance data
          ! ---------------------------
          ! Total transmittance, Tau_ALL
          Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                 Channels(l), &
                                                 Angles(i)  , &
                                                 Profiles(m), &
                                                 ALL_IDX    , &
                                                 Tau_ALL      )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_ALL from ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Water vapor + ozone transmittance, Tau_WVO
          Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                 Channels(l), &
                                                 Angles(i)  , &
                                                 Profiles(m), &
                                                 WVO_IDX    , &
                                                 Tau_WVO      )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_WVO from ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Water vapor only transmittance, Tau_WET
          Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                 Channels(l), &
                                                 Angles(i)  , &
                                                 Profiles(m), &
                                                 WET_IDX    , &
                                                 Tau_WET      )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_WET from ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Water vapor continua only transmittance, Tau_WCO
          Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                 Channels(l), &
                                                 Angles(i)  , &
                                                 Profiles(m), &
                                                 WCO_IDX    , &
                                                 Tau_WCO      )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_WCO from ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF



          ! Compute the effective transmittances
          ! ------------------------------------
          ! Effective DRY transmittance
          CALL Compute_EffTau( Tau_ALL, Tau_WVO )

          ! Effective OZO transmittance
          CALL Compute_EffTau( Tau_WVO, Tau_WET )

          ! Effective WLO transmittance
          CALL Compute_EffTau( Tau_WET, Tau_WCO )



          ! Output the effective transmittance to file
          ! ------------------------------------------
          ! Effective DRY transmittance
          Error_Status = Write_TauProfile_netCDF( TauProfile_Filename, &
                                                  Tau_ALL, &
                                                  Channels(l), &
                                                  Angles(i)  , &
                                                  Profiles(m), &
                                                  EFFECTIVE_DRY_IDX )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error writing channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_EFFDRY to ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Effective OZO transmittance
          Error_Status = Write_TauProfile_netCDF( TauProfile_Filename, &
                                                  Tau_WVO, &
                                                  Channels(l), &
                                                  Angles(i)  , &
                                                  Profiles(m), &
                                                  EFFECTIVE_OZO_IDX )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error writing channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_EFFOZO to ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Effective WLO transmittance
          Error_Status = Write_TauProfile_netCDF( TauProfile_Filename, &
                                                  Tau_WET, &
                                                  Channels(l), &
                                                  Angles(i)  , &
                                                  Profiles(m), &
                                                  EFFECTIVE_WLO_IDX )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error writing channel ",i0,", angle ",i0,", and profile ",i0,&
                            &" Tau_EFFWLO to ", a)' ) &
                            Channels(l), i, Profiles(m), TRIM(TauProfile_Filename)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF



        END DO Channel_Loop
      END DO  Angle_Loop
      WRITE( *,'(15x,"Profile # ",i0," effective transmittances written...")' ) Profiles(m)
    END DO Profile_Loop


    ! Deallocate dimension list arrays for current sensor
    ! ---------------------------------------------------
    DEALLOCATE( Channels, Angles, Profiles, STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating dimension list arrays for ",a,". STAT = ",i0)' ) &
                      TRIM(SensorInfo%Sensor_Id), Allocate_Status
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
  ! The effective transmittance is returned in the first argument,
  ! the numerator transmittance
  ! Only compute effective transmittance if the denominator
  ! is greater than numerical precision.
  SUBROUTINE Compute_EffTau( TauNUM, TauDENOM )
    REAL(fp), INTENT(IN OUT) :: TauNUM(:)
    REAL(fp), INTENT(IN)     :: TauDENOM(:)
    REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ONE)
    WHERE( ABS(TauDENOM) > TOLERANCE )
      TauNUM = TauNUM/TauDENOM
    ELSEWHERE
      TauNUM = ZERO
    END WHERE
  END SUBROUTINE Compute_EffTau

END PROGRAM Effective_TauProfile
