!
! Select_TauProfile
!
! Program to select the combination of LBL-generated transmittance components
! that minimises the occurrance of effective transmittance profiles that
! contain negative optical depths.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Dec-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Select_TauProfile

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
  USE AtmProfile_Define        , ONLY: AtmProfile_type   , &
                                       Destroy_AtmProfile
  USE AtmProfile_netCDF_IO     , ONLY: Read_AtmProfile_netCDF
  USE SpcCoeff_Define          , ONLY: SpcCoeff_type   , &
                                       Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO       , ONLY: Read_SpcCoeff_netCDF
  USE Sensor_Planck_Functions  , ONLY: Sensor_Radiance, &
                                       Sensor_Temperature
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Select_TauProfile'
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
  
  ! Definition of derived tau "sets" (the j1 value)
  INTEGER     , PARAMETER :: N_DERIVED = 3
  INTEGER     , PARAMETER :: WVO_DERIVED = 1
  INTEGER     , PARAMETER :: DOZ_DERIVED = 2
  INTEGER     , PARAMETER :: WVD_DERIVED = 3
  CHARACTER(*), PARAMETER :: DERIVED_NAME(N_DERIVED) = (/'WVO', 'DOZ', 'WVD'/)
  
  ! Definition of derived tau "subsets" (the j2 value)
  INTEGER     , PARAMETER :: N_DERIVED_SETS = 2
  CHARACTER(*), PARAMETER :: DERIVED_SET_NAME(N_DERIVED_SETS) = (/'_1', '_2'/)
  
  ! Number of which transmittances to read
  INTEGER, PARAMETER :: N_READ_IDX = 4
  INTEGER, PARAMETER :: WVO_DERIVED_IDX(N_READ_IDX) = (/ALL_IDX, WVO_IDX, WET_IDX, OZO_IDX/)
  INTEGER, PARAMETER :: DOZ_DERIVED_IDX(N_READ_IDX) = (/ALL_IDX, DOZ_IDX, DRY_IDX, OZO_IDX/)
  INTEGER, PARAMETER :: WVD_DERIVED_IDX(N_READ_IDX) = (/ALL_IDX, WVD_IDX, WET_IDX, DRY_IDX/)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: ID_Tag
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: AtmProfile_Filename
  CHARACTER(256)  :: SensorInfo_Filename
  CHARACTER(256)  :: TauProfile_Filename, Out_TauProfile_Filename(N_DERIVED_SETS,N_DERIVED)
  CHARACTER(256)  :: Signal_Filename
  CHARACTER(256)  :: SpcCoeff_Filename
  CHARACTER(256)  :: dTb_Filename
  INTEGER :: dTb_FileID
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: iDir, iProfile_Set
  INTEGER :: Release, Version
  INTEGER :: l, n_l ! n_Channels
  INTEGER :: i, n_i ! n_Angles
  INTEGER :: m, n_m ! n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: j1, j2
  INTEGER :: n, n_Sensors
  INTEGER :: Tau_Idx(N_READ_IDX)
  INTEGER :: AVG_Tau_Combo(2), RMS_Tau_Combo(2)
  INTEGER , ALLOCATABLE :: Channels(:)
  REAL(fp), ALLOCATABLE :: Angles(:)
  INTEGER , ALLOCATABLE :: Profiles(:)
  REAL(fp), TARGET  :: Tau_ALL(N_LAYERS)
  REAL(fp), TARGET  :: Tau_WVO(N_LAYERS), Tau_DOZ(N_LAYERS), Tau_WVD(N_LAYERS)
  REAL(fp), TARGET  :: Tau_WET(N_LAYERS), Tau_DRY(N_LAYERS), Tau_OZO(N_LAYERS)
  REAL(fp), POINTER :: Tau(:) => NULL()
  REAL(fp) :: Tau_EffWET(N_LAYERS), Tau_EffDRY(N_LAYERS), Tau_EffOZO(N_LAYERS)
  REAL(fp) :: Tau_EffALL(N_LAYERS)
  REAL(fp) :: Tb_All, Tb_EffALL, dTb
  REAL(fp), ALLOCATABLE :: delta_Tb(:,:,:,:)
  TYPE(AtmProfile_type)      :: Atm
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(SpcCoeff_type) :: SpcCoeff

  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to select the combination of LBL-generated transmittance '//&
                        'components that minimises the occurrance of effective transmittance'//&
                        'profiles that contain negative optical depths.', &
                        '$Revision$' )


  ! Get user input
  ! --------------
  ! The profile set being processed
  WRITE(*, FMT='(/5x,"Select the DEPENDENT PROFILE SET")')
  DO i = 1, N_PROFILE_SETS
    WRITE(*,FMT='(10x,i2,") ",a," profile set")') i, TRIM(PROFILE_SET_ID_TAG(i))
  END DO
  WRITE(*,FMT='(5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,FMT='(i1)',IOSTAT=IO_Status ) iProfile_Set
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier input.', &
                          FAILURE )
    STOP
  END IF
  IF ( iProfile_Set < 1 .OR. iProfile_Set > N_PROFILE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF

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


  ! Read the required AtmProfile data file
  ! --------------------------------------
  AtmProfile_Filename = TRIM(PROFILE_SET_ID_TAG(iProfile_Set))//'.AtmProfile.nc'
  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename, Atm )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF AtmProfile file '//&
                          TRIM(AtmProfile_Filename)//'.', &
                          Error_Status )
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


    ! Only operate on sensors for which data is available
    ! ---------------------------------------------------
    SpcCoeff_Filename = TRIM(SensorInfo%Sensor_ID)//'.SpcCoeff.nc'
    TauProfile_Filename = TRIM(DIRECTION_NAME(iDir))//'.'//&
                          TRIM(SensorInfo%Sensor_ID)//'.TauProfile.nc'
    
    ! Cycle loop if no data
    IF ( (.NOT. File_Exists( TauProfile_Filename )) .OR. &
         (.NOT. File_Exists( SpcCoeff_Filename   ))      ) CYCLE Sensor_Loop

    WRITE( *,'(/10x,"Computing effective transmittances for ",a,"...")' ) &
             TRIM(SensorInfo%Sensor_ID)
    
    
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


    ! Create the output files
    ! -----------------------
!    DO j1 = 1, N_DERIVED
!      DO j2 = 1, N_DERIVED_SETS
!        Out_TauProfile_Filename(j2,j1) = TRIM(DIRECTION_NAME(iDir))//'.'//&
!                                         TRIM(SensorInfo%Sensor_ID)//'.'//&
!                                         DERIVED_NAME(j1)//DERIVED_SET_NAME(j2)//&
!                                         '.TauProfile.nc'
!        Error_Status = Create_TauProfile_netCDF( Out_TauProfile_Filename(j2,j1), &
!                                                 LEVEL_PRESSURE, &
!                                                 Channels, &
!                                                 Angles, &
!                                                 Profiles, &
!                                                 (/EFF_WET_IDX,EFF_DRY_IDX,EFF_OZO_IDX/), &
!                                                 Release         =Release, &
!                                                 Version         =Version, &
!                                                 Sensor_ID       =TRIM(SensorInfo%Sensor_ID), &
!                                                 WMO_Satellite_ID=SensorInfo%WMO_Satellite_ID, &
!                                                 WMO_Sensor_ID   =SensorInfo%WMO_Sensor_ID, &
!                                                 ID_Tag          =TRIM(ID_Tag), &
!                                                 Title           =TRIM(DIRECTION_NAME(iDir))//' '//&
!                                                                  DERIVED_NAME(j1)//&
!                                                                  DERIVED_SET_NAME(j2)//&
!                                                                  ' effective transmittances for '//&
!                                                                  TRIM(SensorInfo%Sensor_ID), &
!                                                 History         =PROGRAM_RCS_ID//'; '//TRIM(History), &
!                                                 Comment         =TRIM(Comment) )
!        IF ( Error_Status /= SUCCESS ) THEN
!          CALL Display_Message( PROGRAM_NAME, &
!                                'Error creating netCDF TauProfile file '//&
!                                TRIM(Out_TauProfile_Filename(j2,j1)), &
!                                Error_Status )
!          STOP
!        END IF
!      END DO
!    END DO
    
    
    ! Allocate the statistics arrays
    ! ------------------------------
    ALLOCATE( delta_Tb( n_i, n_m, N_DERIVED_SETS, N_DERIVED), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating dTb array. STAT = ",i0)' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    
    ! Open the file to output stats info
    ! ----------------------------------
    dTb_Filename = TRIM(SensorInfo%Sensor_ID)//'.dTb_Stats.bin'
    Error_Status = Open_Binary_File( dTb_Filename, &
                                     dTb_FileID  , &
                                     For_Output=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error opening Stats file '//TRIM(dTb_Filename), &
                            Error_Status )
      STOP
    END IF
    ! Write the data dimensions and frequency grid
    WRITE( dTb_FileID ) n_i, n_m, N_DERIVED_SETS, N_DERIVED, n_l
    WRITE( dTb_FileID ) SpcCoeff%Wavenumber


    ! Begin CHANNEL loop
    ! ------------------
    Channel_Loop: DO l = 1, n_l

      ! Initialise the statistics arrays
      delta_Tb  = ZERO


      ! Begin DERIVED TAU loop
      ! ----------------------
      Derived_Loop: DO j1 = 1, N_DERIVED  ! WVO, DOZ, or WVD


        ! Set the molecule indices for the current derived master
        ! -------------------------------------------------------
        SELECT CASE (j1)
          CASE (WVO_DERIVED); Tau_Idx = WVO_DERIVED_IDX
          CASE (DOZ_DERIVED); Tau_Idx = DOZ_DERIVED_IDX
          CASE (WVD_DERIVED); Tau_Idx = WVD_DERIVED_IDX
        END SELECT


        ! Begin the DERIVED SUBSET loop
        ! -----------------------------
        DerivedSubset_Loop: DO j2 = 1, N_DERIVED_SETS  ! 2

    
          ! Begin the PROFILE and ANGLE loops
          ! ---------------------------------
          Profile_Loop: DO m = 1, n_m
            Angle_Loop:   DO i = 1, n_i

        
              ! Read the transmittance data
              ! ---------------------------
              Molecule_Loop: DO j = 1, N_READ_IDX
        
                ! Set the particular transmittance to read
                SELECT CASE (Tau_Idx(j))
                  CASE (ALL_IDX); Tau => Tau_ALL
                  CASE (WVO_IDX); Tau => Tau_WVO
                  CASE (DOZ_IDX); Tau => Tau_DOZ
                  CASE (WVD_IDX); Tau => Tau_WVD
                  CASE (WET_IDX); Tau => Tau_WET
                  CASE (DRY_IDX); Tau => Tau_DRY
                  CASE (OZO_IDX); Tau => Tau_OZO
                END SELECT
            
                ! Read the data from file
                Error_Status = Read_TauProfile_netCDF( TauProfile_Filename, &
                                                       Channels(l), &
                                                       Angles(i)  , &
                                                       Profiles(m), &
                                                       Tau_Idx(j) , &
                                                       Tau        , &
                                                       Quiet=SET    )
                IF ( Error_Status /= SUCCESS ) THEN
                  WRITE( Message,'("Error reading channel ",i0,", angle ",i0,", profile ",i0,&
                                  &", and molid ",i0," from ", a)' ) &
                                  Channels(l), i, Profiles(m), Tau_Idx(j), TRIM(TauProfile_Filename)
                  CALL Display_Message( PROGRAM_NAME, &
                                        TRIM(Message), &
                                        Error_Status )
                  STOP
                END IF
            
                ! Nullify the pointer
                NULLIFY(Tau)
            
              END DO Molecule_Loop


              ! Perform RT using the LBL Tau_ALL
              ! --------------------------------
              Tb_ALL = Simple_RT( SpcCoeff, Channels(l), Atm%Layer_Temperature(:,m), Tau_ALL )


              ! Perform RT using the effective total transmittances
              ! ---------------------------------------------------
              ! Initialise the effective transmittance arrays
              Tau_EffWET = ZERO
              Tau_EffDRY = ZERO
              Tau_EffOZO = ZERO
              ! Compute the transmittance components
              SELECT CASE (j1)
                ! WVO-derived
                CASE (WVO_DERIVED)
                  Tau_EffDRY = Compute_EffTau( Tau_ALL, Tau_WVO )
                  IF ( j2 == 1 ) THEN
                    Tau_EffWET = Tau_WET; CALL Correct_EffTau(Tau_EffWET)
                    Tau_EffOZO = Compute_EffTau( Tau_WVO, Tau_WET )
                  ELSE
                    Tau_EffWET = Compute_EffTau( Tau_WVO, Tau_OZO )
                    Tau_EffOZO = Tau_OZO; CALL Correct_EffTau(Tau_EffOZO)
                  END IF
                ! DOZ-derived
                CASE (DOZ_DERIVED)
                  Tau_EffWET = Compute_EffTau( Tau_ALL, Tau_DOZ )
                  IF ( j2 == 1 ) THEN
                    Tau_EffDRY = Tau_DRY; CALL Correct_EffTau(Tau_EffDRY)
                    Tau_EffOZO = Compute_EffTau( Tau_DOZ, Tau_DRY )
                  ELSE
                    Tau_EffDRY = Compute_EffTau( Tau_DOZ, Tau_OZO )
                    Tau_EffOZO = Tau_OZO; CALL Correct_EffTau(Tau_EffOZO)
                  END IF
                ! WVD-derived
                CASE (WVD_DERIVED)
                  Tau_EffOZO = Compute_EffTau( Tau_ALL, Tau_WVD )
                  IF ( j2 == 1 ) THEN
                    Tau_EffWET = Tau_WET; CALL Correct_EffTau(Tau_EffWET)
                    Tau_EffDRY = Compute_EffTau( Tau_WVD, Tau_WET )
                  ELSE
                    Tau_EffWET = Compute_EffTau( Tau_WVD, Tau_DRY )
                    Tau_EffDRY = Tau_DRY; CALL Correct_EffTau(Tau_EffDRY)
                  END IF
              END SELECT

              ! Compute the effective total transmittance
              Tau_EffALL = Tau_EffWET * Tau_EffDRY * Tau_EffOZO
              
              ! Compute the Tb
              Tb_EffALL = Simple_RT( SpcCoeff, Channels(l), Atm%Layer_Temperature(:,m), Tau_EffALL )


              ! Accumulate the temperature differences
              ! --------------------------------------
              delta_Tb(i,m,j2,j1) = Tb_ALL - Tb_EffALL

            END DO Angle_Loop
          END DO Profile_Loop
        END DO DerivedSubset_Loop
      END DO Derived_Loop

      ! Write the dTb's to file
      WRITE( dTb_FileID ) delta_Tb
      WRITE( *,'(5x,"Channel ", i0,"(",f7.2,")")' ) Channels(l), SpcCoeff%Wavenumber(l)
             
    END DO Channel_Loop

    CLOSE( dTb_FileID )


    ! Deallocate arrays for current sensor
    ! ------------------------------------
    DEALLOCATE( Channels, Angles, Profiles, &
                delta_Tb, &
                STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating arrays for ",a,". STAT = ",i0)' ) &
                      TRIM(SensorInfo%Sensor_Id), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF


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


  ! Destroy the AtmProfile structure
  ! --------------------------------
  Error_Status = Destroy_AtmProfile( Atm )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile data structure.', &
                          FAILURE )
    STOP
  END IF
 
    
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


  ! Function to compute the effective transmittances
  ! ------------------------------------------------
  FUNCTION Compute_EffTau( Numerator, Denominator ) RESULT( EffTau )
    ! Arguments
    REAL(fp), INTENT(IN)  :: Numerator(:), Denominator(:)
    ! Function result
    REAL(fp) :: EffTau(SIZE(Numerator))
    ! Local parameters
    REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ONE)
    ! Only compute effective transmittance if the denominator
    ! is greater than numerical precision.
    WHERE( ABS(Denominator) > TOLERANCE )
      EffTau = Numerator/Denominator
    ELSEWHERE
      EffTau = ZERO
    END WHERE
    ! Correct the effective transmittances
    CALL Correct_EffTau( EffTau )
  END FUNCTION Compute_EffTau


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


  ! Subroutine to perform simple radiative transfer
  ! -----------------------------------------------
  FUNCTION Simple_RT( SpcCoeff, Sensor_Channel, Temperature, Tau ) RESULT( Tb )
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN) :: SpcCoeff
    INTEGER            , INTENT(IN) :: Sensor_Channel
    REAL(fp)           , INTENT(IN) :: Temperature(:)
    REAL(fp)           , INTENT(IN) :: Tau(:)
    ! Function result
    REAL(fp) :: Tb
    ! Local variables
    INTEGER :: Error_Status
    INTEGER :: k, n_Layers
    REAL(fp) :: B, Rad
    
    n_Layers = SIZE(Temperature)
    ! First layer
    Error_Status = Sensor_Radiance( SpcCoeff, Sensor_Channel, Temperature(1), B )
    Rad = B*Tau(1)
    ! Rest of profile
    DO k = 2, n_Layers
      Error_Status = Sensor_Radiance( SpcCoeff, Sensor_Channel, Temperature(k), B )
      Rad = Rad + ( B * (Tau(k-1)-Tau(k)) )
    END DO
    ! Pretend surface. Use last layer temperature as Tsfc. Esfc = 1.0
    Rad = Rad + (B*Tau(n_Layers))
    ! Convert radiances to brightness temperature
    Error_Status = Sensor_Temperature( SpcCoeff, Sensor_Channel, Rad, Tb )
  END FUNCTION Simple_RT

END PROGRAM Select_TauProfile
