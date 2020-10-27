!
! Assemble_FTS_TauProfile
!
! Program to assemble the individual TauProfile datafiles into a single
! datafile for an FTS sensor.
!
!
! FILES ACCESSED:
!       Input:  - Sensor TauProfile netCDF data files for each profile and
!                 each molecule set.
!
!       Output: - TauProfile netCDF data file combining all the profile
!                 and molecule set data for a single sensor.
!
! SIDE EFFECTS:
!       Any output files that exist are overwritten.
!
! RESTRICTIONS:
!       *ALL* of the required data must be present for the output files to
!       be successfully written.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Assemble_FTS_TauProfile

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, &
                                       Display_Message, Program_Message
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE Tau_Production_Parameters
  USE Tau_Production_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Assemble_FTS_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: PATH = 'TauProfile_data/'
  INTEGER,      PARAMETER :: SET = 1
  

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: ID_Tag
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: InFile, OutFile
  CHARACTER(20)   :: Generic_Sensor_ID, Sensor_ID
  CHARACTER(20)   :: Sensor_Name, Satellite_Name
  CHARACTER(20)   :: nTag, bTag
  CHARACTER(20)   :: jTag
  CHARACTER(20)   :: mTag
  CHARACTER(20)   :: iTag
  LOGICAL :: Create_Output
  INTEGER :: WMO_Satellite_ID
  INTEGER :: WMO_Sensor_ID   
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: Error_Status
  INTEGER :: n, n1, n2
  INTEGER :: n_m, m, im, m1, m2, iProfile_Set
  INTEGER :: n_j, j, jIdx, Idx(1), Molecule_Set_Numbers(N_MOLECULE_SETS)
  INTEGER :: i, i1, i2
  INTEGER :: iDir
  INTEGER :: n_Channels
  INTEGER, ALLOCATABLE :: Channel_List(:)
  TYPE(TauProfile_type) :: TauProfile


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to assemble the individual TauProfile datafiles '//&
                        'for the bands of an FTS sensor into single datafiles.', &
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

  ! The molecule set index number(s)
  WRITE(*,FMT='(/5x,"Enter the number of MOLECULE SETS to assemble: ")',ADVANCE='NO' )
  READ(*,FMT='(i2)',IOSTAT=IO_Status ) n_j
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input.', &
                          FAILURE )
    STOP
  END IF
  IF ( n_j < 1 .OR. n_j > N_MOLECULE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Value is outside valid range.', &
                          FAILURE )
    STOP
  ENDIF

  Molecule_Set_Numbers = -1
  WRITE(*,FMT='(5x,"Enter the MOLECULE SET numbers to process: ")',ADVANCE='NO')
  READ(*,FMT=*,IOSTAT=IO_Status) Molecule_Set_Numbers(1:n_j)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET input.', &
                          FAILURE )
    STOP
  END IF
  DO j = 1, n_j
    IF ( .NOT. ANY(MOLECULE_SET_TAG_ID == Molecule_Set_Numbers(j)) ) THEN
      WRITE( Message,'("Input MOLECULE SET value ",i0," is invalid.")' ) &
                     Molecule_Set_Numbers(j)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
  END DO

  ! The calculation direction
  ! -------------------------
  WRITE(*, FMT='(/5x,"Select atmospheric path")')
  DO i = 1, N_DIRECTIONS
    WRITE(*,FMT='(10x,i1,") ",a)') i, TRIM(DIRECTION_NAME(i))
  END DO
  WRITE(*,FMT='(5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,FMT='(i1)',IOSTAT=IO_Status) iDir
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


  ! Ask for the instrument bands to process
  ! ---------------------------------------
  WRITE(*, FMT='(/5x,"Enter the being and end instrument bands [n1,n2]: ")', ADVANCE='NO')
  READ(*,FMT='(i5,i5)',IOSTAT=IO_Status) n1, n2
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid BAND input.', &
                          FAILURE )
    STOP
  END IF


  ! Define limits
  ! -------------
  ! Define the generic sensor id (hardwired for IASI for now)
  Sensor_Name = 'iasi'
  Satellite_Name = 'metop-a'
  Generic_Sensor_Id = TRIM(Sensor_Name)//'_'//TRIM(Satellite_Name)

  ! Define the profile limits
  m1 = 1; m2 = N_PROFILES(iProfile_Set)

  ! Define the angle limits
  i1 = ZENITH_ANGLE_BEGIN; i2 = ZENITH_ANGLE_END

  ! Begin band loop
  ! ---------------
  Band_Loop: DO n = n1, n2
    WRITE( nTag,'("band",i0)' ) n
    WRITE( bTag,'("B",i0)' ) n
    Sensor_Id = TRIM(Sensor_Name)//TRIM(bTag)//'_'//TRIM(Satellite_Name)
    
    ! Construct the current band output filename
    OutFile = TRIM(DIRECTION_NAME(iDir))//'.'//TRIM(Sensor_Id)//'.TauProfile.nc'

    ! Create an output file for every band
    Create_Output = .TRUE.
  
    ! Deallocate the channel list array if required
    IF ( ALLOCATED(Channel_List) ) THEN
      DEALLOCATE( Channel_List, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error deallocating channel list array for ",a,". STAT=",i0)' ) &
               TRIM(nTag), Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF
    END IF
    
  
    ! Begin molecule set loop
    ! -----------------------
    Molecule_Loop: DO jIdx = 1, n_j

      ! Define the molecule set number and name
      Idx = PACK((/(j,j=1,N_MOLECULE_SETS)/), Molecule_Set_Numbers(jIdx)==MOLECULE_SET_TAG_ID)
      j    = Idx(1)
      jTag = MOLECULE_SET_TAG(j)


      ! Begin profile loop
      ! ------------------
      Profile_Loop: DO m = m1, m2
         WRITE( mTag,'("profile",i2.2)' ) m


        ! Begin angle loop
        ! ----------------
        Angle_Loop: DO i = i1, i2
          WRITE( iTag,'("angle",i1)' ) i

          ! Construct the input TauProfile filename
          InFile = TRIM(nTag)//'/'//&
                   TRIM(jTag)//'/'//&
                   TRIM(mTag)//'/'//&
                   TRIM(iTag)//'/'//&
                   TRIM(DIRECTION_NAME(iDir))//'.'//&
                   TRIM(Generic_Sensor_Id)//'.REAL.TauProfile.nc'
          
          
          ! Create the output file
          ! ----------------------
          Create_Output_File: IF ( Create_Output ) THEN
          
            ! Turn off output creation
            Create_Output = .FALSE.
            
            ! Inquire the current input file for
            ! its channel dimension and attributes
            Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                                      n_Channels       = n_Channels      , &
                                                      WMO_Satellite_ID = WMO_Satellite_ID, &
                                                      WMO_Sensor_ID    = WMO_Sensor_ID   , &
                                                      ID_Tag           = ID_Tag          , &
                                                      History          = History         , &
                                                      Comment          = Comment           )
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error inquiring netCDF TauProfile file '//&
                                    TRIM(InFile), &
                                    Error_Status )
              STOP
            END IF
            
            ! Inquire the file for its channel list
            ALLOCATE( Channel_List(n_Channels), STAT=Allocate_Status )
            IF ( Allocate_Status /= 0 ) THEN
              WRITE( Message,'("Error allocating channel list array for ",a,". STAT=",i0)' ) &
                     TRIM(InFile), Allocate_Status
              CALL Display_Message( PROGRAM_NAME, &
                                    TRIM(Message), &
                                    FAILURE )
              STOP
            END IF
            Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                                      Channel_List = Channel_List )
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error obtaining channel list from '//TRIM(InFile), &
                                    Error_Status )
              STOP
            END IF

            ! Create the output file (CLOBBER mode)
            Error_Status = Create_TauProfile_netCDF( PATH//TRIM(OutFile), &
                                                     LEVEL_PRESSURE, &
                                                     Channel_List, &
                                                     ZENITH_ANGLE_SECANT(i1:i2), &
                                                     (/(im,im=m1,m2)/), &
                                                     Molecule_Set_Numbers(1:n_j), &
                                                     Release = TauProfile%Release, &
                                                     Version = TauProfile%Version, &
                                                     Sensor_ID        = TRIM(Sensor_ID), &
                                                     WMO_Satellite_ID = WMO_Satellite_ID, &
                                                     WMO_Sensor_ID    = WMO_Sensor_ID, &
                                                     ID_Tag  = TRIM(ID_Tag), &
                                                     Title   = TRIM(DIRECTION_NAME(iDir))//' '//&
                                                               TRIM(nTag)//' transmittances for '//&
                                                               TRIM(Generic_Sensor_Id), &
                                                     History = PROGRAM_RCS_ID//'; '//TRIM(History), &
                                                     Comment = TRIM(Comment) )
            IF ( Error_Status /= SUCCESS ) THEN
              CALL Display_Message( PROGRAM_NAME, &
                                    'Error creating netCDF TauProfile file '//&
                                    TRIM(OutFile), &
                                    Error_Status )
              STOP
            END IF

            WRITE( *,'(10x,a," created.")' ) TRIM(OutFile)
          
          END IF Create_Output_File
          

          ! Read the current input file data
          ! --------------------------------
          Error_Status = Read_TauProfile_netCDF( InFile, TauProfile, Quiet=SET )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF TauProfile file '//&
                                  TRIM(InFile), &
                                  Error_Status )
            STOP
          END IF

          ! Check that it's the correct molecule set, profile and angle
          IF ( TauProfile%Molecule_Set(1) /= j .OR. &
               TauProfile%Profile(1)      /= m .OR. &
               TauProfile%Angle(1)        /= ZENITH_ANGLE_SECANT(i) ) THEN
            Error_Status = FAILURE
            WRITE( Message,'("Molecule_Set, Profile, or Angle value for ",a,&
                            &" (",i2,",",i3,",",f4.2,") are different than expected",&
                            &" (",i2,",",i3,",",f4.2,").")' ) &
                            TRIM(InFile), &
                            TauProfile%Molecule_Set(1), &
                            TauProfile%Profile(1), &
                            TauProfile%Angle(1), &
                            j, m, ZENITH_ANGLE_SECANT(i)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF

          ! Check that the channel lists are the same
          IF ( ANY( (TauProfile%Channel-Channel_List) /= 0 ) ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Channel list values differ for '//TRIM(InFile), &
                                  FAILURE )
            STOP
          END IF
          

          ! Write the data into the output file
          ! -----------------------------------
          Error_Status = Write_TauProfile_netCDF( PATH//TRIM(OutFile), &
                                                  TauProfile%Tau(:,:,1,1,1), &
                                                  Angle       =ZENITH_ANGLE_SECANT(i), &
                                                  Profile     =m, &
                                                  Molecule_Set=j, &
                                                  Quiet       =SET )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error writing data from '//&
                                  TRIM(InFile)//' to '//TRIM(OutFile), &
                                  Error_Status )
            STOP
          END IF

          
          ! Destroy the TauProfile structure
          ! --------------------------------
          Error_Status = Destroy_TauProfile( TauProfile )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error destroying TauProfile structure read from '//TRIM(InFile), &
                                  Error_Status )
            STOP
          END IF
 
        END DO Angle_Loop
        
      END DO Profile_Loop
      
    END DO Molecule_Loop


    ! Create a signal file
    ! --------------------
    Error_Status = Create_Signal_File( TRIM(OutFile) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error creating signal file for '//TRIM(OutFile), &
                            Error_Status )
      STOP
    END IF
    
  END DO Band_Loop

END PROGRAM Assemble_FTS_TauProfile
