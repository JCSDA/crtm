!
! Test_TauProfile
!
! Program to test the TauProfile structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Sep-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_TauProfile

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message, Program_Message
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  CHARACTER(*), PARAMETER :: NC_FILENAME = 'TauProfile.nc'
  ! Test dimensions
  INTEGER, PARAMETER :: N_LAYERS        = 10
  INTEGER, PARAMETER :: N_CHANNELS      =  2  
  INTEGER, PARAMETER :: N_ANGLES        =  3    
  INTEGER, PARAMETER :: N_PROFILES      =  4    
  INTEGER, PARAMETER :: N_MOLECULE_SETS =  5
  INTEGER, PARAMETER :: N_MAX = N_LAYERS*N_CHANNELS*N_ANGLES*N_PROFILES*N_MOLECULE_SETS
  ! Loop parameters for memory leak tests
  INTEGER, PARAMETER :: READ_MAX_N_LOOPS  = 50000
  INTEGER, PARAMETER :: READ_INFO_N_LOOPS = 5000
  INTEGER, PARAMETER :: ASSIGN_MAX_N_LOOPS  = 10*READ_MAX_N_LOOPS
  INTEGER, PARAMETER :: ASSIGN_INFO_N_LOOPS = 10*READ_INFO_N_LOOPS
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Output_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n

  ! TauProfile dimensions
  INTEGER :: k, n_k ! n_Layers       
  INTEGER :: l, n_l ! n_Channels     
  INTEGER :: i, n_i ! n_Angles       
  INTEGER :: m, n_m ! n_Profiles     
  INTEGER :: j, n_j ! n_Molecule_Sets

  ! TauProfile global attributes
  INTEGER         :: Release, Version
  CHARACTER(256)  :: Sensor_ID
  INTEGER         :: WMO_Satellite_ID
  INTEGER         :: WMO_Sensor_ID
  CHARACTER(256)  :: ID_Tag
  CHARACTER(256)  :: Title
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment

  ! TauProfile list data
  INTEGER,  ALLOCATABLE :: Channel_List(:)
  REAL(fp), ALLOCATABLE :: Angle_List(:)
  INTEGER,  ALLOCATABLE :: Profile_List(:)
  INTEGER,  ALLOCATABLE :: Molecule_Set_List(:)

  ! TauProfile data arrays
  REAL(fp), ALLOCATABLE :: Tau1(:)
  REAL(fp), ALLOCATABLE :: Tau2(:,:)
  REAL(fp), ALLOCATABLE :: Tau3(:,:,:)
  REAL(fp), ALLOCATABLE :: Tau4(:,:,:,:)
  REAL(fp), ALLOCATABLE :: Tau5(:,:,:,:,:)

  ! TauProfile data structure
  TYPE(TauProfile_type) :: TauProfile, TauProfile1


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the TauProfile structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the TauProfile structure
  ! ---------------------------------
  Error_Status = Allocate_TauProfile( N_LAYERS       , &
                                      N_CHANNELS     , &
                                      N_ANGLES       , &
                                      N_PROFILES     , &
                                      N_MOLECULE_SETS, &
                                      TauProfile       )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating TauProfile structure array.', &
                          FAILURE )
    STOP
  END IF
  
  ! Fill structure with pretend data
  ! --------------------------------
  TauProfile%Sensor_Id = 'sensor_platform'
  TauProfile%Level_Pressure = EXP( (/(REAL(k+1,fp),k=0,N_LAYERS)/)/SQRT(2.5_fp) )
  TauProfile%Channel        = (/(l,l=1,N_CHANNELS)/)
  TauProfile%Angle          = (/(REAL(i-1,fp),i=1,N_ANGLES)/)*0.25_fp + 1.0_fp
  TauProfile%Profile        = (/(m,m=1,N_PROFILES)/)
  TauProfile%Molecule_Set   = (/(j,j=1,N_MOLECULE_SETS)/)
  TauProfile%Tau = RESHAPE( (/(REAL(n,fp),n=1,N_MAX)/)/REAL(N_MAX,fp), &
                            (/N_LAYERS       , &
                              N_CHANNELS     , &
                              N_ANGLES       , &
                              N_PROFILES     , &
                              N_MOLECULE_SETS /) )
  

  ! Create a test TauProfile file
  ! -----------------------------
  WRITE(*,'( /5x, "Creating a test TauProfile file ..." )' )

  ! Create the file
  Error_Status = Create_TauProfile_netCDF( NC_FILENAME                                 , &  ! Input
                                           TauProfile%Level_Pressure                   , &  ! Input
                                           TauProfile%Channel                          , &  ! Input
                                           TauProfile%Angle                            , &  ! Input
                                           TauProfile%Profile                          , &  ! Input
                                           TauProfile%Molecule_Set                     , &  ! Input
                                           Release         =TauProfile%Release         , &  ! Optional Input
                                           Version         =TauProfile%Version         , &  ! Optional Input
                                           Sensor_Id       =TauProfile%Sensor_Id       , &  ! Optional Input
                                           WMO_Satellite_Id=TauProfile%WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =TauProfile%WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          ='PROFILE_SET'              , &  ! Optional input
                                           Title           ='This is a title'          , &  ! Optional input
                                           History         =PROGRAM_RCS_ID             , &  ! Optional input
                                           Comment         ='This is a comment'          )  ! Optional input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating test netCDF TauProfile file '//NC_FILENAME, &
                          FAILURE )
    STOP
  END IF

  ! Write transmittances to the file
  Error_Status = Write_TauProfile_netCDF( NC_FILENAME, &  ! Input
                                          TauProfile   )  ! Input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing transmittance data to test netCDF TauProfile file '//&
                          NC_FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! Get the file dimensions and global attributes
  ! ---------------------------------------------
  Error_Status = Inquire_TauProfile_netCDF( NC_FILENAME                      , &  ! Input
                                            n_Layers        =n_k             , &  ! Optional output
                                            n_Channels      =n_l             , &  ! Optional output
                                            n_Angles        =n_i             , &  ! Optional output
                                            n_Profiles      =n_m             , &  ! Optional output
                                            n_Molecule_Sets =n_j             , &  ! Optional output
                                            Release         =Release         , &  ! Optional output
                                            Version         =Version         , &  ! Optional output
                                            Sensor_ID       =Sensor_ID       , &  ! Optional output
                                            WMO_Satellite_ID=WMO_Satellite_ID, &  ! Optional output
                                            WMO_Sensor_ID   =WMO_Sensor_ID   , &  ! Optional output
                                            ID_Tag          =ID_Tag          , &  ! Optional output
                                            Title           =Title           , &  ! Optional output
                                            History         =History         , &  ! Optional output
                                            Comment         =Comment           )  ! Optional output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          NC_FILENAME//&
                          ' for dimensions and global attributes', &
                          Error_Status )
    STOP
  END IF

  ! Output some info
  WRITE( *, '( /5x, "Release.Version : ", i0,".",i0, &
             & /5x, "Dimensions of ", a, " TauProfile file:", &
             &/10x, "n_Layers        = ", i5, &
             &/10x, "n_Channels      = ", i5, &
             &/10x, "n_Angles        = ", i5, &
             &/10x, "n_Profiles      = ", i5, &
             &/10x, "n_Molecule_Sets = ", i5 )' ) &
            Release, Version, &
            NC_FILENAME, &
            n_Layers, &
            n_Channels, &
            n_Angles, &
            n_Profiles, &
            n_Molecule_Sets

  WRITE( *, '( /5x, "Global attributes of ", a, " TauProfile file:", &
             & /," Sensor_ID : ",&
             & /," -----------",/,1x,a,&
             &//," WMO_Satellite_ID : ",&
             & /," ------------------",/,1x,i0,&
             &//," WMO_Sensor_ID : ",&
             & /," ---------------",/,1x,i0,&
             & /," ID_Tag : ",&
             & /," --------",/,1x,a,&
             &//," Title : ",&
             & /," -------",/,1x,a,&
             &//," History : ",&
             & /," ---------",/,1x,a,&
             &//," Comment : ",&
             & /," ---------",/,1x,a)' ) &
            NC_FILENAME, &
            TRIM(Sensor_ID), &
            WMO_Satellite_ID, &
            WMO_Sensor_ID, &
            TRIM(ID_Tag), &
            TRIM(Title), &
            TRIM(History), &
            TRIM(Comment)


  ! Get the list data
  ! -----------------
  ! Allocate the list arrays
  ALLOCATE( Channel_List( n_Channels ), &
            Angle_List( n_Angles ), &
            Profile_List( n_Profiles ), &
            Molecule_Set_List( n_Molecule_Sets ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Error allocating list arrays array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read the list data
  Error_Status = Inquire_TauProfile_netCDF( NC_FILENAME                        , &
                                            Channel_List     =Channel_List     , &
                                            Angle_List       =Angle_List       , &
                                            Profile_List     =Profile_List     , &
                                            Molecule_Set_List=Molecule_Set_List  )
                                           
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          NC_FILENAME//&
                          ' for list data.', &
                          Error_Status )
    STOP
  END IF

  ! Output some info
  WRITE( *,'(//5x,"Channel list of ",a," TauProfile file: ",/,10(1x,i0))' ) &
         NC_FILENAME, Channel_List
  WRITE( *,'(5x,"Angle list of ",a," TauProfile file: ",/,7(1x, f6.3))' ) &
         NC_FILENAME, Angle_List
  WRITE( *,'(5x,"Profile list of ",a," TauProfile file: ",/,10(1x,i0))' ) &
         NC_FILENAME, Profile_List
  WRITE( *,'(5x,"Molecule set list of ",a," TauProfile file: ",/,10(1x,i0))' ) &
         NC_FILENAME, Molecule_Set_List

  WRITE( *, '( //5x, "Press <ENTER> to test the netCDF TauProfile Read interfaces..." )' )
  READ( *, * )



  ! -----------------------------------------------------------------------
  ! Read the test netCDF TauProfile file
  !
  ! Note that the Read_TauProfile_netCDF() function is overloaded so that
  ! various groupings of transmittance profiles can be read. See the header
  ! documentation for more information.                                    
  ! -----------------------------------------------------------------------

  ! Rank-1 array read
  ! -----------------
  WRITE( *,'(//5x,"Test reading the netCDF TauProfile file: Rank-1 interface ...")' )

  ! Allocate the array
  ALLOCATE( Tau1( n_Layers ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message, '("Error allocating rank-1 transmittance data array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read loop
  n = 0
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles
        DO l = 1, n_Channels
          Error_Status = Read_TauProfile_netCDF( NC_FILENAME         , &
                                                 Channel_List(l)     , &
                                                 Angle_List(i)       , &
                                                 Profile_List(m)     , &
                                                 Molecule_Set_List(j), &
                                                 Tau1                , &
                                                 Quiet=SET             )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error reading TauProfile file ",a,&
                            &" using rank-1 interface at channel index, ",i0,&
                            &", angle index ",i0,&
                            &", profile index ",i0,&
                            &", and molecule set index ",i0)' ) &
                            NC_FILENAME, l, i, m, j
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF
          n = n+1
        END DO
      END DO
    END DO
  END DO
  WRITE( *,'(5x,i0," reads completed.")' ) n


  ! Rank-2 array read
  ! -----------------
  WRITE( *,'(/5x,"Test reading the netCDF TauProfile file: Rank-2 interface ...")' )

  ! Allocate the array
  ALLOCATE( Tau2( n_Layers, n_Channels ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message,'("Error allocating rank-2 transmittance data array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read loop
  n = 0
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles
        Error_Status = Read_TauProfile_netCDF( NC_FILENAME         , &
                                               Angle_List(i)       , &
                                               Profile_List(m)     , &
                                               Molecule_Set_List(j), &
                                               Tau2                , &
                                               Quiet=SET             )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading TauProfile file ", a, &
                            &" using rank-2 interface at angle index ", i4, &
                            &", profile index ", i4, &
                            &", and molecule set index ", i4 )' ) &
                          NC_FILENAME, i, m, j
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF
        n = n+1
      END DO
    END DO
  END DO
  WRITE( *,'(5x,i0," reads completed.")' ) n


  ! Rank-3 array read
  ! -----------------
  WRITE( *, '(/5x,"Test reading the netCDF TauProfile file: Rank-3 interface ...")' )

  ! Allocate the array
  ALLOCATE( Tau3( n_Layers, n_Channels, n_Angles ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message,'("Error allocating rank-3 transmittance data array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read loop
  n = 0
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      Error_Status = Read_TauProfile_netCDF( NC_FILENAME         , &
                                             Profile_List(m)     , &
                                             Molecule_Set_List(j), &
                                             Tau3                , &
                                             Quiet=SET             )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading TauProfile file ", a, &
                          &" using rank-3 interface at profile index ", i4, &
                          &", and molecule set index ", i4 )' ) &
                        NC_FILENAME, m, j
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF
      n = n+1
    END DO
  END DO
  WRITE( *,'(5x,i0," reads completed.")' ) n


  ! Rank-4 array read
  ! -----------------
  WRITE( *,'(/5x,"Test reading the netCDF TauProfile file: Rank-4 interface ...")' )

  ! Allocate the array
  ALLOCATE( Tau4( n_Layers, n_Channels, n_Angles, n_Profiles ), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message,'("Error allocating rank-4 transmittance data array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read loop
  n = 0
  DO j = 1, n_Molecule_Sets
    Error_Status = Read_TauProfile_netCDF( NC_FILENAME         , &
                                           Molecule_Set_List(j), &
                                           Tau4                , &
                                           Quiet=SET             )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading TauProfile file ",a,&
                      &" using rank-4 interface at molecule set index ",i0)' ) &
                      NC_FILENAME, j
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF
    n = n+1
  END DO
  WRITE( *,'(5x,i0," reads completed.")' ) n


  ! Rank-5 array read
  ! -----------------
  WRITE( *,'(/5x,"Test reading the netCDF TauProfile file: Rank-5 interface ...")' )

  ! Allocate the array
  ALLOCATE( Tau5( n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets ), &
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    Error_Status = FAILURE
    WRITE( Message,'("Error allocating rank-5 transmittance data array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Read the data
  Error_Status = Read_TauProfile_netCDF( NC_FILENAME, &
                                         Tau5       , &
                                         Quiet=SET    )
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error reading TauProfile file ", a, &
                      &" using rank-5 interface" )' ) &
                    NC_FILENAME
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF
  WRITE( *,'(5x,"Read completed.")' )


  ! Structure read
  ! --------------
  WRITE( *,'(/5x,"Test reading the netCDF TauProfile file: Structure interface ...")' )
  Error_Status = Read_TauProfile_netCDF( NC_FILENAME, &
                                         TauProfile , &
                                         Quiet=SET    )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF TauProfile file '//&
                          NC_FILENAME, &
                          Error_Status )
    STOP
  END IF
  WRITE( *,'(5x,"Read completed.")' )

  ! Output some structure info
  WRITE( *,'(//5x,"TauProfile channels:",/,10(1x,i0))' ) TauProfile%Channel
  WRITE( *,'(5x,"TauProfile angle secants:",/,7(1x,f6.3))' ) TauProfile%Angle
  WRITE( *,'(5x,"TauProfile profiles:",/,10(1x,i0))' ) TauProfile%Profile
  WRITE( *,'(5x,"TauProfile molecule sets:",/,10(1x,i0))' ) TauProfile%Molecule_Set


  WRITE( *,'(//5x,"Press <ENTER> to test the netCDF TauProfile Write interfaces...")' )
  READ( *,* )



  ! ----------------------------------------------------------------------
  ! Write a test netCDF TauProfile file
  !
  ! Note that the Write_TauProfile_netCDF() function is overloaded so that
  ! various groupings of transmittance profiles can be written. See the
  ! header documentation for more information.                                    
  ! ----------------------------------------------------------------------

  ! Rank-1 array write
  ! ------------------
  WRITE( *,'(//5x,"Test writing a netCDF TauProfile file: Rank-1 interface ...")' )

  ! Create an output file
  Output_Filename = 'Output.Rank-1.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Rank-1 test output file; '//&
                                                            TRIM(Comment)     )  ! Optional Input
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating test netCDF TauProfile file '//&
                          TRIM(Output_Filename), &
                          FAILURE )
    STOP
  END IF

  ! Write loop
  n = 0
  DO j = 1, n_j
    DO m = 1, n_m
      DO i = 1, n_i
        DO l = 1, n_l
          Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename)    , &
                                                  TauProfile%Tau(:,l,i,m,j), &
                                                  Channel_List(l)          , &
                                                  Angle_List(i)            , &
                                                  Profile_List(m)          , &
                                                  Molecule_Set_List(j)     , &
                                                  Quiet=SET                  )
          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message,'("Error writing TauProfile file ",a,&
                            &" using rank-1 interface at channel index, ",i0,&
                            &", angle index ",i0,&
                            &", profile index ",i0,&
                            &", and molecule set index ",i0)' ) &
                            TRIM(Output_Filename), l, i, m, j
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  Error_Status )
            STOP
          END IF
          n = n+1
        END DO
      END DO
    END DO
  END DO
  WRITE( *,'(5x,i0," writes completed.")' ) n


  ! Rank-2 array write
  ! ------------------
  WRITE( *,'(/5x,"Test writing a netCDF TauProfile file: Rank-2 interface ...")' )

  ! Create an output file
  Output_Filename = 'Output.Rank-2.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Rank-2 test output file; '//&
                                                            TRIM(Comment)     )

  ! Write loop
  n = 0
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      DO i = 1, n_Angles
        Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename)    , &
                                                TauProfile%Tau(:,:,i,m,j), &
                                                Angle_List(i)            , &
                                                Profile_List(m)          , &
                                                Molecule_Set_List(j)     , &
                                                Quiet=SET                  )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error writing TauProfile file ",a,&
                          &" using rank-2 interface at angle index ",i0,&
                          &", profile index ",i0,&
                          &", and molecule set index ",i0)' ) &
                          TRIM(Output_Filename), i, m, j
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF
        n = n+1
      END DO
    END DO
  END DO
  WRITE( *,'(5x,i0," writes completed.")' ) n


  ! Rank-3 array write
  ! ------------------
  WRITE( *,'(/5x,"Test writing a netCDF TauProfile file: Rank-3 interface ...")' )

  ! Create an output file
  Output_Filename = 'Output.Rank-3.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Rank-3 test output file; '//&
                                                            TRIM(Comment)     )

  ! Write loop
  n = 0
  DO j = 1, n_Molecule_Sets
    DO m = 1, n_Profiles
      Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename)    , &
                                              TauProfile%Tau(:,:,:,m,j), &
                                              Profile_List(m)          , &
                                              Molecule_Set_List(j)     , &
                                              Quiet=SET                  )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message,'("Error writing TauProfile file ",a,&
                        &" using rank-3 interface at profile index ",i0,&
                        &", and molecule set index ",i0)' ) &
                        TRIM(Output_Filename), m, j
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              Error_Status )
        STOP
      END IF
      n = n+1
    END DO
  END DO
  WRITE( *,'(5x,i0," writes completed.")' ) n


  ! Rank-4 array write
  ! ------------------
  WRITE( *, '( /5x, "Test writing a netCDF TauProfile file: Rank-4 interface ..." )' )

  ! Create an output file
  Output_Filename = 'Output.Rank-4.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Rank-4 test output file; '//&
                                                            TRIM(Comment)     )

  ! Write loop
  n = 0
  DO j = 1, n_Molecule_Sets
    Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename)    , &
                                            TauProfile%Tau(:,:,:,:,j), &
                                            Molecule_Set_List(j)     , &
                                            Quiet=SET                  )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error writing TauProfile file ",a,&
                      &" using rank-4 interface at molecule set index ",i0)' ) &
                      TRIM(Output_Filename), j
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            Error_Status )
      STOP
    END IF
    n = n+1
  END DO
  WRITE( *,'(5x,i0," writes completed.")' ) n


  ! Rank-5 array write
  ! ------------------
  WRITE( *,'(/5x,"Test writing a netCDF TauProfile file: Rank-5 interface ...")' )

  ! Create an output file
  Output_Filename = 'Output.Rank-5.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Rank-5 test output file; '//&
                                                            TRIM(Comment)     )

  ! Write array
  Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename), &
                                          TauProfile%Tau       , &
                                          Quiet=SET              )
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message,'("Error writing TauProfile file ",a,&
                    &" using rank-5 interface")' ) &
                    TRIM(Output_Filename)
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF
  WRITE( *,'(5x,"Write completed.")' )


  ! Structure write
  ! ---------------
  WRITE( *,'(/5x,"Test writing a netCDF TauProfile file: Structure interface ...")' )

  ! Create an output file
  Output_Filename = 'Output.Structure.TauProfile.nc'
  Error_Status = Create_TauProfile_netCDF( TRIM(Output_Filename)            , &  ! Input
                                           TauProfile%Level_Pressure        , &  ! Input
                                           Channel_List                     , &  ! Input
                                           Angle_List                       , &  ! Input
                                           Profile_List                     , &  ! Input
                                           Molecule_Set_List                , &  ! Input
                                           Release         =Release         , &  ! Optional Input
                                           Version         =Version         , &  ! Optional Input
                                           Sensor_Id       =TRIM(Sensor_Id) , &  ! Optional Input
                                           WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional Input
                                           WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional Input
                                           ID_Tag          =TRIM(ID_Tag)    , &  ! Optional Input
                                           Title           =TRIM(Title)     , &  ! Optional Input
                                           History         =PROGRAM_RCS_ID//'; '//&
                                                            TRIM(History)   , &  ! Optional Input
                                           Comment         ='Structure test output file; '//&
                                                            TRIM(Comment)     )

  ! Write structure
  Error_Status = Write_TauProfile_netCDF( TRIM(Output_Filename), &
                                          TauProfile           , &
                                          Quiet=SET              )
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message,'("Error writing TauProfile file ",a,&
                    &" using structure interface")' ) &
                    TRIM(Output_Filename)
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF
  WRITE( *,'(5x,"Write completed.")' )


  ! -------------------------------------------------
  ! Deallocate all the arrays; list and transmittance
  ! -------------------------------------------------
  
  ! Deallocate list arrays
  ! ----------------------
  DEALLOCATE( Channel_List, Angle_List, Profile_List, Molecule_Set_List, &
              STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating list arrays array. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
  END IF


  ! Deallocate transmittance arrays
  ! -------------------------------
  DEALLOCATE( Tau1, Tau2, Tau3, Tau4, Tau5, &
              STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating transmittance data arrays. STAT = ",i0)' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
    STOP
  END IF


  WRITE( *,'(//5x,"Press <ENTER> to test for memory leaks...")' )
  READ( *,* )



  ! ---------------------------------------------------------------
  ! Test functions for memory leaks
  !
  ! User should run the unix "top" utility during program execution
  ! and monitor the program memory usage to determine leakage.
  ! ---------------------------------------------------------------

  ! Test the netCDF structure reader for memory leaks
  ! -------------------------------------------------
  WRITE( *,'(/5x,"Looping for netCDF structure read memory leak test ...")' )
  DO n = 1, READ_MAX_N_LOOPS
    Error_Status = Read_TauProfile_netCDF( NC_FILENAME, &
                                           TauProfile , &
                                           Quiet=SET    )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error in structure read.', &
                            Error_Status )
      STOP
    END IF
    
    ! Output loop info
    IF ( MOD(n,READ_INFO_N_LOOPS) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i6," of ",i6)' ) n, READ_MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Test the Assign function for memory leaks
  ! -----------------------------------------
  WRITE( *, '( /5x, "Looping for structure copy memory leak test ..." )' )
  DO n = 1, ASSIGN_MAX_N_LOOPS
    Error_Status = Assign_TauProfile( TauProfile, TauProfile1 )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error in structure assign.', &
                            Error_Status )
      STOP
    END IF
    
    ! Output loop info
    IF ( MOD(n,ASSIGN_INFO_N_LOOPS) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i6," of ",i6)' ) n, ASSIGN_MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! --------
  ! Clean up
  ! --------
  Error_Status = Destroy_TauProfile( TauProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauProfile structure.', &
                          WARNING )
  END IF
  Error_Status = Destroy_TauProfile( TauProfile1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauProfile1 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_TauProfile
