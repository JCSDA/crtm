!
! Extract_AIRS_SpcCoeff_Subset
!
! Program to extract AIRS channel subsets from the individual
! AIRS module netCDF format SpcCoeff data files.
!
!
! FILES ACCESSED:
!       Input: - netCDF format individual AIRS module SpcCoeff datafiles.
!              - For user specified channel subsetting, a list file containing
!                the required AIRS channels to subset.
!
!       Output: netCDF format AIRS channel SUBSET SpcCoeff datafile.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Extract_AIRS_SpcCoeff_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds        , ONLY: fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Display_Message, Program_Message
  USE List_File_Utility , ONLY: Integer_List_File_type, &
                                Read_List_File, Get_List_Size, Get_List_Entry
  USE SpcCoeff_Define   , ONLY: SpcCoeff_Sensor_type, &
                                Allocate_SpcCoeff, &
                                Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO, ONLY: Inquire_SpcCoeff_netCDF, &
                                Read_SpcCoeff_netCDF, &
                                Write_SpcCoeff_netCDF
  USE AIRS_Define       , ONLY: N_AIRS_CHANNELS, &
                                N_AIRS_MODULES, &
                                AIRS_MODULE
  USE AIRS_Subset       , ONLY: AIRS_Subset_type, &
                                N_AIRS_SUBSET_281, &
                                AIRS_SUBSET_281, &
                                AIRS_SUBSET_281_COMMENT, &
                                N_AIRS_SUBSET_324, &
                                AIRS_SUBSET_324, &
                                AIRS_SUBSET_324_COMMENT, &
                                Allocate_AIRS_Subset, &
                                Destroy_AIRS_Subset, &
                                Index_AIRS_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Extract_AIRS_SpcCoeff_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  INTEGER,      PARAMETER :: N_VALID_SETS = 4
  CHARACTER(*), PARAMETER :: VALID_SET_NAME(N_VALID_SETS) = (/ '281 channel set', &
                                                               '324 channel set', &
                                                               'All channels   ', &
                                                               'User specified ' /)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: List_Filename
  CHARACTER(256) :: In_Filename
  CHARACTER(256) :: Out_Filename
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: Sensor_Name
  CHARACTER(256)  :: Platform_Name
  CHARACTER(20)   :: Sensor_ID
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: Release
  INTEGER :: Version
  INTEGER :: i, Set
  INTEGER :: l, l1, l2
  LOGICAL :: First_Module
  INTEGER              :: n_Subset_Channels
  CHARACTER(256)       :: Subset_Comment
  INTEGER, ALLOCATABLE :: Subset_List(:)
  TYPE(Integer_List_File_type) :: User_Subset_List
  TYPE(SpcCoeff_Sensor_type)   :: In_SpcCoeff,  Out_SpcCoeff
  TYPE(AIRS_Subset_type)       :: Subset

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to extract the AIRS channel SUBSET spectral '//&
                        'coefficient data from the individual module netCDF '//&
                        'SpcCoeff files and write them to a separate netCDF '//&
                        'datafile.', &
                        '$Revision$' )

  ! Select a subset set
  ! -------------------
  Select_Loop: DO

    ! Prompt user to select a subset set 
    WRITE( *,'(/5x,"Select an AIRS channel subset")' )
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

    ! The 281 channel subset
    ! ----------------------
    CASE (1)
    
      ! Assign values
      n_Subset_Channels = N_AIRS_SUBSET_281
      Subset_Comment    = AIRS_SUBSET_281_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = AIRS_SUBSET_281
      Sensor_ID   = 'airs281SUBSET_aqua'


    ! The 324 channel subset
    ! ----------------------
    CASE (2)
    
      ! Assign values
      n_Subset_Channels = N_AIRS_SUBSET_324
      Subset_Comment    = AIRS_SUBSET_324_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = AIRS_SUBSET_324
      Sensor_ID   = 'airs324SUBSET_aqua'


    ! All the channels
    ! ----------------
    CASE(3)
    
      ! Assign values
      n_Subset_Channels = N_AIRS_CHANNELS
      Subset_Comment    = 'AIRS full channel set'

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = (/(l,l=1,N_AIRS_CHANNELS)/)
      Sensor_ID   = 'airs_aqua'


    ! A user specified channel subset
    ! -------------------------------
    CASE (4)
    
      ! Get a channel subset list filename
      WRITE( *, FMT='(/5x,"Enter an AIRS channel subset list filename : ")', &
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
      IF ( n_Subset_Channels < 1 .OR. n_Subset_Channels > N_AIRS_CHANNELS ) THEN
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
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! Fill the subset list
      DO l = 1, n_Subset_Channels
        Error_Status = Get_List_Entry( User_Subset_List, l, Subset_List(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error retrieving user subset channel list entry ",i4)' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF
      END DO

      ! Create the sensor id
      WRITE( Sensor_ID,'("airs",i0,"SUBSET_aqua")' ) n_Subset_Channels

  END SELECT



  ! Allocate the output SpcCoeff structure
  ! --------------------------------------
  Error_Status = Allocate_SpcCoeff( n_Subset_Channels, &
                                    Out_SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating output SpcCoeff data structure.', &
                          Error_Status )
    STOP
  END IF


  ! Initialise the start index for output
  ! and the "initial module" flag
  ! -------------------------------------
  l1 = 1
  First_Module = .TRUE.


  ! Loop over modules to extract subset channels
  ! --------------------------------------------
  Module_Loop: DO l = 1, N_AIRS_MODULES


    ! Current module channel subset
    ! -----------------------------
    ! Determine the subset channel indices
    ! for the current module
    Error_Status = Index_AIRS_Subset( l, Subset_List, Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for module '//&
                            TRIM(AIRS_MODULE(l)), &
                            Error_Status )
      STOP
    END IF

    ! Output the number of channels to extract
    WRITE( *,'(/10x,"There are ",i0," channels to be extracted from module ",a,":")' ) &
             Subset%n_Channels, TRIM(AIRS_MODULE(l))


    ! Read the input SpcCoeff file if required
    ! ----------------------------------------
    Non_Zero_n_Channels: IF ( Subset%n_Channels > 0 ) THEN

      ! Output the list of channel numbers to extract
      WRITE( *,'(10x,10i5)' ) Subset%Channel_Number

      ! Define the filename
      In_Filename = 'airs'//TRIM(AIRS_MODULE(l))//'_aqua.SpcCoeff.nc'


      ! Get the file release/version info and
      ! global attributes for the initial module
      ! ----------------------------------------
      IF ( First_Module ) THEN

        ! Update the test flag
        First_Module = .FALSE.

        ! Inquire the SpcCoeff data file
        Error_Status = Inquire_SpcCoeff_netCDF( TRIM(In_Filename), &
                                                Release       = Out_SpcCoeff%Release, &
                                                Version       = Out_SpcCoeff%Version, &
                                                History       = History, &
                                                Sensor_Name   = Sensor_Name, &
                                                Platform_Name = Platform_Name, &
                                                Comment       = Comment )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error inquiring the input netCDF SpcCoeff file '//&
                                TRIM( In_Filename ), &
                                Error_Status )
          STOP
        END IF

        ! Append onto the comment attribute.
        Comment = TRIM(Subset_Comment)//'; '//TRIM(Comment)

      END IF

      ! Get the Version info for all modules and compare
      ! ------------------------------------------------
      Error_Status = Inquire_SpcCoeff_netCDF( TRIM(In_Filename), &
                                              Version=Version )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error inquiring the input netCDF SpcCoeff file '//&
                              TRIM(In_Filename)//' for Release/Version info.', &
                              Error_Status )
        STOP
      END IF

      ! Check the Version value. If different - issue warning and continue, 
      ! but modify the Comment global attribute field for output
      IF ( Out_SpcCoeff%Version /= Version ) THEN
        WRITE( Message,'("Input file ",a," Version, ",i0, &
                        &", is different from previous file value, ",i0,".")' ) &
                        TRIM(In_Filename), Version, Out_SpcCoeff%Version
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              WARNING )
        Comment = TRIM(Message)//'; '//TRIM(Comment)
      END IF


      ! Read the data
      ! -------------
      Error_Status = Read_SpcCoeff_netCDF( In_Filename, &
                                           In_SpcCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF AIRS SpcCoeff file '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF
    

      ! Copy the required channel's data
      ! --------------------------------
      l2 = l1 + Subset%n_Channels - 1
      Out_SpcCoeff%Sensor_Descriptor(l1:l2)          = TRIM(Sensor_ID)
      Out_SpcCoeff%Sensor_Type(l1:l2)                = In_SpcCoeff%Sensor_Type(Subset%Channel_Index)
      Out_SpcCoeff%NCEP_Sensor_ID(l1:l2)             = In_SpcCoeff%NCEP_Sensor_ID(Subset%Channel_Index)
      Out_SpcCoeff%WMO_Satellite_ID(l1:l2)           = In_SpcCoeff%WMO_Satellite_ID(Subset%Channel_Index)
      Out_SpcCoeff%WMO_Sensor_ID(l1:l2)              = In_SpcCoeff%WMO_Sensor_ID(Subset%Channel_Index)
      Out_SpcCoeff%Sensor_Channel(l1:l2)             = In_SpcCoeff%Sensor_Channel(Subset%Channel_Index)
      Out_SpcCoeff%Frequency(l1:l2)                  = In_SpcCoeff%Frequency(Subset%Channel_Index)
      Out_SpcCoeff%Wavenumber(l1:l2)                 = In_SpcCoeff%Wavenumber(Subset%Channel_Index)
      Out_SpcCoeff%Planck_C1(l1:l2)                  = In_SpcCoeff%Planck_C1(Subset%Channel_Index)
      Out_SpcCoeff%Planck_C2(l1:l2)                  = In_SpcCoeff%Planck_C2(Subset%Channel_Index)
      Out_SpcCoeff%Band_C1(l1:l2)                    = In_SpcCoeff%Band_C1(Subset%Channel_Index)
      Out_SpcCoeff%Band_C2(l1:l2)                    = In_SpcCoeff%Band_C2(Subset%Channel_Index)
      Out_SpcCoeff%Is_Microwave_Channel(l1:l2)       = In_SpcCoeff%Is_Microwave_Channel(Subset%Channel_Index)
      Out_SpcCoeff%Polarization(l1:l2)               = In_SpcCoeff%Polarization(Subset%Channel_Index)
      Out_SpcCoeff%Cosmic_Background_Radiance(l1:l2) = In_SpcCoeff%Cosmic_Background_Radiance(Subset%Channel_Index)
      Out_SpcCoeff%Is_Solar_Channel(l1:l2)           = In_SpcCoeff%Is_Solar_Channel(Subset%Channel_Index)
      Out_SpcCoeff%Solar_Irradiance(l1:l2)           = In_SpcCoeff%Solar_Irradiance(Subset%Channel_Index)
      l1 = l2 + 1


      ! Destropy the input SpcCoeff structure
      ! for the next module read
      ! -------------------------------------
      Error_Status = Destroy_SpcCoeff( In_SpcCoeff )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff structure for input from '//&
                              TRIM( In_Filename ), &
                              Error_Status )
        STOP
      END IF

    END IF Non_Zero_n_Channels


    ! Destroy the AIRS_Subset structure
    ! ---------------------------------
    Error_Status = Destroy_AIRS_Subset( Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying AIRS Subset structure for input from '//&
                            TRIM( In_Filename ), &
                            Error_Status )
      STOP
    END IF

  END DO Module_Loop


  ! Write the output SpcCoeff file
  ! ------------------------------
  ! Set the output filename
  Out_Filename = TRIM(Sensor_ID)//'.SpcCoeff.nc'

  ! Set the number of sensors
  Out_SpcCoeff%n_Sensors = 1

  ! Write the data
  WRITE( *,'(/10x,"Creating the output file...")' )
  Error_Status = Write_SpcCoeff_netCDF( TRIM(Out_Filename), &
                                        Out_SpcCoeff, &
                                        Title         = 'Spectral coefficients for '//&
                                                        TRIM(Sensor_ID), &
                                        History       = PROGRAM_RCS_ID//'; '//&
                                                        TRIM(History), &
                                        Sensor_Name   = TRIM(Sensor_Name), &
                                        Platform_Name = TRIM(Platform_Name), &
                                        Comment       = 'Data extracted from the individual '//&
                                                        'AIRS module SpcCoeff datafiles.; '//&
                                                        TRIM(Comment) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the AIRS SpcCoeff file '//&
                          TRIM(Out_Filename), &
                          FAILURE )
    STOP
  END IF


  ! Destroy the output SpcCoeff structure
  ! -------------------------------------
  Error_Status = Destroy_SpcCoeff( Out_SpcCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff structure for output to '//&
                          TRIM(Out_Filename), &
                          Error_Status )
  END IF

END PROGRAM Extract_AIRS_SpcCoeff_Subset
