!
! Extract_IASI_SpcCoeff_Subset
!
! Program to extract IASI channel subsets from the individual
! IASI band binary (i.e. sequential unformatted) SpcCoeff data files.
!
!
! FILES ACCESSED:
!       Input: - binary format individual IASI band SpcCoeff datafiles.
!              - For user specified channel subsetting, a list file containing
!                the required IASI channels to subset.
!
!       Output: binary format IASI channel SUBSET SpcCoeff datafile.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Extract_IASI_SpcCoeff_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds        , ONLY: fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Display_Message, Program_Message
  USE List_File_Utility , ONLY: Integer_List_File_type, &
                                Read_List_File, &
                                Get_List_Size, &
                                Get_List_Entry
  USE SpcCoeff_Define   , ONLY: SpcCoeff_type, &
                                SpcCoeff_Associated, &
                                SpcCoeff_Create, &
                                SpcCoeff_Destroy, &
                                SpcCoeff_Subset, &
                                SpcCoeff_Concat
  USE SpcCoeff_Binary_IO, ONLY: SpcCoeff_Binary_ReadFile, &
                                SpcCoeff_Binary_WriteFile
  USE Subset_Define     , ONLY: Subset_type, &
                                Subset_Associated, &
                                Subset_Destroy
  USE IASI_Define       , ONLY: N_IASI_PLATFORMS, &
                                IASI_PLATFORM_NAME, &
                                N_IASI_CHANNELS, &
                                N_IASI_BANDS, &
                                IASI_BandName
  USE IASI_Subset,        ONLY: N_IASI_SUBSET_300, IASI_SUBSET_300, IASI_SUBSET_300_COMMENT, &
                                N_IASI_SUBSET_316, IASI_SUBSET_316, IASI_SUBSET_316_COMMENT, &
                                N_IASI_SUBSET_616, IASI_SUBSET_616, IASI_SUBSET_616_COMMENT, &
                                N_IASI_VALID_SUBSETS, IASI_VALID_SUBSET_NAME, &
                                IASI_Subset_Index
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Extract_IASI_SpcCoeff_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  ! String length
  INTEGER, PARAMETER :: SL = 256


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat   ;  CHARACTER(SL) :: err_msg
  INTEGER :: io_stat    ;  CHARACTER(SL) :: io_msg
  INTEGER :: alloc_stat ;  CHARACTER(SL) :: alloc_msg
  CHARACTER(SL) :: list_filename, in_filename, out_filename
  CHARACTER(20) :: sensor_id
  INTEGER :: i, set, platform
  INTEGER :: n_subset_channels
  INTEGER, ALLOCATABLE :: subset_list(:)
  TYPE(Integer_List_File_type) :: user_subset_list
  TYPE(SpcCoeff_type)          :: in_spccoeff, sc_subset(N_IASI_BANDS),  out_spccoeff


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to extract the IASI channel subset spectral '//&
                        'coefficient data from the individual band '//&
                        'SpcCoeff files and write them to a separate '//&
                        'datafile.', &
                        '$Revision$' )


  ! Select a platform
  Select_Platform_Loop: DO
    ! ...Prompt user to select a platform
    WRITE( *,'(/5x,"Select an IASI platform")' )
    DO i = 1, N_IASI_PLATFORMS
      WRITE( *,'(10x,i1,") ",a)' ) i, IASI_PLATFORM_NAME(i)
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,FMT='(i5)',IOSTAT=io_stat, IOMSG=io_msg ) platform
    ! ...Check for I/O errors
    IF ( io_stat /= 0 ) THEN
      err_msg = 'Invalid input - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...Check the input
    IF ( platform < 1 .OR. platform > N_IASI_PLATFORMS ) THEN
      err_msg = 'Invalid selection'
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    ELSE
      EXIT Select_Platform_Loop
    END IF
  END DO Select_Platform_Loop


  ! Select a subset set
  Select_Subset_Loop: DO
    ! ...Prompt user to select a subset set 
    WRITE( *,'(/5x,"Select an IASI channel subset")' )
    DO i = 1, N_IASI_VALID_SUBSETS
      WRITE( *,'(10x,i1,") ",a)' ) i, IASI_VALID_SUBSET_NAME(i)
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,FMT='(i5)',IOSTAT=io_stat, IOMSG=io_msg ) set
    ! ...Check for I/O errors
    IF ( io_stat /= 0 ) THEN
      err_msg = 'Invalid input - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...Check the input
    IF ( set < 1 .OR. set > N_IASI_VALID_SUBSETS ) THEN
      err_msg = 'Invalid selection'
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    ELSE
      EXIT Select_Subset_Loop
    END IF
  END DO Select_Subset_Loop


  ! Get the required channels list
  SELECT CASE ( set )

    ! The 300 channel subset
    CASE (1)
      ! ...Allocate list array
      n_subset_channels = N_IASI_SUBSET_300
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat )!, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '!//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      subset_list = IASI_SUBSET_300
      sensor_id   = 'iasi300_'//TRIM(IASI_PLATFORM_NAME(platform))


    ! The 316 channel subset
    CASE (2)
      ! ...Allocate list array
      n_subset_channels = N_IASI_SUBSET_316
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat )!, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '!//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      subset_list = IASI_SUBSET_316
      sensor_id   = 'iasi316_'//TRIM(IASI_PLATFORM_NAME(platform))


    ! The 616 channel subset
    CASE (3)
      ! ...Allocate list array
      n_subset_channels = N_IASI_SUBSET_616
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat )!, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '!//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      subset_list = IASI_SUBSET_616
      sensor_id   = 'iasi616_'//TRIM(IASI_PLATFORM_NAME(platform))


    ! All the channels
    CASE(4)
      ! ...Allocate list array
      n_subset_channels = N_IASI_CHANNELS
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat)!, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '!//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      Subset_List = (/(i,i=1,N_IASI_CHANNELS)/)
      WRITE( sensor_id,'("iasi",i0,"_",a)' ) n_subset_channels, TRIM(IASI_PLATFORM_NAME(platform))


    ! A user specified channel subset
    CASE (5)
      ! ...Get the list of channels required
      WRITE( *, FMT='(/5x,"Enter an IASI channel subset list filename : ")', ADVANCE='NO' )
      READ( *,FMT='(a)' ) list_filename
      list_filename = ADJUSTL(list_filename)
      ! ...
      err_stat = Read_List_File( list_filename, user_subset_list )
      IF ( err_stat /= SUCCESS ) THEN
        err_msg = 'Error reading channel subset list file '//TRIM(list_filename)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...
      n_subset_channels = Get_List_Size( user_subset_list )
      IF ( n_subset_channels < 1 ) THEN
        err_msg = 'No channels listed in '//TRIM(list_filename)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Check the number of channels
      IF ( n_subset_channels < 1 .OR. n_subset_channels > N_IASI_CHANNELS ) THEN
        err_msg = 'Number of channels listed in '//TRIM(list_filename)//' outside of valid range.'
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Allocate list array
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat )!, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '!//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      DO i = 1, n_subset_channels
        err_stat = Get_List_Entry( user_subset_list, i, subset_list(i) )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( err_msg,'("Error retrieving user subset channel list entry ",i0)' ) i
          CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
        END IF
      END DO
      WRITE( sensor_id,'("iasi",i0,"_",a)' ) n_subset_channels, TRIM(IASI_PLATFORM_NAME(platform))

  END SELECT


  ! Subset the individual band data
  DO i = 1, N_IASI_BANDS
    ! ...Define the filename
    in_filename = 'iasi'//TRIM(IASI_BandName(i))//'_'//&
                  TRIM(IASI_PLATFORM_NAME(platform))//'.SpcCoeff.bin'
    ! ...Read the input data
    err_stat = SpcCoeff_Binary_ReadFile( in_filename, in_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      err_msg = 'Error reading input SpcCoeff file '//TRIM(In_Filename)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...Channel subset the data
    CALL SpcCoeff_Subset( in_spccoeff, subset_list, sc_subset(i) )
  END DO
  
  
  ! Concatenate the individual band subsets
  CALL SpcCoeff_Concat( out_spccoeff, sc_subset, Sensor_Id = sensor_id )


  ! Write the output file
  out_filename = TRIM(sensor_id)//'.SpcCoeff.bin'
  err_stat = SpcCoeff_Binary_WriteFile( out_filename, out_spccoeff )
  IF ( err_stat /= SUCCESS ) THEN
    err_msg = 'Error writing the output SpcCoeff file '//TRIM(out_filename)
    CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
  END IF


  ! Cleanup
  CALL SpcCoeff_Destroy( in_spccoeff )
  CALL SpcCoeff_Destroy( sc_subset )
  CALL SpcCoeff_Destroy( out_spccoeff )

END PROGRAM Extract_IASI_SpcCoeff_Subset
