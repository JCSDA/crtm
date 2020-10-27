!
! Extract_CrIS_SpcCoeff_Subset
!
! Program to extract CrIS channel subsets from the individual
! CrIS band binary (i.e. sequential unformatted) SpcCoeff data files.
!
!
! FILES ACCESSED:
!       Input: - binary format individual CrIS band SpcCoeff datafiles.
!              - For user specified channel subsetting, a list file containing
!                the required CrIS channels to subset.
!
!       Output: binary format CrIS channel SUBSET SpcCoeff datafile.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 02-Jun-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM Extract_CrIS_SpcCoeff_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds        , ONLY: fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Display_Message, Program_Message
  USE ListFile_Utility  , ONLY: Integer_ListFile_type, &
                                ListFile_ReadFile, &
                                ListFile_GetSize, &
                                ListFile_GetEntry
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
  USE CrIS_Define       , ONLY: N_CRIS_CHANNELS, &
                                N_CRIS_BANDS, &
                                CrIS_BandName
  USE CrIS_Subset       , ONLY: CRIS_SUBSET_374_COMMENT, N_CRIS_SUBSET_374, CRIS_SUBSET_374, &
                                CRIS_SUBSET_399_COMMENT, N_CRIS_SUBSET_399, CRIS_SUBSET_399, &
                                N_CRIS_VALID_SUBSETS, CRIS_VALID_SUBSET_NAME, &
                                CrIS_Subset_Index
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Extract_CrIS_SpcCoeff_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  INTEGER, PARAMETER :: SL = 256


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat   ;  CHARACTER(SL) :: err_msg
  INTEGER :: io_stat    ;  CHARACTER(SL) :: io_msg
  INTEGER :: alloc_stat ;  CHARACTER(SL) :: alloc_msg
  CHARACTER(SL) :: list_filename, in_filename, out_filename
  CHARACTER(20) :: sensor_id
  INTEGER :: i, set
  INTEGER :: n_subset_channels
  INTEGER, ALLOCATABLE :: subset_list(:)
  TYPE(Integer_ListFile_type) :: user_subset_list
  TYPE(SpcCoeff_type)         :: in_spccoeff, sc_subset(N_CRIS_BANDS),  out_spccoeff


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to extract the CrIS channel subset spectral '//&
                        'coefficient data from the individual band '//&
                        'SpcCoeff files and write them to a separate '//&
                        'datafile.', &
                        '$Revision$' )


  ! Select a subset set
  Select_Loop: DO
    ! ...Prompt user to select a subset set 
    WRITE( *,'(/5x,"Select an CrIS channel subset")' )
    DO i = 1, N_CRIS_VALID_SUBSETS
      WRITE( *,'(10x,i1,") ",a)' ) i, CRIS_VALID_SUBSET_NAME(i)
    END DO
    WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )
    READ( *,FMT='(i5)',IOSTAT=io_stat, IOMSG=io_msg ) set
    ! ...Check for I/O errors
    IF ( io_stat /= 0 ) THEN
      err_msg = 'Invalid input - '//TRIM(io_msg)
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    END IF
    ! ...Check the input
    IF ( set < 1 .OR. set > N_CRIS_VALID_SUBSETS ) THEN
      err_msg = 'Invalid selection'
      CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
    ELSE
      EXIT Select_Loop
    END IF
  END DO Select_Loop


  ! Get the required channels list
  SELECT CASE ( set )

    ! The 374 channel subset
    CASE (1)
      ! ...Allocate list array
      n_subset_channels = N_CRIS_SUBSET_374
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      subset_list = CRIS_SUBSET_374
      sensor_id   = 'cris374_npp'


    ! The 399 channel subset
    CASE (2)
      ! ...Allocate list array
      n_subset_channels = N_CRIS_SUBSET_399
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      subset_list = CRIS_SUBSET_399
      sensor_id   = 'cris399_npp'


    ! All the channels
    CASE(3)
      ! ...Allocate list array
      n_subset_channels = N_CRIS_CHANNELS
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      Subset_List = (/(i,i=1,n_subset_channels)/)
      WRITE( sensor_id,'("cris",i0,"_npp")' ) n_subset_channels


    ! A user specified channel subset
    CASE (4)
      ! ...Get the list of channels required
      WRITE( *, FMT='(/5x,"Enter an CrIS channel subset list filename : ")', ADVANCE='NO' )
      READ( *,FMT='(a)' ) list_filename
      list_filename = ADJUSTL(list_filename)
      ! ...
      err_stat = ListFile_ReadFile( user_subset_list, list_filename )
      IF ( err_stat /= SUCCESS ) THEN
        err_msg = 'Error reading channel subset list file '//TRIM(list_filename)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...
      n_subset_channels = ListFile_GetSize( user_subset_list )
      IF ( n_subset_channels < 1 ) THEN
        err_msg = 'No channels listed in '//TRIM(list_filename)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Check the number of channels
      IF ( n_subset_channels < 1 .OR. n_subset_channels > N_CRIS_CHANNELS ) THEN
        err_msg = 'Number of channels listed in '//TRIM(list_filename)//' outside of valid range.'
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Allocate list array
      ALLOCATE( subset_list(n_subset_channels), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        err_msg = 'Error allocating Subset_List array - '//TRIM(alloc_msg)
        CALL Display_Message( PROGRAM_NAME, err_msg, FAILURE ); STOP
      END IF
      ! ...Fill values
      DO i = 1, n_subset_channels
        subset_list(i) = ListFile_GetEntry( user_subset_list, i )
      END DO
      WRITE( sensor_id,'("cris",i0,"_npp")' ) n_subset_channels

  END SELECT


  ! Subset the individual band data
  DO i = 1, N_CRIS_BANDS
    ! ...Define the filename
    in_filename = 'cris'//TRIM(CrIS_BandName(i))//'_npp.SpcCoeff.bin'
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

END PROGRAM Extract_CrIS_SpcCoeff_Subset
