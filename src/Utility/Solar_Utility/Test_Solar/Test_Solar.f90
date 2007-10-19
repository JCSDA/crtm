!
! Test_Solar
!
! Program to test the Solar structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_Solar

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE Solar_Define
  USE Solar_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_Solar'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.Solar.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 50000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10000
  ! Structure dimensions and init data
  INTEGER , PARAMETER :: N_FREQUENCIES = 1000
  REAL(fp), PARAMETER :: BEGIN_FREQUENCY =  400.0_fp
  REAL(fp), PARAMETER :: END_FREQUENCY   = 3600.0_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Source
  CHARACTER(256) :: References
  INTEGER :: Error_Status
  INTEGER :: n
  TYPE(Solar_type) :: Solar, Solar_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the Solar structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the Solar structure
  ! ---------------------------------
  Error_Status = Allocate_Solar( N_FREQUENCIES, &  ! Input
                                 Solar          )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Solar structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  Solar%Begin_Frequency       = BEGIN_FREQUENCY
  Solar%End_Frequency         = END_FREQUENCY  
  Solar%Frequency_Interval    = ( Solar%End_Frequency-Solar%Begin_Frequency ) / &
                                REAL(N_FREQUENCIES-1,fp)
  Solar%Irradiance           = 1.0_fp
  Solar%Blackbody_Irradiance = 2.0_fp

  ! Compute the frequency grid
  Error_Status = FRequency_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing Solar frequency grid.', &
                          FAILURE )
    STOP
  END IF


  ! Test the Solar netCDF I/O functions
  ! ----------------------------------------
  WRITE( *,'(/5x,"Testing Solar netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE( *,'(/10x,"Writing/reading/checking test netCDF Solar datafile ...")' )
  Error_Status = Write_Solar_netCDF( NC_FILENAME, Solar, &
                                     Title     ='This is a title'                 , &
                                     History   =PROGRAM_RCS_ID                    , &
                                     Comment   ='This is a comment'               , &
                                     Source    ='This is the irradiance source'   , &
                                     References='This is the irradiance reference'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test Solar netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_Solar_netCDF( NC_FILENAME, Solar_Copy, &
                                    Title     =Title     , &
                                    History   =History   , &
                                    Comment   =Comment   , &
                                    Source    =Source    , &
                                    References=References  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test Solar netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *,'(10x,"Title  :    ",a)') TRIM(Title     )
  WRITE( *,'(10x,"History:    ",a)') TRIM(History   )
  WRITE( *,'(10x,"Comment:    ",a)') TRIM(Comment   )
  WRITE( *,'(10x,"Source:     ",a)') TRIM(Source    )
  WRITE( *,'(10x,"References: ",a)') TRIM(References)
  Error_Status = Equal_Solar( Solar, Solar_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test Solar data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *,'(/10x,"Looping for Solar netCDF read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_Solar_netCDF( NC_FILENAME, Solar, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading Solar datafile on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE( *,'(//5x,"Looping for Solar structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_Solar( Solar, Solar_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying Solar structure array on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = Destroy_Solar( Solar )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_Solar( Solar_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Solar_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_Solar
