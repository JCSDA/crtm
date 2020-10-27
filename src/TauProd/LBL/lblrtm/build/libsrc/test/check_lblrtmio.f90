!
! check_lblrtmio
!
! Check/example program for the LBLRTM File I/O.
!
! This program reads LBLRTM-generated single-layer/single-panel (ODdeflt_100),
! single-layer/double-panel (TAPE12), and multiple-layer/double-panel (TAPE13)
! datafiles.
!
! The default compilation of the LBLRTM I/O library is for double-precision
! LBLRTM files. The test datafiles are double-precision, little-endian format
! files.
!
! The minimum steps required to read an LBLRTM file are listed as such.
!
!

PROGRAM check_lblrtmio

  ! ============================================================================
  ! STEP 1. **** ENVIRONMENT SETUP FOR LBLRTM I/O USAGE ****
  !
  ! Module usage
  USE LBLRTMIO_Module
  ! Disable all implicit typing
  IMPLICIT NONE
  ! ============================================================================



  ! ============================================================================
  ! STEP 2. **** DEFINE THE LBLRTM File OBJECT AND REQUIRED VARIABLES ****
  !
  TYPE(LBLRTM_File_type) :: ofile, ofile_test
  INTEGER :: err_stat
  ! ============================================================================


  ! Local parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'check_lblrtmio'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id: check_lblrtmio.f90 37082 2014-02-21 15:32:14Z paul.vandelst@noaa.gov $'

  ! Local variables
  CHARACTER(256) :: version
  CHARACTER(256) :: message



  ! Program header
  CALL LBLRTMIO_Version( Version )
  CALL Program_Message( PROGRAM_NAME, &
    'Check/example program for the LBLRTM File I/O functions using ', &
    'LBLRTM I/O library version: '//TRIM(version) )



  ! ============================================================================
  ! STEP 3a. **** READ A SINGLE LAYER, SINGLE PANEL LBLRTM File ****
  !
  WRITE(*,'(/5x,"Test reading a single layer, single panel LBLRTM file...",/)')

  err_stat = LBLRTM_File_Read(ofile, 'test_data/ODdeflt_100')

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading single layer, single panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test write, re-read, and comparison of the File object
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...write
  err_stat = LBLRTM_File_Write(ofile,'slsp.dat',Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error writing test single layer, single panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...read
  err_stat = LBLRTM_File_Read(ofile_test,'slsp.dat',Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test single layer, single panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test single layer, single panel file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 3b. **** CONVERT A SINGLE LAYER, SINGLE PANEL LBLRTM File to netCDF ****
  !
  WRITE(*,'(/5x,"Converting a single layer, single panel LBLRTM file to netCDF...",/)')

  err_stat = LBLRTM_netCDF_ConvertFile( &
    'test_data/ODdeflt_100', &
    Title   = 'Single layer, single panel LBLRTM file to netCDF test', &
    History = PROGRAM_VERSION_ID, &
    Comment = 'Created as part of LBLRTMIO library test' )

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error converting single layer, single panel file to netCDF'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test read, and comparison of the netCDF file
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...read
  err_stat = LBLRTM_netCDF_ReadFile(ofile_test,'test_data/ODdeflt_100.nc') !,Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test single layer, single panel netCDF file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test single layer, single panel netCDF file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 4a. **** READ A SINGLE LAYER, DOUBLE PANEL LBLRTM File ****
  !
  WRITE(*,'(//5x,"Test reading a single layer, double panel LBLRTM file...",/)')

  err_stat = LBLRTM_File_Read(ofile, 'test_data/TAPE12',Double_Panel=.TRUE.)

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading single layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test write, re-read, and comparison of the File object
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...write
  err_stat = LBLRTM_File_Write(ofile,'sldp.dat',Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error writing test single layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...read
  err_stat = LBLRTM_File_Read(ofile_test,'sldp.dat',Quiet=.TRUE.,Double_Panel=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test single layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test single layer, double panel file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 4b. **** CONVERT A SINGLE LAYER, DOUBLE PANEL LBLRTM File to netCDF ****
  !
  WRITE(*,'(//5x,"Converting a single layer, double panel LBLRTM file to netCDF...",/)')

  err_stat = LBLRTM_netCDF_ConvertFile( &
    'test_data/TAPE12', &
    Double_Panel = .TRUE., &
    Title        = 'Single layer, double panel LBLRTM file to netCDF test', &
    History      = PROGRAM_VERSION_ID, &
    Comment      = 'Created as part of LBLRTMIO library test' )

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error converting single layer, double panel file to netCDF'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test read, and comparison of the netCDF file
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...read
  err_stat = LBLRTM_netCDF_ReadFile(ofile_test,'test_data/TAPE12.nc') !,Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test single layer, double panel netCDF file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test single layer, double panel netCDF file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 5a. **** READ A MULTIPLE LAYER, DOUBLE PANEL LBLRTM File ****
  !
  WRITE(*,'(//5x,"Test reading some layers from a multiple layer, double panel LBLRTM file...",/)')

  err_stat = LBLRTM_File_Read(ofile, 'test_data/TAPE13',n_Layers=3,Double_Panel=.TRUE.)

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading multiple layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test write, re-read, and comparison of the File object
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...write
  err_stat = LBLRTM_File_Write(ofile,'mldp.dat',Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error writing test multiple layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...read
  err_stat = LBLRTM_File_Read(ofile_test,'mldp.dat',n_Layers=3,Double_Panel=.TRUE.,Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test multiple layer, double panel file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test multiple layer, double panel file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 5b. **** CONVERT A MULTIPLE LAYER, DOUBLE PANEL LBLRTM File to netCDF ****
  !
  WRITE(*,'(//5x,"Converting a multiple layer, double panel LBLRTM file to netCDF...",/)')

  err_stat = LBLRTM_netCDF_ConvertFile( &
    'test_data/TAPE13', &
    n_Layers     = 3, &
    Double_Panel = .TRUE., &
    Title        = 'Multiple layer, double panel LBLRTM file to netCDF test', &
    History      = PROGRAM_VERSION_ID, &
    Comment      = 'Created as part of LBLRTMIO library test' )

  ! Error handler
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error converting multiple layer, double panel file to netCDF'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ============================================================================

  ! Now do a test read, and comparison of the netCDF file
  ! Screen output for these tests are suppressed via the "Quiet" argument.
  ! ...read
  err_stat = LBLRTM_netCDF_ReadFile(ofile_test,'test_data/TAPE13.nc') !,Quiet=.TRUE.)
  IF ( err_stat /= SUCCESS ) THEN
    message = 'Error reading test multiple layer, double panel netCDF file'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF
  ! ...check
  IF ( .NOT. LBLRTM_File_Compare(ofile_test, ofile) ) THEN
    message = 'Comparison for test multiple layer, double panel netCDF file failed'
    CALL Display_Message( PROGRAM_NAME, message, FAILURE )
    STOP
  END IF



  ! ============================================================================
  ! STEP 6. **** (OPTIONAL) CLEANUP THE LBLRTM File OBJECTS ****
  !
  CALL LBLRTM_File_Destroy(ofile)
  CALL LBLRTM_File_Destroy(ofile_test)
  ! ============================================================================



  ! Create signal file to indicate successful test
  OPEN(50, FILE='.signal')
  WRITE(50,*) 'Success'
  CLOSE(50)

END PROGRAM check_lblrtmio
