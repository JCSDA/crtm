! Test_SRF
!
! Program to test the SRF structure definition, I/O, and Utility procedures.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Sep-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_SRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE Fundamental_Constants, ONLY: PI
  USE SRF_Define
  USE SRF_netCDF_IO
  USE SRF_ASCII_IO
  USE SRF_Utility
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_SRF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  ! Test output filenames
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.SRF.nc'
  CHARACTER(*), PARAMETER :: INC_FILENAME = 'Test.iSRF.nc'
  CHARACTER(*), PARAMETER :: ASC_FILENAME = 'Test.SRF'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO  = 0.0_fp
  REAL(fp), PARAMETER :: ONE   = 1.0_fp
  REAL(fp), PARAMETER :: TWO   = 2.0_fp
  REAL(fp), PARAMETER :: THREE = 3.0_fp
  REAL(fp), PARAMETER :: FOUR  = 4.0_fp
  REAL(fp), PARAMETER :: TWOPI = TWO*PI
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Interpolation order
  INTEGER, PARAMETER :: ORDER = 3
  ! Memory leak looping values
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 500
  INTEGER, PARAMETER :: INFO_N_LOOPS = 100
  ! Structure dimensions and data
  INTEGER, PARAMETER :: N_CHANNELS = 10
  INTEGER, PARAMETER :: N_POINTS = 100
  INTEGER, PARAMETER :: SENSOR_TYPE = INFRARED_SENSOR
  INTEGER, PARAMETER :: INT_MULTIPLIER = 3
  ! Band frequency parameters
  REAL(fp), PARAMETER :: F1_B1(N_CHANNELS/2) = &
    (/667.45_fp,700.35_fp,801.37_fp,851.33_fp,1002.21_fp/)
  REAL(fp), PARAMETER :: F2_B1(N_CHANNELS/2) = &
    (/687.45_fp,750.35_fp,821.37_fp,871.33_fp,1102.21_fp/)
  REAL(fp), PARAMETER :: F1_B2(2,N_CHANNELS/2) = &
    RESHAPE( (/ 710.11_fp, 725.11_fp, &
                980.13_fp,1050.13_fp, &
               1300.49_fp,1500.49_fp, &
               2100.01_fp,2280.01_fp, &
               2400.01_fp,2580.01_fp /), &
             (/2,N_CHANNELS/2/) )
  REAL(fp), PARAMETER :: F2_B2(2,N_CHANNELS/2) = &
    RESHAPE( (/ 715.11_fp, 730.11_fp, &
               1030.57_fp,1100.57_fp, &
               1400.64_fp,1600.64_fp, &
               2120.01_fp,2300.01_fp, &
               2420.01_fp,2600.01_fp /), &
             (/2,N_CHANNELS/2/) )
  ! Spectrum creation parameters
  REAL(fp), PARAMETER :: F1SPC = 600.0_fp
  REAL(fp), PARAMETER :: F2SPC = 2620.0_fp
  INTEGER,  PARAMETER :: N_SPC = 10000
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: Sensor_Id
  CHARACTER(256) :: Title    
  CHARACTER(256) :: History  
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: i, j, n
  INTEGER :: FileId
  INTEGER :: Channel_List(N_CHANNELS)
  TYPE(SRF_type) :: SRF(N_CHANNELS), SRF_Copy, iSRF
  REAL(fp) :: f(N_SPC), spc(N_SPC), cnv_spc

  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the SRF structure definition, '//&
                        'I/O, and Utility procedures.', &
                        '$Revision$' )


  ! Create some pretend netCDF output files
  ! ---------------------------------------
  Error_Status = Create_SRF_netCDF( NC_FILENAME, &
                                    SENSOR_TYPE, &
                                    (/(i,i=1,N_CHANNELS)/), &
                                    Sensor_Id = 'sensor_platform', &
                                    WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID, &
                                    WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID, &
                                    Title   = 'This is a Title', &
                                    History = 'This is a History', &
                                    Comment = 'This is a Comment' )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error creating SRF output file',FAILURE ); STOP
  END IF
                                    
  Error_Status = Create_SRF_netCDF( INC_FILENAME, &
                                    SENSOR_TYPE, &
                                    (/(i,i=1,N_CHANNELS)/), &
                                    Sensor_Id = 'sensor_platform', &
                                    WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID, &
                                    WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID, &
                                    Title   = 'This is a Title', &
                                    History = 'This is a History', &
                                    Comment = 'This is a Comment' )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error creating iSRF output file',FAILURE ); STOP
  END IF
                                    
                                    
  ! Fill the SRF structure array
  ! ----------------------------
  DO i = 1, N_CHANNELS
  
    ! Create single and double band SRFs
    IF ( i <= 5 ) THEN
      ! Allocate it
      Error_Status = Allocate_SRF( N_POINTS,SRF(i) )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error allocating channel ",i0," single band SRF structure array")' ) i
        CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
      END IF
      ! Fill structure with pretend data
      SRF(i)%Sensor_Id        = 'sensor_platform'
      SRF(i)%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
      SRF(i)%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
      SRF(i)%Sensor_Type      = SENSOR_TYPE
      SRF(i)%Channel          = i
      SRF(i)%f1_Band          = F1_B1(i)
      SRF(i)%f2_Band          = F2_B1(i)
      SRF(i)%npts_Band        = N_POINTS
      SRF(i)%Response         = SIN((/(PI*REAL(j-1,fp)/REAL(N_POINTS-1,fp),j=1,N_POINTS)/))
      
    ELSE
      ! Allocate it
      Error_Status = Allocate_SRF( N_POINTS,SRF(i),n_Bands=2 )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error allocating channel ",i0," double band SRF structure array")' ) i
        CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
      END IF
      ! Fill structure with pretend data
      SRF(i)%Sensor_Id        = 'sensor_platform'
      SRF(i)%WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
      SRF(i)%WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID   
      SRF(i)%Sensor_Type      = SENSOR_TYPE
      SRF(i)%Channel          = i
      SRF(i)%f1_Band          = F1_B2(:,i-5)
      SRF(i)%f2_Band          = F2_B2(:,i-5)
      SRF(i)%npts_Band        = (/N_POINTS/2,N_POINTS/2/)
      SRF(i)%Response         = ABS(SIN((/(TWOPI*REAL(j-1,fp)/REAL(N_POINTS-1,fp),j=1,N_POINTS)/)))
    END IF
  
    ! Compute frequency grid
    Error_Status = Frequency_SRF( SRF(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Error computing SRF frequency grid.',FAILURE ); STOP
    END IF
  
    ! Integrate SRF
    Error_Status = Integrate_SRF( SRF(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Error integrating SRF.',FAILURE ); STOP
    END IF
  END DO

  ! Test the SRF netCDF I/O functions
  ! ---------------------------------
  WRITE(*,'( /5x, "Testing SRF netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test netCDF SRF datafile ..." )' )
  DO i = 1, N_CHANNELS
    ! Write SRF
    Error_Status = Write_SRF_netCDF( NC_FILENAME,SRF(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error writing channel ",i0," SRF to ",a)' ) i, NC_FILENAME
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Read SRF
    Error_Status = Read_SRF_netCDF( NC_FILENAME,i,SRF_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error reading channel ",i0," SRF from ",a)' ) i, NC_FILENAME
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Test for equality
    Error_Status = Equal_SRF( SRF(i), SRF_Copy, Check_All=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error comparing channel ",i0," SRF data")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    WRITE( *,'(5x,"Channel ",i0," SRFs are equal",/)' ) i
  END DO
  
  ! Test read the datafile multiple times for memleak check
  WRITE( *, '( 10x, "Looping for SRF netCDF read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    DO i = 1, N_CHANNELS
      Error_Status = Read_SRF_netCDF( NC_FILENAME,i,SRF(i),Quiet=SET )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error reading channel ",i0," SRF from ",a)' ) i, NC_FILENAME
        CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
      END IF
    END DO
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( msg,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),INFORMATION )
    END IF
  END DO


  ! Test the SRF ASCII I/O functions
  ! --------------------------------
  WRITE(*,'( /5x, "Testing SRF ASCII I/O functions ..." )' )

  ! Write a file
  WRITE(*,'( 10x, "Writing test ASCII SRF datafile ..." )' )
  ! Write header
  Error_Status = Write_SRF_ASCII_Header( ASC_FILENAME, FileId, &
                                         n_Channels, &
                                         (/(i,i=1,N_CHANNELS)/), &
                                         Sensor_Id = 'sensor_platform', &
                                         Title     = 'This is a Title', &
                                         History   = 'This is a History', &
                                         Comment   = 'This is a Comment' )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error writing ASCII SRF file header to '//ASC_FILENAME
    CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
  END IF
  ! Write channel data
  DO i = 1, N_CHANNELS
    ! Write SRF
    Error_Status = Write_SRF_ASCII( ASC_FILENAME,FileId,SRF(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error writing channel ",i0," SRF to ",a)' ) i, ASC_FILENAME
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
  END DO
  CLOSE(FileId)
  
  ! Read a file
  WRITE(*,'( 10x, "Reading test ASCII SRF datafile ..." )' )
  ! Read header
  Error_Status = Read_SRF_ASCII_Header( ASC_FILENAME, FileId, &
                                        n, Channel_List, &
                                        Sensor_Id = Sensor_Id, &
                                        Title     = Title    , &
                                        History   = History  , &
                                        Comment   = Comment    )
  IF ( Error_Status /= SUCCESS ) THEN
    msg = 'Error reading ASCII SRF file header from '//ASC_FILENAME
    CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
  END IF
  ! Read channel data
  DO i = 1, N_CHANNELS
    ! Read SRF
    Error_Status = Read_SRF_ASCII( ASC_FILENAME,FileId,i,SRF_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error reading channel ",i0," SRF from ",a)' ) i, ASC_FILENAME
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Copy integral results to ensure equality
    SRF_Copy%Integrated_SRF = SRF(i)%Integrated_SRF
    SRF_Copy%Summation_SRF  = SRF(i)%Summation_SRF
    ! Test for equality
    Error_Status = Equal_SRF( SRF(i), SRF_Copy, Check_All=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error comparing channel ",i0," SRF data")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    WRITE( *,'(5x,"Channel ",i0," SRFs are equal",/)' ) i
  END DO


  ! Loop for assign memleak test
  ! ----------------------------
  WRITE(*,'( /5x, "Looping for SRF structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    DO i = 1, N_CHANNELS
      Error_Status = Assign_SRF( SRF(i), SRF_Copy )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( msg,'("Error copying channel ",i0," SRF on attempt # ",i0)' ) i, n
        CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
      END IF
    END DO
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( msg,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),INFORMATION )
    END IF
  END DO


  ! Test the SRF interpolator
  ! -------------------------
  WRITE(*,'( /5x, "Test the SRF interpolation function..." )' )
  DO i = 1, N_CHANNELS
    ! Allocate the structure for the interpolated SRF
    Error_Status = Allocate_SRF( SRF(i)%n_Points*INT_MULTIPLIER,iSRF,n_Bands=SRF(i)%n_Bands )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error allocating channel ",i0," iSRF")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Copy over the band frequency info
    iSRF%f1_Band   = SRF(i)%f1_Band
    iSRF%f2_Band   = SRF(i)%f2_Band
    iSRF%npts_Band = SRF(i)%npts_Band * INT_MULTIPLIER
    ! Compute the frequency grid
    Error_Status = Frequency_SRF( iSRF )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error computing channel ",i0," iSRF frequency grid")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Interpolate the SRF
    Error_Status = Interpolate_SRF( SRF(i),iSRF,Order=ORDER )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error interpolating channel ",i0," SRF")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    ! Write interpolated SRF to file
    Error_Status = Write_SRF_netCDF( INC_FILENAME,iSRF )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error writing channel ",i0," iSRF to ",a)' ) i, INC_FILENAME
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
  END DO
  

  ! Test the SRF convolver
  ! ----------------------
  WRITE(*,'( /5x, "Test the SRF convolution function..." )' )
  ! Create a pretend spectrum
  f = (/(REAL(i-1,fp),i=1,N_SPC)/) / REAL(N_SPC-1,fp)
  spc = SIN(PI*f*50.0_fp) + ONE
  f = F1SPC + f*(F2SPC-F1SPC)
  ! Convolve each channel separately  
  DO i = 1, N_CHANNELS
    Error_Status = Convolve_with_SRF( f, spc, SRF(i), &
                                      cnv_spc, &
                                      Interpolate=SET, &
                                      Order      =Order )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( msg,'("Error convolving SPC with channel ",i0," iSRF")' ) i
      CALL Display_Message( PROGRAM_NAME,TRIM(msg),FAILURE ); STOP
    END IF
    WRITE( msg,'("Ch.",i0," convolved result: ",es17.10)' ) SRF(i)%Channel, cnv_spc
    CALL Display_Message( PROGRAM_NAME,TRIM(msg),INFORMATION )
  END DO
  
  
  ! Destroy the structures
  ! ----------------------
  DO i = 1, N_CHANNELS
    Error_Status = Destroy_SRF( SRF(i) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME,'Error destroying SRF structure.',Error_Status )
      STOP
    END IF
  END DO
  Error_Status = Destroy_SRF( SRF_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying SRF_Copy structure.',Error_Status )
    STOP
  END IF
  Error_Status = Destroy_SRF( iSRF )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error destroying iSRF structure.',Error_Status )
    STOP
  END IF
    
END PROGRAM Test_SRF
