!
! Test_AntCorr_Application
!
! Program to test the AntCorr application and removal routines using data embedded
! within an SpcCoeff data structure, as well as a standalone AntCorr structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Aug-2008
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_AntCorr_Application

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO

  USE AntCorr_Application
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_AntCorr_Application'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  
  ! Test data filenames
  INTEGER, PARAMETER :: N_SENSORS = 2
  CHARACTER(*), PARAMETER :: SENSOR_ID(N_SENSORS) = (/'amsua_metop-a',&
                                                      'mhs_n18      '/)
  
!  CHARACTER(*), PARAMETER :: SC_FILENAME  = 'Data/amsua_metop-a.SpcCoeff.bin'
!  CHARACTER(*), PARAMETER :: AC_FILENAME  = 'Data/amsua_metop-a.AntCorr.nc'
  
  ! Test temperature
  REAL(fp), PARAMETER :: TEST_TEMPERATURE = 283.1415927_fp
  
  ! Data output format stuff
  CHARACTER(*), PARAMETER :: RPT_FMT = '8'
  CHARACTER(*), PARAMETER :: T_FMT   = 'f13.7'
  CHARACTER(*), PARAMETER :: DT_FMT  = 'es13.6'

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Filename
  INTEGER :: Error_Status, Allocate_Status
  INTEGER :: i, n
  TYPE(AntCorr_type)  :: AntCorr
  TYPE(SpcCoeff_type) :: SpcCoeff
  REAL(fp), ALLOCATABLE :: Torig(:), Tsc(:), Tac(:), Tb(:,:) 

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the AntCorr application and removal routines '//&
                        'using data embedded within an SpcCoeff data structure, as well '//&
                        'as a standalone AntCorr structure.', &
                        '$Revision$' )


  ! Print the spacing between model numbers
  ! ---------------------------------------
  WRITE( *,'(/2x,"Spacing of model numbers near T =",'//T_FMT//'," : ",'//DT_FMT//',/)' ) &
           TEST_TEMPERATURE, SPACING(TEST_TEMPERATURE)

  ! Begin loop over sensors
  ! -----------------------
  Sensor_Loop: DO n = 1, N_SENSORS


    ! =========================================================
    ! Application of the AntCorr data via the SpcCoeff datafile
    ! =========================================================
    ! Read the SpcCoeff datafile
    ! --------------------------
    Filename = 'Data/'//TRIM(SENSOR_ID(n))//'.SpcCoeff.bin'
    Error_Status = Read_SpcCoeff_Binary( Filename, SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error reading SpcCoeff data.', FAILURE )
      STOP
    END IF
  
    ! Allocate some arrays for temperatures
    ! -------------------------------------
    ALLOCATE( Torig(SpcCoeff%AC%n_Channels), &
              Tsc(SpcCoeff%AC%n_Channels), &
              Tb(SpcCoeff%AC%n_Channels,SpcCoeff%AC%n_FOVS), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error allocating SpcCoeff T arrays.', FAILURE )
      STOP
    END IF
  
    ! Some pretend antenna temperatures
    ! ---------------------------------
    Torig = TEST_TEMPERATURE
    Tsc   = Torig

    ! Apply and remove the antenna correction at each FOV
    ! ---------------------------------------------------
    WRITE( *,'(/2x,"SpcCoeff test case: ",a,". n_FOVS,n_Channels:",i0,",",i0)' ) &
              TRIM(Filename), SpcCoeff%AC%n_FOVS, SpcCoeff%AC%n_Channels
    DO i = 1, SpcCoeff%AC%n_FOVS
      WRITE(*,'(/2x,"FOV: ",i0)') i
      
      CALL Apply_AntCorr(SpcCoeff%AC, i, Tsc)
      
      WRITE(*,'(4x,"Tb values:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//T_FMT//'))') Tsc
      Tb(:,i) = Tsc
      
      CALL Remove_AntCorr(SpcCoeff%AC, i, Tsc)
      
      WRITE(*,'(4x,"Restored Ta values:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//T_FMT//'))') Tsc
      WRITE(*,'(4x,"Torig-Ta residuals:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//DT_FMT//'))') Torig-Tsc
      READ(*,*)
    END DO


    ! ========================================================
    ! Application of the AntCorr data via the AntCorr datafile
    ! ========================================================

    ! Read the AntCorr datafile
    ! -------------------------
    Filename = 'Data/'//TRIM(SENSOR_ID(n))//'.AntCorr.nc'
    Error_Status = Read_AntCorr_netCDF( Filename, AntCorr )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error reading AntCorr data.', FAILURE )
      STOP
    END IF
  
    ! Allocate some arrays for temperatures
    ! -------------------------------------
    ALLOCATE( Tac(SpcCoeff%AC%n_Channels), &
              STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error allocating AntCorr T arrays.', FAILURE )
      STOP
    END IF
  
    ! Some pretend antenna temperatures
    ! ---------------------------------
    Tac = TEST_TEMPERATURE

    ! Apply and remove the antenna correction at each FOV
    ! ---------------------------------------------------
    WRITE( *,'(/2x,"AntCorr test case: ",a,". n_FOVS,n_Channels:",i0,",",i0)' ) &
              TRIM(Filename), AntCorr%n_FOVS, AntCorr%n_Channels
    DO i = 1, AntCorr%n_FOVS
      WRITE(*,'(/2x,"FOV: ",i0)') i
      
      CALL Apply_AntCorr(AntCorr, i, Tac)
      
      WRITE(*,'(4x,"Tb values:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//T_FMT//'))') Tac
      WRITE(*,'(4x,"Tb(SC)-Tb(AC) residuals:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//DT_FMT//'))') Tb(:,i)-Tac
      
      CALL Remove_AntCorr(AntCorr, i, Tac)
      
      WRITE(*,'(4x,"Restored Ta values:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//T_FMT//'))') Tac
      WRITE(*,'(4x,"Torig-Ta residuals:")') 
      WRITE(*,'('//RPT_FMT//'(1x,'//DT_FMT//'))') Torig-Tac
      READ(*,*)
    END DO


    ! ========
    ! Clean up
    ! ========
    DEALLOCATE( Torig, Tsc, Tac, Tb, STAT=Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error deallocating T arrays.', FAILURE )
      STOP
    END IF
    Error_Status = Destroy_AntCorr( AntCorr )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error destroying AntCorr.', Error_Status )
      STOP
    END IF
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, 'Error destroying SpcCoeff.', Error_Status )
      STOP
    END IF
  
  
    ! ==============
    ! Do next sensor
    ! ==============
    IF ( n < N_SENSORS ) THEN
      WRITE(*,'(//5x,"Press <ENTER> to test next sensor...")')
      READ(*,*)
    END IF
    
  END DO Sensor_Loop

END PROGRAM Test_AntCorr_Application
