!
! ODPSNC2ODSSUBIN
!
! Program to convert the ODPS tau coeff data to ODSSU, ODPS in netCDF format, and
! ODSSU in binary format.
!
! CREATION HISTORY:
!       Written by:    Yong Chen, 06-Nov-2009
!                      yong.chen@noaa.gov
!

PROGRAM ODPSNC2ODSSUBIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds        , ONLY: Long, fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Program_Message, Display_Message
  USE SignalFile_Utility, ONLY: Create_SignalFile
  ! the new ssu tau coeff. data type and reader
  USE ODSSU_Define      , ONLY: ODSSU_type, Allocate_ODSSU, Destroy_ODSSU 
  USE ODSSU_Binary_IO   , ONLY: Write_ODSSU_Binary
  ! the ODAS tau coeff. data structure
  USE ODPS_Define       , ONLY: ODPS_type, Allocate_ODPS, Assign_ODPS, Destroy_ODPS, ODPS_ALGORITHM
  USE ODPS_netCDF_IO    , ONLY: Read_ODPS_netCDF, Write_ODPS_netCDF
  USE File_Utility      , ONLY: Get_Lun, File_Exists, File_Open

  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ODPSNC2ODSSUBIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER, PARAMETER :: n_absorbers = 3  
   
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  CHARACTER(256) :: msg
  CHARACTER(256) :: CellPresssure_Filename
  CHARACTER(256) :: ODSSU_Filename
  CHARACTER(256) :: ODPS_Filename
  CHARACTER(20)  :: Sensor_Id
  TYPE(ODPS_type)  :: ODPS
  TYPE(ODSSU_type) :: ODSSU
  INTEGER :: k 
  INTEGER :: n_Channels, n_TC_CellPressures, n_Ref_CellPressures
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: TC_CellPressure
  REAL(fp), DIMENSION(:)  , ALLOCATABLE :: Ref_Time         
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: Ref_CellPressure 
  
  ! Output prgram header
  ! --------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert the ODPS SSU tau coeff. data file  '//&
                        'the new ODSSU data structure file for '//&
                        'use with the multiple-algorithm form of the CRTM.', &
                        '$Revision$' )

  ! Enter a sensor Id and type
  ! --------------------------
  WRITE( *,FMT='(/5x,"Enter a sensor ID: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  

  ! Construct the filenames
  ! -----------------------
  CellPresssure_Filename = TRIM(Sensor_Id)//'.cellpressure.ASC'
  ODSSU_Filename = TRIM(Sensor_Id)//'.TauCoeff.bin'
    
  ! Read the cell pressure ASCII data
  ! ----------------------
  WRITE( *,'(/5x,"Reading TauCoeff cell pressure file ",a,"...")' ) TRIM(CellPresssure_Filename)
  ! Open the file if required                    
  !--------------------------                    
  IF ( .NOT. File_Open( TRIM( CellPresssure_Filename) ) ) THEN  

    ! Get a file unit number                     
    FileID = Get_Lun()                           
 
    ! Open the file                                                          
    OPEN( FileID, FILE   = TRIM(CellPresssure_Filename), &                                 
                  STATUS = 'OLD', &                                          
                  FORM   = 'FORMATTED', &                                    
                  ACCESS = 'SEQUENTIAL', &                                   
                  ACTION = 'READ', &                                         
                  IOSTAT = IO_Status )                                       
    IF ( IO_Status /= 0 ) THEN                                               
      Error_Status = FAILURE                                                   
      WRITE( msg,'("Error opening ASCII Cell Pressure file ",a,". IOSTAT = ",i0)' ) &  
                 TRIM(CellPresssure_Filename ), IO_Status                                   
      CALL Display_Message( PROGRAM_NAME, &                                    
                            msg, &                                   
                            Error_Status)                          
      STOP
    END IF                                                                   
  END IF
  ! Read the dimensions
  ! -------------------------------------
  READ(FileID, *, IOSTAT=IO_Status) n_Channels, n_TC_CellPressures, n_Ref_CellPressures
  IF ( IO_Status /= 0 ) THEN                                                        
    Error_Status = FAILURE                                                          
    WRITE( msg,'("Error reading number of channels from ",a,". IOSTAT = ",i0)' ) &  
               TRIM(CellPresssure_Filename ), IO_Status                             
    CALL Display_Message( PROGRAM_NAME, &                                           
                          msg, &                                                    
                          Error_Status)                                             
    STOP                                                                            
  END IF                                                                            

  ALLOCATE( TC_CellPressure( n_TC_CellPressures, n_Channels)  , &
            Ref_Time( n_Ref_CellPressures )                   , &
            Ref_CellPressure( n_Ref_CellPressures, n_Channels), & 
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN                
    Error_Status = FAILURE                                                   
    CALL Display_Message( PROGRAM_NAME, &                                    
                          'Error allocating ODSSU data arrays ' , &                                   
                          Error_Status)                          
    STOP                                                                   
  END IF                                                                     
  
  READ(FileID, *) TC_CellPressure
  READ(FileID, *) Ref_CellPressure
  READ(FileID, *) Ref_Time
  CLOSE( FileID)  
  
  ODSSU%subAlgorithm = ODPS_ALGORITHM
  
  Error_Status = Allocate_ODSSU(n_Absorbers        , &    
                                n_Channels         , &    
                                n_TC_CellPressures , &    
                                n_Ref_CellPressures, &    
                                ODSSU )            
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating memory for the ODSSU structure ', &
                          Error_Status )
    STOP
  END IF                                                           
  
  ! assign values taken from the ASCII file to ODSSU            
  ODSSU%Sensor_Id        = TRIM(Sensor_Id)               
  ODSSU%TC_CellPressure  = TC_CellPressure      
  ODSSU%Ref_Time         = Ref_Time             
  ODSSU%Ref_CellPressure = Ref_CellPressure     


  CellPressure_loop: DO k = 1, ODSSU%n_TC_CellPressures                                                 
 
    ! Read the input netCDF file
    WRITE( *,FMT='(/5x,"Enter the netCDF ODPS series data: ")',ADVANCE='NO' )
    READ( *,'(a)' ) ODPS_Filename
    
    Error_Status = Read_ODPS_netCDF( TRIM(ODPS_Filename), ODPS )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading netCDF ODPS file '//&
                            TRIM( ODPS_Filename ), &
                            Error_Status )
      STOP
    END IF
    IF ( k == 1 ) THEN
!      ODSSU%Release          =  ODPS%Release
!      ODSSU%Version          =  ODPS%Version
      ODSSU%Sensor_Channel   =  ODPS%Sensor_Channel   
      ODSSU%Absorber_ID      =  ODPS%Absorber_ID      
      ODSSU%WMO_Satellite_ID =  ODPS%WMO_Satellite_ID 
      ODSSU%WMO_Sensor_ID    =  ODPS%WMO_Sensor_ID    
    ENDIF
    
    Error_Status = Assign_ODPS( ODPS, &                               
                                ODSSU%ODPS(k))                        
                                                                      
    IF ( Error_Status /= SUCCESS ) THEN                               
      CALL Display_Message( PROGRAM_NAME, &                           
                            'Error assigning ODPS to ODSSU%ODPS ', &  
                            Error_Status )                            
      STOP                                                            
    END IF                                                            

  END DO CellPressure_loop
    
  Error_Status = Write_ODSSU_Binary( ODSSU_Filename, &
                                     ODSSU )    
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing ODSSU file '//&
                          TRIM(ODSSU_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Create a signal file indicating success
  Error_Status = Create_SignalFile( ODSSU_Filename )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(ODSSU_Filename), &
                          FAILURE )
  END IF


  ! Clean up
  ! --------
  DEALLOCATE( TC_CellPressure , &
              Ref_Time        , &
              Ref_CellPressure, & 
              STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN                
    Error_Status = FAILURE                                                   
    CALL Display_Message( PROGRAM_NAME, &                                    
                          'Error deallocating ODSSU data arrays ' , &                                   
                          Error_Status)                          
    STOP                                                                   
  END IF                                                                     

  Error_Status = Destroy_ODPS( ODPS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODSSU( ODSSU )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODSSU structure', &
                          WARNING )
  END IF

END PROGRAM ODPSNC2ODSSUBIN
