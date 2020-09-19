!
! TauCoeff2ODSSU
!
! Program to convert the old tau coeff data to ODSSU, both in binary format
!
!
! CREATION HISTORY:
!       Written by:    Yong Han, Oct. 7, 2009
!

PROGRAM TauCoeff2ODSSU

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds        , ONLY: Long, fp
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Program_Message, Display_Message
  ! old ssu tau coeff. data type and reader
  USE TauCoeff_Define_ssu, ONLY: TauCoeff_ssu_type, Destroy_TauCoeff_ssu
  USE TauCoeff_Binary_IO_ssu, ONLY: Read_TauCoeff_Binary_ssu
  ! the new ssu tau coeff. data type and reader
  USE ODSSU_Define      , ONLY: ODSSU_type, Allocate_ODSSU, Destroy_ODSSU 
  USE ODSSU_Binary_IO   , ONLY: Write_ODSSU_Binary
  ! the ODAS tau coeff. data structure
  USE ODAS_Define       , ONLY: ODAS_type, Allocate_ODAS, Assign_ODAS, Destroy_ODAS, ODAS_ALGORITHM

  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauCoeff2ODSSU'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: TauCoeff_Filename
  CHARACTER(256) :: ODSSU_Filename
  CHARACTER(20)  :: Sensor_Id
  INTEGER        :: Sensor_Type
  TYPE(TauCoeff_ssu_type) :: TauCoeff
  TYPE(ODAS_type)  :: ODAS
  TYPE(ODSSU_type) :: ODSSU
  INTEGER, PARAMETER :: n_Alphas = 3  ! number of alpha coefficients, fixed
  INTEGER :: i, j, k, l, m, io, ip, no, np, ps, n_Coeffs


  ! Output prgram header
  ! --------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert the old binary SSU tau coeff. data file  '//&
                        'the new ODSSU data structure file for '//&
                        'use with the multiple-algorithm form of the CRTM.', &
                        '$Revision$' )

  ! Enter a sensor Id and type
  ! --------------------------
  WRITE( *,FMT='(/5x,"Enter a sensor ID: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  

  ! Construct the filenames
  ! -----------------------
  TauCoeff_Filename = TRIM(Sensor_Id)//'.TauCoeff.bin'  

  IF ( TRIM(Sensor_Id) == 'ssu_n05' ) Sensor_Id = 'ssu_tirosn'

  ODSSU_Filename    = TRIM(Sensor_Id)//'.ODSSU.TauCoeff.bin'
    
  ! Read the TauCoeff data
  ! ----------------------
  WRITE( *,'(/5x,"Reading TauCoeff file ",a,"...")' ) TRIM(TauCoeff_Filename)
  Error_Status = Read_TauCoeff_Binary_ssu( TauCoeff_Filename, &
                                           TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading TauCoeff file '//&
                          TRIM(TauCoeff_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ODSSU%subAlgorithm = ODAS_ALGORITHM
  
  Error_Status = Allocate_ODSSU(TauCoeff%n_Absorbers        , &    
                                TauCoeff%n_Channels         , &    
                                TauCoeff%n_TC_CellPressures , &    
                                TauCoeff%n_Ref_CellPressures, &    
                                ODSSU )            
  IF ( Error_Status /= SUCCESS ) THEN                              
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating memory for the ODSSU structure ', &
                          Error_Status )
    STOP
  END IF                                                           
  
  ! assign values taken from an ODAS to ODSSU            
  ODSSU%Sensor_Channel   = TauCoeff%Sensor_Channel       
  ODSSU%Absorber_ID      = TauCoeff%Absorber_ID          
  ODSSU%Sensor_Id        = TRIM(Sensor_Id)               
  ODSSU%WMO_Satellite_ID = TauCoeff%WMO_Satellite_ID(1)  
  ODSSU%WMO_Sensor_ID    = TauCoeff%WMO_Sensor_ID(1)     
  ODSSU%TC_CellPressure  = TauCoeff%TC_CellPressure      
  ODSSU%Ref_Time         = TauCoeff%Ref_Time             
  ODSSU%Ref_CellPressure = TauCoeff%Ref_CellPressure     

  CellPressure_loop: DO k = 1, ODSSU%n_TC_CellPressures                                                 
                                                                                                        
    n_Coeffs = 0                                                                                        
    DO l = 1, TauCoeff%n_Channels                                                                       
      DO j = 1, TauCoeff%n_Absorbers                                                                    
        np = TauCoeff%Predictor_Index(0, j, l, k)                                                       
        IF( np > 0 )THEN                                                                                
          IF(MAXVAL(TauCoeff%Order_Index(:, j, l, k)) /= MINVAL(TauCoeff%Order_Index(:, j, l, k)))THEN  
            CALL Display_Message( PROGRAM_NAME, &                                                       
                              'Error: the polynomial orders for each C coeffs are not equal. '//&       
                              TRIM(TauCoeff_Filename), &                                                
                              Error_Status )                                                            
            STOP                                                                                        
          END IF                                                                                        
          no = TauCoeff%Order_Index(0, j, l, k)  ! the polynomial order                                 
          n_Coeffs = n_Coeffs + (no+1)*(np+1)                                                           
        END IF                                                                                          
      END DO                                                                                            
    END DO                                                                                              

    ! Allocate the ODAS structure                           
    Error_Status = Allocate_ODAS( TauCoeff%n_Predictors, &  
                                  TauCoeff%n_Absorbers , &  
                                  TauCoeff%n_Channels  , &  
                                  n_Alphas             , &  
                                  n_Coeffs             , &  
                                  ODAS                   )  
    IF ( Error_Status /= SUCCESS ) THEN                     
      CALL Display_Message( PROGRAM_NAME, &                 
                            'ODAS allocation failed', &     
                            Error_Status )                  
      STOP                                                  
    END IF                                                  

    ODAS%Pos_Index = -9                                                                
    ODAS%Order     = -9                                                                
    ODAS%Pos_Index = -9                                                                
    ODAS%Pre_Index = -9                                                                
    ODAS%Pre_Index(0,:,:) = -9                                                         
    ps = 1                                                                             
    DO l = 1, TauCoeff%n_Channels                                                      
      DO j = 1, TauCoeff%n_Absorbers                                                   
        np = TauCoeff%Predictor_Index(0, j, l, k)                                      
        IF( np > 0 )THEN                                                               
          ODAS%Order(j, l) = TauCoeff%Order_Index(0, j, l, k)  ! the polynomial order  
          ODAS%Pos_Index(j, l) = ps                                                    
          ODAS%Pre_Index(0, j, l) = np                                                 
          ODAS%Pre_Index(1:np, j, l) = TauCoeff%Predictor_Index(1:np, j, l, k)         
          no = ODAS%Order(j, l)                                                        
          m = 0                                                                        
          DO ip = 0, np                                                                
            DO io = 0, no                                                              
              ODAS%C(ps+m) = TauCoeff%C(io, ip, j, l, k)                               
              m = m + 1                                                                
            END DO                                                                     
          END DO                                                                       
          ps = ps + (no+1)*(np+1)                                                      
        END IF                                                                         
      END DO                                                                           
    END DO                                                                             

    DO j = 1, TauCoeff%n_Absorbers                                    
      ODAS%Max_Order(j) = MAXVAL(ODAS%Order(j, :))                    
    END DO                                                            
    ODAS%Alpha(1, :)      = TauCoeff%Alpha                            
    ODAS%Alpha(2, :)      = TauCoeff%Alpha_C1                         
    ODAS%Alpha(3, :)      = TauCoeff%Alpha_C2                         

    ! Copy over the other data                                        
    ! ------------------                                              
    ODAS%Release          = TauCoeff%RELEASE + 1 !TauCoeff%Release + 1        
    ODAS%Version          = TauCoeff%Version                          

    ODAS%Sensor_Id        = Sensor_Id                                 
    ODAS%WMO_Satellite_ID = TauCoeff%WMO_Satellite_ID(1)              
    ODAS%WMO_Sensor_ID    = TauCoeff%WMO_Sensor_ID(1)                 
    ODAS%Sensor_Type      = Sensor_Type                               
    ODAS%Sensor_Channel   = TauCoeff%Sensor_Channel                   
    ODAS%Absorber_ID      = TauCoeff%Absorber_ID                      

    Error_Status = Assign_ODAS( ODAS, &                               
                                ODSSU%ODAS(k))                        
                                                                      
    IF ( Error_Status /= SUCCESS ) THEN                               
      CALL Display_Message( PROGRAM_NAME, &                           
                            'Error assigning ODAS to ODSSU%ODAS ', &  
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


  ! Clean up
  ! --------
  Error_Status = Destroy_TauCoeff_ssu( TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff_ssu structure', &
                          WARNING )
  END IF
  
  Error_Status = Destroy_ODAS( ODAS )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODSSU( ODSSU )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODSSU structure', &
                          WARNING )
  END IF

END PROGRAM TauCoeff2ODSSU
