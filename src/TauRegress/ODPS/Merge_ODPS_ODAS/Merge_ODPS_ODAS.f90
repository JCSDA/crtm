PROGRAM Merge_ODPS_ODAS

  USE Type_Kinds
  USE ODPS_Define
  USE ODPS_netCDF_IO
  USE File_Utility
  USE Message_Handler
  USE TauCoeff_Define
  USE TauCoeff_netCDF_IO, ONLY : Read_OPTRAN_netCDF => Read_TauCoeff_netCDF 
  USE ODPS_Predictor, ONLY : Get_Component_ID, &
                             WLO_ComID, WET_ComID, &
                             GROUP_1, GROUP_2, GROUP_3
                             
  
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Merge_ODPS_ODAS'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  
  REAL(fp), PARAMETER :: SIGNIFICANT_LEVEL = 0.1_fp
  INTEGER(Long)  :: n_coeffs
  INTEGER        :: i, j, l, m, n_orders, n_predictors, n_channels
  INTEGER        :: fid, Error_Status, Allocate_Status
  CHARACTER(256) :: ODPS_file, ODPS_errfile, OPTRAN_file, OPTRAN_errfile, file_out
  TYPE(ODPS_type):: ODPS
  TYPE(TauCoeff_type)    :: OPTRAN
  REAL(fp), ALLOCATABLE  :: err_ODPS(:), err_optran(:)

  CHARACTER(80)     :: ID_Tag   
  CHARACTER(256)    :: Title    
  CHARACTER(3000)   :: History  
  CHARACTER(3000)   :: Comment  
  
  WRITE(*, *)'Enter ODPS file name:'
  READ(*, '(A)')ODPS_file
  WRITE(*, *)'Enter ODPS fitting error file name:'
  READ(*, '(A)')ODPS_errfile
  WRITE(*, *)'Enter OPTRAN file name (netCDF file):'
  READ(*, '(A)')OPTRAN_file
  WRITE(*, *)'Enter OPTRAN fitting error file name:'
  READ(*, '(A)')OPTRAN_errfile
  WRITE(*, *)'Enter Output data file name:'
  READ(*, '(A)')file_out

  Error_Status = Read_ODPS_netCDF(ODPS_file, &
                                  ODPS,      &
                                  Title=Title,             &  
                                  History=History,         &         
                                  Comment=Comment,         &         
                                  Profile_Set_ID =ID_Tag )          

  IF(Error_Status /= SUCCESS)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: cannot read ODPS data from file "//TRIM(ODPS_file), &           
                          Error_Status)          
    STOP
  END IF
  
  Error_Status = Read_OPTRAN_netCDF(OPTRAN_file, &
                                    OPTRAN)
  IF(Error_Status /= SUCCESS)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: cannot read OPTRAN data from file "//TRIM(OPTRAN_file), &           
                          Error_Status)          
    STOP
  END IF

  IF(ODPS%n_Channels /= OPTRAN%n_Channels)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: the channel dimensions in ODPS and OPTRAN structures are different", &           
                          Error_Status)          
    STOP
  END IF
  n_Channels = ODPS%n_Channels
  ALLOCATE(err_ODPS(n_Channels), &
           err_OPTRAN(n_Channels), &
           STAT=Allocate_Status)
  IF(Allocate_Status /= 0)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: can not allocate memory for err_ODPS and err_OPTRAN", &           
                          FAILURE)          
    STOP
  END IF
           
  fid = Get_Lun()                                   
  OPEN(fid, FILE=ODPS_errfile, STATUS='OLD')
  DO l = 1, ODPS%n_Channels
    READ(fid, '(43x, e12.5)')err_ODPS(l)
  END DO
  CLOSE(fid)

  OPEN(fid, FILE=OPTRAN_errfile, STATUS='OLD')
  DO l = 1, OPTRAN%n_Channels
    READ(fid, '(58x, f8.5)')err_OPTRAN(l)
  END DO
  CLOSE(fid)
  
  n_coeffs = 0
  DO l = 1, OPTRAN%n_Channels
    n_orders      = OPTRAN%Order_Index(0, 1, l)
    n_predictors  = OPTRAN%Predictor_Index(0, 1, l)
    n_coeffs      = n_coeffs + (n_orders+1)*(n_predictors+1)
  END DO 
 
  Error_Status = Allocate_ODPS_OPTRAN(n_coeffs, &
                                      ODPS)

  IF(Error_Status /= SUCCESS)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: can not allocate ODPS OPTRAN data arrays ", &           
                          Error_Status)          
    STOP
  END IF

  ! Save the component index for OPTRAN
  DO j = 1, ODPS%n_Components
    IF(ODPS%Group_index == GROUP_1 .OR. ODPS%Group_index == GROUP_2)THEN
      IF(Get_Component_ID(j, ODPS%Group_index) == WLO_ComID)THEN
        ODPS%OComponent_Index = j
      END IF
    ELSE IF(ODPS%Group_index == GROUP_3)THEN
      IF(Get_Component_ID(j, ODPS%Group_index) == WET_ComID)THEN
        ODPS%OComponent_Index = j
      END IF    
    ELSE
      CALL Display_Message( PROGRAM_NAME,    &           
                            "Error: can not find a match for Group Index", &         
                            Error_Status)          
      STOP
    END IF
  END DO
        
  ODPS%Alpha    = OPTRAN%Alpha(1)
  ODPS%Alpha_C1 = OPTRAN%Alpha_C1(1)
  ODPS%Alpha_C2 = OPTRAN%Alpha_C2(1)
  n_coeffs = 0
  DO l = 1, OPTRAN%n_Channels
    n_orders = OPTRAN%Order_Index(0, 1, l)
    n_predictors        = OPTRAN%Predictor_Index(0, 1, l)
    ODPS%Order(l)       = n_orders
    ODPS%OP_Index(0, l) = n_predictors
    DO i = 1, n_predictors
      ODPS%OP_Index(i, l) = OPTRAN%Predictor_Index(i, 1, l)   
    END DO
    ODPS%OPos_Index(l) = n_coeffs+1
    m = 0
    DO i = 0, n_predictors
    DO j = 0, n_orders
      m = m + 1
      ODPS%OC(n_coeffs+m) = OPTRAN%C(j, i, 1, l)
    END DO
    END DO
    n_coeffs = n_coeffs + m
    
    IF(err_OPTRAN(l) < err_ODPS(l) .OR. &
       err_OPTRAN(l) < SIGNIFICANT_LEVEL)THEN
       ODPS%OSignificance(l) = SIGNIFICANCE_OPTRAN
    ELSE
       ODPS%OSignificance(l) = 0
    END IF
  END DO

  Error_Status = Write_ODPS_netCDF(file_out, &
                                   ODPS,     &
                                   Title         = TRIM(Title)  , &       
                                   History       = TRIM(PROGRAM_RCS_ID)//'  '//TRIM(History), &       
                                   Comment       = TRIM(Comment), &       
                                   Profile_Set_ID= TRIM(ID_Tag) )         
  IF(Error_Status /= SUCCESS)THEN
    CALL Display_Message( PROGRAM_NAME,    &           
                          "Error: cannot write ODPS to the file "//TRIM(file_out), &           
                          Error_Status)          
    STOP
  END IF

END PROGRAM Merge_ODPS_ODAS
