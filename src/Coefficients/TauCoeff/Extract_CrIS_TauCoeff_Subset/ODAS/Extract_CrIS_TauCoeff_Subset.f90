!
! Extract_CrIS_TauCoeff_Subset
!
! Program to extract the CrIS channel subset from the individual
! CrIS band netCDF format TauCoeff data files.
!
!
! CREATION HISTORY:
!       Written by:     David Groff, 10-Jun-2011
!                       david.groff@noaa.gov
!

PROGRAM Extract_CrIS_TauCoeff_Subset

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, &
                                   Display_Message, Program_Message
  ! -----------------------------------------------------
  !***********May need to be used for testing************
  
  !USE Compare_Float_Numbers, ONLY: Compare_Float
  
  
  USE List_File_Utility,     ONLY: Integer_List_File_type, &
                                   Read_List_File, &
                                   Get_List_Size, &
                                   Get_List_Entry
  USE ODAS_Define,           ONLY: Allocate_ODAS, &
                                   ODAS_type, &
                                   Destroy_ODAS
  USE ODAS_netCDF_IO,        ONLY: Inquire_ODAS_netCDF, &
                                   Read_ODAS_netCDF, &
                                   Write_ODAS_netCDF
  USE SensorInfo_Parameters, ONLY: INFRARED_SENSOR
  
  ! ------------------------------------------------------
  ! **************Need to add ODPS------------------------
  !USE ODPS_Define
  !USE ODPS_netCDF_IO
  
  USE Subset_Define,         ONLY: Subset_type, &
                                   Subset_Destroy, &
                                   Subset_GetValue
  USE CrIS_Define,           ONLY: N_CRIS_BANDS, &
                                   N_CRIS_CHANNELS, &
                                   CrIS_BandName
  USE CrIS_Subset,           ONLY: N_CRIS_SUBSET_374, CRIS_SUBSET_374, CRIS_SUBSET_374_COMMENT, &
                                   N_CRIS_SUBSET_399, CRIS_SUBSET_399, CRIS_SUBSET_399_COMMENT, &
                                   CrIS_Subset_Index
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Extract_CrIS_TauCoeff_Subset'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  INTEGER,      PARAMETER :: N_VALID_SUBSETS = 4
  
  ! ------------------------------------------------------------------
  ! ********Need to add ODPS******************************************
  ! INTEGER,      PARAMETER :: N_VALID_ALGORITHMS = 2
  ! CHARACTER(*), PARAMETER :: VALID_ALGORITHM_NAME(N_VALID_ALGORITHMS) = &
  !  (/ 'ODAS', &
  !     'ODPS' /)
  
  
  CHARACTER(*), PARAMETER :: VALID_SUBSET_NAME(N_VALID_SUBSETS) = &
    (/ 'NESDIS 374 channel set', &
       'NESDIS 399 channel set', &
       'All channels          ', &
       'User specified        ' /)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: Message
  CHARACTER(256)  :: List_Filename
  CHARACTER(256)  :: In_Filename
  CHARACTER(256)  :: Out_Filename
  CHARACTER(256)  :: Inquire_File
  
  ! ******************************
  ! Previously used to write files
  ! ******************************  
!  CHARACTER(256)  :: Sensor_Name
!  CHARACTER(256)  :: Platform_Name
!  CHARACTER(256)  :: ID_Tag
  CHARACTER(2000) :: Profile_Set_ID
  CHARACTER(2000) :: Title
  CHARACTER(5000) :: History
  CHARACTER(5000) :: Comment
  CHARACTER(256)  :: Subset_Comment
  CHARACTER(20)   :: Sensor_ID
  INTEGER :: WMO_Satellite_ID
  INTEGER :: WMO_Sensor_ID
  INTEGER :: i, Set
  
  ! ********ODPS will be added **********
  INTEGER :: Algorithm
  INTEGER :: n_Orders
  INTEGER :: n_Predictors     
  INTEGER :: n_Absorbers
  INTEGER :: n_Alphas
  INTEGER :: Poly_Order  
  INTEGER :: C1_Input_Index, C2_Input_Index
  INTEGER :: C1_Output_Index, C2_Output_Index
  INTEGER :: n_Absorber_Coeffs, n_Channel_Coeffs, n_Output_Coeffs    
  INTEGER :: Pos_Index1
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: Allocate_Status
  INTEGER :: j, l, a, b, l1, l2
  INTEGER :: n_Subset_Channels
  INTEGER :: lcounter
  INTEGER :: n_values
  INTEGER, ALLOCATABLE :: index(:), number(:)
  INTEGER, ALLOCATABLE :: Subset_List(:)
  TYPE(Integer_List_File_type) :: User_Subset_List
  TYPE(ODAS_type) :: In_TauCoeff, Out_TauCoeff
  TYPE(Subset_type) :: Subset

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to extract the CrIS channel SUBSET transmittance '//&
                       'coefficient data from the individual band netCDF '//&
                       'TauCoeff files and write them to a separate netCDF '//&
                       'datafile.', &
                       '$Revision$' )

  ! Prompt user to select a subset set                   
  WRITE( *,'(/5x,"Select an CrIS channel subset")' )     
  DO i = 1, N_VALID_SUBSETS                              
    WRITE( *,'(10x,i1,") ",a)' ) i, VALID_SUBSET_NAME(i) 
  END DO                                                 
  WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )    
  READ( *,FMT='(i5)',IOSTAT=IO_Status ) Set
  
  ! ********MULTIPLE ALGORITHM CODE**************
  ! Prompt user to select an algorithm 
!  WRITE( *,'(/5x,"Select an Algorithm")' )
!  DO i = 1, N_VALID_ALGORITHMS
!    WRITE(*,'(10x,i1,") ",a)' ) i, VALID_ALGORITHM_NAME(i)
!  END DO
!  WRITE( *,FMT='(5x,"Enter choice: ")',ADVANCE='NO' )    
!  READ( *,FMT='(i5)',IOSTAT=IO_Status ) Algorithm
    
  ! Check for I/O errors
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input', &
                          FAILURE )
    STOP
  END IF
    
  ! Check the input
  IF ( Set < 1 .OR. Set > N_VALID_SUBSETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid selection', &
                          FAILURE )
    STOP
  END IF

  ! Get the required channels list
  ! ------------------------------
  SELECT CASE ( Set )

    ! The 374 channel subset
    ! ----------------------
    CASE (1)
    
      ! Assign values
      n_Subset_Channels = N_CRIS_SUBSET_374
      Subset_Comment    = 'Data extracted from the individual CRIS band ODAS datafiles.; '//&
                          CRIS_SUBSET_374_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = CRIS_SUBSET_374
      Sensor_ID   = 'cris374_npp'

    ! The 399 channel subset
    ! ----------------------
    CASE (2)
    
      ! Assign values
      n_Subset_Channels = N_CRIS_SUBSET_399
      Subset_Comment    = 'Data extracted from the individual CRIS band ODAS datafiles.; '//&
                          CRIS_SUBSET_374_COMMENT

      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = CRIS_SUBSET_399
      Sensor_ID   = 'cris399_npp'


    ! All the channels
    ! ----------------
    CASE (3)
    
      ! Assign values
      n_Subset_Channels = N_CRIS_CHANNELS
      Subset_Comment    = 'CrIS full channel set'
      
      ! Allocate list array
      ALLOCATE( Subset_List( n_Subset_Channels ), &
                STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message,'("Error allocating Subset_List array. STAT = ",i0)' ) Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill values
      Subset_List = (/(l,l=1,N_CRIS_CHANNELS)/)
      Sensor_ID   = 'cris_npp'

    ! A user specified channel subset
    ! -------------------------------
    CASE (4)
    
      ! Get a channel subset list filename
      WRITE( *, FMT='(/5x,"Enter an CrIS channel subset list filename : ")', &
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
      IF ( n_Subset_Channels < 1 .OR. n_Subset_Channels > N_CRIS_CHANNELS ) THEN
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
                              TRIM(Message), &
                              FAILURE )
        STOP
      END IF

      ! Fill the subset list
      DO l = 1, n_Subset_Channels
        Error_Status = Get_List_Entry( User_Subset_List, l, Subset_List(l) )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message,'("Error retrieving user subset channel list entry ",i4)' ) l
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM(Message), &
                                Error_Status )
          STOP
        END IF
      END DO

      ! Create the subset comment and sensor id
      WRITE( Subset_Comment,'("User specified CrIS ",i0," channel SUBSET")' ) n_Subset_Channels
      WRITE( Sensor_ID,'("cris",i0,"_npp")' ) n_Subset_Channels
  
  END SELECT
  
  ! Arbitrarily set the inquire file to the
  ! filename for band 1. This file is used
  ! in this program to get the band independent
  ! dimensions and metadata for the output
  ! subset file
  Inquire_File = 'crisB1_npp.TauCoeff.nc'
   
  ! -----------------------------------------
  ! Get the band independent dimensions and
  ! the global attributes for the output file
  ! -----------------------------------------                                                                   
  Error_Status = Inquire_ODAS_netCDF( TRIM(Inquire_File)                      , &                    
                                      n_Predictors     = n_Predictors         , &                   
                                      n_Absorbers      = n_Absorbers          , &                   
                                      n_Alphas         = n_Alphas             , &
                                      WMO_Sensor_ID    = WMO_Sensor_ID        , &
                                      WMO_Satellite_ID = WMO_Satellite_ID     , &
                                      Release          = Out_TauCoeff%Release , &
                                      Version          = Out_TauCoeff%Version , &
                                      Title            = Title                , &
                                      History          = History              , &
                                      Profile_Set_Id   = Profile_Set_Id       , &                                      
                                      Comment          = Comment                )                                         
  IF ( Error_Status /= SUCCESS ) THEN                                                      
    CALL Display_Message( PROGRAM_NAME, &                                                  
                          'Error inquiring the input netCDF ODAS TauCoeff file '//&
                          TRIM(In_Filename), &        
                          Error_Status )                                             
    STOP                                                   
  END IF                                                                  
    
  n_Output_Coeffs = 0
    
  ! --------------------------
  ! Get the n_Coeffs dimension  
  ! --------------------------
  First_Band_Loop: DO b = 1, N_CRIS_BANDS
  
    ! Current band 
    Error_Status = CrIS_Subset_Index( b, Subset_List, Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for band '//&
                            TRIM(CrIS_BandName(l)), &
                            Error_Status )
      STOP
    END IF
    CALL Subset_GetValue( Subset, n_Values = n_values, Index = index )
    
    
    ! Set the band filename to be read
    In_Filename = 'cris'//TRIM(CrIS_BandName(b))//'_npp.TauCoeff.nc'
      
    ! ---------------------------
    ! Read the data for the band 
    ! to get the Pos_Index field
    ! --------------------------
    Error_Status = Read_ODAS_netCDF( In_Filename, &
                                     In_TauCoeff  )
    IF ( Error_Status /= SUCCESS ) THEN                                    
      CALL Display_Message( PROGRAM_NAME, &                                
                            'Error reading netCDF CrIS TauCoeff file '//&  
                            TRIM(In_Filename), &                           
                            Error_Status )                                 
      STOP                                                                 
    END IF 

    First_Channel_Loop: DO l = 1, n_values
      
      n_Channel_Coeffs = 0
    
      First_Absorber_Loop: DO j = 1, n_Absorbers
    
        IF ( In_TauCoeff%Order(j,index(l)) == -9 ) THEN
          Poly_Order = 0
        ELSE 
          Poly_Order = In_TauCoeff%Order(j,index(l))
        END IF
        
        n_Channel_Coeffs = n_Channel_Coeffs + (n_Predictors+1)*(Poly_Order+1)
      
      END DO First_Absorber_Loop 
                         
      n_Output_Coeffs = n_Output_Coeffs + n_Channel_Coeffs
        
    END DO First_Channel_Loop
  
  END DO First_Band_Loop

  
  ! Allocate the output structure
  Error_Status = Allocate_ODAS( n_Predictors      , &
                                n_Absorbers       , &
                                n_Subset_Channels , &
                                n_Alphas          , &
                                n_Output_Coeffs   , &
                                Out_TauCoeff        )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating the output ODAS data structure.', &
                          Error_Status )
    STOP
  END IF
  
  ! Fill the remaining scalar fields
  Out_TauCoeff%Sensor_Id        = Sensor_ID 
  Out_TauCoeff%Sensor_Type      = INFRARED_SENSOR
  Out_TauCoeff%WMO_Satellite_ID = WMO_Satellite_ID
  Out_TauCoeff%WMO_Sensor_ID    = WMO_Sensor_ID
  
  ! Assign the Sensor Channel field
  Out_TauCoeff%Sensor_Channel = Subset_List
  
  ! --------------------------------------------
  ! Fill the band and channel independent arrays
  ! --------------------------------------------
  Error_Status = Read_ODAS_netCDF( TRIM(Inquire_File), &
                                   In_TauCoeff  )
  IF ( Error_Status /= SUCCESS ) THEN                                    
    CALL Display_Message( PROGRAM_NAME, &                                
                          'Error reading netCDF CrIS TauCoeff file '//&  
                          TRIM(In_Filename), &                           
                          Error_Status )                                 
    STOP                                                                 
  END IF
  
  Out_TauCoeff%Absorber_ID = In_TauCoeff%Absorber_ID
  Out_TauCoeff%Max_Order   = In_TauCoeff%Max_Order
  Out_TauCoeff%Alpha       = In_TauCoeff%Alpha
    
  ! Initialise the begin index for the channels
  ! and initialize the current position index
  ! -------------------------------------------
  l1 = 1
  C1_Output_Index = 1
  Pos_Index1 = 1
  lcounter = 0
  ! Loop over bands to assign band 
  ! and channel dependent fields
  ! to the output structure
  ! ------------------------------
  Second_Band_Loop: DO b = 1, N_CRIS_BANDS

    ! Current band channel subset
    ! ---------------------------
    ! Determine the subset channel indices
    ! for the current band
    Error_Status = CrIS_Subset_Index( b, Subset_List, Subset )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error extracting subset channel indices for band '//&
                            TRIM(CrIS_BandName(b)), &
                            Error_Status )
      STOP
    END IF
    CALL Subset_GetValue( Subset, n_Values = n_values, Index = index, Number = number )

    ! Output the number of channels to extract
    WRITE( *,'(/10x,"There are ",i0," channels to be extracted from band ",a,":")' ) &
             n_values, TRIM(CrIS_BandName(b))

    ! Read the input TauCoeff file if required
    ! ----------------------------------------
    Non_Zero_n_Channels: IF ( n_values > 0 ) THEN
    
      ! Output the list of channel numbers to extract
      WRITE( *,'(10x,10i5)' ) number

      ! Define the filename
      In_Filename = 'cris'//TRIM(CrIS_BandName(b))//'_npp.TauCoeff.nc'
      
      ! ********multiple algorithm stuff********************
      !SELECT CASE ( Algorithm )
      !  CASE (1) 
          
      ! Read the current band data                                                                         
      Error_Status = Read_ODAS_netCDF( TRIM(In_Filename), &                                                
                                       In_TauCoeff        )                                                
      IF ( Error_Status /= SUCCESS ) THEN                                                                  
        CALL Display_Message( PROGRAM_NAME, &                                                              
                              'Error reading the ODAS TauCoeff data structure.', &                         
                              Error_Status )                                                               
        STOP                                                                                               
      END IF                                                                                               
                                                                                                           
      ! Copy the required channel's data                                                                   
      ! --------------------------------                                                                   
      l2 = l1 + n_values - 1                                                                        
      Out_TauCoeff%Order(:,l1:l2)       = In_TauCoeff%Order(:,index)                                
      Out_TauCoeff%Pre_Index(:,:,l1:l2) = In_TauCoeff%Pre_Index(:,:,index)                          
      Second_Channel_Loop: DO l = 1, n_values                                                       
                                                                                                           
        lcounter = lcounter + 1                                                                            
                                                                                                           
        IF ( ALL(In_TauCoeff%Pos_Index(:,index(l)) > 0) ) THEN                                      
          Out_TauCoeff%Pos_Index(1,lcounter) = Pos_Index1                                                  
          Out_TauCoeff%Pos_Index(2,lcounter) = Pos_Index1 + &                                              
            ( In_TauCoeff%Pos_Index(2,index(l)) - In_TauCoeff%Pos_Index(1,index(l)) )        
          Out_TauCoeff%Pos_Index(3,lcounter) = Pos_Index1 + &                                              
            ( In_TauCoeff%Pos_Index(3,index(l)) - In_TauCoeff%Pos_Index(1,index(l)) )        
        END IF                                                                                             
                                                                                                           
        IF (  In_TauCoeff%Pos_Index(3,index(l)) == -9 .AND. &                                       
              ALL(In_TauCoeff%Pos_Index(1:2,index(l)) > 0) ) THEN                                   
          Out_TauCoeff%Pos_Index(3,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(1,lcounter) = Pos_Index1                                                  
          Out_TauCoeff%Pos_Index(2,lcounter) = Pos_Index1 + &                                              
            ( In_TauCoeff%Pos_Index(2,index(l)) - In_TauCoeff%Pos_Index(1,index(l)) )        
        END IF                                                                                             
                                                                                                           
        IF ( In_TauCoeff%Pos_Index(2,index(l)) == -9 .AND. &                                        
             ALL(In_TauCoeff%Pos_Index(1:3:2,index(l)) > 0) ) THEN                                  
          Out_TauCoeff%Pos_Index(2,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(1,lcounter) = Pos_Index1                                                  
          Out_TauCoeff%Pos_Index(3,lcounter) = Pos_Index1 + &                                              
            ( In_TauCoeff%Pos_Index(3,index(l)) - In_TauCoeff%Pos_Index(1,index(l)) )        
        END IF                                                                                             
                                                                                                           
        IF ( In_TauCoeff%Pos_Index(1,index(l)) == -9 .AND. &                                        
             ALL(In_TauCoeff%Pos_Index(2:3,index(l)) > 0) ) THEN                                    
          Out_TauCoeff%Pos_Index(1,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(2,lcounter) = Pos_Index1                                                  
          Out_TauCoeff%Pos_Index(3,lcounter) = Pos_Index1 + &                                              
            ( In_TauCoeff%Pos_Index(3,index(l)) - In_TauCoeff%Pos_Index(2,index(l)) )        
        END IF                                                                                             
                                                                                                           
        IF ( In_TauCoeff%Pos_Index(1,index(l)) > 0 .AND. &                                          
             ALL(In_TauCoeff%Pos_Index(2:3,index(l)) == -9) ) THEN                                  
          Out_TauCoeff%Pos_Index(2,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(3,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(1,lcounter) = Pos_Index1                                                  
        END IF                                                                                             

        IF ( In_TauCoeff%Pos_Index(2,index(l)) > 0 .AND. &                                          
             ALL(In_TauCoeff%Pos_Index(1:3:2,index(l)) == -9) ) THEN                                
          Out_TauCoeff%Pos_Index(1,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(3,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(2,lcounter) = Pos_Index1                                                  
        END IF                                                                                             
                                                                                                           
        IF ( In_TauCoeff%Pos_Index(3,index(l)) > 0 .AND. &                                          
             ALL(In_TauCoeff%Pos_Index(1:2,index(l)) == -9) ) THEN                                  
          Out_TauCoeff%Pos_Index(1,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(2,lcounter) = -9                                                          
          Out_TauCoeff%Pos_Index(3,lcounter) = Pos_Index1                                                  
        END IF                                                                                             

        Pos_Index1 = MAXVAL(Out_TauCoeff%Pos_Index(:,lcounter)) + 1                                        

        C1_Input_Index = MINVAL(In_TauCoeff%Pos_Index(:,index(l)), &                                
                                mask=In_TauCoeff%Pos_Index(:,index(l)) .GT. 0)                      
        IF ( In_TauCoeff%n_Channels > index(l) ) THEN                                               
          C2_Input_Index = MINVAL(In_TauCoeff%Pos_Index(:,index(l)+1), &                            
                                  mask=In_TauCoeff%Pos_Index(:,index(l)+1) .GT. 0) - 1              
        ELSE                                                                                               
          C2_Input_Index = SIZE(In_TauCoeff%C)                                                             
        END IF                                                                                             

        n_Channel_Coeffs = C2_Input_Index - C1_Input_Index                                                 
        C2_Output_Index = C1_Output_Index + n_Channel_Coeffs                                               
                                                                                                           
        Out_TauCoeff%C(C1_Output_Index:C2_Output_Index) = In_TauCoeff%C(C1_Input_Index:C2_Input_Index)     
        C1_Output_Index = C2_Output_Index + 1                                                              
                                                                                                           
      END DO Second_Channel_Loop                                                                        
         
      l1 = l2 + 1                                                                                       
                                                                                                           
      ! Destroy the input TauCoeff structure                                                            
      ! for the next band read                                                                          
      ! ------------------------------------                                                            
      Error_Status = Destroy_ODAS( In_TauCoeff )                                                        
      IF ( Error_Status /= SUCCESS ) THEN                                                               
        CALL Display_Message( PROGRAM_NAME, &                                                           
                              'Error destroying TauCoeff structure for input from '//&                   
                              TRIM(In_Filename), &                                                       
                              Error_Status )                                                             
        STOP                                                                                            
      END IF                                                                                            
          
    END IF Non_Zero_n_Channels

    ! Destroy the CrIS channel subset structure
    ! -----------------------------------------
    CALL Subset_Destroy( Subset )

  END DO Second_Band_Loop

  ! Write the output SpcCoeff file
  ! ------------------------------
  ! The output filename
  Out_Filename = TRIM(Sensor_ID)//'.TauCoeff.nc'

  Error_Status = Write_ODAS_netCDF( Out_Filename , &
                                    Out_TauCoeff , &
                                    Title          = TRIM(Title) , &                                    
                                    History        = PROGRAM_VERSION_ID//&
                                                     TRIM(History) , &
                                    Comment        = TRIM(Subset_Comment)//&
                                                     '; '//TRIM(Comment) , &
                                    Profile_Set_Id = TRIM(Profile_Set_Id) )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the CrIS TauCoeff file '//&
                          TRIM(Out_Filename), &
                          FAILURE )
    STOP
  END IF                                  

  ! Destroy the output TauCoeff structure
  ! -------------------------------------
  Error_Status = Destroy_ODAS( Out_TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure for output to '//&
                          TRIM(Out_Filename), &
                          Error_Status )
  END IF

END PROGRAM Extract_CrIS_TauCoeff_Subset
