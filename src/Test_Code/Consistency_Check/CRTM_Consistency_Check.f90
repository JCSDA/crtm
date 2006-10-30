PROGRAM CRTM_Consistency_Check

  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  ! -- CRTM module
  USE CRTM_Module
  USE CRTM_Atmosphere_Define

  ! -- Modules to read in Atmosphere and Surface data
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Surface_Binary_IO

  USE FWDTLMtest_Define
  USE FWDTLMtest_netCDF_IO

  USE TLADMtest_Define
  USE TLADMtest_netCDF_IO

  USE ADKMtest_Define
  USE ADKMtest_netCDF_IO

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_CRTM_TL_AD_K'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: CRTM_Consistency_Check.f90,v 1.2 2006/05/02 14:58:35 dgroff Exp $'

  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  INTEGER, PARAMETER :: N_ABSORBERS = 2

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  REAL( fp_kind ), PARAMETER :: MIN_PERTURBATION = -0.1_fp_kind   ! +/- 10%
  REAL( fp_kind ), PARAMETER :: MAX_PERTURBATION =  0.1_fp_kind   ! +/- 10%
  INTEGER,         PARAMETER :: N_PERTURBATIONS  = 51

  INTEGER, PARAMETER :: N_LAYER_VARIABLES = 3
  INTEGER, PARAMETER :: N_SURFACE_VARIABLES = 0
  INTEGER, PARAMETER :: N_VARIABLES = N_LAYER_VARIABLES + N_SURFACE_VARIABLES
!  INTEGER, PARAMETER :: NV_LEVEL_PRESSURE       = 1
!  INTEGER, PARAMETER :: NV_LAYER_PRESSURE       = 2
  INTEGER, PARAMETER :: NV_LAYER_TEMPERATURE    = 1
  INTEGER, PARAMETER :: NV_LAYER_WATER_VAPOR    = 2
  INTEGER, PARAMETER :: NV_LAYER_OZONE          = 3
  INTEGER, PARAMETER :: NV_SURFACE_TEMPERATURE  = 6
  INTEGER, PARAMETER :: NV_SURFACE_EMISSIVITY   = 7
  INTEGER, PARAMETER :: NV_SURFACE_REFLECTIVITY = 8
  INTEGER, PARAMETER :: NV_SOLAR_REFLECTIVITY   = 9
  CHARACTER( * ), PARAMETER, DIMENSION( N_VARIABLES ) :: &
    VARIABLE_NAME = (/ 'Layer temperature   ', &
                       'Layer water vapor   ', &
                       'Layer ozone         ' /)

  CHARACTER( * ), PARAMETER :: AerosolCoeff_File = 'dummy.AerosolCoeff.bin.Big_Endian'
  CHARACTER( * ), PARAMETER :: CloudCoeff_File = 'Cloud_scatter_IR_MW.bin.Big_Endian'
  CHARACTER( * ), PARAMETER :: EmisCoeff_File = 'EmisCoeff.bin.Big_Endian'

  CHARACTER( * ), PARAMETER :: atmosphere_filename = 'atmProfile_umbc.txt'
  CHARACTER( * ), PARAMETER :: ID_Tag = atmosphere_filename

  INTEGER, PARAMETER :: FWDTLMtest = 1, TLADMtest = 2, ADKMtest = 3
  INTEGER, PARAMETER :: H2O_idx = 1, O3_idx = 2

  CHARACTER( * ), PARAMETER :: COMMENTE = 'Tb TL and AD'

  ! ---------
  ! Variables
  ! ---------
  
  INTEGER :: Test_Type  ! will be set to FWDTLMtest, TLADMtest or ADKMtest

  TYPE( LayerFWDTLMtest_type ) :: FWDTLMtestResult
  TYPE( LayerTLADMtest_type )  :: TLADMtestResult
  TYPE( LayerADKMtest_type )   :: ADKMtestResult

  CHARACTER( 256 ) :: Message

  CHARACTER( 256 ) :: Sensor_Descriptor
  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: Out_filename

  TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo

  TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo

  TYPE( CRTM_Surface_type )      :: Surface

  TYPE( CRTM_Atmosphere_type ), ALLOCATABLE, DIMENSION( : ) :: Atmosphere

  TYPE( CRTM_Atmosphere_type ) :: Atmosphere_TL, Atmosphere_AD
  TYPE( CRTM_Surface_type )    :: Surface_TL, Surface_AD

  TYPE( CRTM_Atmosphere_type ), ALLOCATABLE, DIMENSION( : ) :: Atmosphere_K
  TYPE( CRTM_Surface_type ), ALLOCATABLE, DIMENSION( : )    :: Surface_K

  TYPE( CRTM_RTSolution_type ), ALLOCATABLE, DIMENSION( : ) :: RTSolution, RTSolution_TL, &
                                                               RTSolution_AD, RTSolution_K, &
                                                               RTSolution_FWDBL

  ! -- Perturbation fraction and amount
  REAL( fp_kind ) :: dPerturbation, base_value
  REAL( fp_kind ), DIMENSION( N_PERTURBATIONS ) :: Perturbation_Fraction

  INTEGER :: j, k, l, ll, m, n, nP, nV, n_profiles, n_layers, n_clouds, n_Aerosols, idummy
  INTEGER :: Atm_fileID, Error_Status, Allocate_Status
  INTEGER :: New_File

  ! ---------------------------------------------------------------
  ! Enter the instrument Sensor_Descriptor, e.g. hirs3_n16 and Test Type
  ! ---------------------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the Sensor_Descriptor : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Sensor_Descriptor
  Sensor_Descriptor = ADJUSTL( Sensor_Descriptor )

  WRITE( *, FMT = '( /5x, "Enter the Test Type(1 - FWDTLtest; 2 - TLADtest; 3 - ADKtest) : " )', &
            ADVANCE = 'NO' )
  READ( *, '( i5 )' ) test_type


  !#----------------------------------------------------------------------------#
  !#         -- READ THE Atmosphere AND Surface STRUCTURE DATA FILES --         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading Amosphere file..." )' )

  Atm_fileID = Get_Lun()
  OPEN(Atm_fileID,file=atmosphere_filename,status='old', IOSTAT=Error_Status)
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error opening Atmosphere file '//&
                           atmosphere_filename, & 
                           Error_Status )
   STOP
  END IF

  read(Atm_fileID, *) n_profiles, n_layers
  n_clouds = 0
  n_Aerosols = 0

  ALLOCATE( Atmosphere(n_profiles), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating array of type CRTM_Atmosphere_type ', & 
                            Error_Status)  
   STOP
  END IF
  
  Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &     
                                           N_ABSORBERS, &    
                                           Atmosphere%n_Clouds, &       
                                           Atmosphere%n_Aerosols, &     
                                           Atmosphere)                  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Atmosphere structure ', &
                           Error_Status )
   STOP
  END IF

  read(Atm_fileID, *) Atmosphere(1)%Level_pressure

  DO m = 1, n_profiles
     Atmosphere(m)%n_Layers = n_layers
     Atmosphere(m)%n_Absorbers = N_ABSORBERS
     Atmosphere(m)%n_Clouds = n_Clouds
     Atmosphere(m)%n_Aerosols = n_Aerosols
     Atmosphere(m)%Absorber_ID(H2O_Idx) = H2O_ID
     Atmosphere(m)%Absorber_ID(O3_Idx) = O3_ID
     Atmosphere(m)%Absorber_Units(H2O_Idx) = MASS_MIXING_RATIO_UNITS
     Atmosphere(m)%Absorber_Units(O3_Idx) = VOLUME_MIXING_RATIO_UNITS

     Atmosphere(m)%Level_pressure = Atmosphere(1)%Level_pressure

     read(Atm_fileID, *) idummy                                                
     read(Atm_fileID, *) Atmosphere(m)%Level_temperature                          
     Do k = 1, n_layers                                                        
       read(Atm_fileID, *) Atmosphere(m)%pressure(k), Atmosphere(m)%temperature(k), &
                   Atmosphere(m)%Absorber(k,H2O_idx), Atmosphere(m)%Absorber(k,O3_idx)          
     ENDDO 
  ENDDO                                                                    

  CLOSE( Atm_fileID )

  ! --------------------
  ! Create the filenames
  ! --------------------

  SpcCoeff_File = TRIM( Sensor_Descriptor )//'.Sensor.SpcCoeff.bin.Big_Endian'
  TauCoeff_File = TRIM( Sensor_Descriptor )//'.TauCoeff.bin.Big_Endian'

  !#----------------------------------------------------------------------------#    
  !#                          -- INITIALISE THE CRTM --                         #    
  !#----------------------------------------------------------------------------#    

  WRITE( *, '( /5x, "Initializing the CRTM..." )' )                                  

  Error_Status = CRTM_Init( ChannelInfo, &                                           
                            SpcCoeff_File     = SpcCoeff_File, &                     
                            TauCoeff_File     = TauCoeff_File, &                     
                            AerosolCoeff_File = AerosolCoeff_File, &                 
                            EmisCoeff_File    = EmisCoeff_File, &                    
                            CloudCoeff_File = CloudCoeff_File )                  

  !#----------------------------------------------------------------------------#
  !#         -- Allocate THE Atmosphere TL, AD and K structure  --             #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &     
                                           n_Absorbers, &  
                                           n_Clouds, &     
                                           n_Aerosols, &   
                                           Atmosphere_TL)                
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Atmosphere_TL structure',&
                           Error_Status )
   STOP
  END IF

  Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &     
                                           n_Absorbers, &    
                                           n_Clouds, &       
                                           n_Aerosols, &     
                                           Atmosphere_AD)
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Atmosphere_AD structure ', &
                           Error_Status )
   STOP
  END IF

  ALLOCATE( Atmosphere_K( ChannelInfo%n_channels ), STAT = Allocate_status)

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Atmosphere_K structure ', &
                           Error_Status )
   STOP
  END IF

  ALLOCATE( Surface_K( ChannelInfo%n_channels ), STAT = Allocate_status)
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Surface_K structure ', &
                           Error_Status )
   STOP
  END IF

  Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &     
                                           n_Absorbers, &  
                                           n_Clouds, &     
                                           n_Aerosols, &   
                                           Atmosphere_K)                
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating Atmosphere_K structure ', &
                           Error_Status )
   STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#    -- ALLOCATE THE OUTPUT RTSolution ARRAY TO THE NUMBER OF CHANNELS --    #
  !#----------------------------------------------------------------------------#

  ALLOCATE( RTSolution( ChannelInfo%n_Channels ),RTSolution_TL( ChannelInfo%n_Channels ), &
            RTSolution_AD( ChannelInfo%n_Channels ),RTSolution_K( ChannelInfo%n_Channels ),&
            RTSolution_FWDBL( ChannelInfo%n_Channels ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
   STOP
  END IF

  New_File = SET                                                                 

  !#--------------------------------------------------------------------#
  !#         -- Do things specific to the test type --                  #
  !#--------------------------------------------------------------------#

  SELECT CASE( Test_Type )                                                       
  CASE( FWDTLMtest )                                                              
    out_Filename = TRIM(Sensor_Descriptor)//'.FWDTLMtest.nc'                                              

    Error_Status = Allocate_FWDTLMtest( n_Layers, &                   
                                        ChannelInfo%n_Channels, &                            
                                        N_PERTURBATIONS, &                       
                                        N_LAYER_VARIABLES, &                     
                                        FWDTLMtestResult ) 
    !#-----------------------------------------------------------------------#
    !#       -- COMPUTE THE PERTURBATION ARRAY --                            #
    !#-----------------------------------------------------------------------#

    dPerturbation = ( MAX_PERTURBATION - MIN_PERTURBATION ) / REAL( N_PERTURBATIONS - 1, fp_kind ) 

    nP = 1                                                                                         
    DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2                                                   
      Perturbation_Fraction(nP) = REAL( n, fp_kind ) * dPerturbation                               
      nP = nP + 1                                                                                  
    END DO                                                                                         
                                                                                                   
  CASE( TLADMtest )                                                              
    out_Filename = TRIM(Sensor_Descriptor)//'.TLADMtest.nc'                                               

    Error_Status = Allocate_TLADMtest( n_Layers, &                    
                                       ChannelInfo%n_Channels, &                             
                                       N_LAYER_VARIABLES, &                      
                                       TLADMtestResult )                               
  CASE( ADKMtest )                                                               
    out_Filename = TRIM(Sensor_Descriptor)//'.ADKMtest.nc'                                               
    Error_Status = Allocate_ADKMtest( n_Layers, &                     
                                      ChannelInfo%n_Channels, &                              
                                      N_LAYER_VARIABLES, &                       
                                      ADKMtestResult )                                 
  CASE DEFAULT                                                                   
     WRITE( *, '( 5x, "Unkown Test Type: ", i5 )' ) Test_Type                    
     Stop                                                                        
  END SELECT                                                                     

  IF ( Error_Status /= SUCCESS ) THEN                                            
     CALL Display_Message( PROGRAM_NAME, &                                       
                           'Error occurred allocating test result structure', &    
                            Error_Status )                                       
   STOP                                                                          
  END IF                                                                         


  !#------------------------------------------------------------------------#      
  !#                        -- BEGIN PROFILE LOOP --                        #      
  !#------------------------------------------------------------------------#      

!  Profile_Loop: DO m = 1, n_profiles                                    
  Profile_Loop: DO m = 1, 2                                    


    WRITE( *, '( /10x, "Profile #", i5 )' ) m                                      

    !#------------------------------------------------------------------------#    
    !#               -- Set surface parameters --                             #    
    !#------------------------------------------------------------------------#    

    Surface%Water_Type = SEA_WATER
    Surface%Water_Coverage = 1.0_fp_kind
    Surface%Salinity = 30.0_fp_kind
    Surface%Wind_Speed = 5.0_fp_kind
    Surface%Water_Temperature = Atmosphere(m)%temperature(n_layers)
 
    ! --- set geometry and surface data
    GeometryInfo%Sensor_Zenith_Angle = 50.0_fp_kind  

    !#------------------------------------------------------------------------#    
    !#               -- FORWARD MODEL BASELINE COMPUTATION --                 #    
    !#------------------------------------------------------------------------#    

    Error_Status = CRTM_Forward( Atmosphere(m), &                                     
                                 Surface, &                                        
                                 GeometryInfo, &                                   
                                 ChannelInfo, &                                    
                                 RTSolution_FWDBL )                                      

    IF ( Error_Status /= SUCCESS ) THEN                                            
      CALL Display_Message( PROGRAM_NAME, &                                        
                            'Error calling CRTM_Forward', &                        
                            Error_Status )                                         
      STOP                                                                         
    END IF                                                                         

   
    !---------------------------------------------------------!   
    !  Test cases requiring TL model                          !   
    !---------------------------------------------------------!   

    IF( Test_Type == FWDTLMtest .OR. Test_Type == TLADMtest )THEN   

      ! -- Zero out Atmosphere_TL structure members 
      CALL CRTM_Zero_Atmosphere( Atmosphere_TL )
      CALL CRTM_Zero_Surface( Surface_TL ) 

      !#----------------------------------------------------------------------#                                        
      !#                       -- BEGIN THE VARIABLE LOOP --                  #                                        
      !#----------------------------------------------------------------------#                                        

      Variable_Loop: DO nV = 1, N_VARIABLES                                                                            

        WRITE( *, '( 5x, "variable: ", a )' ) VARIABLE_NAME(nV)                                                        

        ! -- Loop over the layers                                                                                      
        Layer_Loop: DO k = 1, n_Layers                                                                      

          IF( Test_Type == FWDTLMtest )THEN                                                                             

            ! -- Loop over PERTURBATIONS                                                                               
            Perturbation_Loop: DO nP = 1, N_PERTURBATIONS                                                              

              ! -- Perturb the TL inputs                                                                                 

              CALL Get_Perturbation( Atmosphere(m), k, nV, Perturbation_Fraction(nP), &
                                   ZERO, Atmosphere_TL, base_value)      

              ! -- Call the tangent-linear model                                                                         

              Error_Status = CRTM_Tangent_Linear( Atmosphere(m), &                                             
                                                  Surface, &                                                
                                                  Atmosphere_TL, &                                          
                                                  Surface_TL, &                                             
                                                  GeometryInfo, &                                           
                                                  ChannelInfo, &                                            
                                                  RTSolution, &                                             
                                                  RTSolution_TL )                                           
              IF ( Error_Status /= SUCCESS ) THEN                                                           
                CALL Display_Message( PROGRAM_NAME, &                                                       
                                      'Error calling Tangent_Linear', &                                     
                                      Error_Status )                                                        
                STOP                                                                                        
              END IF                                                                                        

              ! -- put back the base value                                                                             
              CALL Set_Base_Value(Atmosphere_TL, k, nV, base_value)                                                       

              ! -- Call the forward  model                                                                             

              CALL Get_Perturbation( Atmosphere(m), k, nV, ONE+Perturbation_Fraction(nP), &
                                   ZERO, Atmosphere(m), base_value)      

              Error_Status = CRTM_Forward( Atmosphere(m), &                           
                                           Surface, &                              
                                           GeometryInfo, &                         
                                           ChannelInfo, &                          
                                           RTSolution )  

              IF ( Error_Status /= SUCCESS ) THEN                                  
                CALL Display_Message( PROGRAM_NAME, &                              
                                      'Error calling CRTM_Forward with perturbed input', &              
                                      Error_Status )                               
                STOP                                                               
              END IF                                                               

              ! -- put back the base value                                                                             
              CALL Set_Base_Value(Atmosphere(m), k, nV, base_value)                                                       

              DO l = 1, ChannelInfo%n_channels
                FWDTLMtestResult%d_TL( k, l, nP, nV ) = RTSolution_TL(l)%Brightness_Temperature
                FWDTLMtestResult%d_NL( k, l, nP, nV ) = RTSolution(l)%Brightness_Temperature - &
                                                        RTSolution_FWDBL(l)%Brightness_Temperature 
              ENDDO
            END DO Perturbation_Loop                                                                                 

          ELSE  ! TLADMtest                                                                                              

            ! -- Perturb the TL input by a value ONE                                                                   
            CALL Get_Perturbation( Atmosphere(m), k, nV, Zero, ONE, Atmosphere_TL, base_value)

            Error_Status = CRTM_Tangent_Linear( Atmosphere(m), &                                               
                                                Surface, &                                                  
                                                Atmosphere_TL, &                                            
                                                Surface_TL, &                                               
                                                GeometryInfo, &                                             
                                                ChannelInfo, &                                              
                                                RTSolution, &                                               
                                                RTSolution_TL )                                             
            IF ( Error_Status /= SUCCESS ) THEN                                                             
              CALL Display_Message( PROGRAM_NAME, &                                                         
                                    'Error calling Tangent_Linear', &                                       
                                    Error_Status )                                                          
              STOP                                                                                          
            END IF                                                                                          

            ! -- set back previous value
            CALL Set_Base_Value(Atmosphere_TL, k, nV, base_value)                                                         

            ! -- Call the tangent-linear model                                                                         
            DO l = 1, ChannelInfo%n_channels                                              
              TLADMtestResult%d_TL( k, l, nV ) = RTSolution_TL(l)%Brightness_Temperature  
            ENDDO                                                                         

          ENDIF                                                                                                          
        
        END DO Layer_Loop

      END DO Variable_Loop

    ENDIF

    !---------------------------------------------------------
    !  Test cases requiring AD model                          
    !---------------------------------------------------------

    IF( Test_Type == TLADMtest .OR. Test_Type == ADKMtest)THEN

      !#---------------------------------------------------------------------#            
      !#                   -- BEGIN ADJOINT VARIABLE LOOP --                 #            
      !#---------------------------------------------------------------------#            

      WRITE( *, '( 10x, "Performing AD calculations...." )' )                             


      ! --------------                                                                    
      ! Begin the loop                                                                    
      ! --------------                                                                    

      AD_Channel_Loop: DO l = 1, ChannelInfo%n_Channels                                               

        ! -------------------                                                             
        ! Zero out everything                                                             
        ! -------------------                                                             
        CALL CRTM_Zero_Atmosphere( Atmosphere_AD )
        CALL CRTM_Zero_Surface( Surface_AD ) 
        DO ll = 1, ChannelInfo%n_Channels
          RTSolution_AD(ll)%radiance = ZERO
          RTSolution_AD(ll)%Brightness_temperature = ZERO
        END DO
        RTSolution_AD(l)%Brightness_temperature = ONE                                     

        Error_Status = CRTM_Adjoint( Atmosphere(m),        &                                 
                                     Surface,           &                                 
                                     RTSolution_AD,     &                                 
                                     GeometryInfo,      &                                 
                                     ChannelInfo,       &                                 
                                     Atmosphere_AD,     &                                 
                                     Surface_AD,        &                                 
                                     RTSolution )                                

        IF ( Error_Status /= SUCCESS ) THEN                                               
          CALL Display_Message( PROGRAM_NAME, &                                           
                                'Error calling CRTM_Adjoint', &                           
                                Error_Status )                                            
          STOP                                                                            
        END IF                                                                            

        IF( Test_Type == TLADMtest )THEN                                                  
          TLADMtestResult%d_AD( :, l, NV_LAYER_TEMPERATURE ) = Atmosphere_AD%Temperature 
          TLADMtestResult%d_AD( :, l, NV_LAYER_WATER_VAPOR ) = Atmosphere_AD%Absorber(:, H2O_idx)                          
          TLADMtestResult%d_AD( :, l, NV_LAYER_OZONE )       = Atmosphere_AD%Absorber(:, O3_idx)
        ELSE  ! ADKMtest                                                              
          ADKMtestResult%d_AD( :, l, NV_LAYER_TEMPERATURE ) = Atmosphere_AD%Temperature   
          ADKMtestResult%d_AD( :, l, NV_LAYER_WATER_VAPOR ) = Atmosphere_AD%Absorber(:, H2O_idx)
          ADKMtestResult%d_AD( :, l, NV_LAYER_OZONE )       = Atmosphere_AD%Absorber(:, O3_idx)
        ENDIF                                                                         

      END DO AD_Channel_Loop                                                              

    ENDIF                                                                                 

    !---------------------------------------------------------                            
    !  Test cases requiring K-Matrix model                                                
    !---------------------------------------------------------                            

    IF( Test_Type == ADKMtest )THEN
                                                                                          
      !#------------------------------------------------------------------------#         
      !#                   -- BEGIN K-MATRIX CALCULATIONS --                    #         
      !#------------------------------------------------------------------------#         

      WRITE( *, '( 10x, "Performing K-Matrix calculations...." )' )                       

      CALL CRTM_Zero_Atmosphere( Atmosphere_K )
                                           
      CALL CRTM_Zero_Surface( Surface_K )                                                 
      DO l = 1, ChannelInfo%n_channels                                                    
        RTSolution_K(l)%Radiance = ZERO                                     
        RTSolution_K(l)%Brightness_Temperature = ONE                                     
      ENDDO                                                                               

      ! -- Call the K_Matrix model                                                                         
      Error_Status = CRTM_K_Matrix(  Atmosphere(m),      &                                   
                                     Surface,         &                                   
                                     RTSolution_K,    &                                   
                                     GeometryInfo,    &                                   
                                     ChannelInfo,     &                                   
                                     Atmosphere_K,    &                                   
                                     Surface_K,       &                                   
                                     RTSolution )                                         

      IF ( Error_Status /= SUCCESS ) THEN                                                 
        CALL Display_Message( PROGRAM_NAME, &                                             
                              'Error calling CRTM_K_Matrix', &                            
                              Error_Status )                                              
        STOP                                                                              
      END IF                                                                              
      
      DO l = 1, ChannelInfo%n_channels                                                                                    
        ADKMtestResult%d_K( :, l, NV_LAYER_TEMPERATURE ) = Atmosphere_K(l)%Temperature          
        ADKMtestResult%d_K( :, l, NV_LAYER_WATER_VAPOR ) = Atmosphere_K(l)%Absorber(:, H2O_idx)       
        ADKMtestResult%d_K( :, l, NV_LAYER_OZONE )       = Atmosphere_K(l)%Absorber(:, O3_idx)       
      ENDDO

    ENDIF                                                                                 

    !#------------------------------------------------------------------------#           
    !#       -- WRITE THE CURRENT PROFILE ADKMtest STRUCTURE TO FILE --       #           
    !#------------------------------------------------------------------------#           

    WRITE( *, '( 10x, "Writing data to output file...." )' )                              

    SELECT CASE( Test_Type )                                                              
    CASE( FWDTLMtest )                                                                     
      FWDTLMtestResult%nM = m
      WRITE(FWDTLMtestResult%nM_Name, '("Profile # ", i5)') m
      FWDTLMtestResult%DataType = FWDTLMTEST_SENSOR_TYPE
      FWDTLMtestResult%pressure = Atmosphere(m)%Pressure
      FWDTLMtestResult%Channel = ChannelInfo%Sensor_Channel
      FWDTLMtestResult%Perturbation = Perturbation_Fraction

      DO nV = 1, N_VARIABLES
        FWDTLMtestResult%Variable_Name(nv) = TRIM(VARIABLE_NAME(nv))
      END DO
      Error_Status = Write_FWDTLMtest_netCDF( TRIM( out_Filename ), &                                   
                                              FWDTLMtestResult, &                                                    
                                              New_File = New_File, &                                           
                                              Title         = 'FWD/TL CRTM test results for '//&          
                                                              TRIM( Sensor_Descriptor ), &
                                              History       = PROGRAM_RCS_ID, &
                                              Comment       = COMMENTE, &                             
                                              ID_Tag        = TRIM( ID_Tag ) )                                 
    CASE( TLADMtest )                                                                     
      TLADMtestResult%nM = m
      WRITE(TLADMtestResult%nM_Name, '("Profile # ", i5)') m
      TLADMtestResult%DataType = TLADMTEST_SENSOR_TYPE
      TLADMtestResult%pressure = Atmosphere(m)%Pressure
      TLADMtestResult%Channel = ChannelInfo%Sensor_Channel
      DO nV = 1, N_VARIABLES
        TLADMtestResult%Variable_Name(nV) = TRIM(VARIABLE_NAME(nV))
      END DO
      Error_Status = Write_TLADMtest_netCDF( TRIM( out_Filename ), &                     
                                             TLADMtestResult, &                                 
                                             New_File = New_File, &                       
                                             Title         = 'TL/AD CRTM test results     for '//&
                                                              TRIM( Sensor_Descriptor ), &
                                             History       = PROGRAM_RCS_ID, &            
                                             Comment       = COMMENTE, &                             
                                             ID_Tag        = TRIM( ID_Tag ) )             
    CASE( ADKMtest )                                                                      
      ADKMtestResult%nM = m
      WRITE(ADKMtestResult%nM_Name, '("Profile # ", i5)') m
      ADKMtestResult%DataType = ADKMTEST_SENSOR_TYPE
      ADKMtestResult%pressure = Atmosphere(m)%Pressure
      ADKMtestResult%Channel = ChannelInfo%Sensor_Channel
      DO nV = 1, N_VARIABLES
        ADKMtestResult%Variable_Name(nV) = TRIM(VARIABLE_NAME(nV))
      END DO
      Error_Status = Write_ADKMtest_netCDF( TRIM( out_Filename ), &                      
                                            ADKMtestResult, &                                   
                                            New_File = New_File, &                        
                                            Title         = 'AD/K CRTM test results f    or '//&
                                                              TRIM( Sensor_Descriptor ), &
                                            History       = PROGRAM_RCS_ID, &             
                                            Comment       = COMMENTE, &                             
                                            ID_Tag        = TRIM( ID_Tag ) )              

    END SELECT                                                                            

    IF ( Error_Status /= SUCCESS ) THEN                                                   
      WRITE( Message, '( "Error writing result for profile #", i5, " to ", a )' ) &       
                      m, TRIM( out_Filename )                                            
      CALL Display_Message( PROGRAM_NAME, &                                               
                            TRIM( Message ), &                                            
                             Error_Status )                                               
      STOP                                                                                
    END IF                                                                                

    ! -----------------------                  
    ! Reset the new file flag                  
    ! -----------------------                  

    IF ( New_File == SET ) New_File = UNSET    

  END DO Profile_Loop


CONTAINS

  SUBROUTINE Get_Perturbation(Atmosphere, k, nV, c1, c2, Atmosphere_p, base_value)  

    TYPE(CRTM_Atmosphere_type), INTENT( IN OUT )  :: Atmosphere 
    INTEGER,               INTENT( IN )      :: k, nV 
    REAL( fp_kind ),       INTENT( IN )      :: c1, c2
    REAL( fp_kind ),       INTENT( OUT )     :: base_value
    TYPE(CRTM_Atmosphere_type), INTENT( IN OUT )  :: Atmosphere_p

    SELECT CASE ( nV )                                                                                 
      CASE ( NV_LAYER_TEMPERATURE )                                                  
        base_value = Atmosphere_p%Temperature( k )                                                             
        Atmosphere_p%Temperature( k ) = c1 * Atmosphere%Temperature( k ) + c2
      CASE ( NV_LAYER_WATER_VAPOR )                                                  
        base_value = Atmosphere_p%Absorber( k, H2O_Idx)                                                                  
        Atmosphere_p%Absorber( k, H2O_Idx ) = c1 * Atmosphere%Absorber( k, H2O_Idx ) + c2       
      CASE ( NV_LAYER_OZONE )                                                                          
        base_value = Atmosphere_p%Absorber( k, O3_idx )                                                                  
        Atmosphere_p%Absorber( k, O3_idx ) = c1 * Atmosphere%Absorber( k, O3_idx) + c2       
    END SELECT                                                                                         

  END SUBROUTINE Get_Perturbation                                                   

  SUBROUTINE Set_Base_Value(Atmosphere, k, nV, base_value)                           
     TYPE(CRTM_Atmosphere_type), INTENT( IN OUT )  :: Atmosphere                          
     INTEGER,               INTENT( IN )      :: k, nV                               
     REAL( fp_kind ),       INTENT( IN )      :: base_value                          

       SELECT CASE ( nV )                                                                              
         CASE ( NV_LAYER_TEMPERATURE ) 
           Atmosphere%Temperature( k ) = base_value                               
         CASE ( NV_LAYER_WATER_VAPOR )                                               
           Atmosphere%Absorber( k, H2O_Idx ) = base_value                               
         CASE ( NV_LAYER_OZONE )                                                                       
           Atmosphere%Absorber( k, O3_idx ) = base_value                               
       END SELECT                                                                                      
  END SUBROUTINE Set_Base_Value

END PROGRAM CRTM_Consistency_Check
