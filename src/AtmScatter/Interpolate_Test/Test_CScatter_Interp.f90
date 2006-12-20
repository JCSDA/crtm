PROGRAM Test_CScatter_Interp
  USE Type_Kinds           , ONLY: fp=>fp_kind
  USE Message_Handler
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE CRTM_Parameters
  USE CRTM_CloudScatter
  USE CRTM_CloudCoeff
  USE CRTM_SpcCoeff
  USE CRTM_Cloud_Define
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Cloud_Binary_IO
  USE Interp_ND

  IMPLICIT NONE

  ! Atmospheric data info/dimensions
  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'
  INTEGER, PARAMETER :: N_PROFILES = 52  
  
  ! variable to hold atmospheric profile data
  TYPE( CRTM_Atmosphere_Type ), DIMENSION(N_Profiles) :: Atmosphere
  
  INTEGER, PARAMETER :: SL = 512  
 
  ! Names used for loading SpcCoeff/CloudCoeff
  CHARACTER( SL ) :: File_Prefix
  CHARACTER( SL ) :: SpcCoeff_File
  CHARACTER( SL ) :: CloudCoeff_File
  CHARACTER( SL ), PARAMETER :: MODULE_RCS_ID = &
  '$Id: Test_CScatter_Interp.f90,v 1.6 2006/12/18 23:20:10 wx20gd Exp $'

  
  INTEGER :: Error_Status, l, n, t
  
  ! Variables needed when calling cloudscatter interpolation routine
  INTEGER,  PARAMETER :: CLOUD_TYPE = WATER_CLOUD
  REAL(fp), PARAMETER :: EFF_v = ZERO
  REAL(fp) :: ext_IR, ext_MW
  REAL(fp) :: w0_IR, w0_MW
  REAL(fp) :: g_IR, g_MW
  
  ! Interpolation dimension parameter(used to dimension the input for 
  ! calculating the test value
  INTEGER, PARAMETER :: D=2
 
  ! These variables/parameters will be used to calculate the Test_Value
  REAL(fp) :: d1, d2, dx1, dx2
  REAL(fp), DIMENSION(D,D) :: y
  REAL(fp) :: Test_Value
  INTEGER, PARAMETER :: x1pos1=605, x1pos2=606, x2pos1=1, x2pos2=2
  INTEGER, PARAMETER :: channel_number=18
  INTEGER, PARAMETER :: layer_number=85
  
  
  ! IR/MW phase coefficient arrays are allocatable
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: p_coef_IR 
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: p_coef_MW
  
  
  ! Program name
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_CScatter_Interp'
  

  


  ! ------------------
  ! Program descriptor
  ! ------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to write CloudScatter interpolation '//&
                        'calculations to a file', &
                        MODULE_RCS_ID )


  !#----------------------------------------------------------------------------#
  !#                -- READ THE Atmosphere STRUCTURE DATA FILE --               #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Reading the Atmosphere structure file..." )' )

  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                              Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMOSPHERE_FILENAME, & 
                           Error_Status )
   STOP
  END IF

                        


  ! ------------------------------------------------
  ! Enter the instrument file prefix, e.g. hirs3_n16
  ! ------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL( File_Prefix )


  ! --------------------
  ! Create the filenames
  ! --------------------

  SpcCoeff_File     = TRIM( File_Prefix )//'.SpcCoeff.bin'
  CloudCoeff_File   = 'CloudCoeff.bin'

  ! -------------------------------------------
  ! Load the SpcCoeff spectral coefficient data 
  ! into the public data structure SC
  ! -------------------------------------------

  Error_Status = CRTM_Load_SpcCoeff( SpcCoeff_File )  ! Input
                                     
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
                           'Error loading SpcCoeff_File', &
                            Error_Status)
   STOP
  END IF


  ! --------------------------------
  ! Load the CloudCoeff data into
  ! the public data structure CloudC
  ! --------------------------------

  Error_Status = CRTM_Load_CloudCoeff( CloudCoeff_File )  ! Input

  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
                           'Error loading CloudCoeff_File', &
                           Error_Status)
   STOP
  END IF


  ! Allocate for the IR phase coefficient array
  Allocate( p_coef_IR(0:CloudC%n_Legendre_Terms, 1),      &
                                 STAT = Error_Status      ) 

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating p_coef_IR.', & 
                           Error_Status )
     STOP
  END IF


  ! Allocate for the MW phase coefficient array
  Allocate( p_coef_MW(0:CloudC%n_Legendre_Terms, CloudC%n_Phase_Elements),      &
                                                       STAT = Error_Status      ) 

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating p_coef_MW.', & 
                           Error_Status )
     STOP
  END IF  

  
  ! assign input data used to calculate test value
  y(1,1) = CloudC%ext_L_IR(x1pos1,x2pos1)
  y(2,1) = CloudC%ext_L_IR(x1pos2,x2pos1)
  y(1,2) = CloudC%ext_L_IR(x1pos1,x2pos2)
  y(2,2) = CloudC%ext_L_IR(x1pos2,x2pos2)

  dx1 = (CloudC%Wavenumber(x1pos2) - CloudC%Wavenumber(x1pos1))
  dx2 = (CloudC%Reff_IR(x2pos2) - CloudC%Reff_IR(x2pos1))

  d1 = ((SC%Wavenumber(channel_number) - CloudC%Wavenumber(x1pos1))/dx1)
  d2 = ((Atmosphere(1)%Cloud(1)%Effective_Radius(layer_number) - CloudC%Reff_IR(x2pos1))/dx2)


  ! Calculate test value(used for comparison with interpolation calculations)
  Test_Value = Interp_2D(d1, d2, y)
  
  ! Loop over channels(corresponding to a wavenumber) and effective radius(corresponding                           
    Infrared_Channel_Loop: DO l=18, 18 
      Infrared_Eff_Radius_Loop: DO n=85, 85 
            
            CALL Get_Cloud_Opt_IR(CloudC%n_Legendre_Terms,    &  ! Input
                                  CloudC%n_Phase_Elements,    &  ! Input
                                         SC%Wavenumber(l),    &  ! Input
                                               CLOUD_TYPE,    &  ! Input
               Atmosphere(1)%Cloud(1)%Effective_Radius(n),    &  ! Input
                                                    EFF_V,    &  ! Input
                                                   ext_IR,    &  ! Output
                                                    w0_IR,    &  ! Output
                                                     g_IR,    &  ! Output
                                                p_coef_IR)       ! Output

               
            PRINT *, l, 'channel_number', n, 'layer_number', CloudC%Reff_IR(1), Atmosphere(1)%Cloud(1)%Effective_Radius(n)
            PRINT *, CloudC%Wavenumber(605), SC%Wavenumber(l), CloudC%Wavenumber(606)
      END DO Infrared_Eff_Radius_Loop
    END DO Infrared_Channel_Loop 
    PRINT *, Test_Value, ext_IR  


    Channel_Loop: DO l=1, CloudC%n_Frequencies 
      Eff_Radius_Loop: DO n=1, CloudC%n_Reff_MW
        Temperature_Loop: DO t=1, CloudC%n_Temperatures 
            
            CALL Get_Cloud_Opt_MW(CloudC%n_Legendre_Terms,    &  ! Input
                                  CloudC%n_Phase_Elements,    &  ! Input
                                      CloudC%Frequency(l),    &  ! Input
                                               CLOUD_TYPE,    &  ! Input
                                        CloudC%Reff_MW(n),    &  ! Input
                                                    EFF_V,    &  ! Input
                                    CloudC%Temperature(t),    &  ! Input
                                                   ext_MW,    &  ! Output
                                                    w0_MW,    &  ! Output
                                                     g_MW,    &  ! Output
                                                p_coef_MW)       ! Output


         !   PRINT *, ext_MW, 'MW', CloudC%ext_L_MW(l,1,t), l, n, t
         !   PRINT *, CloudC%n_Temperatures, CloudC%n_Reff_MW, CloudC%n_Wavenumbers                
        END DO Temperature_Loop
      END DO Eff_Radius_Loop
    END DO Channel_Loop                     
  

  ! deallocate p_coef for the microwave
  Deallocate( p_coef_MW,                 &
              STAT = Error_Status        ) 
  IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error DEAllocating p_coef_MW.', & 
                            Error_Status )
   STOP
  END IF
  
  ! deallocate p_coef for the infrared
  Deallocate( p_coef_IR,                 &
              STAT = Error_Status        ) 
  IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                            'Error DEAllocating p_coef_IR.', & 
                            Error_Status )
   STOP
  END IF

  ! deallocate CloudC structure
  Error_Status = CRTM_Destroy_CloudCoeff( )
    
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating structure CloudC', &
                           Error_Status)
   STOP
  END IF
  
  ! deallocate SC structure
  Error_Status = CRTM_Destroy_SpcCoeff( )
    
  IF ( Error_Status /= SUCCESS ) THEN
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating structure CloudC', &
                           Error_Status)
   STOP
  END IF

END PROGRAM Test_CScatter_Interp

