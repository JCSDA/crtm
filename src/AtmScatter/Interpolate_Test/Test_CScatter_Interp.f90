PROGRAM Test_CScatter_Interp
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler
  USE Compare_Float_Numbers
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
  
  ! String length parameter 
  INTEGER, PARAMETER :: SL = 512  
 
  ! Names used for loading CloudCoeff data
  CHARACTER( SL ) :: CloudCoeff_File
  CHARACTER( SL ), PARAMETER :: MODULE_RCS_ID = &
  '$Id: Test_CScatter_Interp.f90,v 1.6 2006/12/18 23:20:10 wx20gd Exp $'

  

  ! Index variables used for loop control
  INTEGER :: Error_Status, l, n, tp, ct
  
  ! Number of cloud types
  INTEGER, PARAMETER :: NC=2

  ! Cloud type array
  INTEGER,  PARAMETER, DIMENSION(2) :: CLOUD_TYPE = (/WATER_CLOUD, ICE_CLOUD/)
  
  ! Density of Ice Cloud
  INTEGER, PARAMETER :: M=3 

  ! variables calculated in CRTM_CloudScatter interpolation
  REAL(fp), PARAMETER :: EFF_v = ZERO
  REAL(fp) :: ext_IR, ext_MW
  REAL(fp) :: w0_IR, w0_MW
  REAL(fp) :: g_IR, g_MW
  
  ! Interpolation dimension parameter(used to dimension the input for 
  ! calculating the test value
  INTEGER, PARAMETER :: D=2

  ! Wavenumber spacing
  REAL(fp), PARAMETER :: dx1=4.0_fp
 
  ! These variables/parameters will be used to calculate the Test_Value
  REAL(fp) :: d1, d2, dx2
  INTEGER, PARAMETER :: NP = 5
  REAL(fp), DIMENSION(D,D) :: ext, w0, g, p_coeff
  REAL(fp), PARAMETER, DIMENSION(NP) :: EFFECTIVE_RADIUS = (/4, 16, 34, 18, 24/)
  REAL(fp), PARAMETER, DIMENSION(NP) :: WAVENUMBER = (/1100, 1443, 1634, 1269, 1317/)
  INTEGER, PARAMETER, DIMENSION(NP) :: LEG_TERM = (/6, 12, 18, 14, 13/)
  INTEGER, PARAMETER :: MINIMUM_RADIUS = 5
  INTEGER, PARAMETER :: MAXIMUM_RADIUS = 30
  INTEGER, PARAMETER :: CHANNEL_NUMBER = 5
  INTEGER, PARAMETER :: LAYER_NUMBER = 85
  INTEGER :: l1pos1, l1pos2, l2pos1, l2pos2
  
  ! ULP for comparing test value with current cloudscatter interpolation
  ! value
  INTEGER, PARAMETER :: ULP = 20

  ! Declare the test value
  REAL(fp), DIMENSION(8) :: Test_Value
  
  
  ! IR/MW phase coefficient arrays are allocatable
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: p_coef_IR 
  REAL(fp), DIMENSION(:,:), ALLOCATABLE :: p_coef_MW
  
  
  ! Program name
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_CScatter_Interp'

  ! Logical variables (True if interpolation is correct) (False otherwise)
  LOGICAL :: ext_liq_equal, w0_liq_equal, g_liq_equal, &
             p_coeff_liq_equal, ext_sol_equal, w0_sol_equal, &
             g_sol_equal, p_coeff_sol_equal
             
  

  


  ! ------------------
  ! Program descriptor
  ! ------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to write CloudScatter interpolation '//&
                        'calculations to a file', &
                        MODULE_RCS_ID )


 
  CloudCoeff_File   = 'CloudCoeff.bin'


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
   
    ! Loop over test positions
    Test_Position_Loop: DO tp=1, NP
      ! Loop over coud types
      Cloud_Type_Loop: DO ct=1, NC
         ! Calculate lookup table spacing
         l1pos1 = findIdx(CloudC%Wavenumber(1), dx1, WAVENUMBER(tp))
         l2pos1 = findIdx(CloudC%Reff_IR, EFFECTIVE_RADIUS(tp))
         l1pos2 = l1pos1 + 1
         l2pos2 = l2pos1 + 1
         dx2 = (CloudC%Reff_IR(l2pos2) - CloudC%Reff_IR(l2pos1))
  
        ! Calculate interpolation weighting factors
        d1 = ((WAVENUMBER(tp) - CloudC%Wavenumber(l1pos1))/dx1)
        
        IF (EFFECTIVE_RADIUS(tp) < MINIMUM_RADIUS) THEN 
          d2 = ZERO

        ELSE IF (EFFECTIVE_RADIUS(tp) > MINIMUM_RADIUS .AND. EFFECTIVE_RADIUS(tp) < MAXIMUM_RADIUS) THEN
          d2 = ((EFFECTIVE_RADIUS(tp) - CloudC%Reff_IR(l2pos1))/dx2)

        ELSE
          d2 = ONE

        END IF
        
     
        IF (ct == 1) THEN
          ! assign input data used to calculate test value
          ext = CloudC%ext_L_IR(l1pos1:l1pos2,l2pos1:l2pos2)
          
          w0 = CloudC%w_L_IR(l1pos1:l1pos2,l2pos1:l2pos2)
          
          g = CloudC%g_L_IR(l1pos1:l1pos2,l2pos1:l2pos2)
         
          p_coeff = CloudC%phase_coeff_L_IR(l1pos1:l1pos2,l2pos1:l2pos2,LEG_TERM(tp))
         

          ! Calculate test values used for comparison with interpolation calculations
          Test_Value(1) = Interp_2D(d1, d2, ext)
          Test_Value(2) = Interp_2D(d1, d2, w0)
          Test_Value(3) = Interp_2D(d1, d2, g)
          Test_Value(4) = Interp_2D(d1, d2, p_coeff)
         

        ELSE

          ! assign input data used to calculate test value
          ext = CloudC%ext_S_IR(l1pos1:l1pos2,l2pos1:l2pos2,M)
          
          w0 = CloudC%w_S_IR(l1pos1:l1pos2,l2pos1:l2pos2,M)
          
          g = CloudC%g_S_IR(l1pos1:l1pos2,l2pos1:l2pos2,M)
         
          p_coeff = CloudC%phase_coeff_S_IR(l1pos1:l1pos2,l2pos1:l2pos2,M,LEG_TERM(tp))
         

          ! Calculate test values used for comparison with interpolation calculations
          Test_Value(5) = Interp_2D(d1, d2, ext)
          Test_Value(6) = Interp_2D(d1, d2, w0)
          Test_Value(7) = Interp_2D(d1, d2, g)
          Test_Value(8) = Interp_2D(d1, d2, p_coeff)
        
        END IF 

  
                  CALL Get_Cloud_Opt_IR(CloudC%n_Legendre_Terms,    &  ! Input
                                        CloudC%n_Phase_Elements,    &  ! Input
                                                 WAVENUMBER(tp),    &  ! Input
                                                 Cloud_TYPE(ct),    &  ! Input
                                           Effective_Radius(tp),    &  ! Input
                                                          EFF_V,    &  ! Input
                                                         ext_IR,    &  ! Output
                                                          w0_IR,    &  ! Output
                                                           g_IR,    &  ! Output
                                                      p_coef_IR)       ! Output


        IF (ct == 1) THEN
          ext_liq_equal = Compare_Float(        ext_IR, & 
                                         Test_Value(1), &
                                               ULP=ULP  )

          w0_liq_equal = Compare_Float(         w0_IR, & 
                                        Test_Value(2), &
                                              ULP=ULP  )

          g_liq_equal = Compare_Float(          g_IR, &  
                                       Test_Value(3), &
                                             ULP=ULP  )

          p_coeff_liq_equal = Compare_Float( p_coef_IR(LEG_TERM(tp),1), & 
                                                         Test_Value(4), &
                                                               ULP=ULP  )

      

        ELSE
          ext_sol_equal = Compare_Float(        ext_IR,  & 
                                         Test_Value(5),  &
                                               ULP=ULP   )

          w0_sol_equal = Compare_Float(         w0_IR, & 
                                        Test_Value(6), &
                                              ULP=ULP  )

          g_sol_equal = Compare_Float(          g_IR, & 
                                       Test_Value(7), &
                                             ULP=ULP  )

          p_coeff_sol_equal = Compare_Float(     p_coef_IR(LEG_TERM(tp),1), & 
                                                             Test_Value(8), &
                                                                   ULP=ULP  )
          PRINT *, g_sol_equal, Test_Value(8), p_coef_IR(LEG_TERM(tp),1)

        END IF  
                   
                 
                 
                    

      END DO Cloud_Type_Loop
    END DO Test_Position_Loop     
   
  IF (ext_liq_equal .AND. w0_liq_equal .AND. &
      g_liq_equal .AND. p_coeff_liq_equal &
     .AND. ext_sol_equal .AND. w0_sol_equal .AND. &
      g_sol_equal .AND. p_coeff_sol_equal) THEN

    PRINT *, 'All tests passed'  

  

  ELSE

    PRINT *, 'Test Failed' 
  
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
  

END PROGRAM Test_CScatter_Interp

