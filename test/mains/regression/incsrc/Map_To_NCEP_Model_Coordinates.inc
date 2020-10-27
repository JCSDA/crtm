  ! 
  ! Include file containing an internal subprogram to map
  ! atmospheres to representative NCEP model coordinates
  !
  SUBROUTINE Map_To_NCEP_Model_Coordinates() 
    ! Local parameter
    REAL(fp), PARAMETER :: PRESSURE_DIFFERENCE_FILL_VALUE = 1500.0_fp
    ! Local variables
    INTEGER :: n_Profiles
    INTEGER :: Best_Match_Index
    REAL(fp) :: Pressure_Difference
    ! Layer counters
    INTEGER :: k, kr, kg
    ! Profile counter
    INTEGER :: m
    
    n_Profiles = SIZE(Atm)
    
    ! Assign profile 1 vertical coordinates 
    Atm_NAM(1)%Pressure = &
    (/7.557_fp,  18.680_fp,  29.832_fp,  41.024_fp,  52.266_fp,  63.614_fp, &
     75.140_fp,  86.867_fp,  98.898_fp, 111.283_fp, 124.021_fp, 137.265_fp, &                               
    151.065_fp, 165.624_fp, 181.345_fp, 198.683_fp, 217.994_fp, 239.376_fp, &                               
    263.135_fp, 289.310_fp, 317.552_fp, 347.341_fp, 378.054_fp, 409.310_fp, &                               
    441.043_fp, 473.178_fp, 505.540_fp, 537.933_fp, 570.153_fp, 602.116_fp, &                               
    633.675_fp, 664.585_fp, 694.747_fp, 723.816_fp, 751.247_fp, 776.696_fp, &                               
    799.965_fp, 821.053_fp, 839.864_fp, 856.496_fp, 871.196_fp, 884.014_fp, &                               
    895.245_fp, 904.942_fp, 913.304_fp, 920.824_fp, 927.750_fp, 934.330_fp, &                               
    940.613_fp, 946.599_fp, 952.337_fp, 957.878_fp, 963.221_fp, 968.415_fp, &                               
    973.510_fp, 978.506_fp, 983.403_fp, 988.202_fp, 992.940_fp, 997.651_fp/)                                
    Atm_NAM(1)%Level_Pressure = &
    (/2.000_fp,  13.114_fp,  24.246_fp,  35.417_fp,  46.629_fp,  57.902_fp, &
     69.326_fp,  80.953_fp,  92.781_fp, 105.014_fp, 117.551_fp, 130.492_fp, &                                     
    144.039_fp, 158.092_fp, 173.156_fp, 189.534_fp, 207.833_fp, 228.154_fp, &                                     
    250.598_fp, 275.671_fp, 302.949_fp, 332.154_fp, 362.528_fp, 393.579_fp, &                                     
    425.042_fp, 457.045_fp, 489.310_fp, 521.770_fp, 554.095_fp, 586.211_fp, &                                     
    618.021_fp, 649.329_fp, 679.840_fp, 709.653_fp, 737.978_fp, 764.517_fp, &                                     
    788.875_fp, 811.054_fp, 831.053_fp, 848.675_fp, 864.316_fp, 878.076_fp, &                                     
    889.952_fp, 900.539_fp, 909.346_fp, 917.262_fp, 924.386_fp, 931.114_fp, &                                     
    937.546_fp, 943.680_fp, 949.518_fp, 955.157_fp, 960.598_fp, 965.843_fp, &                                     
    970.987_fp, 976.032_fp, 980.980_fp, 985.827_fp, 990.577_fp, 995.302_fp, &                                     
   1000.000_fp/)                                                                                                  
    Atm_GFS(1)%Pressure = &
    (/0.321_fp,   1.010_fp,   1.798_fp,   2.701_fp,   3.733_fp,   4.914_fp, &
      6.264_fp,   7.807_fp,   9.570_fp,  11.584_fp,  13.882_fp,  16.503_fp, &
     19.492_fp,  22.895_fp,  26.769_fp,  31.172_fp,  36.172_fp,  41.842_fp, &
     48.263_fp,  55.520_fp,  63.709_fp,  72.928_fp,  83.282_fp,  94.879_fp, &
    107.828_fp, 122.239_fp, 138.215_fp, 155.853_fp, 175.235_fp, 196.427_fp, &                               
    219.467_fp, 244.369_fp, 271.106_fp, 299.614_fp, 329.784_fp, 361.464_fp, &                               
    394.453_fp, 428.513_fp, 463.366_fp, 498.709_fp, 534.218_fp, 569.567_fp, &                               
    604.432_fp, 638.510_fp, 671.523_fp, 703.229_fp, 733.431_fp, 761.972_fp, &                               
    788.744_fp, 813.681_fp, 836.757_fp, 857.984_fp, 877.401_fp, 895.071_fp, &                               
    911.079_fp, 925.518_fp, 938.495_fp, 950.117_fp, 960.493_fp, 969.733_fp, &                               
    977.941_fp, 985.216_fp, 991.651_fp, 997.335_fp/)                                                        
    Atm_GFS(1)%Level_Pressure = &
    (/0.001_fp,   0.642_fp,   1.377_fp,   2.219_fp,   3.182_fp,   4.284_fp, &
      5.544_fp,   6.984_fp,   8.630_fp,  10.510_fp,  12.657_fp,  15.107_fp, &                                     
     17.900_fp,  21.083_fp,  24.707_fp,  28.830_fp,  33.514_fp,  38.830_fp, &                                     
     44.854_fp,  51.671_fp,  59.370_fp,  68.048_fp,  77.808_fp,  88.756_fp, &                                     
    101.002_fp, 114.655_fp, 129.823_fp, 146.607_fp, 165.099_fp, 185.372_fp, &                                     
    207.481_fp, 231.454_fp, 257.284_fp, 284.928_fp, 314.300_fp, 345.269_fp, &                                     
    377.659_fp, 411.248_fp, 445.778_fp, 480.955_fp, 516.463_fp, 551.974_fp, &                                     
    587.160_fp, 621.705_fp, 655.315_fp, 687.730_fp, 718.729_fp, 748.133_fp, &                                     
    775.811_fp, 801.676_fp, 825.685_fp, 847.830_fp, 868.138_fp, 886.663_fp, &
    903.479_fp, 918.678_fp, 932.359_fp, 944.631_fp, 955.603_fp, 965.384_fp, & 
    974.082_fp, 981.799_fp, 988.632_fp, 994.671_fp, 1000.000_fp/)  
    
    ! Assign profile 2 vertical coordinates                                              
    Atm_NAM(2)%Pressure = &
    (/7.557_fp,  18.680_fp,  29.832_fp,  41.024_fp,  52.266_fp,  63.614_fp, &
     75.140_fp,  86.867_fp,  98.898_fp, 111.283_fp, 124.021_fp, 137.265_fp, &
    151.065_fp, 165.624_fp, 181.345_fp, 198.683_fp, 217.994_fp, 239.376_fp, &
    263.135_fp, 289.310_fp, 317.552_fp, 347.341_fp, 378.054_fp, 409.310_fp, &
    441.043_fp, 473.178_fp, 505.540_fp, 537.933_fp, 570.153_fp, 602.116_fp, &
    633.675_fp, 664.585_fp, 694.747_fp, 723.816_fp, 751.247_fp, 776.696_fp, &
    799.965_fp, 821.053_fp, 839.864_fp, 856.496_fp, 871.196_fp, 884.014_fp, &
    895.245_fp, 904.942_fp, 913.304_fp, 920.824_fp, 927.750_fp, 934.330_fp, &
    940.613_fp, 946.599_fp, 952.337_fp, 957.878_fp, 963.221_fp, 968.415_fp, &
    973.510_fp, 978.506_fp, 983.403_fp, 988.202_fp, 992.940_fp, 997.651_fp/)
    Atm_NAM(2)%Level_Pressure = &
    (/2.000_fp,  13.114_fp,  24.246_fp,  35.417_fp,  46.629_fp,  57.902_fp, &
     69.326_fp,  80.953_fp,  92.781_fp, 105.014_fp, 117.551_fp, 130.492_fp, &
    144.039_fp, 158.092_fp, 173.156_fp, 189.534_fp, 207.833_fp, 228.154_fp, &
    250.598_fp, 275.671_fp, 302.949_fp, 332.154_fp, 362.528_fp, 393.579_fp, &
    425.042_fp, 457.045_fp, 489.310_fp, 521.770_fp, 554.095_fp, 586.211_fp, &
    618.021_fp, 649.329_fp, 679.840_fp, 709.653_fp, 737.978_fp, 764.517_fp, &
    788.875_fp, 811.054_fp, 831.053_fp, 848.675_fp, 864.316_fp, 878.076_fp, &
    889.952_fp, 900.539_fp, 909.346_fp, 917.262_fp, 924.386_fp, 931.114_fp, &
    937.546_fp, 943.680_fp, 949.518_fp, 955.157_fp, 960.598_fp, 965.843_fp, &
    970.987_fp, 976.032_fp, 980.980_fp, 985.827_fp, 990.577_fp, 995.302_fp, &
   1000.000_fp/)
    Atm_GFS(2)%Pressure = &
    (/0.321_fp,   1.010_fp,   1.798_fp,   2.701_fp,   3.733_fp,   4.914_fp, &
      6.264_fp,   7.807_fp,   9.570_fp,  11.584_fp,  13.882_fp,  16.503_fp, &
     19.492_fp,  22.895_fp,  26.769_fp,  31.172_fp,  36.172_fp,  41.842_fp, &
     48.263_fp,  55.520_fp,  63.709_fp,  72.928_fp,  83.282_fp,  94.879_fp, &
    107.828_fp, 122.239_fp, 138.215_fp, 155.853_fp, 175.235_fp, 196.427_fp, &
    219.467_fp, 244.369_fp, 271.106_fp, 299.614_fp, 329.784_fp, 361.464_fp, &
    394.453_fp, 428.513_fp, 463.366_fp, 498.709_fp, 534.218_fp, 569.567_fp, &
    604.432_fp, 638.510_fp, 671.523_fp, 703.229_fp, 733.431_fp, 761.972_fp, &
    788.744_fp, 813.681_fp, 836.757_fp, 857.984_fp, 877.401_fp, 895.071_fp, &
    911.079_fp, 925.518_fp, 938.495_fp, 950.117_fp, 960.493_fp, 969.733_fp, &
    977.941_fp, 985.216_fp, 991.651_fp, 997.335_fp/)
    Atm_GFS(2)%Level_Pressure = &
    (/0.001_fp,   0.642_fp,   1.377_fp,   2.219_fp,   3.182_fp,   4.284_fp, &
      5.544_fp,   6.984_fp,   8.630_fp,  10.510_fp,  12.657_fp,  15.107_fp, &
     17.900_fp,  21.083_fp,  24.707_fp,  28.830_fp,  33.514_fp,  38.830_fp, &
     44.854_fp,  51.671_fp,  59.370_fp,  68.048_fp,  77.808_fp,  88.756_fp, &
    101.002_fp, 114.655_fp, 129.823_fp, 146.607_fp, 165.099_fp, 185.372_fp, &
    207.481_fp, 231.454_fp, 257.284_fp, 284.928_fp, 314.300_fp, 345.269_fp, &
    377.659_fp, 411.248_fp, 445.778_fp, 480.955_fp, 516.463_fp, 551.974_fp, &
    587.160_fp, 621.705_fp, 655.315_fp, 687.730_fp, 718.729_fp, 748.133_fp, &
    775.811_fp, 801.676_fp, 825.685_fp, 847.830_fp, 868.138_fp, 886.663_fp, &
    903.479_fp, 918.678_fp, 932.359_fp, 944.631_fp, 955.603_fp, 965.384_fp, &
    974.082_fp, 981.799_fp, 988.632_fp, 994.671_fp, 1000.000_fp/)
    
    !------------------------------
    ! Assign layer temperatures,
    ! absorber amounts and metadata
    !------------------------------
    DO m = 1, n_Profiles
      ! Assign metadata
      ! Regional
      Atm_NAM(m)%Climatology = Atm(m)%Climatology
      Atm_NAM(m)%Absorber_Id(:) = Atm(m)%Absorber_Id(:)
      Atm_NAM(m)%Absorber_Units(:) = Atm(m)%Absorber_Units(:)
      ! Global
      Atm_GFS(m)%Climatology = Atm(m)%Climatology
      Atm_GFS(m)%Absorber_Id(:) = Atm(m)%Absorber_Id(:)
      Atm_GFS(m)%Absorber_Units(:) = Atm(m)%Absorber_Units(:)            
      ! Regional assignments of temperature and absorber amounts
      DO kr = 1, Atm_NAM(m)%n_Layers
        Pressure_Difference = PRESSURE_DIFFERENCE_FILL_VALUE
        DO k = 1, Atm(m)%n_Layers
          IF(ABS(Atm_NAM(m)%Pressure(kr)-Atm(m)%Pressure(k)) < &
             Pressure_Difference) THEN
            Pressure_Difference = ABS(Atm_NAM(m)%Pressure(kr)-Atm(m)%Pressure(k))
            Best_Match_Index = k
          END IF
        ENDDO
        Atm_NAM(m)%Temperature(kr) = Atm(m)%Temperature(Best_Match_Index)
        Atm_NAM(m)%Absorber(kr,1)  = Atm(m)%Absorber(Best_Match_Index,1)
        Atm_NAM(m)%Absorber(kr,2)  = Atm(m)%Absorber(Best_Match_Index,2)
      ENDDO
      ! Global assignments of temperature and absorber amounts
      DO kg = 1, Atm_GFS(m)%n_Layers
        Pressure_Difference = PRESSURE_DIFFERENCE_FILL_VALUE
        DO k = 1, Atm(m)%n_Layers
          IF(ABS(Atm_GFS(m)%Pressure(kg)-Atm(m)%Pressure(k)) < &
             Pressure_Difference) THEN
            Pressure_Difference = ABS(Atm_GFS(m)%Pressure(kg)-Atm(m)%Pressure(k)) 
            Best_Match_Index = k
          END IF
        ENDDO
        Atm_GFS(m)%Temperature(kg) = Atm(m)%Temperature(Best_Match_Index)
        Atm_GFS(m)%Absorber(kg,1)  = Atm(m)%Absorber(Best_Match_Index,1)
        Atm_GFS(m)%Absorber(kg,2)  = Atm(m)%Absorber(Best_Match_Index,2)
      ENDDO
    ENDDO

  END SUBROUTINE Map_To_NCEP_Model_Coordinates
