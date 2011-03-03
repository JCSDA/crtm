!
! Helper module conmtaining the reflection correction routines for the
! CRTM implementation of FASTEM4
!
!
! CREATION HISTORY:
!       Written by:     Original FASTEM1/2/3 authors
!
!       Modified by:    Quanhua Liu, Quanhua.Liu@noaa.gov
!                       Stephen English, Stephen.English@metoffice.gov.uk
!                       July, 2009
!
!       Refactored by:  Paul van Delst, October 2010
!                       paul.vandelst@noaa.gov
!

MODULE Reflection_Correction

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE CRTM_Parameters, ONLY: ZERO, ONE, TWO
  USE Slope_Variance , ONLY: svVar_type => iVar_type, &
                             Compute_Slope_Variance   , &
                             Compute_Slope_Variance_TL, &
                             Compute_Slope_Variance_AD 
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_R_Correction
  PUBLIC :: Compute_R_Correction_TL
  PUBLIC :: Compute_R_Correction_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'

  ! Fitting coefficients
  REAL(fp), PARAMETER :: t_c(45) = (/ &
  -0.675700E-01_fp, 0.214600E+00_fp,-0.363000E-02_fp, 0.636730E+01_fp, 0.900610E+00_fp, &
  -0.524880E+00_fp,-0.370920E+01_fp,-0.143310E+01_fp, 0.397450E+00_fp, 0.823100E-01_fp, &
  -0.255980E+00_fp, 0.552000E-02_fp, 0.208000E+01_fp, 0.244920E+01_fp,-0.456420E+00_fp, &
  -0.224900E-01_fp, 0.616900E-01_fp,-0.344000E-02_fp,-0.507570E+01_fp,-0.360670E+01_fp, &
   0.118750E+01_fp, 0.124950E+00_fp, 0.121270E+00_fp, 0.714000E-02_fp, 0.736620E+01_fp, &
  -0.114060E+00_fp,-0.272910E+00_fp,-0.504350E+01_fp,-0.336450E+00_fp, 0.161260E+00_fp, &
  -0.154290E+00_fp,-0.141070E+00_fp,-0.809000E-02_fp, 0.395290E+01_fp, 0.958580E+00_fp, &
  -0.159080E+00_fp, 0.368500E-01_fp, 0.307100E-01_fp, 0.810000E-03_fp,-0.619960E+01_fp, &
  -0.172580E+01_fp, 0.641360E+00_fp, 0.100000E+01_fp, 0.200000E-01_fp, 0.300000E+00_fp /)

  ! Rx_Rough regression equation parameters
  ! ...Number of predictors
  INTEGER, PARAMETER :: N_PREDICTORS = 9
  ! ...Number of summation loops and equation terms
  INTEGER, PARAMETER :: N_TERMS = 3
  ! ...Coefficients for Rh_Rough
  REAL(fp), PARAMETER :: CH(N_TERMS,N_PREDICTORS-2) = RESHAPE((/ &
    -0.675700E-01_fp, 0.214600E+00_fp,-0.363000E-02_fp, &
     0.636730E+01_fp, 0.900610E+00_fp,-0.524880E+00_fp, &
    -0.370920E+01_fp,-0.143310E+01_fp, 0.397450E+00_fp, &
     0.823100E-01_fp,-0.255980E+00_fp, 0.552000E-02_fp, &
     0.208000E+01_fp, 0.244920E+01_fp,-0.456420E+00_fp, &
    -0.224900E-01_fp, 0.616900E-01_fp,-0.344000E-02_fp, &
    -0.507570E+01_fp,-0.360670E+01_fp, 0.118750E+01_fp /), &
    (/N_TERMS,N_PREDICTORS-2/) )
  ! ...Coefficients for Rv_Rough
  REAL(fp), PARAMETER :: CV(N_TERMS,N_PREDICTORS-2) = RESHAPE((/ &
     0.124950E+00_fp, 0.121270E+00_fp, 0.714000E-02_fp, &
     0.736620E+01_fp,-0.114060E+00_fp,-0.272910E+00_fp, &
    -0.504350E+01_fp,-0.336450E+00_fp, 0.161260E+00_fp, &
    -0.154290E+00_fp,-0.141070E+00_fp,-0.809000E-02_fp, &
     0.395290E+01_fp, 0.958580E+00_fp,-0.159080E+00_fp, &
     0.368500E-01_fp, 0.307100E-01_fp, 0.810000E-03_fp, &
    -0.619960E+01_fp,-0.172580E+01_fp, 0.641360E+00_fp /), &
    (/N_TERMS,N_PREDICTORS-2/) )
    

  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Forward model input values
    REAL(fp) :: Transmittance = ZERO
    REAL(fp) :: Wind_Speed    = ZERO
    REAL(fp) :: cos_z         = ZERO
    REAL(fp) :: Frequency     = ZERO
    ! Optical depth
    REAL(fp) :: od = ZERO
    ! Predictors
    REAL(fp) :: zx(N_PREDICTORS) = ZERO
    ! Reflectance variables
    REAL(fp) :: Rv_Rough = ZERO
    REAL(fp) :: Rh_Rough = ZERO
    REAL(fp) :: Rv_Mod   = ZERO
    REAL(fp) :: Rh_Mod   = ZERO
    ! Components
    TYPE(svVar_type) :: svVar
  END TYPE iVar_type
  
CONTAINS


  ! ===============================================================
  ! Use the transmittance to compute anisotropic downward radiation 
  ! effect through a corrected surface reflection.
  ! ===============================================================
  
  ! Forward model
  SUBROUTINE Compute_R_Correction( &
    Transmittance, &  ! Input
    Wind_Speed   , &  ! Input
    cos_z        , &  ! Input
    Frequency    , &  ! Input
    iVar         , &  ! Internal variable output
    Rv_Mod       , &  ! Output
    Rh_Mod         )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Transmittance
    REAL(fp)       , INTENT(IN)  :: Wind_Speed   
    REAL(fp)       , INTENT(IN)  :: cos_z        
    REAL(fp)       , INTENT(IN)  :: Frequency    
    TYPE(iVar_type), INTENT(OUT) :: iVar
    REAL(fp)       , INTENT(OUT) :: Rv_Mod
    REAL(fp)       , INTENT(OUT) :: Rh_Mod
    ! Local variables
    REAL(fp) :: variance
    INTEGER :: i

    ! Save forward input variables for TL and AD calculations
    iVar%Transmittance = Transmittance 
    iVar%Wind_Speed    = Wind_Speed    
    iVar%cos_z         = cos_z         
    iVar%Frequency     = Frequency    

    ! Compute the wave slope variance
    CALL Compute_Slope_Variance( Frequency, Wind_Speed, iVar%svVar, Variance )

    ! Compute surface to space optical depth
    iVar%od = -LOG(Transmittance) * cos_z

    ! Define predictors for the effective angle calculation
    iVar%zx(1) = ONE
    iVar%zx(2) = variance
    iVar%zx(4) = ONE / cos_z
    iVar%zx(3) = iVar%zx(2) * iVar%zx(4)
    iVar%zx(5) = iVar%zx(3) * iVar%zx(3)
    iVar%zx(6) = iVar%zx(4) * iVar%zx(4)
    iVar%zx(7) = iVar%zx(2) * iVar%zx(2)
    iVar%zx(8) = LOG(iVar%od)
    iVar%zx(9) = iVar%zx(8) * iVar%zx(8)

    ! Compute the rough surface reflectivity
    iVar%Rv_Rough = ONE
    iVar%Rh_Rough = ONE
    DO i = 1, N_PREDICTORS-2
      iVar%Rh_Rough = iVar%Rh_Rough + iVar%zx(i) * (CH(1,i) + iVar%zx(8)*CH(2,i) + iVar%zx(9)*CH(3,i) )
      iVar%Rv_Rough = iVar%Rv_Rough + iVar%zx(i) * (CV(1,i) + iVar%zx(8)*CV(2,i) + iVar%zx(9)*CV(3,i) )
    END DO
    
    ! Compute the reflectivity modifier
    Rv_Mod = (ONE - Transmittance**iVar%Rv_Rough) / (ONE - Transmittance)
    Rh_Mod = (ONE - Transmittance**iVar%Rh_Rough) / (ONE - Transmittance) 
    ! ...and save it
    iVar%Rv_Mod = Rv_Mod
    iVar%Rh_Mod = Rh_Mod 

  END SUBROUTINE Compute_R_Correction


  ! Tangent-linear model
  SUBROUTINE Compute_R_Correction_TL( &
    Transmittance_TL, &  ! TL  Input
    Wind_Speed_TL   , &  ! TL  Input
    iVar            , &  ! Internal variable input
    Rv_Mod_TL       , &  ! TL  Output
    Rh_Mod_TL         )  ! TL  Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Transmittance_TL
    REAL(fp)       , INTENT(IN)  :: Wind_Speed_TL       
    TYPE(iVar_type), INTENT(IN)  :: iVar
    REAL(fp)       , INTENT(OUT) :: Rv_Mod_TL
    REAL(fp)       , INTENT(OUT) :: Rh_Mod_TL
    ! Local variables
    REAL(fp) :: variance_TL,od_TL,zx_TL(N_PREDICTORS),Rv_Rough_TL,Rh_Rough_TL
    INTEGER :: i
    
    ! Compute the wave slope variance
    CALL Compute_Slope_Variance_TL( Wind_Speed_TL, iVar%svVar, Variance_TL )

    ! Compute surface to space optical depth
    od_TL = -Transmittance_TL * iVar%cos_z / iVar%Transmittance

    ! Define predictors for the effective angle calculation
    zx_TL    = ZERO
    zx_TL(1) = ZERO
    zx_TL(2) = variance_TL
    zx_TL(4) = ZERO
    zx_TL(3) = zx_TL(2) * iVar%zx(4) 
    zx_TL(5) = TWO * zx_TL(3) * iVar%zx(3)
    zx_TL(6) = TWO * zx_TL(4) * iVar%zx(4)
    zx_TL(7) = TWO * zx_TL(2) * iVar%zx(2)
    zx_TL(8) = od_TL / iVar%od
    zx_TL(9) = TWO * zx_TL(8) * iVar%zx(8)

    ! Compute the rough surface reflectivity
    Rv_Rough_TL = ZERO
    Rh_Rough_TL = ZERO
    DO i = 1, N_PREDICTORS-2
      Rh_Rough_TL = Rh_Rough_TL + &
                    zx_TL(i)   * (CH(1,i) + iVar%zx(8)*CH(2,i) + iVar%zx(9)*CH(3,i)) + &
                    iVar%zx(i) * (zx_TL(8)*CH(2,i) + zx_TL(9)*CH(3,i))
      Rv_Rough_TL = Rv_Rough_TL + &
                    zx_TL(i)   * (CV(1,i) + iVar%zx(8)*CV(2,i) + iVar%zx(9)*CV(3,i)) + &
                    iVar%zx(i) * (zx_TL(8)*CV(2,i) + zx_TL(9)*CV(3,i))
    END DO

    ! Compute the reflectivity modifier
    Rv_Mod_TL = ((iVar%Rv_Mod - iVar%Rv_Rough*(iVar%Transmittance**(iVar%Rv_Rough-ONE)))*Transmittance_TL - &
                 (LOG(iVar%Transmittance)*(iVar%Transmittance**iVar%Rv_Rough))*Rv_Rough_TL) / &
                (ONE - iVar%Transmittance)
    Rh_Mod_TL = ((iVar%Rh_Mod - iVar%Rh_Rough*(iVar%Transmittance**(iVar%Rh_Rough-ONE)))*Transmittance_TL - &
                 (LOG(iVar%Transmittance)*(iVar%Transmittance**iVar%Rh_Rough))*Rh_Rough_TL) / &
                (ONE - iVar%Transmittance)
    
  END SUBROUTINE Compute_R_Correction_TL


  ! Adjoint model
  SUBROUTINE Compute_R_Correction_AD( &
    Rv_Mod_AD       , &  ! AD Input
    Rh_Mod_AD       , &  ! AD Input
    iVar            , &  ! Internal variable input
    Transmittance_AD, &  ! AD Output
    Wind_Speed_AD     )  ! AD Output
    ! Arguments
    REAL(fp)       , INTENT(IN OUT) :: Rv_Mod_AD
    REAL(fp)       , INTENT(IN OUT) :: Rh_Mod_AD
    TYPE(iVar_type), INTENT(IN)     :: iVar
    REAL(fp)       , INTENT(IN OUT) :: Transmittance_AD
    REAL(fp)       , INTENT(IN OUT) :: Wind_Speed_AD
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Rv_Rough_AD, Rh_Rough_AD
    REAL(fp) :: zx_AD(N_PREDICTORS)
    REAL(fp) :: od_AD
    REAL(fp) :: variance_AD


    ! Compute the reflectivity modifier
    Rv_Rough_AD = ZERO
    Rh_Rough_AD = ZERO
    ! ...for Rh_Mod
    Transmittance_AD = Transmittance_AD + &
                       (iVar%Rh_Mod - iVar%Rh_Rough * iVar%Transmittance**(iVar%Rh_Rough-ONE)) * Rh_Mod_AD / &
                       (ONE - iVar%Transmittance)
    Rh_Rough_AD = Rh_Rough_AD - &
                  (iVar%Transmittance**iVar%Rh_Rough * LOG(iVar%Transmittance)) * Rh_Mod_AD / &
                  (ONE - iVar%Transmittance)
    Rh_Mod_AD = ZERO
    ! ...for Rv_Mod
    Transmittance_AD = Transmittance_AD + &
                       (iVar%Rv_Mod - iVar%Rv_Rough * iVar%Transmittance**(iVar%Rv_Rough-ONE)) * Rv_Mod_AD / &
                       (ONE - iVar%Transmittance)
    Rv_Rough_AD = Rv_Rough_AD - &
                  (iVar%Transmittance**iVar%Rv_Rough * LOG(iVar%Transmittance)) * Rv_Mod_AD / &
                  (ONE - iVar%Transmittance)
    Rv_Mod_AD = ZERO

    ! Compute the rough surface reflectivity
    zx_AD = ZERO
    DO i = 1, N_PREDICTORS-2
      ! ...Rh_Rough components
      zx_AD(9) = zx_AD(9) + iVar%zx(i)*CH(3,i)*Rh_Rough_AD
      zx_AD(8) = zx_AD(8) + iVar%zx(i)*CH(2,i)*Rh_Rough_AD
      zx_AD(i) = zx_AD(i) + (CH(1,i) + iVar%zx(8)*CH(2,i) + iVar%zx(9)*CH(3,i))*Rh_Rough_AD
      ! ...Rv_Rough components
      zx_AD(9) = zx_AD(9) + iVar%zx(i)*CV(3,i)*Rv_Rough_AD
      zx_AD(8) = zx_AD(8) + iVar%zx(i)*CV(2,i)*Rv_Rough_AD
      zx_AD(i) = zx_AD(i) + (CV(1,i) + iVar%zx(8)*CV(2,i) + iVar%zx(9)*CV(3,i))*Rv_Rough_AD
    END DO
    Rv_Rough_AD = ZERO
    Rh_Rough_AD = ZERO

    ! Define predictors for the effective angle calculation
    ! ...(9)
    zx_AD(8) = zx_AD(8) + TWO*iVar%zx(8)*zx_AD(9)
    zx_AD(9) = ZERO

    ! ...(8)
    od_AD = zx_AD(8) / iVar%od
    zx_AD(8) = ZERO
    
    ! ...(7)
    zx_AD(2) = zx_AD(2) + TWO*iVar%zx(2)*zx_AD(7)
    zx_AD(7) = ZERO
    
    ! ...(6)
    zx_AD(4) = zx_AD(4) + TWO*iVar%zx(4)*zx_AD(6)
    zx_AD(6) = ZERO
    
    ! ...(5)
    zx_AD(3) = zx_AD(3) + TWO*iVar%zx(3)*zx_AD(5)
    zx_AD(5) = ZERO
    
    ! ...(3)
    zx_AD(2) = zx_AD(2) + iVar%zx(4)*zx_AD(3)
    zx_AD(3) = ZERO
    
    ! ...(4)
    zx_AD(4) = ZERO
    
    ! ...(2)
    variance_AD = zx_AD(2)
    zx_AD(2) = ZERO
    
    ! ...(1)
    zx_AD(1) = ZERO


    ! Compute surface to space optical depth
    Transmittance_AD = Transmittance_AD - od_AD*iVar%cos_z/iVar%Transmittance
    

    ! Compute the wave slope variance
    CALL Compute_Slope_Variance_AD( variance_AD, iVar%svVar, Wind_Speed_AD )

  END SUBROUTINE Compute_R_Correction_AD

END MODULE Reflection_Correction
