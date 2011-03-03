!
! Helper module containing the azimuth emissivity routines for the
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
!       Refactored by:  Paul van Delst, November 2010
!                       paul.vandelst@noaa.gov
!

MODULE Azimuth_Emissivity

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds     , ONLY: fp
  USE CRTM_Parameters, ONLY: ZERO, ONE, TWO, &
                             DEGREES_TO_RADIANS
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Data types
  PUBLIC :: iVar_type
  ! Science routines
  PUBLIC :: Compute_Azimuth_Emissivity
  PUBLIC :: Compute_Azimuth_Emissivity_TL
  PUBLIC :: Compute_Azimuth_Emissivity_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'

  ! Number of component predictors for harmonic coefficients
  INTEGER, PARAMETER :: N_PREDICTORS = 10

  ! Number of Stokes parameters, and their enumerations<->vector index
  INTEGER, PARAMETER :: N_STOKES = 4
  INTEGER, PARAMETER :: IV_IDX = 1  ! Vertical polarisation, Iv, Stokes component #1
  INTEGER, PARAMETER :: IH_IDX = 2  ! Horizontal polarisation, Ih, Stokes component #2
  INTEGER, PARAMETER :: U_IDX  = 3  ! +/- 45deg. plane of polarisation, U, Stokes component #3
  INTEGER, PARAMETER :: V_IDX  = 4  ! Circular polarisation, V, Stokes component #4
  
  ! The number of harmonics considered in the trignometric parameterisation
  INTEGER, PARAMETER :: N_HARMONICS = 3
  
  ! Fitting coefficients for the azimuth dependence of emissivity
  REAL(fp), PARAMETER :: AC(N_PREDICTORS, N_STOKES, N_HARMONICS) = RESHAPE( &
       ! COS(phi) and SIN(phi) coefficients  
    (/ 1.318143E-02_fp,-1.660586E-04_fp,-7.102244E-03_fp, 8.771616E-05_fp,-3.418311E-03_fp, &
       3.784895E-05_fp, 5.763184E-05_fp,-6.290578E-07_fp, 1.839451E-03_fp,-1.856317E-05_fp, &
       6.459324E-03_fp,-7.570050E-05_fp,-3.777932E-03_fp, 4.270676E-05_fp,-1.247285E-03_fp, &
       1.136239E-05_fp, 2.123934E-05_fp,-2.377368E-07_fp, 7.070105E-04_fp,-5.092876E-06_fp, &
      -6.296038E-03_fp, 3.835747E-05_fp, 3.013694E-03_fp,-9.366178E-06_fp, 1.680703E-03_fp, &
      -5.745778E-06_fp,-2.942056E-05_fp, 1.889216E-07_fp,-9.058433E-04_fp,-1.136992E-06_fp, &
      -5.854263E-04_fp, 5.546263E-06_fp, 2.485058E-04_fp,-1.531698E-06_fp, 1.243394E-04_fp, &
      -1.575561E-06_fp,-2.437488E-06_fp, 2.986237E-08_fp,-5.555700E-05_fp, 6.076001E-07_fp, &
       ! COS(2*phi) and SIN(2*phi) coefficients  
       4.605486E-03_fp, 5.781246E-05_fp,-2.746737E-03_fp,-4.690045E-05_fp, 1.512049E-04_fp, &
      -7.411844E-09_fp,-3.476559E-06_fp, 1.466902E-07_fp,-6.472364E-05_fp,-1.776898E-06_fp, &
      -1.863094E-02_fp, 2.768660E-04_fp, 7.624930E-03_fp,-1.397481E-04_fp, 3.550912E-03_fp, &
      -5.533696E-05_fp,-6.557083E-05_fp, 9.948138E-07_fp,-1.626538E-03_fp, 2.307157E-05_fp, &
      -2.880306E-02_fp, 2.418851E-04_fp, 1.290535E-02_fp,-8.803702E-05_fp, 5.057109E-06_fp, &
      -2.715428E-05_fp,-6.912266E-05_fp, 7.852767E-07_fp, 5.337096E-04_fp, 6.585635E-06_fp, &
       6.042016E-03_fp,-1.135219E-04_fp,-2.231061E-03_fp, 5.729232E-05_fp,-1.543391E-03_fp, &
       2.288614E-05_fp, 2.828443E-05_fp,-4.384802E-07_fp, 7.080137E-04_fp,-9.827192E-06_fp, &
       ! COS(3*phi) and SIN(3*phi) coefficients  
       1.205735E-03_fp,-1.748276E-05_fp,-6.002919E-04_fp, 1.174144E-05_fp,-1.735732E-04_fp, &
       2.148296E-06_fp, 2.955853E-06_fp,-3.609258E-08_fp, 9.669164E-05_fp,-1.282544E-06_fp, & 
      -7.610401E-04_fp, 1.293120E-05_fp, 3.796897E-04_fp,-5.562741E-06_fp, 8.865672E-05_fp, &
      -1.313724E-06_fp, 7.009076E-08_fp, 2.426378E-08_fp,-8.192732E-05_fp, 5.333771E-07_fp, &
      -1.834561E-03_fp, 2.896784E-05_fp, 7.613927E-04_fp,-1.367783E-05_fp, 4.887281E-04_fp, &
      -5.810380E-06_fp,-9.568319E-06_fp, 1.207029E-07_fp,-2.210790E-04_fp, 2.159904E-06_fp, &      
      -2.054959E-04_fp, 1.806305E-07_fp, 1.144686E-04_fp, 4.638982E-07_fp, 3.581176E-05_fp, &
      -3.870976E-07_fp,-6.861957E-07_fp, 6.989780E-09_fp,-1.526136E-05_fp, 1.887424E-07_fp /), &
    SHAPE(AC) )

  
  ! --------------------------------------
  ! Structure definition to hold internal
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: iVar_type
    PRIVATE
    ! Direct inputs
    REAL(fp) :: wind_speed = ZERO
    REAL(fp) :: frequency  = ZERO
    ! Derived inputs
    REAL(fp) :: sec_z = ZERO
    ! Cosine and sine of the harmonic angle, m*phi
    REAL(fp) :: cos_angle(N_HARMONICS) = ZERO
    REAL(fp) :: sin_angle(N_HARMONICS) = ZERO
    ! Intermediate variables
    REAL(fp) :: trig_coeff(N_STOKES, N_HARMONICS) = ZERO
   END TYPE iVar_type

  
CONTAINS


  ! ===========================================================
  ! Compute emissivity as a function of relative azimuth angle.
  ! ===========================================================
  
  ! Forward model
  SUBROUTINE Compute_Azimuth_Emissivity( &
    Wind_Speed   , &  ! Input
    Azimuth_Angle, &  ! Input
    Frequency    , &  ! Input
    cos_z        , &  ! Input
    iVar         , &  ! Internal variable output
    e_Azimuth      )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Wind_Speed   
    REAL(fp)       , INTENT(IN)  :: Azimuth_Angle
    REAL(fp)       , INTENT(IN)  :: Frequency    
    REAL(fp)       , INTENT(IN)  :: cos_z        
    TYPE(iVar_type), INTENT(OUT) :: iVar
    REAL(fp)       , INTENT(OUT) :: e_Azimuth(:)
    ! Local variables
    INTEGER :: m, l1
    REAL(fp) :: phi, angle
    REAL(fp) :: predictor(N_PREDICTORS)
  
    ! Initialise output
    e_Azimuth = ZERO
  
    ! Save inputs for TL and AD calls
    iVar%wind_speed = Wind_Speed
    iVar%frequency  = Frequency
    iVar%sec_z      = ONE/cos_z
    
    ! Convert angle
    phi = Azimuth_Angle * DEGREES_TO_RADIANS

    ! Compute the azimuth emissivity component predictors
    CALL Compute_Predictors( Wind_Speed, Frequency, iVar%sec_z, Predictor )

    ! Compute the azimuth emissivity vector
    DO m = 1, N_HARMONICS
      l1 = 10*(m-1)

      angle = REAL(m,fp) * phi
      iVar%cos_angle(m) = COS(angle)
      iVar%sin_angle(m) = SIN(angle)

      ! Vertical polarisation, Iv, Stokes component #1
      CALL Compute_Coefficient( &
             Predictor, &
             IV_IDX, m, &
             iVar%trig_coeff(IV_IDX,m) )    
      e_Azimuth(IV_IDX) = e_Azimuth(IV_IDX) + iVar%trig_coeff(IV_IDX,m)*iVar%cos_angle(m)
      
      ! Horizontal polarisation, Ih, Stokes component #2
      CALL Compute_Coefficient( &
             Predictor, &
             IH_IDX, m, &
             iVar%trig_coeff(IH_IDX,m) ) 
      e_Azimuth(IH_IDX) = e_Azimuth(IH_IDX) + iVar%trig_coeff(IH_IDX,m)*iVar%cos_angle(m)
      
      ! +/- 45deg. plane of polarisation, U, Stokes component #3
      CALL Compute_Coefficient( &
             Predictor, &
             U_IDX, m , &
             iVar%trig_coeff(U_IDX,m) )
      e_Azimuth(U_IDX) = e_Azimuth(U_IDX) + iVar%trig_coeff(U_IDX,m)*iVar%sin_angle(m)
      
      ! Circular polarisation, V, Stokes component #4
      CALL Compute_Coefficient( &
             Predictor, &
             V_IDX, m , &
             iVar%trig_coeff(V_IDX,m) )
      e_Azimuth(V_IDX) = e_Azimuth(V_IDX) + iVar%trig_coeff(V_IDX,m)*iVar%sin_angle(m)

    END DO

    ! Apply frequency correction 
    e_Azimuth = e_Azimuth * Azimuth_Freq_Correction(Frequency)

  END SUBROUTINE Compute_Azimuth_Emissivity


  ! Tangent-linear model
  SUBROUTINE Compute_Azimuth_Emissivity_TL( &
    Wind_Speed_TL   , &  ! Input
    Azimuth_Angle_TL, &  ! Input
    iVar            , &  ! Internal variable input
    e_Azimuth_TL      )  ! Output
    ! Arguments
    REAL(fp)       , INTENT(IN)  :: Wind_Speed_TL
    REAL(fp)       , INTENT(IN)  :: Azimuth_Angle_TL
    TYPE(iVar_type), INTENT(IN)  :: iVar
    REAL(fp)       , INTENT(OUT) :: e_Azimuth_TL(:)
    ! Local variables
    REAL(fp) :: phi_TL
    REAL(fp) :: angle_TL
    REAL(fp) :: trig_coeff_TL
    REAL(fp) :: predictor_TL(N_PREDICTORS)
    INTEGER :: m, l1
    
    ! Initialise output
    e_Azimuth_TL = ZERO

    ! Compute angle perturbation in radians    
    phi_TL = Azimuth_Angle_TL * DEGREES_TO_RADIANS
    
    ! Compute the azimuth emissivity component tangent-linear predictors
    CALL Compute_Predictors_TL( iVar%wind_speed, iVar%frequency, iVar%sec_z, Wind_Speed_TL, Predictor_TL )

    ! Compute the tangent-linear azimuth emissivity vector
    DO m = 1, N_HARMONICS
      l1 = 10*(m-1)
      
      angle_TL = REAL(m,fp) * phi_TL

      ! Vertical polarisation, Iv, Stokes component #1
      CALL Compute_Coefficient_TL( &
             Predictor_TL , &
             IV_IDX, m    , &
             trig_coeff_TL  )               
      e_Azimuth_TL(IV_IDX) = e_Azimuth_TL(IV_IDX) + iVar%cos_angle(m)*trig_coeff_TL - &
                                                    iVar%trig_coeff(IV_IDX,m)*iVar%sin_angle(m)*angle_TL

      ! Horizontal polarisation, Ih, Stokes component #2
      CALL Compute_Coefficient_TL( &
             Predictor_TL , &
             IH_IDX, m    , &
             trig_coeff_TL  ) 
      e_Azimuth_TL(IH_IDX) = e_Azimuth_TL(IH_IDX) + iVar%cos_angle(m)*trig_coeff_TL - &
                                                    iVar%trig_coeff(IH_IDX,m)*iVar%sin_angle(m)*angle_TL
      
      ! +/- 45deg. plane of polarisation, U, Stokes component #3
      CALL Compute_Coefficient_TL( &
             Predictor_TL , &
             U_IDX, m     , &
             trig_coeff_TL  ) 
      e_Azimuth_TL(U_IDX) = e_Azimuth_TL(U_IDX) + iVar%sin_angle(m)*trig_coeff_TL + &
                                                  iVar%trig_coeff(U_IDX,m)*iVar%cos_angle(m)*angle_TL 

      ! Circular polarisation, V, Stokes component #4
      CALL Compute_Coefficient_TL( &
             Predictor_TL , &
             V_IDX, m     , &
             trig_coeff_TL  )      
      e_Azimuth_TL(V_IDX) = e_Azimuth_TL(V_IDX) + iVar%sin_angle(m)*trig_coeff_TL + &
                                                  iVar%trig_coeff(V_IDX,m)*iVar%cos_angle(m)*angle_TL
    END DO

    ! Apply tangent-linear frequency correction
    e_Azimuth_TL = e_Azimuth_TL * Azimuth_Freq_Correction(iVar%frequency)

  END SUBROUTINE Compute_Azimuth_Emissivity_TL


  ! Adjoint model
  SUBROUTINE Compute_Azimuth_Emissivity_AD( &
    e_Azimuth_AD    , &  ! AD Input
    iVar            , &  ! Internal variable input
    Wind_Speed_AD   , &  ! AD Output
    Azimuth_Angle_AD  )  ! AD Output
    ! Arguments
    REAL(fp)       , INTENT(IN OUT) :: e_Azimuth_AD(:)
    TYPE(iVar_type), INTENT(IN)     :: iVar
    REAL(fp)       , INTENT(IN OUT) :: Wind_Speed_AD
    REAL(fp)       , INTENT(IN OUT) :: Azimuth_Angle_AD
    ! Local variables
    REAL(fp) :: phi_AD
    REAL(fp) :: angle_AD
    REAL(fp) :: trig_coeff_AD
    REAL(fp) :: predictor_AD(N_PREDICTORS)
    INTEGER :: m, l1

    ! Initialise local adjoints
    phi_AD       = ZERO
    predictor_AD = ZERO
    
    ! Adjoint of frequency correction
    e_Azimuth_AD = e_Azimuth_AD * Azimuth_Freq_Correction(iVar%frequency)
    
    ! Compute the azimuth emissivity vector adjoint
    DO m = 1, N_HARMONICS
      l1 = 10*(m-1)
      
      ! Circular polarisation, V, Stokes component #4
      angle_AD      = iVar%trig_coeff(V_IDX,m)*iVar%cos_angle(m)*e_Azimuth_AD(V_IDX)
      trig_coeff_AD = iVar%sin_angle(m)*e_Azimuth_AD(V_IDX)
      CALL Compute_Coefficient_AD( &
             trig_coeff_AD, &
             V_IDX, m     , &
             predictor_AD   )

      ! +/- 45deg. plane of polarisation, U, Stokes component #3
      angle_AD      = angle_AD + iVar%trig_coeff(U_IDX,m)*iVar%cos_angle(m)*e_Azimuth_AD(U_IDX)
      trig_coeff_AD = iVar%sin_angle(m)*e_Azimuth_AD(U_IDX)
      CALL Compute_Coefficient_AD( &
             trig_coeff_AD, &
             U_IDX, m     , &
             predictor_AD   )

      ! Horizontal polarisation, Ih, Stokes component #2
      angle_AD      = angle_AD - iVar%trig_coeff(IH_IDX,m)*iVar%sin_angle(m)*e_Azimuth_AD(IH_IDX)
      trig_coeff_AD = iVar%cos_angle(m)*e_Azimuth_AD(IH_IDX)
      CALL Compute_Coefficient_AD( &
             trig_coeff_AD, &
             IH_IDX, m    , &
             predictor_AD   )

      ! Vertical polarisation, Iv, Stokes component #1
      angle_AD      = angle_AD - iVar%trig_coeff(IV_IDX,m)*iVar%sin_angle(m)*e_Azimuth_AD(IV_IDX)
      trig_coeff_AD = iVar%cos_angle(m)*e_Azimuth_AD(IV_IDX)
      CALL Compute_Coefficient_AD( &
             trig_coeff_AD, &
             IV_IDX, m    , &
             predictor_AD   )

      phi_AD = phi_AD + REAL(m,fp)*angle_AD
      
    END DO
    
    ! Compute the azimuth emissivity component predictor adjoint
    CALL Compute_Predictors_AD( iVar%wind_speed, iVar%frequency, iVar%sec_z, predictor_AD, Wind_Speed_AD )

    ! Adjoint of the angle perturbation in radians
    Azimuth_Angle_AD = Azimuth_Angle_AD + phi_AD*DEGREES_TO_RADIANS
    
  END SUBROUTINE Compute_Azimuth_Emissivity_AD


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! =============================================
  ! Compute predictors for the azimuth components
  ! =============================================
  
  ! Forward model
  SUBROUTINE Compute_Predictors( &
    Wind_Speed, &  ! Input
    Frequency , &  ! Input
    sec_z     , &  ! Input
    Predictor   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(IN)  :: Frequency 
    REAL(fp), INTENT(IN)  :: sec_z     
    REAL(fp), INTENT(OUT) :: Predictor(N_PREDICTORS) 
    ! Compute the predictors.
    Predictor( 1) = ONE
    Predictor( 2) = Frequency
    Predictor( 3) = sec_z
    Predictor( 4) = sec_z * Frequency
    Predictor( 5) = Wind_Speed
    Predictor( 6) = Wind_Speed * Frequency
    Predictor( 7) = Wind_Speed**2
    Predictor( 8) = Frequency * Wind_Speed**2
    Predictor( 9) = Wind_Speed * sec_z
    Predictor(10) = Wind_Speed * sec_z * Frequency
  END SUBROUTINE Compute_Predictors
    
  
  ! Tangent-linear model  
  SUBROUTINE Compute_Predictors_TL(  &
    Wind_Speed   , &  ! FWD Input
    Frequency    , &  ! FWD Input
    sec_z        , &  ! FWD Input
    Wind_Speed_TL, &  ! TL  Input
    Predictor_TL   )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Wind_Speed
    REAL(fp), INTENT(IN)  :: Frequency 
    REAL(fp), INTENT(IN)  :: sec_z     
    REAL(fp), INTENT(IN)  :: Wind_Speed_TL
    REAL(fp), INTENT(OUT) :: Predictor_TL(N_PREDICTORS)
    ! Compute the tangent-linear predictors.
    Predictor_TL( 1) = ZERO
    Predictor_TL( 2) = ZERO
    Predictor_TL( 3) = ZERO
    Predictor_TL( 4) = ZERO
    Predictor_TL( 5) = Wind_Speed_TL
    Predictor_TL( 6) = Frequency * Wind_Speed_TL
    Predictor_TL( 7) = TWO * Wind_Speed * Wind_Speed_TL
    Predictor_TL( 8) = TWO * Frequency * Wind_Speed * Wind_Speed_TL
    Predictor_TL( 9) = sec_z * Wind_Speed_TL
    Predictor_TL(10) = sec_z * Frequency * Wind_Speed_TL
  END SUBROUTINE Compute_Predictors_TL
    
  
  ! Adjoint model
  SUBROUTINE Compute_Predictors_AD(  &
    Wind_Speed   , &  ! FWD Input
    Frequency    , &  ! FWD Input
    sec_z        , &  ! FWD Input
    Predictor_AD , &  ! AD  Input
    Wind_Speed_AD  )  ! AD  Output
    ! Arguments
    REAL(fp), INTENT(IN)     :: Wind_Speed
    REAL(fp), INTENT(IN)     :: Frequency 
    REAL(fp), INTENT(IN)     :: sec_z     
    REAL(fp), INTENT(IN OUT) :: Predictor_AD(N_PREDICTORS)
    REAL(fp), INTENT(IN OUT) :: Wind_Speed_AD
    ! Compute the predictor adjoints
    Wind_Speed_AD = Wind_Speed_AD + &
                    sec_z * Frequency            * Predictor_AD(10) + &
                    sec_z                        * Predictor_AD( 9) + &
                    TWO * Frequency * Wind_Speed * Predictor_AD( 8) + &
                    TWO             * Wind_Speed * Predictor_AD( 7) + &
                          Frequency              * Predictor_AD( 6) + &
                                                   Predictor_AD( 5)
    Predictor_AD = ZERO
  END SUBROUTINE Compute_Predictors_AD
    
    
  ! ==============================================================
  ! Compute the component coefficient from the regression equation
  ! ==============================================================
  
  ! Forward model
  SUBROUTINE Compute_Coefficient( &
    Predictor   , &  ! Input
    idx_Stokes  , &  ! Input
    idx_Harmonic, &  ! Input
    Coefficient   )  ! Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Predictor(N_PREDICTORS)
    INTEGER , INTENT(IN)  :: idx_Stokes  
    INTEGER , INTENT(IN)  :: idx_Harmonic
    REAL(fp), INTENT(OUT) :: Coefficient
    ! Local variables
    INTEGER :: i
    ! Compute component coefficient
    Coefficient = ZERO
    DO i = 1, N_PREDICTORS
      Coefficient = Coefficient + AC(i,idx_Stokes,idx_Harmonic)*Predictor(i)
    END DO
  END SUBROUTINE Compute_Coefficient


  ! Tangent-linear model
  SUBROUTINE Compute_Coefficient_TL( &
    Predictor_TL  , &  ! TL  Input
    idx_Stokes    , &  ! Input
    idx_Harmonic  , &  ! Input
    Coefficient_TL  )  ! TL  Output
    ! Arguments
    REAL(fp), INTENT(IN)  :: Predictor_TL(N_PREDICTORS)
    INTEGER , INTENT(IN)  :: idx_Stokes  
    INTEGER , INTENT(IN)  :: idx_Harmonic
    REAL(fp), INTENT(OUT) :: Coefficient_TL
    ! Local variables
    INTEGER :: i
    ! Compute tangent-linear coefficient
    Coefficient_TL = ZERO
    DO i = 1, N_PREDICTORS
      Coefficient_TL = Coefficient_TL + AC(i,idx_Stokes,idx_Harmonic)*Predictor_TL(i)
    END DO
  END SUBROUTINE Compute_Coefficient_TL


  ! Adjoint model
  SUBROUTINE Compute_Coefficient_AD( &
    Coefficient_AD, &  ! AD Input
    idx_Stokes    , &  ! Input
    idx_Harmonic  , &  ! Input
    Predictor_AD    )  ! AD Output
    ! Arguments
    REAL(fp), INTENT(IN OUT) :: Coefficient_AD
    INTEGER , INTENT(IN)  :: idx_Stokes  
    INTEGER , INTENT(IN)  :: idx_Harmonic
    REAL(fp), INTENT(IN OUT) :: Predictor_AD(N_PREDICTORS)
    ! Local variables
    INTEGER :: i
    ! Compute coefficient adjoint
    DO i = 1, N_PREDICTORS
      Predictor_AD(i) = Predictor_AD(i) + AC(i,idx_Stokes,idx_Harmonic)*Coefficient_AD
    END DO
    Coefficient_AD = ZERO
  END SUBROUTINE Compute_Coefficient_AD




  PURE FUNCTION  Azimuth_Freq_Correction( Frequency ) RESULT( Fre_C )
    IMPLICIT NONE
    REAL( fp ), INTENT(IN) :: Frequency
    REAL( fp ) :: Fre_C
    INTEGER :: i
      ! Data for the frequency correction
    REAL(fp), PARAMETER :: x(9) = (/ 0.0_fp, 1.4_fp, 6.8_fp, 10.7_fp, 19.35_fp, &
                                   37._fp, 89._fp, 150._fp, 200._fp/)
    REAL(fp), PARAMETER :: y(9) = (/ 0.0_fp, 0.1_fp, 0.6_fp, 0.9_fp, 1._fp, &
                                   1.0_fp, 0.4_fp, 0.2_fp, 0.0_fp/)
    ! 
    IF( Frequency <= ZERO .or. Frequency >= 200.0_fp ) THEN
      Fre_C = ZERO
      RETURN
    ELSE
      DO i = 1, 8
        IF( Frequency >= x(i) .and. Frequency <= x(i+1) ) THEN
          Fre_C = y(i) + (y(i+1)-y(i))/(x(i+1)-x(i))*(Frequency-x(i))
          RETURN
        END IF
      END DO
    END IF
    Fre_C = ZERO
    
  END FUNCTION  Azimuth_Freq_Correction

END MODULE Azimuth_Emissivity
