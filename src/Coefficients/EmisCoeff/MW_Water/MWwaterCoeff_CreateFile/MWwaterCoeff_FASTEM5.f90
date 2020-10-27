MODULE MWwaterCoeff_FASTEM5

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE Message_Handler    , ONLY: SUCCESS, Display_Message
  USE FitCoeff_Define    , ONLY: FitCoeff_SetValue
  USE MWwaterLUT_Define  , ONLY: MWwaterLUT_Destroy, &
                                 MWwaterLUT_ReadFile
  USE MWwaterCoeff_Define, ONLY: MWwaterCoeff_type, &
                                 MWwaterCoeff_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MWwaterCoeff_LoadCoeffs
  PUBLIC :: MWwaterCoeff_LoadVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &

  ! No. of Stokes components
  INTEGER , PARAMETER :: N_STOKES = 4
  INTEGER , PARAMETER :: N_2STOKES = 2  ! For data with just V and H polarisations

  ! Foam coverage coefficients from equation (5) of
  !
  !   Monahan, E.C., and O'Muircheartaigh, I.G., (1986)
  !     Whitecaps and the passive remote sensing of the ocean surface,
  !     International Journal of Remote Sensing, 7, pp627-642.
  !
  ! but assuming a neutral stability condition, i.e. dT=0, no
  ! difference between the skin and air temperature.
  INTEGER , PARAMETER :: N_FCCOEFFS = 2
  REAL(fp), PARAMETER :: FCCOEFF(N_FCCOEFFS) = (/1.95E-05_fp, 2.55_fp/)


  ! Foam reflectivity coefficients from section d of
  !
  !   Kazumori, M. et al. (2008) Impact Study of AMSR-E Radiances
  !     in the NCEP Global Data Assimilation System,
  !     Monthly Weather Review, 136, pp541-559
  !
  ! A constant value, 0.93, is used as the nadir emissivity, as
  ! well as for all incident angles for vertical polarisation.
  ! The relationship for horizontal polarisation is as listed
  ! in equation (15), same as the Stogryn(1972) model, but for
  ! the new nadir emissivity.
  !
  ! The current frequency dependent term is empirical, and undocumented.
  INTEGER , PARAMETER :: N_FRCOEFFS = 6
  REAL(fp), PARAMETER :: FRCOEFF(N_FRCOEFFS) = &
  (/ 0.93_fp, &                                 ! Nadir emissivity
    -1.748e-3_fp, -7.336e-5_fp, 1.044e-7_fp, &  ! Horizontal polarisation coefficients
     0.4_fp, -0.05_fp /)                        ! Frequency correction coefficients


  ! Small-scale correction coefficients for equation (A4) of
  !
  !   Liu, Q. et al. (2011) An Improved Fast Microwave Water
  !     Emissivity Model, TGRSS, 49, pp1238-1250
  !
  ! where the small-scale correction formulation is given in
  ! equation (17a,b) of
  !
  !   Liu, Q. et al. (1998) Monte Carlo simulations of the microwave
  !     emissivity of the sea surface, JGR, 103, pp24983-24989
  !
  ! and originally in equation (30) of
  !
  !   Guissard,A. and P.Sobieski (1987) An approximate model
  !     for the microwave brightness temperature of the sea,
  !     Int.J.Rem.Sens., 8, pp1607-1627.
  INTEGER , PARAMETER :: N_SSCCOEFFS = 8
  REAL(fp), PARAMETER :: SSCCOEFF(N_SSCCOEFFS) = &
  (/ -5.0208480E-06_fp,   2.3297951E-08_fp,   4.6625726E-08_fp,  -1.9765665E-09_fp, &
     -7.0469823E-04_fp,   7.5061193E-04_fp,   9.8103876E-04_fp,   1.5489504E-04_fp /)


  ! Large-scale correction coefficients for equations (A5a,b) of
  !
  !   Liu, Q. et al. (2011) An Improved Fast Microwave Water
  !     Emissivity Model, TGRSS, 49, pp1238-1250
  INTEGER , PARAMETER :: N_FCOEFFS   = 3 ! No. of frequency polynomial coefficients
  INTEGER , PARAMETER :: N_LSCCOEFFS = 6 ! No. of regression coefficients
  REAL(fp), PARAMETER :: LSCCOEFF(N_FCOEFFS, N_LSCCOEFFS, N_2STOKES) = RESHAPE( &
  (/ -5.994667E-02_fp, 9.341346E-04_fp,-9.566110E-07_fp, 8.360313E-02_fp,-1.085991E-03_fp, 6.735338E-07_fp, &
     -2.617296E-02_fp, 2.864495E-04_fp,-1.429979E-07_fp,-5.265879E-04_fp, 6.880275E-05_fp,-2.916657E-07_fp, &
     -1.671574E-05_fp, 1.086405E-06_fp,-3.632227E-09_fp, 1.161940E-04_fp,-6.349418E-05_fp, 2.466556E-07_fp, &

     -2.431811E-02_fp,-1.031810E-03_fp, 4.519513E-06_fp, 2.868236E-02_fp, 1.186478E-03_fp,-5.257096E-06_fp, &
     -7.933390E-03_fp,-2.422303E-04_fp, 1.089605E-06_fp,-1.083452E-03_fp,-1.788509E-05_fp, 5.464239E-09_fp, &
     -3.855673E-05_fp, 9.360072E-07_fp,-2.639362E-09_fp, 1.101309E-03_fp, 3.599147E-05_fp,-1.043146E-07_fp /), &
  (/N_FCOEFFS, N_LSCCOEFFS, N_2STOKES/) )


  ! Reflection correction coefficients
  INTEGER , PARAMETER :: N_ODCOEFFS = 3 ! No. of optical depth coefficients
  INTEGER , PARAMETER :: N_RCCOEFFS = 7 ! No. of regression coefficients
  REAL(fp), PARAMETER :: RCCOEFF(N_ODCOEFFS, N_RCCOEFFS, N_2STOKES) = RESHAPE( &
  (/  0.388242E-01_fp, 0.194901E+00_fp,-0.425093E-01_fp, &
      0.607698E+01_fp,-0.313861E+01_fp,-0.103383E+01_fp, &
     -0.377867E+01_fp, 0.180284E+01_fp, 0.699556E+00_fp, &
     -0.506455E-01_fp,-0.262822E+00_fp, 0.703056E-01_fp, &
      0.362055E+01_fp,-0.120318E+01_fp,-0.124971E+01_fp, &
      0.154014E-01_fp, 0.759848E-01_fp,-0.268604E-01_fp, &
     -0.802073E+01_fp, 0.324658E+01_fp, 0.304165E+01_fp, &

      0.199277E+00_fp, 0.166155E+00_fp, 0.153272E-01_fp, &
      0.399234E+01_fp,-0.130968E+01_fp,-0.874716E+00_fp, &
     -0.169403E+01_fp,-0.260998E-01_fp, 0.540443E+00_fp, &
     -0.282483E+00_fp,-0.219994E+00_fp,-0.203438E-01_fp, &
      0.351731E+00_fp, 0.208641E+01_fp,-0.693299E+00_fp, &
      0.867861E-01_fp, 0.619020E-01_fp, 0.595251E-02_fp, &
     -0.475191E+01_fp,-0.430134E-01_fp, 0.248524E+01_fp /), &
  (/N_ODCOEFFS, N_RCCOEFFS, N_2STOKES/) )


  ! Fitting coefficients for the azimuth dependence of emissivity
  INTEGER , PARAMETER :: N_AZCOEFFS = 10 ! No. of azimuth emissivity coefficients
  INTEGER , PARAMETER :: N_HARMONICS = 3 ! No. of harmonics considered in the trignometric parameterisation
  REAL(fp), PARAMETER :: AZCOEFF(N_AZCOEFFS, N_STOKES, N_HARMONICS) = RESHAPE( &
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
    (/N_AZCOEFFS, N_STOKES, N_HARMONICS/) )


CONTAINS


  SUBROUTINE MWwaterCoeff_LoadCoeffs( MWwaterCoeff )
    TYPE(MWwaterCoeff_type), INTENT(IN OUT) :: MWwaterCoeff
    INTEGER :: err_stat
    INTEGER :: version

    version = 5

    ! Create an instance of the structure
    CALL MWwaterCoeff_Create(MWwaterCoeff)
    MWwaterCoeff%Version = version


    ! Load the foam coverage coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%FCCoeff, FCCOEFF, Version=version)

    ! Load the foam reflectivity coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%FRCoeff, FRCOEFF, Version=version)

    ! Load the reflection correction coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%RCCoeff, RCCOEFF, Version=version)

    ! Load the azimuth emissivity coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%AZCoeff, AZCOEFF, Version=version)

    ! Load the small-scale correction coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%SSCCoeff, SSCCOEFF, Version=version)

    ! Load the large-scale correction coefficients
    CALL FitCoeff_SetValue(MWwaterCoeff%LSCCoeff, LSCCOEFF, Version=version)

    ! Load the emissivity look-up table
    err_stat = MWwaterLUT_ReadFile(MWwaterCoeff%LUT, 'MWwaterLUT.bin')
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message('MWwaterCoeff_LoadCoeffs','Error reading MWwaterLUT data',err_stat)
      CALL MWwaterLUT_Destroy(MWwaterCoeff%LUT)
    END IF

  END SUBROUTINE MWwaterCoeff_LoadCoeffs


  SUBROUTINE MWwaterCoeff_LoadVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWwaterCoeff_LoadVersion

END MODULE MWwaterCoeff_FASTEM5
