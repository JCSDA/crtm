MODULE MWwaterCoeff_FASTEM6

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE Message_Handler    , ONLY: SUCCESS, Display_Message
  USE FitCoeff_Define    , ONLY: FitCoeff_SetValue
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


  ! Coefficients for M.Kazumori azimuth model function
  INTEGER , PARAMETER :: N_AZCOEFFS      = 6 ! No. of azimuth emissivity coefficients
  INTEGER , PARAMETER :: N_AZFREQUENCIES = 6 ! No. of fitting frequencies
  REAL(fp), PARAMETER :: AZCOEFF(N_AZCOEFFS, N_AZFREQUENCIES, N_2STOKES) = RESHAPE( &
  [ 4.401E-02_fp, -1.636E+01_fp,  1.478E+00_fp, -4.800E-02_fp,  3.202E-06_fp, -6.002E-05_fp, &     ! 06V OK
    4.379E-02_fp, -1.633E+01_fp,  1.453E+00_fp, -4.176E-02_fp,  5.561E-06_fp, -4.644E-05_fp, &     ! 10V OK
    5.009E-02_fp, -1.638E+01_fp,  1.520E+00_fp, -3.994E-02_fp,  1.330E-05_fp,  1.113E-05_fp, &     ! 19V OK
    5.165E-02_fp, -1.638E+01_fp,  1.543E+00_fp, -4.066E-02_fp,  1.494E-05_fp,  1.010E-05_fp, &     ! 23V interpolated
    5.553E-02_fp, -1.638E+01_fp,  1.602E+00_fp, -4.246E-02_fp,  1.903E-05_fp,  7.524E-06_fp, &     ! 37V OK
   -9.131E-05_fp,  1.251E+00_fp,  6.769E-01_fp, -2.913E-02_fp,  1.092E+00_fp, -1.806E-04_fp, &     ! 89V OK revised
   -1.234E-07_fp, -8.179E-03_fp, -1.040E+01_fp,  4.477E-01_fp,  0.000E+00_fp,  3.390E-05_fp, &     ! 06H OK
   -1.938E-05_fp, -8.007E-03_fp, -1.039E+01_fp,  4.610E-01_fp,  0.000E+00_fp,  4.419E-05_fp, &     ! 10H OK
    1.362E-04_fp, -1.013E-03_fp, -9.235E+00_fp,  3.844E-01_fp,  0.000E+00_fp,  2.891E-04_fp, &     ! 19H OK
    1.519E-04_fp, -7.865E-04_fp, -9.234E+00_fp,  3.884E-01_fp,  0.000E+00_fp,  6.856E-04_fp, &     ! 23H Interpolated
    1.910E-04_fp, -2.224E-04_fp, -9.232E+00_fp,  3.982E-01_fp,  0.000E+00_fp,  1.673E-03_fp, &     ! 37H OK
    3.554E-04_fp,  5.226E-04_fp,  9.816E-01_fp, -7.783E-03_fp,  0.000E+00_fp,  2.437E+01_fp  ], &  ! 89H OK revised
    [N_AZCOEFFS, N_AZFREQUENCIES, N_2STOKES] )


CONTAINS


  SUBROUTINE MWwaterCoeff_LoadCoeffs( MWwaterCoeff )
    TYPE(MWwaterCoeff_type), INTENT(IN OUT) :: MWwaterCoeff
    INTEGER :: version

    version = 6

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

  END SUBROUTINE MWwaterCoeff_LoadCoeffs


  SUBROUTINE MWwaterCoeff_LoadVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWwaterCoeff_LoadVersion

END MODULE MWwaterCoeff_FASTEM6
