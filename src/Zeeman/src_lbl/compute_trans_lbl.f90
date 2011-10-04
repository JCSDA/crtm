Program compute_trans_lbl

  USE Type_Kinds
  USE transmittance_microwave_lbl
  USE profile_conversion
  USE interpolate
  USE Model_Profile_Set
  USE Read_AtmProfile
  USE Profile_Utility_Parameters

  IMPLICIT NONE

  character(*), parameter  :: InFileName_AtmosProf = &
        '../data/UMBC_101LVL_48ATM.AtmProfile.nc'

  real(fp_kind), parameter :: TWO = 2.0_fp_kind

  !--- pressure at top of the atmosphere
  real(fp_kind), parameter :: P_TOPLEVEL = 0.00004_fp_kind  ! mb

  type(atmProfSet_type)    :: atmProfSet

  integer, parameter       :: N_levels = 101 + 6
  integer, parameter       :: N_layers = N_levels - 1
  real(fp_kind), dimension(N_levels)   :: z_lev, p_lev, t_lev, w_lev
  real(fp_kind), dimension(N_layers)   :: p_lay, t_lay, w_lay, &
                                          tau_rc, tau_lc, &
                                          tau_rc_ch, tau_lc_ch, &
                                          bfield, cbth

  real(fp_kind)            :: freq_center, secant_view_angle, ff, df, f1, f2
  integer                  :: i, iatm, ifrq
  integer                  :: error_status
  integer, parameter       :: fid_out = 61


  call Read_atmosProf(InFileName_AtmosProf, atmProfSet)

  open(fid_out, file='zeeman_tau_umbc_ch_n15.txt', status='replace')

  Bfield = 0.5_fp_kind
  CBTH = 1.0_fp_kind
  secant_view_angle = 1.0_fp_kind
  freq_center  = 63.283248_fp_kind
  f1 = freq_center - 0.285951_fp_kind
  f2 = freq_center - 0.284591_fp_kind  

  
  do iatm = 1, atmProfSet%Natm

    print *, 'profile # ', iatm

    call extend_profile(atmProfSet%p_lev(0:), atmProfSet%t_lev(0:, iatm), &
                  atmProfSet%wet_lev(0:, iatm), atmProfSet%latitude(iatm), &
                  p_lev, t_lev, w_lev )

    p_lay = (p_lev(2:N_levels) - p_lev(1:N_levels-1))/log(p_lev(2:N_levels) / p_lev(1:N_levels-1))
    t_lay = (t_lev(2:N_levels) + t_lev(1:N_levels-1)) / TWO
    w_lay = (w_lev(2:N_levels) + w_lev(1:N_levels-1)) / TWO

    error_status = geopotential_height( p_lev, t_lev, w_lev, 1, z_lev )

    z_lev = z_lev / 1000.0_fp_kind

    write(fid_out, '(i5)') iatm

    tau_rc_ch = 0.0
    tau_lc_ch = 0.0
    df = (f2-f1) / 256.0
    do ifrq = 0, 255

       ff =f1 + df * real(ifrq, fp_kind)

       call tauO2N2_zeeman_lbl(ff, secant_view_angle, &
                            z_lev, p_lay, t_lay, w_lay, &
			    tau_rc, tau_lc , &
                            Bfield, CBTH)

       tau_rc_ch = tau_rc_ch + tau_rc
       tau_lc_ch = tau_lc_ch + tau_lc

    enddo
  
    tau_rc_ch = tau_rc_ch /256.0
    tau_lc_ch = tau_lc_ch /256.0
    
    write(fid_out, '(f12.6, f9.3, 2es16.8)') p_lev(1),t_lev(1), 1.0, 1.0
    do i = 1, n_layers
      write(fid_out, '(f12.6, f9.3, 2es16.8)') p_lev(i+1),t_lev(i+1), tau_rc_ch(i), tau_lc_ch(i)
    enddo

  enddo

CONTAINS

  subroutine extend_profile(p, t, w, latitude, &
                            p_ext, t_ext, w_ext)
    real(fp_kind), intent(in)  :: p(:), t(:), w(:)
    real(fp_kind), intent(in)  :: latitude
    real(fp_kind), intent(out) :: p_ext(:), t_ext(:), w_ext(:)

    ! local
    integer :: N0_levels, n_levels, dn, ilev
    integer :: error_status
    real(fp_kind) :: logp_grid, dp, t_tmp, dt
    real(fp_kind), dimension(N_MODEL_LEVELS) :: model_p, model_t

    N0_levels = SIZE(p)
    N_levels  = SIZE(p_ext)

    if(N0_levels >= N_levels)then
      print *, 'extend_profile(): the # of input levels >= that of requested'
      stop
    endif

    dn = N_levels - N0_levels
    dp = p(1) - P_TOPLEVEL

    if(dp < 0.000001_fp_kind)then
      print *, 'extend_profile(): profile already reach the level: ', &
               P_TOPLEVEL
      stop
    endif

    logp_grid = log(p(1)/P_TOPLEVEL)/dn

    p_ext(dn+1:N_levels) = p(:)
    p_ext(1) = P_TOPLEVEL
    do ilev = 2, dn
      p_ext(ilev) = P_TOPLEVEL * exp( logp_grid * REAL(ilev-1, fp_kind))
    enddo

    if(ABS(latitude) < MODEL_LATITUDE(1))then      ! tropical
      error_status = Load_Model_Profile(1)
    else if(ABS(latitude) < MODEL_LATITUDE(2))then ! midlatitude

      if(t(N0_levels) > 282.0_fp_kind)then
        error_status = Load_Model_Profile(2)       ! summer
      else
        error_status = Load_Model_Profile(3)       ! winter
      endif

    else ! subarctic and arctic

      if(t(N0_levels) > 273.0_fp_kind)then
        error_status = Load_Model_Profile(2)  ! summer
      else
        error_status = Load_Model_Profile(3)  ! winter
      endif

    endif

    model_p = model_level_pressure(N_MODEL_LEVELS:1:-1)
    model_t = model_level_temperature(N_MODEL_LEVELS:1:-1)

    !--- for water vapor, use the end point of the original profile
    w_ext(1:dn) = w(1)
    w_ext(dn+1:N_levels) = w(:)

    ! -- temperature

    error_status = polynomial_interpolate(&
            log(model_p), model_t, log(p_ext(1:dn)), t_ext(1:dn))
    t_ext(dn+1:N_levels) = t(:)

    !--- adjust the temperature profile so that the extended 
    !--- portion is a smooth extension of the profile below
    error_status = polynomial_interpolate(&
            log(model_p), model_t, log(p_ext(dn+1)), t_tmp)

    dt = t_ext(dn+1) - t_tmp
    t_ext(1:dn) = t_ext(1:dn) + dt

  end subroutine extend_profile

end Program compute_trans_lbl             
