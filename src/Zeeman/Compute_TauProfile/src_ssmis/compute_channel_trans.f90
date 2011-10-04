Program compute_channel_trans

  USE Type_Kinds
  USE radiance_ssmis
  USE ssmis_passband, ONLY : Load_passband, NetCDF, RECTANGULAR, SN2ffpb
  USE profile_conversion
  USE interpolate
  USE Model_Profile_Set
  USE Read_AtmProfile
  USE Profile_Utility_Parameters

  IMPLICIT NONE

  character(len=256)  :: InFileName_AtmosProf = &
!             '../data/ECMWF_101LVL_52ATM.AtmProfile.nc'
             '../data/UMBC_101LVL_48ATM.AtmProfile.nc'

!  real(fp_kind), parameter :: TWO = 2.0_fp_kind

  !--- pressure at top of the atmosphere
  integer, parameter :: N_levels = 106
     
  real(fp_kind), parameter :: p_std(N_levels) = (/ &
     0.000071,   0.000082,   0.000094,   0.000109,   0.000126,&
     0.000145,   0.000170,   0.000199,   0.000233,   0.000273,&
     0.000320,   0.000380,   0.000452,   0.000538,   0.000639,&
     0.000760,   0.000907,   0.001082,   0.001292,   0.001542,&
     0.001840,   0.002196,   0.002622,   0.003130,   0.003736,&
     0.004460,   0.005293,   0.006282,   0.007455,   0.008847,&
     0.010500,   0.012388,   0.014615,   0.017243,   0.020343,&
     0.024000,   0.028035,   0.032749,   0.038255,   0.044687,&
     0.052200,   0.060481,   0.070077,   0.081194,   0.094075,&
     0.109000,   0.125323,   0.144090,   0.165667,   0.190476,&
     0.219000,   0.250054,   0.285510,   0.325995,   0.372220,&
     0.425000,   0.482048,   0.546753,   0.620143,   0.703385,&
     0.797800,   0.903872,   1.024047,   1.160478,   1.315398,&
     1.491000,   1.696811,   1.931031,   2.201407,   2.514009,&
     2.871000,   3.326895,   3.855184,   4.429060,   5.044738,&
     5.746000,   6.562546,   7.495129,   8.680102,  10.193175,&
    11.970000,  13.911548,  16.168016,  18.806669,  21.894794,&
    25.490000,  29.720000,  34.670000,  40.470000,  47.290000,&
    55.290000,  64.670000,  75.650000,  88.500000, 103.500000,&
   121.100000, 141.700000, 165.800000, 194.000000, 227.000000,&
   265.000000, 308.000000, 356.500000, 411.100000, 472.200000,&
   540.500000/)

 real(fp_kind), parameter :: base_height = 5000.0_fp_kind ! height at 540.5 mb

  !--- grids of Earth magnetic field (Bfield) and its angle with
  !--- microwave propagation direction (CBTH)
  integer, parameter      :: N_Bfield = 11
  real(fp_kind), parameter :: Bfield(N_Bfield) = &
                         (/0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7/)
  integer, parameter      :: N_CBTH = 11
  real(fp_kind), parameter :: CBTH(N_CBTH) = &
                         (/-1.0, -0.8, -0.6, -0.4, -0.2, 0.0, 0.2, 0.4, 0.6, 0.8, 1.0/)
!                         (/0.0, 0.2, 0.4, 0.6, 0.8, 1.0/)
  
  integer, parameter       :: N_chan = 6
  integer, parameter       :: chan_zeeman(N_chan) = &
                              (/19, 20, 21, 22, 23, 24/)

  ! The local zenith angle range: 0km (surface) - 53.09 km
  !                              20km           - 52.85
  !                             100km           - 51.92
  ! ssmis height = 833.0 km, earth radius = 6370.949 km
  ! The following local zenith angles are computed 
  ! real(fp_kind), parameter :: zenith_angle(N_chan) = &
  !                       (/52.38, 52.26, 52.44, 52.55, 52.67, 52.77/)
  !real(fp_kind)            :: secant_angle(N_chan)
  
   real(fp_kind)            :: zenith_angle
   real(fp_kind)            :: secant_angle
   real(fp_kind)            :: delta_f

  type(atmProfSet_type)    :: atmProfSet

  integer, parameter       :: N_layers = N_levels - 1
  real(fp_kind), dimension(N_levels)   :: z0_lev, z_lev, t_lev, w_lev
  real(fp_kind), dimension(N_layers)   :: p_lay, t_lay, w_lay, &
                                          tau_total, tau_dry, tau_wet

  real                     :: dz_diff
  integer                  :: iatm, ibfield, icbth, local_index, &
                              ichan, i, k, pol
  integer                  :: error_status
  integer, parameter       :: fid_out = 51
  character(256)           :: filename_out
  character(128)           :: sensor_id

  ! input sensor ID
  print *, 'enter sensor id (i.g. ssmis_f16):'
  read(*, *)sensor_id
  
  ! input ssmis channel number
  print *, 'enter ssmis channel # (19, 20, 21, 22, 23 or 24):'
  read(*, '(i5)')ichan
  local_index = ichan-18

  print *, 'enter zenith angle # (51.0, 52.5, 54.0):'
  read(*, '(f9.3)')zenith_angle

  print *, 'enter Doppler frequency shift of the radiation spectrum (in the range -80. - 80. kHz):'
  read(*, '(e16.8)')delta_f

  !******************************************************************************************
  ! Change units to GHz.
  ! The minus sign is added due to the fact that radiance calculation for a shifted spectrum
  ! is equivalent to that for a shifted SRF but the shift is in the opposit direction.
  !******************************************************************************************* 
  delta_f = -delta_f * 1.e-6

  print *, 'enter polarization index (1 - LCP; 2 - RCP):'
  read(*, '(i5)')pol

  ! input atmospheric profile filename
  print *, 'enter input profile filename(Netcdf):'
  read(*, '(A)')InFileName_AtmosProf
  
  ! output file name
  print *, 'enter output filename:'
  read(*, '(A)')filename_out

  CALL Load_passband(NetCDF, sensor_id=TRIM(sensor_id))
  
  secant_angle = ONE  / COS(zenith_angle*PI/180.0_fp_kind)

  call Read_atmosProf(InFileName_AtmosProf, atmProfSet)

  open(fid_out, file=TRIM(filename_out), status='replace')
  write(fid_out, '(4i5)') atmProfSet%Natm, n_layers, N_Bfield, N_CBTH

  do iatm = 1, atmProfSet%Natm

    print *, 'profile # ', iatm

    call map_profile(atmProfSet%p_lev(0:), atmProfSet%t_lev(0:, iatm), &
                  atmProfSet%wet_lev(0:, iatm), atmProfSet%latitude(iatm), &
                  t_lev, w_lev )

    p_lay = (p_std(2:N_levels) - p_std(1:N_levels-1))/log(p_std(2:N_levels) / p_std(1:N_levels-1))
    t_lay = (t_lev(2:N_levels) + t_lev(1:N_levels-1)) / TWO
    w_lay = (w_lev(2:N_levels) + w_lev(1:N_levels-1)) / TWO

    error_status = geopotential_height( p_std, t_lev, w_lev, 1, z_lev, &
                   Gravity_Correction = 1, Latitude=atmProfSet%latitude(iatm), &
                   surface_height = base_height )


    z_lev = z_lev / 1000.0_fp_kind

    write(fid_out, '(i5, 1x, f8.3)') iatm, zenith_angle
    write(fid_out, '(f9.3, 1x, f12.6, 1x, f10.3, es16.8)') &
               (z_lev(i), p_std(i), t_lev(i), w_lev(i), i = 1, n_levels)

    do ibfield = 1, N_Bfield
      do icbth = 1, N_CBTH
!        do chan_index = 1, N_chan

           if(ichan == 23 .or. ichan == 24)then
             call transmittance(ichan, secant_angle, &
                 z_lev, p_lay, t_lay, w_lay, &
                 tau_total, tau_dry, tau_wet, delta_f=delta_f )
           else
             if(pol == 2)then ! RC polarization
               call transmittance(ichan, secant_angle, &
                   z_lev, p_lay, t_lay, w_lay, &
                   tau_total, tau_dry, tau_wet, &
                   Bfield=Bfield(ibfield) , &
                   CBTH=CBTH(icbth), &
                   delta_f=delta_f, RCP=1 )
             else
               call transmittance(ichan, secant_angle, &
                   z_lev, p_lay, t_lay, w_lay, &
                   tau_total, tau_dry, tau_wet, &
                   Bfield=Bfield(ibfield) , &
                   CBTH=CBTH(icbth), &
                   delta_f=delta_f )
             endif
           endif

          write(fid_out, '(2es16.8, 1x, i5)') Bfield(ibfield), CBTH(icbth), ichan
          do i = 1, n_layers
            write(fid_out, '(es16.8)') tau_total(i)
          enddo
!        enddo
      enddo
    enddo

  enddo

  OPEN(23, FILE=TRIM(filename_out)//'.CompleteSignal', STATUS='REPLACE')
  WRITE(23, '("Completed successfully")')
  CLOSE(23)
  
CONTAINS

  subroutine map_profile(p, t, w, latitude, &
                            t_std, w_std)
    real(fp_kind), intent(in)  :: p(:), t(:), w(:)
    real(fp_kind), intent(in)  :: latitude
    real(fp_kind), intent(out) :: t_std(:), w_std(:)

    ! local
    integer, parameter :: nlev_ext = 300
    real(fp_kind) :: p_ext(nlev_ext), t_ext(nlev_ext), w_ext(nlev_ext)
    integer :: N0_levels, dn, ilev, nl, n
    integer :: error_status
    real(fp_kind) :: logp_grid, dp, t_tmp, dt
    real(fp_kind), dimension(N_MODEL_LEVELS) :: model_p, model_t

    N0_levels = SIZE(p)


    if( p(1) <= p_std(1) )then  ! copy data

      p_ext(1:N0_levels) = p                                                 
      t_ext(1:N0_levels) = t                                                 
      w_ext(1:N0_levels) = w 
      nl = N0_levels                                  

    else ! extend the profile

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

      ! extend the profiles using model profile
      do ilev = 1, N_MODEL_LEVELS
         if(model_p(ilev) >= p(1))then
            nl = ilev-1 + N0_levels
            p_ext(1:ilev-1) = model_p(1:ilev-1)
            p_ext(ilev:nl) = p
            t_ext(1:ilev-1) = model_t(1:ilev-1)
            t_ext(ilev:nl) = t
            !--- for water vapor, use the end point of the original profile
            w_ext(1:ilev-1) = w(1)
            w_ext(ilev:nl) = w
            n = ilev
            exit
         endif
      enddo

      !--- adjust the temperature profile so that the extended 
      !--- portion is a smooth extension of the profile below
      error_status = polynomial_interpolate(&
            log(model_p), model_t, log(p_ext(n)), t_tmp)
      dt = t_ext(n) - t_tmp
      t_ext(1:n-1) = t_ext(1:n-1) + dt
            
    endif

    error_status = polynomial_interpolate(&
            log(p_ext(1:nl)), t_ext(1:nl), log(p_std), t_std)
    error_status = polynomial_interpolate(&
            log(p_ext(1:nl)), w_ext(1:nl), log(p_std), w_std)

  end subroutine map_profile

end Program compute_channel_trans             
