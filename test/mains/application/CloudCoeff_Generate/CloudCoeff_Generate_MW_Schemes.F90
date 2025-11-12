! CloudCoeff_Generate_MW_Schemes.F90
! Generate one CRTM CloudCoeff (MW) NetCDF per density scheme (Density1..Density4),
! matching the provided schema. Solid-phase (S) arrays are populated;
! liquid-phase (L) and pcoeff arrays are created and filled with zeros.
!
! Inputs:
!   MW_freq.txt
!   SnowflakeModel_MW/DensityN/T190..T270/isca_freq.dat
!
! Outputs (per scheme):
!   CloudCoeff_MW_Snowflake_DensityN.nc4

module util_kinds
  use iso_fortran_env, only: real64
  implicit none
  integer, parameter :: rk = real64
end module util_kinds

module scheme_mass
  use util_kinds
  implicit none
contains
  pure real(rk) function mass_g_density1(d_um) result(mg)
    real(rk), intent(in) :: d_um
    real(rk) :: d_cm, vcirc_cm3
    d_cm = d_um*1.0e-4_rk
    vcirc_cm3 = (acos(-1.0_rk)/6.0_rk)*d_cm**3
    mg = 0.1_rk*vcirc_cm3
  end function mass_g_density1

  pure real(rk) function mass_g_density2(d_um) result(mg)
    real(rk), intent(in) :: d_um
    real(rk) :: d_mm
    d_mm = d_um*1.0e-3_rk
    mg = 6.9e-5_rk*d_mm**2.0_rk
  end function mass_g_density2

  pure real(rk) function mass_g_density3(d_um) result(mg)
    real(rk), intent(in) :: d_um
    real(rk) :: d_mm
    d_mm = d_um*1.0e-3_rk
    mg = 5.436e-5_rk*d_mm**2.05_rk
  end function mass_g_density3

  pure real(rk) function mass_g_density4(d_um) result(mg)
    real(rk), intent(in) :: d_um
    real(rk) :: d_mm
    d_mm = d_um*1.0e-3_rk
    mg = 8.90e-5_rk*d_mm**2.1_rk
  end function mass_g_density4

  pure real(rk) function reff_um_from_mass_g(mg) result(r_um)
    real(rk), intent(in) :: mg
    real(rk), parameter :: rho_ice_g_cm3 = 0.917_rk
    real(rk) :: r_cm
    r_cm = (3.0_rk*mg/(4.0_rk*acos(-1.0_rk)*rho_ice_g_cm3))**(1.0_rk/3.0_rk)
    r_um = r_cm*1.0e4_rk
  end function reff_um_from_mass_g
end module scheme_mass

module text_io
  use util_kinds
  implicit none
contains
  subroutine read_vector_file(filename, x, nread, allow_comments)
    character(*), intent(in)  :: filename
    real(rk),     intent(out) :: x(:)
    integer,      intent(out) :: nread
    logical,      intent(in),  optional :: allow_comments
    logical :: allowc
    integer :: u, ios, nmax
    character(len=8192) :: line
    real(rk) :: v
    allowc = .false.; if (present(allow_comments)) allowc = allow_comments
    nread = 0; nmax = size(x)
    open(newunit=u, file=filename, status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'failed to open file'
    do
      read(u,'(A)',iostat=ios) line
      if (ios /= 0) exit
      if (trim(line) == '') cycle
      if (allowc) then
        if (line(1:1) == '#' .or. line(1:1) == '!') cycle
      end if
      read(line,*,iostat=ios) v
      if (ios /= 0) cycle
      nread = nread + 1
      if (nread <= nmax) then
        x(nread) = v
      else
        exit
      end if
    end do
    close(u)
  end subroutine read_vector_file
end module text_io

module isca_reader
  use util_kinds
  implicit none
  public :: read_isca_block_header, stream_isca_fill
contains
  subroutine read_isca_block_header(path, nfreq, nsize, freq_out, dmax_um)
    character(*), intent(in)  :: path
    integer,      intent(in)  :: nfreq, nsize
    real(rk),     intent(out) :: freq_out(nfreq)
    real(rk),     intent(out) :: dmax_um(nsize)
    integer :: u, ios, f, s
    real(rk) :: fv, du, vv, av, qv, wv, gv
    open(newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'failed to open isca_freq.dat'
    do f=1,nfreq
      do s=1,nsize
        read(u,*,iostat=ios) fv, du, vv, av, qv, wv, gv
        if (ios /= 0) stop 'bad isca_freq.dat format'
        if (s == 1) freq_out(f) = fv
        if (f == 1) dmax_um(s) = du
      end do
    end do
    close(u)
  end subroutine read_isca_block_header

  subroutine stream_isca_fill(path, nfreq, nsize, freq_ref, masses_g, ke_sf, w_sf, g_sf)
    character(*), intent(in)  :: path
    integer,      intent(in)  :: nfreq, nsize
    real(rk),     intent(in)  :: freq_ref(nfreq)
    real(rk),     intent(in)  :: masses_g(nsize)
    real(rk),     intent(out) :: ke_sf(nsize,nfreq)
    real(rk),     intent(out) :: w_sf(nsize,nfreq)
    real(rk),     intent(out) :: g_sf(nsize,nfreq)
    integer :: u, ios, f, s
    real(rk) :: fv, du, vv, av, qv, wv, gv
    real(rk) :: area_m2, m_kg
    open(newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'open isca failed'
    do f=1,nfreq
      do s=1,nsize
        read(u,*,iostat=ios) fv, du, vv, av, qv, wv, gv
        if (ios /= 0) stop 'bad isca record'
        if (abs(fv - freq_ref(f)) > 1.0e-6_rk) stop 'frequency mismatch in isca_freq.dat'
        area_m2 = av*1.0e-12_rk
        m_kg    = masses_g(s)*1.0e-3_rk
        ke_sf(s,f) = qv*area_m2 / max(m_kg, 1.0e-300_rk)
        w_sf(s,f)  = min(max(wv, 0.0_rk), 1.0_rk)
        g_sf(s,f)  = max(min(gv, 1.0_rk), -1.0_rk)
      end do
    end do
    close(u)
  end subroutine stream_isca_fill
end module isca_reader

module mw_netcdf_writer
  use util_kinds
  use netcdf
  implicit none
contains
  subroutine write_cloudcoeff_mw(outfile, freq_mw, reff_mw, temp_k, density_val, &
                                 ke_S, w_S, g_S)
    character(*), intent(in) :: outfile
    real(rk),     intent(in) :: freq_mw(:)                 ! nF
    real(rk),     intent(in) :: reff_mw(:)                 ! nR
    real(rk),     intent(in) :: temp_k(:)                  ! nT
    real(rk),     intent(in) :: density_val                ! single value (kg m^-3)
    real(rk),     intent(in) :: ke_S(:,:)                  ! (nR, nF)
    real(rk),     intent(in) :: w_S(:,:)                   ! (nR, nF)
    real(rk),     intent(in) :: g_S(:,:)                   ! (nR, nF)

    integer :: ncid, ierr
    integer :: did_f, did_r, did_t, did_d, did_l, did_p
    integer :: nF, nR, nT
    integer :: vid_f, vid_r, vid_T, vid_D
    integer :: vid_keL, vid_wL, vid_gL, vid_pL
    integer :: vid_keS, vid_wS, vid_gS, vid_pS
    real(rk), allocatable :: density(:)
    real(rk), allocatable :: zero_keL(:,:,:)
    real(rk), allocatable :: zero_wL(:,:,:)
    real(rk), allocatable :: zero_gL(:,:,:)
    real(rk), allocatable :: zero_pL(:,:,:,:,:)
    real(rk), allocatable :: zero_pS(:,:,:,:,:)

    integer, parameter :: nPhase=1, nLeg=39, nDen=1

    nF = size(freq_mw); nR = size(reff_mw); nT = size(temp_k)
    allocate(density(nDen)); density = density_val

    ierr = nf90_create(outfile, NF90_NETCDF4, ncid); if (ierr/=nf90_noerr) stop 'create failed'

    ierr = nf90_def_dim(ncid,'n_MW_Frequencies', nF, did_f)
    ierr = nf90_def_dim(ncid,'n_MW_Radii',       nR, did_r)
    ierr = nf90_def_dim(ncid,'n_Temperatures',   nT, did_t)
    ierr = nf90_def_dim(ncid,'n_Densities',      nDen, did_d)
    ierr = nf90_def_dim(ncid,'n_Legendre_Terms', nLeg, did_l)
    ierr = nf90_def_dim(ncid,'n_Phase_Elements', nPhase, did_p)

    ierr = nf90_def_var(ncid,'Frequency_MW', NF90_DOUBLE, (/did_f/), vid_f)
    ierr = nf90_put_att(ncid, vid_f, 'long_name',   'Frequency')
    ierr = nf90_put_att(ncid, vid_f, 'description', 'Microwave frequency LUT dimension vector')
    ierr = nf90_put_att(ncid, vid_f, 'units',       'GigaHertz (GHz)')
    ierr = nf90_put_att(ncid, vid_f, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'Reff_MW', NF90_DOUBLE, (/did_r/), vid_r)
    ierr = nf90_put_att(ncid, vid_r, 'long_name',   'Effective radius')
    ierr = nf90_put_att(ncid, vid_r, 'description', 'Microwave effective radius LUT dimension vector')
    ierr = nf90_put_att(ncid, vid_r, 'units',       'Microns (um)')
    ierr = nf90_put_att(ncid, vid_r, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'Temperature', NF90_DOUBLE, (/did_t/), vid_T)
    ierr = nf90_put_att(ncid, vid_T, 'long_name',   'Temperature')
    ierr = nf90_put_att(ncid, vid_T, 'description', 'Temperature LUT dimension vector')
    ierr = nf90_put_att(ncid, vid_T, 'units',       'Kelvin (K)')
    ierr = nf90_put_att(ncid, vid_T, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'Density', NF90_DOUBLE, (/did_d/), vid_D)
    ierr = nf90_put_att(ncid, vid_D, 'long_name',   'Density')
    ierr = nf90_put_att(ncid, vid_D, 'description', 'Density LUT dimension vector')
    ierr = nf90_put_att(ncid, vid_D, 'units',       'Kilograms per cubic metre (kg.m^-3)')
    ierr = nf90_put_att(ncid, vid_D, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'ke_L_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_keL)
    ierr = nf90_put_att(ncid, vid_keL, 'long_name',   'Microwave ke(L)')
    ierr = nf90_put_att(ncid, vid_keL, 'description', 'Mass extinction coefficient for liquid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_keL, 'units',       'Metres squared per kilogram (m^2.kg^-1)')
    ierr = nf90_put_att(ncid, vid_keL, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'w_L_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_wL)
    ierr = nf90_put_att(ncid, vid_wL, 'long_name',   'Microwave w(L)')
    ierr = nf90_put_att(ncid, vid_wL, 'description', 'Single scatter albedo for liquid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_wL, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_wL, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'g_L_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_gL)
    ierr = nf90_put_att(ncid, vid_gL, 'long_name',   'Microwave g(L)')
    ierr = nf90_put_att(ncid, vid_gL, 'description', 'Asymmetry parameter for liquid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_gL, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_gL, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'pcoeff_L_MW', NF90_DOUBLE, (/did_p,did_l,did_t,did_r,did_f/), vid_pL)
    ierr = nf90_put_att(ncid, vid_pL, 'long_name',   'Microwave pcoeff(L)')
    ierr = nf90_put_att(ncid, vid_pL, 'description', 'Phase coefficients for liquid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_pL, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_pL, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'ke_S_MW', NF90_DOUBLE, (/did_d,did_r,did_f/), vid_keS)
    ierr = nf90_put_att(ncid, vid_keS, 'long_name',   'Microwave ke(S)')
    ierr = nf90_put_att(ncid, vid_keS, 'description', 'Mass extinction coefficient for solid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_keS, 'units',       'Metres squared per kilogram (m^2.kg^-1)')
    ierr = nf90_put_att(ncid, vid_keS, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'w_S_MW', NF90_DOUBLE, (/did_d,did_r,did_f/), vid_wS)
    ierr = nf90_put_att(ncid, vid_wS, 'long_name',   'Microwave w(S)')
    ierr = nf90_put_att(ncid, vid_wS, 'description', 'Single scatter albedo for solid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_wS, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_wS, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'g_S_MW', NF90_DOUBLE, (/did_d,did_r,did_f/), vid_gS)
    ierr = nf90_put_att(ncid, vid_gS, 'long_name',   'Microwave g(S)')
    ierr = nf90_put_att(ncid, vid_gS, 'description', 'Asymmetry parameter for solid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_gS, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_gS, '_FillValue',  0.0_rk)

    ierr = nf90_def_var(ncid,'pcoeff_S_MW', NF90_DOUBLE, (/did_p,did_l,did_d,did_r,did_f/), vid_pS)
    ierr = nf90_put_att(ncid, vid_pS, 'long_name',   'Microwave pcoeff(S)')
    ierr = nf90_put_att(ncid, vid_pS, 'description', 'Phase coefficients for solid phase microwave scatterers')
    ierr = nf90_put_att(ncid, vid_pS, 'units',       'N/A')
    ierr = nf90_put_att(ncid, vid_pS, '_FillValue',  0.0_rk)

    ierr = nf90_enddef(ncid); if (ierr/=nf90_noerr) stop 'enddef failed'

    ierr = nf90_put_var(ncid, vid_f, freq_mw)
    ierr = nf90_put_var(ncid, vid_r, reff_mw)
    ierr = nf90_put_var(ncid, vid_T, temp_k)
    ierr = nf90_put_var(ncid, vid_D, density)

    allocate(zero_keL(nT,nR,nF)); zero_keL = 0.0_rk
    allocate(zero_wL (nT,nR,nF)); zero_wL  = 0.0_rk
    allocate(zero_gL (nT,nR,nF)); zero_gL  = 0.0_rk
    allocate(zero_pL (nPhase,nLeg,nT,nR,nF)); zero_pL = 0.0_rk
    allocate(zero_pS (nPhase,nLeg,nDen,nR,nF)); zero_pS = 0.0_rk

    ierr = nf90_put_var(ncid, vid_keL, zero_keL)
    ierr = nf90_put_var(ncid, vid_wL,  zero_wL)
    ierr = nf90_put_var(ncid, vid_gL,  zero_gL)
    ierr = nf90_put_var(ncid, vid_pL,  zero_pL)

    ierr = nf90_put_var(ncid, vid_keS, reshape(ke_S, (/nDen,nR,nF/)))
    ierr = nf90_put_var(ncid, vid_wS,  reshape(w_S,  (/nDen,nR,nF/)))
    ierr = nf90_put_var(ncid, vid_gS,  reshape(g_S,  (/nDen,nR,nF/)))
    ierr = nf90_put_var(ncid, vid_pS,  zero_pS)

    ierr = nf90_close(ncid); if (ierr/=nf90_noerr) stop 'close failed'
  end subroutine write_cloudcoeff_mw
end module mw_netcdf_writer

program CloudCoeff_Generate_MW_Schemes
  use util_kinds
  use scheme_mass
  use text_io
  use isca_reader
  use mw_netcdf_writer
  implicit none

  integer, parameter :: nF = 70, nR = 131, nT = 5
  character(len=*), parameter :: base_dir = 'SnowflakeModel_MW'
  character(len=*), parameter :: temps(nT) = (/'T190','T210','T230','T250','T270'/)
  real(rk) :: freq_mw(nF)
  real(rk) :: temp_k(nT)
  integer :: i

  character(len=3) :: tstr
  integer :: tval

  do i=1,nT
    tstr = temps(i)(2:4)
    read(tstr,'(I3)') tval
    temp_k(i) = real(tval, rk)
  end do

  call read_freq(freq_mw)
  call do_scheme(1, freq_mw, temp_k)
  call do_scheme(2, freq_mw, temp_k)
  call do_scheme(3, freq_mw, temp_k)
  call do_scheme(4, freq_mw, temp_k)

contains
  subroutine read_freq(freq_mw)
    use text_io, only: read_vector_file
    implicit none
    real(rk), intent(out) :: freq_mw(:)
    integer :: nread
    call read_vector_file('MW_freq.txt', freq_mw, nread, .true.)
    if (nread /= size(freq_mw)) stop 'MW_freq.txt length mismatch'
  end subroutine read_freq
  
  subroutine do_scheme(sid, freq_mw, temp_k)
    use isca_reader
    use scheme_mass,      only: mass_g_density1, mass_g_density2, mass_g_density3, mass_g_density4, reff_um_from_mass_g
    use mw_netcdf_writer, only: write_cloudcoeff_mw
    implicit none
    integer,  intent(in) :: sid
    real(rk), intent(in) :: freq_mw(:)
    real(rk), intent(in) :: temp_k(:)

    character(len=512) :: ddir, tdir, iscapath, outfile
    real(rk) :: fchk(nF), dmax_um(nR)
    real(rk) :: masses_g(nR), reff_um(nR)
    real(rk) :: ke_sf(nR,nF), w_sf(nR,nF), g_sf(nR,nF)
    integer :: t, s

    write(ddir,'(A,"/Density",I1)') trim(base_dir), sid

    write(tdir,'(A,"/",A)') trim(ddir), 'T190'
    write(iscapath,'(A,"/isca_freq.dat")') trim(tdir)
    call read_isca_block_header(trim(iscapath), nF, nR, fchk, dmax_um)
    call assert_freq_match(freq_mw, fchk)

    do s=1,nR
      select case(sid)
      case(1); masses_g(s) = mass_g_density1(dmax_um(s))
      case(2); masses_g(s) = mass_g_density2(dmax_um(s))
      case(3); masses_g(s) = mass_g_density3(dmax_um(s))
      case(4); masses_g(s) = mass_g_density4(dmax_um(s))
      end select
      reff_um(s) = reff_um_from_mass_g(masses_g(s))
    end do
    call assert_monotonic_increasing(reff_um)

    ke_sf = 0.0_rk; w_sf = 0.0_rk; g_sf = 0.0_rk

    do t=1,nT
      write(tdir,'(A,"/",A)') trim(ddir), temps(t)
      write(iscapath,'(A,"/isca_freq.dat")') trim(tdir)
      call stream_isca_fill(trim(iscapath), nF, nR, freq_mw, masses_g, ke_sf, w_sf, g_sf)
    end do

    write(outfile,'("CloudCoeff_MW_Snowflake_Density",I1,".nc4")') sid

    call write_cloudcoeff_mw(trim(outfile), freq_mw, reff_um, temp_k, density_value_for(sid), &
                             ke_sf, w_sf, g_sf)
  end subroutine do_scheme

  real(rk) function density_value_for(sid) result(rho)
    integer, intent(in) :: sid
    select case(sid)
    case(1)
      rho = 100.0_rk
    case default
      rho = 0.0_rk
    end select
  end function density_value_for

  subroutine assert_freq_match(a, b)
    real(rk), intent(in) :: a(:), b(:)
    integer :: i
    if (size(a) /= size(b)) stop 'frequency vector length mismatch'
    do i=1,size(a)
      if (abs(a(i)-b(i)) > 1.0e-6_rk) stop 'frequency mismatch'
    end do
  end subroutine assert_freq_match

  subroutine assert_monotonic_increasing(x)
    real(rk), intent(in) :: x(:)
    integer :: i
    do i=1,size(x)-1
      if (x(i+1) <= x(i)) stop 'Reff not strictly increasing'
    end do
  end subroutine assert_monotonic_increasing
end program CloudCoeff_Generate_MW_Schemes
