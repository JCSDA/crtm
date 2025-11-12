! CloudCoeff_Generate_MW_Schemes.F90
! Generates one CRTM CloudCoeff MW NetCDF per density scheme (Density1..Density4).
! Reads:
!   MW_freq.txt                          -> Frequency_MW (GHz), length 70
!   SnowflakeModel_MW/DensityN/Txxx/isca_freq.dat  -> per scheme, per temperature
! Assumes temperatures: T190, T210, T230, T250, T270
! Outputs:
!   CloudCoeff_MW_Snowflake_DensityN.nc4 per scheme.
!
! Build: link with NetCDF Fortran (use FindNetCDF in CMake).

module util_kinds
  use iso_fortran_env, only: real64
  implicit none
  integer, parameter :: rk = real64
end module util_kinds

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


  subroutine split_path(dir, files, n, list)
    character(*), intent(in)  :: dir
    character(*), intent(in)  :: files(:)
    integer,      intent(in)  :: n
    character(len=:), allocatable, intent(out) :: list(:)
    integer :: i, L, m, lt
    
    ! compute max filename length among the first n entries
    m = 0
    do i = 1, n
       lt = len_trim(files(i))
       if (lt > m) m = lt
    end do

    L = len_trim(dir) + 1 + m
    allocate(character(len=L) :: list(n))
    
    do i = 1, n
       list(i) = trim(dir)//'/'//trim(files(i))
    end do
  end subroutine split_path
end module text_io

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
    r_cm = (3.0_rk*mg/(4.0_rk*acos( -1.0_rk )*rho_ice_g_cm3))**(1.0_rk/3.0_rk)
    r_um = r_cm*1.0e4_rk
  end function reff_um_from_mass_g
end module scheme_mass

module isca_reader
  use util_kinds
  implicit none
contains
  subroutine read_isca_block(path, nfreq, nsize, freq_out, dmax_um, vol_um3, area_um2, qext, w, g)
    character(*), intent(in)  :: path
    integer,      intent(in)  :: nfreq, nsize
    real(rk),     intent(out) :: freq_out(nfreq)
    real(rk),     intent(out) :: dmax_um(nsize)
    real(rk),     intent(out) :: vol_um3(nsize)
    real(rk),     intent(out) :: area_um2(nsize)
    real(rk),     intent(out) :: qext(nsize)
    real(rk),     intent(out) :: w(nsize)
    real(rk),     intent(out) :: g(nsize)
    integer :: u, ios, f, s, count
    real(rk) :: fv, du, vv, av, qv, wv, gv
    open(newunit=u, file=path, status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'failed to open isca_freq.dat'
    do f=1,nfreq
      do s=1,nsize
        read(u,*,iostat=ios) fv, du, vv, av, qv, wv, gv
        if (ios /= 0) stop 'bad isca_freq.dat format'
        if (s == 1) freq_out(f) = fv
        if (f == 1) then
          dmax_um(s) = du
          vol_um3(s) = vv
          area_um2(s)= av
          qext(s)    = qv
          w(s)       = wv
          g(s)       = gv
        end if
      end do
    end do
    close(u)
  end subroutine read_isca_block

  subroutine read_isca_freq_slice(path, nsize, ssp_area_um2, ssp_qext, ssp_w, ssp_g, freq_value)
    character(*), intent(in)  :: path
    integer,      intent(in)  :: nsize
    real(rk),     intent(out) :: ssp_area_um2(nsize)
    real(rk),     intent(out) :: ssp_qext(nsize)
    real(rk),     intent(out) :: ssp_w(nsize)
    real(rk),     intent(out) :: ssp_g(nsize)
    real(rk),     intent(out) :: freq_value
    integer :: u, ios, s
    real(rk) :: fv, du, vv, av, qv, wv, gv
    save
    if (.true.) then
      ! Not used in streaming version in this file
    end if
  end subroutine read_isca_freq_slice
end module isca_reader

module mw_writer
  use util_kinds
  use netcdf
  implicit none
contains
  subroutine write_mw_file(outfile, freq_mw, reff_um, temps_k, ke, w, g)
    character(*), intent(in) :: outfile
    real(rk),     intent(in) :: freq_mw(:)
    real(rk),     intent(in) :: reff_um(:)
    real(rk),     intent(in) :: temps_k(:)
    real(rk),     intent(in) :: ke(:,:,:)
    real(rk),     intent(in) :: w(:,:,:)
    real(rk),     intent(in) :: g(:,:,:)
    integer :: ncid, did_f, did_r, did_t
    integer :: vid_freq, vid_reff, vid_temp, vid_ke, vid_w, vid_g
    integer :: ierr
    integer :: nF, nR, nT
    nF = size(freq_mw); nR = size(reff_um); nT = size(temps_k)

    ierr = nf90_create(outfile, NF90_NETCDF4, ncid); if (ierr /= nf90_noerr) stop 'create failed'

    ierr = nf90_def_dim(ncid, 'n_MW_Frequencies', nF, did_f); if (ierr /= nf90_noerr) stop 'defdim f'
    ierr = nf90_def_dim(ncid, 'n_MW_Radii',       nR, did_r); if (ierr /= nf90_noerr) stop 'defdim r'
    ierr = nf90_def_dim(ncid, 'n_Temperatures',   nT, did_t); if (ierr /= nf90_noerr) stop 'defdim t'

    ierr = nf90_def_var(ncid, 'Frequency_MW', NF90_DOUBLE, (/did_f/), vid_freq); if (ierr /= nf90_noerr) stop 'defvar freq'
    ierr = nf90_put_att(ncid, vid_freq, 'long_name', 'Frequency')
    ierr = nf90_put_att(ncid, vid_freq, 'description', 'Microwave frequency LUT dimension vector')
    ierr = nf90_put_att(ncid, vid_freq, 'units', 'GigaHertz (GHz)')
    ierr = nf90_put_att(ncid, vid_freq, '_FillValue', 0.0_rk)

    ierr = nf90_def_var(ncid, 'Reff_MW', NF90_DOUBLE, (/did_r/), vid_reff); if (ierr /= nf90_noerr) stop 'defvar reff'
    ierr = nf90_put_att(ncid, vid_reff, 'long_name', 'Mass-equivalent sphere radius')
    ierr = nf90_put_att(ncid, vid_reff, 'description', 'Per-scheme effective radius grid derived from m(D)')
    ierr = nf90_put_att(ncid, vid_reff, 'units', 'micrometers (um)')
    ierr = nf90_put_att(ncid, vid_reff, '_FillValue', 0.0_rk)

    ierr = nf90_def_var(ncid, 'Temperature_K', NF90_DOUBLE, (/did_t/), vid_temp); if (ierr /= nf90_noerr) stop 'defvar temp'
    ierr = nf90_put_att(ncid, vid_temp, 'units', 'K')

    ierr = nf90_def_var(ncid, 'ke_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_ke); if (ierr /= nf90_noerr) stop 'defvar ke'
    ierr = nf90_put_att(ncid, vid_ke, 'long_name', 'Mass extinction coefficient')
    ierr = nf90_put_att(ncid, vid_ke, 'units', 'm2 kg-1')
    ierr = nf90_put_att(ncid, vid_ke, '_FillValue', 0.0_rk)

    ierr = nf90_def_var(ncid, 'w_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_w); if (ierr /= nf90_noerr) stop 'defvar w'
    ierr = nf90_put_att(ncid, vid_w, 'long_name', 'Single-scattering albedo')
    ierr = nf90_put_att(ncid, vid_w, 'units', '1')
    ierr = nf90_put_att(ncid, vid_w, '_FillValue', 0.0_rk)

    ierr = nf90_def_var(ncid, 'g_MW', NF90_DOUBLE, (/did_t,did_r,did_f/), vid_g); if (ierr /= nf90_noerr) stop 'defvar g'
    ierr = nf90_put_att(ncid, vid_g, 'long_name', 'Asymmetry factor')
    ierr = nf90_put_att(ncid, vid_g, 'units', '1')
    ierr = nf90_put_att(ncid, vid_g, '_FillValue', 0.0_rk)

    ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.8')
    ierr = nf90_enddef(ncid); if (ierr /= nf90_noerr) stop 'enddef failed'

    ierr = nf90_put_var(ncid, vid_freq, freq_mw); if (ierr /= nf90_noerr) stop 'write freq'
    ierr = nf90_put_var(ncid, vid_reff, reff_um); if (ierr /= nf90_noerr) stop 'write reff'
    ierr = nf90_put_var(ncid, vid_temp, temps_k); if (ierr /= nf90_noerr) stop 'write temp'
    ierr = nf90_put_var(ncid, vid_ke,   ke); if (ierr /= nf90_noerr) stop 'write ke'
    ierr = nf90_put_var(ncid, vid_w,    w); if (ierr /= nf90_noerr) stop 'write w'
    ierr = nf90_put_var(ncid, vid_g,    g); if (ierr /= nf90_noerr) stop 'write g'

    ierr = nf90_close(ncid); if (ierr /= nf90_noerr) stop 'close failed'
  end subroutine write_mw_file
end module mw_writer

program build_cloudcoeff_mw_schemes
  use util_kinds
  use text_io
  use scheme_mass
  use isca_reader
  use mw_writer
  implicit none

  integer, parameter :: nF = 70, nR = 131, nT = 5
  character(len=*), parameter :: base_dir = 'SnowflakeModel_MW'
  character(len=*), parameter :: temps(nT) = (/'T190','T210','T230','T250','T270'/)
  real(rk) :: freq_mw(nF)
  real(rk) :: temps_k(nT)
  integer :: i
  integer :: tval
  character(len=3) :: tstr

  
  do i = 1, nT
     tstr = temps(i)(2:4)
     read(tstr, '(I3)') tval
     temps_k(i) = real(tval, rk)
  end do
  call build_freq(freq_mw)

  call process_scheme(1, base_dir, freq_mw, temps, temps_k)
  call process_scheme(2, base_dir, freq_mw, temps, temps_k)
  call process_scheme(3, base_dir, freq_mw, temps, temps_k)
  call process_scheme(4, base_dir, freq_mw, temps, temps_k)
contains
  subroutine build_freq(freq_mw)
    real(rk), intent(out) :: freq_mw(:)
    integer :: nread
    call read_vector_file('MW_freq.txt', freq_mw, nread, .true.)
    if (nread /= size(freq_mw)) stop 'MW_freq.txt length mismatch'
  end subroutine build_freq

  subroutine process_scheme(scheme_id, base_dir, freq_mw, temps, temps_k)
    integer,      intent(in) :: scheme_id
    character(*), intent(in) :: base_dir
    real(rk),     intent(in) :: freq_mw(:)
    character(*), intent(in) :: temps(:)
    real(rk),     intent(in) :: temps_k(:)
    character(len=512) :: ddir, tdir, iscapath
    real(rk) :: reff_um(nR)
    real(rk) :: dmax_um(nR), vol_um3(nR), area_um2(nR), qext(nR), wv(nR), gv(nR)
    real(rk) :: fchk(nF)
    real(rk) :: masses_g(nR)
    real(rk) :: ke(nT,nR,nF), wout(nT,nR,nF), gout(nT,nR,nF)
    integer :: t, f, s
    real(rk) :: area_m2(nR), m_kg(nR), ke_row(nR)
    character(len=256) :: outfile

    write(ddir,'(A,"/Density",I1)') trim(base_dir), scheme_id

    write(tdir,'(A,"/",A)') trim(ddir), trim(temps(1))
    write(iscapath,'(A,"/isca_freq.dat")') trim(tdir)
    call read_isca_block(trim(iscapath), nF, nR, fchk, dmax_um, vol_um3, area_um2, qext, wv, gv)
    call assert_freq_match(freq_mw, fchk)

    do s=1,nR
      select case(scheme_id)
      case(1); masses_g(s) = mass_g_density1(dmax_um(s))
      case(2); masses_g(s) = mass_g_density2(dmax_um(s))
      case(3); masses_g(s) = mass_g_density3(dmax_um(s))
      case(4); masses_g(s) = mass_g_density4(dmax_um(s))
      end select
      reff_um(s) = reff_um_from_mass_g(masses_g(s))
    end do
    call assert_monotonic_increasing(reff_um)

    ke = 0.0_rk; wout = 0.0_rk; gout = 0.0_rk

    do t=1,nT
      write(tdir,'(A,"/",A)') trim(ddir), trim(temps(t))
      write(iscapath,'(A,"/isca_freq.dat")') trim(tdir)
      call fill_from_isca(trim(iscapath), freq_mw, masses_g, ke(t,:,:), wout(t,:,:), gout(t,:,:))
    end do

    write(outfile,'("CloudCoeff_MW_Snowflake_Density",I1,".nc4")') scheme_id
    call write_mw_file(trim(outfile), freq_mw, reff_um, temps_k, ke, wout, gout)
  end subroutine process_scheme

  subroutine fill_from_isca(iscapath, freq_mw, masses_g, ke_slice, w_slice, g_slice)
    character(*), intent(in)  :: iscapath
    real(rk),     intent(in)  :: freq_mw(:)
    real(rk),     intent(in)  :: masses_g(:)
    real(rk),     intent(out) :: ke_slice(:,:)
    real(rk),     intent(out) :: w_slice(:,:)
    real(rk),     intent(out) :: g_slice(:,:)
    integer :: u, ios, f, s, nF_loc, nR_loc
    real(rk) :: fv, du, vv, av, qv, wv, gv
    real(rk) :: area_m2, m_kg
    nF_loc = size(freq_mw); nR_loc = size(masses_g)
    open(newunit=u, file=iscapath, status='old', action='read', iostat=ios)
    if (ios /= 0) stop 'open isca failed'
    do f=1,nF_loc
      do s=1,nR_loc
        read(u,*,iostat=ios) fv, du, vv, av, qv, wv, gv
        if (ios /= 0) stop 'bad isca record'
        if (abs(fv - freq_mw(f)) > 1.0e-3_rk) stop 'frequency mismatch within isca_freq.dat'
        area_m2 = av*1.0e-12_rk
        m_kg    = masses_g(s)*1.0e-3_rk
        ke_slice(s,f) = qv*area_m2 / max(m_kg, 1.0e-300_rk)
        w_slice(s,f)  = min(max(wv, 0.0_rk), 1.0_rk)
        g_slice(s,f)  = max(min(gv, 1.0_rk), -1.0_rk)
      end do
    end do
    close(u)
  end subroutine fill_from_isca

  subroutine assert_freq_match(a, b)
    real(rk), intent(in) :: a(:), b(:)
    integer :: i
    if (size(a) /= size(b)) stop 'frequency vector length mismatch'
    do i=1,size(a)
       if (abs(a(i)-b(i)) > 1.0e-3_rk) then
          print '(I5,3G12.4)', i, a(i), b(i), abs(a(i)-b(i))
          stop 'frequency mismatch'
       end if
    end do
  end subroutine assert_freq_match

  subroutine assert_monotonic_increasing(x)
    real(rk), intent(in) :: x(:)
    integer :: i
    do i=1,size(x)-1
      if (x(i+1) <= x(i)) stop 'Reff not strictly increasing'
    end do
  end subroutine assert_monotonic_increasing
end program build_cloudcoeff_mw_schemes
