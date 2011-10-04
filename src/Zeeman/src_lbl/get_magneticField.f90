Program get_magneticField


  IMPLICIT NONE

  real     :: lat, lon(4), elevkm(2), gh(170), ext(3), date, x, y, z
  real     :: v, zenith, pha
  integer  :: nmax, iext, ios, iu, i, j, k, io_out

  iu = 10
  open(UNIT=iu, file='IGRF95.DAT', status='old')

  io_out = 12
  open(UNIT=io_out, file='magnetic2.txt', status='replace')

  date = 1996.2
  call getigrf (iu, date, nmax, gh, ios)

!  lon(1) = -90.0
  lon(1) = -66.99
  lon(2) = 0.0
!  lon(3) = 90.0
  lon(3) = 117.04
  lon(4) = 180.0
  elevkm(1) = 40. 
  elevkm(2) = 70.0 ! km
  write(io_out, '(3i5)')4, 2, 181
  do k = 1, 4
  do j = 1, 2
    write(io_out, '(2f6.1)')lon(k), elevkm(j)
    print *, 'lon, elev = ', lon(k), elevkm(j)
  do i = 0, 180

    lat = 90.0 - i 

    call shval3 (lat, lon(k), elevkm(j), &
                 nmax, gh, iext, ext, x, y, z)

    z = -z  ! now z is upward
    v = sqrt(x*x + y*y + z*z)
    zenith = ACOS(z / v) * 180.0 / 3.1415
    pha = ACOS(x / sqrt(x*x+y*y)) * 180.0 / 3.1415

    write(io_out, '(f6.1, f7.4, 2f8.2, 3f8.4)') &
        lat, v*1.0e-5, zenith, pha, x*1.0e-5, y*1.0e-5, z*1.0e-5

  enddo
  enddo
  enddo

end Program get_magneticField
