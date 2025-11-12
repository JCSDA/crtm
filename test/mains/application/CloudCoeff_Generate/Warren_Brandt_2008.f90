subroutine WB2008ice(wavelength,nr,ni)
  implicit none
  !** see http://www.atmos.washington.edu/ice_optical_constants/
  !** Author: Ben Johnson (jbenjam@gmail.com) :: 15 January 2010
  !** no temperature dependence, wavelength input is microns
  !** output is real and imaginary parts of index of refraction
  !** NOTE: need to update to include temperature dependence from Matzler
  real(8), intent(in) :: wavelength
  real(8), intent(out) :: nr, ni
  integer :: fid1,i
  real(8) :: wavel(486), ndxr(486), ndxi(486)
  REAL(8) :: x1,x2,y1,y2,x,y
  integer :: imax
  
  fid1 = 25

  imax = 486

  open(unit=fid1,file='IOP_2008_ASCIItable.dat',status='old')

  do i = 1,imax
    read(fid1,*,end=111) wavel(i),ndxr(i),ndxi(i)
  end do

  111 continue

  if (wavelength < wavel(1) .or. wavelength > wavel(imax)) then
    print *, 'Wavelength out of range:', wavelength, wavel(1), wavel(imax)
    stop 'Stop: Error in input Wavelength'
  end if
  
  do i = 2,imax
    if (wavelength >= wavel(i-1) .and. wavelength < wavel(i)) then
      !** interpolate 
      x1 = log(wavel(i-1))
      x2 = log(wavel(i))
      y1 = ndxr(i-1)
      y2 = ndxr(i)
       x = log(wavelength)
       y = ((x-x1)*(y2-y1)/(x2-x1))+y1
      nr = y       !** interpolate linear real
      !** imaginary part
      y1 = log(abs(ndxi(i-1)))
      y2 = log(abs(ndxi(i)))
       y = ((x-x1)*(y2-y1)/(x2-x1))+y1
      ni = exp(y)  !** interpolate log imaginary
    end if
  end do
end subroutine WB2008ice
