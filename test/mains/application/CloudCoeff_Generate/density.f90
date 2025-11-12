function rhoa(T, P)

! This function returns the density of air in g/cm**3.
! The input parameters T and P are the temperature (°C) and 
! pressure (mb)                           

  implicit none
  real(8), intent (in) :: T, P
  real(8) :: rhoa
  real(8) :: r = 287.04

  rhoa = P / ((T + 273.15)*r) * 1.0e-1 

  return 
end function rhoa

!-----------------------------------------------------------------------

function rhow(T) 

! This function returns the density of water in g/cm**3.
! The input parameter t is the temperature in deg. C.

  implicit none

  real(8), intent (in) :: T
  real(8) :: rhow
  real(8) :: temp
  real(8) :: a(0:5) = (/999.8396,18.224944,-7.92221e-3,-55.44846e-6, &
       149.7562e-9,-393.2952e-12/)
  real(8) :: b = 18.159725e-3
  integer :: i

  temp = 1.0 
  rhow = a(0) 
  do i = 1, 5 
     temp = temp*T
     rhow = rhow + a(i)*temp 
  enddo
  rhow = rhow * (1.0e-3) / (1.0 + b*T) 

  return
end function rhow

!-----------------------------------------------------------------------

function rhoi(T) 

! This function returns the density of ice in g/cm**3.
! The input parameter T is the temperature in deg. celsius.

  implicit none

  real(8), intent (in) :: T
  real(8) :: rhoi 
  real(8) :: a0 = 0.9167
  real(8) :: a1 = -1.75e-4
  real(8) :: a2 = -5.0e-7

  rhoi = a0 + a1*T + a2*T*T 

  return 
end function rhoi
