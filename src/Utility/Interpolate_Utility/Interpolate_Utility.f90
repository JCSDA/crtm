!
! Interpolate_Utility
!
! Container module for interpolation routine modules
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Oct-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Interpolate_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Modules used
  USE Linear_Interpolation,     ONLY: Linear_Interpolate
  USE Polynomial_Interpolation, ONLY: Polynomial_Interpolate
  USE Spline_Interpolation,     ONLY: Spline_Initialize, &
                                      Spline_Interpolate
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Linear_Interpolate
  PUBLIC :: Polynomial_Interpolate
  PUBLIC :: Spline_Initialize
  PUBLIC :: Spline_Interpolate


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id field
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

END MODULE Interpolate_Utility
