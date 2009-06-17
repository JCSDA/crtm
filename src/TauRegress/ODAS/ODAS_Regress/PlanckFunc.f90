!====================================================================
!
! MODULE: PlanckFunc
!
!   SUBROUTINE: Calc_PlanckTempToRad()
!   SUBROUTINE: Calc_PlanckRadToTemp()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02 to make consistent with OPTRAN
!
!====================================================================

module PlanckFunc

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Calc_PlanckTempToRad
  Public  Calc_PlanckRadToTemp


 contains


  !====================================================================
  !
  ! SUBROUTINE: Calc_PlanckTempToRad()
  !
  !   Calc_PlanckTempToRad() calculates an radiance of a specified 
  !   channel from black body using the Planck Law. 
  !
  !====================================================================

  function Calc_PlanckTempToRad( channel, temperature ) result( radiance )

    !--- interface

    integer      ,intent(in) :: channel         ! ch seq #
    real(fp_kind),intent(in) :: temperature     ! temperature (K)
    real(fp_kind)            :: radiance        ! radiance

    !--- local variables

    real(fp_kind) :: effective_temperature


    !--- apply the polychromaticity correction
    !--- to obtain an effective temperature

    effective_temperature = SpcCoeff%band_c1( channel ) &
                                + ( SpcCoeff%band_c2( channel ) * temperature )


    !--- calculate the Planck radiance

    radiance =                  SpcCoeff%planck_c1( channel )  / &
    !          ----------------------------------------------------------------------
               ( EXP( SpcCoeff%planck_c2( channel ) / effective_temperature ) - ONE )

  end function Calc_PlanckTempToRad



  !====================================================================
  !
  ! SUBROUTINE: Calc_PlanckRadToTemp()
  !
  !   Calc_PlanckRadToTemp() calculates a black body surface
  !   temperature from its emissivity of a specified channel
  !   using the Planck Law.
  !
  !====================================================================

  function Calc_PlanckRadToTemp( channel, radiance ) result( temperature )

    !--- interface

    integer      ,intent(in) :: channel         ! ch seq #
    real(fp_kind),intent(in) :: radiance        ! radiance
    real(fp_kind)            :: temperature     ! temperature (K)

    !--- local variables

    real(fp_kind) :: effective_temperature


    !--- calculate the effective temperature

    effective_temperature =              SpcCoeff%planck_c2( channel )  / &
    !                       ---------------------------------------------------------
                            LOG( ( SpcCoeff%planck_c1( channel ) / radiance ) + ONE )


    !--- apply the polychromatic correction to 
    !--- obtain the true temperature

    temperature = ( effective_temperature - SpcCoeff%band_c1( channel ) ) / &
    !             ---------------------------------------------------------
                                SpcCoeff%band_c2( channel )

  end function Calc_PlanckRadToTemp

end module PlanckFunc
