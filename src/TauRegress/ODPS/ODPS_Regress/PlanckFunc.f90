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
! Modified by Yong Han in Oct, 03
!
! Modified by Yong Chen in Jan, 08
!====================================================================

module PlanckFunc

  !--- modules

  USE Type_Kinds, only : fp_kind
  USE SpcCoeff_Define


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Calc_PlanckTempToRad
  Public  Calc_PlanckRadToTemp
  Public  Pred_BrightTemp
  Public  Pred_BrightTemp2

  real(fp_kind), parameter :: ZERO = 0.0_fp_kind
  real(fp_kind),parameter :: ONE     =  1._fp_kind
  real(fp_kind),parameter :: TOLERANCE        = EPSILON( ONE )

 contains


  !====================================================================
  !
  ! SUBROUTINE: Calc_PlanckTempToRad()
  !
  !   Calc_PlanckTempToRad() calculates an radiance of a specified 
  !   channel from black body using the Planck Law. 
  !
  !====================================================================

  function Calc_PlanckTempToRad( channel, temperature, SpcCoeff ) result( radiance )

    !--- interface

    integer      ,intent(in)            :: channel         ! ch seq #
    real(fp_kind),intent(in)            :: temperature     ! temperature (K)
    type(SpcCoeff_type),intent(in)      :: SpcCoeff        ! spectrum coef (freq, Plunck const)

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

  function Calc_PlanckRadToTemp( channel, radiance, SpcCoeff ) result( temperature )

    !--- interface

    integer      ,intent(in)            :: channel         ! ch seq #
    real(fp_kind),intent(in)            :: radiance        ! radiance
    type(SpcCoeff_type),intent(in)      :: SpcCoeff        ! spectrum coef (freq, Plunck const)

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

  !====================================================================
  !
  ! FUNCTION: Pred_BrightTemp()
  !
  !   Pred_BrightTemp() calculates a brightness temperature
  !   from transmittance and temperature profiles.
  !   Only up-welling radiance is considered as
  !
  !     Iup =   sum    B(l) * ( TransAll(l-1) - TransAll(l) )
  !           l=layers
  !
  !====================================================================

  function Pred_BrightTemp( Ichan, alltrans_lev, t_lay, Tskin, &
                            SurfaceEmiss, SpcCoeff ) result( Tb )

    !--- interface

    integer      ,intent(in) :: Ichan                   ! channel seq #
    real(fp_kind),intent(in) :: alltrans_lev(0:)        ! total transmittances
    real(fp_kind),intent(in) :: t_lay(:)                ! temperatures (K) 
    real(fp_kind),intent(in) :: Tskin                   ! skin temperature (K)
    real(fp_kind),intent(in) :: SurfaceEmiss            ! surface emissivity (K)
    type(SpcCoeff_type),intent(in)   :: SpcCoeff        ! spectrum coef (freq, Plunck const)

    real(fp_kind)            :: Tb                      ! brightness temperature (K)

    !--- local variables

    real(fp_kind) :: rad
    real(fp_kind) :: tau, tau1
    integer       :: Ilev, Nlay


    Nlay = size(alltrans_lev) - 1
   
    !--- surface

    if( alltrans_lev(Nlay) > TOLERANCE )then  
      rad = SurfaceEmiss * Calc_PlanckTempToRad(Ichan,Tskin,SpcCoeff) * alltrans_lev(Nlay)
    else
      rad = ZERO
    endif

    !--- upwelling radiance

    do Ilev = 1, Nlay

      tau1 = alltrans_lev(Ilev-1)
      tau  = alltrans_lev(Ilev  )

!!      if( tau1 >= ZERO .and. tau >= ZERO )then

      !--- check if trans decrease as descending from TOA

!!        if( tau1 < tau )then
!!          print *, "### ERROR ###"
!!          print *, " Total trans be decrease as descending from TOA in Tb calculation"
!!          print *, " Levs :", Ilev-1, Ilev
!!          print *, " Trans:", alltrans_lev(1:Ilev)
!!          stop 99  
!!        endif

      !--- accumulate radiance

      rad = rad + Calc_PlanckTempToRad(Ichan,t_lay(Ilev), SpcCoeff) * ( tau1 - tau )

!!      endif

    enddo


    !--- Tb

    Tb = Calc_PlanckRadToTemp(Ichan,rad, SpcCoeff)

  end function Pred_BrightTemp


 function Pred_BrightTemp2( Ichan, alltrans_lev, bt_lay, bTskin, &
                            SurfaceEmiss, SpcCoeff ) result( Tb )

    !--- interface

    integer      ,intent(in) :: Ichan                   ! channel seq #
    real(fp_kind),intent(in) :: alltrans_lev(0:)        ! total transmittances
    real(fp_kind),intent(in) :: bt_lay(:)                ! brightness temperatures (K) 
    real(fp_kind),intent(in) :: bTskin                   ! brightness skin temperature (K)
    real(fp_kind),intent(in) :: SurfaceEmiss            ! surface emissivity (K)
    type(SpcCoeff_type),intent(in)   :: SpcCoeff        ! spectrum coef (freq, Plunck const)

    real(fp_kind)            :: Tb                      ! brightness temperature (K)

    !--- local variables

    real(fp_kind) :: rad
    real(fp_kind) :: tau, tau1
    integer       :: Ilev, Nlay


    Nlay = size(alltrans_lev) - 1
    
    !--- surface

    if( alltrans_lev(Nlay) > TOLERANCE )then
      rad = SurfaceEmiss * bTskin * alltrans_lev(Nlay)
    else
      rad = ZERO
    endif


    !--- upwelling radiance

    do Ilev = 1, Nlay

      tau1 = alltrans_lev(Ilev-1)
      tau  = alltrans_lev(Ilev  )

      if( tau1 >= ZERO .and. tau >= ZERO )then

        !--- check if trans decrease as descending from TOA

        if( tau1 < tau )then
          print *, "### ERROR ###"
          print *, " Total trans be decrease as descending from TOA in Tb calculation"
          print *, " Levs :", Ilev-1, Ilev
          print *, " Trans:", alltrans_lev
          stop 99  
        endif

        !--- accumulate radiance

        rad = rad + Bt_lay(Ilev) * ( tau1 - tau )

      endif

    enddo


    !--- Tb

    Tb = Calc_PlanckRadToTemp(Ichan,rad, SpcCoeff)

  end function Pred_BrightTemp2


end module PlanckFunc
