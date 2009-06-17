!====================================================================
!
! MODULE: AbsorberAmount
!
!   SUBROUTINE: Calc_AbsAmount()
!   SUBROUTINE: Conv_AbsAmountToLevel()
!   SUBROUTINE: Conv_AbsAmountToLevel_profile()
!   SUBROUTINE: Conv_AbsLevelToAmount()
!   SUBROUTINE: Conv_AbsLevelToAmount_profile()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02
!
! Absorber amount is now computed by Compute_Absorber_Amount() from the
! module absorber_profile
!     Yong Han, 2003/06/26
!====================================================================

module AbsorberAmount

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef
  use absorber_profile

  !--- implicit

  implicit none


  !--- private & public

  private

  public Calc_AbsAmount
  public Calc_CoefAmountLevel
  public Conv_AbsAmountToLevel
  public Conv_AbsAmountToLevel_profile
  public Conv_AbsLevelToAmount
  public Conv_AbsLevelToAmount_profile


 contains


  !====================================================================
  !
  ! SUBROUTINE: Calc_AbsAmount()
  !
  !   Get_AbsAmount() calculates slant path absorber amounts
  !   and parameters used in conversion between absorber amount
  !   space and absorber amount level space.
  !
  !
  ! Level)
  !   dry)
  !     Ad_lev(0) = sec(incang) p_lev(0)
  !     Ad_lev(k) = sec(incang) p_lev(k)
  !
  !   wet,ozone)
  !     Aw,o_lev(0) = 0
  !     Aw,o_lev(k) = sec(incang) sum 1/g q,o_lay(k) dp
  !
  ! Layer)
  !   A_lay(k) = ( A_lev(k-1) + A_lev(k) ) / 2
  !
  !====================================================================

  subroutine Calc_AbsAmount( p_lev, q_lay, o3_lay, &
                             absamount_lev, absamount_lay )

    !--- interface

    real(fp_kind),intent(in)  ::  p_lev(0:Nlay)                     ! pressure (hPa)
    real(fp_kind),intent(in)  ::  q_lay(  Nlay,Natm)                ! wvmr (g/kg)
    real(fp_kind),intent(in)  :: o3_lay(  Nlay,Natm)                ! ozone (ppmw)
    real(fp_kind),intent(out) :: absamount_lev(0:Nlay,Nangle,Natm)  ! absorber amounts on levels
    real(fp_kind),intent(out) :: absamount_lay(  Nlay,Nangle,Natm)  ! absorber amounts on layers

    !--- local variables

    real(fp_kind) :: dp
    real(fp_kind) :: absorber(0:Nlay, Nabsorber)
    integer       :: Iatm, Iang, Ilev


    do Iang =1, Nangle
     do Iatm = 1, Natm
    
      call Compute_Absorber_Amount(p_lev(1:), &
                        q_lay( :, Iatm) * Geometric_Secant(:, Iang, Iatm), &
                        o3_lay(:, Iatm) * Geometric_Secant(:, Iang, Iatm), &
                        absorber)

!      call Compute_Absorber_Amount(p_lev(1:), &
!                        q_lay( :, Iatm), &
!                        o3_lay(:, Iatm), &
!                        absorber)
      
      ! case 1 - dry, case 2 - wet, case 3 - ozo
      
      select case(Iabsorber)
        case(1) 
         absamount_lev(0, Iang, Iatm) = absorber(0, 2) * Geometric_Secant(1, Iang, Iatm) !dry case
         absamount_lev(1:Nlay, Iang, Iatm) = absorber(1:Nlay, 2) * Geometric_Secant(:, Iang, Iatm) !dry case
       case(2)
         absamount_lev(:, Iang, Iatm) = absorber(:, 1) !wet case
       case(3)
         absamount_lev(:, Iang, Iatm) = absorber(:, 3) !ozo case
      end select
!      select case(Iabsorber)
!        case(1) 
!          absamount_lev(:, 1, Iatm) = absorber(:, 2) !dry case
!        case(2)
!          absamount_lev(:, 1, Iatm) = absorber(:, 1) !wet case
!        case(3)
!          absamount_lev(:, 1, Iatm) = absorber(:, 3) !ozo case
!      end select

    end do
   end do
   
    !--- the top of level

!    select case(Iabsorber)
!      case(1)
!        absamount_lev(0,1,:) = p_lev(0)
!      case(2:3)
!        absamount_lev(0,1,:) = ZERO
!    end select


    !--- nadir absorber amounts on levels

!    do Iatm = 1, Natm
!    do Ilev = 1, Nlay

!      dp = p_lev(Ilev) - p_lev(Ilev-1)

!      select case(Iabsorber)
!        case(1)
!          absamount_lev(Ilev,1,Iatm) = p_lev(Ilev)
!        case(2)
!          absamount_lev(Ilev,1,Iatm) = absamount_lev(Ilev-1,1,Iatm) &
!                                        + RECIPROCAL_GRAVITY * dp * q_lay(Ilev,Iatm)
!        case(3)
!          absamount_lev(Ilev,1,Iatm) = absamount_lev(Ilev-1,1,Iatm) &
!                                       + RECIPROCAL_GRAVITY * dp * o3_lay(Ilev,Iatm)
!      end select

!    enddo
!    enddo

    !--- angle absorber amounts on levels

!    do Iang = 2, Nangle
!      absamount_lev(:,Iang,:) = absamount_lev(:,1,:) * secant(Iang)
!    enddo


    !--- absorber amounts on layers

    absamount_lay(1:Nlay,:,:) = (   absamount_lev(1:Nlay    ,:,:) &
                                  + absamount_lev(0:(Nlay-1),:,:) ) / TWO

  end subroutine Calc_AbsAmount



  !====================================================================
  !
  ! SUBROUTINE: Calc_CoefAmountLevel()
  !
  !   Calc_CoefAmountLevel calculates coefficients for absorber 
  !   amount-level conversion.
  !
  !
  ! Amount-level conversion)
  !
  !     A(k) = C1 exp(alpha * k) + C2
  !
  !   alpha : level coordinate constant
  !     alpha defines exponential curve of A as a function of k
  !
  !   C1,C2 : scaling constants
  !     A(0) and A(1) respectively denote the top and bottom of
  !     absorber amount between which LBL transmittance data can 
  !     represent.
  !     Once atmospheric profiles and corresponding transmittance
  !     profiles are given, A(0) and A(1) are obtained.
  !     Therefore C1 and C2 are given by a profile set.
  !
  !====================================================================

  subroutine Calc_CoefAmountLevel( absamount_lev )

    !--- interface

    real(fp_kind),intent(in) :: absamount_lev(0:Nlay,Nangle,Natm)  ! absorber amounts on levels


    !--- alpha, max & min absorber amounts

    if( abs(Alpha) <= TOLERANCE )then
    
      Alpha        = Alpha_list(Iabsorber)

      MinAbsAmount = absamount_lev(0,1,1)
!      MaxAbsAmount = MaxSecAng_MaxAbsAmount * maxval( absamount_lev(Nlay,1,:) )
      MaxAbsAmount = maxval( absamount_lev(Nlay, Nangle,:) )
    
    endif
    

    !--- set Alpha_c1, Alpha_c2

    Abslvl_coef1 = ( MaxAbsAmount - MinAbsAmount ) / ( exp(Alpha) - 1 )
    Abslvl_coef2 = MinAbsAmount - Abslvl_coef1

    print *
    print *, '=== Parameters for absorber amount-level conversion'
    print *, 'Top,Bottom absorber amount A(0),A(1) = ', MinAbsAmount, MaxAbsAmount
    print *, 'Alpha, AbsLvlCoef1, AbsLvlCoef2      = ', Alpha, Abslvl_coef1, Abslvl_coef2

  end subroutine Calc_CoefAmountLevel



  !====================================================================
  !
  ! SUBROUTINE: Conv_AbsAmountToLevel()
  !
  !   Conv_AbsAmountToLevel() converts from an absorber amount value A
  !   to an absorber amount level value k by the equation
  !
  !           1      A(k) - C2
  !     k = ----- ln ---------
  !         alpha        C1
  !
  !   where
  !
  !     alpha : level coordinate constant
  !     C1,C2 : scaling constants
  !
  !====================================================================

  subroutine Conv_AbsAmountToLevel( absamount, n_levels, &
                                    abslev )

    !--- interface

    integer      ,intent(in)  :: n_levels
    real(fp_kind),intent(in)  :: absamount(n_levels,Nangle,Natm)
    real(fp_kind),intent(out) :: abslev   (n_levels,Nangle,Natm)

    !--- local variables
    
    integer :: Iatm, Iang

    !--- conversion

    do Iatm = 1, Natm
    do Iang = 1, Nangle

      call Conv_AbsAmountToLevel_profile( absamount(1,Iang,Iatm), &
                                          n_levels,               &
                                          abslev   (1,Iang,Iatm)  )

    enddo
    enddo

  end subroutine Conv_AbsAmountToLevel


  !====================================================================
  ! Conversion for one profile
  !====================================================================

  subroutine Conv_AbsAmountToLevel_profile( absamount, n_levels, &
                                            abslev )

    integer      ,intent(in)  :: n_levels
    real(fp_kind),intent(in)  :: absamount(n_levels)
    real(fp_kind),intent(out) :: abslev   (n_levels)

    !--- conversion

    abslev(:) = log( ( absamount(:) - Abslvl_coef2 ) / Abslvl_coef1 ) / Alpha

  end subroutine Conv_AbsAmountToLevel_profile 



  !====================================================================
  !
  ! SUBROUTINE: Conv_AbsLevelToAmount()
  !
  !   Conv_AbsLevelToAmount() converts from an absorber amount level
  !   value k to an absorber amount value A by the equation
  !
  !     A(k) = C1 exp(alpha * k) + C2
  !
  !   where
  !
  !     alpha : level coordinate constant
  !     C1,C2 : scaling constants
  !
  !====================================================================

  subroutine Conv_AbsLevelToAmount( abslev, n_levels, &
                                    absamount )

    !--- interface

    integer      ,intent(in)  :: n_levels
    real(fp_kind),intent(in)  :: abslev   (n_levels,Nangle,Natm)
    real(fp_kind),intent(out) :: absamount(n_levels,Nangle,Natm)

    !--- local variables
    
    integer :: Iatm, Iang

    !--- conversion

    do Iatm = 1, Natm
    do Iang = 1, Nangle

      call Conv_AbsLevelToAmount_profile( abslev   (1,Iang,Iatm), &
                                          n_levels,               &
                                          absamount(1,Iang,Iatm)  )

    enddo
    enddo

  end subroutine Conv_AbsLevelToAmount


  !====================================================================
  ! Conversion for one profile
  !====================================================================

  subroutine Conv_AbsLevelToAmount_profile( abslev, n_levels, &
                                            absamount )
                                    
    integer      ,intent(in)  :: n_levels
    real(fp_kind),intent(in)  :: abslev   (n_levels)
    real(fp_kind),intent(out) :: absamount(n_levels)

    !--- conversion

    absamount(:) = Abslvl_coef1 * exp( Alpha * abslev(:) ) + Abslvl_coef2

  end subroutine Conv_AbsLevelToAmount_profile

end module AbsorberAmount
