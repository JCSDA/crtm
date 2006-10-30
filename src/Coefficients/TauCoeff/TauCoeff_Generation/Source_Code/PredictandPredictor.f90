!====================================================================
!
! MODULE: PredictandPredictor
!
!   SUBROUTINE: Get_Predictand()
!   SUBROUTINE: Get_AtmosPredictor()
!   FUNCTION:   Check_SensitivePredSet()
!   SUBROUTINE: Unuse_PredFlag()
!   SUBROUTINE: Get_RegPredictor()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02 to introduce Check_SensitivePredSet()
!
! replace the subroutine Get_AtmosPredictor_Profile by Compute_Predictors from
! the module predictors.  Yong Han, 2003/06/26
!
!====================================================================

module PredictandPredictor

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef
  use predictors


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Get_Predictand
  Public  Get_AtmosPredictor
  Public  Check_SensitivePredSet
  Public  Unuse_PredFlag
  Public  Get_RegPredictor


 contains


  !====================================================================
  !
  ! SUBROUTINE: Get_Predictand()
  !
  !   Get_Predictand() calculates predictands for regression
  !   coefficient generation from transmittance profiles.
  !   Logarithm of absorption coefficnet is introduced as
  !   predictand
  !
  !                   d ln Trans
  !     ln k = ln ( - ---------- )
  !                      d A
  !
  !   where
  !
  !     k : an absorption coefficnet
  !     A : an absorber amount
  !
  !   On descrete space, it is modified by
  !
  !                      ln ( Trans_lev(l-1) / Trans_lev(l) )
  !     ln k_lay(l) = ln ------------------------------------
  !                              A_lev(l) - A_lev(l-1)  
  !
  !   The predictand calculation dose not allow zero transmittance
  !   change, otherwise zero logarithm calculation would occur.
  !   In such case, predictands are estimated exterpolating toward
  !   top and bottom.
  !   The estimation is crucial for the stability of regression
  !   estimation over zero transmittance change region.
  !
  !====================================================================

  subroutine Get_Predictand( trans_lev,     &
                             absamount_lev, &
                             predictand_lay )

    !--- interface

    real(fp_kind),intent(in)  :: trans_lev     (0:Nlay,Nangle,Natm)     ! particular gas transmittances
    real(fp_kind),intent(in)  :: absamount_lev (0:Nlay,Nangle,Natm)     ! absorber mount on levels
    real(fp_kind),intent(out) :: predictand_lay(  Nlay,Nangle,Natm)     ! predictands

    !--- local variable

    integer :: Iatm
    integer :: Iang

    !--- predictands over all profiles
    
    do Iatm = 1, Natm
    do Iang = 1, Nangle

      call Get_Predictand_Profile( trans_lev     (0,Iang,Iatm), &
                                   absamount_lev (0,Iang,Iatm), &
                                   predictand_lay(1,Iang,Iatm)  )

    enddo
    enddo
    
  end subroutine Get_Predictand



  !====================================================================
  ! Predictand for one profile
  !====================================================================

  subroutine Get_Predictand_Profile( trans_lev,     &
                                     absamount_lev, &
                                     predictand_lay ) 

    !--- interface

    real(fp_kind),intent(in)  :: trans_lev     (0:Nlay)     ! particular gas transmittances
    real(fp_kind),intent(in)  :: absamount_lev (0:Nlay)     ! absorber mount on levels
    real(fp_kind),intent(out) :: predictand_lay(  Nlay)     ! predictands

    !--- local variable

    real(fp_kind)     :: sum
    integer           :: num
    integer           :: Ilev, Ilev_1

    integer,parameter :: NumLevsUse_KEstMean = 3


    !--- initialize

    predictand_lay(:) = RMISS


    !--- layer loop

    Ilev_1 = 0
    do Ilev = 1, Nlay

      !--- check if trans_lev is useful

      if( trans_lev(Ilev_1) < TOLERANCE  .or. &
          trans_lev(Ilev  ) < TOLERANCE )exit


      !--- pass zero absorbing layer

      if( ( trans_lev(Ilev_1) - trans_lev(Ilev) ) < 10._fp_kind * TOLERANCE .and. &
            trans_lev(Ilev_1) >= POINT1 )cycle


      !--- a predictand is not calculated at the top

      if( Ilev > 1 .and. trans_lev(Ilev-1) == ONE  .and. &
                         trans_lev(Ilev  ) <  ONE )then
        Ilev_1 = Ilev
        cycle
      endif


      !--- calculate ln (absorber coef)

      predictand_lay(Ilev) = &
             log( - log( trans_lev    (Ilev) / trans_lev    (Ilev_1) ) &
                     / ( absamount_lev(Ilev) - absamount_lev(Ilev_1) ) )


      !--- update Ilev_1

      Ilev_1 = Ilev

    enddo


    !=== create artificial absorption coefficient over missing layers ================

    !--- exterpolate predictands toward top
    
    if( predictand_lay(1) < HMISS )then

      !--- find useful k from top

      find_loop_top: do
        do Ilev = 2, Nlay-1

          if( predictand_lay(Ilev) > HMISS )then
            Ilev_1 = Ilev
            exit find_loop_top
          endif

        enddo
        return          ! all ks are missing
      enddo find_loop_top


      !--- calc mean

      sum = ZERO
      num = 0

      do Ilev = Ilev_1, Nlay

        if( predictand_lay(Ilev) < HMISS )cycle

        num = num + 1
        sum = sum + predictand_lay(Ilev)

        if( num >= NumLevsUse_KEstMean )exit

      enddo

      !--- exterpolate

      if( num > 0 )then

        predictand_lay(1:(Ilev_1-1)) = sum / num

      endif

    endif


    !--- exterpolate predictands toward bottom

    if( predictand_lay(Nlay) < HMISS )then


      !--- find no missing value from bottom

      find_loop_bottom: do 
        do Ilev = Nlay-1, 1, -1

          if( predictand_lay(Ilev) > HMISS )then
            Ilev_1 = Ilev
            exit find_loop_bottom
          endif

        enddo
        return          ! all missing
      enddo find_loop_bottom


      !--- calc mean

      sum = ZERO
      num  = 0

      do Ilev = Ilev_1, 1, -1

        if( predictand_lay(Ilev) < HMISS )cycle

        num = num + 1
        sum = sum + predictand_lay(Ilev)

        if( num >= NumLevsUse_KEstMean )exit

      enddo

      !--- exterpolate
      
      if( num > 0 )then

        predictand_lay((Ilev_1+1):Nlay) = sum / num

      endif

    endif

  end subroutine Get_Predictand_Profile



  !====================================================================
  !
  ! SUBROUTINE: Get_AtmosPredictor()
  !
  !   Get_AtmosPredictor() calculates OPTRAN atmospheric predictors.
  !   The list of predictors is shown below,
  !
  !     X(1)    T
  !     X(2)    P
  !     X(3)    T^2
  !     X(4)    P^2
  !     X(5)    TxP
  !     X(6)    T^2xP
  !     X(7)    TxP^2
  !     X(8)    T^2xP^2
  !     X(9)    T*
  !     X(10)   P*
  !     X(11)   T**
  !     X(12)   P**
  !     X(13)   T***
  !     X(14)   P***
  !     X(15)   Water vapor mixing ratio, W
  !     X(16)   W/T^2
  !     X(17)   P^0.25
  !
  !====================================================================

  subroutine Get_AtmosPredictor( p_lay, t_lay, q_lay, o3_lay, &
                                 absamount_lev, &
                                 atmpred_lay )

    !--- interface

    real(fp_kind),intent(in)   ::  p_lay(Nlay)          ! pressure on layers (hPa)
    real(fp_kind),intent(in)   ::  t_lay(Nlay,Natm)     ! temperature (K)
    real(fp_kind),intent(in)   ::  q_lay(Nlay,Natm)     ! wv mr (g/kg)
    real(fp_kind),intent(in)   :: o3_lay(Nlay,Natm)     ! ozone content (ppmw)
    real(fp_kind),intent(in)   :: absamount_lev(0:Nlay,Nangle,Natm)             ! absorber amounts on levels
    real(fp_kind),intent(out)  :: atmpred_lay(Natmpred_max,Nlay,Nangle,Natm)    ! atmos predictors

    !--- local variable

    integer :: Iatm, Iang
    real(fp_kind) :: Absorber(0:Nlay, 1)


    !--- compute standard predictors

    do Iatm = 1, Natm
    do Iang = 1, Nangle 


      Absorber(0:Nlay,1) = absamount_lev (0:Nlay,Iang,Iatm)
      call Compute_Predictors(p_lay, t_lay(:,Iatm), &
                              q_lay(:,Iatm), &
			      Absorber, &
			      atmpred_lay (:,:,Iang,Iatm) )

    enddo
    enddo

  end subroutine Get_AtmosPredictor


   
  !====================================================================
  ! Predictor for one profile
  !====================================================================

  subroutine Get_AtmosPredictor_Profile( p_lay, t_lay, q_lay, o3_lay, &
                                         absamount_lev, &
                                         atmpred_lay )
  
    !--- interface

    real(fp_kind),intent(in)   ::  p_lay(Nlay)          ! pressure on layers (hPa)
    real(fp_kind),intent(in)   ::  t_lay(Nlay)          ! temperature (K)
    real(fp_kind),intent(in)   ::  q_lay(Nlay)          ! wv mr (g/kg)
    real(fp_kind),intent(in)   :: o3_lay(Nlay)          ! ozone content (ppmw)
    real(fp_kind),intent(in)   :: absamount_lev(0:Nlay)             ! absorber amounts on levels
    real(fp_kind),intent(out)  :: atmpred_lay(Natmpred_max,Nlay)    ! atmos predictors

    !--- local variables

    real(fp_kind) :: s(9)
    real(fp_kind) :: x(9,0:Nlay)
    real(fp_kind) :: factor_1, factor_2
    real(fp_kind) :: inverse_1, inverse_2, inverse_3
    real(fp_kind) :: d_absorber
    integer       :: Ilay


    !--- standard predictors

    atmpred_lay( 1,1:Nlay) = t_lay(1:Nlay)
    atmpred_lay( 2,1:Nlay) = p_lay(1:Nlay)
    atmpred_lay( 3,1:Nlay) = t_lay(1:Nlay)**2
    atmpred_lay( 4,1:Nlay) = p_lay(1:Nlay)**2
    atmpred_lay( 5,1:Nlay) = t_lay(1:Nlay)*p_lay(1:Nlay)
    atmpred_lay( 6,1:Nlay) = atmpred_lay(3,1:Nlay)*p_lay(1:Nlay)
    atmpred_lay( 7,1:Nlay) = atmpred_lay(4,1:Nlay)*t_lay(1:Nlay)
    atmpred_lay( 8,1:Nlay) = atmpred_lay(3,1:Nlay)*atmpred_lay(4,1:Nlay)
    atmpred_lay(15,1:Nlay) =  q_lay(1:Nlay)

    atmpred_lay(16,1:Nlay) = q_lay (1:Nlay) / (t_lay(1:Nlay)**2)	! for H2O continuum
    atmpred_lay(17,1:Nlay) = sqrt(sqrt( p_lay(1:Nlay) ))	! for ozo & wet(high peak)

    !--- integrated predictors

    s(:)   = ZERO
    x(:,:) = ZERO

    do Ilay = 1, Nlay

      d_absorber =   absamount_lev(Ilay)    - absamount_lev(Ilay-1)
      factor_1   = ( absamount_lev(Ilay)    + absamount_lev(Ilay-1)    ) * d_absorber
      factor_2   = ( absamount_lev(Ilay)**2 + absamount_lev(Ilay-1)**2 ) * d_absorber

      s(1) = s(1) +  t_lay(Ilay) * d_absorber
      s(2) = s(2) +  p_lay(Ilay) * d_absorber
      s(3) = s(3) +  t_lay(Ilay) * factor_1
      s(4) = s(4) +  p_lay(Ilay) * factor_1
      s(5) = s(5) +  t_lay(Ilay) * factor_2
      s(6) = s(6) +  p_lay(Ilay) * factor_2

      if( absamount_lev(Ilay) /= ZERO )then
        inverse_1 = ONE / absamount_lev(Ilay)
      else
        inverse_1 = ZERO
      endif

      inverse_2 = inverse_1 * inverse_1
      inverse_3 = inverse_2 * inverse_1

      x(1,Ilay) = s(1) * POINT5  * inverse_1
      x(2,Ilay) = s(2) * POINT5  * inverse_1
      x(3,Ilay) = s(3) * POINT5  * inverse_2
      x(4,Ilay) = s(4) * POINT5  * inverse_2
      x(5,Ilay) = s(5) * POINT75 * inverse_3
      x(6,Ilay) = s(6) * POINT75 * inverse_3

      atmpred_lay( 9,Ilay) = x(1,Ilay) + x(1,Ilay-1)
      atmpred_lay(10,Ilay) = x(2,Ilay) + x(2,Ilay-1)
      atmpred_lay(11,Ilay) = x(3,Ilay) + x(3,Ilay-1)
      atmpred_lay(12,Ilay) = x(4,Ilay) + x(4,Ilay-1)
      atmpred_lay(13,Ilay) = x(5,Ilay) + x(5,Ilay-1)
      atmpred_lay(14,Ilay) = x(6,Ilay) + x(6,Ilay-1)

    enddo
  
  end subroutine Get_AtmosPredictor_Profile
  


  !====================================================================
  !
  ! FUNCTION: Check_SensitivePredSet()
  !
  !   Check_SensitivePredSet() checks whether a specified predictor set
  !   is a sensitive set or not.
  !
  !
  ! Note)
  !   # of combinations
  !   
  !     Natmpred_max           = 17 
  !     Natmpred_allcombsearch = 6
  !       Dry pass,rjct : 738  4267 
  !       Wet pass,rjct : 567 11809
  !       Ozo pass,rjct : 461  4544
  !
  !   The indexes of predictor_flag are reordered on the line with
  !   the RT model
  !====================================================================
  
  function Check_SensitivePredSet( Natmpred, predcomb ) result( Iflag )
  
    !--- interface
  
    integer,intent(in) :: Natmpred             ! # of atmospheric predictors
    integer,intent(in) :: predcomb(Natmpred)   ! an atmos predictor index set
    integer            :: Iflag                ! 0) not sensitive, 1) sensetive

    !--- local variables

    integer :: predictor_flag(Natmpred_max)
    integer :: n_p_sense1, n_p_sense2, n_p_sense3
    integer :: n_t_sense1

    !--- set flags

    predictor_flag = 0
    predictor_flag( predcomb(1:Natmpred) ) = 1


    !---------------------
    !--- check for dry ---
    !---------------------

    if( Iabsorber == 1 )then

      !--- two or more out of 
      !---   p,p*,p**,p***
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag( 2) &
                   + predictor_flag(13) &
                   + predictor_flag(15) &
                   + predictor_flag(17)

      if( n_p_sense1 >= 2 )then
        Iflag = 1
        return
      endif
      
      !--- both
      !---   t,t^2
      !--- cannot be used at a time

      n_t_sense1 =   predictor_flag(1) &
                   + predictor_flag(3)

      if( n_t_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- two or more out of 
      !---   t*,t**,t***
      !--- cannot be used at a time

      n_t_sense1 =   predictor_flag(12) &
                   + predictor_flag(14) &
                   + predictor_flag(16)

      if( n_t_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- all of
      !---   p^2,tp^2,t^2p^2
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(4) &
                   + predictor_flag(7) &
                   + predictor_flag(8)

      if( n_p_sense1 >= 3 )then
        Iflag = 1
        return
      endif
      
      !--- all of
      !---   p,tp,t^2p
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(2) &
                   + predictor_flag(5) &
                   + predictor_flag(6)

      if( n_p_sense1 >= 3 )then
        Iflag = 1
        return
      endif

    endif

    
    !---------------------
    !--- check for wet ---
    !---------------------

    if( Iabsorber == 2 )then

      !--- two or more out of
      !---   t,t^2,t*,t**,t***
      !--- cannot be used at a time

      n_t_sense1 =   predictor_flag( 1) &
                   + predictor_flag( 3) &
                   + predictor_flag(12) &
                   + predictor_flag(14) &
                   + predictor_flag(16)

      if( n_t_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- two or more out of
      !---   p^2,tp^2,t^2p^2
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(4) &
                   + predictor_flag(7) &
                   + predictor_flag(8)

      if( n_p_sense1 >= 2 )then
        Iflag = 1
        return
      endif
      
      !--- two or more out of
      !---   p,tp,t^2p
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(2) &
                   + predictor_flag(5) &
                   + predictor_flag(6)

      if( n_p_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- two or more out of 
      !---   p*,p**,p***
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(13) &
                   + predictor_flag(15) &
                   + predictor_flag(17)
      
      if( n_p_sense1 >= 2 )then
        Iflag = 1
        return
      endif
      
    endif


    !---------------------
    !--- check for ozo ---
    !---------------------

    if( Iabsorber == 3 )then

      !--- both of
      !---   t,t^2
      !--- cannot be used at a time

      n_t_sense1 =   predictor_flag( 1) &
                   + predictor_flag( 3)

      if( n_t_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- two or more out of
      !---   t*,t**,t***
      !--- cannot be used at a time

      n_t_sense1 = + predictor_flag(12) &
                   + predictor_flag(14) &
                   + predictor_flag(16)

      if( n_t_sense1 >= 2 )then
        Iflag = 1
        return
      endif

      !--- two or more out of 
      !---   p*,p**,p***
      !--- cannot be used at a time
      
      n_p_sense1 =   predictor_flag(13) &
                   + predictor_flag(15) &
                   + predictor_flag(17)
      
      if( n_p_sense1 >= 2 )then
        Iflag = 1
        return
      endif
      
      !--- tp^2 cannot be used with p^2 or t^2p^2
      
      n_p_sense1 =   predictor_flag(4) &
                   + predictor_flag(8)

      if( predictor_flag(7) == 1 .and. n_p_sense1 >= 1 )then
        Iflag = 1
        return
      endif
      
      !--- tp cannot be used with p or t^2p
      
      n_p_sense1 =   predictor_flag(2) &
                   + predictor_flag(6)

      if( predictor_flag(5) == 1 .and. n_p_sense1 >= 1 )then
        Iflag = 1
        return
      endif

    endif


    !--- not sensitive predictor set
    
    Iflag = 0

  end function Check_SensitivePredSet



  !====================================================================
  !
  ! SUBROUTINE: Unuse_PredFlag()
  !
  !   Unuse_PredFlag set unused for the flags of atmospheric
  !   predictors according to the usage rule.
  !   ex) Predictor q cannot be used for ozone trans and
  !       dry trans of not wet channel.
  !
  !====================================================================

  subroutine Unuse_PredFlag( chtype,    &
                             atmpredflag )

    !--- interface

    integer,intent(in)    :: chtype
    integer,intent(inout) :: atmpredflag(Natmpred_max)

 
    !--- dry gas prediction

    if( Iabsorber == 1 )then

      !--- q can be used only wet absorption line dominated region
      if( chtype /= 2 .and. chtype /= 12 )then
        atmpredflag(10) = 0
      endif
      
      !--- q/T^2, p^(1/4) cannot be used
      atmpredflag(11) = 0
      atmpredflag(9) = 0

      return

    endif


    !--- wet gas trans prediction

    if( Iabsorber == 2 )then
 
      !--- p^(1/4) can be used only wet absorption line dominated region
      if( chtype /= 2 .and. chtype /= 12 )then
        atmpredflag(9) = 0
      endif

      return 
 
    endif
      

    !--- ozo gas trans prediction

    if( Iabsorber == 3 )then

      !--- q cannot be used
      atmpredflag(10) = 0
      
      !--- q/T^2 cannot be used
      atmpredflag(11) = 0

      return

    endif

  end subroutine Unuse_PredFlag  


  
  !====================================================================
  !
  ! SUBROUTINE: Get_RegPredictor()
  !
  !   Get_RegPredictor() calculates regression predictors,
  !   which are calculated as a function of atmospheric predictors and
  !   absorber amount level values.
  !   Each predictor is defined by,
  !
  !     Predictor = AtmosPred * AbsAmountLev ** p
  !
  !====================================================================

  subroutine Get_RegPredictor( Natmpred, predcomb, &
                               atmpred_lay, abslev_lay, &
                               Npolyorder, &
                               predictors )

    !--- interface

    integer      ,intent(in)  :: Natmpred                               ! # of atmospheric predictors
    integer      ,intent(in)  :: predcomb(Natmpred)                     ! a atmos predictor index set
    real(fp_kind),intent(in)  :: atmpred_lay(Natmpred_max)              ! atmos predictors on a layer
    real(fp_kind),intent(in)  :: abslev_lay                             ! an absorber amount level on a layer
    integer      ,intent(in)  :: Npolyorder
    real(fp_kind),intent(out) :: predictors(0:Npolyorder,0:Natmpred)    ! regression predictors

    !--- local variables

    integer       :: Iorder
    real(fp_kind) :: levpow


    !--- calc predictors

    if( Natmpred > 0 )then

      levpow = ONE
      do Iorder = 0, Npolyorder
        predictors(Iorder,0         ) = levpow
        predictors(Iorder,1:Natmpred) = levpow * atmpred_lay(predcomb(1:Natmpred))
        levpow = levpow * abslev_lay
      enddo
    
    else  !-- zero atmos predictor

      levpow = ONE
      do Iorder = 0, Npolyorder
        predictors(Iorder,0) = levpow
        levpow = levpow * abslev_lay
      enddo

    endif

  end subroutine Get_RegPredictor

end module PredictandPredictor
