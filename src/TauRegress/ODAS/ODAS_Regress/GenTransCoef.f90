!==============================================================================
!
! PROGRAM: GenTransCoef
!
!   The program 'GenTransCoef' selects atmospheric predictors used for
!   transmittance estimation by Compact OPTRAN and generates its transmittance
!   coefficients.
!
!   A varying-order algorithm is implemented, which chooses the lowest polynomial order
!   for a specified channel, under the condition that the fitting error is less than
!   the specified threshold  TB_RMSE_MIN. The maximum order is still 10.  Yong Han, March, 2004
!
!
! OPTRAN (up-welling radiance only)
!
! > Up-welling radiance
!
!     Rad = sum B(k) * (Tau(k-1) - Tau(k))
!            k
!
!       Rad    : up-welling radiance from top
!       B(k)   : radiance from layer k
!       Tau(k) : total transmittance from top to the bottom of layer k
!
!
! > Gas transmittance
!
!     Tau = TauWet * TauOzo' * TauDry'
!
!               TauWetOzo                 Tau
!     TauOzo' = --------- ,  TauDry' = ---------
!                TauWet                TauWetOzo
!
!     TauWet    : transmittances effected by only water vapor absorption
!     TauOzo'   : effective transmittance for ozone gas
!     TauDry'   : effective transmittance for dry gas
!     TauWetOzo : transmittances effected by water vapor and ozone absorption
!
!
! > Absorber amount (level)
!
!     Adry(k) = sec(IncAng) P(k) 
!     Awet(k) = sec(IncAng) sum 1/g Q(k) dP
!     Aozo(k) = sec(IncAng) sum 1/g O(k) dP
!                            k
!       A???(k) : absorber amount from top to the bottom of layer k
!       P(k)    : pressure at the bottom of layer k
!       Q,O(k)  : water vapor and ozone content at layer k
!       IncAng  : incidence angle
!
!
! > Absorption coefficnet
!
!                 ln ( TauGas(k-1) / TauGas(k) )
!     Phi(k) = ln ------------------------------
!                         A(k) - A(k-1)
!
!       TauGas(k-1) : gas transmittance
!       Phi(k)      : absorption coefficient at layer k
!
!
! > Prediction of absorption coefficient from atmospheric state
!
!     ln Phi(k) = sum sum  C(i,n) * X(i,k) * L(A(k)) ** n
!                  i   n
!
!       C(i,n) : transmittance coefficient for atmospheric predictor i
!                and polynomial term n
!       X(i,k) : atmospheric predictor i at layer k
!       L(A)   : absorber amount level function in terms of A
!
!     --------------------------------------------------------------------
!     Note that the order of the polynominal, N, varies from channel
!     to channel and its value is determined in the selection process
!     to be discussed later.  The varying order algorithm is a departure
!     from the original fixed-order algorithm developed by Yoshi Tahara and
!     is more stable, accurate and efficient.  Y. Han, March, 04
!     ---------------------------------------------------------------------  
!
! > Absorber amount-level conversion
!
!     A(L) = C1 exp( Alpha * A ) + C2
!
!              1      A - C2
!     L(A) = ----- ln ------
!            Alpha      C1
!
!       Alpha : level coordinate constant which defines exponential curve
!               of A as a function of k
!       C1,C2 : scaling constants
!       Alpha, C1 and C2 satisfy A(0) and A(1) represents minimum and
!       maximum absorber amount respectively.
!
!
! > Regression weight for generating transmittance coefficient         
!
!               Tau(k-1)                    N-1
!     W(k) = ln -------- ( B(N) TauAll(N) + sum | B(i+1) - B(i) | TauAll(i) ) + (offset)
!                Tau(k)                     i=k
!
!   where
!
!     W(k) : regression weight for predicting ln phi(k)
!     TauAll(l) : total transmittance
!
!
! ------------------------------------------------------------------------
! Method of choosing atmospheric predictors,  Yong Han, March, 2004
!
!   There are Natmpred_max atmospheric predictors, from which only  
!   a subset with Natmpred_maxused (< Natmpred_max atmospheric) predictors
!   is selected for a particular channel and used to predict absorption 
!   coefficients.  The procedure to select the best set of predictors is
!   the following:
!
!   (1) Compute rms fitting errors in transmittance idomain for all 
!       combinations of Natmpred_maxused predictors out of the Natmpred_max
!       predictors and select the set of predictors with the smallest error.
!
!   (2) From the selected Natmpred_maxused predictors, do stepwise
!       selection starting from one predictor prediction.  After this
!       step most important predictor occurs first, and so on.  Note,
!       it is possible that it ends up with predictors fewer than 
!       Natmpred_maxused.
!
!   (3) Repeat the above step for a set of polynominal orders, {1, 2, ..., 10}
!       The loop ternimates if the Tb fitting error < TB_RMSE_MIN.
!
!   (4) An extra step is taken trying to avoid high polynomail order (i.g 10)
!       If high order has been selected, it is compared with the lower order.
!       If the result is not significant (say the fitting accuracy has not
!       improved by 0.025 K), the lower order is selected.
!       
!   (5) The fitting accuracy from the above step is also compared with
!       0 order of polynominal fiiting and with the accuracy when
!       transmittances = 1 at all levels.
!
!--------------------------------------------------------------------------
!
! Created by Y.Tahara in Oct,02 
! Modified by Yong Han, cleaning up the codes, fixing bugs, replacing and
!                       updating modules with those used in operation. v1.0
! Modified by Yong Han, March, 2004, adding varying order (max. order = 10) algorithm => V1.1
! Modified by Yong Han, June 24, 2004, limiting max. order = 5 for ozone and microwave => V1.2
! Modified by Yong Han, Dec. 1, 2006, max. order for microwave is changed back to 10 => V1.3
! Modified by Yong Chen, Oct. 2, 2007, microwave surface emissivity is set to 0.98 (actually the
!                          microwave surface emissivity has always been 0.98, not 0.6). 
!==============================================================================


program GenTransCoef

  !--- modules

  use type_kinds, only : fp_kind
  USE Message_Handler    , ONLY : SUCCESS, WARNING, FAILURE, Display_Message     
  use file_utility
  use ParametersGenCoef
  use ReadParameters
  use ReadProfile_netCDF
  use AbsorberAmount
  use PredictandPredictor
  use CalcRegWeight
  use CalcRegCoef
  use PredAbsCoefTransTemp
  use CalcStatTransTemp
  use WriteTransTable
  use MR_PPMV 
  use AtmProfile_Define
  use AtmProfile_netCDF_IO

  !--- implicit

  implicit none


  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'GenTransCoef'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &

  INTEGER, PARAMETER  :: TAUCOEFF_VERSION   = 4
  INTEGER, PARAMETER  :: AbsID_HITRAN_H2O   = 1     ! HITRAN atmospheric indices
  INTEGER, PARAMETER  :: AbsID_HITRAN_O3    = 3
  INTEGER, PARAMETER  :: UnitID_HITRAN_ppmw = 1
  INTEGER, PARAMETER  :: UnitID_HITRAN_gpkg = 3

 
  CHARACTER(512) :: Algorithm_comments
  TYPE( AtmProfile_type ) :: AtmProfile
  character(len=256)  :: ID_Tag   
  character(len=256)  :: title    
  character(len=512)  :: history  
  character(len=256)  :: comment  

  !--- atmospheric profile

  real(fp_kind),allocatable ::  p_lev(:)                    ! lev,atm       ! pressure (hPa)
  real(fp_kind),allocatable ::  p_lay(:)                    ! lay,atm       ! pressure (hPa)
  real(fp_kind),allocatable ::  t_lay(:,:)                    ! lay,atm       ! temperature (K)
  real(fp_kind),allocatable ::  q_lay(:,:)                    ! lay,atm       ! water vapor mixing ratio (g/kg)
  real(fp_kind),allocatable :: o3_lay(:,:)                    ! lay,atm       ! ozone content (ppmv)

  !--- absorber amount and its level

  real(fp_kind),allocatable :: absamount_lay(:,:,:)           ! lev,ang,atm   ! absorber amounts on layers
  real(fp_kind),allocatable :: absamount_lev(:,:,:)           ! lev,ang,atm   ! absorber amounts on levels
  real(fp_kind),allocatable :: abslev_lay(:,:,:)              ! lay,ang,atm   ! absorber amount levels on atmos central layers
  real(fp_kind),allocatable :: abslev_lev(:,:,:)              ! lev,ang,atm   ! absorber amount levels on atmos levels

  !--- transmittances

  real(fp_kind),allocatable ::    trans_lev(:,:,:)            ! lev,ang,atm   ! LBL specified gas trans
  real(fp_kind),allocatable :: drytrans_lev(:,:,:)            ! lev,ang,atm   ! LBL dry gas trans
  real(fp_kind),allocatable :: wettrans_lev(:,:,:)            ! lev,ang,atm   ! LBL wet gas trans
  real(fp_kind),allocatable :: ozntrans_lev(:,:,:)            ! lev,ang,atm   ! LBL ozone gas trans
  real(fp_kind),allocatable :: alltrans_lev(:,:,:)            ! lev,ang,atm   ! LBL total trans
  real(fp_kind),allocatable :: efctrans_lev(:,:,:)            ! lev,ang,atm   ! LBL other two gas trans
  real(fp_kind),allocatable :: caltrans_lev(:,:,:)            ! lev,ang,atm   ! predicted specified gas trans
  real(fp_kind),allocatable :: calalltrans_lev(:,:,:)         ! lev,ang,atm   ! predicted total trans

  !--- regression weight

  real(fp_kind),allocatable :: sqrtwgt_lay(:,:,:)             ! lay,ang,atm      ! square root of regression weights
  real(fp_kind),allocatable :: sense_lay  (:,:,:)             ! lay,ang,atm      ! sensetivity

  !--- predictor and predictand

  integer      ,parameter   :: NSELECTION_POLYORDER_MAX = Npolyorder_max ! max. number of orders being searched
  integer                   :: Nselection_Polyorder   ! the actual number of orders being searched
  integer                   :: Npolyorder_sel(NSELECTION_POLYORDER_MAX)  ! order array 

  integer      ,allocatable :: Npolyorder_chan(:)  ! polyorder for each channel
  integer                   :: Npolyorder
  integer                   :: Npolyorder_arr(NSELECTION_POLYORDER_MAX)
  integer                   :: predcomb_arr(Natmpred_maxused, NSELECTION_POLYORDER_MAX) 
  integer                   :: Npredcomb_arr(NSELECTION_POLYORDER_MAX)

  integer      ,allocatable :: Npredcomb(:)                   ! chan      ! # of atmos pred specified in predcomb()
  integer      ,allocatable :: predcomb(:,:)                  ! pred,chan ! atmos pred index being used
  integer                   :: Nsample                                    ! # of samples to be used in regression
  integer                   :: dmy_predcomb(Natmpred_maxused)             ! (work) predictor index
  integer                   :: atmpredflag_canbeused(Natmpred_max)        ! (work) predictor flag

  integer                   :: Natmpred_allcombsearch_orig
  integer,allocatable       :: atmpredflag_avail(:,:)             ! atm,chan    ! atmos pred availability flags

  real(fp_kind),allocatable :: predictand_lay(:,:,:)          ! lay,ang,atm      ! predictands
  real(fp_kind),allocatable :: calabscoef_lay(:,:,:)          ! lay,ang,atm      ! predicted absorption coefficients
  real(fp_kind),allocatable :: atmpred_lay(:,:,:,:)           ! pred,lay,ang,atm ! atmospheric predictors

  !--- brightness temperature

  real(fp_kind),allocatable :: tb_lbl(:,:)                    ! ang,atm
  real(fp_kind),allocatable :: tb_cal(:,:)                    ! ang,atm

  !--- regression coefficients

  integer                   :: Ncoef
  real(fp_kind),allocatable :: coef(:,:)                      ! coef,chan ! regression coefficients

  !--- statistics

  type(StatTbTau)   :: stat
  real(fp_kind)     :: MinIdx_AllComb
  real(fp_kind)     :: MinIdx_Npred(Natmpred_max)
  real(fp_kind)     :: MinIdx_Ipred(Natmpred_max)

  real(fp_kind)     :: MinTbrmse_Ipred(Natmpred_max)
  real(fp_kind)     :: MinTbrmse_Npred(Natmpred_max)
  real(fp_kind)     :: MinTbrmse_arr(NSELECTION_POLYORDER_MAX)
  real(fp_kind)     :: MinIdx_Npred_arr(NSELECTION_POLYORDER_MAX)

  !--- other working variables

  integer           :: Ilev, Ilay, Iang, Iatm, Ichan 
  integer           :: n_AbsInAtmos

  integer           :: Iabsgas
  integer           :: Ipred_search_top
  integer           :: Ipred, Npred

  integer           :: Iflag, Iabs
  integer           :: signal_fileID
  integer           :: idmy, ii, jj, Isel, i
  real(fp_kind)     :: rdmy, rdmy1, tbdmy, r
  character(len=80) :: cdmy

  integer             :: IabsWet_AtmosProf
  integer             :: IabsOzn_AtmosProf
  integer,allocatable :: Absorber_Units_ID(:)
  integer,allocatable :: Absorber_ID(:)

  Write(algorithm_comments, '("Training parameters: varying order; max. order = ", &
                              &3(i2, 1x), "(wv, dry, ozone); TB_RMSE_MIN= ",&
                              & f6.4, 1x, "DTAU_THRESHOLD= ", f6.4, 1x, "; Surf_emi_ir=",&
                              & f6.4, 1x, "; Surf_emi_mw=", f6.4)')&
                              Npolyorder_max, Npolyorder_max, &
                              Npolyorder_max_preferred, &
                              TB_RMSE_MIN, DTAU_THRESHOLD, VirtEmiss_IR, VirtEmiss_MW

  !====================================================================
  ! Initialization
  !====================================================================

  !--- read parameters and keep them in global variables

  call Read_Parameters()

  Natmpred_allcombsearch_orig = Natmpred_allcombsearch

  !--- open an atmospheric data file and read parameters
   
  ID_Tag  = ' '                                                          
  title   = ' '                                                          
  history = ' '                                                          
  comment = ' '                                                          

  Iflag = Inquire_AtmProfile_netCDF( InFileName_AtmosProf,       &       
                                     n_Layers    = Nlay,         &       
                                     n_Absorbers = n_AbsInAtmos, &       
                                     n_Profiles  = Natm,         &       
                                     ID_Tag      = ID_Tag,       &       
                                     title       = title,        &       
                                     history     = history,      &       
                                     comment     = comment       )       

  if( Iflag /= SUCCESS )then                                             
    print *, '### ERROR IN GETTING ATMOSPHERIC PROFILE INFORMATION ###'  
    print *, 'Error flg : ', Iflag                                       
    stop 90                                                              
  endif                                                                  

  !--- cascade history information                                       

  History_all = TRIM(history)//';  '//TRIM(History_all)                  

  ProfileTag = ID_TAG                                                    
                                                                         
  print *, '# of profiles     : ', Natm                                  
  print *, '# of layers       : ', Nlay                                  
  print *, '# of absorbers    : ', n_AbsInAtmos                          
  print *, 'ID Tag            : ', trim( ID_Tag  )                       
  print *, 'Title             : ', trim( title   )                       
  print *, 'History           : ', trim( history )                       
  print *, 'Comment           : ', trim( comment )                       

  Iflag = Read_AtmProfile_netCDF( TRIM( InFileName_AtmosProf ), &
                                        AtmProfile, Reverse = 1 ) 

  IF ( Iflag /= SUCCESS ) THEN
    CALL Display_Message(  PROGRAM_NAME, &
                          'Error reading AtmProfile file '//TRIM(InFileName_AtmosProf), &
                          Iflag ) 
    STOP 
  END IF 

  Nlay = AtmProfile%n_Layers 
  Natm = AtmProfile%n_Profiles 
  n_AbsInAtmos=AtmProfile%n_Absorbers
 
  !--- absorber ID and unit                    

  allocate( Absorber_ID      (n_AbsInAtmos) )  
  allocate( Absorber_Units_ID(n_AbsInAtmos) )  
  Absorber_ID(:) = AtmProfile%Absorber_ID(:)
  Absorber_Units_ID(:) = AtmProfile%Absorber_Units_ID(:)
    !--- retrieve absorber profile seq num for wet and ozone
  print *, 'Absorber ID       : ', Absorber_ID
  print *, 'Absorber Units ID : ', Absorber_Units_ID
  print *, 'Absorber Units    : ', ( trim(ATMPROFILE_ABSORBER_UNITS_NAME(Absorber_Units_ID(Iabs))) // ' ', Iabs=1, n_AbsInAtmos )

    do Iabs = 1, n_AbsInAtmos
      if( Absorber_ID(Iabs) == AbsID_HITRAN_H2O )then
        IabsWet_AtmosProf = Iabs
        goto 1100
      endif
    enddo
    print *, '### ERROR IN FINDING WATER VAPOR IN ABSORBER ID LIST ###'
    stop 90
1100 continue

    do Iabs = 1, n_AbsInAtmos
      if( Absorber_ID(Iabs) == AbsID_HITRAN_O3 )then
        IabsOzn_AtmosProf = Iabs
        goto 1200
      endif
    enddo
    print *, '### ERROR IN FINDING OZONE IN ABSORBER ID LIST ###'
    stop 90
1200 continue

    print *, 'WV index #        : ', IabsWet_AtmosProf
    print *, 'O3 index #        : ', IabsOzn_AtmosProf


    !--- check absorber units

    if( Absorber_Units_ID(IabsWet_AtmosProf) /= UnitID_HITRAN_ppmw .and. &
        Absorber_Units_ID(IabsWet_AtmosProf) /= UnitID_HITRAN_gpkg )then
      print *, '### ERROR IN ATMOS DATA ###'
      print *, 'Unit of WV is not acceptable.'
      stop 90
    endif

    if( Absorber_Units_ID(IabsOzn_AtmosProf) /= UnitID_HITRAN_ppmw )then
      print *, '### ERROR IN ATMOS DATA ###'
      print *, 'Unit of O3 is not acceptable.'
      stop 90
    endif
    
  !--- allocation

  allocate( p_lev (0:Nlay) )
  allocate( p_lay (  Nlay) )
  allocate( t_lay (  Nlay,Natm) )
  allocate( q_lay (  Nlay,Natm) )
  allocate( o3_lay(  Nlay,Natm) )

  allocate( absamount_lev(0:Nlay,Nangle,Natm) )
  allocate( absamount_lay(  Nlay,Nangle,Natm) )
  allocate( abslev_lev   (0:Nlay,Nangle,Natm) )
  allocate( abslev_lay   (  Nlay,Nangle,Natm) )

  allocate(    trans_lev   (0:Nlay,Nangle,Natm) )
  allocate( drytrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( wettrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( ozntrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( alltrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( efctrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( caltrans_lev   (0:Nlay,Nangle,Natm) )
  allocate( calalltrans_lev(0:Nlay,Nangle,Natm) )

  allocate( predictand_lay(Nlay,Nangle,Natm) )
  allocate( calabscoef_lay(Nlay,Nangle,Natm) )
  allocate( atmpred_lay   (Natmpred_max,Nlay,Nangle,Natm) )
  allocate( sqrtwgt_lay   (Nlay,Nangle,Natm) )
  allocate( sense_lay     (Nlay,Nangle,Natm) )

  allocate( tb_lbl(Nangle,Natm) )
  allocate( tb_cal(Nangle,Natm) )

  allocate( Npredcomb(Nchan)                  )
  allocate( predcomb (Natmpred_maxused,Nchan) )
  allocate( coef(Ncoef_max,Nchan) )
  allocate( Npolyorder_chan(Nchan) )
  allocate( atmpredflag_avail(Natmpred_max,Nchan) )

  !--- set predictor flags (all predictors are available
  
  atmpredflag_avail(:,:) = 1

  !--- set unuse for some predictor according to channel type  
                                                               
  do Ichan = 1, Nchan                                          
    call Unuse_PredFlag( channel_type(Ichan),       &          
                         atmpredflag_avail(:,Ichan) )          
  enddo                                                        
  

  !--- assign atmospheric profiles
  P_lev(0:NLay)    = AtmProfile%Level_Pressure(:, 1)
  p_lay(:)         = AtmProfile%Layer_Pressure(:,1)
  t_lay(:,:)       = AtmProfile%Layer_Temperature(:,:)
  print *, P_lev(0:NLay) 
   
  if( Absorber_Units_ID(IabsWet_AtmosProf) == UnitID_HITRAN_gpkg )then                                            
    q_lay(:,:) = AtmProfile%Layer_Absorber(:,IabsWet_AtmosProf,:)                       
  else   
    do Iatm = 1, Natm
     CALL ppmv_to_mr( AtmProfile%Layer_Absorber(:,IabsWet_AtmosProf,Iatm), q_lay(:,Iatm), Molecule_ID=1 )
    enddo      
  endif                                                                                                           
  o3_lay(:,:) =  AtmProfile%Layer_Absorber(:,IabsOzn_AtmosProf,:)
  
  !--- open a transmittance data file and read parameters

  call Open_TransProf()
 

  !--- get slant path absorber amount

  call Calc_AbsAmount( p_lev, q_lay, o3_lay, &
                       absamount_lev, absamount_lay )


  !--- get coefficients for absorber amount-level conversion
  
  call Calc_CoefAmountLevel( absamount_lev )
  

  !--- get absorber amount levels

  call Conv_AbsAmountToLevel( absamount_lev, (Nlay+1), abslev_lev )
  call Conv_AbsAmountToLevel( absamount_lay,  Nlay   , abslev_lay )


  !--- get atmos predictors

  call Get_AtmosPredictor( p_lay, t_lay, q_lay, o3_lay, &
                           absamount_lev, &
                           atmpred_lay )



  !====================================================================
  ! channel loop (long loop)
  !====================================================================

  !--- initalize

  Npredcomb(:)  = -1
  predcomb(:,:) =  0


  !--- top of channel loop

  Loop_Ichan: do Ichan = Ichan_top, Ichan_last

    print *
    print *, 'Channel (Seq #, Ch #) : ', Ichan, channel_list(Ichan)


    !==================================================================
    ! Initialize for each channel
    !==================================================================

    !--- read LBL transmittances

!    if( Flag_netCDF_file )then 
      call Read_TransProf( Ichan, &
                           alltrans_lev, &
                           drytrans_lev, wettrans_lev, ozntrans_lev )
!    else
!      call Read_TransProf_Bin( Ichan, &
!                               alltrans_lev, &
!                               drytrans_lev, wettrans_lev, ozntrans_lev )
!    endif

    !--- Set surface emissivity for IR and MW channels
    
!    if(SpcCoeff%Is_Microwave_Channel(Ichan) == 1)then
    if(SpcCoeff%Sensor_Type == 1)then
      VirtEmiss = VirtEmiss_MW
    else
      VirtEmiss = VirtEmiss_IR
    endif
 
    !--- calculate Tb from LBL trans

    do Iatm = 1, Natm
    do Iang = 1, Nangle

      tb_lbl(Iang,Iatm) = Pred_BrightTemp( Ichan,                     &
                                           alltrans_lev(0,Iang,Iatm), &
                                           t_lay(1,Iatm),             &
                                           t_lay(Nlay,Iatm)           )

    enddo
    enddo

    !------------------------------------------------------------
    ! Set Polyorder array.  The order selection process will loop
    ! over each element of this array.  For example, if
    ! Npolyorder_max_preferred = 5 and Npolyorder_max = 10
    ! The following will set the array with
    !  2, 3, 4, 5, 10
    ! As a result the order loop will go over these orders
    !------------------------------------------------------------
    Nselection_Polyorder = Npolyorder_max_preferred
    Npolyorder_sel(1:Nselection_Polyorder-1) = (/(i, i=2, Npolyorder_max_preferred)/)
    Npolyorder_sel(Nselection_Polyorder) = Npolyorder_max
      !*******************************************************************  
      ! For ozone we limit the max. order to Npolyorder_max_preferred. It   
      ! is found that beyond that number, OPTRAN becomes instable,          
      ! and more importantly, there is no fitting accuracy improvement      
      ! when taking the higher order.                                       
      !******************************************************************* 
    if(Iabsorber == 3)then 
      Nselection_Polyorder = Nselection_Polyorder - 1
    endif

    !--- copy specified gas trans to a working variable and
    !--- get effective transmittances for other two gases.

    select case(Iabsorber)
      case(1)
        trans_lev(:,:,:)    = drytrans_lev(:,:,:)
        efctrans_lev(:,:,:) = min( max( wettrans_lev(:,:,:) * ozntrans_lev(:,:,:), ZERO), ONE )
      case(2)
        trans_lev(:,:,:)    = wettrans_lev(:,:,:)
        efctrans_lev(:,:,:) = min( max( drytrans_lev(:,:,:) * ozntrans_lev(:,:,:), ZERO), ONE )
      case(3)
        trans_lev(:,:,:)    = ozntrans_lev(:,:,:)
        efctrans_lev(:,:,:) = min( max( drytrans_lev(:,:,:) * wettrans_lev(:,:,:), ZERO), ONE )
    end select

    !--- get regression weight

    call Calc_RegWeight( Ichan, t_lay, alltrans_lev, trans_lev, &
                         p_lev, sense_lay, &
                         sqrtwgt_lay )


    !--- get predictands

    call Get_Predictand( trans_lev,     &
                         absamount_lev, &
                         predictand_lay )


    !--- count # of predictands available

    Nsample = count( predictand_lay(:,:,:) > HMISS )

    print *, 'Nsample=', Nsample


      !--- increment # of samples
    !--- get max predictands

    Max_Predictand = ONE

    do Iatm = 1, Natm
    do Iang = 1, Nangle
      do Ilay = 1, Nlay

        if( alltrans_lev(Ilay,Iang,Iatm) < MinTrans_SenseCheck ) exit

        if( predictand_lay(Ilay,Iang,Iatm) < HMISS ) cycle
        
        if( Max_Predictand < abs(predictand_lay(Ilay,Iang,Iatm)) )then
          Max_Predictand = abs(predictand_lay(Ilay,Iang,Iatm))
        endif

      enddo    
    enddo    
    enddo    

    print '(a,f7.2)', ' Max abs(predictand) :', real( Max_Predictand, 4 )
    print '(a,f7.2)', ' Max atm term        :', real( Max_Predictand * Criterion_AtmPredSensitivity(Iabsorber), 4 )


    !--- check if samples are enough to generate coef

    if( Nsample < Natm * Nangle * Nlay / 4 )then
      Npredcomb(Ichan) = -1
      predcomb(1:Natmpred_maxused,Ichan) = 0
      Npolyorder_chan(Ichan) = 0
      print *, '### PASS GENERATING COEFFICIENTS (NO ENOUGH PREDICTANDS AVAILABLE) ###'
      print *, '### ZERO ABSORPTION IS ASSUMED                                     ###'
      goto 1000
    endif


    !--- check where there are any predictors to be used

    if( all( atmpredflag_avail(:,Ichan) <= 0 ) )then
      Npredcomb(Ichan) = -1
      print *, '### PASS GENERATING COEFFICIENTS (ALL PREDICTORS CANNOT BE USED) ###'
      goto 1000
    endif

    Npolyorder_arr(:) = 0
    MinTbrmse_arr(:) = INFINITE
    
    Loop_polyOrder: do Isel = 1, Nselection_Polyorder
    
      Npolyorder = Npolyorder_sel(Isel)
      Ncoef = Ncoef_atmpred * (Npolyorder + 1)


      !==================================================================
      !
      ! Find the best atmos predictor combination
      !
      !  1) All combinations are attempted
      !     choosing (Natmpred_allcombsearch) predictors
      !     among available predictors specified by 'atmpredflag_avail'
      !
      !  2) Add predictors to the selected predictor set by step 1.
      !     The addition is done by attempting to add each available
      !     predictor and find the best.
      !
      !  3) Try zero absorption (all 1 trans)
      !
      !==================================================================

      !==================================================================
      !
      ! Find the best atmos predictors attempting all available
      ! predictor combinations
      !
      !==================================================================

      !--- If the best predictor set is not found, come back here

      Natmpred_allcombsearch = Natmpred_allcombsearch_orig

500   continue


      !--- initialize predictor set

      if( Natmpred_allcombsearch > 1 )then
        dmy_predcomb(1:(Natmpred_allcombsearch-1)) = (/ (Ipred,Ipred=1,(Natmpred_allcombsearch-1)) /)
      endif
      dmy_predcomb(Natmpred_allcombsearch) = Natmpred_allcombsearch - 1


      !--- reset index used in choosing the best predictor combination

      MinIdx_AllComb = INFINITE


      !--- combination loop

      Loop_allcomb: do


        !--- get the next pred comb

        Iflag = Get_PredComb( dmy_predcomb )
        if( Iflag > 0 )then
          exit Loop_allcomb
        endif

        !--- check whether all selected pred can be used

        do Ipred = 1, Natmpred_allcombsearch
          if( atmpredflag_avail(dmy_predcomb(Ipred),Ichan) <= 0 )then
            cycle Loop_allcomb
          endif
        enddo

        !--- check whether the atmos pred conbmination is sensitive

        Iflag = Check_SensitivePredSet( Natmpred_allcombsearch, dmy_predcomb )

        if( Iflag > 0 )then
          cycle Loop_allcomb
        endif


        !--- generate coefficients and get statistics

        call Gen_TransCoefSub( &
                       Ichan,  &
                       Natmpred_allcombsearch, dmy_predcomb, &
                       Nsample,                       &
                       atmpred_lay, predictand_lay,   &
                       absamount_lev, abslev_lay,     &
                       efctrans_lev, alltrans_lev,    &
                       sqrtwgt_lay,                   &
                       t_lay,                         &
                       tb_lbl,                        &
                       Npolyorder,                    &
                       Ncoef,                         &
                       coef(1:Ncoef,Ichan), stat,     &
                       calabscoef_lay,                &
                       caltrans_lev, calalltrans_lev, &
                       tb_cal,                        &
                       Iflag                          )


        !--- check whether the atmos pred conbmination is sensitive

        if( Iflag > 0 )then
          cycle Loop_allcomb
        endif


        !--- check whether it's a better predictor set

        if( stat%index < MinIdx_AllComb )then
          MinIdx_AllComb = stat%index
          predcomb(1:Natmpred_allcombsearch,Ichan) = dmy_predcomb(1:Natmpred_allcombsearch)
        endif


      enddo Loop_allcomb


      !--- find the best set?

      if( MinIdx_AllComb >= INFINITE / TWO )then
        Natmpred_allcombsearch = Natmpred_allcombsearch - 1
        if( Natmpred_allcombsearch < 1) then
          exit Loop_polyOrder
        else
         print *, 'Natmpred_allcombsearch:', Natmpred_allcombsearch
         goto 500
        endif 
      endif


      !==================================================================
      !
      ! Stepwise selection based on the set of predictors determined from
      ! the previous step (all combination serach). When this step is done
      ! the first in the selected predictors is the most important, the
      ! next is the second important one, and so on.
      !
      !==================================================================

      !--- initialize

      MinIdx_Npred(:)                      = INFINITE

      Ipred_search_top = 1

      atmpredflag_canbeused(:) = 0
      atmpredflag_canbeused(predcomb(1:Natmpred_maxused,Ichan)) = 1 

      !--- loop for increasing predictors

      Loop_Npred: do Npred = Ipred_search_top, Natmpred_maxused


        !--- initialize for searching for a new predictor

        MinIdx_Ipred(:) = INFINITE

        atmpredflag_canbeused( predcomb(1:(Npred-1),Ichan) ) = 0


        !--- loop to find a best predicor among the rests

        Loop_Ipred: do Ipred = 1, Natmpred_max         ! ### limit to OPTRAN originalredictors


          !--- chech whether a predictor can be used

          if( atmpredflag_canbeused(Ipred) <= 0 )then
            cycle Loop_Ipred
          endif

          predcomb(Npred,Ichan) = Ipred


          !--- check whether the atmos pred conbmination is sensitive

          Iflag = Check_SensitivePredSet( Npred, predcomb(1,Ichan) )

          if( Iflag > 0 )then
            cycle Loop_Ipred
          endif


          !--- get coefficients and statistics

          call Gen_TransCoefSub( &
                       Ichan,  &
                       Npred, predcomb(1,Ichan),      &
                       Nsample,                       &
                       atmpred_lay, predictand_lay,   &
                       absamount_lev, abslev_lay,     &
                       efctrans_lev, alltrans_lev,    &
                       sqrtwgt_lay,                   &
                       t_lay,                         &
                       tb_lbl,                        &
                       Npolyorder,                    &
                       Ncoef,                         &
                       coef(1:Ncoef,Ichan), stat,     &
                       calabscoef_lay,                &
                       caltrans_lev, calalltrans_lev, &
                       tb_cal,                        &
                       Iflag                          )

          !--- check whether the atmos pred conbmination is sensitive

          if( Iflag > 0 )then
            cycle Loop_Ipred
          endif


          !--- keep predictor selecting index

          MinIdx_Ipred(Ipred) = stat%index
          
          MinTbrmse_Ipred(Ipred) = stat%tbrmse

        enddo Loop_Ipred

        !--- select the best predictor to be added in a predictor set

        rdmy = INFINITE

        do Ipred = 1, Natmpred_max
          if( MinIdx_Ipred(Ipred) < rdmy )then
            idmy = Ipred
            rdmy = MinIdx_Ipred(Ipred)
            tbdmy = MinTbrmse_Ipred(Ipred)
          endif
        enddo

        if( rdmy > INFINITE / TWO )then
          exit Loop_Npred
        endif

        predcomb(Npred,Ichan) = idmy
        MinIdx_Npred(Npred)   = rdmy
        
        MinTbrmse_Npred(Npred)   = tbdmy
        
        Npolyorder_chan(Ichan) = Npolyorder


      enddo Loop_Npred


      !--- check whether predictors less than Natmpred_maxused is better

      idmy = Natmpred_maxused

      do Ipred = Natmpred_maxused-1, 1, -1
        if( MinIdx_Npred(Ipred) < MinIdx_Npred(idmy) )then
          idmy = Ipred
        endif
      enddo

      Npredcomb(Ichan) = idmy

      if( idmy < Natmpred_maxused )then
        predcomb(idmy+1:Natmpred_maxused,Ichan) = 0
      endif

      !--- Exit if fitting error < TB_RMSE_MIN, otherwise loop over
      !--- polyorder selections      

      if(MinTbrmse_Npred(idmy) < TB_RMSE_MIN)then

        exit Loop_polyOrder
        
      else
      
        predcomb_arr(:,Isel) = predcomb(:,Ichan)     
        Npolyorder_arr(Isel) = Npolyorder            
        Npredcomb_arr(Isel) = Npredcomb(Ichan)       
        MinTbrmse_arr(Isel) = MinTbrmse_Npred(idmy)  
        MinIdx_Npred_arr(Isel) = MinIdx_Npred(idmy)  

        !--- When Isel == Nselection_Polyorder, the polyorder
        !--- loop is over.  But before ending polyorder selection,
        !--- compare fitting errors among different polyorders and
        !--- select the lowest polyorder if its fitting error is
        !--- not significantly larger than others.
        
        if(Isel == Nselection_Polyorder)then

          ii = Nselection_Polyorder       
          do jj = Nselection_Polyorder-1, 1, -1
          
            if(MinIdx_Npred_arr(jj) > TOLERANCE)then
            
              r = (MinIdx_Npred_arr(jj) - MinIdx_Npred_arr(ii) ) / &
                   MinIdx_Npred_arr(jj)
              if( r < DTAU_THRESHOLD )then            
                ii = jj
              endif  
              
            else
            
              ii = jj
              
            endif 

          enddo

          !***********************************************************
          ! If possible, avoid using polyorder > Npolyorder_max_preferred
          !***********************************************************
          
!!          if(Npolyorder_arr(ii) > Npolyorder_max_preferred .and. ii > 1)then
!!          
!!            if(MinTbrmse_arr(ii-1) - MinTbrmse_arr(ii) < DELTA_TB_RMSE)then 
            
!!              ii = ii - 1
              
!!            endif

!!          endif
          
          Npolyorder_chan(Ichan) = Npolyorder_arr(ii)
          predcomb(:,Ichan) = predcomb_arr(:,ii)
          Npredcomb(Ichan) = Npredcomb_arr(ii)
          MinIdx_Npred(Npredcomb_arr(ii)) = MinIdx_Npred_arr(ii)
                    
          exit Loop_polyOrder
           
        endif

      endif
      
    enddo Loop_polyOrder
    
    Ncoef = Ncoef_atmpred * (Npolyorder_chan(Ichan) + 1) 
    
    !==================================================================
    !
    ! Attempt whether polynomial approximation with no atmospheric
    ! predictor is better.
    !
    !==================================================================

      !--- try to predict trans & Tb without atmos pred

      call Gen_TransCoefSub( &
                     Ichan,  &
                     0, predcomb(1,Ichan),          &
                     Nsample,                       &
                     atmpred_lay, predictand_lay,   &
                     absamount_lev, abslev_lay,     &
                     efctrans_lev, alltrans_lev,    &
                     sqrtwgt_lay,                   &
                     t_lay,                         &
                     tb_lbl,                        &
                     Npolyorder_chan(Ichan),         &
                     Ncoef,                         &
                     coef(1:Ncoef,Ichan), stat,     &
                     calabscoef_lay,                &
                     caltrans_lev, calalltrans_lev, &
                     tb_cal,                        &
                     Iflag                          )


      !--- check if better

      if( Iflag == 0 .and. stat%index < MinIdx_Npred(Npredcomb(Ichan)) )then
        print *, '### PREDICTING WITH NO ATMOSPHERIC PREDICTOR IS BETTER ###'
        Npredcomb(Ichan)                   = 0
        predcomb(1:Natmpred_maxused,Ichan) = 0
        MinIdx_Npred(Npredcomb(Ichan))     = stat%index
      endif


    !==================================================================
    !
    ! Check whether zero absorption is better
    !
    !==================================================================

      !--- try to predict trans & Tb with zero absorption

      call Gen_TransCoefSub( &
                     Ichan,  &
                     -1, predcomb(1,Ichan),         &
                     Nsample,                       &
                     atmpred_lay, predictand_lay,   &
                     absamount_lev, abslev_lay,     &
                     efctrans_lev, alltrans_lev,    &
                     sqrtwgt_lay,                   &
                     t_lay,                         &
                     tb_lbl,                        &
                     Npolyorder_chan(Ichan),         &
                     Ncoef,                         &
                     coef(1:Ncoef,Ichan), stat,     &
                     calabscoef_lay,                &
                     caltrans_lev, calalltrans_lev, &
                     tb_cal,                        &
                     Iflag                          )


      !--- check if better

      if( Iflag == 0 .and. stat%index < MinIdx_Npred(Npredcomb(Ichan)) )then
        print *, '### PREDICTING NO ABSORPTION IS BETTER ###'
        Npredcomb(Ichan)                   = -1
        predcomb(1:Natmpred_maxused,Ichan) = 0
        Npolyorder_chan(Ichan) = 0
      endif


    !==================================================================
    !
    ! Post process for channel loop
    !
    !==================================================================

1000 continue

    !--- regenerate regression coefficients

    print *, '---> The best predictor set for channel', channel_list(Ichan), 'is'

    call Gen_TransCoefSub( &
                     Ichan,  &
                     Npredcomb(Ichan), predcomb(1,Ichan),      &
                     Nsample,                       &
                     atmpred_lay, predictand_lay,   &
                     absamount_lev, abslev_lay,     &
                     efctrans_lev, alltrans_lev,    &
                     sqrtwgt_lay,                   &
                     t_lay,                         &
                     tb_lbl,                        &
                     Npolyorder_chan(Ichan),        &
                     Ncoef,                         &
                     coef(1:Ncoef,Ichan), stat,     &                     
                     calabscoef_lay,                &
                     caltrans_lev, calalltrans_lev, &
                     tb_cal,                        &
                     Iflag                          )

    !--- check whether the atmos pred conbmination is sensitive

    if( Iflag > 0 )then
      print *, '### ERROR ###'
      print *, 'SENSITIVE ATMOSPHERIC PREDICTOR SET IS SELECTED'
      stop 90
    endif

    !--- zero out non-usable elements
    
    Ncoef = (Npredcomb(Ichan) + 1)*(Npolyorder_chan(Ichan) + 1)
    coef(Ncoef+1:Ncoef_max,Ichan) = ZERO
     
    !--- write stat

    write( cdmy, '("(i4,2h :,2i3,1h),",i1,"i3,F8.4,3h : ,4F8.5,3h : ,4F9.6,3h : ,3F9.6,3h : ,F7.3)")' ) Natmpred_maxused
    write(OutStat,cdmy) &
                channel_list(Ichan), Npolyorder_chan(Ichan), &
                Npredcomb(Ichan), predcomb(1:Natmpred_maxused,Ichan), &
                stat%residual, &
                stat%tbmean,     stat%tbsderr,     stat%tbrmse,     stat%tbmaxerr, &
                stat%taumean,    stat%tausderr,    stat%taurmse,    stat%taumaxerr, &
                stat%taumaxmean, stat%taumaxsderr, stat%taumaxrmse, &
                stat%maxpredterm


  enddo Loop_Ichan

  !====================================================================
  ! End of channel loop (long loop)
  !====================================================================


  !====================================================================
  ! Output the results
  !====================================================================

  !--- adjust Npredcomb = -1 to 0

  Npredcomb(:) = max( Npredcomb(:), 0 )


  !--- output transmittance table for GDAS
  Comment_all = TRIM(algorithm_comments)//';  '//Comment_all

  call Write_TransTable_netCDF( Npredcomb, predcomb, Npolyorder_chan, coef, &
                                PROGRAM_RCS_ID, TAUCOEFF_VERSION )

  !--- Normal end

    !--- create a signal file indicating the completion 
   
  signal_fileID=get_lun()
   
  OPEN(UNIT=signal_fileID, FILE=OutFileName_CompleteSignal, &
       STATUS='REPLACE')
  write(signal_fileID, *) 'Normal End'
  
  CLOSE(signal_fileID)
  
  print *
  print *, '*** NORMAL END ***'




 contains

  !====================================================================
  ! Generate a predictor combination to be verified next
  !====================================================================

  function Get_PredComb( predcomb ) result( Iflag )

    !--- interface  

    integer,intent(inout) :: predcomb(Natmpred_allcombsearch)
    integer               :: Iflag        ! 0) find a comb, 1) no more comb

    !--- local variables

    integer               :: i, j

    !--- find the next combination

    do i = Natmpred_allcombsearch, 1, -1

      predcomb(i) = predcomb(i) + 1

      if( predcomb(i) <= Natmpred_max - (Natmpred_allcombsearch - i) )then

        if( i < Natmpred_allcombsearch )then
          do j = i+1, Natmpred_allcombsearch
            predcomb(j) = predcomb(j-1) + 1
          enddo
        endif

        Iflag = 0
        return

      endif

    enddo

    !--- no more combination

    Iflag = 1

  end function Get_PredComb



  !====================================================================
  ! Generate transmittance coefficient,
  ! Predict transmittances and brightness temperatures and
  ! Calculate statistics
  !====================================================================

  subroutine Gen_TransCoefSub( &
                Ichan,                         &
                Natmpred, predcomb,            &
                Nsample,                       &
                atmpred_lay, predictand_lay,   &
                absamount_lev, abslev_lay,     &
                efctrans_lev, alltrans_lev,    &
                sqrtwgt_lay,                   &
                t_lay,                         &
                tb_lbl,                        &
                Npolyorder,                    &
                Ncoef,                         &
                coef, stat,                    &
                calabscoef_lay,                &
                caltrans_lev, calalltrans_lev, &
                tb_cal,                        &
                Isensitive                     )

    !--- interface

    integer        ,intent(in)  :: Ichan                                        ! channel seq #
    integer        ,intent(in)  :: Natmpred                                     ! # of predictors used in prediction
    integer        ,intent(in)  :: predcomb   (Natmpred_maxused)                ! atmos predictor indices
    integer        ,intent(in)  :: Nsample                                      ! # of regression samples
    real(fp_kind)  ,intent(in)  :: atmpred_lay(Natmpred_max,Nlay,Nangle,Natm)   ! atm predictor set
    real(fp_kind)  ,intent(in)  :: predictand_lay (Nlay,Nangle,Natm)            ! predictands
    real(fp_kind)  ,intent(in)  :: absamount_lev(0:Nlay,Nangle,Natm)            ! absorber amount on levels
    real(fp_kind)  ,intent(in)  :: abslev_lay   (  Nlay,Nangle,Natm)            ! absorber amount level on layers
    real(fp_kind)  ,intent(in)  :: efctrans_lev (0:Nlay,Nangle,Natm)            ! LBL effective trans of other two gases 
    real(fp_kind)  ,intent(in)  :: alltrans_lev (0:Nlay,Nangle,Natm)            ! LBL total trans
    real(fp_kind)  ,intent(in)  :: sqrtwgt_lay    (Nlay,Nangle,Natm)            ! regression weight
    real(fp_kind)  ,intent(in)  :: t_lay (Nlay,Natm)                            ! temperature
    real(fp_kind)  ,intent(in)  :: tb_lbl(Nangle,Natm)                          ! LBL Tb
    integer        ,intent(in)  :: Npolyorder
    integer        ,intent(in)  :: Ncoef
    
    real(fp_kind)  ,intent(out) :: coef(Ncoef)                          ! regression coefficients
    type(StatTbTau),intent(out) :: stat                                 ! predicted trans and Tb statistics against LBL set
    real(fp_kind)  ,intent(out) :: calabscoef_lay (  Nlay,Nangle,Natm)  ! predicted absorption coef
    real(fp_kind)  ,intent(out) :: caltrans_lev   (0:Nlay,Nangle,Natm)  ! predicted specified gas trans
    real(fp_kind)  ,intent(out) :: calalltrans_lev(0:Nlay,Nangle,Natm)  ! predicted total trans
    real(fp_kind)  ,intent(out) :: tb_cal(Nangle,Natm)                  ! predicted Tb
    integer        ,intent(out) :: Isensitive                           ! 1)sensitive atmos pred set, 0)not sensitive

    !--- local variables

    real(fp_kind)     :: max_atmpred_term
    character(len=80) :: cformat
    integer           :: dmy_predcomb(Natmpred_maxused)


    !--- generate transmittance coefficients

    call Calc_RegCoef( Natmpred,       &
                       predcomb,       &
                       Nsample,        &
                       atmpred_lay,    &
                       predictand_lay, &
                       abslev_lay,     &
                       sqrtwgt_lay,    &
                       Npolyorder,     &
                       Ncoef,          &
                       coef,           &
                       stat%residual   )


    !--- calculate trans and Tb

    call Pred_AbsCoefTransTemp(     &
                   Ichan,           &
                   Natmpred,        &
                   predcomb,        &
                   atmpred_lay,     &
                   absamount_lev,   &
                   abslev_lay,      &
                   alltrans_lev,    &
                   efctrans_lev,    &
                   t_lay,           &
                   tb_lbl,          &
                   Npolyorder,      &
                   Ncoef,           &                   
                   coef,            &
                   calabscoef_lay,  &
                   caltrans_lev,    &
                   calalltrans_lev, &
                   tb_cal,          &
                   max_atmpred_term )

    !--- sisitivity check
    
    stat%maxpredterm = max_atmpred_term / Max_Predictand
    
    if( stat%maxpredterm > Criterion_AtmPredSensitivity(Iabsorber) )then
      Isensitive = 1
    else
      Isensitive = 0
    endif
    

    !--- calculate statistics

    call Calc_StatTransTemp( &
                        alltrans_lev,    tb_lbl, &
                        calalltrans_lev, tb_cal, &
                        stat                     )


    !--- print the results

    dmy_predcomb(:) = 0
    if( Natmpred > 0 )then
      dmy_predcomb(1:Natmpred) = predcomb(1:Natmpred)
    endif

    write( cformat, '("(2i2,1h),",i1,"i3,F7.3,1X,4F9.5,1X,4F10.7,1X,3F10.7,1X,F10.7,1X,F7.2,I2)")' ) Natmpred_maxused
!   write( cformat, '("(i2,1h),",i1,"i3,F7.3,X,4F9.5,X,4F10.7,X,3F10.7,X,F10.7,X,F15.2,I2)")' ) Natmpred_maxused
    print cformat, Npolyorder, Natmpred, dmy_predcomb(1:Natmpred_maxused), &
                  stat%residual, &
                  stat%tbmean,     stat%tbsderr,     stat%tbrmse,     stat%tbmaxerr, &
                  stat%taumean,    stat%tausderr,    stat%taurmse,    stat%taumaxerr, &
                  stat%taumaxmean, stat%taumaxsderr, stat%taumaxrmse, &
                  stat%index,      stat%maxpredterm, Isensitive

  end subroutine Gen_TransCoefSub


end program GenTransCoef

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
! $date: 2006/06/11, Yong Han
!
! Add a version parameter and pass it to Write_TransTable_netCDF to over write
! the TauCoeff version number
