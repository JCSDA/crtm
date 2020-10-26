
MODULE NESDIS_SSMI_SnowEM_Module


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE NESDIS_LandEM_Module
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC  :: NESDIS_SSMI_SnowEM


  ! -----------------
  ! Module parameters
  ! -----------------


CONTAINS





subroutine NESDIS_SSMI_SnowEM(frequency,                                          & ! INPUT
                               Angle,                                             & ! INPUT
                               Ts,                                                & ! INPUT
                               tb,                                                & ! INPUT
                               Depth,                                             & ! INPUT
                               Emissivity_H,                                      & ! OUTPUT
                               Emissivity_V)                                        ! OUTPUT

  integer, parameter:: nw=7,nwv=4,nwh=3

  real(fp), parameter :: SSMI_Angle= 53.0_fp

  REAL(fp), PARAMETER ::  ev_default = 0.9_fp

  REAL(fp), PARAMETER ::  eh_default = 0.88_fp

  real(fp)     :: Depth,Angle,frequency,Ts,tb(nw),tv(nwv),th(nwh)

  real(fp)     :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem

  real(fp), intent(out) :: Emissivity_H, Emissivity_V


  Emissivity_H  =  eh_default  ;  Emissivity_V  =  ev_default

  tv(1) = tb(1);  tv(2) = tb(3);  tv(3) = tb(4);  tv(4) = tb(6)

  th(1) = tb(2); th(2) = tb(5);  th(3) = tb(7)


  call SSMI_SnowEM_CORE(frequency,Ts,tv,th,em_vector)


  if (Depth .lt. one_tenth) Depth = one_tenth

  if (Depth .gt. 10.0_fp) Depth = 10.0_fp

  call NESDIS_LandEM(SSMI_Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,Depth,esh1,esv1)

  call NESDIS_LandEM(Angle,frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,Depth,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp


  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem


  if(Emissivity_H.gt.one) Emissivity_H = one

  if(Emissivity_H.lt.0.3_fp) Emissivity_H = 0.3_fp

  if(Emissivity_V.gt.one) Emissivity_V = one

  if(Emissivity_V.lt.0.3_fp) Emissivity_V = 0.3_fp

  if(Emissivity_V.lt.Emissivity_H) Emissivity_V = Emissivity_H


  return

end subroutine NESDIS_SSMI_SnowEM



subroutine SSMI_SnowEM_CORE(frequency,Ts,tv,th,em_vector)


  integer,parameter :: ntype = 3, nv=4, nh=3,ncoev=5,ncoeh=4

  integer :: ich,lp,nch

  real(fp), parameter, dimension(nv) ::   &

                freq_v=(/19.35_fp, 22.235_fp, 37.0_fp, 85.0_fp/)

  real(fp), parameter, dimension(nh) ::   &

                freq_h=(/19.35_fp, 37.0_fp, 85.0_fp/)

  real(fp) frequency,Ts,tv(*),th(*),em_vector(*)

  real(fp) ev(nv),eh(nh),ev_22

  real(fp) coe_v(nv,ncoev),coe_h(nh,ncoeh),pe , ev_cor,eh_cor

  logical data_invalid

  save  coe_v,coe_h

  coe_v(1,1:5) = (/  1.109066e-001_fp,  5.449409e-003_fp,  &
       1.835799e-004_fp, -1.765248e-003_fp, -2.996101e-004_fp/)
  coe_v(2,1:5) = (/ 9.356505e-002_fp,  1.320617e-003_fp,  &
       4.449195e-003_fp, -1.786960e-003_fp, -3.479687e-004_fp/)
  coe_v(3,1:5) = (/ 6.387097e-002_fp,  1.252447e-003_fp,  &
       1.998846e-004_fp, 2.680219e-003_fp, -3.740141e-004_fp/)
  coe_v(4,1:5) = (/ 4.150689e-002_fp,  1.420274e-003_fp,  &
       1.223339e-004_fp, -1.948946e-003_fp,  4.248289e-003_fp/)

  coe_h(1,1:4) = (/ &
       8.503807e-002_fp,  5.357374e-003_fp, -1.361660e-003_fp, &
       -3.319696e-004_fp/)
  coe_h(2,1:4) = (/ &
       4.200333e-002_fp,  1.278894e-003_fp,  2.963129e-003_fp, &
       -4.087036e-004_fp/)
  coe_h(3,1:4) = (/ &
       2.082461e-002_fp,  1.438480e-003_fp, -1.723992e-003_fp,  &
       4.194914e-003_fp/)




  em_vector(1) = 0.7_fp

  em_vector(2) = 0.8_fp


  data_invalid = .False.

  if ( (Ts <= 140.0_fp) .or. (Ts >= 330.0_fp) ) data_invalid = .True.

  do ich = 1, nv
     if ( (tv(ich) .le. 50.0_fp) .or. (tv(ich) .ge. 330.0_fp) )  then
        data_invalid = .True.
        exit
     end if
  end do

  do ich = 1, nh
     if ( (th(ich) <= 50.0_fp) .or. (th(ich) >= 330.0_fp) )  then
        data_invalid = .True.
        exit
     end if
  end do

  if (data_invalid) RETURN



  do ich=1,nv
     ev(ich) =  coe_v(ich,1) + coe_v(ich,2)*tv(1)  &
          + coe_v(ich,3)*tv(2) + coe_v(ich,4)*tv(3)  &
          + coe_v(ich,5)*tv(4)
  end do

  do ich=1,nh
     eh(ich) =  coe_h(ich,1)
     do lp =2,4
        eh(ich) =  eh(ich) + coe_h(ich,lp)*th(lp-1)
     end do
  end do


  pe=     -0.002_fp + 4.470142e-003_fp*(tv(1) - th(1)) -  &
          1.991876e-004_fp*(tv(3) - th(2)) -  &
          1.704354e-005_fp*(tv(4) - th(3))

  ev_cor = one - pe*(Ts-tv(1))/(tv(1)-th(1))

  if (ev_cor >  one)         ev_cor = one
  if (ev_cor <= 0.2_fp) ev_cor = 0.2_fp
  eh_cor = ev_cor - pe
  ev_cor = ev(1) - ev_cor
  eh_cor = eh(1) - eh_cor

  do ich=1, nv
     ev(ich) = ev(ich) - ev_cor
     if(ich <= 3) eh(ich) = eh(ich) - eh_cor
  end do


  ev_22 = ev(1) + (ev(3)-ev(1))*(22.235_fp-19.35_fp)/(37.0_fp-19.35_fp)
  if( (ev(2) .gt. ev(1)) .and. (ev(2) .gt. ev(3)) ) ev(2) = ev_22
  if( (ev(2) .lt. ev(1)) .and. (ev(2) .lt. ev(3)) ) ev(2) = ev_22






  nch = 4
  do ich=1,nv
     if(frequency <= freq_v(ich)) then
        nch = ich
        exit
     end if
  end do


  if (nch == 1) then
     em_vector(2) = ev(1)
  else
     if (frequency .ge. freq_v(nv)) then
        em_vector(2) = ev(4)
     else
        em_vector(2) = ev(nch-1) + (ev(nch) - ev(nch-1))* &
             (frequency - freq_v(nch-1))/(freq_v(nch) - freq_v(nch-1))
     end if
  end if


  nch = 3
  do ich=1,nh
     if(frequency <= freq_h(ich)) then
        nch = ich
        exit
     end if
  end do
  if (nch == 1) then
     em_vector(1) = eh(1)
  else
     if (frequency .ge. freq_h(nh)) then
        em_vector(1) = eh(3)
     else
        em_vector(1) = eh(nch-1) + (eh(nch) - eh(nch-1))* &
             (frequency - freq_h(nch-1))/(freq_h(nch) - freq_h(nch-1))
     end if

  end if

end subroutine SSMI_SnowEM_CORE


END MODULE NESDIS_SSMI_SnowEM_Module
