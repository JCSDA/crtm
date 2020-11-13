
MODULE NESDIS_AMSU_SICEEM_Module


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
  PUBLIC :: NESDIS_ICEEM_AMSU


  ! -----------------
  ! Module parameters
  ! -----------------


CONTAINS



!-------------------------------------------------------------------------------------------------------------
!
! NAME:
!       NESDIS_ICEEM_AMSU
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over snow/sea ice conditions from AMSU measurements at
!       window channels.
!
! REFERENCES:
!       Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, 8th Specialist Meeting on
!       Microwave Radiometry and Remote Sension Applications,24-27 February, 2004, Rome, Italy.
!
! CATEGORY:
!       CRTM : Surface : MW ICE EM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL ICEEM_AMSU
!
! INPUT ARGUMENTS:
!
!         Frequency                Frequency User defines
!                                  This is the "I" dimension
!                                  UNITS:      GHz
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
!         Satellite_Angle          The local zenith angle in degree for AMSU measurements.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         User_Angle               The local angle value in degree user defines.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Rank-1, (I)
!
!         Tba                      BRIGHTNESS TEMPERATURES AT FOUR AMSU-A WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   4*1 SCALAR
!
!                        WHICH ARE
!                                  tba[1] = TB at 23.8 GHz
!                                  tba[2] = TB at: 31.4 GHz
!                                  tba[3] = TB at 50.3 GHz
!                                  tba[4] = TB at 89 GHz
!
!         Tbb                      BRIGHTNESS TEMPERATURES AT TWO AMSU-B WINDOW CHANNELS
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION   2*1 SCALAR
!
!                         WHICH ARE
!
!                                  tbb[1] = TB at 89 GHz
!                                  tbb[2] = TB at 150 GHz
!
!
!         Ts = Sea Ice Temperature:        The sea ice surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! OUTPUT ARGUMENTS:
!
!         Emissivity_H:            The surface emissivity at a horizontal polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!         Emissivity_V:            The surface emissivity at a vertical polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp )
!                                  DIMENSION:  Scalar
!
!
! INTERNAL ARGUMENTS:
!
!      User_theta                User_Angle in radian
!
!      Satellite_theta           Satellite_Angle in radian
!
! CALLS:
!
!     AMSU_IATs           : Subroutine to calculate sea ice emissivity from AMSU-A and Ts
!
!     AMSU_IBTs           : Subroutine to calculate sea ice emissivity from AMSU-B and Ts
!
!
!
! PROGRAM HISTORY LOG:
!   2004-01-01  yan,b   - implement the algorithm for the ice emissivity
!   2004-03-01  yan,b   - modify the code for SSI
!   2005-05-27  yan,b - modify the code for CRTM

!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (28-May-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!
!------------------------------------------------------------------------------------------------------------

subroutine  NESDIS_ICEEM_AMSU(Satellite_Angle,                                               & ! INPUT
                              User_Angle,                                                    & ! INPUT
                              frequency,                                                     & ! INPUT
                              Ts,                                                            & ! INPUT
                              tba,                                                           & ! INPUT
                              tbb,                                                           & ! INPUT
                              Emissivity_H,                                                  & ! OUTPUT
                              Emissivity_V)                                                    ! OUTPUT


  integer,PARAMETER ::  AMSU_IATs_ALG    = 1
  integer,PARAMETER ::  AMSU_IBTs_ALG     = 2
  integer, parameter:: nwcha = 4, nwchb = 2, nwch = 5,nalg = 7
  integer :: input_type,i
  real(fp)    :: Satellite_Angle,User_Angle,Satellite_theta,frequency,Ts
  real(fp)    :: em_vector(2),esh1,esv1,esh2,esv2,desh,desv,dem
  real(fp)    :: tba(nwcha),tbb(nwchb)
  real(fp), intent(out) :: Emissivity_H,Emissivity_V



  em_vector(1) = 0.82_fp

  em_vector(2) = 0.85_fp

  Satellite_theta = User_Angle*pi/180.0_fp

  input_type = 1



  do i=1,nwcha
     if((tba(i) <= 100.0_fp) .or. (tba(i) >= 320.0_fp) ) then
        input_type = 2
        exit
     end if
  end do


  if (input_type .eq. 2) then
     do i=1,nwchb
        if((tbb(i) <= 100.0_fp) .or. (tbb(i) >= 320.0_fp) ) then
           input_type = 3
           exit
        end if
     end do
  endif


  if ((Ts <= 150.0_fp) .or. (Ts >= 280.0_fp) ) Ts = 260.0



  GET_option: SELECT CASE (input_type)

  CASE (AMSU_IATs_ALG)

     call AMSU_IATs(frequency,tba,Ts,em_vector)

  CASE (AMSU_IBTs_ALG)

     call AMSU_IBTs(Satellite_theta,frequency,tbb,Ts,em_vector)

  END SELECT GET_option



  call NESDIS_LandEM(Satellite_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh1,esv1)

  call NESDIS_LandEM(User_Angle,Frequency,0.0_fp,0.0_fp,Ts,Ts,0.0_fp,9,13,2.0_fp,esh2,esv2)

  desh = esh1 - esh2

  desv = esv1 - esv2

  dem = ( desh + desv ) * 0.5_fp


  Emissivity_H = em_vector(1) - dem;  Emissivity_V = em_vector(2)- dem

  if (Emissivity_H > one)         Emissivity_H = one

  if (Emissivity_V > one)         Emissivity_V = one

  if (Emissivity_H < 0.3_fp) Emissivity_H = 0.3_fp

  if (Emissivity_V < 0.3_fp) Emissivity_V = 0.3_fp


end subroutine NESDIS_ICEEM_AMSU


subroutine AMSU_IATs(frequency,tba,ts,em_vector)

  integer,parameter:: nch =10,nwch = 5,ncoe = 4
  real(fp)    :: tba(*)
  real(fp)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer     :: ich
  real(fp)    :: coe(100)

  save coe

  coe(1:5) = (/ 9.815214e-001_fp,  3.783815e-003_fp,  &
       6.391155e-004_fp, -9.106375e-005_fp, -4.263206e-003_fp/)
  coe(21:25) = (/ 9.047181e-001_fp, -2.782826e-004_fp,  &
       4.664207e-003_fp, -3.121744e-005_fp, -3.976189e-003_fp/)
  coe(41:45) = (/ 1.163853e+000_fp, -1.419205e-003_fp,  &
       5.505238e-003_fp,  1.506867e-003_fp, -6.157735e-003_fp/)
  coe(61:65) = (/  1.020753e+000_fp, -8.666064e-004_fp,  &
       9.624331e-004_fp,  4.878773e-003_fp, -5.055044e-003_fp/)
  coe(81:85) = (/ 1.438246e+000_fp,  5.667756e-004_fp, &
       -2.621972e-003_fp,  5.928146e-003_fp, -5.856687e-003_fp/)



  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*20)
     discriminator(ich) = discriminator(ich) + coe((ich-1)*20 + 2)*tba(1)  &
          + coe((ich-1)*20 + 3)*tba(2)  &
          + coe((ich-1)*20 + 4)*tba(4)  &
          + coe( (ich-1)*20 + 5 )*ts
  end do

  call siem_interpolate(frequency,discriminator,emissivity)

  em_vector(1) = emissivity
  em_vector(2) = emissivity


end subroutine AMSU_IATs



subroutine AMSU_IBTs(theta,frequency,tbb,ts,em_vector)

  integer,parameter:: nch =10,nwch = 5,ncoe = 6
  real(fp)    :: tbb(*),theta
  real(fp)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer     :: i,ich,nvalid_ch
  real(fp)    :: coe(nch*(ncoe+1))

  save coe

  coe(1:7) = (/ 2.239429e+000_fp, -2.153967e-002_fp,  &
       5.785736e-005_fp,  1.366728e-002_fp,    &
       -3.749251e-005_fp, -5.128486e-002_fp, -2.184161e-003_fp/)
  coe(11:17) = (/ 1.768085e+000_fp, -1.643430e-002_fp,  &
       4.850989e-005_fp,  1.288753e-002_fp,   &
       -3.628051e-005_fp, -4.751277e-002_fp, -2.580649e-003_fp/)
  coe(21:27) = (/ 8.910227e-001_fp,  6.170706e-003_fp, &
       -3.772921e-006_fp, -4.146567e-004_fp,   &
       -2.208121e-006_fp, -3.163193e-002_fp, -3.863217e-003_fp/)

  do ich = 1, nwch-2
     discriminator(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tbb(i) + &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
     discriminator(ich) = discriminator(ich) +           &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 )*cos(theta)  +   &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 + 1 )*ts
  end do
  discriminator(4) = 9.278287e-001_fp +  5.549908e-003_fp*tbb(1) &
       - 5.728596e-004_fp*tbb(2) -  4.701641e-003_fp*ts
  discriminator(5) = 1.520531e+000_fp + 1.119648e-003_fp*tbb(1) &
       +  4.518667e-003_fp*tbb(2) - 7.744607e-003_fp*ts

  call siem_interpolate(frequency,discriminator,emissivity)

  em_vector(1) = emissivity
  em_vector(2) = emissivity

end subroutine AMSU_IBTs


subroutine  siem_interpolate(frequency,discriminator,emissivity)

  integer,parameter:: ncand = 16,nch =5
  integer:: i
  real(fp)   :: frequency,freq(nch),emissivity,discriminator(*)
  freq = (/23.8_fp, 31.4_fp, 50.3_fp,89.0_fp, 150.0_fp/)



  do i = 2, nch
     if(frequency < freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency < freq(i)) then
        emissivity = discriminator(i-1) + (discriminator(i)-discriminator(i-1))* &
             (frequency - freq(i-1))/(freq(i) - freq(i-1))
        exit
     end if

  end do

  if(frequency < freq(1))    emissivity = discriminator(1)


  if (frequency >= freq(nch)) emissivity = discriminator(nch)

end subroutine siem_interpolate

END MODULE NESDIS_AMSU_SICEEM_Module
