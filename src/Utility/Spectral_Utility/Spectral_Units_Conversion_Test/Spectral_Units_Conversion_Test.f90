!
! Atmospheric_Properties_Test
!
! Program to test the Atmospheric_Properties routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Spectral_Units_Conversion_Test

  ! ----------------
  ! Enviroment setup
  ! ----------------
  ! Module usage
  USE Spectral_Utility, fp=>fp_kind
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Spectral_Units_Conversion_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Spectral_Units_Conversion_Test.f90,v 1.1 2006/05/23 15:31:05 wd20pd Exp $'
  ! Microwave data
  INTEGER,  PARAMETER :: N_MW_CHANNELS = 12
  REAL(fp), PARAMETER, DIMENSION( N_MW_CHANNELS ) :: MW_FREQUENCIES = &
    (/ 23.800000_fp, 31.400000_fp, 50.300000_fp, &
       52.800000_fp, 53.596000_fp, 54.500000_fp, &
       54.940000_fp, 55.500000_fp, 57.290344_fp, &
       89.000000_fp,150.00000_fp, 183.31000_fp   /)
  ! Infrared data
  INTEGER,  PARAMETER :: N_IR_CHANNELS = 18
  REAL(fp), PARAMETER, DIMENSION( N_IR_CHANNELS ) :: IR_FREQUENCIES = &
    (/  679.805_fp,  694.359_fp,  710.290_fp, &
        731.781_fp,  746.722_fp,  789.398_fp, &
        827.560_fp,  906.404_fp, 1028.098_fp, &
       1339.184_fp, 1422.206_fp, 1535.405_fp, &
       2182.836_fp, 2205.188_fp, 2243.165_fp, &
       2419.615_fp, 2506.577_fp, 2662.740_fp  /)


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: l
  REAL(fp) :: mw
  REAL(fp) :: ir


  ! ---------------------
  ! Convert GHz <-> cm^-1
  ! ---------------------
  WRITE( *, '( /5x, "Converting MW GHz <-> cm^-1...." )' )
  DO l = 1, N_MW_CHANNELS
    WRITE( *, FMT     = '( 10x, "MW frequency: ", f12.6, " Ghz   -> " )', &
              ADVANCE = 'NO' ) MW_FREQUENCIES(l)
    mw = GHz_to_inverse_cm(MW_FREQUENCIES(l))
    WRITE( *, FMT     = '( f12.6, " cm^-1 -> " )', &
              ADVANCE = 'NO' ) mw
    mw = inverse_cm_to_GHz(mw)
    WRITE( *, FMT = '( f12.6, " GHz " )' ) mw
  END DO


  ! -------------------------
  ! Convert cm^-1 <-> microns
  ! -------------------------
  WRITE( *, '( /5x, "Converting IR cm^-1 <-> microns...." )' )
  DO l = 1, N_IR_CHANNELS
    WRITE( *, FMT = '( 10x, "IR frequency: ", f12.6, " cm^-1 -> " )', &
              ADVANCE = 'NO' ) IR_FREQUENCIES(l)
    ir = inverse_cm_to_micron(IR_FREQUENCIES(l))
    WRITE( *, FMT = '( f12.6, " um    -> " )', &
              ADVANCE = 'NO' ) ir
    ir = micron_to_inverse_cm(ir)
    WRITE( *, FMT = '( f12.6, " cm^-1 " )' ) ir
  END DO

END PROGRAM Spectral_Units_Conversion_Test
