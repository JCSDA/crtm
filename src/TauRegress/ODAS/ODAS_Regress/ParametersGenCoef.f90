!====================================================================
!
! MODULE: ParametersGenCoef
!
!   The module 'ParametersGenCoef' defines parametric and global
!   variables for generating OPTRAN transmittance coefficinets.
!
!   Most of the global variables are substitued in the subroutine
!   ReadParameters() reading namelist and other data files.
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02 to read netCDF, etc.
!
!====================================================================


module ParametersGenCoef

  !--- modules

  use type_kinds, only : fp_kind
  use SpcCoeff_Define


  !--- Implicit

  implicit none


  !--- numbers

  real(fp_kind),parameter :: ZERO    =  0._fp_kind
  real(fp_kind),parameter :: ONE     =  1._fp_kind
  real(fp_kind),parameter :: TWO     =  2._fp_kind
  real(fp_kind),parameter :: THREE   =  3._fp_kind
  real(fp_kind),parameter :: FOUR    =  4._fp_kind
  real(fp_kind),parameter :: FIVE    =  5._fp_kind
  real(fp_kind),parameter :: TEN     = 10._fp_kind
  real(fp_kind),parameter :: POINT1  = ONE / TEN
  real(fp_kind),parameter :: POINT5  = ONE / TWO
  real(fp_kind),parameter :: POINT75 = THREE / FOUR


  !--- missing

  real(fp_kind),parameter :: RMISS = -2**15 * ONE
  real(fp_kind),parameter :: HMISS = RMISS / TWO


  !--- tolerance, infinite

  real(fp_kind),parameter :: TOLERANCE        = EPSILON( ONE )
  real(fp_kind),parameter :: INFINITE         = HUGE( ONE )
  real(fp_kind),parameter :: LOG_INF          = 709._fp_kind     !      log( INFINITE )
  real(fp_kind),parameter :: HALF_LOG_INF     = LOG_INF / TWO    ! 0.5 * log( INFINITE )
  real(fp_kind),parameter :: EXP_HALF_LOG_INF = 1.E+150_fp_kind

  !--- some thresholds
  
    !--- minimum rmse Tb error in K, predictor search is terminated if the fitting 
    !    error is less than this number.
  
!  real(fp_kind),parameter :: TB_RMSE_MIN = 0.025_fp_kind
  real(fp_kind),parameter :: TB_RMSE_MIN = 0.01_fp_kind
  
    !--- threshold for the deffience between two TB_RMSEs
    !    below that the improvement is not significant
    
!  real(fp_kind),parameter :: DELTA_TB_RMSE = 0.025_fp_kind

    !--- significant fraction change of RMSE tau, used for searching best predictors
    
  real(fp_kind),parameter :: DTAU_THRESHOLD = 0.005_fp_kind 
!  real(fp_kind),parameter :: DTAU_THRESHOLD = 0.001_fp_kind 

    !--- Max polynomial order.
    
  integer      ,parameter :: Npolyorder_max     = 10    ! max order of polynomial predictors
  integer      ,parameter :: Npolyorder_max_preferred  = 5
    
  !--- physical constants (referred to NIST web site at physics.nist.gov in Apr 2002)

  real(fp_kind),parameter :: RECIPROCAL_GRAVITY = 1._fp_kind / 980.665_fp_kind  ! reciprocal gravity (scaled by 100 for use with pressure in hPa)
  real(fp_kind),parameter :: SPEED_OF_LIGHT     = 2.99792458e+08_fp_kind        ! (m/s)
  real(fp_kind),parameter :: PLANCK_CONSTANT    = 6.62606876e-34_fp_kind        ! (J s)
  real(fp_kind),parameter :: BOLTZMANN_CONSTANT = 1.3806503e-23_fp_kind         ! (J/K)


  !--- parameters for OPTRAN

  real(fp_kind), SAVE     :: VirtEmiss                           ! virtial emissivity
  real(fp_kind),parameter :: VirtEmiss_MW         = 0.98_fp_kind ! virtial emissivity
  real(fp_kind),parameter :: VirtEmiss_IR         = 0.98_fp_kind ! virtial emissivity

  integer      ,parameter :: Nabsorber           = 3    ! # of absorber gases considered in OPTRAN
  integer                 :: Iabsorber                  ! absorber index to be processed
                                                        ! 1:dry, 2:wet, 3:ozone

  real(fp_kind),parameter :: TOP_AbsAmountLev    = ZERO ! top of absorber amount level
  real(fp_kind),parameter :: Bottom_AbsAmountLev = ONE  ! bottom of absorber amount level
  real(fp_kind)           :: Alpha                      ! alpha
  real(fp_kind),parameter :: Alpha_list(Nabsorber) &    ! alpha (default)
                                = (/ 10._fp_kind,  &    ! dry
!original                            15._fp_kind,  &    ! wet
                                     13._fp_kind,  &    ! wet
!most stable                         12._fp_kind,  &    ! wet
!less stable                          4._fp_kind  /)    ! ozone
                                      8._fp_kind  /)    ! ozone
  real(fp_kind)           :: Abslvl_coef1, Abslvl_coef2 ! A(k) = abscoef1 * exp(alpha*k) + abscoef2
  real(fp_kind)           :: MaxAbsAmount
  real(fp_kind)           :: MinAbsAmount

  integer      ,parameter :: Natmpred_max       = 17    ! # of atmospheric predictors

  integer      ,parameter :: Natmpred_maxused   = 6     ! # of atmos. predictors maxmumly used in regression equation
! integer      ,parameter :: Natmpred_maxused   = 2     ! (experiment)
! integer      ,parameter :: Natmpred_maxused   = 3     ! (experiment)

  integer      ,parameter :: Ncoef_atmpred      = ( Natmpred_maxused   + 1 )
  integer      ,parameter :: Ncoef_polyfit_max  = ( Npolyorder_max + 1 )
  integer      ,parameter :: Ncoef_max          = Ncoef_atmpred * Ncoef_polyfit_max   ! # of regression coefficients

  real(fp_kind),parameter :: Weight_base_list(Nabsorber) &      ! min value for regression weight
                                = (/ 5.e-6_fp_kind, &           ! dry
                                     1.e-2_fp_kind, &           ! wet
                                     5.e-3_fp_kind /)           ! ozone

  ! molecule ID used in line-by-line transmittance calculations

  integer,parameter   :: IdMolecule_All     =  10   ! molecule ID in netCDF file (total)
  integer,parameter   :: IdMolecule_Dry     = 113   ! molecule ID in netCDF file (dry gas)
  integer,parameter   :: IdMolecule_Wet     =  12   ! molecule ID in netCDF file (wet gas)
!  integer,parameter   :: IdMolecule_Wet     = 112   ! molecule ID in netCDF file (wet gas)
  integer,parameter   :: IdMolecule_Ozo     = 114   ! molecule ID in netCDF file (o3  gas)

  ! Gas IDs used internally: 1 - dry; 2 - wet; 3 - ozo
  ! This arrary converts internal gas Id to those used in line-by-line program
  
  integer, parameter   :: gasID_convert(3) = &
                        (/IdMolecule_Dry,IdMolecule_Wet,IdMolecule_Ozo /)
  
  !--- parameters for atmospheric and transmittance profiles

  real(fp_kind),parameter   :: MaxSecAng_MaxAbsAmount = 2.25_fp_kind

  integer                   :: Natm                             ! # of atmospheric profiles
  integer                   :: Nlay                             ! # of atmospheric profile layers
  integer                   :: Nangle                           ! # of incidence angles to be processed
  real(fp_kind),allocatable :: secant(:)                        ! secant incidence angles

!  integer      ,parameter   :: Natm_max   = 48                  ! max # of atmospheric profiles
  integer      ,parameter   :: Natm_max   = 100                  ! max # of atmospheric profiles
  integer      ,parameter   :: Nlay_max   = 100                 ! max # of atmospheric profile layers
  integer      ,parameter   :: Nangle_max = 7                   ! max # of incidence angles to be processed

  logical                   :: Flag_netCDF_file                 ! T) read netCDF profile data, F) CIMSS3246 binary data file


  !--- parameters for generating trans coef

  integer                 :: Natmpred_allcombsearch             ! # of predictors searched for all the combinations

  real(fp_kind),parameter :: MinTrans_SenseCheck = 0.001_fp_kind  ! min trans to check atmos pred set sensitivity
  real(fp_kind),parameter :: Criterion_AtmPredSensitivity(Nabsorber) &
                                        = (/ 3._fp_kind, &      ! criterion for the check the sensitivity (dry)
                                             2._fp_kind, &      ! criterion for the check the sensitivity (wet)
                                             3._fp_kind /)      ! criterion for the check the sensitivity (ozo)
!                                            4._fp_kind /)      ! criterion for the check the sensitivity (ozo)

  real(fp_kind),parameter :: Threshold_TransFitTest = 0.0001_fp_kind ! threshold to test trans fitting for predictor selection

  real(fp_kind),save      :: Max_Predictand                     ! max value of predictands
  logical                 :: Flag_AtmProfile_Lev2Lay            ! T) convert atmos profiles from level to layer
                                                                ! F) read layer atmos profiles

  !--- satellite and sensor's name, ID, channel, coef

  character(len=20)   :: SatName                ! Satellite name
  character(len=20)   :: SenName                ! Sensor name
  integer             :: SatIdWmo               ! Sat ID (WMO)
  integer             :: SenIdWmo               ! Sen ID (WMO)
!  integer             :: SenType                ! Sen Type
  integer             :: SenIdNcep              ! Sen ID (NCEP)

  type(SpcCoeff_type), save :: SpcCoeff  ! spectrum coef (freq, Plunck const)

  integer             :: Nchan                  ! # of ch
  integer,allocatable :: channel_list(:)        ! ch ID list
  integer,allocatable :: channel_type(:)        ! channel type
                                                ! 1:dry 2:dominated by wet abs line 3:ozone
                                                ! +10:affected by wet continuum

  integer             :: Ichan_top              ! top seq ch # to be processed
  integer             :: Ichan_last             ! last seq ch # to be processed

  ! --- comments

!  integer, parameter            :: Comment_strlen = 2048
  integer, parameter            :: Comment_strlen = 3000
  character(len=Comment_strlen) :: Comment_all           ! Comment

  ! --- history information
  
!  integer, parameter 		:: History_strlen = 2048
  integer, parameter :: History_strlen = 3000
  character(len=History_strlen) :: History_all           ! history information 

  character(len=80) :: ProfileTag    ! tag of dependent profile set
  
  !--- in file names and seq #

  character(len=80),parameter :: InFileName_Namelist   = 'Namelist.txt'         ! namelist data file
  character(len=80),parameter :: InFileName_AtmPredFlg = 'AtmPredFlag.txt'      ! atmos pred availability flags
  character(len=80),parameter :: InFileName_SenInfo    = 'SenInfo.txt'          ! sensor information
  character(len=80),parameter :: InFileName_SpcCoef    = 'SpcCoef.nc'          ! sensor information
  character(len=80),parameter :: InFileName_AtmosProf  = 'AtmProfile.nc'        ! atmos prof netCDF file
  character(len=80),parameter :: InFileName_TransProf  = 'TauProfile.nc'        ! trans netCDF file

  !--- files sequence number (out file)

  character(len=80),parameter :: OutFileName_TransCoeff = "TauCoeff.nc" ! Trans coef table
  character(len=80),parameter :: OutFileName_CompleteSignal = "Completion_signal.txt" ! Signal job completion 
  integer,parameter   :: OutGdasTable    = 30          ! binary Trans coef table
  integer,parameter   :: OutAtmPred      = 31          ! Atmos pred availability flags
  integer,parameter   :: OutStat         = 40          ! statisitical result in text
  integer,parameter   :: OutTb           = 41          ! Tb (cal & lbl)
! integer,parameter   :: OutTau          = 42          ! Tau (cal & lbl)
  integer,parameter   :: OutLayer        = 43          ! Monitor output for layer values
  integer,parameter   :: OutLevel        = 44          ! Monitor output for level values

  ! Interferometer WMO_Sensor_IDs
  INTEGER, PUBLIC, PARAMETER :: INTERFEROMETER_LIST(2) = (/221, 620/)  !IASI and CrIS 
  
end module ParametersGenCoef




