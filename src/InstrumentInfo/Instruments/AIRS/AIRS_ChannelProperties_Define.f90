
MODULE AIRS_ChannelProperties_Define


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: AIRS_ChannelProperties_Define.f90,v 1.5 2004/08/11 19:59:51 paulv Exp $'

  ! -- Data structure string lengths
  INTEGER, PRIVATE, PARAMETER :: ML = 5
  INTEGER, PRIVATE, PARAMETER :: CL = 8

  ! -- Initialization values
  INTEGER,         PRIVATE, PARAMETER :: I_INIT = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: R_INIT = -999.9_fp_kind
  CHARACTER( * ),  PRIVATE, PARAMETER :: C_INIT = ' ' 

  ! -------------------------------------
  ! AIRS_ChannelInfo data type definition
  ! -------------------------------------

  ! -- Structure for the channel info
  TYPE, PUBLIC :: AIRS_ChannelProperties_type
    INTEGER                         :: Channel_Number            = I_INIT
    REAL( fp_kind )                 :: Frequency                 = R_INIT
    CHARACTER( ML )                 :: Module_Name               = C_INIT
    INTEGER                         :: Calibration_Channel_Index = I_INIT
    REAL( fp_kind )                 :: NEdT                      = R_INIT
    REAL( fp_kind )                 :: FWHM                      = R_INIT
    REAL( fp_kind )                 :: Cij                       = R_INIT
    REAL( fp_kind ), DIMENSION( 2 ) :: Centroid                  = R_INIT
    REAL( fp_kind )                 :: RTA_Fitting_Error         = R_INIT
    INTEGER                         :: AB_State                  = I_INIT
    INTEGER                         :: Radiometric_Quality       = I_INIT
    INTEGER                         :: Bad_Flag                  = I_INIT
    CHARACTER( CL )                 :: Comment                   = C_INIT
  END TYPE AIRS_ChannelProperties_type

END MODULE AIRS_ChannelProperties_Define


