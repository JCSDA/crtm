!
! LBLRTM_Parameters
!
! Module containing shared parameters required for LBLRTM
! format file IO
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_Parameters

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------
  PRIVATE

 
  ! -----------------
  ! Module parameters
  ! -----------------

  ! Data types. The data types are respecified here so that they are
  ! easy to change for the case where the "double precision" version
  ! of LBLRTM has been used. In this case, all the single precision
  ! variable become double precision, and the 4-byte integers become
  ! 8-byte integers. The latter is not available on all platforms.
  ! ----------------------------------------------------------------
  ! Single precision types
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_KIND    = Long
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_N_BYTES = n_Bytes_Long
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_KIND    = Single
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_N_BYTES = n_Bytes_Single
  ! Double precision types
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_KIND    = LLong
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_N_BYTES = n_Bytes_LLong
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_KIND    = Double
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_N_BYTES = n_Bytes_Double


  ! Maximum number of molecules
  ! ---------------------------
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_MAX_N_MOLECULES = 64


  ! Maximum number of points in a panel of data
  ! -------------------------------------------
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_MAX_PANEL_POINTS = 2400


  ! File status flags
  ! -----------------
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_OK    =   1  ! File status is o.k.
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_EOF   =   0  ! End-Of-File
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_EOL   = -99  ! End-Of-Layer
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_UNDEF =  -1  ! File status is undefined


  ! ----------------
  ! Panel inforation
  ! ----------------
  ! The number of recognised panel types
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_N_PANEL_TYPES = 4

  ! Panel type codes
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_UNKNOWN_PANEL_TYPE   = 1
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_UNDEFINED_PANEL_TYPE = 2
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_SINGLE_PANEL_TYPE    = 3 ! This must be 3
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_DOUBLE_PANEL_TYPE    = 4 ! This must be 4
  INTEGER, PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_PANEL_TYPE = (/ LBLRTM_UNKNOWN_PANEL_TYPE,   &   
                           LBLRTM_UNDEFINED_PANEL_TYPE, & 
                           LBLRTM_SINGLE_PANEL_TYPE,    &    
                           LBLRTM_DOUBLE_PANEL_TYPE    /)

  ! The number of panels for each type    
  INTEGER, PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_N_PANELS = (/ -1, &  ! LBLRTM_UNKNOWN_PANEL_TYPE  
                         -1, &  ! LBLRTM_UNDEFINED_PANEL_TYPE
                          1, &  ! LBLRTM_SINGLE_PANEL_TYPE   
                          2 /)  ! LBLRTM_DOUBLE_PANEL_TYPE   

  ! The panel type names. Used for error/message output.
  CHARACTER(*), PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_PANEL_TYPE_NAME = (/ 'UNKNOWN  ', &  ! LBLRTM_UNKNOWN_PANEL_TYPE  
                                'UNDEFINED', &  ! LBLRTM_UNDEFINED_PANEL_TYPE
                                'SINGLE   ', &  ! LBLRTM_SINGLE_PANEL_TYPE   
                                'DOUBLE   ' /)  ! LBLRTM_DOUBLE_PANEL_TYPE   

END MODULE LBLRTM_Parameters
