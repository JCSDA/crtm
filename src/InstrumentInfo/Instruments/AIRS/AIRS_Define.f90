!
! AIRS_Define
!
! Module containing AIRS instrument definitions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Nov-2002
!                       paul.vandelst@noaa.gov
!

MODULE AIRS_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything is default private
  PRIVATE
  ! Parameters
  PUBLIC :: N_AIRS_BANDS
  PUBLIC :: N_AIRS_CHANNELS
  ! Procedures
  PUBLIC :: AIRS_BeginChannel
  PUBLIC :: AIRS_EndChannel
  PUBLIC :: AIRS_nPts
  PUBLIC :: AIRS_Channels
  PUBLIC :: AIRS_BandName
  PUBLIC :: AIRS_DefineVersion


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &

  ! Instrument parameters
  INTEGER, PARAMETER :: N_AIRS_BANDS    = 17
  INTEGER, PARAMETER :: N_AIRS_CHANNELS = 2378


  ! Band parameters
  ! ...Band names IN FREQUENCY/CHANNEL ORDER
  CHARACTER(*), PARAMETER :: BAND_NAME(N_AIRS_BANDS) = &
    (/ 'M12','M11','M10','M9 ','M8 ','M7 ','M6 ','M5 ', &
       'M4d','M4c','M3 ','M4b','M4a','M2b','M1b','M2a','M1a' /)
  ! ...Channel numbering
  INTEGER, PARAMETER :: BEGIN_CHANNEL(N_AIRS_BANDS) = &
    (/     1,  131,  275,  442,  609,  770,  937, 1104, &
        1263, 1369, 1463, 1655, 1761, 1865, 2015, 2145, 2261 /)
  INTEGER, PARAMETER :: END_CHANNEL(N_AIRS_BANDS) = &
    (/   130,  274,  441,  608,  769,  936, 1103, 1262, &
        1368, 1462, 1654, 1760, 1864, 2014, 2144, 2260, 2378 /)
  INTEGER, PARAMETER :: N_CHANNELS_PER_BAND(N_AIRS_BANDS) = &
    (/   130,  144,  167,  167,  161,  167,  167,  159, &
         106,   94,  192,  106,  104,  150,  130,  116,  118 /)
  INTEGER, PARAMETER :: MAX_N_BAND_CHANNELS = 192


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_nPts
!
! PURPOSE:
!       Pure function to compute the number of spectral points in an AIRS band.
!
! CALLING SEQUENCE:
!       n = AIRS_nPts(band)
!
! INPUT ARGUMENTS:
!       band:    AIRS band number (1 to 17).
!                  If Band < 1, then 1 is used.
!                     Band > 17, then 17 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n:         Number of spectral points for the specified AIRS band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION AIRS_nPts(band) RESULT(n)
    INTEGER, INTENT(IN) :: band
    INTEGER :: n
    INTEGER :: ib

    ! Setup
    ib = MAX(MIN(band,N_AIRS_BANDS),1)
    
    ! Get the number of points
    n = N_CHANNELS_PER_BAND(ib)

  END FUNCTION AIRS_nPts


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_Channels
!
! PURPOSE:
!       Pure function to compute the channel numbers for an AIRS band.
!
! CALLING SEQUENCE:
!       ch = AIRS_Channels(band)
!
! INPUT ARGUMENTS:
!       band:      AIRS band number (1 to 17).
!                  If Band < 1, then 1 is used.
!                     Band > 17, then 17 is used.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  SCALAR
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch:        The channel numbers for the specified AIRS band.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Rank-1
!
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION AIRS_Channels(band) RESULT(ch)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch(AIRS_nPts(band))
    ! Local variables
    INTEGER :: i, ib, ic1, ic2

    ! Setup
    ib = MAX(MIN(band,N_AIRS_BANDS),1)
    
    ! Select channel bounds
    ic1 = AIRS_BeginChannel(ib)
    ic2 = AIRS_EndChannel(ib)
    
    ! Construct channel array
    ch = (/(i, i=ic1,ic2)/)
    
  END FUNCTION AIRS_Channels


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_BeginChannel
!
! PURPOSE:
!       Pure function to return the AIRS band begin channel number.
!
! CALLING SEQUENCE:
!       ch1 = AIRS_BeginChannel(band)
!
! INPUTS:
!       band:     AIRS band number (1 to 17).
!                  If Band < 1, then 1 is used.
!                     Band > 17, then 17 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch1:      Begin channel number for the AIRS band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION AIRS_BeginChannel(band) RESULT(ch1)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch1
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_AIRS_BANDS),1)

    ! Retrieve the begin channel number
    ch1 = BEGIN_CHANNEL(ib)
    
  END FUNCTION AIRS_BeginChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_EndChannel
!
! PURPOSE:
!       Pure function to return the AIRS band end channel number.
!
! CALLING SEQUENCE:
!       ch2 = AIRS_EndChannel(band)
!
! INPUTS:
!       band:     AIRS band number (1 to 17).
!                  If Band < 1, then 1 is used.
!                     Band > 17, then 17 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       ch2:      End channel number for the AIRS band.
!                 UNITS:      N/A
!                 TYPE:       REAL(fp)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION AIRS_EndChannel(band) RESULT(ch2)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    INTEGER :: ch2
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_AIRS_BANDS),1)

    ! Retrieve the end channel number
    ch2 = END_CHANNEL(ib)
    
  END FUNCTION AIRS_EndChannel


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_BandName
!
! PURPOSE:
!       Pure function to return the AIRS band name string.
!
! CALLING SEQUENCE:
!       name = AIRS_BandName(band)
!
! INPUTS:
!       band:     AIRS band number (1 to 17).
!                  If Band < 1, then 1 is used.
!                     Band > 17, then 17 is used.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  SCALAR
!                 ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:     String containing the AIRS band name.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!:sdoc-:
!--------------------------------------------------------------------------------
  PURE FUNCTION AIRS_BandName(band) RESULT(name)
    ! Arguments
    INTEGER, INTENT(IN) :: band
    ! Function result
    CHARACTER(LEN(BAND_NAME(1))) :: name
    ! Variables
    INTEGER  :: ib

    ! Setup
    ib = MAX(MIN(band,N_AIRS_BANDS),1)

    ! Retrieve the band name
    name = BAND_NAME(ib)
    
  END FUNCTION AIRS_BandName


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AIRS_DefineVersion( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AIRS_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AIRS_DefineVersion

END MODULE AIRS_Define
