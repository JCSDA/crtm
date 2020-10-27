!
! AIRS_Subset
!
! Module containing AIRS channel subsetting routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Nov-2002
!                       paul.vandelst@noaa.gov
!

MODULE AIRS_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE AIRS_Define    , ONLY: N_AIRS_BANDS, N_AIRS_CHANNELS, &
                             AIRS_BeginChannel, &
                             AIRS_EndChannel
  USE Subset_Define  , ONLY: Subset_type, &
                             Subset_Associated, &
                             Subset_Generate
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: AIRS_SUBSET_281_COMMENT, N_AIRS_SUBSET_281, AIRS_SUBSET_281
  PUBLIC :: AIRS_SUBSET_324_COMMENT, N_AIRS_SUBSET_324, AIRS_SUBSET_324
  PUBLIC :: N_AIRS_VALID_SUBSETS, AIRS_VALID_SUBSET_NAME
  ! Procedures
  PUBLIC :: AIRS_Subset_Index
  PUBLIC :: AIRS_SubsetVersion


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256

  ! The AIRS subset 281 channel list
  CHARACTER(*), PARAMETER :: AIRS_SUBSET_281_COMMENT = 'AIRS 281 channel SUBSET'
  INTEGER, PARAMETER :: N_AIRS_SUBSET_281 = 281
  INTEGER, PARAMETER :: AIRS_SUBSET_281(N_AIRS_SUBSET_281) = &
    (/    1,    6,    7,   10,   11,   15,   16,   17,   20,   21,   22,   24,   27, &
         28,   30,   36,   39,   40,   42,   51,   52,   54,   55,   56,   59,   62, &
         63,   68,   69,   71,   72,   73,   74,   75,   76,   77,   78,   79,   80, &
         82,   83,   84,   86,   92,   93,   98,   99,  101,  104,  105,  108,  110, &
        111,  113,  116,  117,  123,  124,  128,  129,  138,  139,  144,  145,  150, &
        151,  156,  157,  159,  162,  165,  168,  169,  170,  172,  173,  174,  175, &
        177,  179,  180,  182,  185,  186,  190,  192,  198,  201,  204,  207,  210, &
        215,  216,  221,  226,  227,  232,  252,  253,  256,  257,  261,  262,  267, &
        272,  295,  299,  300,  305,  310,  321,  325,  333,  338,  355,  362,  375, &
        453,  475,  484,  497,  528,  587,  672,  787,  791,  843,  870,  914,  950, &
       1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082, 1083, 1088, 1090, &
       1092, 1095, 1104, 1111, 1115, 1116, 1119, 1120, 1123, 1130, 1138, 1142, 1178, &
       1199, 1206, 1221, 1237, 1252, 1260, 1263, 1266, 1285, 1301, 1304, 1329, 1371, &
       1382, 1415, 1424, 1449, 1455, 1466, 1477, 1500, 1519, 1538, 1545, 1565, 1574, &
       1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, 1674, 1681, 1694, 1708, 1717, &
       1723, 1740, 1748, 1751, 1756, 1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, &
       1803, 1806, 1812, 1826, 1843, 1852, 1865, 1866, 1868, 1869, 1872, 1873, 1876, &
       1881, 1882, 1883, 1911, 1917, 1918, 1924, 1928, 1937, 1941, 2099, 2100, 2101, &
       2103, 2104, 2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, 2116, &
       2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, 2134, 2141, 2145, 2149, 2153, &
       2164, 2189, 2197, 2209, 2226, 2234, 2280, 2318, 2321, 2325, 2328, 2333, 2339, &
       2348, 2353, 2355, 2357, 2363, 2370, 2371, 2377 /)


  ! The AIRS subset 324 channel list
  CHARACTER(*), PARAMETER :: AIRS_SUBSET_324_COMMENT = 'AIRS 324 channel SUBSET'
  INTEGER, PARAMETER :: N_AIRS_SUBSET_324 = 324
  INTEGER, PARAMETER :: AIRS_SUBSET_324(N_AIRS_SUBSET_324) = &
    (/  1,    6,    7,   10,   11,   15,   16,   17,   20,   21,   22,   24,   27, &
       28,   30,   36,   39,   40,   42,   51,   52,   54,   55,   56,   59,   62, &
       63,   68,   69,   71,   72,   73,   74,   75,   76,   77,   78,   79,   80, &
       82,   83,   84,   86,   92,   93,   98,   99,  101,  104,  105,  108,  110, &
      111,  113,  116,  117,  123,  124,  128,  129,  138,  139,  144,  145,  150, &
      151,  156,  157,  159,  162,  165,  168,  169,  170,  172,  173,  174,  175, &
      177,  179,  180,  182,  185,  186,  190,  192,  193,  198,  201,  204,  207, &
      210,  213,  215,  216,  218,  221,  224,  226,  227,  232,  239,  248,  250, &
      251,  252,  253,  256,  257,  261,  262,  267,  272,  295,  299,  300,  305, &
      308,  309,  310,  318,  321,  325,  333,  338,  355,  362,  375,  453,  475, &
      484,  497,  528,  587,  672,  787,  791,  843,  870,  914,  950, 1003, 1012, &
     1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082, 1083, 1088, 1090, 1092, 1095, &
     1104, 1111, 1115, 1116, 1119, 1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, &
     1221, 1237, 1252, 1260, 1263, 1266, 1278, 1285, 1290, 1301, 1304, 1329, 1371, &
     1382, 1400, 1401, 1402, 1403, 1415, 1424, 1449, 1455, 1466, 1471, 1477, 1479, &
     1488, 1500, 1519, 1520, 1538, 1545, 1565, 1574, 1583, 1593, 1614, 1627, 1636, &
     1644, 1652, 1669, 1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
     1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, 1812, 1826, 1843, &
     1852, 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 1877, 1881, 1882, &
     1883, 1884, 1897, 1901, 1911, 1917, 1918, 1921, 1923, 1924, 1928, 1937, 1938, &
     1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, 1988, 1995, 2084, 2085, 2097, &
     2098, 2099, 2100, 2101, 2103, 2104, 2106, 2107, 2108, 2109, 2110, 2111, 2112, &
     2113, 2114, 2115, 2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, 2134, &
     2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, 2234, 2280, 2318, 2321, &
     2325, 2328, 2333, 2339, 2348, 2353, 2355, 2357, 2363, 2370, 2371, 2377 /)


  ! The list of supported channel subsets
  INTEGER,      PARAMETER :: N_AIRS_VALID_SUBSETS = 4
  CHARACTER(*), PARAMETER :: AIRS_VALID_SUBSET_NAME(N_AIRS_VALID_SUBSETS) = &
    (/ '281 channel set', &
       '324 channel set', &
       'All channels   ', &
       'User specified ' /)


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_Subset_Index
! 
! PURPOSE:
!       Function to index AIRS channels for requested bands.
!
! CALLING SEQUENCE:
!       Error_Status = AIRS_Subset_Index( Band       , &  ! Input
!                                         Subset_List, &  ! Input
!                                         Subset       )  ! Output
!
! INPUTS:
!       Bands:           The AIRS band from which to subset channels.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Subset_List:     The list of AIRS channels to subset. This can be a 
!                        user defined channel list, or the standard 300 or 316
!                        channel subset lists parameterised in the arrays
!                        AIRS_SUBSET_300 and AIRS_SUBSET_316.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Subset:          The AIRS Subset structure containing the indexed
!                        subset channels for the specified AIRS module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Subset_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the channel subsetting was successful
!                           == FAILURE an error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AIRS_Subset_Index( &
    Band       , &  ! Input
    Subset_List, &  ! Input
    Subset     ) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER          , INTENT(IN)  :: Band
    INTEGER          , INTENT(IN)  :: Subset_List(:)
    TYPE(Subset_type), INTENT(OUT) :: Subset
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AIRS_Subset_Index'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_subset_channels
    INTEGER :: ch1, ch2, i

    ! Set up
    err_stat = SUCCESS
    ! ...Check the band
    IF ( Band < 1 .OR. Band > N_AIRS_BANDS ) THEN
      msg = 'Invalid band.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      RETURN
    END IF
    ! ...No channel list data?
    n_subset_channels = SIZE(Subset_List)
    IF ( n_subset_channels < 1 ) THEN
      msg = 'Input Subset_List contains no data.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Any weird channel numbers?
    IF ( ANY( Subset_List < 1 .OR. Subset_List > N_AIRS_CHANNELS ) ) THEN
      msg = 'Invalid channel in Subset_List input.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Set the band limits
    ch1 = AIRS_BeginChannel( Band )
    ch2 = AIRS_EndChannel( Band )


    ! Generate the subset
    CALL Subset_Generate( Subset, (/(i,i=ch1,ch2)/), Subset_List )
    IF ( .NOT. Subset_Associated( Subset ) ) THEN
      msg = 'Error generating Subset structure.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF

  END FUNCTION AIRS_Subset_Index

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AIRS_SubsetVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL AIRS_SubsetVersion( Id )
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

  SUBROUTINE AIRS_SubsetVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE AIRS_SubsetVersion

END MODULE AIRS_Subset
