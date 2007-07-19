!
! AIRS_Subset
!
! Module containing AIRS channel subsetting routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AIRS_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Display_Message
  USE AIRS_Define       , ONLY: N_AIRS_MODULES, &
                                N_AIRS_CHANNELS, &
                                N_AIRS_CHANNELS_PER_MODULE, &
                                AIRS_MODULE_BEGIN_CHANNEL, &
                                AIRS_MODULE_END_CHANNEL
  USE AIRS_Subset_Define, ONLY: AIRS_Subset_type, &
                                Destroy_AIRS_Subset, &
                                Allocate_AIRS_Subset, &
                                Assign_AIRS_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types inherited from the
  ! AIRS_Subset_Define module
  PUBLIC :: AIRS_Subset_type
  ! Routines inherited from the 
  ! AIRS_Subset_Define module
  PUBLIC :: Destroy_AIRS_Subset
  PUBLIC :: Allocate_AIRS_Subset
  PUBLIC :: Assign_AIRS_Subset
  ! Parameters
  PUBLIC :: AIRS_SUBSET_281_COMMENT
  PUBLIC :: N_AIRS_SUBSET_281
  PUBLIC :: AIRS_SUBSET_281
  PUBLIC :: AIRS_SUBSET_324_COMMENT
  PUBLIC :: N_AIRS_SUBSET_324
  PUBLIC :: AIRS_SUBSET_324
  ! Procedures
  PUBLIC :: Index_AIRS_Subset


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

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


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Index_AIRS_Subset
! 
! PURPOSE:
!       Function to index AIRS channels for requested modules.
!
! CALLING SEQUENCE:
!       Error_Status = Index_AIRS_Subset( Module_Number,          &  ! Input
!                                         Subset_List,            &  ! Input
!                                         Subset,                 &  ! Output
!                                         RCS_Id     =RCS_Id,     &  ! Optional output
!                                         Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Module_Numbers:  The AIRS detector module number from which to subset 
!                        channels.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Subset_List:     The list of AIRS channels to subset. This can be a 
!                        user defined channel list, or the standard 281 or 324
!                        channel subset lists parameterised in the arrays
!                        AIRS_SUBSET_281 and AIRS_SUBSET_324.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Subset:          The AIRS Subset structure containing the indexed
!                        subset channels for the specified AIRS module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(AIRS_Subset_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the channel subsetting was successful
!                           == FAILURE - errors were detected with the input data, or
!                                      - the structure allocation failed..
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Index_AIRS_Subset( Module_Number, &  ! Input
                              Subset_List,   &  ! Input
                              Subset,        &  ! Output
                              RCS_Id,        &  ! Revision control
                              Message_Log )  &  ! Error mssaging
                            RESULT( Error_Status )
    ! Arguments
    INTEGER               , INTENT(IN)     :: Module_Number
    INTEGER               , INTENT(IN)     :: Subset_List(:)
    TYPE(AIRS_Subset_type), INTENT(IN OUT) :: Subset
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Index_AIRS_Subset'
    ! Local variables
    INTEGER :: n_Subset_Channels
    INTEGER :: l, l1, l2
    INTEGER :: l_Subset, l_Extract
    INTEGER :: n_Channels
    INTEGER :: Channel

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the module number
    IF ( Module_Number < 1 .OR. Module_Number > N_AIRS_MODULES ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid AIRS module number.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! No channel list data?
    n_Subset_Channels = SIZE(Subset_List)
    IF ( n_Subset_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Subset_List contains no data.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Any weird channel numbers?
    IF ( ANY( Subset_List < 1 .OR. Subset_List > N_AIRS_CHANNELS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid channel in Subset_List input.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Set the module limits
    ! ---------------------
    l1 = AIRS_MODULE_BEGIN_CHANNEL( Module_Number )
    l2 = AIRS_MODULE_END_CHANNEL( Module_Number )


    ! Count the channels to subset
    ! ----------------------------
    n_Channels = COUNT( Subset_List >= l1 .AND. Subset_List <= l2 )
    IF ( n_Channels == 0 ) RETURN


    ! Allocate the AIRS Subset structure
    ! ----------------------------------
    Error_Status = Allocate_AIRS_Subset( n_Channels, &
                                         Subset, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating Subset structure.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Define the start points for the channel search
    ! ----------------------------------------------
    ! Determine the starting index in the SUBSET channel list array
    l_Subset = MINLOC( Subset_List - l1, &
                       MASK=( (Subset_List - l1) >= 0 ), &
                       DIM =1 )

    ! Set the starting index in the MODULE channel list array.
    ! This is always 1.
    l_Extract = 1


    ! Loop over the number of channels in the current module
    ! ------------------------------------------------------
    Channel_Loop: DO l = 1, N_AIRS_CHANNELS_PER_MODULE( Module_Number )

      ! Determine the current channel number
      Channel = AIRS_MODULE_BEGIN_CHANNEL( Module_Number ) + l - 1

      ! Is the current channel in the subset?
      IF ( Channel == Subset_List( l_Subset ) ) THEN

        ! Save the channel index and number
        Subset%Channel_Index(  l_Extract ) = l
        Subset%Channel_Number( l_Extract ) = Channel

        ! Increment the subset and extract indices
        l_Extract = l_Extract + 1
        l_Subset  = l_Subset  + 1

        ! If subset index is greater than n_Subset_Channels
        ! then we've just found the last subset channel
        IF ( l_Subset > n_Subset_Channels ) EXIT Channel_Loop

      END IF

    END DO Channel_Loop

  END FUNCTION Index_AIRS_Subset

END MODULE AIRS_Subset
