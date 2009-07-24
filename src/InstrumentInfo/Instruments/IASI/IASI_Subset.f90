!
! IASI_Subset
!
! Module containing IASI channel subsetting routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 23-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE IASI_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  USE Sort_Utility         , ONLY: InsertionSort
  USE IASI_Define          , ONLY: N_IASI_BANDS, &
                                   N_IASI_CHANNELS, &
                                   N_IASI_CHANNELS_PER_BAND, &
                                   IASI_BAND_BEGIN_CHANNEL, &
                                   IASI_BAND_END_CHANNEL
  USE Channel_Subset_Define, ONLY: Channel_Subset_type, &
                                   Destroy_Channel_Subset, &
                                   Allocate_Channel_Subset, &
                                   Assign_Channel_Subset
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Parameters
  PUBLIC :: IASI_SUBSET_300_COMMENT, N_IASI_SUBSET_300, IASI_SUBSET_300
  PUBLIC :: IASI_SUBSET_316_COMMENT, N_IASI_SUBSET_316, IASI_SUBSET_316
  PUBLIC :: IASI_SUBSET_616_COMMENT, N_IASI_SUBSET_616, IASI_SUBSET_616
  ! Procedures
  PUBLIC :: Index_IASI_Subset


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! The EUMETSAT IASI subset 300 channel list
  CHARACTER(*), PARAMETER :: IASI_SUBSET_300_COMMENT = 'EUMETSAT IASI 300 channel SUBSET'
  INTEGER, PARAMETER :: N_IASI_SUBSET_300 = 300
  INTEGER, PARAMETER :: IASI_SUBSET_300(N_IASI_SUBSET_300) = &
    (/   16,   38,   49,   51,   55,   57,   59,   61,   63,   66,   70,   72,   74, &
         79,   81,   83,   85,   87,   89,   92,   95,   97,   99,  101,  104,  106, &
        109,  111,  113,  116,  119,  122,  125,  128,  131,  133,  135,  138,  141, &
        144,  146,  148,  151,  154,  157,  159,  161,  163,  167,  170,  173,  176, &
        180,  185,  187,  193,  199,  205,  207,  210,  212,  214,  217,  219,  222, &
        224,  226,  230,  232,  236,  239,  243,  246,  249,  252,  254,  260,  262, &
        265,  267,  269,  275,  282,  294,  296,  299,  303,  306,  323,  327,  329, &
        335,  345,  347,  350,  354,  356,  360,  366,  371,  373,  375,  377,  379, &
        381,  383,  386,  389,  398,  401,  404,  407,  410,  414,  416,  426,  428, &
        432,  434,  439,  445,  457,  515,  546,  552,  559,  566,  571,  573,  646, &
        662,  668,  756,  867,  906,  921, 1027, 1046, 1121, 1133, 1191, 1194, 1271, &
       1479, 1509, 1513, 1521, 1536, 1574, 1579, 1585, 1587, 1626, 1639, 1643, 1652, &
       1658, 1671, 1786, 1805, 1884, 1991, 2019, 2094, 2119, 2213, 2239, 2271, 2321, &
       2398, 2701, 2741, 2819, 2889, 2907, 2910, 2919, 2939, 2944, 2948, 2951, 2958, &
       2977, 2985, 2988, 2991, 2993, 3002, 3008, 3014, 3027, 3029, 3036, 3047, 3049, &
       3053, 3058, 3064, 3069, 3087, 3093, 3098, 3105, 3107, 3110, 3127, 3136, 3151, &
       3160, 3165, 3168, 3175, 3178, 3207, 3228, 3244, 3248, 3252, 3256, 3263, 3281, &
       3303, 3309, 3312, 3322, 3375, 3378, 3411, 3438, 3440, 3442, 3444, 3446, 3448, &
       3450, 3452, 3454, 3458, 3467, 3476, 3484, 3491, 3497, 3499, 3504, 3506, 3509, &
       3518, 3527, 3555, 3575, 3577, 3580, 3582, 3586, 3589, 3599, 3653, 3658, 3661, &
       4032, 5368, 5371, 5379, 5381, 5383, 5397, 5399, 5401, 5403, 5405, 5455, 5480, &
       5483, 5485, 5492, 5502, 5507, 5509, 5517, 5558, 5988, 5992, 5994, 6003, 6982, &
       6985, 6987, 6989, 6991, 6993, 6995, 6997, 7267, 7269, 7424, 7426, 7428, 7885, &
       8007 /)


  ! The NESDIS IASI subset 316 channel list
  CHARACTER(*), PARAMETER :: IASI_SUBSET_316_COMMENT = 'NESDIS IASI 316 channel SUBSET'
  INTEGER, PARAMETER :: N_IASI_SUBSET_316 = 316
  INTEGER, PARAMETER :: IASI_SUBSET_316(N_IASI_SUBSET_316) = &
    (/   29,   32,   35,   41,   44,   47,   50,   53,   56,   62,   68,   76,   78, &
         82,   84,   86,   93,  103,  110,  150,  160,  179,  191,  197,  200,  202, &
        203,  213,  218,  225,  228,  231,  237,  259,  279,  285,  300,  309,  313, &
        320,  326,  332,  363,  372,  405,  408,  411,  418,  423,  433,  442,  450, &
        459,  472,  477,  483,  509,  578,  584,  594,  625,  705,  739,  797, 1090, &
       1098, 1173, 1222, 1283, 1338, 1409, 1414, 1420, 1424, 1427, 1430, 1434, 1440, &
       1442, 1445, 1450, 1454, 1460, 1463, 1469, 1474, 1483, 1487, 1494, 1496, 1502, &
       1505, 1510, 1518, 1526, 1529, 1532, 1537, 1541, 1545, 1548, 1553, 1560, 1568, &
       1583, 1606, 1659, 1666, 1675, 1681, 1694, 1697, 1710, 1791, 1839, 1913, 1946, &
       1947, 2289, 2333, 2346, 2349, 2352, 2359, 2367, 2374, 2426, 2562, 2745, 2760, &
       2921, 2945, 2971, 2990, 3030, 3052, 3055, 3116, 3129, 3146, 3189, 3295, 3326, &
       3354, 3366, 3416, 3432, 3610, 3626, 3638, 3646, 3673, 3689, 3700, 3710, 3726, &
       3763, 3814, 3841, 3888, 4059, 4068, 4082, 4095, 4160, 4234, 4257, 4411, 4498, &
       4520, 4552, 4567, 4608, 4646, 4698, 4808, 4849, 4920, 4939, 4947, 4967, 4991, &
       4996, 5015, 5028, 5056, 5128, 5130, 5144, 5170, 5178, 5183, 5188, 5191, 5446, &
       5472, 5497, 5528, 5697, 5714, 5749, 5766, 5785, 5798, 5799, 5801, 5817, 5833, &
       5834, 5836, 5849, 5851, 5852, 5865, 5869, 5881, 5884, 5897, 5900, 5916, 5932, &
       5948, 5963, 5968, 5978, 5997, 6008, 6023, 6026, 6039, 6053, 6056, 6067, 6071, &
       6082, 6085, 6098, 6112, 6126, 6135, 6140, 6149, 6154, 6158, 6161, 6168, 6174, &
       6182, 6187, 6205, 6209, 6213, 6317, 6339, 6342, 6366, 6381, 6391, 6489, 6962, &
       6966, 6970, 6975, 6977, 6999, 7000, 7004, 7008, 7013, 7016, 7021, 7024, 7027, &
       7029, 7032, 7038, 7043, 7046, 7049, 7069, 7072, 7076, 7081, 7084, 7089, 7099, &
       7209, 7222, 7231, 7235, 7247, 7284, 7389, 7419, 7423, 7431, 7436, 7444, 7475, &
       7549, 7584, 7665, 7666, 7831, 7836, 7853, 7865, 7888, 7912, 7950, 7972, 7980, &
       7995, 8015, 8055, 8078 /)

  ! The combined EUMETSAT/NESDIS IASI subset 616 channel list
  CHARACTER(*), PARAMETER :: IASI_SUBSET_616_COMMENT = 'Combined EUMETSAT/NESDIS IASI 616 channel SUBSET'
  INTEGER, PARAMETER :: N_IASI_SUBSET_616 = 616
  INTEGER, PARAMETER :: IASI_SUBSET_616(N_IASI_SUBSET_616) = RESHAPE((/ IASI_SUBSET_300, &
                                                                        IASI_SUBSET_316 /), &
                                                                     SHAPE=SHAPE(IASI_SUBSET_616))


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
!       Index_IASI_Subset
! 
! PURPOSE:
!       Function to index IASI channels for requested bands.
!
! CALLING SEQUENCE:
!       Error_Status = Index_IASI_Subset( Band                   , &  ! Input
!                                         Subset_List            , &  ! Input
!                                         Subset                 , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Optional output
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Bands:           The IASI band from which to subset channels.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Subset_List:     The list of IASI channels to subset. This can be a 
!                        user defined channel list, or the standard 300 or 316
!                        channel subset lists parameterised in the arrays
!                        IASI_SUBSET_300 and IASI_SUBSET_316.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Subset:          The IASI Subset structure containing the indexed
!                        subset channels for the specified IASI module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Channel_Subset_type)
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

  FUNCTION Index_IASI_Subset( Band       , &  ! Input
                              Subset_List, &  ! Input
                              Subset     , &  ! Output
                              RCS_Id     , &  ! Revision control
                              Message_Log) &  ! Error mssaging
                            RESULT( Error_Status )
    ! Arguments
    INTEGER                  , INTENT(IN)     :: Band
    INTEGER                  , INTENT(IN)     :: Subset_List(:)
    TYPE(Channel_Subset_type), INTENT(IN OUT) :: Subset
    CHARACTER(*),    OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),    OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Index_IASI_Subset'
    ! Local variables
    INTEGER :: Sorted_List(SIZE(Subset_List))
    INTEGER :: n_Subset_Channels
    INTEGER :: l, l1, l2
    INTEGER :: l_Subset, l_Extract
    INTEGER :: n_Channels
    INTEGER :: Channel

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the band
    IF ( Band < 1 .OR. Band > N_IASI_BANDS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid IASI band.', &
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
    IF ( ANY( Subset_List < 1 .OR. Subset_List > N_IASI_CHANNELS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid channel in Subset_List input.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Sort the subset list
    ! --------------------
    Sorted_List = Subset_List
    CALL InsertionSort( Sorted_List )
    
    
    ! Set the module limits
    ! ---------------------
    l1 = IASI_BAND_BEGIN_CHANNEL( Band )
    l2 = IASI_BAND_END_CHANNEL( Band )


    ! Count the channels to subset
    ! ----------------------------
    n_Channels = COUNT( Sorted_List >= l1 .AND. Sorted_List <= l2 )
    IF ( n_Channels == 0 ) RETURN


    ! Allocate the IASI Subset structure
    ! ----------------------------------
    Error_Status = Allocate_Channel_Subset( n_Channels, &
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
    l_Subset = MINLOC( Sorted_List - l1, &
                       MASK=( (Sorted_List - l1) >= 0 ), &
                       DIM =1 )

    ! Set the starting index in the MODULE channel list array.
    ! This is always 1.
    l_Extract = 1


    ! Loop over the number of channels in the current band
    ! ----------------------------------------------------
    Channel_Loop: DO l = 1, N_IASI_CHANNELS_PER_BAND( Band )

      ! Determine the current channel number
      Channel = IASI_BAND_BEGIN_CHANNEL( Band ) + l - 1

      ! Is the current channel in the subset?
      IF ( Channel == Sorted_List( l_Subset ) ) THEN

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

  END FUNCTION Index_IASI_Subset

END MODULE IASI_Subset
