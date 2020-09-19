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
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE IASI_Define    , ONLY: N_IASI_BANDS, N_IASI_CHANNELS, &
                             IASI_BeginChannel, &
                             IASI_EndChannel
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
  PUBLIC :: IASI_SUBSET_300_COMMENT, N_IASI_SUBSET_300, IASI_SUBSET_300
  PUBLIC :: IASI_SUBSET_316_COMMENT, N_IASI_SUBSET_316, IASI_SUBSET_316
  PUBLIC :: IASI_SUBSET_616_COMMENT, N_IASI_SUBSET_616, IASI_SUBSET_616
  PUBLIC :: N_IASI_VALID_SUBSETS, IASI_VALID_SUBSET_NAME
  ! Procedures
  PUBLIC :: IASI_Subset_Index
  PUBLIC :: IASI_SubsetVersion


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256

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
  INTEGER,      PARAMETER :: N_IASI_SUBSET_616 = 616
  INTEGER,      PARAMETER :: IASI_SUBSET_616(N_IASI_SUBSET_616) = &
    RESHAPE((/ IASI_SUBSET_300, IASI_SUBSET_316 /), SHAPE=SHAPE(IASI_SUBSET_616))


  ! The list of supported channel subsets
  INTEGER,      PARAMETER :: N_IASI_VALID_SUBSETS = 5
  CHARACTER(*), PARAMETER :: IASI_VALID_SUBSET_NAME(N_IASI_VALID_SUBSETS) = &
    (/ 'EUMETSAT 300 channel set                ', &
       'NESDIS 316 channel set                  ', &
       'Combined EUMETSAT/NESDIS 616 channel set', &
       'All channels                            ', &
       'User specified                          ' /)


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
!       IASI_Subset_Index
! 
! PURPOSE:
!       Function to index IASI channels for requested bands.
!
! CALLING SEQUENCE:
!       Error_Status = IASI_Subset_Index( Band       , &  ! Input
!                                         Subset_List, &  ! Input
!                                         Subset       )  ! Output
!
! INPUTS:
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
! OUTPUTS:
!       Subset:          The IASI Subset structure containing the indexed
!                        subset channels for the specified IASI module.
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

  FUNCTION IASI_Subset_Index( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'IASI_Subset_Index'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_subset_channels
    INTEGER :: ch1, ch2, i

    ! Set up
    err_stat = SUCCESS
    ! ...Check the band
    IF ( Band < 1 .OR. Band > N_IASI_BANDS ) THEN
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
    IF ( ANY( Subset_List < 1 .OR. Subset_List > N_IASI_CHANNELS ) ) THEN
      msg = 'Invalid channel in Subset_List input.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Set the band limits
    ch1 = IASI_BeginChannel( Band )
    ch2 = IASI_EndChannel( Band )


    ! Generate the subset
    CALL Subset_Generate( Subset, (/(i,i=ch1,ch2)/), Subset_List )
    IF ( .NOT. Subset_Associated( Subset ) ) THEN
      msg = 'Error generating Subset structure.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF

  END FUNCTION IASI_Subset_Index

  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       IASI_SubsetVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL IASI_SubsetVersion( Id )
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

  SUBROUTINE IASI_SubsetVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE IASI_SubsetVersion

END MODULE IASI_Subset
