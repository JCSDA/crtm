!
! CrIS_Subset
!
! Module containing CrIS channel subsetting routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-May-2011
!                       paul.vandelst@noaa.gov
!

MODULE CrIS_Subset

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CrIS_Define    , ONLY: N_CRIS_BANDS, N_CRIS_CHANNELS, &
                             CrIS_BeginChannel, &
                             CrIS_EndChannel
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
  PUBLIC :: CRIS_SUBSET_374_COMMENT, N_CRIS_SUBSET_374, CRIS_SUBSET_374
  PUBLIC :: CRIS_SUBSET_399_COMMENT, N_CRIS_SUBSET_399, CRIS_SUBSET_399
  PUBLIC :: N_CRIS_VALID_SUBSETS, CRIS_VALID_SUBSET_NAME
  ! Procedures
  PUBLIC :: CrIS_Subset_Index
  PUBLIC :: CrIS_SubsetVersion


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256

  ! The NESDIS CrIS subset 374 channel list
  CHARACTER(*), PARAMETER :: CRIS_SUBSET_374_COMMENT = 'NESDIS CrIS 374 channel SUBSET'
  INTEGER, PARAMETER :: N_CRIS_SUBSET_374 = 374
  INTEGER, PARAMETER :: CRIS_SUBSET_374(N_CRIS_SUBSET_374) = &
    (/   27,   28,   31,   32,   33,   37,   49,   51,   53,   59,   61,   63,   64, &
         65,   67,   69,   71,   73,   75,   79,   80,   81,   83,   85,   87,   88, &
         89,   93,   95,   96,   99,  101,  102,  104,  106,  107,  111,  113,  116, &
        120,  123,  124,  125,  126,  130,  132,  133,  136,  137,  138,  142,  143, &
        144,  145,  147,  148,  150,  151,  153,  154,  155,  157,  158,  159,  160, &
        161,  162,  163,  164,  165,  166,  168,  170,  171,  173,  175,  176,  181, &
        183,  208,  216,  224,  228,  236,  238,  242,  243,  248,  266,  268,  278, &
        283,  300,  311,  317,  330,  333,  334,  338,  340,  341,  349,  352,  358, &
        361,  364,  366,  367,  368,  378,  390,  391,  394,  395,  396,  397,  398, &
        399,  427,  433,  447,  464,  473,  481,  484,  506,  528,  556,  557,  558, &
        560,  561,  562,  564,  565,  566,  569,  573,  574,  577,  580,  581,  584, &
        585,  587,  590,  591,  594,  597,  598,  601,  604,  607,  611,  614,  616, &
        617,  619,  622,  626,  628,  634,  637,  638,  640,  641,  642,  644,  646, &
        647,  650,  651,  652,  654,  655,  657,  659,  663,  667,  670,  707,  709, &
        713,  716,  730,  735,  736,  739,  743,  744,  746,  748,  751,  754,  755, &
        756,  757,  758,  760,  761,  762,  763,  766,  767,  768,  771,  772,  773, &
        776,  777,  778,  779,  780,  782,  783,  784,  785,  786,  787,  788,  789, &
        790,  791,  792,  794,  796,  798,  800,  802,  803,  804,  806,  807,  808, &
        809,  811,  812,  814,  816,  819,  820,  821,  822,  823,  824,  825,  826, &
        827,  828,  829,  830,  831,  832,  833,  834,  835,  836,  838,  839,  840, &
        842,  843,  844,  845,  846,  847,  848,  849,  850,  851,  852,  853,  854, &
        856,  861,  862,  864,  865,  866,  867,  869,  871,  872,  874,  876,  878, &
        879,  880,  884,  886,  887,  888,  889,  890,  900,  921,  924,  927,  945, &
        991,  994, 1007, 1015, 1030, 1094, 1106, 1130, 1132, 1133, 1135, 1142, 1147, &
       1148, 1149, 1150, 1151, 1152, 1153, 1154, 1155, 1156, 1157, 1158, 1159, 1160, &
       1161, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1170, 1171, 1172, 1173, &
       1174, 1175, 1177, 1178, 1179, 1180, 1181, 1187, 1202, 1204, 1206, 1208, 1218, &
       1226, 1231, 1234, 1235, 1236, 1237, 1238, 1239, 1241, 1242, 1243, 1244, 1245, &
       1247, 1248, 1250, 1271, 1282, 1285, 1290, 1295, 1298, 1301 /)

  ! The NESDIS CrIS subset 399 channel list
  CHARACTER(*), PARAMETER :: CRIS_SUBSET_399_COMMENT = 'NESDIS CrIS 399 channel SUBSET'
  INTEGER, PARAMETER :: N_CRIS_SUBSET_399 = 399
  INTEGER, PARAMETER :: CRIS_SUBSET_399(N_CRIS_SUBSET_399) = &
    (/   27,   28,   31,   32,   33,   37,   49,   51,   53,   59,   61,   63,   64, &
         65,   67,   69,   71,   73,   75,   79,   80,   81,   83,   85,   87,   88, &
         89,   93,   95,   96,   99,  101,  102,  104,  106,  107,  111,  113,  116, &
        120,  123,  124,  125,  126,  130,  132,  133,  136,  137,  138,  142,  143, &
        144,  145,  147,  148,  150,  151,  153,  154,  155,  157,  158,  159,  160, &
        161,  162,  163,  164,  165,  166,  168,  170,  171,  173,  175,  181,  183, &
        198,  208,  211,  216,  224,  228,  236,  238,  242,  248,  266,  268,  279, &
        283,  311,  317,  330,  333,  334,  338,  340,  341,  342,  349,  352,  358, &
        361,  364,  366,  367,  368,  378,  390,  391,  392,  394,  395,  396,  397, &
        398,  399,  404,  427,  447,  464,  473,  482,  484,  501,  529,  556,  557, &
        558,  560,  561,  562,  564,  565,  566,  569,  573,  574,  577,  580,  581, &
        584,  585,  587,  590,  591,  594,  597,  598,  601,  604,  607,  611,  614, &
        616,  617,  619,  622,  626,  628,  634,  637,  638,  640,  641,  642,  644, &
        646,  647,  650,  651,  652,  654,  655,  657,  659,  663,  667,  670,  707, &
        710,  713,  716,  730,  735,  736,  739,  743,  744,  746,  748,  751,  754, &
        755,  756,  757,  758,  760,  761,  762,  763,  766,  767,  768,  771,  772, &
        773,  776,  777,  778,  779,  780,  782,  783,  784,  785,  786,  787,  788, &
        789,  790,  791,  792,  794,  796,  798,  800,  802,  803,  804,  806,  807, &
        808,  809,  811,  812,  814,  816,  819,  820,  821,  822,  823,  824,  825, &
        826,  827,  828,  829,  830,  831,  832,  833,  834,  835,  836,  838,  839, &
        840,  842,  843,  844,  845,  846,  847,  848,  849,  850,  851,  852,  853, &
        854,  856,  861,  862,  864,  865,  866,  867,  869,  871,  872,  874,  876, &
        878,  879,  880,  884,  886,  887,  888,  889,  890,  900,  921,  924,  927, &
        945,  991,  994, 1007, 1015, 1030, 1094, 1106, 1130, 1132, 1133, 1135, 1142, &
       1147, 1148, 1149, 1150, 1151, 1152, 1153, 1154, 1155, 1156, 1157, 1158, 1159, &
       1160, 1161, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1170, 1171, 1172, &
       1173, 1174, 1175, 1177, 1178, 1179, 1180, 1181, 1187, 1189, 1190, 1192, 1193, &
       1194, 1196, 1197, 1198, 1199, 1200, 1202, 1203, 1204, 1206, 1207, 1208, 1210, &
       1212, 1214, 1215, 1217, 1218, 1220, 1222, 1224, 1226, 1228, 1229, 1231, 1232, &
       1234, 1235, 1236, 1237, 1238, 1239, 1241, 1242, 1243, 1244, 1245, 1247, 1250, &
       1270, 1271, 1282, 1285, 1288, 1290, 1293, 1298, 1301 /)
  
  ! The list of supported channel subsets
  INTEGER,      PARAMETER :: N_CRIS_VALID_SUBSETS = 4
  CHARACTER(*), PARAMETER :: CRIS_VALID_SUBSET_NAME(N_CRIS_VALID_SUBSETS) = &
    (/ '374 channel subset', &
       '399 channel subset', &
       'All channels      ', &
       'User specified    ' /)


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
!       CrIS_Subset_Index
! 
! PURPOSE:
!       Function to index CrIS channels for requested bands.
!
! CALLING SEQUENCE:
!       Error_Status = CrIS_Subset_Index( Band       , &  ! Input
!                                         Subset_List, &  ! Input
!                                         Subset       )  ! Output
!
! INPUTS:
!       Band:            The CrIS band from which to subset channels.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Subset_List:     The list of CrIS channels to subset. This can be a 
!                        user defined channel list, or the standard 374
!                        channel subset list parameterised in the array
!                        CRIS_SUBSET_374.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Subset:          The CrIS Subset structure containing the indexed
!                        subset channels for the specified CrIS module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(Subset_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
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

  FUNCTION CrIS_Subset_Index( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CrIS_Subset_Index'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_subset_channels
    INTEGER :: i, ch1, ch2

    ! Set up
    err_stat = SUCCESS
    ! ...Check the band
    IF ( Band < 1 .OR. Band > N_CRIS_BANDS ) THEN
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
    IF ( ANY( Subset_List < 1 .OR. Subset_List > N_CRIS_CHANNELS ) ) THEN
      msg = 'Invalid channel in Subset_List input.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Set the band limits
    ch1 = CrIS_BeginChannel( Band )
    ch2 = CrIS_EndChannel( Band )


    ! Generate the subset
    CALL Subset_Generate( Subset, (/(i,i=ch1,ch2)/), Subset_List )
    IF ( .NOT. Subset_Associated( Subset ) ) THEN
      msg = 'Error generating Subset structure.'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF

  END FUNCTION CrIS_Subset_Index


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CrIS_SubsetVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CrIS_SubsetVersion( Id )
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

  SUBROUTINE CrIS_SubsetVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CrIS_SubsetVersion

END MODULE CrIS_Subset
