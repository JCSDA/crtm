

MODULE Tau_Production_Utility


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers

  USE Tau_Production_Parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Create_Signal_File
  PUBLIC :: Compute_LBL_Band
  PUBLIC :: Compute_dF_Index
  PUBLIC :: Compute_Frequency_Index
  PUBLIC :: Find_Indices

  INTERFACE Compute_LBL_Band 
    MODULE PROCEDURE Compute_LBL_Band_Real  
    MODULE PROCEDURE Compute_LBL_Band_Int 
  END INTERFACE Compute_LBL_Band 

CONTAINS




  FUNCTION Create_Signal_File( Filename,     &  ! Input
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),           INTENT( IN ) :: Filename
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_Signal_File'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: FileID
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                      -- CREATE THE SIGNAL FILE --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a free unit number
    ! ----------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit for '//&
                            TRIM( Filename )//' signal file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = TRIM( Filename )//'.signal', &
                  STATUS = 'REPLACE', &
                  FORM   = 'FORMATTED', &
                  ACCESS = 'SEQUENTIAL', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//&
                            TRIM( Filename )//' signal file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------------
    ! Write the program filename to the signal file
    ! ---------------------------------------------

    WRITE( FileID, FMT    = '( a )',  &
                   IOSTAT = IO_Status ) Filename

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing to '//&
                            TRIM( Filename )//' signal file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = 'DELETE' )
    END IF


    ! ---------------------
    ! Close the signal file
    ! ---------------------

    CLOSE( FileID, STATUS = 'KEEP' )

  END FUNCTION Create_Signal_File






  FUNCTION Compute_LBL_Band_Real( Frequency, dF ) &  ! Input
                           RESULT ( LBL_Band )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: Frequency
    REAL( fp_kind ), INTENT( IN ) :: dF

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: LBL_Band


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n, dF_Index
    REAL( fp_kind ) :: Numerator
    REAL( fp_kind ) :: Denominator



    !#--------------------------------------------------------------------------#
    !#                    -- DEFINE AN INVALID LBL BAND --                      #
    !#--------------------------------------------------------------------------#

    LBL_Band = -1



    !#--------------------------------------------------------------------------#
    !#               -- DETERMINE THE FREQUENCY INTERVAL INDEX --               #
    !#--------------------------------------------------------------------------#

    dF_Index = Compute_dF_Index( dF )

    ! -- Check the result
    IF ( dF_Index < 0 ) RETURN



    !#--------------------------------------------------------------------------#
    !#                       -- COMPUTE THE LBL BAND --                         #
    !#--------------------------------------------------------------------------#

    Numerator   = Frequency - FREQUENCY_BEGIN
    Denominator = FREQUENCY_BANDWIDTH(dF_Index) + FREQUENCY_INTERVAL(dF_Index)

    LBL_Band = INT( Numerator / Denominator ) + 1

  END FUNCTION Compute_LBL_Band_Real

  FUNCTION Compute_LBL_Band_Int( Frequency, dF_Index ) &  ! Input
                           RESULT ( LBL_Band )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: Frequency
    INTEGER,         INTENT( IN ) :: dF_Index

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: LBL_Band


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n
    REAL( fp_kind ) :: Numerator
    REAL( fp_kind ) :: Denominator



    !#--------------------------------------------------------------------------#
    !#                    -- DEFINE AN INVALID LBL BAND --                      #
    !#--------------------------------------------------------------------------#

    LBL_Band = -1



    !#--------------------------------------------------------------------------#
    !#               -- DETERMINE THE FREQUENCY INTERVAL INDEX --               #
    !#--------------------------------------------------------------------------#

    ! -- Check the result
    IF ( dF_Index < 0 ) RETURN



    !#--------------------------------------------------------------------------#
    !#                       -- COMPUTE THE LBL BAND --                         #
    !#--------------------------------------------------------------------------#

    Numerator   = Frequency - FREQUENCY_BEGIN
    Denominator = FREQUENCY_BANDWIDTH(dF_Index) + FREQUENCY_INTERVAL(dF_Index)

    LBL_Band = INT( Numerator / Denominator ) + 1
 
  END FUNCTION Compute_LBL_Band_Int






  FUNCTION Compute_dF_Index( dF ) RESULT ( dF_Index )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: dF


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: dF_Index


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                    -- DEFINE AN INVALID dF_Index --                      #
    !#--------------------------------------------------------------------------#

    dF_Index = -1



    !#--------------------------------------------------------------------------#
    !#               -- DETERMINE THE FREQUENCY INTERVAL INDEX --               #
    !#--------------------------------------------------------------------------#

    ! -- Compare the numbers. Adjust ULP as required.
    Index_Search: DO n = 1, N_FREQUENCY_INTERVALS
      IF ( Compare_Float( dF, FREQUENCY_INTERVAL(n), ULP = 10000 ) ) THEN
        dF_Index = n
        EXIT Index_Search
      END IF
    END DO Index_Search

  END FUNCTION Compute_dF_index






  FUNCTION Compute_Frequency_Index( Begin_Frequency, &  ! Input
                                    dF,              &  ! Input
                                    Frequency )      &  ! Input
                                  RESULT ( Frequency_Index )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: Begin_Frequency
    REAL( fp_kind ), INTENT( IN ) :: dF
    REAL( fp_kind ), INTENT( IN ) :: Frequency


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Frequency_Index


    ! ----------------
    ! Local parameters
    ! ----------------

    REAL( fp_kind ), PARAMETER :: ONEpointFIVE = 1.5_fp_kind



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE THE POSITION INDEX --                     #
    !#--------------------------------------------------------------------------#

    Frequency_Index = INT( ( ( Frequency - Begin_Frequency ) / dF ) + ONEpointFIVE )

  END FUNCTION Compute_Frequency_Index






  FUNCTION Find_Indices( Mask,         &
                         Indices,      &
                         Message_Log ) &
                       RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    LOGICAL, DIMENSION( : ),  INTENT( IN )  :: Mask

    ! -- Output
    INTEGER, DIMENSION( : ),  INTENT( OUT ) :: Indices

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Find_Indices'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    n = COUNT( Mask )

    ! -- Issue warning if mask is all FALSE
    IF ( n == 0 ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'All MASK values are .FALSE.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Are the sizes consistent?
    IF ( n /= SIZE( Indices ) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Number of .TRUE. MASK values, ", i4, &
                        &", is different from the size of", &
                        &" the INDICES array, ", i4, "." )' ) &
                      n, SIZE( Indices )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- FIND THE .TRUE. VALUE INDICES --                  #
    !#--------------------------------------------------------------------------#

    n = 0
    DO i = 1, SIZE( Mask )
      IF ( Mask( i ) ) THEN
        n = n + 1
        Indices( n ) = i
      END IF
    END DO

  END FUNCTION Find_Indices

END MODULE Tau_Production_Utility
