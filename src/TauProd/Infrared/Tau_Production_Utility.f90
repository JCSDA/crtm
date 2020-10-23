!------------------------------------------------------------------------------
!M+
! NAME:
!       Tau_Production_Utility
!
! PURPOSE:
!       Module continaing utility routines for the LBL transmittance
!       production runs
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Tau_Production_Utility
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility routines
!
!       Message_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:      Module containing routines to perform equality
!                                   and relational comparisons on floating point
!                                   numbers.
!                                   USEs: TYPE_KINDS module
!
!       Tau_Production_Parameters:  Module defining parameters used in the LBL
!                                   transmittance production runs.
!                                   USEs: TYPE_KINDS module
!                                         LBLRTM_PARAMETERS module
!
! CONTAINS:
!       Create_Signal_File:         Function to create a "signal" file that
!                                   can be checked for in the transmittance
!                                   production scripts. The presence of a signal
!                                   file indicates the program completed
!                                   successfully.
!
!       Compute_LBL_Band:           Function to determine what LBLband a
!                                   particular frequency is within.
!
!       Compute_dF_Index:           Function to determine which frequency
!                                   interval index is being used. More than
!                                   one frequency interval is valid for the
!                                   transmittance production code -- this
!                                   utility determines which one is being used.
!
!       Compute_Frequency_Index:    Function to determine the array index of
!                                   a particular frequency value.
!
!       Find_Indices:               Function to determine the indices of the
!                                   passed logical mask array that are .TRUE.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!M-
!------------------------------------------------------------------------------


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


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Create_Signal_File
!
! PURPOSE:
!       Function to create a "signal" file that can be checked for in the
!       transmittance production scripts. The presence of a signal file
!       indicates the program completed successfully.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_Signal_File( Filename,                 &  ! Input
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Root of the name of the signal file to create. The
!                     created signal file will be this name appended with
!                     the string ".signal"
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the signal file creation was successful
!                        == FAILURE an unrecoverable error occured
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Get_Lun:            Function to return a free logical unit number for
!                           file access.
!                           SOURCE: FILE_UTILITY module
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       If the signal file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-May-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_LBL_Band
!
! PURPOSE:
!       Function to determine what LBLband a particular frequency is within.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       LBL_Band = Compute_LBL_Band( Frequency, dF ) ! Inputs
!
! INPUT ARGUMENTS:
!       Frequency:  Frequency value for which the containing LBL band
!                   is required.
!                   UNITS:      Inverse centimetres (cm^-1)
!                   TYPE:       REAL( fp_kind )
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT( IN )
!
!       dF:         LBLRTM frequency interval for the current
!                   processing.
!                   UNITS:      Inverse centimetres (cm^-1)
!                   TYPE:       REAL( fp_kind )
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       LBL_Band:   The LBL band containing the supplied frequency.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
!
! CALLS:
!       Compute_dF_Index:  Function to determine which frequency interval
!                          index is being used.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_dF_Index
!
! PURPOSE:
!       Function to determine which frequency interval index is being used.
!       More than one frequency interval is valid for the transmittance
!       production code -- this utility determines which one is being used.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       dF_Index = Compute_dF_Index( dF ) ! Input
!
! INPUT ARGUMENTS:
!       dF:         LBLRTM frequency interval for the current
!                   processing.
!                   UNITS:      Inverse centimetres (cm^-1)
!                   TYPE:       REAL( fp_kind )
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       dF_Index:   The frequency interval index currently being used.
!                   If a valid frequency interval is not detected, the
!                   return value is -1. 
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Scalar
! CALLS:
!       Compare_Float:    Function to compare floating point scalars
!                         and arrays with adjustible precision tolerance.
!                         SOURCE: COMPARE_FLOAT_NUMBERS module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Frequency_Index
!
! PURPOSE:
!       Function to determine the position index of a particular frequency
!       value for a defined frequency grid.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Frequency_Index = Compute_Frequency_Index( Begin_Frequency, &  ! Input
!                                                  dF,              &  ! Input
!                                                  Frequency        )  ! Input
!
! INPUT ARGUMENTS:
!       Begin_Frequency:  Beginning frequency of the grid.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       dF:               Frequency interval of the grid.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Frequency:        Frequency for which the position index in the
!                         grid is required.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Frequency_Index:  The position index of the requested frequency in
!                         the defined frequency grid.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Find_Indices
!
! PURPOSE:
!       Function to determine the indices of the passed logical mask array
!       that are .TRUE.
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Find_Indices( Mask,                     &  ! Input
!                                    Indices,                  &  ! Output
!                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Mask:         Logical array in which the index positions
!                     of TRUE values are to be found.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Indices:      Array of index positions for *only* the true values
!                     the input mask array
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the TRUE value index search was successful
!                        == FAILURE the number of TRUE values in the input MASK
!                                   array and the size of the output INDICES
!                                   array are different.
!                        == WARNING all of the MASK values are FALSE.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
! CALLS:
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Jun-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

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
