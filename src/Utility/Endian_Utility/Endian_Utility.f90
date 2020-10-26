
MODULE Endian_Utility


  ! ----------
  ! Module use
  ! ----------
 
  USE Type_Kinds

 
  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Big_Endian
  PUBLIC :: Swap_Endian

          
  ! ------------------
  ! Overload interface
  ! ------------------

  INTERFACE Swap_Endian
    MODULE PROCEDURE Swap_Short_Integer
    MODULE PROCEDURE Swap_Long_Integer
    MODULE PROCEDURE Swap_LLong_Integer
    MODULE PROCEDURE Swap_Single_Float
    MODULE PROCEDURE Swap_Double_Float
    MODULE PROCEDURE Swap_Single_Complex
    MODULE PROCEDURE Swap_Double_Complex
  END INTERFACE Swap_Endian


CONTAINS




  FUNCTION Big_Endian()


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Short ) :: Source = 1_Short


    ! ------------
    ! The function
    ! ------------

    LOGICAL :: Big_Endian


    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC TRANSFER, ICHAR


    ! ----------------------------------
    ! Initialise result to little-endian
    ! ----------------------------------

    Big_Endian = .FALSE.


    ! ------------------------------------------------------------
    ! Test for "endian-ness".
    !
    ! TRANSFER( source, 'a' ) returns a result with the physical
    !   representation of the number 1, i.e. an integer, but
    !   interpreted as a character (the type of 'a' - a character,
    !   not the value, is what is important).
    !
    ! IACHAR returns the position of a character in the ASCII
    !   collating sequence associated with the kind type parameter
    !   of the character.
    ! ------------------------------------------------------------

    IF ( IACHAR( TRANSFER( Source, 'a' ) ) == 0 ) Big_Endian = .TRUE.

  END FUNCTION Big_Endian






  ELEMENTAL FUNCTION Swap_Short_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( Short ), INTENT( IN ) :: Input
    INTEGER( Short )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Short


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Short_Integer


  ELEMENTAL FUNCTION Swap_Long_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( Long ), INTENT( IN ) :: Input
    INTEGER( Long )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Long


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Long_Integer


  ELEMENTAL FUNCTION Swap_LLong_Integer ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    INTEGER( LLong ), INTENT( IN ) :: Input
    INTEGER( LLong )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_LLong


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_LLong_Integer


  ELEMENTAL FUNCTION Swap_Single_Float ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    REAL( Single ), INTENT( IN ) :: Input
    REAL( Single )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Single


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Single_Float


  ELEMENTAL FUNCTION Swap_Double_Float ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    REAL( Double ), INTENT( IN ) :: Input
    REAL( Double )               :: Output


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER, PARAMETER :: N = n_Bytes_Double


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER( Byte ), DIMENSION( N ) :: Byte_Equivalent


    ! ------------------------------------------------
    ! Byte swap the data. The extra step in the middle
    ! is necessary for those compilers that can't
    ! handle a negative strided input to TRANSFER
    ! ------------------------------------------------

    Byte_Equivalent = TRANSFER( Input, Byte_Equivalent )
    Byte_Equivalent = Byte_Equivalent( N:1:-1 )
    Output          = TRANSFER( Byte_Equivalent, Output )

  END FUNCTION Swap_Double_Float


  ELEMENTAL FUNCTION Swap_Single_Complex ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    COMPLEX( Single ), INTENT( IN ) :: Input
    COMPLEX( Single )               :: Output


    ! ------------------
    ! Byte-swap the data
    ! ------------------

    Output = CMPLX( Swap_Endian( REAL( Input,          Single ) ), &   ! Real
                    Swap_Endian( REAL( AIMAG( Input ), Single ) ), &   ! Imaginary
                    Single )

  END FUNCTION Swap_Single_Complex


  ELEMENTAL FUNCTION Swap_Double_Complex ( Input ) RESULT ( Output )


    ! -------------------
    ! Argument and result
    ! -------------------

    COMPLEX( Double ), INTENT( IN ) :: Input
    COMPLEX( Double )               :: Output


    ! ------------------
    ! Byte-swap the data
    ! ------------------

    Output = CMPLX( Swap_Endian( REAL( Input,          Double ) ), &   ! Real
                    Swap_Endian( REAL( AIMAG( Input ), Double ) ), &   ! Imaginary
                    Double )


  END FUNCTION Swap_Double_Complex

END MODULE Endian_Utility



