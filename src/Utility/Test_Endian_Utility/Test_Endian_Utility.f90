
PROGRAM Test_Endian_Utility
  

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Endian_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Endian_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Endian_Utility.f90,v 1.4 2004/11/30 20:43:04 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Dimensions
  INTEGER, PARAMETER :: N = 2


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Answer

  INTEGER :: i

  INTEGER( Short )                    :: Short_Integer_Scalar,   Swapped_Short_Integer_Scalar
  INTEGER( Long )                     :: Long_Integer_Scalar,    Swapped_Long_Integer_Scalar
  REAL( Single )                      :: Single_Real_Scalar,     Swapped_Single_Real_Scalar
  REAL( Double )                      :: Double_Real_Scalar,     Swapped_Double_Real_Scalar
  COMPLEX( Single )                   :: Single_Complex_Scalar,  Swapped_Single_Complex_Scalar
  COMPLEX( Double )                   :: Double_Complex_Scalar,  Swapped_Double_Complex_Scalar

  INTEGER( Short ),  DIMENSION( N )   :: Short_Integer_Rank1,   Swapped_Short_Integer_Rank1
  INTEGER( Long ),   DIMENSION( N )   :: Long_Integer_Rank1,    Swapped_Long_Integer_Rank1
  REAL( Single ),    DIMENSION( N )   :: Single_Real_Rank1,     Swapped_Single_Real_Rank1
  REAL( Double ),    DIMENSION( N )   :: Double_Real_Rank1,     Swapped_Double_Real_Rank1
  COMPLEX( Single ), DIMENSION( N )   :: Single_Complex_Rank1,  Swapped_Single_Complex_Rank1
  COMPLEX( Double ), DIMENSION( N )   :: Double_Complex_Rank1,  Swapped_Double_Complex_Rank1

  INTEGER( Short ),  DIMENSION( N,N ) :: Short_Integer_Rank2,   Swapped_Short_Integer_Rank2
  INTEGER( Long ),   DIMENSION( N,N ) :: Long_Integer_Rank2,    Swapped_Long_Integer_Rank2
  REAL( Single ),    DIMENSION( N,N ) :: Single_Real_Rank2,     Swapped_Single_Real_Rank2
  REAL( Double ),    DIMENSION( N,N ) :: Double_Real_Rank2,     Swapped_Double_Real_Rank2
  COMPLEX( Single ), DIMENSION( N,N ) :: Single_Complex_Rank2,  Swapped_Single_Complex_Rank2
  COMPLEX( Double ), DIMENSION( N,N ) :: Double_Complex_Rank2,  Swapped_Double_Complex_Rank2



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the Endian_Utility module routines." )' )
  WRITE( *, '(/5x, " $Revision: 1.4 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#               -- DETERMINE THE ENDIAN-NESS OF THE PLATFORM --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Current platform is " )', ADVANCE = 'NO' )

  IF ( Big_Endian() ) THEN
    WRITE( *, '( "big-endian." )' )
  ELSE
    WRITE( *, '( "little-endian." )' )
  END IF



  !#----------------------------------------------------------------------------#
  !#               -- BYTE-SWAP THE DIFFERENT SCALAR KIND TYPES --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //5x, "Testing the SCALAR calls...." )' )


  ! -----------------------
  ! Short integer data type
  ! -----------------------

  Short_Integer_Scalar         = 1_Short
  Swapped_Short_Integer_Scalar = Swap_Endian( Short_Integer_Scalar )

  WRITE( *, '( /5x, "Short integer          = ", i10, &
              &/5x, "Swapped Short integer  = ", i10 )' ) &
            Short_Integer_Scalar, Swapped_Short_Integer_Scalar


  ! -----------------------
  ! Long integer data type
  ! -----------------------

  Long_Integer_Scalar         = 1_Long
  Swapped_Long_Integer_Scalar = Swap_Endian( Long_Integer_Scalar )

  WRITE( *, '( /5x, "Long integer           = ", i10, &
              &/5x, "Swapped Long integer   = ", i10 )' ) &
            Long_Integer_Scalar, Swapped_Long_Integer_Scalar


  ! ----------------------
  ! Single float data type
  ! ----------------------

  Single_Real_Scalar         = 1.0_Single
  Swapped_Single_Real_Scalar = Swap_Endian( Single_Real_Scalar )

  WRITE( *, '( /5x, "Single real            = ", es13.6, &
              &/5x, "Swapped Single real    = ", es13.6 )' ) &
            Single_Real_Scalar, Swapped_Single_Real_Scalar


  ! ----------------------
  ! Double float data type
  ! ----------------------

  Double_Real_Scalar         = 1.0_Double
  Swapped_Double_Real_Scalar = Swap_Endian( Double_Real_Scalar )

  WRITE( *, '( /5x, "Double real            = ", es21.13e3, &
              &/5x, "Swapped Double real    = ", es21.13e3 )' ) &
            Double_Real_Scalar, Swapped_Double_Real_Scalar


  ! ------------------------
  ! Single complex data type
  ! ------------------------

  Single_Complex_Scalar         = ( 1.0_Single, -1.0_Single )
  Swapped_Single_Complex_Scalar = Swap_Endian( Single_Complex_Scalar )

  WRITE( *, '( /5x, "Single complex         = (", es13.6, ",", es13.6, ")", &
              &/5x, "Swapped Single complex = (", es13.6, ",", es13.6, ")" )' ) &
            Single_Complex_Scalar, Swapped_Single_Complex_Scalar


  ! ------------------------
  ! Double complex data type
  ! ------------------------

  Double_Complex_Scalar         = ( 1.0_Double, -1.0_Double )
  Swapped_Double_Complex_Scalar = Swap_Endian( Double_Complex_Scalar )

  WRITE( *, '( /5x, "Double complex         = (", es21.13e3, ",", es21.13e3, ")", &
              &/5x, "Swapped Double complex = (", es21.13e3, ",", es21.13e3, ")" )' ) &
            Double_Complex_Scalar, Swapped_Double_Complex_Scalar

  WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
  READ( *, '(a)' ) Answer



  !#----------------------------------------------------------------------------#
  !#               -- BYTE-SWAP THE DIFFERENT RANK-1 KIND TYPES --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //5x, "Testing the RANK-1, dim=(/",i1,"/), calls...." )' ) N


  ! -----------------------
  ! Short integer data type
  ! -----------------------

  Short_Integer_Rank1         = (/ ( Short_Integer_Scalar, i=1,N ) /)
  Swapped_Short_Integer_Rank1 = Swap_Endian( Short_Integer_Rank1 )

  WRITE( *, '( /5x, "Short integer          = ", 8i10 )' ) Short_Integer_Rank1
  WRITE( *, '(  5x, "Swapped Short integer  = ", 8i10 )' ) Swapped_Short_Integer_Rank1


  ! -----------------------
  ! Long integer data type
  ! -----------------------

  Long_Integer_Rank1         = (/ ( Long_Integer_Scalar, i=1,N ) /)
  Swapped_Long_Integer_Rank1 = Swap_Endian( Long_Integer_Rank1 )

  WRITE( *, '( /5x, "Long integer           = ", 8i10 )' ) Long_Integer_Rank1
  WRITE( *, '(  5x, "Swapped Long integer   = ", 8i10 )' ) Swapped_Long_Integer_Rank1


  ! ----------------------
  ! Single float data type
  ! ----------------------

  Single_Real_Rank1         = (/ ( Single_Real_Scalar, i=1,N ) /)
  Swapped_Single_Real_Rank1 = Swap_Endian( Single_Real_Rank1 )

  WRITE( *, '( /5x, "Single real            = ", 8(1x,es13.6) )' ) Single_Real_Rank1
  WRITE( *, '(  5x, "Swapped Single real    = ", 8(1x,es13.6) )' ) Swapped_Single_Real_Rank1


  ! ----------------------
  ! Double float data type
  ! ----------------------

  Double_Real_Rank1         = (/ ( Double_Real_Scalar, i=1,N ) /)
  Swapped_Double_Real_Rank1 = Swap_Endian( Double_Real_Rank1 )

  WRITE( *, '( /5x, "Double real            = ", 8(1x,es21.13e3) )' ) Double_Real_Rank1
  WRITE( *, '(  5x, "Swapped Double real    = ", 8(1x,es21.13e3) )' ) Swapped_Double_Real_Rank1


  ! ------------------------
  ! Single complex data type
  ! ------------------------

  Single_Complex_Rank1         = (/ ( Single_Complex_Scalar, i=1,N ) /)
  Swapped_Single_Complex_Rank1 = Swap_Endian( Single_Complex_Rank1 )

  WRITE( *, '( /5x, "Single complex         = ", 8("(", es13.6, ",", es13.6, ")", : ) )' ) &
            Single_Complex_Rank1
  WRITE( *, '(  5x, "Swapped Single complex = ", 8("(", es13.6, ",", es13.6, ")", : ) )' ) &
            Swapped_Single_Complex_Rank1


  ! ------------------------
  ! Double complex data type
  ! ------------------------

  Double_Complex_Rank1         = (/ ( Double_Complex_Scalar, i=1,N ) /)
  Swapped_Double_Complex_Rank1 = Swap_Endian( Double_Complex_Rank1 )

  WRITE( *, '( /5x, "Double complex         = ", 8("(", es21.13e3, ",", es21.13e3, ")", : ) )' ) &
            Double_Complex_Rank1
  WRITE( *, '(  5x, "Swapped Double complex = ", 8("(", es21.13e3, ",", es21.13e3, ")", : ) )' ) &
            Swapped_Double_Complex_Rank1

  WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
  READ( *, '(a)' ) Answer



  !#----------------------------------------------------------------------------#
  !#               -- BYTE-SWAP THE DIFFERENT RANK-2 KIND TYPES --              #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //5x, "Testing the RANK-2, dim=(/",i1,",",i1,"/), calls...." )' ) N, N


  ! -----------------------
  ! Short integer data type
  ! -----------------------

  Short_Integer_Rank2         = RESHAPE( (/ ( Short_Integer_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Short_Integer_Rank2 = Swap_Endian( Short_Integer_Rank2 )

  WRITE( *, '( /5x, "Short integer          = ", 2i10, /30x, 2i10, &
              &/5x, "Swapped Short integer  = ", 2i10, /30x, 2i10 )' ) &
            Short_Integer_Rank2, Swapped_Short_Integer_Rank2


  ! -----------------------
  ! Long integer data type
  ! -----------------------

  Long_Integer_Rank2         = RESHAPE( (/ ( Long_Integer_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Long_Integer_Rank2 = Swap_Endian( Long_Integer_Rank2 )

  WRITE( *, '( /5x, "Long integer           = ", 2i10, /30x, 2i10, &
              &/5x, "Swapped Long integer   = ", 2i10, /30x, 2i10 )' ) &
            Long_Integer_Rank2, Swapped_Long_Integer_Rank2


  ! ----------------------
  ! Single float data type
  ! ----------------------

  Single_Real_Rank2         = RESHAPE( (/ ( Single_Real_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Single_Real_Rank2 = Swap_Endian( Single_Real_Rank2 )

  WRITE( *, '( /5x, "Single real            = ", 2(1x,es13.6), /30x, 2(1x,es13.6), &
              &/5x, "Swapped Single real    = ", 2(1x,es13.6), /30x, 2(1x,es13.6) )' ) &
            Single_Real_Rank2, Swapped_Single_Real_Rank2


  ! ----------------------
  ! Double float data type
  ! ----------------------

  Double_Real_Rank2         = RESHAPE( (/ ( Double_Real_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Double_Real_Rank2 = Swap_Endian( Double_Real_Rank2 )

  WRITE( *, '( /5x, "Double real            = ", 2(1x,es21.13e3), /30x, 2(1x,es21.13e3), &
              &/5x, "Swapped Double real    = ", 2(1x,es21.13e3), /30x, 2(1x,es21.13e3) )' ) &
            Double_Real_Rank2, Swapped_Double_Real_Rank2


  ! ------------------------
  ! Single complex data type
  ! ------------------------

  Single_Complex_Rank2         = RESHAPE( (/ ( Single_Complex_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Single_Complex_Rank2 = Swap_Endian( Single_Complex_Rank2 )

  WRITE( *, '( /5x, "Single complex         = ", 2("(", es13.6, ",", es13.6, ")"), /30x, &
                                                &2("(", es13.6, ",", es13.6, ")"), &
              &/5x, "Swapped Single complex = ", 2("(", es13.6, ",", es13.6, ")"), /30x, &
                                                &2("(", es13.6, ",", es13.6, ")") )' ) &
            Single_Complex_Rank2, Swapped_Single_Complex_Rank2


  ! ------------------------
  ! Double complex data type
  ! ------------------------

  Double_Complex_Rank2         = RESHAPE( (/ ( Double_Complex_Scalar, i=1,N*N ) /), (/N,N/) )
  Swapped_Double_Complex_Rank2 = Swap_Endian( Double_Complex_Rank2 )

  WRITE( *, '( /5x, "Double complex         = ", 2("(", es21.13e3, ",", es21.13e3, ")"), /30x, &
                                                &2("(", es21.13e3, ",", es21.13e3, ")"), &
              &/5x, "Swapped Double complex = ", 2("(", es21.13e3, ",", es21.13e3, ")"), /30x, &
                                                &2("(", es21.13e3, ",", es21.13e3, ")") )' ) &
            Double_Complex_Rank2, Swapped_Double_Complex_Rank2

END PROGRAM Test_Endian_Utility



