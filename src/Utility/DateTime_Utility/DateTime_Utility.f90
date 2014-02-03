!
! DateTime_Utility
!
! Module defining the DateTime structure and utility routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 14-Sep-2007
!                       paul.vandelst@noaa.gov
!

MODULE DateTime_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Date_Utility, ONLY: Date_IsLeapYear  => IsLeapYear , &
                          Date_DayOfYear   => DayOfYear  , &
                          Date_DaysInMonth => DaysInMonth, &
                          Date_NameOfMonth => NameOfMonth
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Datatpye
  PUBLIC :: DateTime_type
  ! Procedures
  PUBLIC :: DateTime_Now
  PUBLIC :: IsLeapYear
  PUBLIC :: DayOfYear
  PUBLIC :: DaysInMonth
  PUBLIC :: NameOfMonth


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE IsLeapYear
    MODULE PROCEDURE Date_IsLeapYear
    MODULE PROCEDURE DateTime_IsLeapYear
  END INTERFACE IsLeapYear

  INTERFACE DayOfYear
    MODULE PROCEDURE Date_DayOfYear
    MODULE PROCEDURE DateTime_DayOfYear
  END INTERFACE DayOfYear

  INTERFACE DaysInMonth
    MODULE PROCEDURE Date_DaysInMonth
    MODULE PROCEDURE DateTime_DaysInMonth
  END INTERFACE DaysInMonth

  INTERFACE NameOfMonth
    MODULE PROCEDURE Date_NameOfMonth
    MODULE PROCEDURE DateTime_NameOfMonth
  END INTERFACE NameOfMonth


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  INTEGER, PARAMETER :: NL = 20


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  !:tdoc+:
  TYPE :: DateTime_type
    INTEGER :: Year        = 0
    INTEGER :: Month       = 0
    INTEGER :: Day         = 0
    INTEGER :: UTC_Delta   = 0
    INTEGER :: Hour        = 0
    INTEGER :: Minute      = 0
    INTEGER :: Second      = 0
    INTEGER :: Millisecond = 0
    INTEGER :: DoY         = 0
    CHARACTER(NL) :: Month_Name
  END TYPE DateTime_type
  !:tdoc-:


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DateTime_Now
!
! PURPOSE:
!       Function to return a DateTime structure with the current
!       date and time
!
! CALLING SEQUENCE:
!       DateTime = DateTime_Now()
!
! FUNCTION RESULT:
!       DateTime:  DateTime structure containing current date and time.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION DateTime_Now() RESULT(DateTime)
    TYPE(DateTime_type) :: DateTime
    INTEGER :: Values(8)
    CALL DATE_AND_TIME(VALUES=Values)
    DateTime%Year        = Values(1)
    DateTime%Month       = Values(2)
    DateTime%Day         = Values(3)
    DateTime%UTC_Delta   = Values(4)
    DateTime%Hour        = Values(5)
    DateTime%Minute      = Values(6)
    DateTime%Second      = Values(7)
    DateTime%Millisecond = Values(8)
    DateTime%DoY        = DayOfYear( DateTime )
    DateTime%Month_Name = NameOfMonth( DateTime )
  END FUNCTION DateTime_Now


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       IsLeapYear
!
! PURPOSE:
!       Elemental function to determine if a specified DateTime structure
!       is for a leap year.
!
! CALLING SEQUENCE:
!       Result = IsLeapYear( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Result:    The return value is a logical value indicating whether
!                  the specified year is a leap year.
!                  If .TRUE.  the specified year IS a leap year.
!                     .FALSE. the specified year is NOT a leap year
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as input
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_IsLeapYear( DateTime ) RESULT( IsLeapYear )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    LOGICAL :: IsLeapYear
    IsLeapYear = Date_IsLeapYear( DateTime%Year )
  END FUNCTION DateTime_IsLeapYear


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DayOfYear
!
! PURPOSE:
!       Elemental function to determine the day-of-year value for a
!       a DateTime structure.
!
! CALLING SEQUENCE:
!       DoY = DayOfYear( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       DoY:       Integer defining the day-of-year.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_DayOfYear( DateTime ) RESULT( DoY )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: DoY
    DoY = Date_DayOfYear( DateTime%Day, DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_DayOfYear


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       DaysInMonth
!
! PURPOSE:
!       Elemental function to return the number of days in a given
!       month and year.
!
! CALLING SEQUENCE:
!       n_Days = DaysInMonth( DateTime )
!
! INPUTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Days:    The number of days in the month.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Same as input
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_DaysInMonth( DateTime ) RESULT( n_Days )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: n_Days
    n_Days = Date_DaysInMonth( DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_DaysInMonth


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       NameOfMonth
!
! PURPOSE:
!       Elemental function to return the name of the month.
!
! CALLING SEQUENCE:
!       name = NameOfMonth( DateTime )
!
! INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       name:      The return value is a character string containing the
!                  name of the month.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Conformable with input DateTime arugment
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION DateTime_NameOfMonth( DateTime ) RESULT( name )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    CHARACTER(NL) :: name
    name = Date_NameOfMonth( DateTime%Month )
  END FUNCTION DateTime_NameOfMonth

END MODULE DateTime_Utility
