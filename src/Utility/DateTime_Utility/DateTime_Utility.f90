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
  USE Date_Utility, ONLY: N_MONTHS, &
                          DAYS_PER_MONTH_IN_NONLEAP, &
                          MONTH_NAME, &
                          N_DAYS, &
                          DAY_NAME, &
                          Date_Is_Leap_Year  => Is_Leap_Year, &
                          Date_Day_of_Year   => Day_of_Year, &
                          Date_Days_in_Month => Days_in_Month
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Datatpye
  PUBLIC :: DateTime_type
  ! Procedures
  PUBLIC :: Get_DateTime
  PUBLIC :: Is_Leap_Year
  PUBLIC :: Day_of_Year
  PUBLIC :: Days_in_Month


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Is_Leap_Year
    MODULE PROCEDURE Date_Is_Leap_Year
    MODULE PROCEDURE DateTime_Is_Leap_Year
  END INTERFACE Is_Leap_Year

  INTERFACE Day_of_Year
    MODULE PROCEDURE Date_Day_of_Year
    MODULE PROCEDURE DateTime_Day_of_Year
  END INTERFACE Day_of_Year

  INTERFACE Days_in_Month
    MODULE PROCEDURE Date_Days_in_Month
    MODULE PROCEDURE DateTime_Days_in_Month
  END INTERFACE Days_in_Month


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'

  
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
  END TYPE DateTime_type
  !:tdoc-:


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Get_DateTime
!
! PURPOSE:
!       Function to return a DateTime structure with the current
!       date and time
!
! CALLING SEQUENCE:
!       DateTime = Get_DateTime()
!
! FUNCTION RESULT:
!       DateTime:  DateTime structure containing current date and time.
!                  UNITS:      N/A
!                  TYPE:       DateTime_type
!                  DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Get_DateTime() RESULT(DateTime)
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
    DateTime%DoY = Day_of_Year( DateTime )
  END FUNCTION Get_DateTime

  
!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Is_Leap_Year
!
! PURPOSE:
!       Elemental function to determine if a specified DateTime structure
!       is for a leap year.
!
! CALLING SEQUENCE:
!       Result = Is_Leap_Year( DateTime )
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

  ELEMENTAL FUNCTION DateTime_Is_Leap_Year( DateTime ) RESULT( Is_Leap_Year )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    LOGICAL :: Is_Leap_Year
    Is_Leap_Year = Date_Is_Leap_Year( DateTime%Year )
  END FUNCTION DateTime_Is_Leap_Year


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Day_of_Year
!
! PURPOSE:
!       Elemental function to determine the day-of-year value for a
!       a DateTime structure.
!
! CALLING SEQUENCE:
!       DoY = Day_of_Year( DateTime )
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

  ELEMENTAL FUNCTION DateTime_Day_of_Year( DateTime ) RESULT( DoY )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: DoY
    DoY = Date_Day_of_Year( DateTime%Day, DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_Day_of_Year


!------------------------------------------------------------------------------
!:sdoc+:
! NAME:
!       Days_in_Month
!
! PURPOSE:
!       Elemental function to return the number of days in a given
!       month and year.
!
! CALLING SEQUENCE:
!       n_Days = Days_in_Month( DateTime )
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

  ELEMENTAL FUNCTION DateTime_Days_in_Month( DateTime ) RESULT( n_Days )
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    INTEGER :: n_Days
    n_Days = Date_Days_in_Month( DateTime%Month, DateTime%Year )
  END FUNCTION DateTime_Days_in_Month

END MODULE DateTime_Utility
