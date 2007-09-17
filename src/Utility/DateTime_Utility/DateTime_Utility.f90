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


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! Number of Months in a Year
  INTEGER, PARAMETER :: N_MONTHS = 12
  ! Days per Month in a non leap Year
  INTEGER, PARAMETER :: DAYS_PER_MONTH_IN_NONLEAP(N_MONTHS) = &
  (/ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /)
  ! Month names
  CHARACTER(*), PARAMETER :: MONTH_NAME(N_MONTHS) = &
  (/'January  ','February ','March    ','April    ','May      ','June     ', &
    'July     ','August   ','September','October  ','November ','December ' /)

  ! Number of Days in a Week
  INTEGER, PARAMETER :: N_DAYS = 7
  ! Day names
  CHARACTER(*),PARAMETER :: DAY_NAME(N_DAYS) = &
  (/'Sunday   ','Monday   ','Tuesday  ','Wednesday','Thursday ','Friday   ','Saturday '/)
  
  
  ! -----------------------
  ! Derived type definition
  ! -----------------------
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


CONTAINS


!------------------------------------------------------------------------------
!
! NAME:
!       Get_DateTime
!
! PURPOSE:
!       Routine to return a DateTime structure with the current date and time
!
! CALLING SEQUENCE:
!       DateTime = Get_DateTime()
!
! OPTIONAL INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       TYPE(DateTime_type)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Year:      Integer specifying the Year in the format YYYY.
!                  This argument is ignored if the DateTime argument
!                  is also specified.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       DateTime:  DateTime structure containing current date and time.
!                  UNITS:      N/A
!                  TYPE:       TYPE(DateTime_type)
!                  DIMENSION:  Scalar
!
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
!
! NAME:
!       Is_Leap_Year
!
! PURPOSE:
!       Routine to determine if a specified year is a leap year.
!
! CALLING SEQUENCE:
!       Result = Is_Leap_Year( DateTime=DateTime, & ! Optional input
!                              Year    =Year      ) ! Optional input
!
! OPTIONAL INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       TYPE(DateTime_type)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Year:      Integer specifying the Year in the format YYYY.
!                  This argument is ignored if the DateTime argument
!                  is also specified.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Result:    The return value is a logical value indicating whether
!                  the specified year is a leap year.
!                  If .TRUE.  the specified year IS a leap year.
!                     .FALSE. - the specified year is NOT a leap year, or
!                             - no date or year was specified.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Is_Leap_Year( DateTime, Year )
    ! Arguments
    TYPE(DateTime_type), OPTIONAL, INTENT(IN) :: DateTime
    INTEGER,             OPTIONAL, INTENT(IN) :: Year
    ! Function result
    LOGICAL :: Is_Leap_Year
    ! Local variables
    INTEGER :: lYear
    
    ! Set up
    Is_Leap_Year = .FALSE.
    IF ( PRESENT(DateTime) ) THEN
      lYear = DateTime%Year
    ELSEIF ( PRESENT(Year) ) THEN
      lYear = Year
    ELSE
      RETURN
    END IF

    ! Determine leap year-ness
    IF ( ( (MOD(lYear,4) == 0) .AND. (MOD(lYear,100) /= 0) ) .OR. &
         (MOD(lYear,400) == 0) ) Is_Leap_Year =  .TRUE.
  END FUNCTION Is_Leap_Year


!------------------------------------------------------------------------------
!
! NAME:
!       Day_of_Year
!
! PURPOSE:
!       Routine to determine the day-of-year value given a DateTime structure.
!
! CALLING SEQUENCE:
!       DoY = Day_of_Year( DateTime )
!
! INPUT ARGUMENTS:
!       DateTime:  DateTime structure containing date information.
!                  UNITS:      N/A
!                  TYPE:       TYPE(DateTime_type)
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       DoY:       Integer defining the day-of-year.
!                  Return value is 0 for invalid input.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Day_of_Year(DateTime) RESULT(DoY)
    ! Arguments
    TYPE(DateTime_type), INTENT(IN) :: DateTime
    ! Function result
    INTEGER :: DoY
    ! Local variables
    INTEGER :: Days_per_Month(N_MONTHS)

    ! Set up
    ! ------
    DoY = 0
    
    ! Check year and month input
    IF ( DateTime%Year  < 1 .OR. &
         DateTime%Month < 1 .OR. &
         DateTime%Month > N_MONTHS ) RETURN

    ! Check the day of month
    Days_per_Month = DAYS_PER_MONTH_IN_NONLEAP
    IF ( Is_Leap_Year(Year=DateTime%Year) ) Days_per_Month(2) = 29
    IF ( DateTime%Day > Days_per_Month(DateTime%Month) ) RETURN


    ! Compute the day of year
    ! -----------------------
    DoY = SUM(Days_per_Month(1:DateTime%Month-1)) + DateTime%Day

  END FUNCTION Day_of_Year

END MODULE DateTime_Utility
