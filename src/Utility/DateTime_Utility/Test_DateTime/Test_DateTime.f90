!
! Test_DateTime
!
! Program to test the DateTime_Utility module procedures
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Sep-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_DateTime

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE DateTime_Utility
  USE Unit_Test

  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_DateTime'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: DT_Values(8)
  TYPE(DateTime_type) :: DateTime
  TYPE(UTest_type)    :: UTest


  ! Initialise the test counters
  ! ----------------------------
  CALL Init_AllTests(UTest)
  
  
  ! Test the Get_DateTime routine
  ! -----------------------------
  CALL Init_Test(UTest,'Get_DateTime procedure tests')
  CALL DATE_AND_TIME(VALUES=DT_Values)
  DateTime = Get_DateTime()
  CALL Is_Equal( DT_Values(1), DateTime%Year       , UTest )
  CALL Is_Equal( DT_Values(2), DateTime%Month      , UTest )
  CALL Is_Equal( DT_Values(3), DateTime%Day        , UTest )
  CALL Is_Equal( DT_Values(4), DateTime%UTC_Delta  , UTest )
  CALL Is_Equal( DT_Values(5), DateTime%Hour       , UTest )
  CALL Is_Equal( DT_Values(6), DateTime%Minute     , UTest )
  
  CALL Report_Test(UTest)
  
  
  ! Test the leap year routine
  ! --------------------------
  CALL Init_Test(UTest,'Is_Leap_Year procedure tests')
  ! 2006 was NOT a leap year
  DateTime%Year = 2006
  CALL Assert( .NOT. Is_Leap_Year(DateTime), UTest )
  ! 2000 was a leap year
  DateTime%Year = 2000
  CALL Assert( Is_Leap_Year(DateTime), UTest )
  ! 1900 was NOT a leap year
  DateTime%Year = 1900
  CALL Assert( .NOT. Is_Leap_Year(DateTime), UTest )
  ! 2004 was a leap year
  DateTime%Year = 2004
  CALL Assert( Is_Leap_Year(DateTime), UTest )
  CALL Report_Test(UTest)

  
  ! Test the day-of-year routine
  ! ----------------------------
  CALL Init_Test(UTest,'Day_of_Year procedure tests')
  DateTime%Year  = 2007
  DateTime%Month = 1
  DateTime%Day   = 1
  CALL Is_Equal(1, Day_of_Year(DateTime), UTest)

  DateTime%Year  = 2007
  DateTime%Month = 12
  DateTime%Day   = 31
  CALL Is_Equal(365, Day_of_Year(DateTime), UTest)
  
  DateTime%Year  = 2000
  DateTime%Month = 12
  DateTime%Day   = 31
  CALL Is_Equal(366, Day_of_Year(DateTime), UTest)
  
  DateTime%Year  = 2006
  DateTime%Month = 12
  DateTime%Day   = 31
  CALL Is_Equal(365, Day_of_Year(DateTime), UTest)
  
  CALL Report_Test(UTest)


  ! Report all the test results
  ! ---------------------------
  CALL Report_AllTests( UTest )
END PROGRAM Test_DateTime
