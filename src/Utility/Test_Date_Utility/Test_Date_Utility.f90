!
! Test_Date_Utility
!
! Program to test the routines in the Date_Utility module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Nov-2000
!                       paul.vandelst@ssec.wisc.edu

PROGRAM Test_Date_Utility

  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler
  USE Date_Utility
  ! Disable all implicit typing
  IMPLICIT NONE

  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Date_Utility'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER, PARAMETER :: DAYS_IN_A_YEAR = 365

  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: answer
  INTEGER :: errstat
  INTEGER :: Max_DoY
  INTEGER :: DoY, Year, Re_DoY
  INTEGER :: DoM, Month
  CHARACTER(80) :: Date


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test Date_Utility module procedures', &
                        '$Revision$' )
  
  ! Get user input
  WRITE( *,'(/5x,"Enter a year [YYYY]: ")', ADVANCE = 'NO' )
  READ( *,* ) Year


  ! Begin test
  ! ----------
  
  ! Determine the number of days in the year in question
  IF ( Is_Leap_Year( Year ) ) THEN
    Max_DoY = DAYS_IN_A_YEAR + 1
  ELSE
    Max_DoY = DAYS_IN_A_YEAR
  END IF

  ! Loop over the days of the year
  DO DoY = 1, Max_DoY
  
    ! Convert Day-of-Year to Date
    errstat = Day_of_Year_to_Date( DoY , &  ! Input
                                   Year, &  ! Input
                                   Day_of_Month = DoM  , &  ! Optional output
                                   Month        = Month, &  ! Optional output
                                   Date_String  = Date   )  ! Optional output
    IF ( errstat /= SUCCESS ) THEN
      WRITE( msg,'("Error converting day-of-year ",i0," to date.")' ) DoY
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), errstat )
      STOP
    END IF

    ! Convert Date back to Day-of-Year
    errstat = Date_to_Day_of_Year( DoM   , &  ! Input
                                   Month , &  ! Input
                                   Year  , &  ! Input
                                   Re_DoY  )  ! Output
    IF ( errstat /= SUCCESS ) THEN
      WRITE( msg,'("Error converting date ",a," to day-of-year.")' ) TRIM(Date)
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), errstat )
      STOP
    END IF

    ! Output result
    WRITE( *,'(2x,"Input DoY: ",i3, " -->DoM,Month,Date: ",1x,2(i2,", "),a, &
               &" --> Output DoY: ",i3)' ) &
               DoY, DoM, Month, TRIM(Date), Re_DoY
    IF ( MOD( DoY, 40 ) == 0 ) THEN
      WRITE( *,'(/10x,"Press <ENTER> to continue...")' )
      READ( *,'(a)' ) answer
    END IF

  END DO

END PROGRAM Test_Date_Utility
