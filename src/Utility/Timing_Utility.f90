! Utility to define a timing structure and
! timing utility routines.
!
MODULE Timing_Utility

  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: INFORMATION, FAILURE, Display_Message

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Visibilities
  PRIVATE
  PUBLIC :: Timing_type
  PUBLIC :: Begin_Timing
  PUBLIC :: End_Timing
  PUBLIC :: Display_Timing

  ! Derived type definitions
  TYPE :: Timing_type
    PRIVATE
    LOGICAL :: Is_Valid = .FALSE.
    INTEGER :: Hertz       = 0
    INTEGER :: Begin_Clock = 0
    INTEGER :: End_Clock   = 0
  END TYPE Timing_type

  ! Module parameters
  INTEGER,      PARAMETER :: CR = 13
  INTEGER,      PARAMETER :: LF = 10
  CHARACTER(2), PARAMETER :: CRLF = ACHAR(CR)//ACHAR(LF)


CONTAINS

  ! Subroutine to set the begin time count
  ! in the timing structure variable
  SUBROUTINE Begin_Timing( Timing )  ! In/Output
    TYPE(Timing_type), INTENT(OUT) :: Timing
    CALL SYSTEM_CLOCK( COUNT_RATE=Timing%Hertz, &
                       COUNT     =Timing%Begin_Clock )
    IF ( Timing%Hertz == 0 ) RETURN
    Timing%Is_Valid = .TRUE.
  END SUBROUTINE Begin_Timing


  ! Subroutine to set the end time count
  ! in the timing structure variable
  SUBROUTINE End_Timing( Timing )  ! In/Output
    TYPE(Timing_type), INTENT(IN OUT) :: Timing
    CALL SYSTEM_CLOCK( COUNT=Timing%End_Clock )
  END SUBROUTINE End_Timing


  ! Subroutine to display the elapsed time between
  ! the begin and end time counts in the timing
  ! structure variable
  SUBROUTINE Display_Timing( Timing, &  ! Input
                             Caller  )  ! Optional input
    ! Arguments
    TYPE(Timing_type),      INTENT(IN OUT) :: Timing
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Caller
    ! Local parameters
    REAL(fp), PARAMETER :: N_SECONDS_IN_HOUR        = 3600.0_fp
    REAL(fp), PARAMETER :: N_SECONDS_IN_MINUTE      =   60.0_fp
    REAL(fp), PARAMETER :: N_MILLISECONDS_IN_SECOND = 1000.0_fp
    ! Local variables
    CHARACTER(256) :: Routine_Name
    CHARACTER(256) :: Elapsed_Time
    REAL(fp)       :: Total_Time
    INTEGER        :: n_Hours
    INTEGER        :: n_Minutes
    INTEGER        :: n_Seconds
    INTEGER        :: n_milliSeconds

    ! Set up
    Routine_Name = 'Display_Timing'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    ! ...Check if timing structure valid for display
    IF ( .NOT. Timing%Is_Valid ) THEN
      CALL Display_Message( TRIM(Routine_Name), &
                            '***** Invalid timing structure! *****', &
                            FAILURE )
      RETURN
    END IF

    ! Compute the total time in seconds
    Total_Time = REAL(Timing%End_Clock - Timing%Begin_Clock, fp) / REAL(Timing%Hertz, fp)

    ! Split the total time into hours, minutes, seconds, and millseconds
    n_Hours        = INT(Total_Time / N_SECONDS_IN_HOUR)
    n_Minutes      = INT(MOD(Total_Time,N_SECONDS_IN_HOUR) / N_SECONDS_IN_MINUTE)
    n_Seconds      = INT(MOD(MOD(Total_Time,N_SECONDS_IN_HOUR), N_SECONDS_IN_MINUTE))
    n_milliSeconds = INT((Total_Time - INT(Total_Time)) * N_MILLISECONDS_IN_SECOND)

    ! Construct the character string
    WRITE( Elapsed_Time, '("Elapsed time-- ",i2.2,":",i2.2,":",i2.2,".",i3.3 )' ) &
                         n_Hours, n_Minutes, n_Seconds, n_milliSeconds
    CALL Display_Message( TRIM(Routine_Name), &
                          TRIM(Elapsed_Time), &
                          INFORMATION )
    
    ! Destroy the timing information
    CALL Destroy_Timing(Timing)
    
  END SUBROUTINE Display_Timing


  ! Subroutine to reinitialise a timing structure
  SUBROUTINE Destroy_Timing(Timing)
    TYPE(Timing_type), INTENT(OUT) :: Timing
    Timing%Is_Valid = .FALSE.
  END SUBROUTINE Destroy_Timing

END MODULE Timing_Utility
