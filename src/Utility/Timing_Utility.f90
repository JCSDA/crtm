! Utility to define a timing structure and
! timing utility routines.
!
MODULE Timing_Utility

  ! Module usage
  USE Type_Kinds

  ! Disable all implicit typing
  IMPLICIT NONE

  ! Visibilities
  PRIVATE
  PUBLIC :: Begin_Timing
  PUBLIC :: End_Timing
  PUBLIC :: Display_Timing

  ! Derived type definitions
  TYPE, PUBLIC :: Timing_type
    PRIVATE
    INTEGER :: Hertz
    INTEGER :: Begin_Clock
    INTEGER :: End_Clock
  END TYPE Timing_type


CONTAINS


  ! Subroutine to set the begin time count
  ! in the timing structure variable
  !
  SUBROUTINE Begin_Timing( Timing )  ! In/Output
    TYPE( Timing_type ), INTENT( IN OUT ) :: Timing
    CALL SYSTEM_CLOCK( COUNT_RATE = Timing%Hertz, &
                       COUNT      = Timing%Begin_Clock )
  END SUBROUTINE Begin_Timing


  ! Subroutine to set the end time count
  ! in the timing structure variable
  !
  SUBROUTINE End_Timing( Timing )  ! In/Output
    TYPE( Timing_type ), INTENT( IN OUT ) :: Timing
    CALL SYSTEM_CLOCK( COUNT = Timing%End_Clock )
  END SUBROUTINE End_Timing


  ! Subroutine to display the elapsed time between
  ! the begin and end time counts in the timing
  ! structure variable
  !
  SUBROUTINE Display_Timing( Timing )  ! In/Output
    ! Arguments
    TYPE( Timing_type ), INTENT( IN ) :: Timing
    ! Local parameters
    REAL( fp_kind ), PARAMETER :: N_SECONDS_IN_HOUR        = 3600.0_fp_kind
    REAL( fp_kind ), PARAMETER :: N_SECONDS_IN_MINUTE      =   60.0_fp_kind
    REAL( fp_kind ), PARAMETER :: N_MILLISECONDS_IN_SECOND = 1000.0_fp_kind
    ! Local variables
    REAL( fp_kind ) :: Total_Time
    INTEGER         :: n_Hours
    INTEGER         :: n_Minutes
    INTEGER         :: n_Seconds
    INTEGER         :: n_milliSeconds
    CHARACTER( 12 ) :: Elapsed_Time

    ! Compute the total time in seconds
    Total_Time = REAL( Timing%End_Clock - Timing%Begin_Clock, fp_kind ) / &
    !            ------------------------------------------------------
                            REAL( Timing%Hertz, fp_kind )

    ! Split the total time into hours, minutes, seconds, and millseconds
    n_Hours        = INT( Total_Time/N_SECONDS_IN_HOUR )
    n_Minutes      = INT( MOD( Total_Time,N_SECONDS_IN_HOUR )/N_SECONDS_IN_MINUTE )
    n_Seconds      = INT( MOD( MOD( Total_Time,N_SECONDS_IN_HOUR ), N_SECONDS_IN_MINUTE ) )
    n_milliSeconds = INT( ( Total_Time - INT( Total_Time ) ) * N_MILLISECONDS_IN_SECOND )

    ! Construct the character string
    WRITE( Elapsed_Time, '( i2.2,":",i2.2,":",i2.2,".",i3.3 )' ) &
                         n_Hours, n_Minutes, n_Seconds, n_milliSeconds
    WRITE( *, '( 5x, "Elapsed time: ", a )' ) Elapsed_Time

  END SUBROUTINE Display_Timing

END MODULE Timing_Utility
