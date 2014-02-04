! Utility to define a timing structure and
! timing utility routines.
!
MODULE Timing_Utility

  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, INFORMATION, FAILURE, Display_Message
  USE File_Utility   , ONLY: Get_Lun

  ! Disable all implicit typing
  IMPLICIT NONE


  ! Visibilities
  PRIVATE
  ! ...Datatypes
  PUBLIC :: Timing_type
  ! ...Procedures
  PUBLIC :: Timing_Begin
  PUBLIC :: Timing_End
  PUBLIC :: Timing_Display
  PUBLIC :: Timing_Inspect
  PUBLIC :: Timing_Set
  PUBLIC :: Timing_Get
  PUBLIC :: Timing_WriteFile
  ! ...Old named procedures
  PUBLIC :: Begin_Timing
  PUBLIC :: End_Timing
  PUBLIC :: Display_Timing


  ! Parameters
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  INTEGER, PARAMETER :: ML = 256


  ! Overloads
  INTERFACE Begin_Timing
    MODULE PROCEDURE Timing_Begin
  END INTERFACE Begin_Timing

  INTERFACE End_Timing
    MODULE PROCEDURE Timing_End
  END INTERFACE End_Timing

  INTERFACE Display_Timing
    MODULE PROCEDURE Timing_Display
  END INTERFACE Display_Timing


  ! Derived type definitions
  !:tdoc+:
  TYPE :: Timing_type
    PRIVATE
    LOGICAL :: Is_Valid = .FALSE.
    INTEGER :: Hertz       = 0
    INTEGER :: Begin_Clock = 0
    INTEGER :: End_Clock   = 0
  END TYPE Timing_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Begin
!
! PURPOSE:
!   Subroutine to set the begin time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_Begin( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Begin( self )  ! In/Output
    TYPE(Timing_type), INTENT(OUT) :: self
    CALL SYSTEM_CLOCK( COUNT_RATE=self%Hertz, &
                       COUNT     =self%Begin_Clock )
    IF ( self%Hertz == 0 ) RETURN
    self%Is_Valid = .TRUE.
  END SUBROUTINE Timing_Begin


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_End
!
! PURPOSE:
!   Subroutine to set the end time count in a timing object
!
! CALLING SEQUENCE:
!   CALL Timing_End( timing )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_End( self )  ! In/Output
    TYPE(Timing_type), INTENT(IN OUT) :: self
    CALL SYSTEM_CLOCK( COUNT=self%End_Clock )
  END SUBROUTINE Timing_End


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Display
!
! PURPOSE:
!   Subroutine to display the elapsed time defined by the begin and end time
!   counts in the timing object.
!
! CALLING SEQUENCE:
!   CALL Timing_Display( timing, Caller = caller )
!
! INPUTS:
!   timing:  Timing object.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   caller:  String containing the name of the calling routine.
!            If not specified, the name of this procedure is used.
!            UNITS:      N/A
!            TYPE:       CHARACTER(*)
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Display( self  , &  ! Input
                             Caller  )  ! Optional input
    ! Arguments
    TYPE(Timing_type),      INTENT(IN) :: self
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Caller
    ! Local parameters
    REAL(fp), PARAMETER :: N_SECONDS_IN_HOUR        = 3600.0_fp
    REAL(fp), PARAMETER :: N_SECONDS_IN_MINUTE      =   60.0_fp
    REAL(fp), PARAMETER :: N_MILLISECONDS_IN_SECOND = 1000.0_fp
    ! Local variables
    CHARACTER(ML) :: Routine_Name
    CHARACTER(ML) :: Elapsed_Time
    REAL(fp)      :: Total_Time
    INTEGER       :: n_Hours
    INTEGER       :: n_Minutes
    INTEGER       :: n_Seconds
    INTEGER       :: n_milliSeconds

    ! Set up
    Routine_Name = 'Timing_Display'
    IF ( PRESENT(Caller) ) Routine_Name = TRIM(ADJUSTL(Caller))
    ! ...Check if timing structure valid for display
    IF ( .NOT. self%Is_Valid ) THEN
      CALL Display_Message( TRIM(Routine_Name), &
                            '***** Invalid timing structure! *****', &
                            FAILURE )
      RETURN
    END IF

    ! Compute the total time in seconds
    Total_Time = REAL(self%End_Clock - self%Begin_Clock, fp) / REAL(self%Hertz, fp)

    ! Split the total time into hours, minutes, seconds, and millseconds
    n_Hours        = INT(Total_Time / N_SECONDS_IN_HOUR)
    n_Minutes      = INT(MOD(Total_Time,N_SECONDS_IN_HOUR) / N_SECONDS_IN_MINUTE)
    n_Seconds      = INT(MOD(MOD(Total_Time,N_SECONDS_IN_HOUR), N_SECONDS_IN_MINUTE))
    n_milliSeconds = INT((Total_Time - AINT(Total_Time,fp)) * N_MILLISECONDS_IN_SECOND)

    ! Construct the character string
    WRITE( Elapsed_Time, '("Elapsed time-- ",i2.2,":",i2.2,":",i2.2,".",i3.3 )' ) &
                         n_Hours, n_Minutes, n_Seconds, n_milliSeconds
    CALL Display_Message( TRIM(Routine_Name), &
                          TRIM(Elapsed_Time), &
                          INFORMATION )

  END SUBROUTINE Timing_Display


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_Inspect
!
! PURPOSE:
!   Subroutine to print the contents of a Timing object to stdout.
!
! CALLING SEQUENCE:
!   CALL Timing_Inspect( timing )
!
! OBJECTS:
!   Timing:  Timing object to display.
!            UNITS:      N/A
!            TYPE:       Timing_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Timing_Inspect( self )
    TYPE(Timing_type), INTENT(IN) :: self
    WRITE(*,'(1x,"Timing OBJECT")')
    WRITE(*,'(3x,"Hertz       : ",i0)') self%Hertz
    WRITE(*,'(3x,"Begin_Clock : ",i0)') self%Begin_Clock
    WRITE(*,'(3x,"End_Clock   : ",i0)') self%End_Clock
    WRITE(*,'(3x,"Is_Valid    : ",l1)') self%Is_Valid
  END SUBROUTINE Timing_Inspect




  ! Subroutine to set the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Set( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN OUT) :: self
    INTEGER, OPTIONAL, INTENT(IN)     :: Hertz
    INTEGER, OPTIONAL, INTENT(IN)     :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(IN)     :: End_Clock
    LOGICAL, OPTIONAL, INTENT(IN)     :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) self%Hertz       = Hertz
    IF ( PRESENT(Begin_Clock) ) self%Begin_Clock = Begin_Clock
    IF ( PRESENT(End_Clock  ) ) self%End_Clock   = End_Clock
    IF ( PRESENT(Is_Valid   ) ) self%Is_Valid    = Is_Valid
  END SUBROUTINE Timing_Set


  ! Subroutine to get the components of a timing object
  ! Public, but only for testing so it's undocumented.
  SUBROUTINE Timing_Get( &
    self       , &
    Hertz      , &
    Begin_Clock, &
    End_Clock  , &
    Is_Valid     )
    ! Arguments
    TYPE(Timing_type), INTENT(IN)  :: self
    INTEGER, OPTIONAL, INTENT(OUT) :: Hertz
    INTEGER, OPTIONAL, INTENT(OUT) :: Begin_Clock
    INTEGER, OPTIONAL, INTENT(OUT) :: End_Clock
    LOGICAL, OPTIONAL, INTENT(OUT) :: Is_Valid
    ! Set object components
    IF ( PRESENT(Hertz      ) ) Hertz       = self%Hertz
    IF ( PRESENT(Begin_Clock) ) Begin_Clock = self%Begin_Clock
    IF ( PRESENT(End_Clock  ) ) End_Clock   = self%End_Clock
    IF ( PRESENT(Is_Valid   ) ) Is_Valid    = self%Is_Valid
  END SUBROUTINE Timing_Get
  
  ! Subroutine to write timing information in
  ! seconds and the date for a given timing object
  SUBROUTINE Timing_WriteFile( &
    self    , &
    filename  )
    ! Arguments
    TYPE(Timing_type), INTENT(IN OUT) :: self
    CHARACTER(*),      INTENT(IN) :: filename
    ! Local Variables
    REAL(fp) :: Total_Time
    CHARACTER(8) :: date
    CHARACTER(10) :: time
    ! Local Parameters
    INTEGER :: LUN = 1
    ! Compute the total time in seconds
    Total_Time = REAL(self%End_Clock - self%Begin_Clock, fp) / REAL(self%Hertz, fp)
    ! Write the total time in seconds to file
    CALL DATE_AND_TIME(DATE=date, TIME=time)
    OPEN(Unit=LUN, FILE=filename, POSITION="APPEND", Action="WRITE")
    WRITE(LUN, FMT='(1X, A10, 3X, A10, F12.2)') date, time, Total_Time
    CLOSE(LUN)
  END SUBROUTINE Timing_WriteFile  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Timing_WriteFile
!
! PURPOSE:
!   Function to write timing object information to an ASCII file.
!
! CALLING SEQUENCE:
!   Error_Status = Timing_WriteFile( &
!                    Timing_Array, &
!                    Filename    , &
!                    Clobber = clobber, &
!                    Heading = heading  )
!
! OBJECTS:
!   Timing_Array:  Array of Timing objects to write to file. All elements
!                  must be valid.
!                  UNITS:      N/A
!                  TYPE:       Timing_type
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!   Filename:      Name of the file to write.
!                  - If file does not exist, it is created.
!                  - If file does exist, it is opened for appending data.
!                  UNITS:      N/A
!                  TYPE:       Timing_type
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   Clobber:       Set this logical argument to overwrite an existing filename.
!                  If == .FALSE., an existing file is opened with OLD status and
!                                 positioned at end-of-file for appending data [DEFAULT].
!                     == .TRUE.,  an existing file is opened with REPLACE status.
!                  If not specified, default is .FALSE.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   Heading:       Array of character strings to use as heading labels for each
!                  timing object element.
!                  - If not specified, default is "Time 1", "Time 2", etc..
!                  - If specified, size must be same as Timing_Array.
!                  UNITS:      N/A
!                  TYPE:       CHARACTER
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:  The return value is an integer defining the error status.
!                  The error codes are defined in the Message_Handler module.
!                  If == SUCCESS the data write was successful
!                     == FAILURE an unrecoverable error occurred.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

!  FUNCTION Timing_WriteFile( &
!    Timing_Array, &
!    Filename    , &
!    Clobber     , &
!    Heading     ) &
!  RESULT( err_stat )
!    ! Arguments
!    TYPE(Timing_type),           INTENT(IN) :: Timing_Array(:)
!    CHARACTER(*)     ,           INTENT(IN) :: Filename
!    LOGICAL          , OPTIONAL, INTENT(IN) :: Clobber
!    CHARACTER(*)     , OPTIONAL, INTENT(IN) :: Heading(:)
!    ! Function result
!    INTEGER :: err_stat
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Timing_WriteFile'
!    ! Local variables
!    CHARACTER(ML) :: msg, err_msg, io_msg
!    CHARACTER(ML), ALLOCATABLE :: title(:)
!    CHARACTER(8) :: status, position
!    CHARACTER(8)  :: date
!    CHARACTER(10) :: time
!    LOGICAL :: append
!    INTEGER :: fid
!    INTEGER :: alloc_stat, io_stat
!    INTEGER :: i, n_times
!
!    ! Set up
!    err_stat = SUCCESS
!    ! ...Check structures
!    IF ( .NOT. ALL(Timing_Array%Is_Valid) ) THEN
!      err_stat = FAILURE
!      msg = 'Input timing array contains invalid elements'
!      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!    END IF
!    n_times = SIZE(Timing_Array)
!    ! ...Check clobber argument
!    append = .TRUE.
!    IF ( PRESENT(Clobber) ) append = .NOT. Clobber
!    ! ...Check header argument
!    ALLOCATE(title(n_times), STAT=alloc_stat, ERRMSG=err_msg)
!    IF ( alloc_stat /= 0 ) THEN
!      err_stat = FAILURE
!      msg = 'Local title array allocation failed - '//TRIM(err_msg)
!      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!    END IF
!    IF ( PRESENT(Heading) ) THEN
!      IF ( SIZE(Heading) /= n_times ) THEN
!        err_stat = FAILURE
!        msg = 'Input heading array different size from timing array'
!        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!      END IF
!      title = Heading
!    ELSE
!      DO i = 1, n_times
!        WRITE(title(i),'("Time ",i0)') i
!      END DO
!    END IF
!
!
!    ! Open the file
!    IF ( append ) THEN
!      status   = 'UNKNOWN'
!      position = 'APPEND'
!    ELSE
!      status   = 'REPLACE'
!      position = 'REWIND'
!    END IF
!    fid = Get_Lun()
!    IF ( fid < 0 ) THEN
!      err_stat = FAILURE
!      msg = 'Error obtaining free logical unit number'
!      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!    END IF
!    OPEN( fid, FILE     = filename   , &
!               FORM     = 'FORMATTED', &
!               STATUS   = status     , &
!               POSITION = position   , &
!               IOSTAT   = io_stat    , &
!               IOMSG    = io_msg       )
!    IF ( io_stat /= 0 ) THEN
!      err_stat = FAILURE
!      msg = 'Error opening file '//TRIM(filename)//' - '//TRIM(io_msg)
!      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!    END IF
!
!
!    ! Get the current date/time for output
!    CALL DATE_AND_TIME(DATE=date, TIME=time)
!print *, date, ' ', time
!
!
!    ! Done
!    CLOSE(fid, IOSTAT = io_stat, &
!               IOMSG  = io_msg   )
!    IF ( io_stat /= 0 ) THEN
!      err_stat = FAILURE
!      msg = 'Error closing file '//TRIM(filename)//' - '//TRIM(io_msg)
!      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
!    END IF
!
!  END FUNCTION Timing_WriteFile


END MODULE Timing_Utility
