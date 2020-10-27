!
! Module containing averaging procedures
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Feb-2009
!                       paul.vandelst@noaa.gov
!

MODULE Average_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Boxcar_Average


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Default message length
  INTEGER, PARAMETER :: ML = 256
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TWO  = 2.0_fp
  REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp

CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Boxcar_Average
! 
! PURPOSE:
!       Function to compute the boxcar average for an input y-vector given
!       a specified bin size for an input x-vector.
!
! CALLING SEQUENCE:
!       Error_Status = Boxcar_Average( x_in, y_in, x1, x2, dx, &  ! Input   
!                                      y_out, n_out,           &  ! Output
!                                      RCS_Id     =RCS_Id,     &  ! Revision control
!                                      Message_Log=Message_Log )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       x_in, y_in:   The tabulated (x,y) vector pairs to be averaged.
!                     The x-spacing doesn't need to be uniform (but it
!                     is recommended).
!                     UNITS:      Argument dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x1, x2:       The first and last output x-value to report.
!                     UNITS:      Same as input x_in.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       dx:           The width of the averaging window.
!                     UNITS:      Same as input x_in.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y_out:        The boxcar average of the y_in data.
!                     UNITS:      Same as input y_in.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!       n_out:        The number of output averaged points.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the function executed normally.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-Feb-2009
!                       paul.vandelst@noaa.gov
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Boxcar_Average( x_in        , &  ! Input
                           y_in        , &  ! Input
                           x1          , &  ! Input
                           x2          , &  ! Input
                           dx          , &  ! Input
                           y_out       , &  ! Input
                           n_out       , &  ! Output
                           RCS_Id      , &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    REAL(fp)              , INTENT(IN)  :: x_in(:), y_in(:)
    REAL(fp)              , INTENT(IN)  :: x1, x2, dx
    REAL(fp)              , INTENT(OUT) :: y_out(:)
    INTEGER               , INTENT(OUT) :: n_out
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Boxcar_Average'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i, n_in, n_avg, j_start, j1, j2, j
    REAL(fp) :: min_dx_in, dx2, max_x_in, x, xb, xe
    
    ! Setup
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Compute and check the averaging parameters
    n_in = SIZE(x_in)
    IF ( SIZE(y_in) /= n_in ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X_IN and Y_IN have different sizes.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF
    min_dx_in = MINVAL(x_in(2:n_in) - x_in(1:n_in-1))
    IF ( dx < TWO*min_dx_in ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Requested DX output is too small given input X_IN.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF
    n_avg = INT(dx/min_dx_in) + 5  !...plus some slop
    dx2 = dx/TWO
    max_x_in = MAXVAL(x_in)
    IF ( x1-dx2 < x_in(1) .OR. x2+dx2 > max_x_in ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Input data range (",es13.6,",",es13.6,") insufficient to produce ",&
                  &"average output. Range of at least (",es13.6,",",es13.6,") required")' ) &
                  x_in(1), max_x_in, x1-dx2, x2+dx2
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF
    n_out = INT((x2 - x1)/dx + ONEpointFIVE)
    IF ( SIZE(y_out) < n_out ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Y_OUT size too small (",i0,") for result (",i0,").")' ) SIZE(y_out),n_out
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Initialise search start point for input
    j_start = 1

    ! Begin the averaging loop over the binned output
    Average: DO i = 1, n_out
      x = x1 + REAL(i-1,fp)*dx
      xb = x - dx2 - SPACING(x); xe = x + dx2 + SPACING(x)
      ! ..Linear search for averaging boundary indices 
      ! ..since input x-spacing may not be uniform
      j1_Search: DO j = j_start, n_in
        IF ( x_in(j) >= xb ) THEN
          j1 = j
          j_start = j
          EXIT j1_Search
        END IF
      END DO j1_Search
      j2_Search: DO j = j_start, n_in
        IF ( x_in(j) > xe ) THEN
          j2 = j-1
          j_start = j-1
          EXIT j2_Search
        END IF
      END DO j2_Search
      n_avg = j2-j1+1
      y_out(i) = SUM(y_in(j1:j2)) / REAL(n_avg,fp)
    END DO Average
    
  END FUNCTION Boxcar_Average

END MODULE Average_Utility
