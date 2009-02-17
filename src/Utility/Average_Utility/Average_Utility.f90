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
    '$Id$'
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
!       Error_Status = Boxcar_Average( x_in, y_in, x1, dx,     &  ! Input   
!                                      y_out, n_out,           &  ! Output
!                                      RCS_Id     =RCS_Id,     &  ! Revision control
!                                      Message_Log=Message_Log )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       x_in, y_in:   The tabulated (x,y) vector pairs to be averaged.
!                     UNITS:      Argument dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!       x1:           The first output x-value to report.
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
                           dx          , &  ! Input
                           y_out       , &  ! Input
                           n_out       , &  ! Output
                           RCS_Id      , &  ! Revision control
                           Message_Log ) &  ! Error messaging
                         RESULT( Error_Status )
    ! Arguments
    REAL(fp)              , INTENT(IN)  :: x_in(:), y_in(:)
    REAL(fp)              , INTENT(IN)  :: x1, dx
    REAL(fp)              , INTENT(OUT) :: y_out(:)
    INTEGER               , INTENT(OUT) :: n_out
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Boxcar_Average'
    ! Local variables
    INTEGER :: i, n_in, n, n_avg
    INTEGER, ALLOCATABLE :: idx(:)
    INTEGER :: idx_x_in(SIZE(x_in))
    LOGICAL :: mask_x_in(SIZE(x_in))
    REAL(fp) :: min_dx_in, dx2, max_x_in, x2, x, xb, xe
    
    ! Setup
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
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
    
    ! Compute the averaging parameters
    dx2 = dx/TWO
    IF ( x1-dx2 < x_in(1) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Not enough input data to produce output at X1.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF
    max_x_in = MAXVAL(x_in)
    n = INT((max_x_in - x1)/dx + ONEpointFIVE)
    Find_n_and_x2: DO
      x2 = x1 + REAL(n-1,fp)*dx
      IF ( x2+dx2 < max_x_in ) EXIT Find_n_and_x2
      n = n-1
    END DO Find_n_and_x2
    IF ( SIZE(y_out) < n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Y_OUT array not large enough for result.', &
                            Error_Status, & 
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Allocate the local averaging arrays
    n_avg = INT(dx/min_dx_in) + 5  !...plus some slop
    ALLOCATE(idx(n_avg))
    
    ! Begin the averaging loop over the binned output
    idx_x_in = (/(i,i=1,n_in)/)
    Average: DO i = 1, n
      x = x1 + REAL(i-1,fp)*dx
      xb = x - dx2 - SPACING(x); xe = x + dx2 + SPACING(x)
      mask_x_in = (x_in >= xb .AND. x_in < xe)
      n_avg = COUNT(mask_x_in)
      idx(1:n_avg) = PACK(idx_x_in,mask_x_in)
      y_out(i) = SUM(y_in(idx(1:n_avg))) / REAL(n_avg,fp)
    END DO Average
    n_out = n
    
    ! Cleanup
    DEALLOCATE(idx)
    
  END FUNCTION Boxcar_Average

END MODULE Average_Utility
