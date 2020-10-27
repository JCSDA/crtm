!
! Linear_Interpolation
!
! Module containing linear interpolation routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Oct-2006
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Linear_Interpolation

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Modules used
  USE Type_Kinds,      ONLY: fp=>fp_kind
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE Search_Utility,  ONLY: Value_Locate, Bisection_Search
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Linear_Interpolate


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Linear_Interpolate
    MODULE PROCEDURE linint_scalar
    MODULE PROCEDURE linint_rank1
  END INTERFACE Linear_Interpolate


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id field
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       Linear_Interpolate
!
! PURPOSE:
!       Function to perform linear interpolation on input scalars
!       or vectors.
!
! CALLING SEQUENCE:
!       Error_Status = Linear_Interpolate( x,                      &  ! Input
!                                          y,                      &  ! Input
!                                          x_int,                  &  ! Input
!                                          y_int,                  &  ! Output
!                                          RCS_Id     =RCS_Id,     &  ! Optional output
!                                          Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       x:            The abscissa values for the tabulated function, y=F(x).
!                     Must be monotonically ascending or descending values.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
!       y:            An input vector of tabulated values, y=F(x), for which
!                     interpolates are required. Must have the same number of
!                     elements as the X input argument.
!                     UNITS:      Data dependent.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Rank-1 array, same as x.
!                     ATTRIBUTES: INTENT(IN)
!
!       x_int:        Abscissa values for which interpolates are required.
!                     Must be monotonically ascending or descending in the
!                     same direction as the X input argument.
!                     UNITS:      Data dependent, same as x.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to the screen.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       y_int:        Interpolate values corresponding to the X_INT abscissa
!                     values. Must have the same number of elements as the
!                     X_INT input argument.
!                     UNITS:      Data dependent, same as y.
!                     TYPE:       REAL(fp)
!                     DIMENSION:  Scalar or Rank-1 array, same as x_int.
!                     ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control System
!                     Id field for the module.
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
! RESTRICTIONS:
!       Input X and X_INT vectors *must* be monotonically ascending
!       or descending in the same direction.
!
! PROCEDURE:
!       The function computes the interpolates like so:
!
!                  y2 - y1 
!         y_int = ---------.(x - x1) + y1
!                  x2 - x1
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Oct-2006
!                       paul.vandelst@ssec.wisc.edu
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION linint_scalar( x,            &  ! Input
                          y,            &  ! Input
                          x_int,        &  ! Input
                          y_int,        &  ! Output
                          x_idx,        &  ! Optional input
                          RCS_Id,       &  ! Optional output
                          Message_Log ) &  ! Error messaging
                        RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN)  :: y
    REAL(fp), DIMENSION(:), INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: x_int
    REAL(fp),               INTENT(OUT) :: y_int
    INTEGER,      OPTIONAL, INTENT(IN)  :: x_idx
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Linear_Interpolate(scalar)'
    ! Local variables
    INTEGER :: nPoints
    INTEGER :: i


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Check array sizes for consistency
    nPoints = SIZE(x)
    IF ( nPoints /= SIZE(y) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

    ! Check that input array is large enough
    IF ( nPoints < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y arrays must have 2 or '//&
                            'more points for linear interpolation', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------------
    ! Find the locations for which interpolates are desired
    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    IF ( PRESENT(x_idx) ) THEN
      i = x_idx
    ELSE
      i = Bisection_Search(x, x_int)
    END IF

    ! -------------------------
    ! Perform the interpolation
    ! -------------------------
    y_int = ((y(i+1)-y(i))/(x(i+1)-x(i)))*(x_int-x(i)) + y(i)

  END FUNCTION linint_scalar


  FUNCTION linint_rank1( x,            &  ! Input
                         y,            &  ! Input
                         x_int,        &  ! Input
                         y_int,        &  ! Output
                         x_idx,        &  ! Optional input
                         RCS_Id,       &  ! Optional output
                         Message_Log ) &  ! Error messaging
                       RESULT( Error_Status )
    ! Arguments
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: x
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: y
    REAL(fp), DIMENSION(:),           INTENT(IN)  :: x_int
    REAL(fp), DIMENSION(:),           INTENT(OUT) :: y_int
    INTEGER,  DIMENSION(:), OPTIONAL, INTENT(IN)  :: x_idx
    CHARACTER(*),           OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),           OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Linear_Interpolate(rank-1)'
    ! Local variables
    INTEGER :: nPoints, nIntPoints
    INTEGER,  DIMENSION(SIZE(x_int)) :: i


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_Id

    ! Check array sizes for consistency
    nPoints = SIZE(x)
    IF ( nPoints /= SIZE(y) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    nIntPoints = SIZE(x_int)
    IF ( SIZE(y_int) /= nIntPoints ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X_INT and output Y_INT vectors have different sizes.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( PRESENT(x_idx) ) THEN
      IF ( SIZE(x_idx) /= nIntPoints ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input X_IDX vector has an inconsistent size.', &
                              Error_Status, &
                              Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Check that input array is large enough
    IF ( nPoints < 2 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input X and Y arrays must have 2 or '//&
                            'more points for linear interpolation', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------------
    ! Find the locations for which interpolates are desired
    ! -----------------------------------------------------
    ! Result is the lower index, i.e. x(i) < x_int < x(i+1)
    IF ( PRESENT(x_idx) ) THEN
      i = x_idx
    ELSE
      i = Value_Locate( x, x_int )
    END IF

    ! -------------------------
    ! Perform the interpolation
    ! -------------------------
    y_int = ((y(i+1)-y(i))/(x(i+1)-x(i)))*(x_int-x(i)) + y(i)

  END FUNCTION linint_rank1

END MODULE Linear_Interpolation
