!
! FitCoeff_Inspect
!
! Program to inspect the contents of an FitCoeff file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Nov-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM FitCoeff_Inspect

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility   , ONLY: File_Exists
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE FitCoeff_Define, ONLY: FITCOEFF_MAX_N_DIMENSIONS, &
                             FitCoeff_1D_type, &
                             FitCoeff_2D_type, &
                             FitCoeff_3D_type, &
                             FitCoeff_Destroy, &
                             FitCoeff_InquireFile, &
                             FitCoeff_ReadFile, &
                             Inspect => FitCoeff_Inspect
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'FitCoeff_Inspect'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg, filename
  CHARACTER(2000) :: title, comment
  INTEGER :: n_dimensions
  TYPE(FitCoeff_1D_type) :: C_1D
  TYPE(FitCoeff_2D_type) :: C_2D
  TYPE(FitCoeff_3D_type) :: C_3D

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to display the contents of an '//&
                        'FitCoeff file to stdout.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *,FMT='(/5x,"Enter the FitCoeff filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) filename
  filename = ADJUSTL(filename)
  IF ( .NOT. File_Exists( TRIM(filename) ) ) THEN
    msg = 'File '//TRIM(filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

  ! Get the number of dimensions and global attributes
  err_stat = FitCoeff_InquireFile( &
               filename                   , &
               n_Dimensions = n_dimensions, &
               Title        = title       , &
               Comment      = comment       )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring FitCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Check dimensions
  IF ( n_dimensions < 1 .OR. n_dimensions > FITCOEFF_MAX_N_DIMENSIONS ) THEN
    WRITE( msg,'("Invalid number of dimensions, ",i0,", specified in ",a)' ) &
               n_dimensions, TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Display attributes
  IF ( LEN_TRIM(title)   > 0 ) WRITE( *,'(/3x,"Title:",/,a)' ) TRIM(title)
  IF ( LEN_TRIM(comment) > 0 ) WRITE( *,'(/3x,"Comment:",/,a,/)' ) TRIM(comment)

  
  ! Inspect the relevant file
  SELECT CASE (n_dimensions)
    ! 1-D coefficients
    CASE (1)
      err_stat = FitCoeff_ReadFile( C_1D, filename, Quiet=.TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading FitCoeff file '//TRIM(filename)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      CALL Inspect( C_1D )
      CALL FitCoeff_Destroy( C_1D )
    ! 2-D coefficients
    CASE (2)
      err_stat = FitCoeff_ReadFile( C_2D, filename, Quiet=.TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading FitCoeff file '//TRIM(filename)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      CALL Inspect( C_2D )
      CALL FitCoeff_Destroy( C_2D )
    ! 3-D coefficients
    CASE (3)
      err_stat = FitCoeff_ReadFile( C_3D, filename, Quiet=.TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading FitCoeff file '//TRIM(filename)
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      CALL Inspect( C_3D )
      CALL FitCoeff_Destroy( C_3D )
    ! Higher dimensions not implemented
    CASE DEFAULT
      msg = 'Super-invalid FitCoeff dimension! How did that happen?'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END SELECT

END PROGRAM FitCoeff_Inspect
