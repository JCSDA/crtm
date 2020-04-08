!
! CloudCoeff_Convert
!
! Program to convert a CRTM CloudCoeff data file
! between netCDF and Binary formats
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 27-Apr-2007
!                   paul.vandelst@noaa.gov
!

PROGRAM CloudCoeff_Convert

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE CloudCoeff_Define , ONLY: CloudCoeff_type
  USE CloudCoeff_IO     , ONLY: CloudCoeff_netCDF_to_Binary, &
                                CloudCoeff_Binary_to_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_Convert'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  INTEGER, PARAMETER :: N_CONVERSIONS = 2
  INTEGER, PARAMETER :: NC2BIN_CONVERSION = 1
  INTEGER, PARAMETER :: BIN2NC_CONVERSION = 2
  INTEGER, PARAMETER :: CONVERSION(N_CONVERSIONS) = &
    [ NC2BIN_CONVERSION, &
      BIN2NC_CONVERSION  ]
  CHARACTER(*), PARAMETER :: CONVERSION_NAME(N_CONVERSIONS,2) = &
    RESHAPE([ 'netCDF', 'Binary', 'Binary', 'netCDF'  ], &
            [N_CONVERSIONS,2])

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: input_filename, output_filename
  INTEGER :: err_stat
  INTEGER :: i, direction
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM CloudCoeff data file '//&
                        'between netCDF and Binary formats.', &
                        '$Revision$')
  
  ! Determine conversion direction
  WRITE( *,FMT='(/5x,"Select format conversion direction:")' )
  DO i = 1, N_CONVERSIONS
    WRITE( *,FMT='(7x,i0,") ",a," -> ",a)' ) i, CONVERSION_NAME(i,1), CONVERSION_NAME(i,2)
  END DO
  WRITE( *,FMT='(5x,"Enter choice: ")', ADVANCE='NO' )
  READ( *,* ) direction
  
  
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT ",a," CloudCoeff filename : ")', ADVANCE='NO') &
              TRIM(CONVERSION_NAME(direction,1))
  READ(*,'(a)') input_filename
  input_filename = ADJUSTL(input_filename)
  
  WRITE(*,FMT='(/5x,"Enter the OUTPUT ",a," CloudCoeff filename: ")', ADVANCE='NO') &
              TRIM(CONVERSION_NAME(direction,2))
  READ(*,'(a)') output_filename
  output_filename = ADJUSTL(output_filename)

  ! ...Sanity check that they're not the same
  IF ( input_filename == output_filename ) THEN
    msg = 'CloudCoeff input '//TRIM(CONVERSION_NAME(direction,1))//&
          ' and output '//TRIM(CONVERSION_NAME(direction,2))//' filenames are the same!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
    STOP
  END IF
  
  
  ! Perform the conversion
  SELECT CASE (direction)
    CASE (NC2BIN_CONVERSION)
      err_stat = CloudCoeff_netCDF_to_Binary( input_filename, output_filename )
    CASE (BIN2NC_CONVERSION)
      err_stat = CloudCoeff_Binary_to_netCDF( input_filename, output_filename )
    CASE DEFAULT
      msg = 'Invalid selection'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END SELECT
  
  ! ...Check the result      
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'CloudCoeff '//TRIM(CONVERSION_NAME(direction,1))//&
          ' -> '//TRIM(CONVERSION_NAME(direction,2))//' conversion failed.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
    STOP
  END IF
  
END PROGRAM CloudCoeff_Convert

