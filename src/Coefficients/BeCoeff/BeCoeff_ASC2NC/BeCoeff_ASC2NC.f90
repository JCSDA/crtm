PROGRAM BeCoeff_ASC2NC

  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, Display_Message, Program_Message
  USE Zeeman_Utility , ONLY: load_bfield_lut, &
                             n_lat, n_lon, BField ! These need to be made public in module
  USE BeCoeff_Define , ONLY: BeCoeff_type, BeCoeff_Create, BeCoeff_Destroy, BeCoeff_DefineVersion
  USE BeCoeff_IO     , ONLY: BeCoeff_WriteFile
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'BeCoeff_ASC2NC'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  CHARACTER(*), PARAMETER :: INPUT_FILE   = 'Be_LUT.2007.txt'
  CHARACTER(*), PARAMETER :: OUTPUT_FILE  = 'BeCoeff.nc'

  CHARACTER(2000) :: title, comment
  INTEGER :: err_stat
  TYPE(BeCoeff_type) :: BeCoeff

  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the BeCoeff netCDF file from its ASCII original', &
                        '$Revision$' )

  ! Read ascii lut
  err_stat = load_bfield_lut(INPUT_FILE)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error loading ASCII LUT',err_stat)
    STOP
  END IF
  
  ! Create structure
  CALL BeCoeff_Create(BeCoeff, n_lat, n_lon)
  
  ! Copy ASCII data to structure
  BeCoeff%LUT = REAL(BField,fp)

  ! Write data to netCDF file
  title = 'Earth geomagnetic field data'
  comment = 'X-component is east, Y-component is north, and Z-component is zenith '//&
            '(positive upward); Latitudes and longitudes are evenly spaced from 90N-90S '//&
            'and 0E-360E respectively; Converted from ASCII file, '//INPUT_FILE//&
            ', committed by Yong Han in the CRTM EXP-Zeeman branch @ r5821'
  err_stat = BeCoeff_WriteFile( OUTPUT_FILE, &
                                BeCoeff, &
                                netCDF = .TRUE., &
                                title = title, &
                                history = PROGRAM_VERSION_ID, &
                                comment = comment )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error writing netCDF LUT',err_stat)
    STOP
  END IF

  ! Cleanup
  CALL BeCoeff_Destroy( BeCoeff )
  
END PROGRAM BeCoeff_ASC2NC
