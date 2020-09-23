!
! Sensor_Planck_Utility_Example
!
! Example program to show usage of the Sensor_Planck_Utility library.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 16-Feb-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM Sensor_Planck_Utility_Example

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Sensor_Planck_Utility, ONLY: &
    ! ...Utility stuff
    fp                       , &  ! The floating point kind type (double)
    SUCCESS                  , &  ! Error status parameters
    ! ...SpcCoeff entities
    SpcCoeff_type            , &  ! The spectral coefficient (SpcCoeff) type definition
    SpcCoeff_Associated      , &  ! The SpcCoeff object lifecycle checker
    SpcCoeff_Destroy         , &  ! The SpcCoeff object destructor
    SpcCoeff_Binary_ReadFile , &  ! The SpcCoeff file reader
    ! ...Sensor Planck entities
    Sensor_Planck_Version    , &  ! Outputs the module information
    Sensor_Planck_Radiance   , &  ! The...
    Sensor_Planck_Temperature, &  !   actual...
    Sensor_Planck_dBdT       , &  !     sensor...
    Sensor_Planck_dTdB            !       procedures.

  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Sensor_Planck_Utility_Example'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(512)      :: id
  CHARACTER(512)      :: spccoeff_filename
  INTEGER             :: err_stat
  INTEGER             :: sensor_channel
  TYPE(SpcCoeff_type) :: spccoeff
  REAL(fp)            :: temperature
  REAL(fp)            :: radiance, brightness_temperature, dbdt, dtdb


  ! Version output
  CALL Sensor_Planck_Version(id)
  WRITE( *,'(/,a,/)' ) TRIM(id)


  ! Read an SpcCoeff datafile
  spccoeff_filename = 'abi_gr.SpcCoeff.bin'
  err_stat = SpcCoeff_Binary_ReadFile( spccoeff_filename, spccoeff )
  IF ( err_stat /= SUCCESS ) THEN
    WRITE( *,'("Error reading SpcCoeff file ",a)' ) TRIM(spccoeff_filename)
    STOP
  END IF
  ! ...Output the list of channels
  WRITE( *,'(/7x,a," channel list:")' )TRIM(spccoeff%Sensor_id)
  WRITE( *,'(10i5,/)' ) spccoeff%sensor_channel


  ! Open loop over user temperature input
  Temperature_Loop: DO
    WRITE( *,FMT='(/5x,"Enter a temperature in Kelvin (-ve to quit): ")', ADVANCE='NO' )
    READ( *,* ) temperature
    IF ( temperature < 0.0_fp ) EXIT Temperature_Loop

    ! Open loop over user sensor channel input
    Channel_Loop: DO
      WRITE( *,FMT='(/5x,"Enter a sensor channel (-ve to quit): ")', ADVANCE='NO' )
      READ( *,* ) sensor_channel
      IF ( sensor_channel < 0 ) EXIT Channel_Loop
      ! ...Valid channel?
      IF ( ALL(spccoeff%sensor_channel /= sensor_channel) ) THEN
        WRITE( *,'("Channel ",i0," is not a valid channel for ",a,". Try again...")' ) sensor_channel, TRIM(spccoeff%Sensor_Id)
        CYCLE Channel_Loop
      END IF

      ! Compute the sensor radiance, brightness temperature, dBdT, and dTdB
      CALL Sensor_Planck_Radiance( spccoeff      , &  ! The SpcCoeff object
                                   sensor_channel, &  ! The sensor channel. NOT the channel index; the actual channel number.
                                   temperature   , &  ! The temperature at which you want the radiance
                                   radiance        )  ! The resulting radiance.
      CALL Sensor_Planck_Temperature( spccoeff, sensor_channel, radiance, brightness_temperature )
      CALL Sensor_Planck_dBdT( spccoeff, sensor_channel, temperature, dbdt )
      CALL Sensor_Planck_dTdB( spccoeff, sensor_channel, radiance, dtdb )

      ! Output the result
      WRITE( *,'(/7x,a," channel ",i0,"; T=",f10.3,"K",&
                &/9x,"R     = ",f10.3,&
                &/9x,"Tb    = ",f10.3,&
                &/9x,"dB/dT = ",f10.3,&
                &/9x,"dT/dB = ",f10.3,/)' ) &
                TRIM(spccoeff%Sensor_id), sensor_channel, temperature, &
                radiance, brightness_temperature, dbdt, dtdb

    END DO Channel_Loop

  END DO Temperature_Loop


  ! Clean up
  CALL SpcCoeff_Destroy(spccoeff)
  IF ( SpcCoeff_Associated(spccoeff) ) THEN
    WRITE( *,'("Error destroying SpcCoeff object")' )
    STOP
  END IF

END PROGRAM Sensor_Planck_Utility_Example
