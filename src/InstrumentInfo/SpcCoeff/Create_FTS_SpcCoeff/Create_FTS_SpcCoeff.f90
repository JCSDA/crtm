!
! Create_FTS_SpcCoeff
!
! Program to create the infrared spectral coefficient (SpcCoeff)
! data files for FTS sensors
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Create_FTS_SpcCoeff


  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                       Display_Message, Program_Message
  USE Fundamental_Constants    , ONLY: C_1, C_2
  USE Spectral_Units_Conversion, ONLY: Inverse_cm_to_GHz
  USE SpcCoeff_Parameters
  USE SpcCoeff_Define          , ONLY: SPCCOEFF_INFRARED => INFRARED_SENSOR, &
                                       UNPOLARIZED, &
                                       SOLAR_FLAG, &
                                       SpcCoeff_type, &
                                       SetFlag_SpcCoeff, &
                                       ClearFlag_SpcCoeff, &
                                       Allocate_SpcCoeff, &
                                       Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO       , ONLY: Write_SpcCoeff_netCDF

  USE IASI_Define
  
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_FTS_SpcCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Keyword set flag
  INTEGER, PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  
  ! TEMPORARY STUFF FOR IASI
  integer, parameter :: n_sensors=3
  character(*), parameter :: sensor_id(n_sensors) = &
  (/'iasiB1_metop-a','iasiB2_metop-a','iasiB3_metop-a'/) 
  integer, parameter :: band_id(n_sensors) = (/1,2,3/)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: SpcCoeff_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: n
  INTEGER :: SpcCoeff_File_Version
  CHARACTER( 256) :: Title
  TYPE(SpcCoeff_type)        :: SpcCoeff


  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to create the infrared SpcCoeff '//&
                       'files for FTS sensors.', &
                       '$Revision$' )

  ! Get user inputs
  ! ---------------
  ! The SpcCoeff version
  WRITE( *,FMT='(/5x,"Default SpcCoeff file version is: ",i0, &
               &".  Enter value: ")', &
           ADVANCE='NO' ) SpcCoeff%Version
  READ( *,* ) SpcCoeff_File_Version
  IF ( SpcCoeff_File_Version < SpcCoeff%Version ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    SpcCoeff_File_Version = SpcCoeff%Version
  END IF


  ! Begin the main sensor loop
  ! ---------------------------
  Sensor_Loop: DO n = 1, n_Sensors


    ! TEMPORARY STUFF FOR IASI
    ! Allocate the SpcCoeff structure
    ! -------------------------------
    Error_Status = Allocate_SpcCoeff( IASI_nPts(band_id(n)), &
                                      SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SpcCoeff data structure.', &
                            FAILURE )
      STOP
    END IF

    ! Assign various data components
    ! ------------------------------
    SpcCoeff%Version          = SpcCoeff_File_Version
    SpcCoeff%Sensor_Id        = sensor_id(n)
    SpcCoeff%Sensor_Type      = SPCCOEFF_INFRARED
    SpcCoeff%WMO_Satellite_ID = 4
    SpcCoeff%WMO_Sensor_ID    = 221
    SpcCoeff%Sensor_Channel   = IASI_Channels(band_id(n))
    SpcCoeff%Polarization     = UNPOLARIZED

    ! Set the frequencies
    SpcCoeff%Wavenumber = IASI_F(band_id(n))
    SpcCoeff%Frequency  = Inverse_cm_to_GHz( SpcCoeff%Wavenumber )
    
    ! Compute the Planck coefficients
    SpcCoeff%Planck_C1 = C_1_SCALE_FACTOR * C_1 * ( SpcCoeff%Wavenumber**3 )
    SpcCoeff%Planck_C2 = C_2_SCALE_FACTOR * C_2 *   SpcCoeff%Wavenumber
    
    ! Set the band correction coefficients
    SpcCoeff%Band_C1 = ZERO
    SpcCoeff%Band_C2 = ONE
    
    ! Fill the cosmic background field
    SpcCoeff%Cosmic_Background_Radiance = ZERO

    ! Set the solar fields
    SpcCoeff%Solar_Irradiance = ZERO
    CALL ClearFlag_SpcCoeff(SpcCoeff%Channel_Flag,SOLAR_FLAG)


    ! Write the SpcCoeff data file
    ! ----------------------------
    SpcCoeff_Filename = TRIM(sensor_id(n))//'.SpcCoeff.nc'
    
    Error_Status = Write_SpcCoeff_netCDF( TRIM(SpcCoeff_Filename), &
                                          SpcCoeff, &
                                          Title = 'Spectral coefficients for '//&
                                                  TRIM(sensor_id(n)), &
                                          History = PROGRAM_RCS_ID, &
                                          Comment = 'Placeholder file: '//&
                                                    'no band correction and no solar')
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing netCDF SpcCoeff data file '//&
                            TRIM(SpcCoeff_Filename), &
                            FAILURE )
      STOP
    END IF


    ! Destroy the current sensor data structures
    ! ------------------------------------------
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff data structure for '//&
                            TRIM(sensor_id(n))//' processing.', &
                            FAILURE )
      STOP
    END IF

  END DO Sensor_loop

END PROGRAM Create_FTS_SpcCoeff
