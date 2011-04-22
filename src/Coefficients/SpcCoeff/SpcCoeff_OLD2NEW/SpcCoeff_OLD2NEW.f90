!
! SpcCoeff_OLD2NEW
!
! Program to convert netCDF format SpcCoeff files from an OLD release 
! format to a NEW one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-May-2004
!                       paul.vandelst@noaa.gov
!

PROGRAM SpcCoeff_OLD2NEW

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds            , ONLY: fp
  USE File_Utility          , ONLY: Get_Lun, File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                    Display_Message, Program_Message
  USE SpcCoeff_Define       , ONLY: SpcCoeff_type, &
                                    SpcCoeff_Associated, &
                                    SpcCoeff_Create    , &
                                    SpcCoeff_Destroy
  USE SpcCoeff_netCDF_IO    , ONLY: SpcCoeff_netCDF_WriteFile
  USE SpcCoeff_Define_old   , ONLY: SpcCoeff_type_old => SpcCoeff_type, &
                                    Allocate_SpcCoeff, &
                                    Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO_old, ONLY: Read_SpcCoeff_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_OLD2NEW'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: msg
  CHARACTER(256)  :: old_filename, filename
  CHARACTER(5000) :: title
  CHARACTER(5000) :: history
  CHARACTER(5000) :: comment
  INTEGER :: err_stat
  INTEGER :: i
  TYPE(SpcCoeff_type    ) :: spccoeff
  TYPE(SpcCoeff_type_old) :: old_spccoeff


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to convert netCDF format SpcCoeff files from '//&
                       'an OLD release format to a NEW one.', &
                       '$Revision$' )

  ! Get user inputs
  WRITE( *,FMT='(/5x,"Enter the OLD format SpcCoeff netCDF filename: ")',ADVANCE='NO' )
  READ( *,FMT='(a)' ) old_filename
  old_filename = ADJUSTL(old_filename)
  IF ( .NOT. File_Exists( old_filename ) ) THEN
    msg = 'oSRF file '//TRIM(old_filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  filename = TRIM(old_filename)//'.NEW'


  ! Read the old release netCDF SpcCoeff file
  err_stat = Read_SpcCoeff_netCDF( &
    old_filename, &
    old_spccoeff, &
    Title         = title  , &
    History       = history, &
    Comment       = comment  )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading old release netCDF SpcCoeff file '//TRIM(old_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...Strip out any "AntCorr" entries from the history and comment
  i = INDEX(history, '; AntCorr')
  IF ( i > 0 ) history = history(1:i-1)
  i = INDEX(comment, '; AntCorr')
  IF ( i > 0 ) comment = comment(1:i-1)


  ! Allocate the new SpcCoeff structure
  CALL SpcCoeff_Create( spccoeff, old_spccoeff%n_Channels )
  IF ( .NOT. SpcCoeff_Associated( spccoeff ) ) THEN   
    msg = 'Error allocating new SpcCoeff strructure'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Copy over the unchanged data
  spccoeff%Sensor_Id                  = old_spccoeff%Sensor_Id
  spccoeff%Sensor_Type                = old_spccoeff%Sensor_Type
  spccoeff%WMO_Satellite_ID           = old_spccoeff%WMO_Satellite_ID
  spccoeff%WMO_Sensor_ID              = old_spccoeff%WMO_Sensor_ID
  spccoeff%Sensor_Channel             = old_spccoeff%Sensor_Channel
  spccoeff%Polarization               = old_spccoeff%Polarization
  spccoeff%Channel_Flag               = old_spccoeff%Channel_Flag
  spccoeff%Frequency                  = old_spccoeff%Frequency
  spccoeff%Wavenumber                 = old_spccoeff%Wavenumber
  spccoeff%Planck_C1                  = old_spccoeff%Planck_C1
  spccoeff%Planck_C2                  = old_spccoeff%Planck_C2
  spccoeff%Band_C1                    = old_spccoeff%Band_C1
  spccoeff%Band_C2                    = old_spccoeff%Band_C2
  spccoeff%Cosmic_Background_Radiance = old_spccoeff%Cosmic_Background_Radiance
  spccoeff%Solar_Irradiance           = old_spccoeff%Solar_Irradiance


  ! Write the new netCDF SpcCoeff file
  err_stat = SpcCoeff_netCDF_WriteFile( &
    filename, &
    spccoeff, &
    Title         = title  , &
    History       = PROGRAM_VERSION_ID//'; '//TRIM(history), &
    Comment       = comment  )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing new release netCDF SpcCoeff file '//TRIM(filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Clean up
  CALL SpcCoeff_Destroy( spccoeff )
  err_stat = Destroy_SpcCoeff( old_spccoeff )

END PROGRAM SpcCoeff_OLD2NEW
