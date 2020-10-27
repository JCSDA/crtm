!
! SpcCoeff_Convert
!
! Program to convert SpcCoeff files between the current and previous
! release format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 17-May-2004
!                       paul.vandelst@noaa.gov
!

PROGRAM SpcCoeff_Convert

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds            , ONLY: fp
  USE File_Utility          , ONLY: Get_Lun, File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                    Display_Message, Program_Message
  USE SpcCoeff_Define       , ONLY: SpcCoeff_type      , &
                                    SpcCoeff_Associated, &
                                    SpcCoeff_Create    , &
                                    SpcCoeff_Destroy   , &
                                    ACCoeff_Associated , &
                                    ACCoeff_Create
  USE SpcCoeff_Binary_IO    , ONLY: SpcCoeff_Binary_WriteFile, &
                                    SpcCoeff_Binary_ReadFile
  USE SpcCoeff_Define_old   , ONLY: SpcCoeff_type_old => SpcCoeff_type, &
                                    Associated_SpcCoeff, &
                                    Allocate_SpcCoeff  , &
                                    Destroy_SpcCoeff   , &
                                    Associated_AntCorr , &
                                    Allocate_AntCorr
  USE SpcCoeff_Binary_IO_old, ONLY: Read_SpcCoeff_Binary , &
                                    Write_SpcCoeff_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Convert'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  INTEGER, PARAMETER :: N_CONVERSIONS = 2
  INTEGER, PARAMETER :: OLD2NEW_CONVERSION = 1
  INTEGER, PARAMETER :: NEW2OLD_CONVERSION = 2
  INTEGER, PARAMETER :: CONVERSION(N_CONVERSIONS) = &
    (/OLD2NEW_CONVERSION, &
      NEW2OLD_CONVERSION/)
  CHARACTER(*), PARAMETER :: CONVERSION_NAME(N_CONVERSIONS) = &
    (/'Previous -> Current', &
      'Current -> Previous'/)


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256)  :: msg
  CHARACTER(256)  :: old_filename, new_filename
  INTEGER :: err_stat
  INTEGER :: i, direction
  TYPE(SpcCoeff_type    ) :: new_spccoeff
  TYPE(SpcCoeff_type_old) :: old_spccoeff


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to convert SpcCoeff files between the current '//&
                       'and previous release formats.', &
                       '$Revision$' )


  ! Determine conversion direction
  WRITE( *,FMT='(/5x,"Select format conversion direction:")' )
  DO i = 1, N_CONVERSIONS
    WRITE( *,FMT='(7x,i0,") ",a)' ) i, CONVERSION_NAME(i)
  END DO
  WRITE( *,FMT='(5x,"Enter choice: ")', ADVANCE='NO' )
  READ( *,* ) direction
  
  
  ! Call requisite internal subprogram
  SELECT CASE (direction)
    CASE (OLD2NEW_CONVERSION); CALL Old2New()
    CASE (NEW2OLD_CONVERSION); CALL New2Old()
    CASE DEFAULT
      msg = 'Invalid selection'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END SELECT


CONTAINS


  SUBROUTINE Old2New()  
        
    ! Get input filename
    WRITE( *,FMT='(/5x,"Enter the PREVIOUS RELEASE format SpcCoeff BINARY filename: ")',ADVANCE='NO' )
    READ( *,FMT='(a)' ) old_filename
    old_filename = ADJUSTL(old_filename)
    IF ( .NOT. File_Exists( old_filename ) ) THEN
      msg = 'SpcCoeff file '//TRIM(old_filename)//' not found.'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    WRITE( new_filename,'(a,".R",i0)' ) TRIM(old_filename), new_spccoeff%Release


    ! Read the old release SpcCoeff file
    err_stat = Read_SpcCoeff_Binary( old_filename, old_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading PREVIOUS RELEASE SpcCoeff file '//TRIM(old_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Allocate the current release SpcCoeff structure
    CALL SpcCoeff_Create( new_spccoeff, old_spccoeff%n_Channels )
    IF ( .NOT. SpcCoeff_Associated( new_spccoeff ) ) THEN   
      msg = 'Error allocating CURRENT RELEASE SpcCoeff strructure'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Copy over the unchanged data
    new_spccoeff%Version                    = old_spccoeff%Version  
    new_spccoeff%Sensor_Id                  = old_spccoeff%Sensor_Id
    new_spccoeff%Sensor_Type                = old_spccoeff%Sensor_Type
    new_spccoeff%WMO_Satellite_ID           = old_spccoeff%WMO_Satellite_ID
    new_spccoeff%WMO_Sensor_ID              = old_spccoeff%WMO_Sensor_ID
    new_spccoeff%Sensor_Channel             = old_spccoeff%Sensor_Channel
    new_spccoeff%Polarization               = old_spccoeff%Polarization
    new_spccoeff%Channel_Flag               = old_spccoeff%Channel_Flag
    new_spccoeff%Frequency                  = old_spccoeff%Frequency
    new_spccoeff%Wavenumber                 = old_spccoeff%Wavenumber
    new_spccoeff%Planck_C1                  = old_spccoeff%Planck_C1
    new_spccoeff%Planck_C2                  = old_spccoeff%Planck_C2
    new_spccoeff%Band_C1                    = old_spccoeff%Band_C1
    new_spccoeff%Band_C2                    = old_spccoeff%Band_C2
    new_spccoeff%Cosmic_Background_Radiance = old_spccoeff%Cosmic_Background_Radiance
    new_spccoeff%Solar_Irradiance           = old_spccoeff%Solar_Irradiance
    ! ...Antenna correction coeffs
    IF ( Associated_AntCorr( old_spccoeff%AC ) ) THEN
      CALL ACCoeff_Create( new_spccoeff%AC, old_spccoeff%AC%n_FOVs, old_spccoeff%AC%n_Channels )
      IF ( .NOT. ACCoeff_Associated( new_spccoeff%AC ) ) THEN
        msg = 'Error allocating ACCoeff structure'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      new_spccoeff%AC%Version          = old_spccoeff%AC%Version  
      new_spccoeff%AC%Sensor_Id        = old_spccoeff%AC%Sensor_Id
      new_spccoeff%AC%WMO_Satellite_Id = old_spccoeff%AC%WMO_Satellite_Id
      new_spccoeff%AC%WMO_Sensor_Id    = old_spccoeff%AC%WMO_Sensor_Id
      new_spccoeff%AC%Sensor_Channel   = old_spccoeff%AC%Sensor_Channel
      new_spccoeff%AC%A_earth          = old_spccoeff%AC%A_earth
      new_spccoeff%AC%A_space          = old_spccoeff%AC%A_space
      new_spccoeff%AC%A_platform       = old_spccoeff%AC%A_platform
    END IF


    ! Write the current release SpcCoeff file
    err_stat = SpcCoeff_Binary_WriteFile( new_filename, new_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing CURRENT RELEASE SpcCoeff file '//TRIM(new_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  
  
    ! Clean up
    CALL SpcCoeff_Destroy( new_spccoeff )
    err_stat = Destroy_SpcCoeff( old_spccoeff )

  END SUBROUTINE Old2New

  
  SUBROUTINE New2Old()  

    ! Get input filename
    WRITE( *,FMT='(/5x,"Enter the CURRENT RELEASE format SpcCoeff BINARY filename: ")',ADVANCE='NO' )
    READ( *,FMT='(a)' ) new_filename
    new_filename = ADJUSTL(new_filename)
    IF ( .NOT. File_Exists( new_filename ) ) THEN
      msg = 'SpcCoeff file '//TRIM(new_filename)//' not found.'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
    WRITE( old_filename,'(a,".R",i0)' ) TRIM(new_filename), old_spccoeff%Release


    ! Read the current release SpcCoeff file
    err_stat = SpcCoeff_Binary_Readfile( new_filename, new_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading CURRENT RELEASE SpcCoeff file '//TRIM(new_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Allocate the previous release SpcCoeff structure
    err_stat = Allocate_SpcCoeff( new_spccoeff%n_Channels, old_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error allocating PREVIOUS RELEASE SpcCoeff strructure'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Copy over the unchanged data
    old_spccoeff%Version                    = new_spccoeff%Version                    
    old_spccoeff%Sensor_Id                  = new_spccoeff%Sensor_Id                  
    old_spccoeff%Sensor_Type                = new_spccoeff%Sensor_Type                
    old_spccoeff%WMO_Satellite_ID           = new_spccoeff%WMO_Satellite_ID           
    old_spccoeff%WMO_Sensor_ID              = new_spccoeff%WMO_Sensor_ID              
    old_spccoeff%Sensor_Channel             = new_spccoeff%Sensor_Channel             
    old_spccoeff%Polarization               = new_spccoeff%Polarization               
    old_spccoeff%Channel_Flag               = new_spccoeff%Channel_Flag               
    old_spccoeff%Frequency                  = new_spccoeff%Frequency                  
    old_spccoeff%Wavenumber                 = new_spccoeff%Wavenumber                 
    old_spccoeff%Planck_C1                  = new_spccoeff%Planck_C1                  
    old_spccoeff%Planck_C2                  = new_spccoeff%Planck_C2                  
    old_spccoeff%Band_C1                    = new_spccoeff%Band_C1                    
    old_spccoeff%Band_C2                    = new_spccoeff%Band_C2                    
    old_spccoeff%Cosmic_Background_Radiance = new_spccoeff%Cosmic_Background_Radiance 
    old_spccoeff%Solar_Irradiance           = new_spccoeff%Solar_Irradiance           
    ! ...Antenna correction coeffs
    IF ( ACCoeff_Associated( new_spccoeff%AC ) ) THEN
      err_stat = Allocate_AntCorr( new_spccoeff%AC%n_FOVs, new_spccoeff%AC%n_Channels, old_spccoeff%AC )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error allocating AntCorr structure'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
      END IF
      old_spccoeff%AC_Present = .TRUE.
      old_spccoeff%AC%Version          = new_spccoeff%AC%Version          
      old_spccoeff%AC%Sensor_Id        = new_spccoeff%AC%Sensor_Id        
      old_spccoeff%AC%WMO_Satellite_Id = new_spccoeff%AC%WMO_Satellite_Id 
      old_spccoeff%AC%WMO_Sensor_Id    = new_spccoeff%AC%WMO_Sensor_Id    
      old_spccoeff%AC%Sensor_Channel   = new_spccoeff%AC%Sensor_Channel   
      old_spccoeff%AC%A_earth          = new_spccoeff%AC%A_earth          
      old_spccoeff%AC%A_space          = new_spccoeff%AC%A_space          
      old_spccoeff%AC%A_platform       = new_spccoeff%AC%A_platform       
    END IF


    ! Write the previous release SpcCoeff file
    err_stat = Write_SpcCoeff_Binary( old_filename, old_spccoeff )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing PREVIOUS RELEASE SpcCoeff file '//TRIM(old_filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  
  
    ! Clean up
    CALL SpcCoeff_Destroy( new_spccoeff )
    err_stat = Destroy_SpcCoeff( old_spccoeff )

  END SUBROUTINE New2Old
  
END PROGRAM SpcCoeff_Convert
