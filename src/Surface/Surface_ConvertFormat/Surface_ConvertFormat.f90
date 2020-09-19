!
! Surface_ConvertFormat
!
! Program to convert CRTM Surface data files from the previous format
! to the current one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-May-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Surface_ConvertFormat

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler        , ONLY: Program_Message, Display_Message, &
                                     SUCCESS, FAILURE
  USE File_Utility           , ONLY: File_Exists
  USE CRTM_Surface_Define    , ONLY: CRTM_Surface_type, &
                                     CRTM_Surface_WriteFile
  USE CRTM_Surface_Define_old, ONLY: Sfc_type    => CRTM_Surface_type
  USE CRTM_Surface_IO_old    , ONLY: Inquire_Sfc => CRTM_Surface_InquireFile, &
                                     Read_Sfc    => CRTM_Surface_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Surface_ConvertFormat'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: previous_filename, current_filename
  INTEGER :: err_stat
  INTEGER :: alloc_stat
  INTEGER :: m
  INTEGER :: n_profiles
  TYPE(Sfc_type)         , ALLOCATABLE :: sfc_old(:)
  TYPE(CRTM_Surface_type), ALLOCATABLE :: sfc(:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert CRTM Surface data files from '//&
                        'the previous format to the current one.', &
                        '$Revision$' )


  ! Get the PREVIOUS format filename
  WRITE( *,FMT='(/5x,"Enter the previous format CRTM Surface filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) previous_filename
  previous_filename = ADJUSTL(previous_filename)
  IF ( .NOT. File_Exists( TRIM(previous_filename) ) ) THEN
    msg = 'File '//TRIM(previous_filename)//' not found.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  ! ...construct the CURRENT format filename
  current_filename = TRIM(previous_filename)//'.current'


  ! Inquire the old file
  err_stat = Inquire_Sfc( previous_filename, n_Profiles=n_Profiles)
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error inquiring previous format Surface file.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Allocate the structure arrays
  ALLOCATE( sfc_old(n_Profiles), sfc(n_Profiles), STAT=alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE(msg,'("Error allocating Surface structure arrays. STAT = ",i0)') alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Read the old format file
  err_stat = Read_Sfc( previous_filename, sfc_old )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error reading old format Surface file.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Transfer over the data
  DO m = 1, n_Profiles

    sfc(m)%Land_Coverage         = sfc_old(m)%Land_Coverage
    sfc(m)%Water_Coverage        = sfc_old(m)%Water_Coverage
    sfc(m)%Snow_Coverage         = sfc_old(m)%Snow_Coverage
    sfc(m)%Ice_Coverage          = sfc_old(m)%Ice_Coverage

    sfc(m)%Land_Type             = sfc_old(m)%Land_Type
    sfc(m)%Land_Temperature      = sfc_old(m)%Land_Temperature
    sfc(m)%Soil_Moisture_Content = sfc_old(m)%Soil_Moisture_Content
    sfc(m)%Canopy_Water_Content  = sfc_old(m)%Canopy_Water_Content
    sfc(m)%Vegetation_Fraction   = sfc_old(m)%Vegetation_Fraction
    sfc(m)%Soil_Temperature      = sfc_old(m)%Soil_Temperature

    sfc(m)%Water_Type            = sfc_old(m)%Water_Type
    sfc(m)%Water_Temperature     = sfc_old(m)%Water_Temperature
    sfc(m)%Wind_Speed            = sfc_old(m)%Wind_Speed
    sfc(m)%Wind_Direction        = sfc_old(m)%Wind_Direction
    sfc(m)%Salinity              = sfc_old(m)%Salinity

    sfc(m)%Snow_Type             = sfc_old(m)%Snow_Type
    sfc(m)%Snow_Temperature      = sfc_old(m)%Snow_Temperature
    sfc(m)%Snow_Depth            = sfc_old(m)%Snow_Depth
    sfc(m)%Snow_Density          = sfc_old(m)%Snow_Density
    sfc(m)%Snow_Grain_Size       = sfc_old(m)%Snow_Grain_Size

    sfc(m)%Ice_Type              = sfc_old(m)%Ice_Type
    sfc(m)%Ice_Temperature       = sfc_old(m)%Ice_Temperature
    sfc(m)%Ice_Thickness         = sfc_old(m)%Ice_Thickness
    sfc(m)%Ice_Density           = sfc_old(m)%Ice_Density
    sfc(m)%Ice_Roughness         = sfc_old(m)%Ice_Roughness

  END DO


  ! Write the current format file
  err_stat = CRTM_Surface_WriteFile( current_filename, sfc )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error writing new format Surface file.'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Destroy the structure arrays
  DEALLOCATE( sfc_old, sfc, STAT = alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE(msg,'("Error deallocating Surface structure arrays. STAT = ",i0)') alloc_stat
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF

END PROGRAM Surface_ConvertFormat
