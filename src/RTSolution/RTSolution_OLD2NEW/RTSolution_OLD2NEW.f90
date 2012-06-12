!
! RTSolution_OLD2NEW
!
! Program to convert old RTSolution data file formats to the latest one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-May-2012
!                       paul.vandelst@noaa.gov
!

PROGRAM RTSolution_OLD2NEW

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility
  USE CRTM_RTSolution_Define
  USE CRTM_RTSolution_Define_old, ONLY: rts_type        => CRTM_RTSolution_type, &
                                        rts_Associated  => CRTM_RTSolution_Associated, &
                                        rts_Destroy     => CRTM_RTSolution_Destroy, &
                                        rts_Create      => CRTM_RTSolution_Create, &
                                        rts_InquireFile => CRTM_RTSolution_InquireFile, &
                                        rts_ReadFile    => CRTM_RTSolution_ReadFile
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME       = 'RTSolution_OLD2NEW'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
    '$Id$'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  CHARACTER(256) :: old_rts_file
  CHARACTER(256) :: new_rts_file
  INTEGER :: err_stat
  INTEGER :: alloc_stat
  INTEGER :: i, j, n_channels, n_profiles
  TYPE(rts_type)            , ALLOCATABLE :: rts_old(:,:)
  TYPE(CRTM_RTSolution_type), ALLOCATABLE :: rts(:,:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read old format RTSolution datafiles and '//&
                        'write new format files.', &
                        '$Revision$' )

  ! Get a filename to convert
  WRITE( *,FMT='(/5x,"Enter an old RTSolution filename : ")', ADVANCE='NO' )
  READ( *,'(a)' ) old_rts_file
  old_rts_file = ADJUSTL(old_rts_file)
  new_rts_file = TRIM(old_rts_file)//'.new'

  ! Inquire the old file
  err_stat = rts_InquireFile( old_rts_file, n_Channels=n_channels,n_Profiles=n_profiles)
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error inquiring old format RTSolution file.',FAILURE )
    STOP
  END IF

  ! Allocate the structure arrays
  ALLOCATE( rts_old(n_channels,n_profiles), &
            rts(n_channels,n_profiles), &
            STAT = alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error allocating RTSolution structure arrays. STAT = ",i0)') alloc_stat
    CALL Display_Message( PROGRAM_NAME,msg,FAILURE )
    STOP
  END IF

  ! Read the old format file
  err_stat = rts_ReadFile( old_rts_file, rts_old )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error reading old format RTSolution file.',FAILURE )
    STOP
  END IF


  ! Transfer over the data
  DO j = 1, n_profiles
    DO i = 1, n_channels
      IF ( rts_Associated(rts_old(i,j)) ) THEN
        CALL CRTM_RTSolution_Create(rts(i,j), rts_old(i,j)%n_Layers)
        rts(i,j)%Upwelling_Radiance  = rts_old(i,j)%Upwelling_Radiance
        rts(i,j)%Layer_Optical_Depth = rts_old(i,j)%Layer_Optical_Depth
      END IF
      rts(i,j)%Sensor_ID               = rts_old(i,j)%Sensor_ID
      rts(i,j)%WMO_Satellite_ID        = rts_old(i,j)%WMO_Satellite_ID
      rts(i,j)%WMO_Sensor_ID           = rts_old(i,j)%WMO_Sensor_ID
      rts(i,j)%Sensor_Channel          = rts_old(i,j)%Sensor_Channel
      rts(i,j)%RT_Algorithm_Name       = rts_old(i,j)%RT_Algorithm_Name
      rts(i,j)%Surface_Emissivity      = rts_old(i,j)%Surface_Emissivity
      rts(i,j)%Up_Radiance             = rts_old(i,j)%Up_Radiance
      rts(i,j)%Down_Radiance           = rts_old(i,j)%Down_Radiance
      rts(i,j)%Down_Solar_Radiance     = rts_old(i,j)%Down_Solar_Radiance
      rts(i,j)%Surface_Planck_Radiance = rts_old(i,j)%Surface_Planck_Radiance
      rts(i,j)%Radiance                = rts_old(i,j)%Radiance
      rts(i,j)%Brightness_Temperature  = rts_old(i,j)%Brightness_Temperature
    END DO
  END DO

  ! Write the new format file
  err_stat = CRTM_RTSolution_WriteFile( new_rts_file, rts )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME,'Error writing new format RTSolution file.',FAILURE )
    STOP
  END IF


  ! Destroy the structure arrays
  DEALLOCATE( rts_old, rts, &
              STAT = alloc_stat )
  IF ( alloc_stat /= 0 ) THEN
    WRITE( msg,'("Error deallocating RTSolution structure arrays. STAT = ",i0)' ) alloc_stat
    CALL Display_Message( PROGRAM_NAME,msg,FAILURE )
  END IF

END PROGRAM RTSolution_OLD2NEW
