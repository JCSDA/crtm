!
! AtmProfile2Atmosphere
!
! Program to convert netCDF AtmProfile datafiles into Binary
! Atmosphere datafiles.
!
!
! FILES ACCESSED:
!       Input:  netCDF format AtmProfile file
!       Output: Binary format Atmosphere file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM AtmProfile2Atmosphere

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Display_Message, Program_Message
  USE Binary_File_Utility
  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'AtmProfile2Atmosphere'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: AtmProfile2Atmosphere.f90,v 1.7 2006/05/02 14:58:34 dgroff Exp $'

  INTEGER, PARAMETER :: N_CLOUDS = 0  ! No cloud data
  INTEGER, PARAMETER :: N_AEROSOLS = 0! No aerosol data


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: AtmProfile_Filename
  CHARACTER(256) :: Atmosphere_Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, nc, na, in
  TYPE(AtmProfile_type) :: AtmProfile
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atmosphere(:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Dummy_Atmosphere(:)


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF AtmProfile datafiles into '//&
                        'Binary Atmosphere datafiles.', &
                        '$Revision: 1.7 $' )


  ! Read the AtmProfile file
  WRITE(*,FMT='(/5x,"Enter an AtmProfile Filename: ")', ADVANCE='NO')
  READ(*,FMT='(a)') AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )

  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename, &
                                         AtmProfile, &
                                         Reverse = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM( AtmProfile_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! Allocate the structure arrays
  ALLOCATE( Atmosphere( AtmProfile%n_Profiles ), &
            Dummy_Atmosphere(  AtmProfile%n_Profiles ), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating the Atmosphere structure arrays. STAT = ", i0 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! Loop over profiles
  Profile_Loop: DO m = 1, AtmProfile%n_Profiles

    ! Allocate the current profile Atmosphere structure
    Error_Status = CRTM_Allocate_Atmosphere( AtmProfile%n_Layers, &
                                             AtmProfile%n_Absorbers, &
                                             N_CLOUDS, &
                                             N_AEROSOLS, &
                                             Atmosphere(m) )
    IF ( Error_Status /= 0 ) THEN
      WRITE(Message,'("Error allocating Atmosphere structure for profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! Copy over the data
    Atmosphere(m)%Climatology    = AtmProfile%Climatology_Model(m)
    Atmosphere(m)%Absorber_ID    = AtmProfile%Absorber_ID
    Atmosphere(m)%Absorber_Units = AtmProfile%Absorber_Units_ID
    Atmosphere(m)%Level_Pressure = AtmProfile%Level_Pressure(:,m)
    Atmosphere(m)%Pressure       = AtmProfile%Layer_Pressure(:,m)
    Atmosphere(m)%Temperature    = AtmProfile%Layer_Temperature(:,m)
    Atmosphere(m)%Absorber       = AtmProfile%Layer_Absorber(:,:,m)

  END DO Profile_Loop


  ! Write the Atmosphere binary file
  WRITE(*,FMT='(/5x,"Enter an output Atmosphere Filename: ")', ADVANCE='NO')
  READ(*,FMT='(a)') Atmosphere_Filename
  Atmosphere_Filename = ADJUSTL( Atmosphere_Filename )
  WRITE(*,'(/5x,"Writing the Atmosphere data file....")')
  Error_Status = CRTM_Write_Atmosphere_Binary( Atmosphere_Filename, &
                                               Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Atmosphere file '//&
                          TRIM( Atmosphere_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! Test read the Atmosphere binary file
  WRITE(*,'(/5x,"Test reading the Atmosphere data file....")')
  Error_Status = CRTM_Read_Atmosphere_Binary( Atmosphere_Filename, &
                                              Dummy_Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading Atmosphere file '//&
                          TRIM( Atmosphere_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! Destroy all the structures
  Error_Status = Destroy_AtmProfile( AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure.', &
                          WARNING )
  END IF
  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere structure array.', &
                          WARNING )
  END IF
  Error_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying dummy Atmosphere structure arrays.', &
                          WARNING )
  END IF
  DEALLOCATE( Atmosphere, Dummy_Atmosphere, STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating the Atmosphere structure arrays. STAT = ",i0)') &
                   Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  END IF

END PROGRAM AtmProfile2Atmosphere
