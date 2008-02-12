!
! Truncate_AtmProfile
!
! Program to truncate an AtmProfile structure at a user specified pressure level
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Oct-2007
!                       paul.vandelst@noaa.gov

PROGRAM Truncate_AtmProfile

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, &
                                  Display_Message, Program_Message
  USE AtmProfile_Define   , ONLY: AtmProfile_type, &
                                  Destroy_AtmProfile, &
                                  Allocate_AtmProfile
  USE AtmProfile_netCDF_IO, ONLY: Read_AtmProfile_netCDF, &
                                  Write_AtmProfile_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Truncate_AtmProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! LIteral constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp


  ! ---------
  ! Variables
  ! ---------

  CHARACTER(256) :: Message
  CHARACTER(256) :: In_File, Out_File
  INTEGER :: Error_Status
  INTEGER :: n_Levels, n_Layers
  REAL(fp) :: Truncation_Pressure
  TYPE(AtmProfile_type) :: Atm1, Atm2


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to truncate an AtmProfile structure at a user '//&
                        'specified pressure level', &
                        '$Revision$' )

  ! Get user input
  ! --------------
  ! An AtmProfile filename
  WRITE( *,'(/5x,"Enter an netCDF AtmProfile filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) In_File
  In_File = ADJUSTL(In_File)
  IF ( .NOT. File_Exists( TRIM(In_File) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM(In_File)//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! The truncation pressure
  WRITE( *,'(/5x,"Enter the truncation pressure (hPa): ")',ADVANCE='NO' )
  READ( *,* ) Truncation_Pressure
  

  ! Read the AtmProfile file
  ! ------------------------
  Error_Status = Read_AtmProfile_netCDF( In_File, Atm1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading the AtmProfile file '//TRIM(In_File), &
                          FAILURE )
    STOP
  END IF


  ! Determine how many levels/layers to keep
  ! ----------------------------------------
  n_Levels = MINLOC( Truncation_Pressure - Atm1%Level_Pressure(:,1), &
                     DIM=1, &
                     MASK=(Truncation_Pressure - Atm1%Level_Pressure(:,1)) > ZERO ) - 1
  n_Layers = n_Levels-1

  WRITE( *,'(/5x,"Truncating ",i0," levels from AtmProfile data...",/)' ) &
         Atm1%n_Levels - n_Levels
         
         
  ! Allocate the truncated structure
  ! --------------------------------  
  Error_Status = Allocate_AtmProfile( n_Layers        , &  ! Input
                                      Atm1%n_Absorbers, &  ! Input
                                      Atm1%n_Profiles , &  ! Input
                                      Atm2              )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating truncated AtmProfile structure', &
                          FAILURE )
    STOP
  END IF
  
  
  ! Copy structure data
  ! -------------------
  ! Absorber info
  Atm2%Absorber_ID           = Atm1%Absorber_ID
  Atm2%Absorber_Units_ID     = Atm1%Absorber_Units_ID
  Atm2%Absorber_Units_Name   = Atm1%Absorber_Units_Name
  Atm2%Absorber_Units_LBLRTM = Atm1%Absorber_Units_LBLRTM

  ! Profile dependent information
  Atm2%Description       = Atm1%Description
  Atm2%Climatology_Model = Atm1%Climatology_Model
  Atm2%DateTime          = Atm1%DateTime
  Atm2%Location          = Atm1%Location

  ! Profile independent pressures
  Atm2%Level_Pressure    = Atm1%Level_Pressure(1:n_Levels,:)
  Atm2%Layer_Pressure    = Atm1%Layer_Pressure(1:n_Layers,:)

  ! Profile dependent LEVEL data
  Atm2%Level_Temperature = Atm1%Level_Temperature(1:n_Levels,:)
  Atm2%Level_Absorber    = Atm1%Level_Absorber(1:n_Levels,:,:)
  Atm2%Level_Altitude    = Atm1%Level_Altitude(1:n_Levels,:)

  ! Profile dependent LAYER data
  Atm2%Layer_Temperature = Atm1%Layer_Temperature(1:n_Layers,:)
  Atm2%Layer_Absorber    = Atm1%Layer_Absorber(1:n_Layers,:,:)
  Atm2%Layer_Delta_Z     = Atm1%Layer_Delta_Z(1:n_Layers,:)


  ! Write the truncated data to file
  ! --------------------------------
  Out_File = 'Truncated.'//TRIM(In_File)
  Error_Status = Write_AtmProfile_netCDF( Out_File, Atm2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing the AtmProfile file '//TRIM(Out_File), &
                          FAILURE )
    STOP
  END IF
  
  
  ! Clean up
  ! --------
  Error_Status = Destroy_AtmProfile( Atm1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atm1 structure.', &
                          WARNING )
  END IF
  Error_Status = Destroy_AtmProfile( Atm2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atm2 structure.', &
                          WARNING )
  END IF

END PROGRAM Truncate_AtmProfile
