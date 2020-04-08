!
! AtmProfile_Validate_RH
!
! Program to validate relative humidity profiles in AtmProfile files
!
! CREATION HISTORY:
!
!       Created by:     David Groff, EMC/SAIC Nov-2009
!                       david.groff@noaa.gov
!

PROGRAM AtmProfile_Validate_RH

  ! -----------------
  ! Environment setup
  ! -----------------
  
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE AtmProfile_netCDF_IO
  USE AtmProfile_Define
  USE RH_MR
  
  IMPLICIT NONE
  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AtmProfile_Validate_RH'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER, PARAMETER :: LUN = 1
  REAL(fp) :: MAX_RELATIVE_HUMIDITY = 100.0_fp

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(15)  :: AtmProfile_Prefix
  CHARACTER(25)  :: AtmProfile_Filename
  CHARACTER(256) :: RH_Output_Filename
  CHARACTER(10)  :: Profile_Number
  TYPE(AtmProfile_Type), ALLOCATABLE :: AtmProfile(:)
  REAL(fp), ALLOCATABLE :: Relative_Humidity(:)
  INTEGER :: k, m
  INTEGER :: n_Profiles
  INTEGER :: Error_Status, Allocate_Status
  LOGICAL :: RH_Test_Failed
  
  ! Enter The AtmProfile filename
  WRITE( *,FMT='(/5x,"Enter the prefix of an AtmProfile '// &
                 'to get RH profiles for: ")', &
                ADVANCE='NO' ) 
  READ( *,FMT='(a)' ) AtmProfile_Prefix
  
  ! From the prefix construct the AtmProfile_Filename
  AtmProfile_Filename = TRIM(AtmProfile_Prefix)//'.AtmProfile.nc'
  
  ! Allocate AtmProfile 
  Error_Status = AtmProfile_netCDF_InquireFile( AtmProfile_Filename, &
                                                n_Profiles = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring AtmProfile file '//&
                          TRIM(AtmProfile_Filename), &
                          FAILURE )
    STOP
  END IF
                                            
  ALLOCATE( AtmProfile(n_Profiles), STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating AtmProfile array. STAT = ",i0)' ) &
                      Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the AtmProfile file into AtmProfile
  Error_Status = AtmProfile_netCDF_ReadFile( AtmProfile_Filename, AtmProfile )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//&
                          TRIM(AtmProfile_Filename), &
                          FAILURE )
    STOP
  END IF
  
  ! Loop over profiles
  DO m = 1, n_Profiles
     
    ! Loop over layers
    ALLOCATE ( Relative_Humidity(0:AtmProfile(m)%n_Layers), STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error allocating AtmProfile array. STAT = ",i0)' ) &
                        Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    
    WRITE( Profile_Number, FMT='(I4)' ) m
    
    Profile_Number = ADJUSTL(Profile_Number)

    RH_Output_Filename = TRIM(AtmProfile_Prefix)//'_RH_Profile_'//TRIM(Profile_Number)//'.txt'
      
    ! Convert mixing ratio to relative humidity
    CALL MR_to_RH( AtmProfile(m)%Level_Pressure,      &
                   AtmProfile(m)%Level_Temperature,   &
                   AtmProfile(m)%Level_Absorber(:,1), &
                   Relative_Humidity                  )
    
    RH_Test_Failed = ANY(Relative_Humidity > MAX_RELATIVE_HUMIDITY, SIZE(Relative_Humidity))                          
    
    ! Write data if there is a RH test failure
    IF ( RH_Test_Failed ) THEN    
      OPEN(Unit=LUN, FILE=RH_Output_Filename, Action = "WRITE")
      WRITE(LUN, FMT='(a48)'), AtmProfile(m)%Description
      WRITE(LUN, FMT='(F11.5)'), AtmProfile(m)%Latitude
      WRITE(LUN, FMT='(i1)'), AtmProfile(m)%Climatology_Model
      DO k = 0, AtmProfile(m)%n_Layers
        WRITE(LUN, FMT='(4F11.5)') AtmProfile(m)%Level_Pressure(k), &
                                   AtmProfile(m)%Level_Temperature(k), &
                                   AtmProfile(m)%Level_Absorber(k,1), &
                                   Relative_Humidity(k)  
      END DO      
      CLOSE(LUN)                               
    END IF
    
    DEALLOCATE ( Relative_Humidity, STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message,'("Error deallocating Relative Humidity array. STAT = ",i0)' ) &
                        Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    
  END DO
  
  DEALLOCATE ( AtmProfile, STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating AtmProfile array. STAT = ",i0)' ) &
                      Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
END PROGRAM AtmProfile_Validate_RH
