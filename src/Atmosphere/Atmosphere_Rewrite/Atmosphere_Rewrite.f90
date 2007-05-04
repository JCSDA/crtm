!
! Atmosphere_Rewrite
!
! Program to convert old Atmosphere data file formats to the latest one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Atmosphere_Rewrite

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO

  USE CRTM_Atmosphere_Define_old, ONLY: Atm_type     => CRTM_Atmosphere_type, &
                                        Destroy_Atm  => CRTM_Destroy_Atmosphere, &
                                        Allocate_Atm => CRTM_Allocate_Atmosphere, &
                                        Assign_Atm   => CRTM_Assign_Atmosphere
  USE CRTM_Atmosphere_Binary_IO_old, ONLY: Inquire_Atm => CRTM_Inquire_Atmosphere_Binary, &
                                           Read_Atm    => CRTM_Read_Atmosphere_Binary, &
                                           Write_Atm   => CRTM_Write_Atmosphere_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Atmosphere_Rewrite'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Atmosphere_Rewrite.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'
  INTEGER,      PARAMETER :: SET = 1
  CHARACTER(*), PARAMETER :: OLD_ATM_FILE = 'Old_Data/ECMWF-Atmosphere.Cloud.Aerosol.bin.old'
  CHARACTER(*), PARAMETER :: NEW_ATM_FILE = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: Message
  CHARACTER( 256 ) :: Filename_old
  CHARACTER( 256 ) :: Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, nc, na, nna, nam
  INTEGER :: n_Profiles
  INTEGER :: n_Aerosols
  TYPE(Atm_type)            , ALLOCATABLE :: Atm_old(:)
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atm(:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read old format Atmosphere datafiles and '//&
                        'write new format files.', &
                        '$Revision: 1.2 $' )


  ! Inquire the old file
  Error_Status = Inquire_Atm( OLD_ATM_FILE, n_Profiles=n_Profiles)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring old format Atmosphere file.', &
                          FAILURE )
    STOP
  END IF

  ! Allocate the structure arrays
  ALLOCATE( Atm_old(n_Profiles), Atm(n_Profiles), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(Message,'("Error allocating Atmosphere structure arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the old format file
  Error_Status = Read_Atm( OLD_ATM_FILE, Atm_old )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading old format Atmosphere file.', &
                          FAILURE )
    STOP
  END IF


  ! Transfer over the data
  DO m = 1, n_Profiles

    ! Determine the number of aerosols
    ! (treating multiple modes as different aerosols)
      n_Aerosols = 0
    DO na = 1, Atm_old(m)%n_Aerosols
      n_Aerosols = n_Aerosols + Atm_old(m)%Aerosol(na)%n_Modes
    END DO
     
    ! Allocate the new structure
    Error_Status = CRTM_Allocate_Atmosphere( Atm_old(m)%n_Layers   , &  ! Input
                                             Atm_old(m)%n_Absorbers, &  ! Input
                                             Atm_old(m)%n_Clouds   , &  ! Input
                                             n_Aerosols            , &  ! Input
                                             Atm(m)                  )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE(Message,'("Error allocating new Atmosphere structure for profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    ! Copy over the atmosphere bits
    Atm(m)%Climatology    = Atm_old(m)%Climatology   
    Atm(m)%Absorber_ID    = Atm_old(m)%Absorber_ID   
    Atm(m)%Absorber_Units = Atm_old(m)%Absorber_Units
    Atm(m)%Level_Pressure = Atm_old(m)%Level_Pressure
    Atm(m)%Pressure       = Atm_old(m)%Pressure      
    Atm(m)%Temperature    = Atm_old(m)%Temperature   
    Atm(m)%Absorber       = Atm_old(m)%Absorber      

    ! Copy over the cloud bits
    DO nc = 1, Atm_old(m)%n_Clouds
      Atm(m)%Cloud(nc)%Type               = Atm_old(m)%Cloud(nc)%Type              
      Atm(m)%Cloud(nc)%Effective_Radius   = Atm_old(m)%Cloud(nc)%Effective_Radius  
      Atm(m)%Cloud(nc)%Effective_Variance = Atm_old(m)%Cloud(nc)%Effective_Variance
      Atm(m)%Cloud(nc)%Water_Content      = Atm_old(m)%Cloud(nc)%Water_Content     
    END DO
    
    ! Copy over the aerosol bits
    nna = 0
    DO na = 1, Atm_old(m)%n_Aerosols
      DO nam = 1, Atm_old(m)%Aerosol(na)%n_Modes
        nna = nna+1
        Atm(m)%Aerosol(nna)%Type             = Atm_old(m)%Aerosol(na)%Type
        Atm(m)%Aerosol(nna)%Effective_Radius = Atm_old(m)%Aerosol(na)%Effective_Radius(:,nam)
        Atm(m)%Aerosol(nna)%Concentration    = Atm_old(m)%Aerosol(na)%Concentration(:,nam)   
      END DO
    END DO

  END DO


  ! Write the new format file
  Error_Status = CRTM_Write_Atmosphere_Binary( NEW_ATM_FILE, Atm )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing new format Atmosphere file.', &
                          FAILURE )
    STOP
  END IF


  ! Destroy the structure arrays
  Error_Status = CRTM_Destroy_Atmosphere( Atm )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying new Atmosphere structure array.', &
                          Error_Status )
  END IF

  Error_Status = Destroy_Atm( Atm_old )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying old Atmosphere structure array.', &
                          Error_Status )
  END IF

  DEALLOCATE( Atm_old, Atm, &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(Message,'("Error deallocating Atmosphere structure arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
  END IF

END PROGRAM Atmosphere_Rewrite
