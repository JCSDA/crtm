!
! Dump_Atmosphere
!
! Program to dump a CRTM Atmosphere datafile to stdout
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-May-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Dump_Atmosphere


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE CRTM_Module
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Dump_Atmosphere'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels, n_Profiles
  INTEGER :: j, m, mc, ma, n
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atm(:)


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to dump a CRTM Atmosphere datafile to stdout', &
                       '$Revision$' )

  ! Get a filename
  WRITE( *,FMT='(/5x,"Enter CRTM Atmosphere filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  Filename = ADJUSTL(Filename)
  
  ! Inquire the atmosphere data file
  Error_Status = CRTM_Inquire_Atmosphere_Binary( Filename              , &
                                                 n_Channels =n_Channels, &
                                                 n_Profiles =n_Profiles  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring CRTM Atmosphere file '//TRIM(Filename), &
                          FAILURE )
    STOP
  END IF
  
  ! No channel dimension for this program
  IF ( n_Channels > 0 ) THEN
    WRITE( Message,'("Channel dimension is non-zero, ",i0,". Exiting.")' ) n_Channels
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          INFORMATION )
    STOP
  END IF
  
  ! Allocate the atmosphere structure array
  ALLOCATE( Atm(n_Profiles),STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating CRTM Atmosphere array. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the atmosphere structure data files
  WRITE( *,'(/5x,"Reading CRTM Atmosphere structure file ",a,"...")' ) TRIM(Filename)
  Error_Status = CRTM_Read_Atmosphere_Binary( Filename, Atm )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading CRTM Atmosphere file '//TRIM(Filename), & 
                          FAILURE )
    STOP
  END IF

  ! Dump the data
  DO m = 1, n_Profiles
    WRITE( *,'(/5x,"Profile #",i0)') m
    WRITE( *,'(10x,"Climatology : ", a )' ) CLIMATOLOGY_MODEL_NAME(Atm(m)%Climatology)
    WRITE( *,'(10x,"Level pressures:",/,20(8f9.3,/))' ) Atm(m)%Level_Pressure
    WRITE( *,'(10x,"Pressures:",/,20(8f9.3,/))' ) Atm(m)%Pressure
    WRITE( *,'(10x,"Temperatures:",/,20(8f9.3,/))' ) Atm(m)%Temperature
    DO j = 1, Atm(m)%n_Absorbers
      WRITE( *,'(10x,"Absorber: ",a,/,20(8es10.3,/))' ) &
             TRIM(ABSORBER_ID_NAME(Atm(m)%Absorber_Id(j))), Atm(m)%Absorber(:,j)
    END DO
    WRITE( *,'(//2x,"Press <ENTER> to continue...")' )
    READ(*,*)
  END DO

  ! Clean up
  Error_Status = CRTM_Destroy_Atmosphere( Atm )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred deallocting CRTM Atmosphere structures.', & 
                          WARNING )
    STOP
  END IF
  DEALLOCATE( Atm,STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating CRTM Atmosphere array. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
    STOP
  END IF
  
END PROGRAM Dump_Atmosphere
