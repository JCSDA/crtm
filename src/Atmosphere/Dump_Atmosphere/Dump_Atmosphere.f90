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
  INTEGER, PARAMETER :: N_DUMP_TYPES=2
  INTEGER, PARAMETER :: ALL_DUMP = 1
  INTEGER, PARAMETER :: ANY_DUMP = 2
  CHARACTER(*), PARAMETER :: DUMP_TYPE_NAME(N_DUMP_TYPES)=(/'All profiles, one by one', &
                                                            'Any profile by number   '/)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Filename
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Channels, n_Profiles
  INTEGER :: i, j, m, mc, ma, n, iDump
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atm(:)


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to dump a CRTM Atmosphere datafile to stdout', &
                       '$Revision$' )

  ! Get a filename
  WRITE( *,'(/5x,"Enter CRTM Atmosphere filename: ")',ADVANCE='NO' )
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

  ! Determine dump type
  WRITE( *,'(/5x,"Select profile dump type:")' )
  DO i = 1, N_DUMP_TYPES
    WRITE( *,'(10x,i0,") ",a)' ) i, TRIM(DUMP_TYPE_NAME(i))
  END DO
  WRITE( *,'(5x,"Enter choice: ")',ADVANCE='NO' )
  READ( *,* ) iDump
  iDump = MAX(MIN(iDump,N_DUMP_TYPES),1)
  

  ! Branch on dump type
  SELECT CASE(iDump)
    ! All profiles, one by one
    CASE(ALL_DUMP)
      DO m = 1, n_Profiles
        CALL DumpAtm(m)
        WRITE( *,'(//2x,"Press <ENTER> to continue...")' )
        READ(*,*)
      END DO
    ! Single profile by number
    CASE(ANY_DUMP)
      Any_Loop: DO
        WRITE( *,'(/5x,"Enter profile number [-ve to quit]: ")',ADVANCE='NO' )
        READ( *,* ) m
        IF ( m < 0 ) EXIT Any_Loop
        m = MAX(MIN(m,n_Profiles),1)
        CALL DumpAtm(m)
      END DO Any_Loop
  END SELECT

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
  
CONTAINS

  SUBROUTINE DumpAtm(m)
    INTEGER, INTENT(IN ) :: m
    WRITE( *,'(/5x,"Profile #",i0)') m
    WRITE( *,'(10x,"n_Layers(Max,Add): ",i0,"(",i0,",",i0,")")') Atm(m)%n_Layers, Atm(m)%Max_Layers, Atm(m)%n_Added_Layers
    WRITE( *,'(10x,"n_Absorbers      : ",i0           )') Atm(m)%n_Absorbers
    WRITE( *,'(10x,"n_Clouds(Max)    : ",i0,"(",i0,")")') Atm(m)%n_Clouds, Atm(m)%Max_Clouds
    WRITE( *,'(10x,"n_Aerosols(Max)  : ",i0,"(",i0,")")') Atm(m)%n_Aerosols, Atm(m)%Max_Aerosols
    WRITE( *,'(10x,"Climatology      : ", a )' ) CLIMATOLOGY_MODEL_NAME(Atm(m)%Climatology)
    WRITE( *,'(10x,"Level pressures:",/,20(8f9.3,/))' ) Atm(m)%Level_Pressure
    WRITE( *,'(10x,"Pressures:",/,20(8f9.3,/))' ) Atm(m)%Pressure
    WRITE( *,'(10x,"Temperatures:",/,20(8f9.3,/))' ) Atm(m)%Temperature
    DO j = 1, Atm(m)%n_Absorbers
      WRITE( *,'(10x,"Absorber: ",a,/,20(8es10.3,/))' ) &
             TRIM(ABSORBER_ID_NAME(Atm(m)%Absorber_Id(j))), Atm(m)%Absorber(:,j)
    END DO
  END SUBROUTINE DumpAtm
  
END PROGRAM Dump_Atmosphere
