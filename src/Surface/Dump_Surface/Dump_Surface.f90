!
! Dump_Surface
!
! Program to dump a CRTM Surface datafile to stdout
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Nov-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Dump_Surface


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
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Dump_Surface'
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
  INTEGER :: i, m, iDump
  TYPE(CRTM_Surface_type), ALLOCATABLE :: Sfc(:)


  ! Program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to dump a CRTM Surface datafile to stdout', &
                       '$Revision$' )

  ! Get a filename
  WRITE( *,'(/5x,"Enter CRTM Surface filename: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Filename
  Filename = ADJUSTL(Filename)
  
  ! Inquire the Surface data file
  Error_Status = CRTM_Inquire_Surface_Binary( Filename              , &
                                                 n_Channels =n_Channels, &
                                                 n_Profiles =n_Profiles  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring CRTM Surface file '//TRIM(Filename), &
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
  
  ! Allocate the Surface structure array
  ALLOCATE( Sfc(n_Profiles),STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating CRTM Surface array. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the Surface structure data files
  WRITE( *,'(/5x,"Reading CRTM Surface structure file ",a,"...")' ) TRIM(Filename)
  Error_Status = CRTM_Read_Surface_Binary( Filename, Sfc )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading CRTM Surface file '//TRIM(Filename), & 
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
        CALL DumpSfc(m)
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
        CALL DumpSfc(m)
      END DO Any_Loop
  END SELECT

  ! Clean up
  Error_Status = CRTM_Destroy_Surface( Sfc )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred deallocting CRTM Surface structures.', & 
                          WARNING )
    STOP
  END IF
  DEALLOCATE( Sfc,STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating CRTM Surface array. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          WARNING )
    STOP
  END IF
  
CONTAINS

  SUBROUTINE DumpSfc(m)
    INTEGER, INTENT(IN) :: m
    WRITE( *,'(/2x,"Profile #",i0)') m
    WRITE( *,'(4x,"Coverage Fraction (Land,Water,Snow,Ice):",4(1x,f4.2))') Sfc(m)%Land_Coverage , &
                                                                           Sfc(m)%Water_Coverage, &
                                                                           Sfc(m)%Snow_Coverage , &
                                                                           Sfc(m)%Ice_Coverage  

    WRITE( *,'(4x,"Wind speed (m.s^-1)       : ",f8.3)') Sfc(m)%Wind_Speed
    WRITE( *,'(4x,"Wind dirn. (deg.N)        : ",f8.3)') Sfc(m)%Wind_Direction

    IF ( Sfc(m)%Land_Coverage > ZERO ) THEN
      WRITE( *,'(/4x,"Land type: ",a)') LAND_TYPE_NAME(Sfc(m)%Land_Type)
      WRITE( *,'(6x,"Land temperature (K)    : ",f8.3)') Sfc(m)%Land_Temperature     
      WRITE( *,'(6x,"Soil moisture (g.cm^-3) : ",f8.3)') Sfc(m)%Soil_Moisture_Content
      WRITE( *,'(6x,"Canopy water (g.cm^-3)  : ",f8.3)') Sfc(m)%Canopy_Water_Content 
      WRITE( *,'(6x,"Veg. fraction           : ",f8.3)') Sfc(m)%Vegetation_Fraction  
      WRITE( *,'(6x,"Soil temperature (K)    : ",f8.3)') Sfc(m)%Soil_Temperature     
    END IF

    IF ( Sfc(m)%Water_Coverage > ZERO ) THEN
      WRITE( *,'(/4x,"Water type: ",a)') WATER_TYPE_NAME(Sfc(m)%Water_Type)
      WRITE( *,'(6x,"Water temperature (K)   : ",f8.3)') Sfc(m)%Water_Temperature
      WRITE( *,'(6x,"Salinitye (ppmv)        : ",f8.3)') Sfc(m)%Salinity         
    END IF

    IF ( Sfc(m)%Snow_Coverage > ZERO ) THEN
      WRITE( *,'(/4x,"Snow type: ",a)') SNOW_TYPE_NAME(Sfc(m)%Snow_Type)
      WRITE( *,'(6x,"Snow temperature (K)    : ",f8.3)') Sfc(m)%Snow_Temperature
      WRITE( *,'(6x,"Snow depth (mm)         : ",f8.3)') Sfc(m)%Snow_Depth      
      WRITE( *,'(6x,"Snow density (g.cm^-3)  : ",f8.3)') Sfc(m)%Snow_Density    
      WRITE( *,'(6x,"Snow grain size (mm)    : ",f8.3)') Sfc(m)%Snow_Grain_Size 
    END IF

    IF ( Sfc(m)%Ice_Coverage > ZERO ) THEN
      WRITE( *,'(/4x,"Ice type: ",a)') ICE_TYPE_NAME(Sfc(m)%Ice_Type)
      WRITE( *,'(6x,"Ice temperature (K)     : ",f8.3)') Sfc(m)%Ice_Temperature
      WRITE( *,'(6x,"Ice thickness (mm)      : ",f8.3)') Sfc(m)%Ice_Thickness  
      WRITE( *,'(6x,"Ice density (g.cm^-3)   : ",f8.3)') Sfc(m)%Ice_Density    
      WRITE( *,'(6x,"Ice roughness           : ",f8.3)') Sfc(m)%Ice_Roughness  
    END IF

  END SUBROUTINE DumpSfc
  
END PROGRAM Dump_Surface
