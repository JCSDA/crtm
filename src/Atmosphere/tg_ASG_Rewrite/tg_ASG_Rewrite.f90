!
! tg_ASG_Rewrite
!
! Program to convert Tom Greenwalds cloudy atmosphere database
! into CRTM Atmosphere and Surface Binary format datafiles
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Apr-2009
!                       paul.vandelst@noaa.gov
!                       Tom Greenwald, CIMSS/SSEC, 21-Apr-2009
!                       tom.greenwald@noaa.gov
!

PROGRAM tg_ASG_Rewrite

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, WARNING, &
                                       Display_Message, Program_Message
  USE File_Utility             , ONLY: Get_Lun, File_Exists
  USE CRTM_Parameters          , ONLY: SET, TWO
  USE CRTM_Atmosphere_Define   , ONLY: H2O_ID, O3_ID, &
                                       CRTM_Atmosphere_type, &
                                       CRTM_Allocate_Atmosphere, &
                                       CRTM_Destroy_Atmosphere, &
                                       CRTM_Get_AbsorberIdx
  USE CRTM_Atmosphere_Binary_IO, ONLY: CRTM_Write_Atmosphere_Binary
  USE CRTM_Surface_Define      , ONLY: CRTM_Surface_type
  USE CRTM_Surface_Binary_IO   , ONLY: CRTM_Write_Surface_Binary
  USE CRTM_GeometryInfo_Define , ONLY: CRTM_GeometryInfo_type, &
                                       CRTM_Assign_GeometryInfo
  USE CRTM_GeometryInfo_IO     , ONLY: CRTM_Read_GeometryInfo, &
                                       CRTM_Write_GeometryInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'tg_ASG_Rewrite'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  ! Default dimensions
  INTEGER, PARAMETER :: N_ABSORBERS = 2  ! H2O and O3
  INTEGER, PARAMETER :: N_AEROSOLS  = 0  ! No aerosols, just clouds
  ! Test GeometryInfo datafile
  CHARACTER(*), PARAMETER :: TEST_GINFO_FILE = 'Test.GeometryInfo.bin'
  INTEGER     , PARAMETER :: N_TEST_GINFO    = 6
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: In_File
  CHARACTER(256) :: atm_File
  CHARACTER(256) :: sfc_File
  CHARACTER(256) :: gInfo_File
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: IO_Status
  INTEGER :: In_File_Id
  INTEGER :: n_Profiles, m
  INTEGER :: n_Layers
  INTEGER :: n_Clouds
  INTEGER :: i, j, k, ii, cloud_layer_idx, icldtype, nrec
  REAL    :: d, re, var, wc
  TYPE(CRTM_GeometryInfo_type) :: test_gInfo(N_TEST_GINFO)
  TYPE(CRTM_Atmosphere_type),   ALLOCATABLE :: atm(:)
  TYPE(CRTM_Surface_type),      ALLOCATABLE :: sfc(:)
  TYPE(CRTM_GeometryInfo_type), ALLOCATABLE :: gInfo(:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        ' Program to convert Tom Greenwalds cloudy atmosphere '//&
                        ' database into CRTM Atmosphere, Surface and GeometryInfo '//&
                        ' Binary format datafiles', &
                        '$Revision$' )


  ! Read the test GeometryInfo file
  Error_Status = CRTM_Read_GeometryInfo( TEST_GINFO_FILE, test_gInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading test gInfo file '//TEST_GINFO_FILE
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF
  
  ! Specify filenames
  ! ...Input filename
  WRITE( *,'(/5x,"Enter the filename to reformat: ")',ADVANCE='NO' )
  READ(*,'(a)') In_File
In_File = '/usr1/wd20tg/ecmwf/ecmwf_profs_winter-ocean.txt'
  IF ( .NOT. File_Exists( In_File ) ) THEN
    Message = 'Input file '//TRIM(In_File)//' not found'
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF
  ! ...Output filenames
  atm_File   = TRIM(In_File)//'.Atmosphere.bin'
  sfc_File   = TRIM(In_File)//'.Surface.bin'
  gInfo_File = TRIM(In_File)//'.GeometryInfo.bin'


  ! Open the input datafile
  In_File_Id = Get_Lun()
  OPEN( In_File_Id, &
        FILE   = In_File, &
        STATUS = 'OLD', &
        FORM   = 'FORMATTED', &
        ACCESS = 'SEQUENTIAL', &
        IOSTAT = IO_Status )
  IF ( IO_Status /= 0 ) THEN
    WRITE( Message,'("Error opening ",a,". IOSTAT = ",i0)' ) TRIM(In_File), IO_Status
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF
    
  ! Allocate the output structure arrays
  ! ...First get the number of profiles from the input file
  READ( In_File_Id,* ) n_Profiles

  ! ...Do the allocation
  ALLOCATE( atm(n_Profiles), &
            sfc(n_Profiles), &
            gInfo(n_Profiles), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating structure arrays. STAT = ",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF

  ! Transfer over the data profile by profile
  Profile_Loop: DO m = 1, n_Profiles

    ! Read the current profile dimensions from the input file

     READ(In_File_ID,*)  ! Profile index
     READ(In_File_ID,*) n_Layers, i, n_Clouds, i ! n_Layers, n_Absorbers, n_Clouds, n_Aerosol

    ! Allocate the output atmosphere structure
    Error_Status = CRTM_Allocate_Atmosphere( n_Layers   , &  ! Input
                                             N_ABSORBERS, &  ! Input
                                             n_Clouds   , &  ! Input
                                             N_AEROSOLS , &  ! Input
                                             Atm(m)       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error allocating Atmosphere structure for profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
      STOP
    END IF


    ! Read the atmosphere and surface data from the input file
    ! ...Explicitly set the absorber indices
    atm(m)%Absorber_Id(1) = H2O_ID
    atm(m)%Absorber_Id(2) = O3_ID
    ! ...Read the data

     IF ( n_Clouds > 0 ) THEN
       READ(In_File_ID,*)  ! Cloudtypes: 1=water, 2=ice, 3=rain, 4=snow'
     END IF
     READ(In_File_ID,*) atm(m)%Climatology ! Climatology
!     write(*,*) atm(m)%Climatology

     READ(In_File_ID,*) ! Absorber_ID(1) for H2O, Absorber_ID(2) for O3'
     READ(In_File_ID,*) ! Absorber units: MASS_MIXING_RATIO_UNITS, VOLUME_MIXING_RATIO_UNITS'
     READ(In_File_ID,*) atm(m)%Level_Pressure(0) ! Set pressure at TOA: Atmosphere%Level_Pressure(0)'
     atm(m)%Level_Pressure(0) = 0.004
!     write(*,*) atm(m)%Level_Pressure(0) ! Set pressure at TOA: Atmosphere%Level_Pressure(0)'
     READ(In_File_ID,*) ! Header
     DO i = 1, n_Layers
       READ(In_File_ID,*) k, atm(m)%Level_Pressure(i), &
    			atm(m)%Pressure(i), &
    			atm(m)%Temperature(i), &
                        ( atm(m)%Absorber(i,j), j=1, N_ABSORBERS)
!       write(*,'(i2,5e12.4)') i, atm(m)%Level_Pressure(i), &
!    			atm(m)%Pressure(i), &
!    			atm(m)%Temperature(i), &
!                        ( atm(m)%Absorber(i,j), j=1, N_ABSORBERS)
     END DO

     READ(In_File_ID,*) ! Surface coverage: Land, Water, Snow, Ice'
     READ(In_File_ID,*) Sfc(m)%Land_Coverage , &
    		        Sfc(m)%Water_Coverage, &
    		        Sfc(m)%Snow_Coverage , &
    		        Sfc(m)%Ice_Coverage
!     write(*,*) Sfc(m)%Land_Coverage , &
!    		        Sfc(m)%Water_Coverage, &
!    		        Sfc(m)%Snow_Coverage , &
!    		        Sfc(m)%Ice_Coverage
     READ(In_File_ID,*) ! Wind speed (m/s)'
     READ(In_File_ID,*) Sfc(m)%Wind_Speed
!      print *, Sfc(m)%Wind_Speed
     READ(In_File_ID,*) ! Land_Type, Land_Temp, Soil_Moisture_Content, Canopy_Water_Content, Vegetation_Fraction, Soil_Temp'
     READ(In_File_ID,*) i, Sfc(m)%Land_Temperature 
     READ(In_File_ID,*) ! Water_Type, Water_Temperature, Wind_Direction, Salinity'
     READ(In_File_ID,*) i, Sfc(m)%Water_Temperature
     READ(In_File_ID,*) ! Snow_Type, Snow_Temperature, Snow_Depth, Snow_Density, Snow_Grain_Size'
     READ(In_File_ID,*) 
     READ(In_File_ID,*) ! Ice_Type,Ice_Temperature,Ice_Thickness,Ice_Density,Ice_Roughness'
     READ(In_File_ID,*)
     IF ( n_Clouds > 0 ) THEN
       READ(In_File_ID,*) ! Cloud information:
       READ(In_File_ID,*) nrec ! Total number of cloud records
       READ(In_File_ID,*) ! cloud_index, cloud_layer_index, cloud_type, effective radius, variance, water path
       DO j = 1, nrec
         READ(In_File_ID,*) i, cloud_layer_idx, icldtype, re, var, wc 
         atm(m)%Cloud(i)%Type = icldtype 
         atm(m)%Cloud(i)%Effective_Radius( cloud_layer_idx ) = re
         atm(m)%Cloud(i)%Water_Content( cloud_layer_idx ) = var
         atm(m)%Cloud(i)%Effective_Variance( cloud_layer_idx ) = wc
!         print *, atm(m)%Cloud(i)%Type, &
!         atm(m)%Cloud(i)%Effective_Radius( cloud_layer_idx ), &
!         atm(m)%Cloud(i)%Water_Content( cloud_layer_idx ), &
!         atm(m)%Cloud(i)%Effective_Variance( cloud_layer_idx )
       END DO
     END IF
 !stop
    ! Copy over gInfo data from the test input
    Error_Status = CRTM_Assign_GeometryInfo( test_gInfo(MOD(m,N_TEST_GINFO)+1), gInfo(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying GeometryInfo structure for profile #",i0)') m
      CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
      STOP
    END IF
    
  END DO Profile_Loop
  
  ! Close the input file
  CLOSE( In_File_Id )
  
  
  ! Write the CRTM Binary format files
  ! ...Atmosphere
  Error_Status = CRTM_Write_Atmosphere_Binary( atm_File, atm )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error writing '//TRIM(atm_File)
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF
  ! ...Surface
  Error_Status = CRTM_Write_Surface_Binary( sfc_File, sfc )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error writing '//TRIM(sfc_File)
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF
  ! ...GeometryInfo
  Error_Status = CRTM_Write_GeometryInfo( gInfo_File, gInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error writing '//TRIM(gInfo_File)
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), FAILURE )
    STOP
  END IF


  ! Clean up
  ! ...Destroy structures
  Error_Status = CRTM_Destroy_Atmosphere( atm )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error destroying Atmosphere structure.'
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), WARNING )
  END IF
  ! ...Deallocate structure arrays
  DEALLOCATE( atm, &
              sfc, &
              gInfo, &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating structure arrays. STAT = ",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, TRIM(Message), WARNING )
    STOP
  END IF
    
END PROGRAM tg_ASG_Rewrite
