!
! Surface_OLD2NEW
!
! Program to convert old SUrface data file formats to the latest one.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-May-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Surface_OLD2NEW

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Surface_Define
  USE CRTM_Surface_Binary_IO

  USE CRTM_Surface_Define_old, ONLY: Sfc_type     => CRTM_Surface_type, &
                                     Destroy_Sfc  => CRTM_Destroy_Surface, &
                                     Allocate_Sfc => CRTM_Allocate_Surface, &
                                     Assign_Sfc   => CRTM_Assign_Surface
  USE CRTM_Surface_Binary_IO_old, ONLY: Inquire_Sfc => CRTM_Inquire_Surface_Binary, &
                                        Read_Sfc    => CRTM_Read_Surface_Binary, &
                                        Write_Sfc   => CRTM_Write_Surface_Binary
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Surface_OLD2NEW'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id:$'
  INTEGER,      PARAMETER :: SET = 1
  CHARACTER(*), PARAMETER :: OLD_SFC_FILE = 'Old_Data/ECMWF-Surface.bin.old'
  CHARACTER(*), PARAMETER :: NEW_SFC_FILE = 'ECMWF-Surface.bin'


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
  TYPE(Sfc_type)         , ALLOCATABLE :: Sfc_old(:)
  TYPE(CRTM_Surface_type), ALLOCATABLE :: Sfc(:)


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read old format Surface datafiles and '//&
                        'write new format files.', &
                        '$Revision$' )


  ! Inquire the old file
  Error_Status = Inquire_Sfc( OLD_SFC_FILE, n_Locations=n_Profiles)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring old format Surface file.', &
                          FAILURE )
    STOP
  END IF

  ! Allocate the structure arrays
  ALLOCATE( Sfc_old(n_Profiles), Sfc(n_Profiles), &
            STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(Message,'("Error allocating Surface structure arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the old format file
  Error_Status = Read_Sfc( OLD_SFC_FILE, Sfc_old )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading old format Surface file.', &
                          FAILURE )
    STOP
  END IF


  ! Transfer over the data
  DO m = 1, n_Profiles

    Sfc(m)%Land_Coverage         = Sfc_old(m)%Land_Coverage        
    Sfc(m)%Water_Coverage        = Sfc_old(m)%Water_Coverage       
    Sfc(m)%Snow_Coverage         = Sfc_old(m)%Snow_Coverage        
    Sfc(m)%Ice_Coverage          = Sfc_old(m)%Ice_Coverage         
    Sfc(m)%Wind_Speed            = Sfc_old(m)%Wind_Speed           
    Sfc(m)%Land_Type             = Sfc_old(m)%Land_Type            
    Sfc(m)%Land_Temperature      = Sfc_old(m)%Land_Temperature     
    Sfc(m)%Soil_Moisture_Content = Sfc_old(m)%Soil_Moisture_Content
    Sfc(m)%Canopy_Water_Content  = Sfc_old(m)%Canopy_Water_Content 
    Sfc(m)%Vegetation_Fraction   = Sfc_old(m)%Vegetation_Fraction  
    Sfc(m)%Soil_Temperature      = Sfc_old(m)%Soil_Temperature     
    Sfc(m)%Water_Type            = Sfc_old(m)%Water_Type           
    Sfc(m)%Water_Temperature     = Sfc_old(m)%Water_Temperature    
    Sfc(m)%Wind_Direction        = Sfc_old(m)%Wind_Direction       
    Sfc(m)%Salinity              = Sfc_old(m)%Salinity             
    Sfc(m)%Snow_Type             = Sfc_old(m)%Snow_Type            
    Sfc(m)%Snow_Temperature      = Sfc_old(m)%Snow_Temperature     
    Sfc(m)%Snow_Depth            = Sfc_old(m)%Snow_Depth           
    Sfc(m)%Snow_Density          = Sfc_old(m)%Snow_Density         
    Sfc(m)%Snow_Grain_Size       = Sfc_old(m)%Snow_Grain_Size      
    Sfc(m)%Ice_Type              = Sfc_old(m)%Ice_Type             
    Sfc(m)%Ice_Temperature       = Sfc_old(m)%Ice_Temperature      
    Sfc(m)%Ice_Thickness         = Sfc_old(m)%Ice_Thickness        
    Sfc(m)%Ice_Density           = Sfc_old(m)%Ice_Density          
    Sfc(m)%Ice_Roughness         = Sfc_old(m)%Ice_Roughness        

  END DO


  ! Write the new format file
  Error_Status = CRTM_Write_Surface_Binary( NEW_SFC_FILE, Sfc )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing new format Surface file.', &
                          FAILURE )
    STOP
  END IF


  ! Destroy the structure arrays
  Error_Status = CRTM_Destroy_Surface( Sfc )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying new Surface structure array.', &
                          Error_Status )
  END IF

  Error_Status = Destroy_Sfc( Sfc_old )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying old Surface structure array.', &
                          Error_Status )
  END IF

  DEALLOCATE( Sfc_old, Sfc, &
              STAT = Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE(Message,'("Error deallocating Surface structure arrays. STAT = ",i0)') Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
  END IF

END PROGRAM Surface_OLD2NEW
