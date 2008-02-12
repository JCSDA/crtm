!
! Test_AddLayers
!
! Program to test the CRTM Atmosphere AddLayers function.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 25-Oct-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_AddLayers

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                       Display_Message, PRogram_MEssage
  USE File_Utility
  USE CRTM_Parameters          , ONLY: SET
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO
  USE CRTM_Atmosphere
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_AddLayers'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: IN_FILENAME = 'Truncated.ECMWF.Atmosphere.bin'
  CHARACTER(*), PARAMETER :: OUT_FILENAME = 'Extra_Layers.ECMWF.Atmosphere.bin'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: m, n_Profiles
  TYPE(CRTM_Atmosphere_type), ALLOCATABLE :: Atm_In(:), Atm_Out(:)

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM Atmosphere AddLayers function.', &
                        '$Revision$' )


  ! Read the atmosphere data file
  ! -----------------------------
  WRITE( *,'(/5x,"Reading Atmosphere structure file ",a)' ) IN_FILENAME
  
  ! Determine the number of profiles
  Error_Status = CRTM_Inquire_Atmosphere_Binary( IN_FILENAME, n_Profiles=n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring Atmosphere structure file '//&
                          IN_FILENAME, & 
                          Error_Status )
    STOP
  END IF

  ! Allocate the structures
  ALLOCATE( Atm_In(n_Profiles), Atm_Out(n_Profiles), STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating Atmosphere structures. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), & 
                          Error_Status )
    STOP
  END IF

  ! Fill the input Atmosphere structure  
  Error_Status = CRTM_Read_Atmosphere_Binary( IN_FILENAME, Atm_In )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           IN_FILENAME, & 
                           Error_Status )
   STOP
  END IF
  
  
  ! Add extra layers to atmosphere data
  ! -----------------------------------
  WRITE( *,'(/5x,"Adding extra layers....")' )
  
  Profile_Loop: DO m = 1, n_Profiles
    Error_Status = CRTM_AddLayers_Atmosphere( Atm_In(m), Atm_Out(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error adding layers for profile ",i0)' ) m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), & 
                            Error_Status )
      STOP
    END IF
    WRITE( *,'(10x,"Profile ",i0," top pressure: ",es13.6,". No. of added layers: ",i0)' ) &
           m, Atm_In(m)%Level_Pressure(0), Atm_Out(m)%n_Layers - Atm_In(m)%n_Layers
  END DO Profile_Loop


  ! Write the output Atmosphere structure
  ! -------------------------------------
  WRITE( *,'(/5x,"Writing test Atmosphere datafile ..." )' )
  Error_Status = CRTM_Write_Atmosphere_Binary( OUT_FILENAME, Atm_Out )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing output Atmosphere data file '//OUT_FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! Clean up
  ! --------
  ! Destroy the structure arrays
  Error_Status = CRTM_Destroy_Atmosphere( Atm_In )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying input Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = CRTM_Destroy_Atmosphere( Atm_Out )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying output Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF

  ! Deallocate the structure arrays
  DEALLOCATE( Atm_In, Atm_Out, STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error deallocating Atmosphere structures. STAT=",i0)' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), & 
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_AddLayers
