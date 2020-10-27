!
! Create_AerosolCoeff
!
! Program to create the netCDF AerosolCoeff datafile from ASCII data.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst 27-Mar-2008 paul.vandelst@noaa.gov
!                       Adapted from Test_AersoolCoeff.f90 modified
!                       by Quanhua Liu quanhua.liu@noaa.gov
!

PROGRAM Create_AerosolCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility
  USE AerosolCoeff_Define
  USE AerosolCoeff_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Create_AerosolCoeff'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: INPUT_FILENAME  = 'Aerosol_Optical_Properties_IR.dat'
  CHARACTER(*), PARAMETER :: OUTPUT_FILENAME = 'AerosolCoeff.nc'
  

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(1000) :: Title
  CHARACTER(1000) :: History
  CHARACTER(1000) :: Comment
  CHARACTER(1000) :: Define_RCS_Id
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  INTEGER :: File_Version
  INTEGER :: i, j, k, l
  INTEGER :: n_Types         
  INTEGER :: n_RH            
  INTEGER :: n_Radii         
  INTEGER :: n_Wavelengths   
  INTEGER :: n_Legendre_Terms
  INTEGER :: n_Phase_Elements  
  TYPE(AerosolCoeff_type) :: Aerosol


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the netCDF AerosolCoeff file from '//&
                        'ASCII data files.', &
                        '$Revision$' )


  ! Get user inputs
  ! ---------------
  WRITE( *,'(/5x,"Default AerosolCoeff file version is: ",i0,". Enter value: ")', &
           ADVANCE='NO' ) Aerosol%Version
  READ( *,* ) File_Version
  IF ( File_Version < Aerosol%Version ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid version number specified. Using default.', &
                          INFORMATION )
    File_Version = Aerosol%Version
  END IF
  
  
  ! Open the input data file
  ! ------------------------
  FileID = Get_Lun()
  OPEN( FileID, FILE  =INPUT_FILENAME, &
                STATUS='OLD'         , &
                FORM  ='FORMATTED'   , &
                ACCESS='SEQUENTIAL'  , &
                ACTION='READ'        , &
                IOSTAT=IO_Status       )
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening input data file '//INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! Allocate the output AerosolCoeff structure
  ! ------------------------------------------
  WRITE( *,'(/5x,"Allocating AerosolCoeff structure...")' )
  ! Read the dimensions from the input file
  READ( FileID,'(6i8)',IOSTAT=IO_Status ) n_Types         , &
                                          n_RH            , &
                                          n_Radii         , &
                                          n_Wavelengths   , &
                                          n_Legendre_Terms, &
                                          n_Phase_Elements
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading dimensions from '//INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! Perform the allocation
  Error_Status = Allocate_AerosolCoeff( n_Wavelengths       , &  ! Input
                                        n_Radii             , &  ! Input
                                        n_Types             , &  ! Input
                                        n_RH                , &  ! Input
                                        n_Legendre_Terms    , &  ! Input
                                        n_Phase_Elements    , &  ! Input
                                        Aerosol             , &  ! Output
                                        RCS_Id=Define_RCS_Id  )  ! Revision control
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating AerosolCoeff structure array.', &
                          FAILURE )
    STOP
  END IF

  ! Read the aerosol optical property data
  ! --------------------------------------
  WRITE( *,'(/5x,"Reading data from ASCII data file...")' )
  ! Aerosol type definitions
  DO i = 1, n_Types
    READ( FileID,'(i2,1x,a)',IOSTAT=IO_Status ) Aerosol%Type(i), Aerosol%Type_Name(i)
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading aerosol type definitions from '//&
                            INPUT_FILENAME, &
                            FAILURE )
      STOP
    END IF
  END DO
  ! The relative humidity dimension vector
  READ( FileID,*,IOSTAT=IO_Status ) Aerosol%RH
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading relative humidity dimension vector from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! The wavelength dimension vector
  READ( FileID,*,IOSTAT=IO_Status ) Aerosol%Wavelength
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading wavelength dimension vector from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  Aerosol%Frequency = 10000.0_fp/Aerosol%Wavelength
  ! The effective radii dimension array
  READ( FileID,*,IOSTAT=IO_Status ) ((Aerosol%Reff(i,j), i=1,n_Radii), j=1,n_Types)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading effective radii dimension vectors from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! The extinction coefficient data
  READ( FileID,*,IOSTAT=IO_Status ) (((Aerosol%ke(i,j,k), j=1,n_Radii), &
                                                          k=1,n_Types), &
                                                          i=1,n_Wavelengths)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading extinction coefficient data from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! The single scatter albedo data
  READ( FileID,*,IOSTAT=IO_Status ) (((Aerosol%w(i,j,k), j=1,n_Radii), &
                                                         k=1,n_Types), &
                                                         i=1,n_Wavelengths)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading single scatter albedo data from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! The asymmetry parameter data
  READ( FileID,*,IOSTAT=IO_Status ) (((Aerosol%g(i,j,k), j=1,n_Radii), &
                                                         k=1,n_Types), &
                                                         i=1,n_Wavelengths)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading asymmetry parameter data from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! The phase coefficient data
  READ( FileID,*,IOSTAT=IO_Status ) ((((Aerosol%pcoeff(i,j,k,l,1), l=0,n_Legendre_Terms), &
                                                                   j=1,n_Radii), &
                                                                   k=1,n_Types), &
                                                                   i=1,n_Wavelengths)
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading phase coefficient data from '//&
                          INPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF
  ! Close the input file
  CLOSE( FileID )
  


  ! Write the data to netCDF file
  ! -----------------------------
  WRITE( *,'(/5x,"Writing netCDF AerosolCoeff data file...")' )
  ! Set the netCDF attributes
  Title   = 'Aerosol Optical Properties in the infrared spectral region.'
  History = TRIM(Define_RCS_Id)//'; '//PROGRAM_RCS_ID
  Comment = 'Spherical particles are assumed. The GOCART aerosol types are adopted.'
  ! Explicitly assign the file version
  Aerosol%Version = File_Version
  ! Write the data file
  Error_Status = Write_AerosolCoeff_netCDF( OUTPUT_FILENAME, &
                                            Aerosol, &
                                            Title  =TRIM(Title)  , &
                                            History=TRIM(History), &
                                            Comment=TRIM(Comment)  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing AerosolCoeff netCDF data file '//&
                          OUTPUT_FILENAME, &
                          FAILURE )
    STOP
  END IF


  ! Cleanup
  ! -------
  Error_Status = Destroy_AerosolCoeff( Aerosol )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolCoeff structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Create_AerosolCoeff
