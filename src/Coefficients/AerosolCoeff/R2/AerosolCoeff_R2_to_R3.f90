PROGRAM AerosolCoeff_R2_to_R3

  USE Message_Handler, ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  ! The old Release 2 software
  USE AerosolCoeff_Define_R2   , ONLY: AerosolCoeff_type_R2 => AerosolCoeff_type
  USE AerosolCoeff_netCDF_IO_R2, ONLY: AerosolCoeff_netCDF_ReadFile_R2 => Read_AerosolCoeff_netCDF
  ! The current Release 3 software
  USE AerosolCoeff_Define      , ONLY: AerosolCoeff_type, &
                                       OPERATOR(==), &
                                       AerosolCoeff_Associated, &
                                       AerosolCoeff_Create
  USE AerosolCoeff_netCDF_IO   , ONLY: AerosolCoeff_netCDF_WriteFile, AerosolCoeff_netCDF_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AerosolCoeff_R2_to_R3'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  INTEGER :: Error_Status
  CHARACTER(5000) :: Title, History, Comment
  CHARACTER(256) :: Filename_R2, Filename
  TYPE(AerosolCoeff_type_R2) :: ac_R2
  TYPE(AerosolCoeff_type)    :: ac, ac_copy
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF AerosolCoeff files from R2 to R3.', &
                        '$Revision$')
  
  ! Get the input filename
  WRITE(*,FMT='(/5x,"Enter the old format netCDF AerosolCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') Filename_R2
  Filename_R2 = ADJUSTL(Filename_R2)
  Filename = TRIM(Filename_R2)//'.new'
  
 
  ! Read the old File
  WRITE(*,'(/5x,"Reading the old file...")')
  Error_Status = AerosolCoeff_netCDF_ReadFile_R2( Filename_R2, ac_R2, &
                                                  Title   = Title  , &
                                                  History = History, &
                                                  Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading old AerosolCoeff file '//&
                          TRIM(Filename_R2), &
                          Error_Status )
    STOP
  END IF
  
  ! Copy over the data
  ! ...Allocate the new structure
  CALL AerosolCoeff_Create( &
         ac, &
         ac_R2%n_Wavelengths   , &
         ac_R2%n_Radii         , &
         ac_R2%n_Types         , &
         ac_R2%n_RH            , &
         ac_R2%n_Legendre_Terms, &
         ac_R2%n_Phase_Elements  )
  IF ( .NOT. AerosolCoeff_Associated(ac) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating new structure', &
                          FAILURE )
    STOP
  END IF
  ! ...Copy item by item
  ac%Version     = ac_R2%Version
  ac%Data_Source = 'GOCART'
  ac%Type        = ac_R2%Type
  ac%Type_Name   = ac_R2%Type_Name
  ac%Wavelength  = ac_R2%Wavelength
  ac%Frequency   = ac_R2%Frequency
  ac%Reff        = ac_R2%Reff
  ac%RH          = ac_R2%RH
  ac%ke          = ac_R2%ke
  ac%w           = ac_R2%w
  ac%g           = ac_R2%g
  ac%pcoeff      = ac_R2%pcoeff

  ! Write the new File
  WRITE(*,'(/5x,"Writing the new file...")')
  Error_Status = AerosolCoeff_netCDF_WriteFile( Filename, ac, &
                                                Title   = TRIM(Title)  , &
                                                History = PROGRAM_VERSION_ID//'; '//&
                                                          TRIM(History), &
                                                Comment = TRIM(Comment)  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing new AerosolCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Check the new file
  WRITE(*,'(/5x,"Checking the new file...")')
  Error_Status = AerosolCoeff_netCDF_ReadFile( Filename, ac_copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading new AerosolCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF
  IF ( .NOT. (ac == ac_copy) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Structures are different!', &
                          FAILURE )
    STOP
  END IF

END PROGRAM AerosolCoeff_R2_to_R3

