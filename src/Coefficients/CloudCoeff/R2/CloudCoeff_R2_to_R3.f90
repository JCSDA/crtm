PROGRAM CloudCoeff_R2_to_R3

  USE Message_Handler, ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  ! The old Release 2 software
  USE CloudCoeff_Define_R2   , ONLY: CloudCoeff_type_old => CloudCoeff_type
  USE CloudCoeff_netCDF_IO_R2, ONLY: CloudCoeff_netCDF_ReadFile_old => Read_CloudCoeff_netCDF
  ! The current Release 3 software
  USE CloudCoeff_Define   , ONLY: CloudCoeff_type, &
                                  OPERATOR(==), &
                                  CloudCoeff_Associated, &
                                  CloudCoeff_Create
  USE CloudCoeff_netCDF_IO, ONLY: CloudCoeff_netCDF_WriteFile, CloudCoeff_netCDF_ReadFile
  ! Disable implicit typing
  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_R2_to_R3'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  INTEGER :: Error_Status
  CHARACTER(5000) :: Title, Comment
  CHARACTER(256) :: Filename_old, Filename
  TYPE(CloudCoeff_type_old) :: cc_old
  TYPE(CloudCoeff_type)    :: cc, cc_copy
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF CloudCoeff files from R2 to R3.', &
                        '$Revision$')
  
  ! Get the input filename
  WRITE(*,FMT='(/5x,"Enter the old format netCDF CloudCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') Filename_old
  Filename_old = ADJUSTL(Filename_old)
  Filename = TRIM(Filename_old)//'.new'
  
 
  ! Read the old File
  WRITE(*,'(/5x,"Reading the old file...")')
  Error_Status = CloudCoeff_netCDF_ReadFile_old( Filename_old, cc_old, &
                                                 Title   = Title  , &
                                                 Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading old CloudCoeff file '//&
                          TRIM(Filename_old), &
                          Error_Status )
    STOP
  END IF
  
  ! Copy over the data
  ! ...Allocate the new structure
  CALL CloudCoeff_Create( cc, &
                          cc_old%n_MW_Frequencies, &
                          cc_old%n_MW_Radii      , &
                          cc_old%n_IR_Frequencies, &
                          cc_old%n_IR_Radii      , &
                          cc_old%n_Temperatures  , &
                          cc_old%n_Densities     , &
                          cc_old%n_Legendre_Terms, &
                          cc_old%n_Phase_Elements  )
  IF ( .NOT. CloudCoeff_Associated(cc) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating new structure', &
                          FAILURE )
    STOP
  END IF
  ! ...Copy item by item
  cc%Version      = cc_old%Version
  cc%Frequency_MW = cc_old%Frequency_MW
  cc%Frequency_IR = cc_old%Frequency_IR
  cc%Reff_MW      = cc_old%Reff_MW
  cc%Reff_IR      = cc_old%Reff_IR
  cc%Temperature  = cc_old%Temperature
  cc%Density      = cc_old%Density
  cc%ke_L_MW      = cc_old%ke_L_MW
  cc%w_L_MW       = cc_old%w_L_MW
  cc%g_L_MW       = cc_old%g_L_MW
  cc%pcoeff_L_MW  = cc_old%pcoeff_L_MW
  cc%ke_S_MW      = cc_old%ke_S_MW
  cc%w_S_MW       = cc_old%w_S_MW
  cc%g_S_MW       = cc_old%g_S_MW
  cc%pcoeff_S_MW  = cc_old%pcoeff_S_MW
  cc%ke_IR        = cc_old%ke_IR
  cc%w_IR         = cc_old%w_IR
  cc%g_IR         = cc_old%g_IR
  cc%pcoeff_IR    = cc_old%pcoeff_IR

  ! Write the new File
  WRITE(*,'(/5x,"Writing the new file...")')
  Error_Status = CloudCoeff_netCDF_WriteFile( Filename, cc, &
                                              Title   = TRIM(Title)  , &
                                              History = PROGRAM_VERSION_ID, &
                                              Comment = TRIM(Comment)  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing new CloudCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Check the new file
  WRITE(*,'(/5x,"Checking the new file...")')
  Error_Status = CloudCoeff_netCDF_ReadFile( Filename, cc_copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading new CloudCoeff file '//&
                          TRIM(Filename), &
                          Error_Status )
    STOP
  END IF
  IF ( .NOT. (cc == cc_copy) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Structures are different!', &
                          FAILURE )
    STOP
  END IF

END PROGRAM CloudCoeff_R2_to_R3

