!
! Dump_CloudCoeff
!
! Program to dump the contents of a CRTM Binary format CloudCoeff to
! an ASCII file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Dump_CloudCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,         ONLY: Get_Lun, File_Exists
  USE Message_Handler,      ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                  Program_Message, Display_Message
  USE CloudCoeff_Define,    ONLY: CloudCoeff_type, &
                                  Destroy_CloudCoeff
  USE CloudCoeff_Binary_IO, ONLY: Read_CloudCoeff_Binary, &
                                  Write_CloudCoeff_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Dump_CloudCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  CHARACTER(256) :: BIN_Filename
  CHARACTER(256) :: ASC_Filename
  TYPE(CloudCoeff_type) :: C

  ! Set up
  ! ------
  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to dump the contents of a CRTM Binary format '//&
                        'CloudCoeff file to an ASCII dumpfile.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *, FMT     = '( /5x, "Enter the Binary CloudCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_Filename )
  IF ( .NOT. File_Exists( TRIM( BIN_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( BIN_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! Read the binary data file
  WRITE( *, '( /5x, "Reading the Binary CloudCoeff data file ..." )' )
  Error_Status = Read_CloudCoeff_Binary( BIN_Filename, C )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary CloudCoeff file '//&
                          TRIM( BIN_Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Open the output dumpfile
  FileID = Get_Lun()
  IF ( FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error getting free file unit number', &
                          FAILURE )
    STOP
  END IF
  ASC_Filename = TRIM(BIN_Filename)//'.ASCII-dump'
  OPEN( FileID, FILE  =ASC_Filename, &
                STATUS='UNKNOWN', &
                FORM  ='FORMATTED', &
                IOSTAT=IO_Status )
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening file '//TRIM(ASC_Filename), &
                          FAILURE )
    STOP
  END IF

  ! Dump the structure contents
  ! ---------------------------
  ! Release version info
  WRITE(FileID,'("Release.Version: ",i2,".",i2.2)')C%Release,C%Version
  ! Dimensions
  WRITE(FileID,'(   "n_MW_Frequencies   = ",i5,&
                 &/,"n_MW_Radii         = ",i5,&
                 &/,"n_IR_Frequencies   = ",i5,&
                 &/,"n_IR_Radii         = ",i5,&
                 &/,"n_Temperatures     = ",i5,&
                 &/,"n_Densities        = ",i5,&
                 &/,"Max_Legendre_Terms = ",i5,&
                 &/,"n_Legendre_Terms   = ",i5,&
                 &/,"Max_Phase_Elements = ",i5,&
                 &/,"n_Phase_Elements   = ",i5 )' ) &
                 C%n_MW_Frequencies  , &
                 C%n_MW_Radii        , &
                 C%n_IR_Frequencies  , &
                 C%n_IR_Radii        , &
                 C%n_Temperatures    , &
                 C%n_Densities       , &
                 C%Max_Legendre_Terms, &
                 C%n_Legendre_Terms  , &
                 C%Max_Phase_Elements, &
                 C%n_Phase_Elements  
                 
  ! Dimension vector data
  WRITE(FileID,'("Frequency_MW:")')
  WRITE(FileID,100)C%Frequency_MW
  WRITE(FileID,'("Frequency_IR:")')        
  WRITE(FileID,100)C%Frequency_IR
  WRITE(FileID,'("Reff_MW:")')           
  WRITE(FileID,100)C%Reff_MW
  WRITE(FileID,'("Reff_IR:")')           
  WRITE(FileID,100)C%Reff_IR
  WRITE(FileID,'("Temperature:")')       
  WRITE(FileID,100)C%Temperature
  WRITE(FileID,'("Density:")')           
  WRITE(FileID,100)C%Density
  
  ! Microwave data for liquid phase clouds
  WRITE(FileID,'("ke_L_MW:")')          
  WRITE(FileID,100)C%ke_L_MW
  WRITE(FileID,'("w_L_MW:")')            
  WRITE(FileID,100)C%w_L_MW
  WRITE(FileID,'("g_L_MW:")')            
  WRITE(FileID,100)C%g_L_MW
  WRITE(FileID,'("pcoeff_L_MW:")')  
  WRITE(FileID,100)C%pcoeff_L_MW
  
  ! Microwave data for solid phase clouds
  WRITE(FileID,'("ke_S_MW:")')          
  WRITE(FileID,100)C%ke_S_MW
  WRITE(FileID,'("w_S_MW:")')            
  WRITE(FileID,100)C%w_S_MW
  WRITE(FileID,'("g_S_MW:")')            
  WRITE(FileID,100)C%g_S_MW
  WRITE(FileID,'("pcoeff_S_MW:")')  
  WRITE(FileID,100)C%pcoeff_S_MW
  
  ! Infrared data.
  WRITE(FileID,'("ke_IR:")')          
  WRITE(FileID,100)C%ke_IR
  WRITE(FileID,'("w_IR:")')            
  WRITE(FileID,100)C%w_IR
  WRITE(FileID,'("g_IR:")')            
  WRITE(FileID,100)C%g_IR
  WRITE(FileID,'("pcoeff_IR:")')  
  WRITE(FileID,100)C%pcoeff_IR


  ! Clean up
  ! --------
  ! Close the dumpfile
  CLOSE(FileID)
  WRITE(*,'(/5x,a," data dumped to ",a)') TRIM(BIN_Filename), TRIM(ASC_Filename)

  ! Destroy the structures
  Error_Status = Destroy_CloudCoeff(C)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CloudCoeff structure.', &
                          WARNING )
  END IF


  ! Format statements
  ! -----------------
  100 FORMAT(8(1x,es13.6))

END PROGRAM Dump_CloudCoeff
