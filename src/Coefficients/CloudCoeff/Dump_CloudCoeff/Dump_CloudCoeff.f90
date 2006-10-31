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
  USE CloudCoeff_Binary_IO, ONLY: Read_CloudCoeff_Binary, Write_CloudCoeff_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Dump_CloudCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Dump_CloudCoeff.f90,v 1.3 2006/06/23 22:02:32 wd20pd Exp $'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  CHARACTER(256) :: BIN_Filename
  CHARACTER(256) :: ASC_Filename
  TYPE(CloudCoeff_type) :: C

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to dump the contents of a CRTM Binary format '//&
                        'CloudCoeff file to an ASCII dumpfile.', &
                        '$Revision: 1.3 $' )

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
  !
  ! Release version info
  WRITE(FileID,'("Release.Version: ",i2,".",i2.2)')C%Release,C%Version
  ! Dimensions
  WRITE(FileID,'(   "n_Frequencies      = ",i5,&
                 &/,"n_Reff_MW          = ",i5,&
                 &/,"n_Wavenumbers      = ",i5,&
                 &/,"n_Reff_IR          = ",i5,&
                 &/,"n_Temperatures     = ",i5,&
                 &/,"n_Densities        = ",i5,&
                 &/,"Max_Legendre_Terms = ",i5,&
                 &/,"n_Legendre_Terms   = ",i5,&
                 &/,"Max_Phase_Elements = ",i5,&
                 &/,"n_Phase_Elements   = ",i5 )' ) &
                 C%n_Frequencies     , &
                 C%n_Reff_MW         , &
                 C%n_Wavenumbers     , &
                 C%n_Reff_IR         , &
                 C%n_Temperatures    , &
                 C%n_Densities       , &
                 C%Max_Legendre_Terms, &
                 C%n_Legendre_Terms  , &
                 C%Max_Phase_Elements, &
                 C%n_Phase_Elements  
  ! Data
  WRITE(FileID,'("frequency:")')
  WRITE(FileID,100)C%frequency
  WRITE(FileID,'("wavenumber:")')        
  WRITE(FileID,100)C%wavenumber
  WRITE(FileID,'("Reff_MW:")')           
  WRITE(FileID,100)C%Reff_MW
  WRITE(FileID,'("Reff_IR:")')           
  WRITE(FileID,100)C%Reff_IR
  WRITE(FileID,'("Temperature:")')       
  WRITE(FileID,100)C%Temperature
  WRITE(FileID,'("Density:")')           
  WRITE(FileID,100)C%Density
  WRITE(FileID,'("ext_L_MW:")')          
  WRITE(FileID,100)C%ext_L_MW
  WRITE(FileID,'("w_L_MW:")')            
  WRITE(FileID,100)C%w_L_MW
  WRITE(FileID,'("g_L_MW:")')            
  WRITE(FileID,100)C%g_L_MW
  WRITE(FileID,'("ext_S_MW:")')          
  WRITE(FileID,100)C%ext_S_MW
  WRITE(FileID,'("w_S_MW:")')            
  WRITE(FileID,100)C%w_S_MW
  WRITE(FileID,'("g_S_MW:")')            
  WRITE(FileID,100)C%g_S_MW
  WRITE(FileID,'("phase_coeff_L_MW:")')  
  WRITE(FileID,100)C%phase_coeff_L_MW
  WRITE(FileID,'("phase_coeff_S_MW:")')  
  WRITE(FileID,100)C%phase_coeff_S_MW
  WRITE(FileID,'("ext_L_IR:")')          
  WRITE(FileID,100)C%ext_L_IR
  WRITE(FileID,'("w_L_IR:")')            
  WRITE(FileID,100)C%w_L_IR
  WRITE(FileID,'("g_L_IR:")')            
  WRITE(FileID,100)C%g_L_IR
  WRITE(FileID,'("ext_S_IR:")')          
  WRITE(FileID,100)C%ext_S_IR
  WRITE(FileID,'("w_S_IR:")')            
  WRITE(FileID,100)C%w_S_IR
  WRITE(FileID,'("g_S_IR:")')            
  WRITE(FileID,100)C%g_S_IR
  WRITE(FileID,'("phase_coeff_L_IR:")')  
  WRITE(FileID,100)C%phase_coeff_L_IR
  WRITE(FileID,'("phase_coeff_S_IR:")')  
  WRITE(FileID,100)C%phase_coeff_S_IR

  ! Close the dumpfile
  CLOSE(FileID)

  ! Destroy the structures
  Error_Status = Destroy_CloudCoeff(C)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CloudCoeff structure.', &
                          WARNING )
  END IF

  ! Format statements
  100 FORMAT(8(1x,es13.6))

END PROGRAM Dump_CloudCoeff
