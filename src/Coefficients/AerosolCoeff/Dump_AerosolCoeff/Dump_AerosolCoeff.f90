!
! Dump_AerosolCoeff
!
! Program to dump the contents of a CRTM Binary format AerosolCoeff to
! an ASCII file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jun-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Dump_AerosolCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,           ONLY: Get_Lun, File_Exists
  USE Message_Handler,        ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                    Program_Message, Display_Message
  USE AerosolCoeff_Define,    ONLY: AerosolCoeff_type, &
                                    Destroy_AerosolCoeff
  USE AerosolCoeff_Binary_IO, ONLY: Read_AerosolCoeff_Binary, &
                                    Write_AerosolCoeff_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Dump_AerosolCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID, i
  CHARACTER(256) :: BIN_Filename
  CHARACTER(256) :: ASC_Filename
  TYPE(AerosolCoeff_type) :: A

  ! Set up
  ! ------
  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to dump the contents of a CRTM Binary format '//&
                        'AerosolCoeff file to an ASCII dumpfile.', &
                        '$Revision$' )

  ! Get the filename
  WRITE( *, FMT     = '( /5x, "Enter the Binary AerosolCoeff file: " )', &
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
  WRITE( *, '( /5x, "Reading the Binary AerosolCoeff data file ..." )' )
  Error_Status = Read_AerosolCoeff_Binary( BIN_Filename, A )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Binary AerosolCoeff file '//&
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
  WRITE(FileID,'("Release.Version: ",i2,".",i2.2)')A%Release,A%Version
  ! Dimensions
  WRITE(FileID,'(   "n_Wavelengths      = ",i5,&
                 &/,"n_Radii            = ",i5,&
                 &/,"n_Types            = ",i5,&
                 &/,"n_RH               = ",i5,&
                 &/,"Max_Legendre_Terms = ",i5,&
                 &/,"n_Legendre_Terms   = ",i5,&
                 &/,"Max_Phase_Elements = ",i5,&
                 &/,"n_Phase_Elements   = ",i5 )' ) &
                 A%n_Wavelengths     , &
                 A%n_Radii           , &
                 A%n_Types           , &
                 A%n_RH              , &
                 A%Max_Legendre_Terms, &
                 A%n_Legendre_Terms  , &
                 A%Max_Phase_Elements, &
                 A%n_Phase_Elements  
                 
  ! Dimension vector data
  WRITE(FileID,'("Wavelength:")')
  WRITE(FileID,100)A%Wavelength
  WRITE(FileID,'("Frequency:")')        
  WRITE(FileID,100)A%Frequency
  WRITE(FileID,'("RH:")')           
  WRITE(FileID,100)A%RH
  
  ! Optical property data
  DO i = 1, A%n_Types
    WRITE(FileID,'(/,"Optical property data for ",a," aerosols")') TRIM(A%Type_Name(i))
    WRITE(FileID,'("Reff:")')         
    WRITE(FileID,100)A%Reff(:,i)
    WRITE(FileID,'("ke:")')        
    WRITE(FileID,100)A%ke(:,:,i)
    WRITE(FileID,'("w:")')          
    WRITE(FileID,100)A%w(:,:,i)
    WRITE(FileID,'("g:")')          
    WRITE(FileID,100)A%g(:,:,i)
    WRITE(FileID,'("pcoeff:")') 
    WRITE(FileID,100)A%pcoeff(:,:,i,:,:)
  END DO


  ! Clean up
  ! --------
  ! Close the dumpfile
  CLOSE(FileID)
  WRITE(*,'(/5x,a," data dumped to ",a)') TRIM(BIN_Filename), TRIM(ASC_Filename)

  ! Destroy the structures
  Error_Status = Destroy_AerosolCoeff(A)
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolCoeff structure.', &
                          WARNING )
  END IF


  ! Format statements
  ! -----------------
  100 FORMAT(8(1x,es13.6))

END PROGRAM Dump_AerosolCoeff
