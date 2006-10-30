!
! AerosolCoeff_IO_Test
!
! Program to test the CRTM AerosolCoeff I/O functions
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 23-May-2006
!                   paul.vandelst@ssec.wisc.edu
!

PROGRAM AerosolCoeff_IO_Test

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE AerosolCoeff_Define
  USE AerosolCoeff_Binary_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'AerosolCoeff_IO_Test'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: AerosolCoeff_IO_Test.f90,v 1.1 2006/05/23 21:54:40 wd20pd Exp $'


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: AerosolCoeff_File
  INTEGER :: Error_Status
  TYPE(AerosolCoeff_type) :: AerosolCoeff


  ! --------------
  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME,&
                       'Program to test the AerosolCoeff I/O functions', &
                       '$Revision: 1.1 $' )


  ! --------------
  ! Get a filename
  ! --------------
  WRITE(*,'(/5x,"Enter an AerosolCoeff filename: ")',ADVANCE='NO')
  READ(*,'(a)') AerosolCoeff_File
  AerosolCoeff_File = ADJUSTL(AerosolCoeff_File)


  ! -------------
  ! Read the file
  ! -------------
  Error_Status = Read_AerosolCoeff_Binary( AerosolCoeff_File, &
                                           AerosolCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AerosolCoeff data from file '//&
                          TRIM(AerosolCoeff_File), &
                          Error_Status )
    STOP
  END IF


  ! -------------------
  ! Output some numbers
  ! -------------------
  WRITE(*,'(/5x,"Number of channels: ",i5)') AerosolCoeff%n_Channels
  WRITE(*,'( 5x,"Absorption coefficient data:")')
  WRITE(*,'( 4(2x,es13.6) )' ) AerosolCoeff%Absorption
  WRITE(*,'( 5x,"Scattering coefficient data:")')
  WRITE(*,'( 4(2x,es13.6) )' ) AerosolCoeff%Scattering


  ! ---------------------
  ! Destroy the structure
  ! ---------------------
  Error_Status = Destroy_AerosolCoeff( AerosolCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolCoeff structure', &
                          Error_Status )
    STOP
  END IF

END PROGRAM AerosolCoeff_IO_Test

