PROGRAM AerosolCoeff_DummyWrite

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE AerosolCoeff_Define
  USE AerosolCoeff_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'AerosolCoeff_DummyWrite'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: AerosolCoeff_DummyWrite.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'

  CHARACTER( * ), PARAMETER :: AEROSOLCOEFF_FILE = 'dummy.AerosolCoeff.bin'
  INTEGER,        PARAMETER :: N_CHANNELS = 100


  ! ---------
  ! Variables
  ! ---------

  INTEGER :: Error_Status

  TYPE( AerosolCoeff_type ) :: AerosolCoeff
  TYPE( AerosolCoeff_type ) :: AerosolCoeff_Tmp



  !#----------------------------------------------------------------------------#
  !#              -- ALLOCATE AND FILL THE AerosolCoeff STRUCTURE --            #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_AerosolCoeff( N_CHANNELS, AerosolCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating AerosolCoeff structure', &
                          Error_Status )
    STOP
  END IF


  AerosolCoeff%Absorption = -73.0
  AerosolCoeff%Scattering = -99.0



  !#----------------------------------------------------------------------------#
  !#               -- WRITE THE AerosolCoeff STRUCTURE TO FILE --               #
  !#----------------------------------------------------------------------------#

  Error_Status = Write_AerosolCoeff_Binary( AEROSOLCOEFF_FILE, &
                                            AerosolCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing AerosolCoeff structure to Binary file', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- READ THE AerosolCoeff FILE AND COMPARE STRUCTURES --         #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_AerosolCoeff_Binary( AEROSOLCOEFF_FILE, &
                                           AerosolCoeff_Tmp )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AerosolCoeff data from Binary file', &
                          Error_Status )
    STOP
  END IF


  Error_Status = Equal_AerosolCoeff( AerosolCoeff, AerosolCoeff_Tmp )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'The AerosolCoeff structures are different!', &
                          Error_Status )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'The AerosolCoeff structures are equal.', &
                          Error_Status )
  END IF



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE AerosolCoeff STRUCTURES --                 #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AerosolCoeff( AerosolCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolCoeff structure', &
                          Error_Status )
    STOP
  END IF


  Error_Status = Destroy_AerosolCoeff( AerosolCoeff_Tmp )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolCoeff_Tmp structure', &
                          Error_Status )
    STOP
  END IF

END PROGRAM AerosolCoeff_DummyWrite

