PROGRAM CloudCoeff_BIN2NC

  ! This program creates a netCDF CloudCoeff file 
  ! from a binary CloudCoeff file 
  
  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE CloudCoeff_Define
  USE CloudCoeff_Binary_IO
  USE CloudCoeff_netCDF_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'CloudCoeff_BIN2NC'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: CloudCoeff_BIN2NC.f90, 2007/11/20 14:58:34 dgroff Exp $'

  CHARACTER( * ), PARAMETER :: CLOUDCOEFF_BINFILE = 'CloudCoeff.bin'
  CHARACTER( * ), PARAMETER :: CLOUDCOEFF_NETFILE = 'CloudCoeff.nc'
  
  ! The following are parameters used to fill the attributes field.
  ! When a change is made comments must be prepended to the comment
  ! PARAMETER describing what changes were made. Additionally the
  ! history needs to be updated.
  CHARACTER(2000), PARAMETER :: Title = &
  'CRTM Cloud Optical Properties in Infrared and Microwave Range'
  CHARACTER(2000), PARAMETER :: Comment = &
  'All MW and IR liquid phase and solid phase with the density < 0.9 g/cm3' // &
  ' are generated using a MIE code (Simmer, 1994). IR solid phase with the' //&
  ' density = 0.9 g/cm3 is adopted  from non-spherical particle of P. Yang (Liou and Yang, 1995)' //&
  ' The asymmetry factor for non-spherical particles is used for the phase function.'  
  CHARACTER(2000), PARAMETER :: History = &
  '$Id: CloudCoeff_BIN2NC.f90 1323 2007-11-20 15:00:00Z david.groff@noaa.gov$'

  ! ---------
  ! Variables
  ! ---------

  INTEGER :: Error_Status

  TYPE( CloudCoeff_type ) :: CloudCoeff
  
  
 
  !#----------------------------------------------------------------------------#
  !#            -- Inquire about file --                                        #
  !#----------------------------------------------------------------------------#
  
  Error_Status = Inquire_CloudCoeff_Binary( CLOUDCOEFF_BINFILE )
  
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring about CloudCoeff data from Binary file', &
                          Error_Status )
    STOP
  END IF

  !#----------------------------------------------------------------------------#
  !#            -- READ THE CloudCoeff FILE AND COMPARE STRUCTURES --         #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_CloudCoeff_Binary( CLOUDCOEFF_BINFILE, &
                                           CloudCoeff        )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading CloudCoeff data from Binary file', &
                          Error_Status )
    STOP
  END IF

  ! Increment the version number
  CloudCoeff%Version = CloudCoeff%Version + 1
  
  !#----------------------------------------------------------------------------#
  !#               -- WRITE THE CloudCoeff STRUCTURE TO FILE --               #
  !#----------------------------------------------------------------------------#

  Error_Status = Write_CloudCoeff_netCDF( CLOUDCOEFF_NETFILE, &
                                           CloudCoeff,        &       
					   Title = Title,     &
					   History = History, &
					   Comment = Comment  )
					   

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing CloudCoeff structure to netCDF file', &
                          Error_Status )
    STOP
  END IF
 
  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE CloudCoeff STRUCTURES --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_CloudCoeff( CloudCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CloudCoeff structure', &
                          Error_Status )
    STOP
  END IF


END PROGRAM CloudCoeff_BIN2NC

