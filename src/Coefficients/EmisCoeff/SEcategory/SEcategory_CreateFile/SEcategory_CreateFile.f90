!
! SEcategory_CreateFile
!
! Program to create the Surface Emissivity coefficient files
! for different sensors and surface type categories.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst 15-Aug-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM SEcategory_CreateFile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler, ONLY: Program_Message
  USE IRVISlandCoeff_Module
  USE IRVISsnowCoeff_Module
  USE IRVISiceCoeff_Module
  USE VISwaterCoeff_Module
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SEcategory_CreateFile'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to generate the various SEcategory '//&
                        'reflectance coefficient files.', &
                        '$Revision$' )

  ! Create the files
  WRITE(*,'(//2x,"CREATING IRlandCoeff file...",/)')
  CALL IRVISlandCoeff_CreateFile()
  WRITE(*,'(//2x,"CREATING IRsnowCoeff file...",/)')
  CALL IRVISsnowCoeff_CreateFile()
  WRITE(*,'(//2x,"CREATING IRiceCoeff file...",/)')
  CALL IRVISiceCoeff_CreateFile()
  WRITE(*,'(//2x,"CREATING VISlandCoeff file...",/)')
  CALL IRVISlandCoeff_CreateFile(visible=.TRUE.)
  WRITE(*,'(//2x,"CREATING VISsnowCoeff file...",/)')
  CALL IRVISsnowCoeff_CreateFile(visible=.TRUE.)
  WRITE(*,'(//2x,"CREATING VISiceCoeff file...",/)')
  CALL IRVISiceCoeff_CreateFile(visible=.TRUE.)
  WRITE(*,'(//2x,"CREATING VISwaterCoeff file...",/)')
  CALL VISwaterCoeff_CreateFile()

END PROGRAM SEcategory_CreateFile
