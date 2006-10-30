!
! Dump_Atmosphere
!
! Program to dump the test CRTM Atmosphere datafile to stdout
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-May-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Dump_Atmosphere


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE File_Utility
  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Dump_Atmosphere'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Dump_Atmosphere.f90,v 1.1 2006/05/08 13:26:08 wd20pd Exp $'
  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF-Atmosphere.Cloud.Aerosol.bin.Big_Endian'
  INTEGER,      PARAMETER :: MAX_N_PROFILES = 52


  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: j, m, mc, ma, n

  TYPE(CRTM_Atmosphere_type),   DIMENSION(MAX_N_PROFILES)   :: Atmosphere


  ! --------------
  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to dump the test CRTM Atmosphere datafile to stdout', &
                       '$Revision: 1.1 $' )

  ! ----------------------------------------
  ! Read the atmosphere structure data files
  ! ----------------------------------------
  WRITE( *, '( /5x, "Reading ECMWF Atmosphere structure file..." )' )
  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILENAME, &
                                              Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Atmosphere structure file '//&
                           ATMOSPHERE_FILENAME, & 
                           Error_Status )
   STOP
  END IF


  ! -------------
  ! Dump the data
  ! -------------
  DO m = 1, MAX_N_PROFILES
    WRITE(*,'(/5x,"Profile #",i3)') m
    WRITE(*,'(10x,"Climatology : ", a )' ) CLIMATOLOGY_MODEL_NAME(Atmosphere(m)%Climatology)
    WRITE(*,'(10x,"Level pressures:",/,20(8f9.3,/))' ) Atmosphere(m)%Level_Pressure
    WRITE(*,'(10x,"Layer pressures:",/,20(8f9.3,/))' ) Atmosphere(m)%Pressure

    WRITE(*,'(//2x,"Press <ENTER> to continue...")' )
    READ(*,*)
  END DO

END PROGRAM Dump_Atmosphere
