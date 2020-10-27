!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Forward
!
! PURPOSE:
!       Program to test the CRTM Forward code.
!
! CATEGORY:
!       CRTM : Test : Forward
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:     Module containing definitions for kinds
!                       of variable types.
!
!       Message_Handler:Module to define simple error codes and
!                       handle error conditions
!                       USEs: FILE_UTILITY module
!
!       CRTM_Module:    The main CRTM module.
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!
!       CRTM_Surface_Binary_IO:    Module to read data into a CRTM_Surface
!                                  structure/array
!
! CONTAINS:
!       Print_ChannelInfo:  Subroutine to print the contents of the
!                           ChannelInfo structure returned from the
!                           initialisation.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

PROGRAM Test_Forward


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM module
  USE CRTM_Module


  ! -- Modules to read in Atmosphere and Surface data
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Surface_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Forward'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER(*), PARAMETER :: ATMOSPHERE_FILENAME = 'ECMWF.Atmosphere.bin.Big_Endian'
  CHARACTER(*), PARAMETER ::    SURFACE_FILENAME = 'Surface.bin.Big_Endian'
  INTEGER,      PARAMETER :: MAX_N_PROFILES = 52


  ! ---------
  ! Variables
  ! ---------

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n_Stokes

  CHARACTER( 256 ) :: File_Prefix
  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: AerosolCoeff_File
  CHARACTER( 256 ) :: ScatterCoeff_File

  TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo

  TYPE( CRTM_Atmosphere_type ),   DIMENSION( MAX_N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Surface_type ),      DIMENSION( MAX_N_PROFILES ) :: Surface
  TYPE( CRTM_GeometryInfo_type ), DIMENSION( MAX_N_PROFILES ) :: GeometryInfo

  TYPE( CRTM_RTSolution_type ), ALLOCATABLE, DIMENSION( :, : ) :: RTSolution



  !#----------------------------------------------------------------------------#
  !#         -- READ THE Atmosphere AND Surface STRUCTURE DATA FILES --         #
  !#----------------------------------------------------------------------------#

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



  WRITE( *, '( /5x, "Reading Surface structure file..." )' )

  Error_Status = CRTM_Read_Surface_Binary( SURFACE_FILENAME, &
                                           Surface )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error reading Surface structure file '//&
                           Surface_FILENAME, & 
                           Error_Status )
   STOP
  END IF

  Surface%Land_Coverage = Surface(1)%Land_Coverage
  Surface%Water_Coverage = Surface(1)%Water_Coverage
  Surface%Snow_Coverage = Surface(1)%Snow_Coverage
  Surface%Ice_Coverage = Surface(1)%Ice_Coverage

  Surface%Land_Temperature = Surface(1)%Land_Temperature
  Surface%Water_Temperature = Surface(1)%Water_Temperature
  Surface%Snow_Temperature = Surface(1)%Snow_Temperature
  Surface%Ice_Temperature = Surface(1)%Ice_Temperature



  !#----------------------------------------------------------------------------#
  !#                     -- GET THE COEFFICIENT FILENAMES --                    #
  !#----------------------------------------------------------------------------#

  ! ------------------------------------------------
  ! Enter the instrument file prefix, e.g. hirs3_n16
  ! ------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) File_Prefix
  File_Prefix = ADJUSTL( File_Prefix )


  ! --------------------
  ! Create the filenames
  ! --------------------

  SpcCoeff_File = TRIM( File_Prefix )//'.SpcCoeff.bin.Big_Endian'
  TauCoeff_File = TRIM( File_Prefix )//'.TauCoeff.bin.Big_Endian'
  AerosolCoeff_File = 'dummy.AerosolCoeff.bin.Big_Endian'
  ScatterCoeff_File = 'dummy.ScatterCoeff.bin.Big_Endian'



  !#----------------------------------------------------------------------------#
  !#           -- ASSIGN DUMMY VALUES TO THE GeometryInfo STRUCTURE --          #
  !#----------------------------------------------------------------------------#

  GeometryInfo%Secant_View_Angle = 1.0_fp_kind   ! Nadir



  !#----------------------------------------------------------------------------#
  !#                          -- INITIALISE THE CRTM --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Initializing the CRTM..." )' )

  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File, &
                            TauCoeff_File     = TauCoeff_File, &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            ScatterCoeff_File = ScatterCoeff_File )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#    -- ALLOCATE THE OUTPUT RTSolution ARRAY TO THE NUMBER OF CHANNELS --    #
  !#----------------------------------------------------------------------------#

  ALLOCATE( RTSolution( ChannelInfo%n_Channels, MAX_N_PROFILES ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
   STOP
  END IF


  n_Stokes = 2
  Error_Status = CRTM_Allocate_RTSolution( n_Stokes, RTSolution )
  
  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF


  !#----------------------------------------------------------------------------#
  !#                          -- PRINT OUT SOME INFO --                         #
  !#----------------------------------------------------------------------------#

  CALL Print_ChannelInfo( ChannelInfo )

  !#----------------------------------------------------------------------------#
  !#                         -- CALL THE FORWARD MODEL --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Calling the Forward CRTM..." )' )
  

  Error_Status = CRTM_Forward( Atmosphere, &
                               Surface, &
                               GeometryInfo, &
                               ChannelInfo, &
                               RTSolution )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error in CRTM Forward Model', & 
                            Error_Status)  
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                           -- DESTROY THE CRTM --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Destroying the CRTM..." )' )


  ! --------------------------
  ! The local RTSolution array
  ! --------------------------

  DEALLOCATE( RTSolution, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating RTSolution structure array', & 
                            Error_Status)  
  END IF


  ! ---------------
  ! The entire CRTM
  ! ---------------

  Error_Status = CRTM_Destroy( ChannelInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
    STOP
  END IF



CONTAINS


  SUBROUTINE Print_ChannelInfo( ChannelInfo )
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN ) :: ChannelInfo
    INTEGER :: l

    WRITE( *, '( /5x, "Number of channels indexed: ", i5 )' ) ChannelInfo%n_Channels
    WRITE( *, '(  /2x, "Channel         Sensor             NCEP          WMO           WMO     Channel", &
                 &/2x, " Index        Descriptor         Sensor ID   Satellite ID   Sensor ID   Number", &
                 &/2x, "------------------------------------------------------------------------------" )' )
    DO l = 1, ChannelInfo%n_Channels
      WRITE( *, '( 2x, 2x, i4, 2x, ">", a, "<", 5x, i3, 11x, i3, 11x, i3, 7x, i4 )' ) &
                ChannelInfo%Channel_Index( l ), &
                ChannelInfo%Sensor_Descriptor( l ), &
                ChannelInfo%NCEP_Sensor_ID( l ), &
                ChannelInfo%WMO_Satellite_ID( l ), &
                ChannelInfo%WMO_Sensor_ID( l ), &
                ChannelInfo%Sensor_Channel( l )
    END DO
 
  END SUBROUTINE Print_ChannelInfo

END PROGRAM Test_Forward


