!------------------------------------------------------------------------------
!P+
! NAME:
!       Aerosol_Rewrite
!
! PURPOSE:
!       Program to read the aerosol profile data file supplied by Clark Weaver
!       and output it in the CRTM common Aerosol format.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:             Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       CRTM_Aerosol_Define:         Module defining the Aerosol data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       CRTM_Aerosol_Binary_IO:      Module containing routines to read and write
!                                    Aerosol Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_AEROSOL_DEFINE module
!
! CONTAINS:
!       None.
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
!       Input:  unformatted, sequential file provided by Clark
!
!       Output: Binary format Atmosphere file.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

PROGRAM Aerosol_Rewrite


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Aerosol_Define
  USE CRTM_Aerosol_Binary_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Aerosol_Rewrite'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Literal constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind

  ! -- The data file names
  CHARACTER( * ), PARAMETER :: INPUT_FILENAME   = 'aerosol_profiles.dat.interpolated_and_layer_averaged'
  CHARACTER( * ), PARAMETER :: AEROSOL_FILENAME = 'Aerosol.bin'

  ! -- The number of profile layers
  INTEGER, PARAMETER :: N_LAYERS = 100

  ! -- The number of aerosol profiles
  INTEGER, PARAMETER :: N_AEROSOLS = 7

  ! -- The aerosol types
  INTEGER, PARAMETER, DIMENSION( N_AEROSOLS ) :: &
    AEROSOL_TYPE = (/ SULFATE_AEROSOL,            &
                      DUST_AEROSOL,               &
                      DRY_BLACK_CARBON_AEROSOL,   &
                      DRY_ORGANIC_CARBON_AEROSOL, &
                      WET_BLACK_CARBON_AEROSOL,   &
                      WET_ORGANIC_CARBON_AEROSOL, &
                      SEASALT_AEROSOL            /)

  ! -- The size modes for the aerosols
  INTEGER, PARAMETER, DIMENSION( N_AEROSOLS ) :: &
    N_MODES = (/ 1, &  ! Sulfate
                 4, &  ! Dust
                 1, &  ! Dry black carbon
                 1, &  ! Dry organic carbon
                 1, &  ! Wet black carbon
                 1, &  ! Wet organic carbon
                 4 /)  ! Sea salt
  INTEGER, PARAMETER :: MAX_N_MODES = 4

  ! -- The effective radii (um) for the aerosols
  REAL( fp_kind ), PARAMETER, DIMENSION( MAX_N_MODES, N_AEROSOLS ) :: EFFECTIVE_RADIUS = &
    RESHAPE( (/ 0.156_fp_kind, ZERO,         ZERO,         ZERO,         &    ! Sulfate
                0.73_fp_kind,  1.4_fp_kind,  2.4_fp_kind,  4.5_fp_kind,  &    ! Dust
                0.039_fp_kind, ZERO,         ZERO,         ZERO,         &    ! Dry black carbon
                0.087_fp_kind, ZERO,         ZERO,         ZERO,         &    ! Dry organic carbon
                0.039_fp_kind, ZERO,         ZERO,         ZERO,         &    ! Wet black carbon
                0.087_fp_kind, ZERO,         ZERO,         ZERO,         &    ! Wet organic carbon
                0.26_fp_kind,  1.19_fp_kind, 2.43_fp_kind, 7.57_fp_kind /), & ! Sea salt
             (/ MAX_N_MODES, N_AEROSOLS /) )

  ! -- The effective variance (um) of the size distribution for the aerosols
  REAL( fp_kind ), PARAMETER, DIMENSION( MAX_N_MODES, N_AEROSOLS ) :: EFFECTIVE_VARIANCE = &
    RESHAPE( (/ 2.03_fp_kind, ZERO,         ZERO,         ZERO,         &     ! Sulfate
                2.0_fp_kind,  2.0_fp_kind,  2.0_fp_kind,  2.0_fp_kind,  &     ! Dust
                2.0_fp_kind,  ZERO,         ZERO,         ZERO,         &     ! Dry black carbon
                2.2_fp_kind,  ZERO,         ZERO,         ZERO,         &     ! Dry organic carbon
                2.0_fp_kind,  ZERO,         ZERO,         ZERO,         &     ! Wet black carbon
                2.2_fp_kind,  ZERO,         ZERO,         ZERO,         &     ! Wet organic carbon
                2.03_fp_kind, 2.03_fp_kind, 2.03_fp_kind, 2.03_fp_kind /), &  ! Sea salt
             (/ MAX_N_MODES, N_AEROSOLS /) )

  ! -- Level temperature flag values
  INTEGER, PARAMETER :: NO  = 0
  INTEGER, PARAMETER :: YES = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: FileID
  INTEGER :: m, n

  TYPE( CRTM_Aerosol_type ), DIMENSION( N_AEROSOLS ) :: Aerosol

  REAL( fp_kind ), DIMENSION( N_LAYERS ) :: Pressure




  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read the aerosol profile data file supplied",&
             &/5x, "   by Clark Weaver and output it in the CRTM common",&
             &/5x, "   Aerosol format.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                     -- ALLOCATE THE Aerosol STRUCTURES --                  #
  !#----------------------------------------------------------------------------#

  DO m = 1, N_AEROSOLS


    ! -------------------------------
    ! Allocate the Aerosol structures
    ! -------------------------------

    Error_Status = CRTM_Allocate_Aerosol( N_LAYERS, N_MODES( m ), &
                                          Aerosol( m ) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error allocating Aerosol structure array element ", i5 )' ) m
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF


    ! ------------------------------------------
    ! Fill the type, radius, and variance arrays
    ! ------------------------------------------

    Aerosol(m)%Type = AEROSOL_TYPE(m)

    DO n = 1, N_MODES(m)
      Aerosol(m)%Effective_Radius(:,n)   = EFFECTIVE_RADIUS(   n, AEROSOL_TYPE(m) )
      Aerosol(m)%Effective_Variance(:,n) = EFFECTIVE_VARIANCE( n, AEROSOL_TYPE(m) )
    END DO

  END DO



  !#----------------------------------------------------------------------------#
  !#                         -- READ THE DATA FILE  --                          #
  !#----------------------------------------------------------------------------#

  ! ------------------------
  ! Open the input data file
  ! ------------------------

  ! -- Get a logical unit number for input
  FileID = Get_Lun()
  IF ( FileID < 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error occurred obtaining file unit number.', &
                          FAILURE )
    STOP
  END IF

  ! -- Open the file
  OPEN( FileID, FILE   = INPUT_FILENAME, &
                STATUS = 'OLD', &
                FORM   = 'UNFORMATTED', &
                ACCESS = 'SEQUENTIAL', &
                ACTION = 'READ', &
                IOSTAT = IO_Status )

  IF ( IO_Status /= 0 ) THEN
    WRITE ( Message, '( "Error opening file ", a, ". IOSTAT = ", i5 )' ) INPUT_FILENAME, IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ----------------------
  ! Read the pressure data
  ! ----------------------

  READ( FileID, IOSTAT = IO_Status ) Pressure

  IF ( IO_Status /= 0 ) THEN
    WRITE ( Message, '( "Error reading pressure from ", a, ". IOSTAT = ", i5 )' ) INPUT_FILENAME, IO_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------
  ! Read the aerosol data
  ! ---------------------

  DO m = 1, N_AEROSOLS

    READ( FileID, IOSTAT = IO_Status ) Aerosol(m)%Concentration

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading aerosol profile #", i5, " (", a, ") from ", a, ". IOSTAT = ", i5 )' ) &
                      m, AEROSOL_TYPE_NAME( AEROSOL_TYPE(m) ), INPUT_FILENAME, IO_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                    -- WRITE THE Aerosol STRUCTURE FILE --                  #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing the Aerosol data file ", a, "...." )' ) AEROSOL_FILENAME

  Error_Status = CRTM_Write_Aerosol_Binary( AEROSOL_FILENAME, &
                                            Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Aerosol file '//AEROSOL_FILENAME, &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- TEST READ THE Atmosphere STRUCTURE FILE --               #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------
  ! Destroy the Aerosol structure array
  ! -----------------------------------

  Error_Status = CRTM_Destroy_Aerosol( Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Aerosol structure array prior to test read.', &
                          Error_Status )
    STOP
  END IF


  ! -------------
  ! Read the file
  ! -------------

  WRITE( *, '( /5x, "Test reading the Aerosol data file...." )' )

  Error_Status = CRTM_Read_Aerosol_Binary( AEROSOL_FILENAME, &
                                           Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading Aerosol file '//AEROSOL_FILENAME, &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE Aerosol STRUCTURE ARRAY --                 #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Aerosol( Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Aerosol structure array.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Aerosol_Rewrite


<<<<<<< variant A
>>>>>>> variant B
!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Aerosol_Rewrite.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/03/28 16:20:20  paulv
! Initial checkin.
!
!
!
!
======= end
