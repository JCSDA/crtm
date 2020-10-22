!--------------------------------------------------------------------------------
!M+
! NAME:
!       Compare_MW_Attenuation
!
! PURPOSE:
!       Program to compare the Liebe89/93 and Rosenkranz03 microwave
!       atmospheric attenuation models.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       Profile_Utility_Parameters:  Module containing parameters used in the
!                                    profile utility modules.
!                                    USEs: TYPE_KINDS module
!                                          FUNDAMENTAL_CONSTANTS module
!
!       Units_Conversion:            Module containing routines to convert
!                                    atmospheric profile concentration units.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          PROFILE_UTILITY_PARAMETERS module
!                                          ATMOSPHERIC_PROPERTIES module
!
!       AtmProfile_Define:           Module defining the AtmProfile data
!                                    structure and containing routines to
!                                    manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:        Module containing routines to read and
!                                    write AtmProfile netCDF format files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          ATMPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       MWLBL_Liebe89:               Module containing data and routines to
!                                    calculate microwave atmospheric
!                                    attenuation according to the Liebe89
!                                    formulation.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          LIEBE89_COEFFICIENTS module
!
!       MWLBL_Liebe93:               Module containing data and routines to
!                                    calculate microwave atmospheric
!                                    attenuation according to the Liebe93
!                                    formulation.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          LIEBE92_COEFFICIENTS module
!                                          LIEBE93_COEFFICIENTS module
!
!       MWLBL_Rosenkranz03:          Module containing data and routines to
!                                    calculate microwave atmospheric
!                                    attenuation according to the Rosenkranz03
!                                    formulation.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          ROSENKRANZ03_COEFFICIENTS module
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
!       Input:  - netCDF format AtmProfile file
!
!       Output: - 
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Nov-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
!
!M-
!--------------------------------------------------------------------------------

PROGRAM Compare_MW_Attenuation



  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE Profile_Utility_Parameters
  USE Units_Conversion

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE Spectra_Define
  USE Spectra_netCDF_IO

  USE MWLBL_Liebe89
  USE MWLBL_Liebe93
  USE MWLBL_Rosenkranz03


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compare_MW_Attenuation'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Keyword argument set value
  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1

  ! -- Frequency data
  REAL( fp_kind ), PARAMETER :: BEGIN_FREQUENCY =   1.0_fp_kind  ! GHz
  REAL( fp_kind ), PARAMETER ::   END_FREQUENCY = 200.0_fp_kind  ! GHz
!  REAL( fp_kind ), PARAMETER :: BEGIN_FREQUENCY =  60.4344_fp_kind  ! GHz
!  REAL( fp_kind ), PARAMETER ::   END_FREQUENCY =  60.4352_fp_kind  ! GHz
  INTEGER,         PARAMETER :: N_FREQUENCIES = 1000

  ! -- Attenuation types
  INTEGER, PARAMETER :: N_ATTENUATION_TYPES = 4

  INTEGER, PARAMETER ::      WETLINE_ATTENUATION_TYPE = 1
  INTEGER, PARAMETER :: WETCONTINUUM_ATTENUATION_TYPE = 2
  INTEGER, PARAMETER ::      DRYLINE_ATTENUATION_TYPE = 3
  INTEGER, PARAMETER :: DRYCONTINUUM_ATTENUATION_TYPE = 4

  INTEGER, PARAMETER, DIMENSION( N_ATTENUATION_TYPES ) :: &
    ATTENUATION_TYPE = (/      WETLINE_ATTENUATION_TYPE, &
                          WETCONTINUUM_ATTENUATION_TYPE, &
                               DRYLINE_ATTENUATION_TYPE, &
                          DRYCONTINUUM_ATTENUATION_TYPE /)

  CHARACTER( * ), PARAMETER, DIMENSION( N_ATTENUATION_TYPES ) :: &
    ATTENUATION_NAME = (/ 'Wet Line     ', &
                          'Wet Continuum', &
                          'Dry Line     ', &
                          'Dry Continuum' /)

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 )        :: AtmProfile_Filename
  TYPE( AtmProfile_type ) :: AtmProfile
  CHARACTER( 256 )        :: AtmProfile_ID_Tag

  CHARACTER( 256 ) :: Spectra_Filename
  INTEGER :: New_File

  INTEGER :: j, k, l, m, n
  INTEGER, DIMENSION( 1 ) :: j_Idx
  INTEGER                 :: Index_H2O
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: H2O_Pressure

  REAL( fp_kind ) :: Dry_Pressure

  REAL( fp_kind ), DIMENSION( N_FREQUENCIES ) :: Frequency

  TYPE( Spectra_type ), TARGET :: Liebe_WetLine
  TYPE( Spectra_type ), TARGET :: Liebe_WetContinuum
  TYPE( Spectra_type ), TARGET :: Liebe_DryLine
  TYPE( Spectra_type ), TARGET :: Liebe_DryContinuum

  TYPE( Spectra_type ), TARGET :: Rosenkranz_WetLine
  TYPE( Spectra_type ), TARGET :: Rosenkranz_WetContinuum
  TYPE( Spectra_type ), TARGET :: Rosenkranz_DryLine
  TYPE( Spectra_type ), TARGET :: Rosenkranz_DryContinuum

  TYPE( Spectra_Type ), POINTER :: MW_Attenuation



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to compare the Liebe89/93 and Rosenkranz03")' )
  WRITE( *, '( 5x, "   microwave atmospheric attenuation models.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE INPUT AtmProfile DATA --                  #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the AtmProfile filename
  ! ---------------------------

  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) AtmProfile_Filename
  AtmProfile_Filename = ADJUSTL( AtmProfile_Filename )


  ! ---------------------------
  ! Read the AtmProfile dataset
  ! ---------------------------

  Error_Status = Read_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                         AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//TRIM( AtmProfile_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! --------------------------
  ! Get the profile set ID tag
  ! --------------------------

  AtmProfile_ID_Tag = ' '

  Error_Status = Inquire_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                            ID_Tag = AtmProfile_ID_Tag )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring AtmProfile input file '//&
                          TRIM( AtmProfile_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#         -- CONVERT THE WATER VAPOUR AMOUNTS TO PARTIAL PRESSURE --         #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Determine the H2O index in the absorber array
  ! ---------------------------------------------

  ! -- Make sure there is one water vapour entry present
  n = COUNT( AtmProfile%Absorber_ID == ID_H2O )

  IF ( n == 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'No H2O in absorber set.', &
                          FAILURE )
    STOP
  END IF

  IF ( n > 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'More than one H2O identifier in absorber set.', &
                          FAILURE )
    STOP
  END IF

  ! -- Determine the water vapour index
  j_idx = PACK( (/ ( j, j = 1, AtmProfile%n_Absorbers ) /), &
                AtmProfile%Absorber_ID == ID_H2O )

  Index_H2O = j_idx(1)


  ! ------------------------------------------------------------
  ! Allocate the H2O partial pressure array for units conversion
  ! ------------------------------------------------------------

  ALLOCATE( H2O_Pressure( AtmProfile%n_Layers ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating H2O partial pressure array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------------------------------
  ! Convert the water vapor amounts to partial pressure
  ! ---------------------------------------------------

  H2O_Convert_Loop: DO m = 1, AtmProfile%n_Profiles

    SELECT CASE ( AtmProfile%Absorber_Units_ID( Index_H2O ) )

      ! -- Convert from ppmv
      CASE ( PPMV_UNITS )
        H2O_Pressure = PPMV_to_PP( AtmProfile%Layer_Pressure( :, m ), &
                                   AtmProfile%Layer_Absorber( :, Index_H2O, m ) )


      ! -- Convert from number density
      CASE ( ND_UNITS )
        H2O_Pressure = ND_to_PP( AtmProfile%Layer_Absorber( :, Index_H2O, m ), &
                                 AtmProfile%Layer_Temperature( :, m ) )


      ! -- Convert from mixing ratio
      CASE ( MR_UNITS )
        H2O_Pressure = MR_to_PP( AtmProfile%Layer_Pressure( :, m ), &
                                 AtmProfile%Layer_Absorber( :, Index_H2O, m ) )


      ! -- Convert from mass density
      CASE ( MD_UNITS )
        H2O_Pressure = MD_to_PP( AtmProfile%Layer_Absorber( :, Index_H2O, m ), &
                                 AtmProfile%Layer_Temperature( :, m ) )


      ! -- Partial pressure direct input
      CASE ( PP_UNITS )
        H2O_Pressure = AtmProfile%Layer_Absorber( :, Index_H2O, m )


      ! -- Any other input
      CASE DEFAULT
        CALL Display_Message( PROGRAM_NAME, &
                              'Unrecognised water vapour units.', &
                              FAILURE )
        STOP

    END SELECT

    ! -- Check the result
    IF ( ANY( H2O_Pressure < ZERO ) ) THEN
      WRITE( Message, '( "Error converting water vapor units to hPa for AtmProfile #", i3, &
                        &" from ", a, "." )' ) &
                      m, TRIM( AtmProfile_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! -- Save the partial pressure in the structure array
    AtmProfile%Layer_Absorber( :, Index_H2O, m ) = H2O_Pressure

  END DO H2O_Convert_Loop

  ! -- Update the absorber units ID
  AtmProfile%Absorber_Units_ID( Index_H2O ) = PP_UNITS



  ! -----------------------------------------
  ! Deallocate the H2O partial pressure array
  ! -----------------------------------------

  DEALLOCATE( H2O_Pressure, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating H2O partial pressure array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                        -- CREATE THE FREQUENCY GRID --                     #
  !#----------------------------------------------------------------------------#

  Frequency = (/ ( REAL( l, fp_kind ), l = 0, N_FREQUENCIES - 1 ) /) / &
  !           ------------------------------------------------------
                         REAL( N_FREQUENCIES - 1, fp_kind )

  Frequency = ( Frequency * ( END_FREQUENCY - BEGIN_FREQUENCY ) ) + BEGIN_FREQUENCY



  !#----------------------------------------------------------------------------#
  !#                    -- ALLOCATE THE Spectra STRUCTURES --                   #
  !#----------------------------------------------------------------------------#

  ! ----------------
  ! Liebe structures
  ! ----------------

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Liebe_WetLine )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Liebe_WetLine Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Liebe_WetContinuum )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Liebe_WetContinuum Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Liebe_DryLine )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Liebe_DryLine Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Liebe_DryContinuum )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Liebe_DryContinuum Spectra structure.', &
                          Error_Status )
    STOP
  END IF


  ! ---------------------
  ! Rosenkranz structures
  ! ---------------------

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Rosenkranz_WetLine )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Rosenkranz_WetLine Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Rosenkranz_WetContinuum )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Rosenkranz_WetContinuum Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Rosenkranz_DryLine )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Rosenkranz_DryLine Spectra structure.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Allocate_Spectra( N_FREQUENCIES, &
                                   AtmProfile%n_Layers, &
                                   Rosenkranz_DryContinuum )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating Rosenkranz_DryContinuum Spectra structure.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- SPECIFY THE FREQUENCIES AND VARIABLE ATTRIBUTES --           #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------------------
  ! Perform the assignments on the Liebe structures
  ! -----------------------------------------------

  DO n = 1, N_ATTENUATION_TYPES

    SELECT CASE ( n )
      CASE (WETLINE_ATTENUATION_TYPE)
        MW_Attenuation => Liebe_WetLine
      CASE (WETCONTINUUM_ATTENUATION_TYPE)
        MW_Attenuation => Liebe_WetContinuum
      CASE (DRYLINE_ATTENUATION_TYPE)
        MW_Attenuation => Liebe_DryLine
      CASE (DRYCONTINUUM_ATTENUATION_TYPE)
        MW_Attenuation => Liebe_DryContinuum
    END SELECT

    MW_Attenuation%Frequency          = Frequency
    MW_Attenuation%Frequency_Longname = 'Frequency'
    MW_Attenuation%Frequency_Units    = 'GHz'
    MW_Attenuation%Spectra_Longname   = 'Attenuation - '//ATTENUATION_NAME(n)
    MW_Attenuation%Spectra_Units      = 'dB/km'

  END DO


  ! ----------------------------------------------------
  ! Perform the assignments on the Rosenkranz structures
  ! ----------------------------------------------------

  DO n = 1, N_ATTENUATION_TYPES

    SELECT CASE ( n )
      CASE (WETLINE_ATTENUATION_TYPE)
        MW_Attenuation => Rosenkranz_WetLine
      CASE (WETCONTINUUM_ATTENUATION_TYPE)
        MW_Attenuation => Rosenkranz_WetContinuum
      CASE (DRYLINE_ATTENUATION_TYPE)
        MW_Attenuation => Rosenkranz_DryLine
      CASE (DRYCONTINUUM_ATTENUATION_TYPE)
        MW_Attenuation => Rosenkranz_DryContinuum
    END SELECT

    MW_Attenuation%Frequency          = Frequency
    MW_Attenuation%Frequency_Longname = 'Frequency'
    MW_Attenuation%Frequency_Units    = 'GHz'
    MW_Attenuation%Spectra_Longname   = 'Attenuation - '//ATTENUATION_NAME(n)
    MW_Attenuation%Spectra_Units      = 'dB/km'

  END DO



  !#----------------------------------------------------------------------------#
  !#                         -- SET THE New_File FLAG --                        #
  !#----------------------------------------------------------------------------#

  New_File = SET



  !#----------------------------------------------------------------------------#
  !#                           -- LOOP OVER PROFILES --                         #
  !#----------------------------------------------------------------------------#

  Profile_Loop: DO m = 1, 5 !AtmProfile%n_Profiles

    WRITE( *, '( 10x, "Processing profile #", i3, "...." )' ) m


    ! -------------------------------------------
    ! Assign the profile number to the structures
    ! -------------------------------------------

    Liebe_WetLine%Profile_Number      = m           
    Liebe_WetContinuum%Profile_Number = m      
    Liebe_DryLine%Profile_Number      = m           
    Liebe_DryContinuum%Profile_Number = m      

    Rosenkranz_WetLine%Profile_Number      = m      
    Rosenkranz_WetContinuum%Profile_Number = m 
    Rosenkranz_DryLine%Profile_Number      = m      
    Rosenkranz_DryContinuum%Profile_Number = m 



    !#--------------------------------------------------------------------------#
    !#                           -- LOOP OVER LAYERS --                         #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, AtmProfile%n_Layers



      !#------------------------------------------------------------------------#
      !#                -- COMPUTE THE DRY GAS PARTIAL PRESSURE --              #
      !#------------------------------------------------------------------------#

      Dry_Pressure = AtmProfile%Layer_Pressure(k,m) - AtmProfile%Layer_Absorber( k, Index_H2O, m )



      !#------------------------------------------------------------------------#
      !#                      -- COMPUTE Liebe ATTENUATION --                   #
      !#------------------------------------------------------------------------#

      ! -----------------------------------------------
      ! Water vapor attenuation using the Liebe89 model
      ! -----------------------------------------------

      Error_Status = Liebe89( Frequency, &
                              Dry_Pressure, &
                              AtmProfile%Layer_Absorber( k, Index_H2O, m ), &
                              AtmProfile%Layer_Temperature( k, m ), &
                              WetLine_Attenuation      = Liebe_WetLine%Spectra(:,k), &
                              WetContinuum_Attenuation = Liebe_WetContinuum%Spectra(:,k), &
                              Quiet = SET )

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error calculating Liebe89 water vapor attentuation at level ", i3, "." )' ) &
                        k
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! -------------------------------------------
      ! Dry gas attenuation using the Liebe93 model
      ! -------------------------------------------

      Error_Status = Liebe93( Frequency, &
                              Dry_Pressure, &
                              AtmProfile%Layer_Absorber( k, Index_H2O, m ), &
                              AtmProfile%Layer_Temperature( k, m ), &
                              DryLine_Attenuation      = Liebe_DryLine%Spectra(:,k), &
                              DryContinuum_Attenuation = Liebe_DryContinuum%Spectra(:,k), &
                              Quiet = SET )

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error calculating Liebe93 dry gas attentuation at level ", i3, "." )' ) &
                        k
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      !#------------------------------------------------------------------------#
      !#                   -- COMPUTE Rosenkranz ATTENUATION --                 #
      !#------------------------------------------------------------------------#

      Error_Status = Rosenkranz03( Frequency, &
                                   Dry_Pressure, &
                                   AtmProfile%Layer_Absorber( k, Index_H2O, m ), &
                                   AtmProfile%Layer_Temperature( k, m ), &
                                   WetLine_Attenuation      = Rosenkranz_WetLine%Spectra(:,k), &
                                   WetContinuum_Attenuation = Rosenkranz_WetContinuum%Spectra(:,k), &
                                   DryLine_Attenuation      = Rosenkranz_DryLine%Spectra(:,k), &
                                   DryContinuum_Attenuation = Rosenkranz_DryContinuum%Spectra(:,k), &
                                   Quiet = SET )

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error calculating Rosenkranz03 water vapor attentuation at level ", i3, "." )' ) &
                        k
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                         -- OUTPUT RESULTS --                           #
      !#------------------------------------------------------------------------#

      ! ----------
      ! Liebe data
      ! ----------

      Spectra_Filename = 'Liebe_WetLine.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Liebe_WetLine, &
                                           New_File = New_File, &
                                           Title    = 'Water vapour attenuation due to line absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Liebe89/93 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Liebe_WetContinuum.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Liebe_WetContinuum, &
                                           New_File = New_File, &
                                           Title    = 'Water vapour attenuation due to continuum absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Liebe89/93 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Liebe_DryLine.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Liebe_DryLine, &
                                           New_File = New_File, &
                                           Title    = 'Oxygen attenuation due to line absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Liebe89/93 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Liebe_DryContinuum.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( Spectra_Filename, &
                                           Liebe_DryContinuum, &
                                           New_File = New_File, &
                                           Title    = 'Dry gas attenuation due to continuum absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Liebe89/93 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      ! ---------------
      ! Rosenkranz data
      ! ---------------

      Spectra_Filename = 'Rosenkranz_WetLine.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Rosenkranz_WetLine, &
                                           New_File = New_File, &
                                           Title    = 'Water vapour attenuation due to line absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Rosenkranz03 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Rosenkranz_WetContinuum.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Rosenkranz_WetContinuum, &
                                           New_File = New_File, &
                                           Title    = 'Water vapour attenuation due to continuum absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Rosenkranz03 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Rosenkranz_DryLine.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( TRIM( Spectra_Filename ), &
                                           Rosenkranz_DryLine, &
                                           New_File = New_File, &
                                           Title    = 'Oxygen attenuation due to line absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Rosenkranz03 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      Spectra_Filename = 'Rosenkranz_DryContinuum.Spectra.nc'

      Error_Status = Write_Spectra_netCDF( Spectra_Filename, &
                                           Rosenkranz_DryContinuum, &
                                           New_File = New_File, &
                                           Title    = 'Dry gas attenuation due to continuum absorption', &
                                           History  = PROGRAM_RCS_ID, &
                                           Comment  = 'Rosenkranz03 absorption model', &
                                           ID_Tag   = AtmProfile_ID_Tag )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile #", i5, " data to ", a )' ) &
                        m, TRIM( Spectra_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( MEssage ), &
                              Error_Status )
        STOP
      END IF


      ! --------------------------
      ! Turn off the new file flag
      ! --------------------------

      New_File = UNSET

    END DO Layer_Loop

  END DO Profile_Loop



  !#----------------------------------------------------------------------------#
  !#                    -- DESTROY THE AtmProfile STRUCTURE --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AtmProfile structure array.', &
                          WARNING )
  END IF

END PROGRAM Compare_MW_Attenuation


!#------------------------------------------------------------------------------#
!#                          -- MODIFICATION HISTORY --                          # 
!#------------------------------------------------------------------------------#
!
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Compare_MW_Attenuation.f90,v $
! Revision 1.2  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 1.1  2004/12/16 18:39:02  paulv
! Initial checkin.
!
!
!
