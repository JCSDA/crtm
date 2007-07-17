!------------------------------------------------------------------------------
!M+
! NAME:
!       Convolve_TauSpc_with_SRF
!
! PURPOSE:
!       Program to convolve netCDF LBL transmittance data files with instrument
!       SRFs
!
! CATEGORY:
!       Transmittance Production
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       File_Utility:                Module containing generic file utility
!                                    routines
!
!       Message_Handler:               Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       ProcessControl_Define:       Module containing the ProcessControl
!                                    data type definition and routines to
!                                    manipulate the structure.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       ProcessControl_IO:           Module containing routines to read and
!                                    write ProcessControl files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          PROCESSCONTROL_DEFINE module
!
!       SRF_Define:                  Module defining the SRF data structure and
!                                    containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       SRF_netCDF_IO:               Module containing routines to read and write
!                                    netCDF format SRF files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          SRF_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       LBLRTM_netCDF_IO:            Module containing routines to read and write
!                                    netCDF format files of LBLRTM output data.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       TauProfile_Define:           Module defining the TauProfile data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       TauProfile_netCDF_IO:        Module containing routines to read and write
!                                    netCDF format TauProfile files.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!                                          TAUPROFILE_DEFINE module
!                                          NETCDF module
!                                          NETCDF_UTILITY module
!
!       Tau_Production_Parameters:   Module defining parameters used in the LBL
!                                    transmittance production runs
!                                    USEs: TYPE_KINDS module
!
!       Tau_Production_Utility:      Module continaing utility routines for the
!                                    LBL transmittance production runs.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          TAU_PRODUCTION_PARAMETERS module
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
!       Input:  ASCII ProcessControl data file
!               netCDF SRF data files
!               netCDF LBLRTM data files
!
!       Output: netCDF TauProfile data files
!               ASCII ProcessControl data file
!
! SIDE EFFECTS:
!       Data is written to the output TauProfile data files based on the
!       contents of the Process Control file. If this file is removed or
!       recreated, the LBL convolvutions will be redone and any data
!       residing in an already created TauProfile file may be overwritten.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       Oof..where to start?
!
!       This program convolves LBL transmittance data for a user defined
!       molecule set and profile number for either an upwelling or downwelling 
!       atmospheric path. These user definitions are used to determine the
!       DIRECTORY PATH and FILE PREFIX for the LBL transmittance data files.
!
!       The user specified Process Control file contains information on which
!       LBL "bands" are required to process the various instrument channels
!       specified in the Process Control file.
!
!       The number of SRF data files required is obtained from the Process
!       Control data (via the ProcessControl data structure). Each SRF entry
!       will result in the creation of TauProfile output file - if the file
!       doesn't already exist. It is assumed that the output TauProfile data
!       file will be filled in a series of steps as the LBL data becomes
!       available.
!
!       Starting at the minimum band number in the ProcessControl structure,
!       the number of channels that are bounded by the band limits BAND1 and
!       BAND2 and have _not_ been processed are counted. If there is one or
!       more channels to process, the associated LBL transmittance data files
!       are opened if they exist. If the complete set of LBL transmittance
!       data files required to process the channel in question are not present,
!       that channel is not included in the actual processing.
!
!       When the processable channels have been identified and their
!       associated LBL transmittance data files opened, the channel SRFs
!       are read in.
!
!       At this point, the LAYER processing loop begins. For every
!       atmospheric layer:
!
!       The LBL nadir transmittance data is read in. A loop over the 
!       process-able channels convolves the SRF to the zenith-angle scaled
!       transmittance. Note that to scale the transmittance to the zenith
!       angle, the nadir transmittance must be converted to optical depth,
!       scaled, and then converted back to a transmittance. This is a 
!       time consuming conversion as it involves expensive EXP() and LOG()
!       intrinsic function calls. The convolution itself is a simply sum,
!       rather than a numerical integration, to save time. The resolution
!       of the LBL transmittance is high enough that the result is comparable
!       to the full integration method by a factor 10e-05 -> 10e-13 for
!       double precision floating point numbers. The convolved transmittance
!       is then normalized by the (precalculated) integrated SRF.
!
!       When all of the channels that can be processed have been processed,
!       the transmittance data is written to the output file and the
!       ProcessControl file is updated with the current state of the
!       processing.
!       
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

PROGRAM Convolve_TauSpc_with_SRF


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE ProcessControl_Define
  USE ProcessControl_IO

  USE SRF_Define
  USE SRF_netCDF_IO

  USE LBLRTM_netCDF_IO

  USE TauProfile_Define
  USE TauProfile_netCDF_IO

  USE Tau_Production_Parameters
  USE Tau_Production_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Convolve_TauSpc_with_SRF'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Convolve_TauSpc_with_SRF.f90,v 3.3 2006/06/30 16:47:16 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: INVALID = -1

  ! -- Literal constants
  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind
  REAL( fp_kind ), PARAMETER :: FIVE = 5.0_fp_kind

  ! -- Numerical precision and transmittance tolerance.
  REAL( fp_kind ), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL( fp_kind ), PARAMETER :: TAU_TOLERANCE = FIVE * TOLERANCE

  INTEGER, PARAMETER ::   UPWELLING_DIRECTION = 1
  INTEGER, PARAMETER :: DOWNWELLING_DIRECTION = 2
  CHARACTER( * ), PARAMETER, DIMENSION( 2 ) :: DIRECTION_NAME = (/ 'upwelling  ', &
                                                                   'downwelling' /)


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER           :: WMO_Satellite_ID
  INTEGER           :: WMO_Sensor_ID   
  CHARACTER( 5000 ) :: History
  CHARACTER( 5000 ) :: Comment

  CHARACTER( 128 )                              :: Tau_FilePrefix
  CHARACTER(   7 )                              :: Tau_FileBand
  CHARACTER( 263 ), DIMENSION( : ), ALLOCATABLE :: Tau_Filename  ! Was DIMENSION( MAX_N_LBLBANDS ) 

  CHARACTER( 256 ) :: Control_Filename

  LOGICAL          :: Output_Signal_File
  CHARACTER( 256 ) :: Signal_Filename

  INTEGER, DIMENSION( 1 ) :: Idx
  INTEGER         :: jIdx
  CHARACTER( 40 ) :: jTag
  CHARACTER(  9 ) :: mTag
  CHARACTER(  6 ) :: iTag

  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER :: n_Allocates

  INTEGER :: i, j, k, l, m, n, nm
  INTEGER :: lBand, lch, lb, lspc
  INTEGER :: l1, l2
  REAL( fp_kind ) :: v1, v2, vb1, vb2

  INTEGER :: Profile_Set
  INTEGER :: Direction

  INTEGER :: Max_Band
  INTEGER :: Band1
  INTEGER :: Band2
  INTEGER :: n_Band1
  INTEGER :: n_Bands
  INTEGER :: n_Available_Bands
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Band_Index  ! Was DIMENSION( MAX_N_LBLBANDS ) 

  INTEGER :: n_Channels
  INTEGER :: n_Channels_to_Process
  INTEGER :: n_Channels_Processed

  INTEGER :: Molecule_Set_Number
  INTEGER :: Profile_Number
  INTEGER :: Angle_Number
  INTEGER :: Band_Number

  INTEGER,         DIMENSION( : ), ALLOCATABLE :: n_SRF_Frequencies
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: Begin_SRF_Frequency
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE :: End_SRF_Frequency

  INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_Index
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Process_Index

  INTEGER :: n_Spectral_Points
  REAL( fp_kind ), DIMENSION( : ), ALLOCATABLE, TARGET  :: Transmittance
  REAL( fp_kind ), DIMENSION( : ),              POINTER :: Tau => NULL()
  REAL( fp_kind ) :: Convolved_Tau

  TYPE( ProcessControl_type ) :: ProcessControl
  INTEGER :: dF_Index
  REAL( fp_kind ) :: dF

  TYPE( SRF_type ), DIMENSION( : ),    ALLOCATABLE :: SRF
  INTEGER,          DIMENSION( :, : ), ALLOCATABLE :: SRF_Index

  TYPE( TauProfile_type ), DIMENSION( : ), ALLOCATABLE :: TauProfile

  ! -- Level pressure array for TauProfile output.
  REAL( fp_kind ), DIMENSION( N_LEVELS ) :: Pressure

  ! -- Timing variables
  INTEGER :: Hertz
  INTEGER :: Begin_Clock_Count, End_Clock_Count
  REAL( fp_kind ) :: Elapsed_Time



  !#----------------------------------------------------------------------------#
  !#                            -- INITIALIZATION --                            #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------
  ! Initialize the allocation checksum
  ! ----------------------------------

  n_Allocates = 0


  ! ---------------------------------
  ! By default, signal file is output
  ! ---------------------------------

  Output_Signal_File = .TRUE.


  ! ---------------------------
  ! Initialize timing variables
  ! ---------------------------

  ! -- Get the timing count rate
  CALL SYSTEM_CLOCK( COUNT_RATE = Hertz )

  ! -- Get the start time
  CALL SYSTEM_CLOCK( COUNT = Begin_Clock_Count )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to convolve LBLRTM transmittance spectra with")' )
  WRITE( *, '( 5x, "   spectrally corresponding SRFs defined in the ")' )
  WRITE( *, '( 5x, "   ProcessControl file.")' )
  WRITE( *, '(/5x, " $Revision: 3.3 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#         -- ENTER DATA FOR TRANSMITTANCE DATA FILE IDENTIFICATION --        #
  !#----------------------------------------------------------------------------#

  ! -------------------------------
  ! The profile set being processed
  ! -------------------------------

  ! -- Get user defined profile set
  WRITE( *, FMT = '( /5x, "Select the DEPENDENT PROFILE SET" )' )
  DO nm = 1, N_PROFILE_SETS
    WRITE( *, FMT = '( 10x, i2, ") ", a, " profile set" )' ) &
              nm, TRIM( PROFILE_SET_ID_TAG( nm ) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i1 )', &
           IOSTAT = IO_Status ) Profile_Set

  ! -- Check the result and value
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Profile_Set < 1 .OR. Profile_Set > N_PROFILE_SETS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid DEPENDENT PROFILE SET identifier value.', &
                          FAILURE )
    STOP
  ENDIF


  ! -----------------------------  
  ! The molecule set index number
  ! -----------------------------  

  ! --Get the user defined molecule set
  WRITE( *, FMT     = '( /5x, "Enter the MOLECULE SET to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Molecule_Set_Number

  ! -- Check the result and value
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET input.', &
                          FAILURE )
    STOP
  END IF

  IF ( .NOT. ANY( Molecule_Set_Number == MOLECULE_SET_TAG_ID ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid MOLECULE SET value.', &
                          FAILURE )
    STOP
  ENDIF

  ! -- Extract out the index of the molecule set number. If this seems
  ! -- a bit convoluted, remember that the molecule set numbers are
  ! -- not necessarily contiguous.
  Idx = PACK( (/ ( j, j = 1, N_MOLECULE_SETS ) /), &
              Molecule_Set_Number == MOLECULE_SET_TAG_ID )
  jIdx = Idx(1)

  ! -- Define a string of the molecule set
  jTag = MOLECULE_SET_TAG( jIdx )
  

  ! ------------------------
  ! The profile index number
  ! ------------------------

  ! -- Get user defined profile number
  WRITE( *, FMT     = '( /5x, "Enter the PROFILE NUMBER to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Profile_Number

  ! -- Check the result and value
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid PROFILE NUMBER input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Profile_Number < 1 .OR. Profile_Number > N_PROFILES( Profile_Set ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid PROFILE NUMBER.', &
                          FAILURE )
    STOP
  ENDIF

  ! -- Create a string of the profile number
  WRITE( mTag, '( "profile", i2.2 )' ) Profile_Number


  ! ----------------------
  ! The angle index number
  ! ----------------------

  ! -- Get user defined angle number
  WRITE( *, FMT     = '( /5x, "Enter the ANGLE NUMBER to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Angle_Number

  ! -- Check the result and value
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ANGLE NUMBER input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Angle_Number < ZENITH_ANGLE_BEGIN .OR. &
       Angle_Number > ZENITH_ANGLE_END        ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ANGLE NUMBER.', &
                          FAILURE )
    STOP
  ENDIF

  ! -- Create a string of the angle number
  WRITE( iTag, '( "angle", i1 )' ) Angle_Number


  ! ---------------
  ! The "direction"
  ! ---------------

  ! -- Get user defined atmosphericpath
  WRITE( *, FMT     = '( /5x, "Select atmospheric path", &
                       &/10x, "1) ", a, &
                       &/10x, "2) ", a, &
                       &/5x, "Enter choice: " )', &
            ADVANCE = 'NO' ) DIRECTION_NAME( 1 ), DIRECTION_NAME( 2 )
  READ( *, FMT    = '( i1 )', &
           IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  ! -- Check the result and value
  IF ( Direction /= UPWELLING_DIRECTION   .AND. &
       Direction /= DOWNWELLING_DIRECTION       ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF

  ! -- Reverse LEVEL_PRESSURE array if required for the
  ! -- TauProfile creation call. LEVEL_PRESSURE is defined
  ! -- in the Tau_Production_Parameters module from the
  ! -- SFC->TOA
  IF ( Direction == UPWELLING_DIRECTION ) THEN
    ! -- Upwelling, so reverse the array
    Pressure = LEVEL_PRESSURE(N_LEVELS:1:-1)
  ELSE
    ! -- Downwelling, so do nothing
    Pressure = LEVEL_PRESSURE
  END IF


  ! ----------------------------------------
  ! Create the LBL transmittance file prefix
  ! ----------------------------------------

  Tau_FilePrefix = TRIM( DIRECTION_NAME( Direction ) )//'_tau.'//&
                   mTag//'_'//iTag//'_'//TRIM( jTag )//'_'



  !#----------------------------------------------------------------------------#
  !#                      -- READ THE ProcessControl FILE --                    #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------------
  ! Create the process filename in the current directory, 
  !   ProcessControl.<angle tag>_<molset tag>.<direction tag>
  ! ----------------------------------------------------------

  WRITE( *, FMT     = '( /5x, "Enter the ProcessControl filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Control_Filename
  Control_Filename = ADJUSTL( Control_Filename )
 

  ! --------------------------------
  ! Read the ProcessControl datafile
  ! --------------------------------

  Error_Status = Read_ProcessControl( Control_Filename, &
                                      ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading ProcessControl file '//&
                          TRIM( Control_Filename )//'.', &
                          FAILURE )
    STOP
  END IF


  ! ------------------------------------------------------------------------
  ! Check that the frequency interval index is the same for every sensor
  ! in the ProcessControl file. Different frequency indices cannot be
  ! mixed as the LBLband numbering differs for different frequency intervals
  ! ------------------------------------------------------------------------

  dF_Index = ProcessControl%dF_Index(1)

  IF ( ANY( ProcessControl%dF_Index /= dF_Index ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Different dF_Index values in ProcessControl file '//&
                          TRIM( Control_Filename )//'.', &
                          FAILURE )
    STOP
  END IF


  ! --------------------------------------------------------------
  ! Allocate arrays that use frequency interval indexed dimensions
  ! --------------------------------------------------------------

  ALLOCATE( Tau_Filename( MAX_N_LBLBANDS( dF_Index ) ), &
            Band_Index( MAX_N_LBLBANDS( dF_Index ) ), &
            STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating Tau_Filename and Band_Index arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  ELSE
    n_Allocates = n_Allocates + 1
  END IF

  


  !#----------------------------------------------------------------------------#
  !#             -- CREATE THE TauProfile OUTPUT STRUCTURE ARRAY --             #
  !#----------------------------------------------------------------------------#

  ALLOCATE( TauProfile( ProcessControl%n_Files ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating TauProfile array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  ELSE
    n_Allocates = n_Allocates + 1
  END IF



  !#----------------------------------------------------------------------------#
  !#          -- FILL THE TauProfile STRUCTURE ARRAY WITH FILE DATA --          #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Initializing the TauProfile data structures with file data..." )' )

  Initialize_TauProfile_Loop: DO n = 1, ProcessControl%n_Files


    ! -------------------------------------------------------
    ! Construct the input SRF and output TauProfile filenames
    ! -------------------------------------------------------

    ! -- SRF filename... simple
    ProcessControl%SRF_Filename(n) = TRIM( ProcessControl%File_Prefix( n ) )//&
                                     '.srf.nc'

    ! -- Construct the TauProfile filname,
    ! --  <direction tag>.<profile tag>_<angle tag>_<molset tag>.<sensor tag>.TauProfile.nc
    ! -- for example,
    ! --   upwelling.profile05_angle3_ozo.hirs3_n17.TauProfile.nc
    ProcessControl%TauProfile_Filename(n) = TRIM( DIRECTION_NAME( Direction ) )//&
                                            '.'//mTag//'_'//iTag//'_'//TRIM( jTag )//&
                                            '.'//TRIM( ProcessControl%File_Prefix(n) )//&
                                            '.TauProfile.nc'


    ! ------------------------------------
    ! The number of channels for this file
    ! ------------------------------------

    n_Channels = ProcessControl%Channel_Index(2,n) - ProcessControl%Channel_Index(1,n) + 1


    ! ---------------------------------
    ! Allocate the SRF frequency arrays
    ! ---------------------------------

    ALLOCATE( n_SRF_Frequencies(   n_Channels ), &
              Begin_SRF_Frequency( n_Channels ), &
              End_SRF_Frequency(   n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating frequency arrays for SRF netCDF file '//&
                            TRIM( ProcessControl%SRF_Filename(n) )//' read.', &
                            FAILURE )
      STOP
    ELSE
      n_Allocates = n_Allocates + 1
    END IF


    ! --------------------
    ! Inquire the SRF file
    ! --------------------

    Error_Status = Inquire_SRF_netCDF( ProcessControl%SRF_Filename(n),         &
                                       n_Points         = n_SRF_Frequencies,   &
                                       Begin_Frequency  = Begin_SRF_Frequency, &
                                       End_Frequency    = End_SRF_Frequency,   &
                                       WMO_Satellite_ID = WMO_Satellite_ID,    &
                                       WMO_Sensor_ID    = WMO_Sensor_ID,       &
                                       Comment          = Comment              )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring SRF file '//&
                            TRIM( ProcessControl%SRF_Filename(n) ), &
                            Error_Status )
      STOP
    END IF


    ! --------------------------------------
    ! Determine the frequency interval index
    ! --------------------------------------

    Frequency_Interval_Loop: DO l = 1, n_Channels

      ! -- Compute the channel frequency interval 
      dF = ( End_SRF_Frequency(l) - Begin_SRF_Frequency(l) ) / &
      !    -------------------------------------------------
               REAL( n_SRF_Frequencies(l) - 1, fp_kind )

      ! -- Compute the frequency interval index
      dF_Index = Compute_dF_Index( dF )

      ! -- Is it valid?
      IF ( dF_Index < 0 ) THEN
        WRITE( Message, '( "Invalid frequency interval, ", es13.6, &
                          &"cm^-1, detected for channel SRF from ", a, "." )' ) &
                        dF, &
                        TRIM( ProcessControl%SRF_Filename(n) )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Compare the index
      IF ( dF_Index /= ProcessControl%dF_Index(n) ) THEN
        WRITE( Message, '( "Frequency interval index for SRF channel index #", i5, &
                          &" in ", a, ", ", i1, ", is different from ProcessControl, ", i1, "." )' ) &
                        l, &
                        TRIM( ProcessControl%SRF_Filename(n) ), &
                        dF_Index, &
                        ProcessControl%dF_Index(n)
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

    END DO Frequency_Interval_Loop


    ! -----------------------------------
    ! Deallocate the SRF frequency arrays
    ! -----------------------------------

    DEALLOCATE( n_SRF_Frequencies, &
                Begin_SRF_Frequency, &
                End_SRF_Frequency, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error deallocating frequency arrays for SRF netCDF file '//&
                            TRIM( ProcessControl%SRF_Filename(n) )//' read.', &
                            FAILURE )
      STOP
    ELSE
      n_Allocates = n_Allocates - 1
    END IF


    ! ------------------------------------------------
    ! If the TauProfile file does not exist, create it
    ! ------------------------------------------------

    IF ( .NOT. ( File_Exists( TRIM( ProcessControl%TauProfile_Filename(n) ) ) ) ) THEN

      ! -- Extract out the begin and end
      ! -- indices for the channel list
      l1 = ProcessControl%Channel_Index( 1, n )
      l2 = ProcessControl%Channel_Index( 2, n )

      ! -- Create the file (CLOBBER mode)
      Error_Status = Create_TauProfile_netCDF( ProcessControl%TauProfile_Filename(n), &
                                               Pressure, &
                                               ProcessControl%List( l1:l2 )%Channel, &
                                               (/ ZENITH_ANGLE_SECANT( Angle_Number ) /), &
                                               (/ Profile_Number /), &
                                               (/ Molecule_Set_Number /), &
                                               Release = TauProfile(n)%Release, &
                                               Version = TauProfile(n)%Version, &
                                               Sensor_ID        = TRIM(ProcessControl%File_Prefix(n)), &
                                               WMO_Satellite_ID = WMO_Satellite_ID, &
                                               WMO_Sensor_ID    = WMO_Sensor_ID, &
                                               ID_Tag  = TRIM(PROFILE_SET_ID_TAG( Profile_Set )), &
                                               Title   = TRIM(DIRECTION_NAME( Direction ))//&
                                                         ' transmittances for '//&
                                                         TRIM(ProcessControl%File_Prefix(n)), &
                                               Comment = TRIM(Comment) )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating netCDF TauProfile file '//&
                              TRIM( ProcessControl%TauProfile_Filename(n) ), &
                              Error_Status )
        STOP
      END IF

    END IF


    ! ------------------------------------------------------------------
    ! Read the TauProfile data file. This is done here to initialize the
    ! TauProfile structure with the currently completed transmittances.
    ! Since the entire TauProfile structure is output in one go outside
    ! the processing loop, not reading the data here could cause valid
    ! data to be overwritten if there was an interruption in a previous
    ! run of this program.
    ! ------------------------------------------------------------------

    Error_Status = Read_TauProfile_netCDF( ProcessControl%TauProfile_Filename(n), &
                                           TauProfile(n) )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading netCDF TauProfile file '//&
                            TRIM( ProcessControl%TauProfile_Filename(n) )//&
                            ' for transmittance initialization.', &
                            Error_Status )
      STOP
    END IF

    WRITE( *, '( 10x, a, " initialised." )' ) TRIM( ProcessControl%TauProfile_Filename(n) )

  END DO Initialize_TauProfile_Loop



  !##############################################################################
  !##############################################################################
  !##                                                                          ##
  !##              ## APPLY THE SRFs TO THE TRANSMITTANCE DATA ##              ##
  !##                                                                          ##
  !##############################################################################
  !##############################################################################

  ! -----------------------
  ! Variable initialisation
  ! -----------------------

  ! -- The maximum band to be processed
  Max_Band = MAXVAL( ProcessControl%List%End_LBLband )

  ! -- The starting bands
  Band1  = MINVAL( ProcessControl%List%Begin_LBLband )
  Band2  = Band1 - 1

  ! -- Summation variable
  n_Channels_Processed = 0



  !#----------------------------------------------------------------------------#
  !#                       -- BEGIN MAIN PROCESSING LOOP --                     #
  !#----------------------------------------------------------------------------#

  Main_Processing_Loop: DO



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE THE BAND LIMITS FOR PROCESSING --              #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! Increment the upper band limit. If the result
    ! is greater than the maximum, we're done.
    ! This is the only valid exit point for the loop
    ! named "Main_Processing_Loop". All other exit
    ! points are in error processing IF blocks.
    ! ----------------------------------------------

    Band2 = Band2 + 1
    IF ( Band2 > Max_Band ) EXIT Main_Processing_Loop


    ! -------------------------------------------
    ! Increment the lower band limit to eliminate
    ! bands that are no longer needed.
    ! -------------------------------------------

    Band1_Increment: DO

      ! -- Determine the number of unprocessed channels
      ! -- that require the current Band1
      n_Band1 = COUNT( ProcessControl%List%Begin_LBLband == Band1 .AND. &
                       ProcessControl%List%Processed     == 0     .AND. &
                       ProcessControl%List%Data_Available               )

      ! -- Check that the number of bands doesn't 
      ! -- exceed the (arbitrary) maximum allowed
      n_Bands = Band2 - Band1 + 1
      IF ( n_Bands > MAX_N_LBLBANDS( dF_Index ) ) THEN
!        Band1 = Band1 + 1
!        Band2 = Band1
        Band1 = Band2
        CYCLE Band1_Increment
      END IF

      ! -- If channels still require the
      ! -- lower band limit, exit the loop
      IF ( n_Band1 /= 0 ) EXIT Band1_Increment

      ! -- Otherwise, increment the band limit
      Band1 = Band1 + 1

      ! -- If we've gone past the upper band limit,
      ! -- cycle the main loop to increment the
      ! -- upper limit.
      IF ( Band1 > Band2 ) CYCLE Main_Processing_Loop

    END DO Band1_Increment



    !#--------------------------------------------------------------------------#
    !#          -- DETERMINE HOW MANY CHANNELS MAY BE PROCESSED --              #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Determine the number of channels
    ! for the current band limits
    ! --------------------------------

    n_Channels = COUNT( ProcessControl%List%Begin_LBLband >= Band1 .AND. &
                        ProcessControl%List%End_LBLband   <= Band2 .AND. &
                        ProcessControl%List%Processed     == 0     .AND. &
                        ProcessControl%List%Data_Available               )


    ! ---------------------------------
    ! If there are none, cycle the main
    ! loop to increment the band limits
    ! ---------------------------------

    IF ( n_Channels == 0 ) CYCLE Main_Processing_Loop



    !#--------------------------------------------------------------------------#
    !#         -- FIND THE INDICES OF THE COMPLETE CURRENT CHANNEL SET --       #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! Allocate an index array
    ! -----------------------

    ALLOCATE( Channel_Index( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating CHANNEL_INDEX array. STAT = ", i5, ". Exiting..." )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      EXIT Main_Processing_Loop
    ELSE
      n_Allocates = n_Allocates + 1
    END IF


    ! ----------------
    ! Find the indices
    ! ----------------

    Channel_Index = PACK( (/ (l, l = 1, ProcessControl%n_Channels ) /), &
                          ( ProcessControl%List%Begin_LBLband >= Band1 .AND. &
                            ProcessControl%List%End_LBLband   <= Band2 .AND. &
                            ProcessControl%List%Processed     == 0     .AND. &
                            ProcessControl%List%Data_Available               ) )



    !#--------------------------------------------------------------------------#
    !#    -- OPEN THE TRANSMITTANCE DATA FILES FOR THE CURRENT SET OF DATA --   #
    !#                                                                          #
    !# Note that some files may be opened un-necessarily if ultimately the SRF  #
    !# convolution requires more data than is available. To avoid this you      #
    !# would have to open the files by CHANNEL rather than BAND but then you    #
    !# would have to check whether or not the file for the current channel was  #
    !# already open (for cases where SRFs span more than one band or there is   #
    !# more than one channel encompassed by a particular band). This is doable, #
    !# but for higher resolution sensors (such as AIRS), it's more efficient    #
    !# this way. For broadband sensors, either method is fine.                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Initialise available band counter
    ! ---------------------------------

    n_Available_Bands = 0


    ! ---------------
    ! Loop over bands
    ! ---------------

    Band_File_Check: DO lBand = 1, n_Bands


      ! --------------------------
      ! Set the actual band number
      ! --------------------------

      Band_Number = Band1 + lBand - 1


      ! --------------------------------------------------
      ! Construct the netCDF LBLRTM transmittance filename
      ! --------------------------------------------------

      WRITE( Tau_FileBand, '( "band",i3.3 )' ) Band_Number
      Tau_Filename( lBand ) = './'//Tau_FileBand//'/'//&
                              TRIM( Tau_FilePrefix )//Tau_FileBand//'.nc'


      ! ------------------------------------------------------------
      ! Check if the signal file exists
      !
      ! The signal file is checked rather than the data file itself
      ! as the signal file should be created *after* the actual data
      ! file has been written and closed. Thus if the signal file
      ! exists, the data file is complete and readable.
      ! ------------------------------------------------------------

      IF ( File_Exists( TRIM( Tau_Filename( lBand ) )//'.signal' ) ) THEN

        ! -- Update the available band variables
        n_Available_Bands = n_Available_Bands + 1
        Band_Index( n_Available_Bands ) = lBand

      ELSE

        ! -- No data. Flag channels requiring this band.
        WRITE( *, '( 5x, "Band number ", i3, " data not available. Flagging channels..." )' ) &
                  Band_Number

        ! -- Loop over channels requiring this band
        DO lch = 1, n_Channels

          IF ( ProcessControl%List( Channel_Index( lch ) )%Begin_LBLband <= Band_Number .AND. &
               ProcessControl%List( Channel_Index( lch ) )%End_LBLband   >= Band_Number       ) THEN

            ProcessControl%List( Channel_Index( lch ) )%Data_Available = .FALSE.

            WRITE( *, '( 10x, a, " channel ", i4, " netCDF Tau data flagged unavailable." )' ) &
                      TRIM( ProcessControl%File_Prefix( ProcessControl%List( Channel_Index( lch ) )%File_Index ) ), &
                      ProcessControl%List( Channel_Index( lch ) )%Channel

          END IF

        END DO

      END IF

    END DO Band_File_Check


    ! ----------------------------------------------------
    ! Set the number of SRF channels that CAN be processed
    ! ----------------------------------------------------

    n_Channels_to_Process = COUNT( ProcessControl%List( Channel_Index )%Data_Available )



    !#--------------------------------------------------------------------------#
    !#      -- ONLY BEGIN PROCESSING IF THE NECESSARY DATA IS AVAILALBE --      #
    !#--------------------------------------------------------------------------#

    Check_Data_Availability: IF ( n_Available_Bands == n_Bands .AND. &
                                  n_Channels_to_Process > 0          ) THEN


      WRITE( *, '( /5x, i4, " channels to be processed out of ", i4, " in ", i3, " bands." )' ) &
                      n_Channels_to_Process, n_Channels, n_Available_Bands


      ! --------------------------------
      ! Allocate the transmittance array
      ! --------------------------------

      n_Spectral_Points = n_Available_Bands * N_FREQUENCIES( dF_Index )

      ALLOCATE( Transmittance( n_Spectral_Points ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating TRANSMITTANCE ", &
                          &"array. STAT = ", i5, ". Exiting..." )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        EXIT Main_Processing_Loop
      ELSE
        n_Allocates = n_Allocates + 1
      END IF


      ! -------------------------------------------------------------
      ! Read the begin and end frequencies of the transmittance bands
      ! -------------------------------------------------------------

      ! -- Initialise the frequency limits for min/max search
      v1 = 100000.0_fp_kind
      v2 = 0.0_fp_kind

      ! -- Only inquire the available data files
      Band_Frequency_Limit_Loop: DO lBand = 1, n_Available_Bands

        ! -- Get the current data file frequency limits
        ! -- and comment global attribute
        Error_Status = Inquire_LBLRTM_netCDF( TRIM( Tau_Filename( Band_Index( lBand ) ) ), &
                                              Begin_Frequency = vb1, &
                                              End_Frequency   = vb2, &
                                              Comment = History )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error inquiring netCDF TAU file ", a, ". Exiting..." )' ) &
                          TRIM( Tau_Filename( Band_Index( lBand ) ) )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          EXIT Main_Processing_Loop
        END IF

        ! -- Keep the min and max
        v1 = MIN( v1, vb1 )
        v2 = MAX( v2, vb2 )

      END DO Band_Frequency_Limit_Loop


      ! ---------------------------------------------------
      ! Find the indices of the process-able channel subset
      ! ---------------------------------------------------

      ! -- Allocate an array
      ALLOCATE( Process_Index( n_Channels_to_Process ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating PROCESS_INDEX array. STAT = ", i5, ". Exiting..." )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        EXIT Main_Processing_Loop
      ELSE
        n_Allocates = n_Allocates + 1
      END IF

      ! -- Find the indices
      Process_Index = PACK( (/ (l, l = 1, n_Channels ) /), &
                            ( ProcessControl%List( Channel_Index )%Data_Available ) )

      Process_Index = Channel_Index( Process_Index )



      ! -------------------------------------
      ! Read only those SRFs that can be used
      ! -------------------------------------

      ! -- Allocate an SRF array and an index array
      ALLOCATE( SRF( n_Channels_to_Process ), &
                SRF_Index( 2, n_Channels_to_Process ), &
                STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error allocating SRF arrays. STAT = ", i5, ". Exiting..." )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        EXIT Main_Processing_Loop
      ELSE
        n_Allocates = n_Allocates + 1
      END IF

      ! -- Read the required SRFs
      SRF_Read: DO lch = 1, n_Channels_to_Process

        ! -- Extract the current file index
        n = ProcessControl%List( Process_Index( lch ) )%File_Index

        ! -- Read the data
        Error_Status = Read_SRF_netCDF( ProcessControl%SRF_Filename(n), &
                                        ProcessControl%List( Process_Index( lch ) )%Channel, &
                                        SRF( lch ) )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading channel ", i4, " SRF from ", a, ". Exiting..." )' ) &
                          ProcessControl%List( Process_Index( lch ) )%Channel, &
                          TRIM( ProcessControl%SRF_Filename(n) )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          EXIT Main_Processing_Loop
        END IF

        WRITE( *, '( 5x, "Channel ", i4, " SRF read from ", a )' ) &
                  ProcessControl%List( Process_Index( lch ) )%Channel, &
                  TRIM( ProcessControl%SRF_Filename(n) )

        ! -- Find the indices where the SRF slots
        ! -- into the transmittance spectrum
        SRF_Index( 1, lch ) = Compute_Frequency_Index( v1, &
                                                       FREQUENCY_INTERVAL( dF_Index ), &
                                                       SRF( lch )%Begin_Frequency )
        SRF_Index( 2, lch ) = Compute_Frequency_Index( v1, &
                                                       FREQUENCY_INTERVAL( dF_Index ), &
                                                       SRF( lch )%End_Frequency )

        ! -- Double check the index values
        IF ( ANY( SRF_Index( :, lch ) < 1                 ) .OR. &
             ANY( SRF_Index( :, lch ) > n_Spectral_Points )      ) THEN
          WRITE( Message, '( "Computed SRF-in-transmittance indices invalid. ", &
                            &"Allowed: [",i6,",",i6,"];", 5x, &
                            &"Computed: [",i6,",",i6,"]. Exiting..." )' ) &
                          1, n_Spectral_Points, &
                          SRF_Index( 1, lch ), SRF_Index( 2, lch )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          EXIT Main_Processing_Loop
        END IF

      END DO SRF_Read



      !#------------------------------------------------------------------------#
      !#                  -- BEGIN LOOP OVER ATMOSPHERIC LAYERS --              #
      !#------------------------------------------------------------------------#

      Process_Layer: DO k = 1, N_LAYERS


        ! ---------------------------------------------
        ! Initialise begin index of transmittance array
        ! ---------------------------------------------

        l1 = 1


        ! -----------------------------
        ! Loop over bands for data read
        ! -----------------------------

        Band_File_Read: DO lBand = 1, n_Available_Bands

          ! -- Set the band index
          lb = Band_Index( lBand )

          ! -- Set the end index of transmittance
          ! -- data for this band read      
          l2 = l1 + N_FREQUENCIES( dF_Index ) - 1

          ! -- Double check the end index
          IF ( l2 > n_Spectral_Points ) THEN
            WRITE( Message, '( "Calculated end index for transmittance array, ", i6, &
                              &", is greater than the array size, ", i6, ". Exiting..." )' ) &
                            l2, n_Spectral_Points
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  FAILURE )
            EXIT Main_Processing_Loop
          END IF

          ! -- Alias the transmittance array
          Tau => Transmittance(l1:l2)

          ! -- Read the transmittance into the optical depth array
          Error_Status = Read_LBLRTM_netCDF( TRIM( Tau_Filename( lb ) ), &
                                             k, &
                                             Transmittance = Tau )

          IF ( Error_Status /= SUCCESS ) THEN
            WRITE( Message, '( "Error reading netCDF TAU file ", a, &
                              &" at layer ", i3, ". Exiting..." )' ) &
                            TRIM( Tau_Filename( lb ) ), k
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  FAILURE )
            EXIT Main_Processing_Loop
          END IF

          ! -- Update the begin index
          l1 = l2 + 1

        END DO Band_File_Read


        ! -------------------------------------------------
        ! Loop over channels to process for SRF convolution
        ! -------------------------------------------------

        Process_Channel: DO lch = 1, n_Channels_to_Process


          ! --------------------------------------------
          ! Set the file index for the current channel
          ! This is effectively an instrument identifier
          ! --------------------------------------------

          n = ProcessControl%List( Process_Index( lch ) )%File_Index


          ! ---------------------------------------
          ! Set the instrument channel index, i.e.
          ! the index of the current channel within
          ! the set for the particular instrument.
          ! ---------------------------------------

          l = Process_Index( lch ) - ProcessControl%Channel_Index( 1, n ) + 1


          ! ------------------------------------
          ! Set the high resolution index limits
          ! and alias the arrays to work with
          ! ------------------------------------

          l1 = SRF_Index( 1, lch )
          l2 = SRF_Index( 2, lch )

          Tau => Transmittance( l1:l2 )


          ! --------------------------------------------
          ! Calculate the transmittance/SRF convolution
          !                       ___
          !                      \
          ! Convolved_Tau = dF .  >   Tau(l) . SRF(l)  
          !                      /___
          !                        l
          !
          ! Note that compensated summation is not used
          ! here since 1) it'll take longer and 2) the
          ! spectral transmittances are not sorted so
          ! I'm not sure if there would be a benefit.
          !
          ! Note also that the use of an expression as
          ! the argument to SUM() will most likely force
          ! a copy of the argument.
          ! --------------------------------------------

          Convolved_Tau = SUM( Tau * SRF( lch )%Response ) * FREQUENCY_INTERVAL( dF_Index )


          ! -------------------------
          ! Save the normalised value
          ! -------------------------
          !                       i m,j
          TauProfile( n )%Tau(k,l,1,1,1) =       Convolved_Tau       / &
          !                                ------------------------
                                           SRF( lch )%Summation_SRF

        END DO Process_Channel

        WRITE( *, '( 5x, "Completed layer ", i3 )' ) k

      END DO Process_Layer


      ! ----------------------------------
      ! Clean up the transmittance pointer
      ! ----------------------------------

      NULLIFY( Tau )



      !#------------------------------------------------------------------------#
      !#                   -- CHECK THE TRANSMITTANCE DATA --                   #
      !#                                                                        #
      !# I have this in its own loop so that I can identify exactly where any   #
      !# bad transmittances are wrt channel                                     #
      !#------------------------------------------------------------------------#

      Check_Channel: DO lch = 1, n_Channels_to_Process


        ! --------------------------------------------
        ! Set the file index for the current channel
        ! This is effectively an instrument identifier
        ! --------------------------------------------

        n = ProcessControl%List( Process_Index( lch ) )%File_Index


        ! ---------------------------------------
        ! Set the instrument channel index, i.e.
        ! the index of the current channel within
        ! the set for the particular instrument.
        ! ---------------------------------------

        l = Process_Index( lch ) - ProcessControl%Channel_Index( 1, n ) + 1


        ! -----------------------------------------------------------
        ! Check the convolved values. I do it here rather than in the
        ! calculation loop as here I can check the data contiguously.
        ! -----------------------------------------------------------

        ! -- Check for any *really* bad transmittances. The values will
        ! -- still be "corrected" in the next section immediately following,
        ! -- but this flags the user to inspect the output to see if it 
        ! -- makes sense.
        IF ( ANY( TauProfile( n )%Tau(:,l,1,1,1) > ( ONE  + TAU_TOLERANCE ) ) .OR. &
             ANY( TauProfile( n )%Tau(:,l,1,1,1) < ( ZERO - TAU_TOLERANCE ) )      ) THEN

          WRITE( Message, '( "Non-physical transmittances found for ", a, &
                            &" channel ", i4, ", angle secant ", f4.2, "." )' ) &
                          TRIM( ProcessControl%File_Prefix( n ) ), &
                          ProcessControl%List( Process_Index( lch ) )%Channel, &
                          ZENITH_ANGLE_SECANT( Angle_Number )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                WARNING )
          WRITE( *, '( "Transmittance precision tolerance: ", f22.20, &
                     &/"transmittances:" )' ) TAU_TOLERANCE
          WRITE( *, '( 4( 1x, f21.18, : ) )' ) TauProfile( n )%Tau(:,l,1,1,1)
        END IF

        ! -- Correct any transmittances for precision-related
        ! -- "non-physicalness", e.g. some ozone only transmittances
        ! -- for very ozone transparent channels have values
        ! -- like 1.00000000000000004 when the high-resolution
        ! -- values are never greater than 1.0. This is most likely
        ! -- due to the accumulation of numerical errors during the
        ! -- summation for the convolution.
        WHERE( TauProfile( n )%Tau(:,l,1,1,1) > ONE )
          TauProfile( n )%Tau(:,l,1,1,1) = ONE
        END WHERE

        WHERE( TauProfile( n )%Tau(:,l,1,1,1) < ZERO )
          TauProfile( n )%Tau(:,l,1,1,1) = ZERO
        END WHERE

      END DO Check_Channel



      !#------------------------------------------------------------------------#
      !#                   -- LET THE DEALLOCATIONS BEGIN! --                   #
      !#------------------------------------------------------------------------#

      ! ------------------------
      ! Deallocate the SRF array
      ! ------------------------

      ! -- Destroy the structures
      Error_Status = Destroy_SRF( SRF )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SRF array.', &
                              FAILURE )
        STOP
      END IF

      ! -- Deallocate the arrays
      DEALLOCATE( SRF, SRF_Index, STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating SRF arrays. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      ELSE
        n_Allocates = n_Allocates - 1
      END IF


      ! -----------------------------------------
      ! Deallocate the channel subset index array
      ! -----------------------------------------

      DEALLOCATE( Process_Index, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating PROCESS_INDEX array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      ELSE
        n_Allocates = n_Allocates - 1
      END IF


      ! ----------------------------------
      ! Deallocate the transmittance array
      ! ----------------------------------

      DEALLOCATE( Transmittance, &
                  STAT = Allocate_Status )

      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating TRANSMITTANCE array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      ELSE
        n_Allocates = n_Allocates - 1
      END IF


    ELSE Check_Data_Availability

      WRITE( *, '( /5x, "For band range [",i2,",",i2,"]; incomplete band range or ", &
                       &"no channels to process. Continuing..." )' ) Band1, Band2

    END IF Check_Data_Availability



    !#--------------------------------------------------------------------------#
    !#                   -- UPDATE CHANNEL PROCESSING FLAGS --                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------
    ! Mark channels that were processed
    ! ---------------------------------

    WHERE( ProcessControl%List( Channel_Index )%Data_Available )
      ProcessControl%List( Channel_Index )%Processed = 1
    END WHERE


    ! ------------------------------------------------
    ! Deallocate the current valid channel index array
    ! ------------------------------------------------

    DEALLOCATE( Channel_Index, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error deallocating CHANNEL_INDEX array. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    ELSE
      n_Allocates = n_Allocates - 1
    END IF


    ! ------------------------------------
    ! Sum the number of channels processed
    ! ------------------------------------

    n_Channels_Processed = n_Channels_Processed + n_Channels_to_Process

  END DO Main_Processing_Loop


  WRITE( *, '( /5x, "Number of channels processed in this run: ", i4, &
                   &" out of possible ", i4, ". " )' ) &
            n_Channels_Processed, ProcessControl%n_Channels
  WRITE( *, '( /5x, "Processed channel list:" )' )
  WRITE( *, '( 20( 1x, i2 ) )' ) ( l, l = 1, 20 )
  WRITE( *, '( 60( "-" ) )' )
  WRITE( *, '( 20( 1x, i2 ) )' ) ProcessControl%List%Processed



  !#----------------------------------------------------------------------------#
  !#                 -- OUTPUT THE TauProfile DATA TO FILE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Writing netCDF TauProfile data files..." )' )

  TauProfile_Write: DO n = 1, ProcessControl%n_Files


    ! -------------------------------------------
    ! Add the TauProfile HISTORY global attribute
    ! with the LBLRTM and HITRAN history
    ! -------------------------------------------

    Error_Status = Modify_TauProfile_GAtts( TRIM( ProcessControl%TauProfile_Filename(n) ), &
                                            History = PROGRAM_RCS_ID//'; '//&
                                                      TRIM( History ) )

    IF ( Error_Status /= SUCCESS ) THEN

      ! -- Disable signal file output
      Output_Signal_File = .FALSE.

      ! -- Output error message
      CALL Display_Message( PROGRAM_NAME, &
                            'Error adding HISTORY attribute to netCDF TauProfile file '//&
                            TRIM( ProcessControl%TauProfile_Filename(n) ), &
                            WARNING )
    END IF


    ! ---------------------------------
    ! Write the TauProfile data to file
    ! ---------------------------------

    Error_Status = Write_TauProfile_netCDF( TRIM( ProcessControl%TauProfile_Filename(n) ), &
    !                                                               i m j
                                            TauProfile( n )%Tau(:,:,1,1,1), &
                                            ZENITH_ANGLE_SECANT( Angle_Number ), &  ! i
                                            Profile_Number, &                       ! m
                                            Molecule_Set_Number )                   ! j

    IF ( Error_Status /= SUCCESS ) THEN

      ! -- Disable signal file output
      Output_Signal_File = .FALSE.

      ! -- Output error message
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing transmittance data to netCDF TauProfile file '//&
                            TRIM( ProcessControl%TauProfile_Filename(n) ), &
                            WARNING )
    END IF

    WRITE( *, '( 10x, a, " written." )' ) TRIM( ProcessControl%TauProfile_Filename(n) )

  END DO TauProfile_Write



  !#----------------------------------------------------------------------------#
  !#                  -- UPDATE THE Process Control FILE --                     #
  !#----------------------------------------------------------------------------#

  Error_Status = Write_ProcessControl( Control_Filename, &
                                       ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Process Control file.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#       -- CREATE A MOLECULE SET AND DIRECTION DEPENDENT SIGNAL FILE --      #
  !#----------------------------------------------------------------------------#

  IF ( Output_Signal_File ) THEN

    WRITE( Signal_Filename, '( a, ".", a, ".", a )' ) &
                            PROGRAM_NAME, &
                            iTag//'_'//TRIM( jTag ), &
                            TRIM( DIRECTION_NAME( Direction ) )

    Error_Status = Create_Signal_File( TRIM( Signal_Filename ) )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing Signal file '//TRIM( Signal_Filename ), &
                            WARNING )
    END IF

  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- COMPLETE THE DEALLOCATIONS --                     #
  !#----------------------------------------------------------------------------#

  ! -----------------------------------------
  ! Deallocate the TauProfile structure array
  ! -----------------------------------------

  ! -- Deallocate the pointer members
  Error_Status = Destroy_TauProfile( TauProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the TauProfile structure array.', &
                          WARNING )
  END IF

  ! -- Deallocate the actual array
  DEALLOCATE( TauProfile, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating TauProfile array. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          WARNING )
  ELSE
    n_Allocates = n_Allocates - 1
  END IF


  ! ------------------------------------------------
  ! Deallocate the frequency interval indexed arrays
  ! ------------------------------------------------

  DEALLOCATE( Tau_Filename, Band_Index, STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error deallocating Tau_Filename and Band_Index arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  ELSE
    n_Allocates = n_Allocates - 1
  END IF


  ! ------------------------------------------
  ! Destroy the Process Control data structure
  ! ------------------------------------------

  Error_Status = Destroy_ProcessControl( ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the Process Control structure.', &
                          WARNING )
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- OUPUT THE ALLOCATION CHECKSUM --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Allocation checksum: ", i5 )' ) n_Allocates



  !#----------------------------------------------------------------------------#
  !#                         -- OUPUT THE ELAPSED TIME --                       #
  !#----------------------------------------------------------------------------#

  ! -- Get the end time
  CALL SYSTEM_CLOCK( COUNT = End_Clock_Count )

  ! -- Output execution time
  Elapsed_Time = REAL( End_Clock_Count - Begin_Clock_Count, fp_kind ) / &
  !              ----------------------------------------------------
                              REAL( Hertz, fp_kind )
  Elapsed_Time = Elapsed_Time / 3600.0_fp_kind
  WRITE( *, '( /5x, "Elapsed time: ", f5.2, "hrs" )' ) Elapsed_Time

END PROGRAM Convolve_TauSpc_with_SRF


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Convolve_TauSpc_with_SRF.f90,v 3.3 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 3.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Convolve_TauSpc_with_SRF.f90,v $
! Revision 3.3  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 3.2  2005/09/16 20:27:43  paulv
! - Updates made to reflect changes in ProcessControl structure name and
!   components.
!
! Revision 3.1  2005/05/15 23:39:49  paulv
! - Upgraded to Fortran-95
! - Modified to use new ProcessControl structure.
! - Modified to use new Tau_Production_Parameters module where the frequency
!   interval can take on two values.
! - Frequency interval index computed and compared with ProcessControl file
!   data.
!
! Revision 3.0  2004/04/08 17:45:09  paulv
! - New version for performing the convolutions on input data for each angle.
!   The nadir optical depths are no longer scaled, converted to transmittances
!   and then convolved. Individual data files for each angle are now read in.
!
! Revision 2.5  2003/12/01 16:39:49  paulv
! - Altered TauProfile creation function call to write the sensor and satellite
!   ID values.
! - Altered the TauProfile creation function call to *not* write the History
!   attribute. The History attribute is now written to the TauProfile file
!   prior to the Write_TauProfile_netCDF() function call. The netCDF LBLRTM
!   Comment attribute is now appended to the program RCS Id to provide info
!   on the LBLRTM and HITRAN versions.
!
! Revision 2.4  2003/09/05 17:38:02  paulv
! - Changes made to use new SRF and Process Control definition and I/O modules.
!
! Revision 2.3  2003/07/23 16:26:04  paulv
! - Added a pressure array variable to allow the LEVEL_PRESSURE parameter
!   array to be reversed as required so that teh pressure levels written to
!   the TauProfile file are in the same order as the transmittances, i.e. up-
!   or downwelling.
!
! Revision 2.2  2003/07/16 20:06:06  paulv
! - Changes made to use new TauProfile and LBLRTM I/O modules.
!
! Revision 2.1  2002/10/30 20:19:46  paulv
! - Major changes.
!   o The data is no longer written out as is it computed. This was causing
!     long execution times by writing little bits out at a time to minimise
!     data loss if the program crashed for whatever reason. Now all the data
!     for a particular set of channels is saved until the processing loop is
!     completed and then output in one go.
!   o To minimise data loss if there is an error the code traps for, any
!     errors cause the main processing loop to be exited and the current set
!     of data to be written. To stop the data being destroyed when the code
!     is restarted, the output file is first read before processing begins.
!   o Profiling of the code revealed the EXP() function to be the slowest
!     part of the code. I have put tested four different scenarios on the
!     IBM SP for the broadband instruments. The elapsed time for the code
!     for these are:
!       1) Vector F90 EXP() intrinsic            - 0.73hrs
!       2) Vector MASS library VEXP() subroutine - 0.32hrs
!       3) Scalar F90 EXP() intrinsic            - 0.69hrs
!       4) Scalar MASS library EXP() function    - 0.39hrs
!     The code now uses method (4) as it's fast and the code is still
!     portable.
!
! Revision 1.22  2002/10/18 20:25:08  paulv
! - Added simple timing code to output elapsed time.
!
! Revision 1.21  2002/09/10 19:54:03  paulv
! - Corrected bug with previous version. CHANNEL_INDEX array was being accessed
!   _before_ it was allocated. D'oh. To skip processing when all the required
!   aren't available the CHECK_DATA_AVAILABILITY IF block now uses the logical
!   test
!     n_available_bands == n_bands
!
! Revision 1.20  2002/09/09 22:02:21  paulv
! - Added TauProfile file check loop. Now if all the required bands aren't
!   present, the main processing loop is cycled.
!
! Revision 1.19  2002/08/08 21:10:56  paulv
! - The transmittance tolerance used to gauge non-physical transmittances
!   is now set to 5x the numerical precision. Since the convolved result is
!   a summation of numbers, it is reasonable to expect any precision errors
!   to accumulate so using the precision value itself will still trip many
!   "false" invalid transmittances.
! - The transmittance tolerance used to gauge non-physical transmittances
!   is now printed out with the offending transmittances.
!
! Revision 1.18  2002/08/08 16:28:34  paulv
! - Altered the logic for the non-physical transmittance check in the write
!   loop. Previously any transmittances > 1.0 or < 0.0 would halt execution.
!   However, it was found that some transmittances for *very* transparent
!   channels could be greater than 1.0, e.g. 1.00000000000000004, even when
!   all the high-resolution transmittances were <= 1.0. This has been attributed
!   to numerical precision errors in the integration/summation of the
!   convolution. So now execution is only halted is any transmittances
!   are > 1.0+eps or < 0.0-eps where eps==effective numerical precision.
!   Once this test has been passed, any transmittances > 1.0 are set to 1.0
!   and any transmittances < 0.0 are set to 0.0.
!
! Revision 1.17  2002/08/06 22:46:15  paulv
! - Changed TauProfile open mode to READWRITE.
! - Added some more info output for cases where the input LBLRTM data is
!   missing.
!
! Revision 1.16  2002/07/19 21:18:50  paulv
! - Corrected bug in Write_TauProfile_netCDF() arguments.
! - Replaced Find_Indices() function with PACK intrinsic.
!
! Revision 1.15  2002/07/19 18:24:38  paulv
! - Added PROFILE SET NUMBER input.
! - Altered TauProfile netCDF functions to account for changes to interface.
!
! Revision 1.14  2002/06/24 18:36:09  paulv
! - Added a number of array indexing and transmittance value checks.
!
! Revision 1.13  2002/06/24 16:55:50  paulv
! - Moved the Process Control file update inside the processing loop. This will
!   update the file more frequently so if there is a failure somewhere, sometime
!   as much data as possible should be recoverable.
!
! Revision 1.12  2002/06/19 19:26:08  paulv
! - Corrected bug in sensor_platform_ID indexing.
!
! Revision 1.11  2002/06/19 17:08:01  paulv
! - Now using the sensor/platform ID to construct the SRF and TauProfile
!   data filenames:
!     PC%SRF( n )%fileNAME = TRIM( PC%sensor_platform_ID( i ) )//'.srf.nc'
!   and
!     PC%TauProfile( n )%fileNAME = TRIM( DIRECTION_NAME( direction ) )//'.'//&
!                                   TRIM( PC%sensor_platform_ID( i ) )//'.'//&
!                                   mtag//'_'//TRIM( jtag )//'.TauProfile.nc'
!   Previously the filenames were stored and read directly from the Process
!   Control file. This required the TauProfile file to be constructed more
!   than once. A PITA.
!
! Revision 1.10  2002/06/14 17:24:11  paulv
! - TauProfile data files now being opened in "WRITESHARE" mode to allow file
!   sharing AND writing by the same process. Using the "SHARE" mode did not
!   allow the current process to also WRITE to the file. The netCDF documentation
!   could be a little clearer for peons such as myself.
!
! Revision 1.9  2002/06/14 14:28:39  paulv
! - Changed the manner in which TauProfile output files are opened. Previously
!   if the file didn't exist it was created and if it did exist it was opened.
!   Now if the file doesn't exist it is created and then closed, and then
!   opened in the normal manner but with the SHARE access mode. This was done
!   to prevent the data access being buffered which led to time consuming
!   periodic "buffer flushes" as the data was written (presumably when the
!   buffers were full).
!
! Revision 1.8  2002/06/13 14:32:57  paulv
! - Cleared the character variables used to read the SRF global attributes
!   prior to the function call. Previous file strings were getting all mixed
!   up with later read, and shorter, strings.
!
! Revision 1.7  2002/06/11 13:53:35  paulv
! - Added signal file output.
! - Rearranged final deallocations. The order is now:
!   o Close the SRF and TauProfile files. This should ensure the TauProfile
!     data is actually written to file (just in case the output is buffered)
!   o The Process Control file is updated.
!   o The Signal file is output. The creation of this files means that the
!     Process Control update was successful.
!   o The TauProfile and PC structures are deallocated. If these fail, only
!     warnings are generated but I wanted the Process Control file update
!     and Signal file write to occur first.....just in case. :o)
!
! Revision 1.6  2002/06/07 22:05:53  paulv
! - Added profile number identifier to output TauProfile filename.
! - Only a one profile TauProfile file is created. All the profiles will
!   be assembled into one file later.
!
! Revision 1.5  2002/06/06 16:50:22  paulv
! - Added documentation.
! - MOLECULE_SET and PROFILE_NUMBER added as inputs.
! - Process_Control filename added as input.
! - Changes made to SRF and TauProfile file open/close to reflect changes in
!   PC data structure.
! - Changes made to Process_Control interface to reflect changes in the
!   PC code.
! - Added allocation checksum.
! - Simplified convolution loop.
! - Added TauProfile write function calls.
!
! Revision 1.4  2002/05/24 21:28:30  paulv
! - Added SRF index limit finder. First convolved transmittance results! Yay.
!   Still a bug in either the transmittance data file frequency determination
!   or the SRF index calculation. Probably should read the transmittance
!   data frequencies in rather than calculate them.
!
! Revision 1.3  2002/05/24 19:15:11  paulv
! - Completed data matchup code. All valid SRFs are being read in prior to
!   the layer loop. It remains to be seen if routine processing will work
!   when a large number of SRFs can be processed. Memory limitation may
!   become an issue.
! - Added test for the number of required bands exceeding the maximum
!   allowed, currently 6. Currently this is only an issue for GOES-12
!   Imager channel 3 which spans 7 bands.
!
! Revision 1.2  2002/05/23 16:10:06  paulv
! - Completed "data searching" portion. Bands are now read in as they are
!   available. Code is a bit unwieldy though.
!
! Revision 1.1  2002/05/22 16:56:11  paulv
! Initial checkin. Code doesn't actually *do* anything at this point.
!
!
!
!
