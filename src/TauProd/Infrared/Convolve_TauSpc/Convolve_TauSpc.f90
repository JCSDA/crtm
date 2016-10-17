!
! Convolve_TauSpc
!
! Program to convolve netCDF LBL transmittance data files with instrument
! SRFs
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-May-2002
!                       paul.vandelst@ssec.wisc.edu
!
!       Modified by:    Yong Chen, CIRA/CSU 15-Dec-2010
!                       Yong.Chen@noaa.gov


PROGRAM Convolve_TauSpc


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE ProcessControl_Define
  USE ProcessControl_IO
!  USE oSRF_Define
  USE oSRF_File_Define 
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

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Convolve_TauSpc'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  INTEGER, PARAMETER :: INVALID = -1

  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: FIVE = 5.0_fp

  ! Numerical precision and transmittance tolerance.
  REAL(fp), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL(fp), PARAMETER :: TAU_TOLERANCE = FIVE * TOLERANCE

!  INTEGER, PARAMETER ::   UPWELLING_DIRECTION = 1
!  INTEGER, PARAMETER :: DOWNWELLING_DIRECTION = 2
!  CHARACTER(*), PARAMETER, DIMENSION( 2 ) :: DIRECTION_NAME = (/ 'upwelling  ', &
!                                                                 'downwelling' /)


  ! ---------
  ! Variables
  ! ---------

  CHARACTER(256) :: Message

  CHARACTER(20) :: Sensor_ID
  INTEGER       :: WMO_Satellite_ID
  INTEGER       :: WMO_Sensor_ID   
  CHARACTER(5000) :: History
  CHARACTER(256)  :: Sensor_Name
  CHARACTER(256)  :: Platform_Name
  CHARACTER(5000) :: Comment

  CHARACTER(128)              :: Tau_FilePrefix
  CHARACTER(7)                :: Tau_FileBand
  CHARACTER(263), ALLOCATABLE :: Tau_Filename(:)  ! Was DIMENSION( MAX_N_LBLBANDS ) 

  CHARACTER(256) :: Control_Filename

  LOGICAL        :: Output_Signal_File
  CHARACTER(256) :: Signal_Filename

  INTEGER       :: Idx(1)
  INTEGER       :: jIdx
  CHARACTER(40) :: jTag
  CHARACTER(9)  :: mTag
  CHARACTER(6)  :: iTag

  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER :: n_Allocates

  INTEGER :: i, j, k, l, m, n, nm
  INTEGER :: lBand, lch, lb, lspc
  INTEGER :: l1, l2
  REAL(fp) :: v1, v2, vb1, vb2

  INTEGER :: Profile_Set
  INTEGER :: Direction

  INTEGER :: Max_Band
  INTEGER :: Band1
  INTEGER :: Band2
  INTEGER :: n_Band1
  INTEGER :: n_Bands
  INTEGER :: n_Available_Bands
  INTEGER, ALLOCATABLE :: Band_Index(:)  ! Was DIMENSION( MAX_N_LBLBANDS ) 

  INTEGER :: n_Channels
  INTEGER :: n_Channels_to_Process
  INTEGER :: n_Channels_Processed

  INTEGER :: Molecule_Set_Number
  INTEGER :: Profile_Number
  INTEGER :: Angle_Number
  INTEGER :: Band_Number

  INTEGER,  ALLOCATABLE :: n_SRF_Frequencies(:)
  REAL(fp), ALLOCATABLE :: Begin_SRF_Frequency(:)
  REAL(fp), ALLOCATABLE :: End_SRF_Frequency(:)

  INTEGER, ALLOCATABLE :: Channel_Index(:)
  INTEGER, ALLOCATABLE :: Process_Index(:)

  INTEGER :: n_Spectral_Points
  REAL(fp), ALLOCATABLE, TARGET  :: Transmittance(:)
  REAL(fp),              POINTER :: Tau(:) => NULL()
  REAL(fp) :: Convolved_Tau

  TYPE(ProcessControl_type) :: ProcessControl
  INTEGER :: dF_Index
  REAL(fp) :: dF

!  TYPE(oSRF_type), ALLOCATABLE :: SRF(:)
  INTEGER,        ALLOCATABLE :: SRF_Index(:,:)

  TYPE(TauProfile_type), ALLOCATABLE :: TauProfile(:)
  TYPE(oSRF_File_type),  ALLOCATABLE :: oSRF_File(:)

  ! Level pressure array for TauProfile output.
  REAL(fp) :: Pressure(N_LEVELS)

  ! Timing variables
  INTEGER :: Hertz
  INTEGER :: Begin_Clock_Count, End_Clock_Count
  REAL(fp) :: Elapsed_Time

  ! ------
  ! Set up
  ! ------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convolve LBLRTM transmittance spectra with '//&
                        'spectrally corresponding SRFs defined in the '//&
                        'ProcessControl file.', &
                        '$Revision$' )
  ! Initialize the allocation checksum
  n_Allocates = 0
  ! By default, signal file is output
  Output_Signal_File = .TRUE.
  ! Initialize timing variables
  CALL SYSTEM_CLOCK( COUNT_RATE = Hertz )
  CALL SYSTEM_CLOCK( COUNT = Begin_Clock_Count )


  ! --------------
  ! Get user input
  ! --------------
  ! The profile set being processed
  WRITE( *, FMT = '( /5x, "Select the DEPENDENT PROFILE SET" )' )
  DO nm = 1, N_PROFILE_SETS
    WRITE( *, FMT = '( 10x, i2, ") ", a, " profile set" )' ) &
              nm, TRIM( PROFILE_SET_ID_TAG( nm ) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i1 )', &
           IOSTAT = IO_Status ) Profile_Set
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


  ! The molecule set index number
  WRITE( *, FMT     = '( /5x, "Enter the MOLECULE SET to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Molecule_Set_Number
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
  ! Extract out the index of the molecule set number. If this seems
  ! a bit convoluted, remember that the molecule set numbers are
  ! not necessarily contiguous.
  Idx = PACK( (/ ( j, j = 1, N_MOLECULE_SETS ) /), &
              Molecule_Set_Number == MOLECULE_SET_TAG_ID )
  jIdx = Idx(1)
  ! Define a string of the molecule set
  jTag = MOLECULE_SET_TAG( jIdx )
  

  ! The profile index number
  WRITE( *, FMT     = '( /5x, "Enter the PROFILE NUMBER to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Profile_Number
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
  ! Create a string of the profile number
  WRITE( mTag, '( "profile", i2.2 )' ) Profile_Number


  ! The angle index number
  WRITE( *, FMT     = '( /5x, "Enter the ANGLE NUMBER to process: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i2 )', &
           IOSTAT = IO_Status ) Angle_Number
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
  ! Create a string of the angle number
  WRITE( iTag, '( "angle", i1 )' ) Angle_Number


  !#----------------------------------------------------------------------------#
  !#                       -- GET DF index --                                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the Frequency intervals index: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( I1 )' )  dF_Index

  ! The "direction"
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
  IF ( Direction /= UPWELLING_DIRECTION   .AND. &
       Direction /= DOWNWELLING_DIRECTION       ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF
  ! Reverse LEVEL_PRESSURE array if required for the
  ! TauProfile creation call. LEVEL_PRESSURE is defined
  ! in the Tau_Production_Parameters module from the
  ! SFC->TOA
  IF ( Direction == UPWELLING_DIRECTION ) THEN
    ! Upwelling, so reverse the array
    Pressure = LEVEL_PRESSURE(N_LEVELS:1:-1)
  ELSE
    ! Downwelling, so do nothing
    Pressure = LEVEL_PRESSURE
  END IF


  ! ----------------------------------------
  ! Create the LBL transmittance file prefix
  ! ----------------------------------------
  Tau_FilePrefix = TRIM( DIRECTION_NAME( Direction ) )//'_tau.'//&
                   mTag//'_'//iTag//'_'//TRIM( jTag )//'_'


  ! ----------------------------
  ! Read the ProcessControl file
  ! ----------------------------
  WRITE( *, FMT     = '( /5x, "Enter the ProcessControl filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Control_Filename
  Control_Filename = ADJUSTL( Control_Filename )
 
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
  !dF_Index = ProcessControl%dF_Index(1)
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

  ALLOCATE( TauProfile( ProcessControl%n_Files ), &
            oSRF_File( ProcessControl%n_Files ),  & 
            STAT = Allocate_Status )

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

    ! SRF filename... simple
    ProcessControl%SRF_Filename(n) = TRIM( ProcessControl%File_Prefix( n ) )//&
                                     '.osrf.nc'

    ! Construct the TauProfile filname,
    !  <direction tag>.<profile tag>_<angle tag>_<molset tag>.<sensor tag>.TauProfile.nc
    ! for example,
    !   upwelling.profile05_angle3_ozo.hirs3_n17.TauProfile.nc
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

!    Error_Status = Inquire_SRF_netCDF( ProcessControl%SRF_Filename(n),         &
!                                       n_Points         = n_SRF_Frequencies,   &
!                                       Begin_Frequency  = Begin_SRF_Frequency, &
!                                       End_Frequency    = End_SRF_Frequency,   &
!                                       WMO_Satellite_ID = WMO_Satellite_ID,    &
!                                       WMO_Sensor_ID    = WMO_Sensor_ID,       &
!                                       Comment          = Comment              )

!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( PROGRAM_NAME, &
!                            'Error inquiring SRF file '//&
!                            TRIM( ProcessControl%SRF_Filename(n) ), &
!                            Error_Status )
!      STOP
!    END IF

    ! --------------------
    ! read oSRF data from file and fill the oSRF_File container
    ! --------------------

    Error_Status = oSRF_File_Read( &                    
                     oSRF_File(n)    , &                
                     ProcessControl%SRF_Filename(n)  )  
                     
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading SRF file '//&
                            TRIM( ProcessControl%SRF_Filename(n) ), &
                            Error_Status )
      STOP
    END IF
    
    ! For Infrared sensor, only have one band for each channel
    DO l = 1, n_Channels
      n_SRF_Frequencies(l) = oSRF_File(n)%oSRF(l)%n_Points(1)
      Begin_SRF_Frequency(l) =oSRF_File(n)%oSRF(l)%f1(1)
      End_SRF_Frequency(l) = oSRF_File(n)%oSRF(l)%f2(oSRF_File(n)%oSRF(l)%n_Bands)
    ENDDO
    
    ! --------------------------------------
    ! Determine the frequency interval index
    ! --------------------------------------

    Frequency_Interval_Loop: DO l = 1, n_Channels

      ! Compute the channel frequency interval 
      dF = ( End_SRF_Frequency(l) - Begin_SRF_Frequency(l) ) / &
      !    -------------------------------------------------
               REAL( n_SRF_Frequencies(l) - 1, fp )

      ! Compute the frequency interval index
!      dF_Index = Compute_dF_Index( dF )

      ! Is it valid?
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

      ! Compare the index
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

      ! Extract out the begin and end
      ! indices for the channel list
      l1 = ProcessControl%Channel_Index( 1, n )
      l2 = ProcessControl%Channel_Index( 2, n )

      ! Create the file (CLOBBER mode)
      Error_Status = Create_TauProfile_netCDF( ProcessControl%TauProfile_Filename(n), &
                                               Pressure, &
                                               ProcessControl%List( l1:l2 )%Channel, &
                                               (/ ZENITH_ANGLE_SECANT( Angle_Number ) /), &
                                               (/ Profile_Number /), &
                                               (/ Molecule_Set_Number /), &
                                               Release = TauProfile(n)%Release, &
                                               Version = TauProfile(n)%Version, &
                                               Sensor_ID        = TRIM(ProcessControl%File_Prefix(n)), &
                                               WMO_Satellite_ID = oSRF_File(n)%WMO_Satellite_ID, &
                                               WMO_Sensor_ID    = oSRF_File(n)%WMO_Sensor_ID, &
                                               ID_Tag  = TRIM(PROFILE_SET_ID_TAG( Profile_Set )), &
                                               Title   = TRIM(DIRECTION_NAME( Direction ))//&
                                                         ' transmittances for '//&
                                                         TRIM(ProcessControl%File_Prefix(n)), &
                                               Comment = TRIM(oSRF_File(n)%Comment) )
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

  ! The maximum band to be processed
  Max_Band = MAXVAL( ProcessControl%List%End_LBLband )

  ! The starting bands
  Band1  = MINVAL( ProcessControl%List%Begin_LBLband )
  Band2  = Band1 - 1

  ! Summation variable
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

      ! Determine the number of unprocessed channels
      ! that require the current Band1
      n_Band1 = COUNT( ProcessControl%List%Begin_LBLband == Band1 .AND. &
                       ProcessControl%List%Processed     == 0     .AND. &
                       ProcessControl%List%Data_Available               )

      ! Check that the number of bands doesn't 
      ! exceed the (arbitrary) maximum allowed
      n_Bands = Band2 - Band1 + 1
      IF ( n_Bands > MAX_N_LBLBANDS( dF_Index ) ) THEN
        Band1 = Band2
        CYCLE Band1_Increment
      END IF

      ! If channels still require the
      ! lower band limit, exit the loop
      IF ( n_Band1 /= 0 ) EXIT Band1_Increment

      ! Otherwise, increment the band limit
      Band1 = Band1 + 1

      ! If we've gone past the upper band limit,
      ! cycle the main loop to increment the
      ! upper limit.
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

        ! Update the available band variables
        n_Available_Bands = n_Available_Bands + 1
        Band_Index( n_Available_Bands ) = lBand

      ELSE

        ! No data. Flag channels requiring this band.
        WRITE( *, '( 5x, "Band number ", i3, " data not available. Flagging channels..." )' ) &
                  Band_Number

        ! Loop over channels requiring this band
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

      ! Initialise the frequency limits for min/max search
      v1 = 100000.0_fp
      v2 = 0.0_fp

      ! Only inquire the available data files
      Band_Frequency_Limit_Loop: DO lBand = 1, n_Available_Bands

        ! Get the current data file frequency limits
        ! and comment global attribute
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

        ! Keep the min and max
        v1 = MIN( v1, vb1 )
        v2 = MAX( v2, vb2 )

      END DO Band_Frequency_Limit_Loop


      ! ---------------------------------------------------
      ! Find the indices of the process-able channel subset
      ! ---------------------------------------------------

      ! Allocate an array
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

      ! Find the indices
      Process_Index = PACK( (/ (l, l = 1, n_Channels ) /), &
                            ( ProcessControl%List( Channel_Index )%Data_Available ) )

      Process_Index = Channel_Index( Process_Index )



      ! -------------------------------------
      ! Read only those SRFs that can be used
      ! -------------------------------------

      ! Allocate an SRF array and an index array
!      ALLOCATE( SRF( n_Channels_to_Process ), &
      ALLOCATE( SRF_Index( 2, n_Channels_to_Process ), &
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

      ! Read the required SRFs
      SRF_Read: DO lch = 1, n_Channels_to_Process

        ! Extract the current file index
        n = ProcessControl%List( Process_Index( lch ) )%File_Index
 
        l = Process_Index( lch ) - ProcessControl%Channel_Index( 1, n ) + 1


        ! Read the data
!        Error_Status = Read_SRF_netCDF( ProcessControl%SRF_Filename(n), &
!                                        ProcessControl%List( Process_Index( lch ) )%Channel, &
!                                        SRF( lch ) )

!        IF ( Error_Status /= SUCCESS ) THEN
!          WRITE( Message, '( "Error reading channel ", i4, " SRF from ", a, ". Exiting..." )' ) &
!                          ProcessControl%List( Process_Index( lch ) )%Channel, &
!                          TRIM( ProcessControl%SRF_Filename(n) )
!          CALL Display_Message( PROGRAM_NAME, &
!                                TRIM( Message ), &
!                                Error_Status )
!          EXIT Main_Processing_Loop
!        END IF

        WRITE( *, '( 5x, "Channel ", i4, " SRF read from ", a, ", Channel index is", i4 )' ) &
                  ProcessControl%List( Process_Index( lch ) )%Channel, &
                  TRIM( ProcessControl%SRF_Filename(n) ), l

        ! Find the indices where the SRF slots
        ! into the transmittance spectrum
        SRF_Index( 1, lch ) = Compute_Frequency_Index( v1, &
                                                       FREQUENCY_INTERVAL( dF_Index ), &
                                                       oSRF_File(n)%oSRF( l )%f1(1) )
        SRF_Index( 2, lch ) = Compute_Frequency_Index( v1, &
                                                       FREQUENCY_INTERVAL( dF_Index ), &
                                                       oSRF_File(n)%oSRF( l )%f2( oSRF_File(n)%oSRF( l )%n_Bands ))

        ! Double check the index values
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

          ! Set the band index
          lb = Band_Index( lBand )

          ! Set the end index of transmittance
          ! data for this band read      
          l2 = l1 + N_FREQUENCIES( dF_Index ) - 1

          ! Double check the end index
          IF ( l2 > n_Spectral_Points ) THEN
            WRITE( Message, '( "Calculated end index for transmittance array, ", i6, &
                              &", is greater than the array size, ", i6, ". Exiting..." )' ) &
                            l2, n_Spectral_Points
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM( Message ), &
                                  FAILURE )
            EXIT Main_Processing_Loop
          END IF

          ! Alias the transmittance array
          Tau => Transmittance(l1:l2)

          ! Read the transmittance into the optical depth array
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

          ! Update the begin index
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

!          Convolved_Tau = SUM( Tau * SRF( lch )%Response ) * FREQUENCY_INTERVAL( dF_Index )
          Convolved_Tau = SUM( Tau * oSRF_File(n)%oSRF( l )%Response(1)%Arr ) !* FREQUENCY_INTERVAL( dF_Index )


          ! -------------------------
          ! Save the normalised value
          ! -------------------------
          !                       i m,j
!          TauProfile( n )%Tau(k,l,1,1,1) =       Convolved_Tau       / &
          !                                ------------------------
!                                           SRF( lch )%Summation_SRF
          TauProfile( n )%Tau(k,l,1,1,1) =       Convolved_Tau       / &
          !                                ------------------------
                                           SUM(oSRF_File(n)%oSRF( l )%Response(1)%Arr) 

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

        ! Check for any *really* bad transmittances. The values will
        ! still be "corrected" in the next section immediately following,
        ! but this flags the user to inspect the output to see if it 
        ! makes sense.
        IF ( ANY( TauProfile( n )%Tau(:,l,1,1,1) > ( ONE  + TAU_TOLERANCE ) ) .OR. &
             ANY( TauProfile( n )%Tau(:,l,1,1,1) < ( ZERO - TAU_TOLERANCE ) )      ) THEN

          WRITE( Message, '( "Non-physical transmittances found for ", a, &
                             " channel ", i4, ", angle secant ", f5.2, "." )' ) &
                          TRIM( ProcessControl%File_Prefix( n ) ), &
                          ProcessControl%List( Process_Index( lch ) )%Channel, &
                          ZENITH_ANGLE_SECANT( Angle_Number )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                WARNING )
          WRITE( *, '( "Transmittance precision tolerance: ", f23.20, &
                     &/"transmittances:" )' ) TAU_TOLERANCE
          WRITE( *, '( 4( 1x, f21.18, : ) )' ) TauProfile( n )%Tau(:,l,1,1,1)
        END IF

        ! Correct any transmittances for precision-related
        ! "non-physicalness", e.g. some ozone only transmittances
        ! for very ozone transparent channels have values
        ! like 1.00000000000000004 when the high-resolution
        ! values are never greater than 1.0. This is most likely
        ! due to the accumulation of numerical errors during the
        ! summation for the convolution.
        WHERE( TauProfile( n )%Tau(:,l,1,1,1) > ONE )
          TauProfile( n )%Tau(:,l,1,1,1) = ONE
        END WHERE

        WHERE( TauProfile( n )%Tau(:,l,1,1,1) < ZERO )
          TauProfile( n )%Tau(:,l,1,1,1) = ZERO
        END WHERE

      END DO Check_Channel



      !#------------------------------------------------------------------------#
      !#                   -- LET THE DEALLOCATIONS BEGIN!                   #
      !#------------------------------------------------------------------------#

      ! ------------------------
      ! Deallocate the SRF array
      ! ------------------------

      ! Destroy the structures
!      Error_Status = oSRF_File_Destroy( oSRF_File )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SRF array.', &
                              FAILURE )
        STOP
      END IF

      ! Deallocate the arrays
!      DEALLOCATE( SRF, SRF_Index, STAT = Allocate_Status )
      DEALLOCATE( SRF_Index, STAT = Allocate_Status )

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

      ! Disable signal file output
      Output_Signal_File = .FALSE.

      ! Output error message
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

      ! Disable signal file output
      Output_Signal_File = .FALSE.

      ! Output error message
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

  ! Deallocate the pointer members
  Error_Status = Destroy_TauProfile( TauProfile )
  CALL oSRF_File_Destroy( oSRF_File )

 
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the TauProfile structure array.', &
                          WARNING )
  END IF

  ! Deallocate the actual array
  DEALLOCATE( TauProfile, oSRF_File, STAT = Allocate_Status )

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

  ! Get the end time
  CALL SYSTEM_CLOCK( COUNT = End_Clock_Count )

  ! Output execution time
  Elapsed_Time = REAL( End_Clock_Count - Begin_Clock_Count, fp ) / &
  !              ----------------------------------------------------
                              REAL( Hertz, fp )
  Elapsed_Time = Elapsed_Time / 3600.0_fp
  WRITE( *, '( /5x, "Elapsed time: ", f5.2, "hrs" )' ) Elapsed_Time

END PROGRAM Convolve_TauSpc
