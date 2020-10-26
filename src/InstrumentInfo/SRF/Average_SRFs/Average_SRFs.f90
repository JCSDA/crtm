
PROGRAM Average_SRFs


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE List_File_Utility

  USE Integrate

  USE SRF_Define
  USE SRF_netCDF_IO
  USE SRF_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME = 'Average_SRFs'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Average_SRFs.f90,v 2.4 2006/08/15 20:51:04 wd20pd Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE = 1.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONEpointFIVE = 1.5_fp_kind
  
  INTEGER, PARAMETER :: INTERPOLATION_ORDER = 3


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: List_Filename
  CHARACTER( 256 ) :: Output_SRF_Filename

  TYPE( Character_List_File_type ) :: Input_SRF_Filename_List
  CHARACTER( 256 ) :: Input_SRF_Filename

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  INTEGER :: n, n_Files
  INTEGER :: l, n_Channels, n_Channels_Save
  INTEGER :: n_SRF_Points

  CHARACTER( 5000 ) :: History
  CHARACTER(  256 ) :: Sensor_Name
  CHARACTER(  256 ) :: Platform_Name
  CHARACTER( 5000 ) :: Comment

  INTEGER,          DIMENSION( : ), ALLOCATABLE :: Channel_List,    Channel_List_Save
  INTEGER,          DIMENSION( : ), ALLOCATABLE :: n_Points
  REAL( fp_kind ),  DIMENSION( : ), ALLOCATABLE :: Begin_Frequency, Begin_Frequency_Save
  REAL( fp_kind ),  DIMENSION( : ), ALLOCATABLE :: End_Frequency,   End_Frequency_Save
  INTEGER :: NCEP_Sensor_ID,   NCEP_Sensor_ID_Save
  INTEGER :: WMO_Satellite_ID, WMO_Satellite_ID_Save
  INTEGER :: WMO_Sensor_ID,    WMO_Sensor_ID_Save

  REAL( fp_kind ),  DIMENSION( : ), ALLOCATABLE :: Min_Begin_Frequency
  REAL( fp_kind ),  DIMENSION( : ), ALLOCATABLE :: Max_End_Frequency
  REAL( fp_kind ),  DIMENSION( : ), ALLOCATABLE :: Max_Delta_Frequency

  REAL( fp_kind ) :: Averaging_Factor

  TYPE( SRF_type ) :: Input_SRF
  TYPE( SRF_type ) :: iSRF
  TYPE( SRF_type ) :: Output_SRF



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to average SRFs read in from separate netCDF   ")' )
  WRITE( *, '( 5x, "   format SRF data files.                               ")' )
  WRITE( *, '(/5x, " $Revision: 2.4 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                          -- GET USER INPUTS --                             #
  !#----------------------------------------------------------------------------#

  ! ------------------------
  ! The input list file name
  ! ------------------------

  WRITE( *, FMT     = '( /5x, "Enter the input SRF list filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) List_Filename
  List_Filename = ADJUSTL( List_Filename )

  ! -----------------------
  ! The output SRF filename
  ! -----------------------

  WRITE( *, FMT     = '( /5x, "Enter the output SRF netCDF filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Output_SRF_Filename
  Output_SRF_Filename = ADJUSTL( Output_SRF_Filename )



  !#----------------------------------------------------------------------------#
  !#       -- READ THE LIST FILE CONTAINING THE FILENAMES TO AVERAGE --         #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_List_File( List_Filename, &
                                 Input_SRF_Filename_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading list file '//TRIM( List_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ----------------------------
  ! Retrieve the number of files
  ! ----------------------------

  n_Files = Get_List_Size( Input_SRF_Filename_List )

  IF ( n_Files < 2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Less than 2 files in list file. No averaging to do.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#              -- LOOP OVER THE INPUT FILE CHECK THEY "MATCH" --             #
  !#----------------------------------------------------------------------------#

  Input_File_Check_Loop: DO n = 1, n_Files


    ! ------------------------
    ! Get the current filename
    ! ------------------------

    Error_Status = Get_List_Entry( Input_SRF_Filename_List, n, &
                                   Input_SRF_Filename )


    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error retrieving filename from list for file checking.', &
                            FAILURE )
      STOP
    END IF


    ! ---------------------------
    ! Read the number of channels
    ! ---------------------------

    Error_Status = Inquire_SRF_netCDF( TRIM( Input_SRF_Filename ), &
                                       n_Channels = n_Channels )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading n_Channels dimension from the netCDF SRF file '//&
                            TRIM( Input_SRF_Filename ), &
                            Error_Status )
      STOP
    END IF


    ! ----------------------------------------------------
    ! Save the number of channels, channel list, frequency
    ! limits and IDs for checking subsequent files values
    ! ----------------------------------------------------

    First_File_Check: IF ( n == 1 ) THEN


      ! -- Save the number of channels
      n_Channels_Save = n_Channels

      ! -- Allocate the list data and frequency limit arrays
      ALLOCATE( Channel_List( n_Channels ), &
                n_Points( n_Channels ), &
                Begin_Frequency( n_Channels ), &
                End_Frequency( n_Channels ), &
                Channel_List_Save( n_Channels ), &
                Begin_Frequency_Save( n_Channels ), &
                End_Frequency_Save( n_Channels ), &
                Min_Begin_Frequency( n_Channels ), &
                Max_End_Frequency( n_Channels ), &
                Max_Delta_Frequency( n_Channels ), &
                STAT = Allocate_Status )


      IF ( Allocate_Status /= 0 ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating SRF data arrays.', &
                              FAILURE )
        STOP
      END IF


      ! -- Inquire the current file for the list data
      Error_Status = Inquire_SRF_netCDF( TRIM( Input_SRF_Filename ), &
                                         Channel_List     = Channel_List_Save, &
                                         n_Points         = n_Points, &
                                         Begin_Frequency  = Begin_Frequency_Save, &
                                         End_Frequency    = End_Frequency_Save, &
                                         NCEP_Sensor_ID   = NCEP_Sensor_ID_Save, &
                                         WMO_Satellite_ID = WMO_Satellite_ID_Save, &
                                         WMO_Sensor_ID    = WMO_Sensor_ID_Save, &
                                         History          = History, &
                                         Sensor_Name      = Sensor_Name, &
                                         Platform_Name    = Platform_Name )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading channel, frequency, and Sensor ID data '//&
                              'from netCDF SRF file '//&
                              TRIM( Input_SRF_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -- Initialise some of the arrays
      Min_Begin_Frequency = Begin_Frequency_Save
      Max_End_Frequency   = End_Frequency_Save
      Max_Delta_Frequency = Compute_Delta_Frequency( Min_Begin_Frequency, &
                                                     Max_End_Frequency, &
                                                     n_Points )

    ELSE First_File_Check


      ! ----------------------------
      ! Check the number of channels
      ! ----------------------------

      IF ( n_Channels /= n_Channels_Save ) THEN
        WRITE( Message, '( "Number of channels in file ", a, " (",i4, &
                          &") is different from that expected (",i4,")." )' ) &
                        TRIM( Input_SRF_Filename ), n_Channels, n_Channels_Save
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF


      ! ---------------------------
      ! Get the current set of data
      ! ---------------------------

      Error_Status = Inquire_SRF_netCDF( TRIM( Input_SRF_Filename ), &
                                         Channel_List     = Channel_List,    &
                                         n_Points         = n_Points, &
                                         Begin_Frequency  = Begin_Frequency, &
                                         End_Frequency    = End_Frequency, &
                                         NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                         WMO_Satellite_ID = WMO_Satellite_ID, &
                                         WMO_Sensor_ID    = WMO_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading channel, frequency, and Sensor ID data '//&
                              'from netCDF SRF file '//&
                              TRIM( Input_SRF_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------------------------------------------
      ! Compare the channel list data. The comparison is not too sophisticated,
      ! it assumes the ORDER of the data is the same as well as the value.
      ! This is not strictly necessary, but to allow different channel
      ! ordering would make this comparison too much of a PITA. :o)
      ! -----------------------------------------------------------------------

      IF ( ANY( ( Channel_List - Channel_List_Save ) /= 0 ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Channel_List from netCDF SRF file'//&
                              TRIM( Input_SRF_Filename )//&
                              ' is different.', &
                              FAILURE )
        STOP
      END IF


      ! ---------------------------------------------------------------
      ! Determine the frequency limits. Note that the frequency limits
      ! determined here may sit outside the limits of some of the SRFs
      ! to be averaged. For those SRFs, the "outside" points are set to
      ! zero. This is done to avoid cutting off potentially significant
      ! parts of an SRF.
      ! ---------------------------------------------------------------

      Min_Begin_Frequency = MIN( Min_Begin_Frequency, Begin_Frequency )
      Max_End_Frequency   = MAX( Max_End_Frequency, End_Frequency )
      Max_Delta_Frequency = MAX( Max_Delta_Frequency, &
                                 Compute_Delta_Frequency( Begin_Frequency, &
                                                          End_Frequency,   &
                                                          n_Points         ) ) 

      ! -------------------
      ! Compare the ID data
      ! -------------------

      ! -- The NCEP Sensor ID
      IF ( NCEP_Sensor_ID /= NCEP_Sensor_ID_Save ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'NCEP_Sensor_ID from netCDF SRF file'//&
                              TRIM( Input_SRF_Filename )//&
                              ' is different.', &
                              FAILURE )
        STOP
      END IF

      ! -- The WMO Satellite ID
      IF ( WMO_Satellite_ID /= WMO_Satellite_ID_Save ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'WMO_Satellite_ID from netCDF SRF file'//&
                              TRIM( Input_SRF_Filename )//&
                              ' is different.', &
                              FAILURE )
        STOP
      END IF

      ! -- The WMO Sensor ID
      IF ( WMO_Sensor_ID /= WMO_Sensor_ID_Save ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'WMO_Sensor_ID from netCDF SRF file'//&
                              TRIM( Input_SRF_Filename )//&
                              ' is different.', &
                              FAILURE )
        STOP
      END IF

    END IF First_File_Check

  END DO Input_File_Check_Loop


  ! ---------------------------------------------------
  ! Compute the averaging weight factor. I do this so
  ! when I average I can multiply across an array
  ! (faster?) rather than do a division across an array
  ! ---------------------------------------------------

  Averaging_Factor = ONE / REAL( n_Files, fp_kind )



  !#----------------------------------------------------------------------------#
  !#                -- DEALLOCATE SOME OF THE LIST DATA FILES --                #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Begin_Frequency, &
              End_Frequency, &
              Channel_List_Save, &
              Begin_Frequency_Save, &
              End_Frequency_Save, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating no-longer-needed list arrays.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                 -- CREATE THE OUTPUT NETCDF SRF FILE --                    #
  !#----------------------------------------------------------------------------#

  Comment = 'Interpolation to common frequency grid performed. Min and Max frequencies '//&
            'of all detectors used as interpolation endpoints.'

  Error_Status = Create_SRF_netCDF( TRIM( Output_SRF_Filename ), &
                                    Channel_List, &
                                    NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                    WMO_Satellite_ID = WMO_Satellite_ID, &
                                    WMO_Sensor_ID    = WMO_Sensor_ID, &
                                    Title         = 'Detector-averaged SRFs for '//&
                                                    TRIM( Platform_Name )//' '//&
                                                    TRIM( Sensor_Name ), &
                                    History       = PROGRAM_RCS_ID//'; '//TRIM( History ), &
                                    Sensor_name   = TRIM( Sensor_Name ), &
                                    Platform_name = TRIM( Platform_Name ), &
                                    Comment       = TRIM( Comment ) )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM( Output_SRF_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                -- BEGIN THE AVERAGING LOOP OVER CHANNELS --                #
  !#----------------------------------------------------------------------------#

  Channel_Average_Loop: DO l = 1, n_Channels

    WRITE( *, '( /5x, "Averaging ", a, " ", a, " channel # ", i4 )' ) &
              TRIM( Platform_Name ), TRIM( Sensor_Name ), Channel_List( l )


    ! ----------------------------------------------------------------
    ! Determine the number of SRF points and confirm the end frequency
    ! ----------------------------------------------------------------

    n_SRF_Points = INT( ONEpointFIVE + ( ( Max_End_Frequency(l) - Min_Begin_Frequency(l) ) / &
    !                                    -------------------------------------------------
                                                       Max_Delta_Frequency(l)              ) )

    Max_End_Frequency(l) = Min_Begin_Frequency(l) + &
                           ( REAL( n_SRF_Points - 1, fp_kind ) * Max_Delta_Frequency(l) )

    WRITE( *, '( 10x, "Channel AVE f1, f2, df, and n: ", 2(f12.6), 2x, f11.9, 1x, i5 )' ) &
              Min_Begin_Frequency(l), Max_End_Frequency(l), Max_Delta_Frequency(l), n_SRF_Points


    ! ---------------------------------
    ! Allocate the output SRF Structure
    ! ---------------------------------

    Error_Status = Allocate_SRF( n_SRF_Points, &
                                 Output_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error allocating output SRF structure for channel ", i4, "." )' ) &
                      Channel_List( l )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

    ! -- Copy over the available scalar data
    Output_SRF%Sensor_Name   = TRIM( Sensor_Name )
    Output_SRF%Platform_Name = TRIM( Platform_Name )

    Output_SRF%NCEP_Sensor_Id   = NCEP_Sensor_Id  
    Output_SRF%WMO_Satellite_Id = WMO_Satellite_Id
    Output_SRF%WMO_Sensor_Id    = WMO_Sensor_Id   
    Output_SRF%Channel          = Channel_List( l )

    Output_SRF%Begin_Frequency = Min_Begin_Frequency(l)
    Output_SRF%End_Frequency   = Max_End_Frequency(l)


    ! -- Compute the output frequency grid
    Error_Status = Frequency_SRF( Output_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error computing frequency grid for channel ", i4, " output SRF." )' ) &
                      Channel_List( l )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

    ! -- Initialise the SRF response array
    Output_SRF%Response = ZERO


    ! -------------------
    ! Begin the file loop
    ! -------------------

    File_Average_Loop: DO n = 1, n_Files


      ! ------------------------
      ! Get the current filename
      ! ------------------------

      Error_Status = Get_List_Entry( Input_SRF_Filename_List, n, &
                                     Input_SRF_Filename )


      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error retrieving filename from list for file averaging.', &
                              FAILURE )
        STOP
      END IF


      ! ---------------------------------------------
      ! Read the SRF for the current file and channel
      ! ---------------------------------------------

      Error_Status = Read_SRF_netCDF( TRIM( Input_SRF_Filename ), &
                                      Channel_List( l ), &
                                      Input_SRF )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading channel ", i4, " SRF from ", a, "." )' ) &
                        Channel_List( l ), TRIM( Input_SRF_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      WRITE( *, '( 10x, "File #", i5, " f1, f2, df, and n: ", 2(f12.6), 2x, f11.9, 1x, i5 )' ) &
                n, &
                Input_SRF%Begin_Frequency, Input_SRF%End_Frequency, &
                Compute_Delta_Frequency( Input_SRF%Begin_Frequency, Input_SRF%End_Frequency, &
                                         Input_SRF%n_Points ), &
                Input_SRF%n_Points


      ! ------------------------------------------------------
      ! Interpolate the input SRF to the output frequency grid
      ! ------------------------------------------------------

      Error_Status = Interpolate_SRF( Output_SRF%Frequency, &
                                      Input_SRF, &
                                      iSRF, &
                                      Order = INTERPOLATION_ORDER )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error interpolating input SRF for channel ", i4, "." )' ) &
                        Channel_List( l )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! ---------------------------
      ! Accumulate the SRF response
      ! ---------------------------

      Output_SRF%Response = Output_SRF%Response + iSRF%Response



      ! ------------------------------------------------
      ! Destroy the input and interpolated SRF structure
      ! ------------------------------------------------

      Error_Status = Destroy_SRF( Input_SRF )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying input SRF structure for channel ", i4, " from ", a, "." )' ) &
                        Channel_List( l ), TRIM( Input_SRF_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      Error_Status = Destroy_SRF( iSRF )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying inteprolated SRF structure for channel ", i4, " from ", a, "." )' ) &
                        Channel_List( l ), TRIM( Input_SRF_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

    END DO File_Average_Loop


    ! ---------------------------------------------
    ! Average and normalise the output SRF response
    ! ---------------------------------------------

    Output_SRF%Response = Averaging_Factor * Output_SRF%Response
    Output_SRF%Response = Output_SRF%Response / MAXVAL( Output_SRF%Response )


    ! ----------------------------------
    ! Integrate the average SRF response
    ! ----------------------------------

    Error_Status = Integrate_SRF( Output_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error occurred integrating channel #", i4, " average SRF." )' ) &
                      Output_SRF%Channel
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! ------------------------------
    ! Write the averaged SRF to file
    ! ------------------------------

    Error_Status = Write_SRF_netCDF( TRIM( Output_SRF_Filename ), &
                                     Output_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing channel ", i4, " average SRF to ", a, "." )' ) &
                      Channel_List( l ), TRIM( Output_SRF_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------------------------------
    ! Destroy the output SRF structure for the next channel
    ! -----------------------------------------------------

    Error_Status = Destroy_SRF( Output_SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying output SRF structure for channel ", i4, "." )' ) &
                      Channel_List( l )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Channel_Average_Loop



  !#----------------------------------------------------------------------------#
  !#                    -- DEALLOCATE CHANNEL LIST ARRAY --                     #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Channel_List, &
              n_Points, &
              Min_Begin_Frequency, &
              Max_End_Frequency, &
              Max_Delta_Frequency, &
              STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error deallocating data arrays.', &
                          WARNING )
  END IF

CONTAINS

  ELEMENTAL FUNCTION Compute_Delta_Frequency( f1, f2, n ) RESULT ( df )
    REAL( fp_kind ), INTENT( IN ) :: f1, f2
    INTEGER,         INTENT( IN ) :: n
    REAL( fp_kind ) :: df

    df = ( f2 - f1 ) / REAL( n-1, fp_kind )

  END FUNCTION Compute_Delta_Frequency

END PROGRAM Average_SRFs


