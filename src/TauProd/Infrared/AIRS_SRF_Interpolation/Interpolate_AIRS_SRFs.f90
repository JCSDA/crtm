!
! NAME:
!       Interpolate_AIRS_SRFs
!
! PURPOSE:
!       Program to read the requested AIRS SRF data from a netCDF format
!       file and interpolate the SRFs to the same frequency grid  used in
!       the line-by-line transmittance data files. The interpolated files
!       are output in netCDF format by module based upon the SensorInfo
!       file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-May-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Interpolate_AIRS_SRFs


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Interpolate_Utility
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  USE AIRS_SRF_Define
  USE AIRS_SRF_netCDF_Reader
  USE SRF_Define
  USE SRF_netCDF_IO
  USE Tau_Production_Parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME = 'Interpolate_AIRS_SRFs'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &

  ! -- Input AIRS SRF filename
  CHARACTER( * ),  PARAMETER ::  AIRS_SRF_FILENAME = 'airs_srf.nc'

  ! -- Numeric literal
  REAL( fp_kind ), PARAMETER :: ONEpointFIVE = 1.5_fp_kind

  ! -- The index for the required frequency interval = 2
  ! -- This corresponds to the 0.0025cm^-1 frequency interval
  ! -- from the Tau_Production_Parameters module.
  INTEGER, PARAMETER :: INTERPOLATION_INDEX = 2

  ! -- Interpolation order
  ! -- If 1 == linear
  ! --    3 == cubic
  INTEGER, PARAMETER :: ORDER = 1

  ! -- Keyword set (TRUE/ON) value
  INTEGER, PARAMETER :: SET = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_FileNAME
  CHARACTER( 256 ) :: iSRF_FileNAME

  INTEGER :: Error_Status

  CHARACTER( 256 ) :: Comment

  REAL( fp_kind ) :: dF
  INTEGER :: l, n_iPoints, i1, i2

  TYPE( AIRS_SRF_type ) :: AIRS_SRF
  TYPE( SRF_type )      :: iSRF

  INTEGER :: n_Sensors, n
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List



  !#----------------------------------------------------------------------------#
  !#                   -- INITIALIZE LINKED LIST STRUCTURE --                   #
  !#----------------------------------------------------------------------------#

  SensorInfo_List = New_SensorInfo_List()



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  CALL Program_Message(PROGRAM_NAME, &
                       'Program to read the netCDF format AIRS SRF data files and '//&
                       'interpolate the requested SRFs to the same frequency '//&
                       'grid used in the line-by-line transmittance data files.', &
                       '$Revision: 2.3 $' )


  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter a SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )


  ! -----------------------------------
  ! Populate the SensorInfo linked list
  ! -----------------------------------

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! Count the number of sensors
  ! ---------------------------

  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#           -- DEFINE THE FREQUENCY INTERPOLATION INFORMATION --             #
  !#----------------------------------------------------------------------------#

  dF = FREQUENCY_INTERVAL( INTERPOLATION_INDEX )



  !#----------------------------------------------------------------------------#
  !#            --INQUIRE THE INPUT AIRS SRF FILE FOR VERSION INFO --           #
  !#----------------------------------------------------------------------------#

  Error_Status = Inquire_AIRS_SRF_netCDF( AIRS_SRF_FILENAME, &
                                          Version = Comment )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error inquiring input AIRS SRF file '//AIRS_SRF_FILENAME, &
                          FAILURE )
    STOP
  END IF

  ! -- Modify COMMENT attribute
  Comment = 'Interpolated SRFs for transmittance production;'//TRIM( Comment )



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors


    ! ---------------------------------------------
    ! Get the current SensorInfo data from the list
    ! ---------------------------------------------

    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- OPERATE ONLY ON AIRS --                         #
    !#--------------------------------------------------------------------------#

    AIRS_Only: IF ( TRIM( SensorInfo%Sensor_Name ) == 'AIRS' ) THEN


      ! ---------------------------------
      ! Construct the output SRF filename
      ! ---------------------------------

      iSRF_Filename = TRIM( SensorInfo%File_Prefix )//'.srf.nc'

      WRITE( *, '(/5x, "Processing SRF file ", a, "..." )' ) TRIM( iSRF_FileNAME )


      ! --------------------------------------------
      ! Create the output interpolated srf data file
      ! --------------------------------------------

      Error_Status = Create_SRF_netCDF( TRIM( iSRF_FileNAME ), &
                                        SensorInfo%Sensor_Channel, &
                                        NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID, &
                                        WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                        WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &
                                        Title         = 'Normalized sounder spectral response functions for '//&
                                                        TRIM( SensorInfo%File_Prefix ), &
                                        History       = PROGRAM_RCS_ID, &
                                        Sensor_Name   = TRIM( SensorInfo%Sensor_Name ), &
                                        Platform_Name = TRIM( SensorInfo%Satellite_Name ), &
                                        Comment       = TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( PROGRAM_NAME, &
                              'Error creating '//TRIM( iSRF_FileNAME ), &
                              Error_Status )
        STOP
      END IF



      !#------------------------------------------------------------------------#
      !#                     -- LOOP OVER THE CHANNELS --                       #
      !#------------------------------------------------------------------------#

      Channel_Loop: DO l = 1, SensorInfo%n_Channels


        ! ---------------------------------------
        ! Read the current channel input SRF data
        ! ---------------------------------------

        Error_Status = Read_AIRS_SRF_netCDF( AIRS_SRF_FILENAME, &
                                             SensorInfo%Sensor_Channel(l), &
                                             AIRS_SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading AIRS channel #", i4, " SRF from ", a )' ) &
                          SensorInfo%Sensor_Channel(l), AIRS_SRF_FILENAME
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! ------------------------------------
        ! Assign the channel number and sensor
        ! IDs to the interpolation structure
        ! ------------------------------------

        iSRF%Channel = SensorInfo%Sensor_Channel(l)
        iSRF%NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID
        iSRF%WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID
        iSRF%WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID


        ! ----------------------------------------------------
        ! Calculate the begin and end interpolated frequencies
        ! and the number of interpolated points
        ! ----------------------------------------------------

        ! -- The begin point and frequency
        i1 = INT( ONEpointFIVE + ( ( AIRS_SRF%Begin_Frequency - FREQUENCY_BEGIN ) / &
             !                     ----------------------------------------------
                                                         dF                       ) )

        iSRF%Begin_Frequency = FREQUENCY_BEGIN + ( REAL( i1 - 1, fp_kind ) * dF )


        ! -- The end point and frequency
        i2 = INT( ONEpointFIVE + ( ( AIRS_SRF%End_Frequency - FREQUENCY_BEGIN ) / &
             !                     --------------------------------------------
                                                        dF                      ) )

        iSRF%End_Frequency = FREQUENCY_BEGIN + ( REAL( i2 - 1, fp_kind ) * dF )

        ! -- Total number of points
        n_iPoints = i2 - i1 + 1


        ! ----------------------------------------------
        ! Allocate the interpolated SRF structure arrays
        ! ----------------------------------------------

        Error_Status = Allocate_SRF( n_iPoints, iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error allocating iSRF arrays for channel #", i4, "." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! ----------------------------------------
        ! Compute the interpolation frequency grid
        ! ----------------------------------------

        Error_Status = Frequency_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error computing the interpolation frequency grid ", &
                            &"for channel #", i5 )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! -------------------------
        ! Interpolate the input SRF
        ! -------------------------

        Error_Status = Polynomial_Interpolate( AIRS_SRF%Frequency, &
                                               AIRS_SRF%Response, &
                                               iSRF%Frequency, &
                                               iSRF%Response, &
                                               Order = ORDER )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error interpolating SRF for channel #", i4 )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Normalise it
        iSRF%Response = iSRF%Response / MAXVAL( iSRF%Response )


        ! ------------------------------
        ! Integrate the interpolated SRF
        ! ------------------------------

        Error_Status = Integrate_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error integrating channel #", i4, " interpolated SRF." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Output the integration comparisons
        WRITE( *, '( 10x, "Interpolated AIRS channel ", i5, " SRF. ", &
                         &"Integrated SRF: ", es13.6, 5x, &
                         &"Summed SRF: ", es13.6, 5x, &
                         &"% difference: ", es13.6 )' ) &
                  SensorInfo%Sensor_Channel(l), &
                  iSRF%Integrated_SRF, &
                  iSRF%Summation_SRF, &
                  100.0_fp_kind * ( iSRF%Integrated_SRF - iSRF%Summation_SRF ) / iSRF%Integrated_SRF


        ! ----------------------------------------
        ! Write the channel SRF to the netCDF file
        ! ----------------------------------------

        Error_Status = Write_SRF_netCDF( TRIM( iSRF_FileNAME ), &
                                         iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred writing channel #", i5, &
                            &" interpolated SRF to ", a, "." )' ) &
                          iSRF%Channel, TRIM( iSRF_FileNAME )
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF


        ! ---------------------------------------
        ! Destroy SRF structures for next channel
        ! ---------------------------------------

        ! -- The interpolated SRF
        Error_Status = Destroy_SRF( iSRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred destroying channel #", i4, &
                            &" iSRF structure." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- The input SRF
        Error_Status = Destroy_AIRS_SRF( AIRS_SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred destroying channel #", i4, &
                           & " AIRS_SRF structure." )' ) &
                          SensorInfo%Sensor_Channel(l)
          CALL display_message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                Error_Status     )
          STOP
        END IF

      END DO Channel_Loop

    END IF AIRS_Only



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo data for sensor # ", i5 )' ) n
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM Interpolate_AIRS_SRFs
