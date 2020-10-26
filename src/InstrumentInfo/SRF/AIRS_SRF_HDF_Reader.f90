
MODULE AIRS_SRF_HDF_Reader


  ! ----------
  ! Module use
  ! ----------

  USE type_kinds
  USE Message_Handler

  USE AIRS_SRF_Define

  USE hdf_utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Open_AIRS_SRF_HDF
  PUBLIC :: Close_AIRS_SRF_HDF
  PUBLIC :: Inquire_AIRS_SRF_HDF
  PUBLIC :: Read_AIRS_SRF_HDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Maximum number of channels
  INTEGER, PRIVATE, PARAMETER :: MAX_CHANNELS  = 2378


  ! ----------------
  ! Module variables
  ! ----------------

  ! -- Message output
  CHARACTER( 256 ), PRIVATE :: message


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC MAXVAL, MINVAL, &
            PRESENT, &
            TRIM


CONTAINS



  FUNCTION get_channel_index( channel_list, channel ) RESULT( channel_index )

    INTEGER, DIMENSION( : ), INTENT( IN ) :: channel_list
    INTEGER :: channel
    INTEGER :: channel_index

    channel_index = MINLOC( ABS( channel_list - channel ), DIM = 1 )

  END FUNCTION get_channel_index






  FUNCTION Open_AIRS_SRF_HDF( HDF_fileNAME, &  ! Input
                              HDF_fileID,   &  ! Output
                              message_log ) &  ! Error messaging

                            RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: HDF_fileNAME

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: HDF_fileID

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_AIRS_SRF_HDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: fileID



    !#--------------------------------------------------------------------------#
    !#                 -- OPEN THE HDF4 DATA FILE FOR READING --                #
    !#--------------------------------------------------------------------------#

    fileID = SFSTART( HDF_fileNAME, DFACC_READ )

    IF ( fileID == FAIL ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error establishing interface to AIRS SRF HDF file, ' // &
                            TRIM( HDF_fileNAME ) // &
                            '.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                               -- DONE --                                 #
    !#--------------------------------------------------------------------------#

    HDF_fileID   = fileID
    error_status = SUCCESS

  END FUNCTION Open_AIRS_SRF_HDF





  FUNCTION Close_AIRS_SRF_HDF( HDF_fileID,   &  ! Input
                               message_log ) &  ! Error messaging

                             RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                    INTENT( IN ) :: HDF_fileID

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,  INTENT( IN ) :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Close_AIRS_SRF_HDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: hdf_error_status



    !#--------------------------------------------------------------------------#
    !#                     -- CLOSE THE HDF DATA FILE --                        #
    !#--------------------------------------------------------------------------#

    hdf_error_status = SFEND( HDF_fileID )

    IF ( hdf_error_status == FAIL ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error closing AIRS SRF HDF file.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                               -- DONE --                                 #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION Close_AIRS_SRF_HDF








  FUNCTION Inquire_AIRS_SRF_HDF( HDF_fileNAME,      &  ! Input
                                 HDF_fileID,        &  ! Input

                                 n_points,          &  ! Optional output
                                 n_channels,        &  ! Optional output
                                 channel_list,      &  ! Optional output
                                 channel_frequency, &  ! Optional output
                                 channel_fwhm,      &  ! Optional output

                                 message_log   )    &  ! Error messaging

                               RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ),                            INTENT( IN )  :: HDF_fileNAME
    INTEGER,                                   INTENT( IN )  :: HDF_fileID

    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_points
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_channels
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: channel_list
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: channel_frequency
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: channel_fwhm

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_AIRS_SRF_HDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: hdf_error_status

    INTEGER                                    :: n_sds_dimensions
    INTEGER, DIMENSION( MAX_N_SDS_DIMENSIONS ) :: sds_dimensions

    INTEGER :: n

    ! -- Read values declared as same type in HDF file.
    INTEGER( AIRS_SRF_chanid_type ), DIMENSION( MAX_CHANNELS ) :: chanid
    REAL( AIRS_SRF_freq_type ),      DIMENSION( MAX_CHANNELS ) :: freq
    REAL( AIRS_SRF_width_type ),     DIMENSION( MAX_CHANNELS ) :: width



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE SRFVAL DIMENSION DATA --                    #
    !#--------------------------------------------------------------------------#

    error_status = get_sds_dimensions( HDF_fileID,                          &
                                       'srfval',                            &
                                       n_sds_dimensions = n_sds_dimensions, &
                                       sds_dimensions   = sds_dimensions,   &
                                       message_log      = message_log       )

    IF ( error_status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error obtaining dimension information for the SRFVAL SDS.', &
                            error_status, &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF

    ! -- Assign a short name for the number of channels
    n = sds_dimensions( 2 )



    !#--------------------------------------------------------------------------#
    !#                      -- ASSIGN THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_points ) ) THEN
      n_points = sds_dimensions( 1 )
    END IF
    
    IF ( PRESENT( n_channels ) ) THEN
      n_channels = n
    END IF
    


    !#--------------------------------------------------------------------------#
    !#                         -- READ CHANNEL LIST --                          #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( channel_list ) ) THEN

      hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'chanid' ), &
                                  (/ 0, 0 /), &  ! Start
                                  (/ 1, 1 /), &  ! Stride
                                  (/ 1, n /), &  ! Edges
                                  chanid( 1:n ) )

      IF ( hdf_error_status == FAIL ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error reading CHANNEL_LIST data from '//&
                              TRIM( HDF_fileNAME ), &
                              error_status, &
                              message_log = message_log )
        hdf_error_status = SFEND( HDF_fileID )
        RETURN
      END IF

      ! -- Save ShortInt in DefaultInt return.
      channel_list( 1:n ) = chanid( 1:n )

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ CHANNEL FREQUENCIES --                       #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( channel_frequency ) ) THEN

      hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'freq' ), &
                                  (/ 0, 0 /), &  ! Start
                                  (/ 1, 1 /), &  ! Stride
                                  (/ 1, n /), &  ! Edges
                                  freq( 1:n ) )

      IF ( hdf_error_status == FAIL ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error reading CHANNEL_FREQUENCY data from '//&
                              TRIM( HDF_fileNAME ), &
                              error_status, &
                              message_log = message_log )
        hdf_error_status = SFEND( HDF_fileID )
        RETURN
      END IF

      ! -- Save DoubleReal in (fp_kind)Real return.
      channel_frequency( 1:n ) = REAL( freq( 1:n ), fp_kind )

    END IF



    !#--------------------------------------------------------------------------#
    !#              -- READ CHANNEL Full-Width-at-Half-Maximums --              #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( channel_fwhm ) ) THEN

      hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'width' ), &
                                  (/ 0, 0 /), &  ! Start
                                  (/ 1, 1 /), &  ! Stride
                                  (/ 1, n /), &  ! Edges
                                  width( 1:n ) )

      IF ( hdf_error_status == FAIL ) THEN
        error_status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error reading CHANNEL_FWHM data from '//&
                              TRIM( HDF_fileNAME ), &
                              error_status, &
                              message_log = message_log )
        hdf_error_status = SFEND( HDF_fileID )
        RETURN
      END IF

      ! -- Save SingleReal in (fp_kind)Real return.
      channel_fwhm( 1:n ) = REAL( width( 1:n ), fp_kind )

    END IF

  END FUNCTION Inquire_AIRS_SRF_HDF



                             
  FUNCTION Read_AIRS_SRF_HDF( HDF_fileNAME, &  ! Input
                              HDF_fileID,   &  ! Input
                              channel,      &  ! Input

                              AIRS_SRF,     &  ! Output

                              message_log ) &  ! Error messaging

                            RESULT( error_status )


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: HDF_fileNAME
    INTEGER,                  INTENT( IN )  :: HDF_fileID
    INTEGER,                  INTENT( IN )  :: channel

    ! -- Output
    TYPE( AIRS_SRF_type ),    INTENT( OUT ) :: AIRS_SRF

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: message_log


    ! ------
    ! Result
    ! ------

    INTEGER :: error_status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AIRS_SRF_HDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: hdf_error_status

    INTEGER :: n_points
    INTEGER :: n_channels
    INTEGER, DIMENSION( MAX_CHANNELS ) :: channel_list
    INTEGER :: channel_index

    REAL( AIRS_SRF_freq_type  ), DIMENSION( 1 ) :: freq
    REAL( AIRS_SRF_width_type ), DIMENSION( 1 ) :: width




    !#--------------------------------------------------------------------------#
    !#                 -- GET THE DIMENSIONS AND CHANNEL LIST --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Read the dimensions
    ! -------------------

    error_status = Inquire_AIRS_SRF_HDF( HDF_fileNAME, &
                                         HDF_fileID,   &
                                         n_points    = n_points,   &
                                         n_channels  = n_channels, &
                                         message_log = message_log )

    IF ( error_status /= SUCCESS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'Error inquiring '//TRIM( HDF_fileNAME )//&
                            ' for the N_POINTS and N_CHANNELS dimensions.', &
                            error_status,    &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF

      
    ! ---------------------
    ! Read the CHANNEL_LIST
    ! ---------------------

    error_status = Inquire_AIRS_SRF_HDF( HDF_fileNAME, &
                                         HDF_fileID,   &
                                         channel_list = channel_list( 1:n_channels ), &
                                         message_log  = message_log )

    IF ( error_status /= SUCCESS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'Error inquiring '//TRIM( HDF_fileNAME )//&
                            ' for CHANNEL_LIST data.', &
                            error_status,    &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CHECK THAT INPUT CHANNEL IS VALID --                 #
    !#--------------------------------------------------------------------------#

    channel_index = get_channel_index( channel_list(1:n_channels), channel )

    IF ( channel_list( channel_index ) /= channel ) THEN
      error_status = FAILURE
      WRITE( message, '( "Requested channel, ", i5, ", not listed in ", a )' ) &
                      channel, TRIM( HDF_fileNAME )
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- ALLOCATE THE AIRS_SRF DATA STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    error_status = Allocate_AIRS_SRF( n_points, &
                                      AIRS_SRF, &
                                      message_log = message_log )

    IF ( error_status /= SUCCESS ) THEN
      WRITE( message, '( "Error allocating AIRS_SRF data structure for channel, ", i5, "." )' ) &
                      channel
      CALL display_message( ROUTINE_NAME,    &
                            TRIM( message ), &
                            error_status,    &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- READ THE SRF DATA --                          #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The central frequency
    ! ---------------------

    hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'freq' ), &
                                (/ 0, channel_index - 1 /), &  ! Start
                                (/ 1, 1 /), &                  ! Stride
                                (/ 1, 1 /), &                  ! Edges
                                freq )

    IF ( hdf_error_status == FAIL ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error reading AIRS SRF central frequency for channel ", i4, "." )' ) &
                      channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF

    AIRS_SRF%central_frequency = freq( 1 )


    ! -----------------
    ! The SRF halfwidth
    ! -----------------

    hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'width' ), &
                                (/ 0, channel_index - 1 /), &  ! Start
                                (/ 1, 1 /), &                  ! Stride
                                (/ 1, 1 /), &                  ! Edges
                                width )

    IF ( hdf_error_status == FAIL ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error reading AIRS SRF FWHM for channel ", i4, "." )' ) &
                      channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF

    AIRS_SRF%fwhm = width( 1 )


    ! ---------------------
    ! The SRF response data
    ! ---------------------

    hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'srfval' ), &
                                (/ 0, channel_index - 1 /), &  ! Start
                                (/ 1, 1 /), &                  ! Stride
                                (/ n_points, 1 /), &           ! Edges
                                AIRS_SRF%response )

    IF ( hdf_error_status == FAIL ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error reading SRF response data for channel ", i4, "." )' ) &
                     channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF


    ! ----------------------
    ! The SRF frequency data
    ! ----------------------

    hdf_error_status = SFRDATA( get_sds_id( HDF_fileID, 'fwgrid' ), &
                                (/ 0, 0 /), &         ! Start
                                (/ 1, 1 /), &         ! Stride
                                (/ n_points, 1 /), &  ! Edges
                                AIRS_SRF%frequency )

    IF ( hdf_error_status == FAIL ) THEN
      error_status = FAILURE
      WRITE( message, '( "Error reading SRF frequency data for channel ", i4, "." )' ) &
                     channel
      CALL display_message( ROUTINE_NAME, &
                            message, &
                            error_status, &
                            message_log = message_log )
      hdf_error_status = SFEND( HDF_fileID )
      RETURN
    END IF


    ! -- Apply transformation to get "true" frequency
    AIRS_SRF%frequency = ( AIRS_SRF%frequency * AIRS_SRF%fwhm ) + &
                         REAL( AIRS_SRF%central_frequency, Single )


    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE OTHER AIRS_SRF STRUCTURE FIELDS --             #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%channel         = channel
    AIRS_SRF%begin_frequency = REAL( MINVAL( AIRS_SRF%frequency ), fp_kind )
    AIRS_SRF%end_frequency   = REAL( MAXVAL( AIRS_SRF%frequency ), fp_kind )

  END FUNCTION Read_AIRS_SRF_HDF

END MODULE AIRS_SRF_HDF_Reader


