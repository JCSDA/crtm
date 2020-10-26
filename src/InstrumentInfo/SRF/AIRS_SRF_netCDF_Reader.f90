
MODULE AIRS_SRF_netCDF_Reader


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE AIRS_SRF_Define

  USE netcdf
  USE netCDF_Utility,  Open_AIRS_SRF_netCDF =>  Open_netCDF, &
                      Close_AIRS_SRF_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_AIRS_SRF_netCDF
  PUBLIC :: Read_AIRS_SRF_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! -- Invalid flag
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Maximum number of channels
  INTEGER, PUBLIC, PARAMETER :: MAX_AIRS_CHANNELS  = 2378

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: VERSION_GATTNAME = 'version' 

  ! -- Dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: NPTS_DIMNAME  = 'npts'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCHAN_DIMNAME = 'nchan'

  ! -- Variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANID_VARNAME = 'chanid'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQ_VARNAME   = 'freq'
  CHARACTER( * ), PRIVATE, PARAMETER :: FWGRID_VARNAME = 'fwgrid'
  CHARACTER( * ), PRIVATE, PARAMETER :: SRFVAL_VARNAME = 'srfval'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIDTH_VARNAME  = 'width'

  ! -- Fillvalues
  INTEGER,         PRIVATE, PARAMETER ::  INT_FILLVALUE = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: REAL_FILLVALUE = -1.0_fp_kind


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC MAXVAL, MINVAL, &
            PRESENT, &
            TRIM


CONTAINS



  FUNCTION Get_Channel_Index( Channel_List, Channel ) RESULT( Channel_Index )

    INTEGER, DIMENSION( : ), INTENT( IN ) :: Channel_List
    INTEGER,                 INTENT( IN ) :: Channel
    INTEGER :: Channel_Index

    INTEGER, DIMENSION( 1 ) :: Array_Index

    Array_Index = MINLOC( ABS( Channel_List - Channel ) )
    Channel_Index = Array_Index( 1 )

    IF ( ( Channel_List( Channel_Index ) - Channel ) /= 0 ) Channel_Index = -1

  END FUNCTION Get_Channel_Index






  FUNCTION Inquire_AIRS_SRF_netCDF( NC_fileNAME,       &  ! Input

                                    n_Points,          &  ! Optional output
                                    n_Channels,        &  ! Optional output
                                    Channel_List,      &  ! Optional output
                                    Channel_Frequency, &  ! Optional output
                                    Channel_FWHM,      &  ! Optional output
                                    Version,           &  ! Optional output
 
                                    RCS_Id,            &  ! Revision control
                                    Message_Log   )    &  ! Error messaging

                                  RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: NC_fileNAME

    ! -- Optional output
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_Points
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_Channels
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Channel_List
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Channel_Frequency
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Channel_FWHM
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Version

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------

    INTEGER :: Error_Status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_AIRS_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: npts
    INTEGER :: nchan



    !#--------------------------------------------------------------------------#
    !#                   -- DEFINE A SUCCESSFUL EXIT STATUS --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_AIRS_SRF_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error opening netCDF AIRS_SRF data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE DIMENSION DATA --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The N_POINTS dimension
    ! ----------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID,   &
                                         NPTS_DIMNAME, &
                                         npts,        &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error obtaining '//NPTS_DIMNAME//&
                            ' dimension from '//TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    IF ( PRESENT( n_Points ) ) THEN
      n_Points = npts
    END IF


    ! ------------------------
    ! The N_CHANNELS dimension
    ! ------------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID,    &
                                         NCHAN_DIMNAME, &
                                         nchan,        &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error obtaining '//NCHAN_DIMNAME//&
                            ' dimension from '//TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    IF ( PRESENT( n_Channels ) ) THEN
      n_Channels = nchan
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUIRED VARIABLE DATA --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The channel ID list
    ! -------------------

    IF ( PRESENT( Channel_List ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Channel_List ) < nchan ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'CHANNEL_LIST array too small to hold data.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Channel_List = INT_FILLVALUE

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID,     &
                                          CHANID_VARNAME, &
                                          Channel_List,  &
                                          START = (/ 1,     1 /), &
                                          COUNT = (/ 1, nchan /)  )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error reading '//CHANID_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF


    ! -------------------------------
    ! The channel central frequencies
    ! -------------------------------

    IF ( PRESENT( Channel_Frequency ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Channel_Frequency ) < nchan ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'CHANNEL_FREQUENCY array too small to hold data.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Channel_Frequency = REAL_FILLVALUE

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID,         &
                                          FREQ_VARNAME,       &
                                          Channel_Frequency, &
                                          START = (/ 1,     1 /), &
                                          COUNT = (/ 1, nchan /)  )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error reading '//FREQ_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF


    ! -----------------
    ! The channel FWHMs
    ! -----------------

    IF ( PRESENT( Channel_FWHM ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Channel_FWHM ) < nchan ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'CHANNEL_FWHM array too small to hold data.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Channel_FWHM = REAL_FILLVALUE

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID,    &
                                          WIDTH_VARNAME, &
                                          Channel_FWHM, &
                                          START = (/ 1,     1 /), &
                                          COUNT = (/ 1, nchan /)  )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL display_message( ROUTINE_NAME, &
                              'Error reading '//WIDTH_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! The VERSION
    ! -----------

    IF ( PRESENT( Version ) ) THEN

      Version = ' '

      NF90_Status = NF90_GET_ATT( NC_fileID, &
                                  NF90_GLOBAL, &
                                  VERSION_GATTNAME, &
                                  Version )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL display_message( ROUTINE_NAME, &
                              'Error reading '//VERSION_GATTNAME//' attribute from '//&
                              TRIM( NC_fileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Version )

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_AIRS_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL display_message( ROUTINE_NAME, &
                            'Error closing netCDF AIRS_SRF data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_AIRS_SRF_netCDF





                             
  FUNCTION Read_AIRS_SRF_netCDF( NC_fileNAME,  &  ! Input
                                 Channel,      &  ! Input

                                 AIRS_SRF,     &  ! Output

                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging

                               RESULT( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_fileNAME
    INTEGER,                  INTENT( IN )  :: Channel

    ! -- Output
    TYPE( AIRS_SRF_type ),    INTENT( OUT ) :: AIRS_SRF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id
   
    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------

    INTEGER :: Error_Status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AIRS_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: n_Points
    INTEGER :: n_Channels
    INTEGER, DIMENSION( MAX_AIRS_CHANNELS ) :: Channel_List
    INTEGER :: Channel_Index



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CHECK THAT SRF CHANNEL IS VALID FOR THIS NETCDF FILE --        #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Read the dimensions
    ! -------------------

    Error_Status = Inquire_AIRS_SRF_netCDF( TRIM( NC_fileNAME ), &

                                            n_Points    = n_Points,   &
                                            n_Channels  = n_Channels, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error obtaining dimensions from '//TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

      
    ! ---------------------
    ! Read the CHANNEL_LIST
    ! ---------------------

    Error_Status = Inquire_AIRS_SRF_netCDF( NC_fileNAME, &
                                            Channel_List = Channel_List( 1:n_Channels ), &
                                            Message_Log  = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME,    &
                            'Error inquiring '//TRIM( NC_fileNAME )//&
                            ' for CHANNEL_LIST data.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    ! ---------------------------
    ! Check the SRF channel value
    ! ---------------------------

    ! -- Get the channel index
    Channel_Index = Get_Channel_Index( Channel_List, &
                                       Channel )

    ! -- Is the channel valid?
    IF ( Channel_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "AIRS_SRF channel ", i4, " not in channel list for ", a, "." )' ) &
                      Channel, TRIM( NC_fileNAME )
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_AIRS_SRF_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( ROUTINE_NAME, &
                            'Error opening netCDF AIRS_SRF data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- ALLOCATE THE AIRS_SRF DATA STRUCTURE --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_AIRS_SRF( n_Points, &
                                      AIRS_SRF, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( message, '( "Error allocating AIRS_SRF data structure for channel, ", i4, "." )' ) &
                      Channel
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- READ THE SRF DATA --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! The SRF central frequency
    ! -------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        FREQ_VARNAME, &
                                        AIRS_SRF%Central_Frequency, &
                                        START = (/ 1, Channel_Index /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( message, '( "Error reading ", a, " data for channel ", i4 )' ) &
                      FREQ_VARNAME, Channel
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------
    ! The SRF fullwidth
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        WIDTH_VARNAME, &
                                        AIRS_SRF%FWHM, &
                                        START = (/ 1, Channel_Index /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( message, '( "Error reading ", a, " data for channel ", i4 )' ) &
                      WIDTH_VARNAME, Channel
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------
    ! The SRF response data
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        SRFVAL_VARNAME, &
                                        AIRS_SRF%Response, &
                                        START = (/ 1, Channel_Index /), &
                                        COUNT = (/ n_Points, 1 /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( message, '( "Error reading ", a, " data for channel ", i4 )' ) &
                      SRFVAL_VARNAME, Channel
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------
    ! The SRF frequency data
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        FWGRID_VARNAME, &
                                        AIRS_SRF%Frequency, &
                                        START = (/ 1, 1 /), &
                                        COUNT = (/ n_Points, 1 /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( message, '( "Error reading ", a, " data for channel ", i4 )' ) &
                      FWGRID_VARNAME, Channel
      CALL display_message( ROUTINE_NAME, &
                            TRIM( message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -- Apply transformation to get "true" frequency
    AIRS_SRF%Frequency = ( AIRS_SRF%Frequency * AIRS_SRF%FWHM ) + &
                         REAL( AIRS_SRF%Central_Frequency, AIRS_SRF_fwgrid_type )



    !#--------------------------------------------------------------------------#
    !#             -- ASSIGN THE OTHER AIRS_SRF STRUCTURE FIELDS --             #
    !#--------------------------------------------------------------------------#

    AIRS_SRF%Channel         = Channel
    AIRS_SRF%Begin_Frequency = REAL( MINVAL( AIRS_SRF%Frequency ), fp_kind )
    AIRS_SRF%End_Frequency   = REAL( MAXVAL( AIRS_SRF%Frequency ), fp_kind )



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_AIRS_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL display_message( ROUTINE_NAME, &
                            'Error closing netCDF AIRS_SRF data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_AIRS_SRF_netCDF

END MODULE AIRS_SRF_netCDF_Reader


