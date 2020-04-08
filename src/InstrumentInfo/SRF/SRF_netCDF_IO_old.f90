!------------------------------------------------------------------------------
!M+
! NAME:
!       SRF_netCDF_IO
!
! PURPOSE:
!       Module containing routines to read and write netCDF format
!       SRF data files.
!       
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SRF_Reader
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       SRF_Define:            Module defining the SRF data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    Message_Handler module
!
!       netcdf:                Module supplied with the Fortran 90 version 
!                              of the netCDF libraries (at least v3.5.0).
!                              See http://www.unidata.ucar.edu/packages/netcdf
!
!       netCDF_Utility:        Module containing utility routines for
!                              netCDF file access.
!                              USEs: NETCDF_DIMENSION_UTILITY module
!                                    NETCDF_ATTRIBUTE_UTILITY module
!                                    NETCDF_VARIABLE_UTILITY module
!                                    
!
! CONTAINS:
!       Create_SRF_netCDF:   Function to create a netCDF SRF data file for
!                            writing.
!
!       Inquire_SRF_netCDF:  Function to inquire a netCDF SRF format file
!                            to obtain the number of channels and the
!                            channel list.
!
!       Write_SRF_netCDF:    Function to write SRF data to a netCDF format
!                            SRF file.
!
!       Read_SRF_netCDF:     Function to read a selected channels' SRF data
!                            from a netCDF SRF format file.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001 Paul van Delst
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

MODULE SRF_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE SRF_Define

  USE netcdf
  USE netCDF_Utility,  Open_SRF_netCDF =>  Open_netCDF, &
                      Close_SRF_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Create_SRF_netCDF
  PUBLIC :: Write_SRF_netCDF
  PUBLIC :: Inquire_SRF_netCDF
  PUBLIC :: Read_SRF_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Invalid flag
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Numeric constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER ::  ONE = 1.0_fp_kind

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME       = 'comment' 

  ! -- Static dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_DIMNAME = 'n_channels'

  ! -- Static variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_VARNAME   = 'NCEP_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_VARNAME = 'WMO_Satellite_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_VARNAME    = 'WMO_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_VARNAME     = 'channel_list'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_VARNAME  = 'begin_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_VARNAME    = 'end_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_VARNAME   = 'integrated_srf'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_VARNAME    = 'summation_srf'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_LONGNAME   = &
'ID used at NOAA/NCEP/EMC to identify a satellite/sensor (-1 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_LONGNAME = &
'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_LONGNAME    = &
'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_LONGNAME     = &
'List of sensor channel numbers associated with the SRF data'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_LONGNAME  = &
'Begin frequencies of SRF response data'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_LONGNAME    = &
'End frequencies of SRF response data'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_LONGNAME  = &
'Integrated spectral response using Simpsons rule'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_LONGNAME    = &
'Integrated spectral response by summation: = SUM( response ) * df'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_UNITS        = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_LIST_UNITS         = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_UNITS            = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: RESPONSE_UNITS             = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: INTEGRATED_SRF_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: SUMMATION_SRF_UNITS        = 'N/A'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER,         PRIVATE, PARAMETER :: NCEP_SENSOR_ID_FILLVALUE    = -1
  INTEGER,         PRIVATE, PARAMETER :: WMO_SATELLITE_ID_FILLVALUE  = -1
  INTEGER,         PRIVATE, PARAMETER :: WMO_SENSOR_ID_FILLVALUE     = -1
  INTEGER,         PRIVATE, PARAMETER :: CHANNEL_LIST_FILLVALUE      = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FREQUENCY_FILLVALUE         = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: RESPONSE_FILLVALUE          = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: INTEGRATED_SRF_FILLVALUE    = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SUMMATION_SRF_FILLVALUE     = -1.0_fp_kind

  ! -- Variable netCDF datatypes
  INTEGER,        PRIVATE, PARAMETER :: NCEP_SENSOR_ID_TYPE   = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: WMO_SATELLITE_ID_TYPE = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: WMO_SENSOR_ID_TYPE    = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: CHANNEL_LIST_TYPE     = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: FREQUENCY_TYPE        = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: RESPONSE_TYPE         = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: INTEGRATED_SRF_TYPE   = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: SUMMATION_SRF_TYPE    = NF90_DOUBLE


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Create_Variable_Names
!
! PURPOSE:
!       Subroutine to create channel-based dimension and variable names
!       for use in accessing netCDF format SRF datafiles.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Create_Variable_Names( Channel,                         &  ! Input
!                                   Channel_Name   = Channel_Name,   &  ! Optional Output
!                                   Dimension_Name = Dimension_Name, &  ! Optional Output
!                                   Variable_Name  = Variable_Name   )  ! Optional Output
!
! INPUT ARGUMENTS:
!       Channel:          The channel number for which the names are required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Channel_Name:     Character string containing the channel number.
!                         For example, if the input channel number is 124,
!                         the channel name is '124'.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Dimension_Name:   Character string containing the channel-based
!                         SRF data dimension name for the requested channel.
!                         For example, if the input channel number is 124,
!                         the dimension name is 'channel_124_n_points'
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Variable_Name:    Character string containing the channel-based
!                         SRF response variable name for the requested channel.
!                         For example, if the input channel number is 124,
!                         the variable name is 'channel_124_response'
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! CALLS:
!       None.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       - Input channel number must be 0 < Channel < 100000. If an invalid
!         channel number is input, the output values are:
!           Channel_Name:   'XXXXX'
!           Dimension_Name: 'channel_XXXXX_n_points'
!           Response_Name:  'channel_XXXXX_response'
!       - If the output strings are not long enough to hold their results,
!         they are truncated.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  SUBROUTINE Create_Variable_Names( Channel,        &  ! Input
                                    Channel_Name,   &  ! Optional Output
                                    Dimension_Name, &  ! Optional Output
                                    Variable_Name   )  ! Optional Output

    INTEGER,                  INTENT( IN  ) :: Channel
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Channel_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Dimension_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Variable_Name

    INTEGER,         PARAMETER :: MAX_CHAR = 5
    CHARACTER( * ),  PARAMETER :: MAX_FMT  = '(i5)'
    REAL( fp_kind ), PARAMETER :: TEN = 10.0_fp_kind

    INTEGER :: Max_Channel
    CHARACTER( 256 ) :: C_String
    CHARACTER( 256 ) :: D_String
    CHARACTER( 256 ) :: V_String

    ! -- Determine the maximum allowed channel + 1
    Max_Channel = INT( TEN**MAX_CHAR )

    ! -- Fill the channel string. Output is 'XXXXX' if
    ! -- an invalid channel is supplied.
    IF ( Channel > 0 .AND. Channel < Max_Channel ) THEN
      WRITE( C_String, FMT = MAX_FMT ) Channel
    ELSE
      C_String = REPEAT( 'X', MAX_CHAR )
    END IF

    C_String     = ADJUSTL( C_String )
    IF ( PRESENT( Channel_Name ) ) Channel_Name = C_String

    ! -- Create the dimension name
    D_String = 'channel_'//TRIM( C_String )//'_n_points'
    IF ( PRESENT( Dimension_Name ) ) Dimension_Name = D_String

    ! -- Create the variable name
    V_String = 'channel_'//TRIM( C_String )//'_response'
    IF ( PRESENT( Variable_Name ) ) Variable_Name = V_String

  END SUBROUTINE Create_Variable_Names





!------------------------------------------------------------------------------
!
! NAME:
!       Write_SRF_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF SRF data file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_GAtts( NC_Filename,                   &  ! Input
!                                       NC_FileID,                     &  ! Input
!                                       Title         = Title,         &  ! Optional input
!                                       History       = History,       &  ! Optional input
!                                       Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                       Platform_Name = Platform_Name, &  ! Optional input
!                                       Comment       = Comment,       &  ! Optional input
!                                       Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SRF format data file to write to.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SRF file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful
!                            == FAILURE an error occurred writing the supplied
!                                       global attributes.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       NF90_PUT_ATT:       Function to write attribute data to a netCDF 
!                           data file.
!                           SOURCE: netCDF library
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_GAtts( NC_Filename,   &  ! Input
                            NC_FileID,     &  ! Input
                            Title,         &  ! Optional input
                            History,       &  ! Optional input
                            Sensor_Name,   &  ! Optional input
                            Platform_Name, &  ! Optional input
                            Comment,       &  ! Optional input
                            Message_Log )  &  ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN ) :: NC_Filename
    INTEGER,                  INTENT( IN ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_GAtts'

    ! -- "Internal" global attributes
    CHARACTER( * ), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER( * ), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    CHARACTER(  8 ) :: cdate
    CHARACTER( 10 ) :: ctime
    CHARACTER(  5 ) :: czone



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#              -- WRITE THE "INTERNAL" GLOBAL ATTRIBUTES --                #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Software ID
    ! -----------

    NF90_Status = NF90_PUT_ATT( NC_FileID,   &
                                NF90_GLOBAL, &
                                WRITE_MODULE_HISTORY_GATTNAME,    &
                                MODULE_RCS_ID )


    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WRITE_MODULE_HISTORY_GATTNAME//&
                            ' attribute to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -------------
    ! Creation date
    ! -------------

    CALL DATE_AND_TIME( cdate, ctime, czone )

    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                CREATION_DATE_AND_TIME_GATTNAME, &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CREATION_DATE_AND_TIME_GATTNAME//&
                            ' attribute to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEFINE THE USER ACCESSIBLE GLOBAL ATTRIBUTES --            #
    !#--------------------------------------------------------------------------#

    ! -----
    ! Title
    ! -----

    IF ( PRESENT( Title ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TITLE_GATTNAME, &
                                  TRIM( Title ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TITLE_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! History
    ! -------

    IF ( PRESENT( History ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  HISTORY_GATTNAME, &
                                  TRIM( History ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//HISTORY_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! Sensor name
    ! -----------

    IF ( PRESENT( Sensor_Name ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  SENSOR_NAME_GATTNAME, &
                                  TRIM( Sensor_Name ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//SENSOR_NAME_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------------
    ! Platform name
    ! -------------

    IF ( PRESENT( Platform_Name ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID,    &
                                  NF90_GLOBAL,   &
                                  PLATFORM_NAME_GATTNAME, &
                                  TRIM( Platform_Name ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//PLATFORM_NAME_GATTNAME//' attribute to '// &
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! Comment
    ! -------

    IF ( PRESENT( Comment ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  COMMENT_GATTNAME, &
                                  TRIM( Comment ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//COMMENT_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_SRF_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Read_SRF_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF SRF data file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_GAtts( NC_Filename,                   &  ! Input
!                                      NC_FileID,                     &  ! Input
!                                      Title         = Title,         &  ! Optional output
!                                      History       = History,       &  ! Optional output
!                                      Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                      Platform_Name = Platform_Name, &  ! Optional output
!                                      Comment       = Comment,       &  ! Optional output
!                                      Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SRF format data file to read.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF SRF file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF SRF
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SRF file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful
!                            == FAILURE an error occurred reading the requested
!                                       global attributes.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       NF90_GET_ATT:       Function to read attribute data from a netCDF 
!                           data file.
!                           SOURCE: netCDF library
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_GAtts( NC_Filename,   &  ! Input
                           NC_FileID,     &  ! Input
                           Title,         &  ! Optional output
                           History,       &  ! Optional output
                           Sensor_Name,   &  ! Optional output
                           Platform_Name, &  ! Optional output
                           Comment,       &  ! Optional output
                           Message_Log )  &  ! Error messaging
                         RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    INTEGER,                  INTENT( IN )  :: NC_FileID

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE GLOBAL ATTRIBUTES --                     #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The TITLE
    ! ---------

    IF ( PRESENT( Title ) ) THEN

      Title = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TITLE_GATTNAME, &
                                  Title )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Title )

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      History = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  HISTORY_GATTNAME, &
                                  History )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( History )

    END IF


    ! ---------------
    ! The SENSOR_NAME
    ! ---------------

    IF ( PRESENT( Sensor_Name ) ) THEN

      Sensor_Name = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  SENSOR_NAME_GATTNAME, &
                                  Sensor_Name )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//SENSOR_NAME_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Sensor_Name )

    END IF


    ! -----------------
    ! The PLATFORM_NAME
    ! -----------------

    IF ( PRESENT( Platform_Name ) ) THEN

      Platform_Name = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  PLATFORM_NAME_GATTNAME, &
                                  Platform_Name )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//PLATFORM_NAME_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Platform_Name )

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Comment = ' '

      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  COMMENT_GATTNAME, &
                                  Comment )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Comment )

    END IF

  END FUNCTION Read_SRF_GAtts





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!------------------------------------------------------------------------------
!S+
! NAME:
!       Create_SRF_netCDF
!
! PURPOSE:
!       Function to create a netCDF SRF data file for writing.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_SRF_netCDF( NC_Filename,                         &  ! Input
!                                         Channel_List,                        &  ! Input
!                                         NCEP_Sensor_ID   = NCEP_Sensor_ID,   &  ! Optional Input
!                                         WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional Input
!                                         WMO_Sensor_ID    = WMO_Sensor_ID,    &  ! Optional Input
!                                         Title            = Title,            &  ! Optional input
!                                         History          = History,          &  ! Optional input
!                                         Sensor_Name      = Sensor_Name,      &  ! Optional input
!                                         Platform_Name    = Platform_Name,    &  ! Optional input
!                                         Comment          = Comment,          &  ! Optional input
!                                         RCS_Id           = RCS_Id,           &  ! Revision control
!                                         Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF SRF format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Channel_List:       The list of Channel numbers to be written
!                           to the SRF file. The N_CHANNELS
!                           dimension is derived from this array.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       NCEP_Sensor_ID:     An "in-house" value used at NOAA/NCEP/EMC 
!                           to identify a satellite/sensor combination.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code for identifying satellite
!                           platforms. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Satellite ID is from Common Code
!                           table C-5, or code table 0 01 007 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code for identifying a satelite
!                           sensor. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Sensor ID is from Common Code
!                           table C-8, or code table 0 02 019 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SRF file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:        Character string written into the SENSOR_NAME
!                           global attribute field of the netCDF SRF
!                           file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:      Character string written into the PLATFORM_NAME
!                           global attribute field of the netCDF SRF
!                           file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the SRF netCDF file creation was successful
!                              == FAILURE an unrecoverable error occurred 
!                              == WARNING - an error occurred writing any of the
!                                           supplied global attributes.
!                                         - an error occurred closing the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       NF90_CREATE:        Function to create a netCDF data file and
!                           place it in DEFINE mode.
!                           SOURCE: netCDF library
!
!       NF90_DEF_DIM:       Function to define a dimension in a netCDF
!                           data file.
!                           SOURCE: netCDF library
!
!       NF90_PUT_ATT:       Function to write attribute data to a netCDF 
!                           data file.
!                           SOURCE: netCDF library
!
!       NF90_DEF_VAR:       Function to define a variable in a netCDF
!                           data file.
!                           SOURCE: netCDF library
!
!       NF90_PUT_VAR:       Function to write variable data to a netCDF 
!                           data file.
!                           SOURCE: netCDF library
!
!       NF90_ENDDEF:        Function to take a netCDF file out of DEFINE
!                           mode and put it in DATA mode.
!                           SOURCE: netCDF library
!
!       NF90_REDEF:         Function to put a netCDF file in DEFINE mode.
!                           SOURCE: netCDF library
!
!       NF90_CLOSE:         Function to close a netCDF file.
!                           SOURCE: netCDF library
!
!       Display_Message:    Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Create_SRF_netCDF( NC_Filename,       &  ! Input
                              Channel_List,      &  ! Input
                              NCEP_Sensor_ID,    &  ! Optional Input
                              WMO_Satellite_ID,  &  ! Optional Input
                              WMO_Sensor_ID,     &  ! Optional Input
                              Title,             &  ! Optional input
                              History,           &  ! Optional input
                              Sensor_Name,       &  ! Optional input
                              Platform_Name,     &  ! Optional input
                              Comment,           &  ! Optional input
                              RCS_Id,            &  ! Revision control
                              Message_Log )      &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                  INTENT( IN )  :: NC_Filename
    INTEGER,         DIMENSION( : ), INTENT( IN )  :: Channel_List

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN )  :: NCEP_Sensor_ID
    INTEGER,               OPTIONAL, INTENT( IN )  :: WMO_Satellite_ID
    INTEGER,               OPTIONAL, INTENT( IN )  :: WMO_Sensor_ID
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Comment

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),        OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status

    INTEGER :: n_Channels, Channel_DimID

    INTEGER :: VarID


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                  -- SET RCS_ID ARGUMENT IF SUPPLIED --                   #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    n_Channels = SIZE( Channel_List )

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'CHANNEL_LIST array must be non-zero size.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( ANY( Channel_List < 1 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid CHANNEL_LIST value found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- DEFINE THE STATIC DIMENSIONS --                   #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The number of channels
    ! ----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                CHANNEL_DIMNAME, &
                                n_Channels, &
                                Channel_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//CHANNEL_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE GLOBAL ATTRIBUTES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_SRF_GAtts( NC_Filename, &  ! Input
                                    NC_fileID,   &  ! Input
                                    Title         = Title, &
                                    History       = History, &
                                    Sensor_Name   = Sensor_Name, &
                                    Platform_Name = Platform_Name, &
                                    Comment       = Comment, &
                                    Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- DEFINE THE VARIABLES --                       #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! NCEP Sensor ID
    ! --------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                NCEP_SENSOR_ID_VARNAME, &
                                NCEP_SENSOR_ID_TYPE, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//NCEP_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    NCEP_SENSOR_ID_LONGNAME, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    NCEP_SENSOR_ID_UNITS, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    NCEP_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = NCEP_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! WMO satellite ID
    ! ----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SATELLITE_ID_VARNAME, &
                                WMO_SATELLITE_ID_TYPE, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SATELLITE_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    WMO_SATELLITE_ID_LONGNAME, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    WMO_SATELLITE_ID_UNITS, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SATELLITE_ID_FILLVALUE, &
                                    Variable_Name = WMO_SATELLITE_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! WMO Sensor ID
    ! -------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WMO_SENSOR_ID_VARNAME, &
                                WMO_SENSOR_ID_TYPE, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WMO_SENSOR_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    WMO_SENSOR_ID_LONGNAME, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    WMO_SENSOR_ID_UNITS, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    WMO_SENSOR_ID_FILLVALUE, &
                                    Variable_Name = WMO_SENSOR_ID_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! Channel list
    ! ------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                CHANNEL_LIST_VARNAME, &
                                CHANNEL_LIST_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//CHANNEL_LIST_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    CHANNEL_LIST_LONGNAME, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    CHANNEL_LIST_UNITS, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    CHANNEL_LIST_FILLVALUE, &
                                    Variable_Name = CHANNEL_LIST_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Begin frequency
    ! ---------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BEGIN_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    BEGIN_FREQUENCY_LONGNAME, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    FREQUENCY_UNITS, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    FREQUENCY_FILLVALUE, &
                                    Variable_Name = BEGIN_FREQUENCY_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! End frequency
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                END_FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//END_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    END_FREQUENCY_LONGNAME, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    FREQUENCY_UNITS, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    FREQUENCY_FILLVALUE, &
                                    Variable_Name = END_FREQUENCY_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Integrated SRF
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                INTEGRATED_SRF_VARNAME, &
                                INTEGRATED_SRF_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    INTEGRATED_SRF_LONGNAME, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    INTEGRATED_SRF_UNITS, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    INTEGRATED_SRF_FILLVALUE, &
                                    Variable_Name = INTEGRATED_SRF_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INTEGRATED_SRF_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! Summation SRF
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                SUMMATION_SRF_VARNAME, &
                                SUMMATION_SRF_TYPE, &
                                dimids = Channel_DimID, &
                                varID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SUMMATION_SRF_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    SUMMATION_SRF_LONGNAME, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    SUMMATION_SRF_UNITS, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    SUMMATION_SRF_FILLVALUE, &
                                    Variable_Name = SUMMATION_SRF_VARNAME )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SUMMATION_SRF_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE INPUT DATA --                        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Take netCDF file out of define mode
    ! -----------------------------------

    NF90_Status = NF90_ENDDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error taking file '//TRIM( NC_Filename )// &
                            ' out of define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------
    ! The sensor/satellite IDs
    ! ------------------------

    ! -- NCEP Sensor ID
    IF ( PRESENT( NCEP_Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          NCEP_SENSOR_ID_VARNAME, &
                                          NCEP_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//NCEP_SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF

    ! -- WMO Satellite ID
    IF ( PRESENT( WMO_Satellite_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SATELLITE_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF

    ! -- WMO Sensor ID
    IF ( PRESENT( WMO_Sensor_ID ) ) THEN
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//WMO_SENSOR_ID_VARNAME//&
                              ' to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! Write the channel list
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Channel_List )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_LIST_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Create_SRF_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_SRF_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF SRF format file to obtain the number of
!       channels, channel list, sensor IDs, and global attributes.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SRF_netCDF( NC_Filename,                         &  ! Input
!                                          n_Channels       = n_Channels,       &  ! Optional output
!                                          n_Points         = n_Points,         &  ! Optional output
!                                          Channel_List     = Channel_List,     &  ! Optional output
!                                          Begin_Frequency  = Begin_Frequency,  &  ! Optional output
!                                          End_Frequency    = End_Frequency,    &  ! Optional output
!                                          NCEP_Sensor_ID   = NCEP_Sensor_ID,   &  ! Optional output
!                                          WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
!                                          WMO_Sensor_ID    = WMO_Sensor_ID,    &  ! Optional output
!                                          Title            = Title,            &  ! Optional output
!                                          History          = History,          &  ! Optional output
!                                          Sensor_Name      = Sensor_Name,      &  ! Optional output
!                                          Platform_Name    = Platform_Name,    &  ! Optional output
!                                          Comment          = Comment,          &  ! Optional output
!                                          RCS_Id           = RCS_Id,           &  ! Revision control
!                                          Message_Log      = Message_Log )        ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         SRF netCDF format data file to inquire.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Channels:         The number of channels dimension of the
!                           SRF data data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Points:           The number of spectral points used to represent the
!                           SRF for each channel.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Channel_List:       The list of channel numbers present in the netCDF
!                           SRF file. The list may not necessarily
!                           start at 1 or contain contiguous values.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Begin_Frequency:    The list of the begin frequency limits for
!                           each channel's SRF.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       End_Frequency:      The list of the end frequency limits for
!                           each channel's SRF.
!                           UNITS:      Inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-1, n_Channels
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       NCEP_Sensor_ID:     An "in-house" value used at NOAA/NCEP/EMC 
!                           to identify a satellite/sensor combination.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code for identifying satellite
!                           platforms. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Satellite ID is from Common Code
!                           table C-5, or code table 0 01 007 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code for identifying a satelite
!                           sensor. Taken from the WMO common
!                           code tables at:
!                             http://www.wmo.ch/web/ddbs/Code-tables.html
!                           The Sensor ID is from Common Code
!                           table C-8, or code table 0 02 019 in BUFR
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Sensor_Name:        Character string written into the SENSOR_NAME global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Platform_Name:      Character string written into the PLATFORM_NAME global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF SRF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE - an error occurred opening the netCDF file, or
!                                         - an error occurred reading any of the
!                                           requested dimension or variable data.
!                              == WARNING - an error occurred reading any of the
!                                           requested global attributes, or
!                                         - an error occurred closing the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_SRF_netCDF:         Function to open an SRF netCDF
!                                format data file.
!                                SOURCE: NETCDF_UTILITY module
!
!       Get_netCDF_Dimension:    Function to return a dimension value from
!                                a netCDF file given the dimension name.
!                                SOURCE: NETCDF_UTILITY module
!                                
!       Get_netCDF_Variable:     Function to return a variable from a
!                                netCDF file given the variable name.
!                                SOURCE: NETCDF_UTILITY module
!
!       Close_SRF_netCDF:        Function to close a SRF netCDF
!                                format data file.
!                                SOURCE: NETCDF_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: Message_Handler module
!
!       NF90_CLOSE:              Function to close a netCDF file.
!                                SOURCE: netCDF library
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       To successfully return any of the channel dimensioned arrays, the
!       dummy arguments must have the same size as the dataset in the netCDF
!       file. Thus, two calls to this routine are required. First, the
!       n_Channels dimension should be read and used either to allocate the
!       required data array of the correct size, or to subset an existing
!       array in the call.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_SRF_netCDF( NC_Filename,      &  ! Input
                               n_Channels,       &  ! Optional output
                               n_Points,         &  ! Optional output
                               Channel_List,     &  ! Optional output
                               Begin_Frequency,  &  ! Optional output
                               End_Frequency,    &  ! Optional output
                               NCEP_Sensor_ID,   &  ! Optional output
                               WMO_Satellite_ID, &  ! Optional output
                               WMO_Sensor_ID,    &  ! Optional output
                               Title,            &  ! Optional output
                               History,          &  ! Optional output
                               Sensor_Name,      &  ! Optional output
                               Platform_Name,    &  ! Optional output
                               Comment,          &  ! Optional output
                               RCS_Id,           &  ! Revision control
                               Message_Log )     &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: n_Channels
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: n_Points
    INTEGER,         OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Channel_List
    REAL( fp_kind),  OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: Begin_Frequency
    REAL( fp_kind),  OPTIONAL, DIMENSION( : ), INTENT( OUT ) :: End_Frequency
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: NCEP_Sensor_ID   
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: WMO_Satellite_ID 
    INTEGER,         OPTIONAL,                 INTENT( OUT ) :: WMO_Sensor_ID
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Title
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: History
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Platform_Name
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,                 INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ),  OPTIONAL,                 INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_SRF_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status

    CHARACTER( 256 ) :: DimName
    INTEGER :: i, l

    INTEGER, DIMENSION(:), ALLOCATABLE :: Local_Channel_List



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF SRF data file '//TRIM( NC_Filename )
      GOTO 1000
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GET THE CHANNEL DIMENSION --                    #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! The number of Channels
    ! ----------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         CHANNEL_DIMNAME, &
                                         l, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//&
                ' dimension from '//TRIM( NC_Filename )
      GOTO 2000
    END IF


    ! ------------------------------
    ! Set the dimension return value
    ! ------------------------------

    IF ( PRESENT( n_Channels ) ) n_Channels = l



    !#--------------------------------------------------------------------------#
    !#                      -- ALLOCATE THE LOCAL ARRAYS --                     #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Local_Channel_List( l ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating local data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE CHANNEL LIST --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Get the channel list data
    ! -------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        CHANNEL_LIST_VARNAME, &
                                        Local_Channel_List )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//CHANNEL_LIST_VARNAME//&
                ' data from '//TRIM( NC_Filename )
      GOTO 3000
    END IF


    ! -------------------------
    ! Set the data return value
    ! -------------------------

    IF ( PRESENT( Channel_List ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Channel_List ) < l ) THEN
        Error_Status = FAILURE
        Message = 'Channel_List array too small to hold data.'
        GOTO 3000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Channel_List = CHANNEL_LIST_FILLVALUE

      ! -- Save the data
      Channel_List(1:l) = Local_Channel_List

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE n_Points DIMENSION LIST --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Points ) ) THEN


      ! -----------------------------
      ! Check the dummy argument size
      ! -----------------------------

      IF ( SIZE( n_Points ) < l ) THEN
        Error_Status = FAILURE
        Message = 'n_Points array too small to hold data.'
        GOTO 3000
      END IF


      ! ------------------------------------------------
      ! Initialise the entire array (in case it's bigger
      ! than the number of elements in the file)
      ! ------------------------------------------------

      n_Points = 0


      ! ----------------------
      ! Get the dimension data
      ! ----------------------

      ! -- Loop over channels
      DO i = 1, l

        ! -- Create the dimension name for this channel
        CALL Create_Variable_Names( Local_Channel_List(i), &
                                    Dimension_Name = DimName )

        ! -- Retrieve the dimension value
        Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                             TRIM( DimName ),  &
                                             n_Points(i) )

        IF ( Error_Status /= SUCCESS ) THEN
          Message = 'Error reading '//TRIM( DimName )//&
                    ' dimension from '//TRIM( NC_Filename )
          GOTO 3000
        END IF

      END DO

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- DEALLOCATE THE LOCAL ARRAYS --                    #
    !#--------------------------------------------------------------------------#

    DEALLOCATE( Local_Channel_List, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error deallocating local data arrays.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GET THE FREQUENCY LIMITS --                     #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The begin frequency
    ! -------------------

    IF ( PRESENT( Begin_Frequency ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( Begin_Frequency ) < l ) THEN
        Error_Status = FAILURE
        Message = 'Begin_Frequency array too small to hold data.'
        GOTO 2000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      Begin_Frequency = FREQUENCY_FILLVALUE


      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          BEGIN_FREQUENCY_VARNAME, &
                                          Begin_Frequency(1:l) )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//BEGIN_FREQUENCY_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! -----------------
    ! The end frequency
    ! -----------------

    IF ( PRESENT( End_Frequency ) ) THEN

      ! -- Check the dummy argument size
      IF ( SIZE( End_Frequency ) < l ) THEN
        Error_Status = FAILURE
        Message = 'End_Frequency array too small to hold data.'
        GOTO 2000
      END IF

      ! -- Initialise the entire array (in case it's bigger
      ! -- than the number of elements in the file)
      End_Frequency = FREQUENCY_FILLVALUE

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          END_FREQUENCY_VARNAME, &
                                          End_Frequency(1:l) )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//END_FREQUENCY_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE SENSOR IDs --                          #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! The NCEP Sensor ID
    ! ------------------

    IF ( PRESENT( NCEP_Sensor_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          NCEP_SENSOR_ID_VARNAME, &
                                          NCEP_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//NCEP_SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! --------------------
    ! The WMO satellite ID
    ! --------------------

    IF ( PRESENT( WMO_Satellite_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WMO_SATELLITE_ID_VARNAME, &
                                          WMO_Satellite_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SATELLITE_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF


    ! -----------------
    ! The WMO Sensor ID
    ! -----------------

    IF ( PRESENT( WMO_Sensor_ID ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WMO_SENSOR_ID_VARNAME, &
                                          WMO_Sensor_ID )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//WMO_SENSOR_ID_VARNAME//&
                  ' data from '//TRIM( NC_Filename )
        GOTO 2000
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_SRF_GAtts( TRIM( NC_Filename ), &
                                   NC_fileID, &
                                   Title         = Title, &
                                   History       = History, &
                                   Sensor_Name   = Sensor_Name, &
                                   Platform_Name = Platform_Name, &
                                   Comment       = Comment, &
                                   Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      Message = 'Error reading global attribute from '//TRIM( NC_Filename )
      GOTO 2000
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      Message = 'Error closing netCDF SRF data file '//TRIM( NC_Filename )
      GOTO 1000
    END IF

    RETURN



    !#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#
    !#                      =- CLEAN-UP AFTER ERRORS -=                         #
    !#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-#

    3000 CONTINUE
    DEALLOCATE( Local_Channel_List, &
                STAT = Allocate_Status )
    2000 CONTINUE
    NF90_Status = NF90_CLOSE( NC_fileID )
    1000 CONTINUE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Inquire_SRF_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SRF_netCDF
!
! PURPOSE:
!       Function to write data in an SRF structure to a netCDF format SRF file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SRF_netCDF( NC_Filename,              &  ! Input
!                                        SRF,                      &  ! Input
!                                        RCS_Id      = RCS_Id,     &  !  Revision control
!                                        Message_Log = Message_Log )  !  Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format SRF data file to write to.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SRF:             Structure containing the SRF data to write to file.
!                        UNITS:      N/A
!                        TYPE:       SRF_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error
!                        status. The error codes are defined in the
!                        Message_Handler module.
!                        If == SUCCESS the netCDF data write was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Inquire_SRF_netCDF:    Function to inquire a netCDF format
!                              SRF file to obtain information
!                              about the data dimensions and attributes.
!
!       Open_SRF_netCDF:       Function to open a SRF netCDF
!                              format file.
!
!       Close_SRF_netCDF:      Function to close a SRF netCDF
!                              format file.
!
!       Put_netCDF_Variable:   Function to write a netCDF file
!                              variable by name.
!                              SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Put_netCDF_Attribute:  Function to write a netCDF file variable
!                              attribute by name.
!                              SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_DEF_DIM:          Function to define a dimension in a netCDF
!                              data file.
!                              SOURCE: netCDF library
!
!       NF90_DEF_VAR:          Function to define a variable in a netCDF
!                              data file.
!                              SOURCE: netCDF library
!
!       NF90_ENDDEF:           Function to take a netCDF file out of DEFINE
!                              mode and put it in DATA mode.
!                              SOURCE: netCDF library
!
!       NF90_REDEF:            Function to put a netCDF file in DEFINE mode.
!                              SOURCE: netCDF library
!
!       NF90_CLOSE:            Function to close a netCDF file.
!                              SOURCE: netCDF library
!
!       Display_Message:       Subroutine to output messages
!                              SOURCE: Message_Handler module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_SRF_netCDF( NC_Filename,  &  ! Input
                             SRF,          &  ! Input
                             RCS_Id,       &  ! Revision control
                             Message_Log ) &  ! Error messaging
                           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    TYPE( SRF_type ),         INTENT( IN )  :: SRF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SRF_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: NF90_Status
    INTEGER :: Allocate_Status
    INTEGER :: Status1, Status2, Status3
    INTEGER :: Close_Status

    INTEGER :: n_Channels
    INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_List
    INTEGER :: Channel_Index

    INTEGER :: NCEP_Sensor_ID
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID

    CHARACTER(  4 ) :: Channel_String
    CHARACTER( 25 ) :: DimNAME
    CHARACTER( 25 ) :: ResponseNAME

    INTEGER :: DimID
    INTEGER :: VarID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- GROSS SRF INPUT CHECK --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Check channel is valid
    ! ----------------------

    IF ( SRF%Channel < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF CHANNEL member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Check SRF array size is valid
    ! -----------------------------

    IF ( SRF%n_Points < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SRF N_POINTS member is < 1.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CHECK THAT SRF CHANNEL IS VALID FOR THIS NETCDF FILE --        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------------
    ! Read the channel dimension and sensor ID values
    ! -----------------------------------------------

    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       n_Channels = n_Channels, &
                                       NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                       WMO_Satellite_ID = WMO_Satellite_ID, &
                                       WMO_Sensor_ID    = WMO_Sensor_ID, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining SRF dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------
    ! Check the sensor ID values first
    ! --------------------------------

    IF ( SRF%NCEP_Sensor_ID   /= NCEP_Sensor_ID   .OR. &
         SRF%WMO_Satellite_ID /= WMO_Satellite_ID .OR. &
         SRF%WMO_Sensor_ID    /= WMO_Sensor_ID         ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'netCDF file '//TRIM( NC_Filename )//&
                            ' sensor IDs different from SRF structure values.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
         

    ! --------------------
    ! Get the channel list
    ! --------------------

    ! -- Allocate the array
    ALLOCATE( Channel_List( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating channel list array. STAT = ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Read it
    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       Channel_List = Channel_List, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_LIST_VARNAME//' data from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      DEALLOCATE( Channel_List )
      RETURN
    END IF


    ! ---------------------------
    ! Check the SRF channel value
    ! ---------------------------

    ! -- Get the channel index
    Channel_Index = MINLOC( ABS( Channel_List - SRF%Channel ), DIM = 1 )
    IF ( ( Channel_List( Channel_Index ) - SRF%Channel ) /= 0 ) Channel_Index = -1

    ! -- Deallocate the channel list array
    DEALLOCATE( Channel_List )

    ! -- Is the channel valid?
    IF ( Channel_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "SRF channel ", i4, " not in channel list for ", a, "." )' ) &
                      SRF%Channel, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READWRITE' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SRF data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- PUT NETCDF FILE INTO DEFINE MODE --                  #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_REDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error putting file '//TRIM( NC_Filename )// &
                            ' into define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- DEFINE THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Create the channel SRF dimension and variable names
    ! ---------------------------------------------------

    CALL Create_Variable_Names( SRF%Channel, & 
                                Channel_String, &
                                DimNAME, &
                                ResponseNAME )


    ! ----------------------------------------------
    ! Define the N_POINTS dimension for this channel
    ! ----------------------------------------------

    NF90_Status = NF90_DEF_DIM( NC_fileID, &
                                TRIM( DimNAME ), &
                                SRF%n_Points, &
                                DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM( DimNAME )//&
                            ' dimension in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF
    

    ! ---------------------------------------------
    ! Define the RESPONSE variable for this channel
    ! ---------------------------------------------

    NF90_Status = NF90_DEF_VAR( NC_fileID, &
                                TRIM( ResponseNAME ), &
                                RESPONSE_TYPE, &
                                dimids = DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM( ResponseNAME )//&
                            ' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Write some attributes
    Status1 = Put_netCDF_Attribute( NC_FileID, &
                                    LONGNAME_ATTNAME, &
                                    'Channel '//TRIM( Channel_String )//' normalised response.', &
                                    Variable_Name = TRIM( ResponseNAME ) )

    Status2 = Put_netCDF_Attribute( NC_FileID, &
                                    UNITS_ATTNAME, &
                                    RESPONSE_UNITS, &
                                    Variable_Name = TRIM( ResponseNAME ) )

    Status3 = Put_netCDF_Attribute( NC_FileID, &
                                    FILLVALUE_ATTNAME, &
                                    RESPONSE_FILLVALUE, &
                                    Variable_Name = TRIM( ResponseNAME ) )

    IF ( Status1 /= SUCCESS .OR. &
         Status2 /= SUCCESS .OR. &
         Status3 /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( ResponseNAME )//&
                            ' variable attributes to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- PUT THE FILE INTO DATA MODE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_ENDDEF( NC_fileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error placing '//TRIM( NC_Filename )//&
                            ' in DATA mode for channel '//TRIM( Channel_String )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- WRITE THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Write the begin frequency
    ! -------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        SRF%Begin_Frequency, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//BEGIN_FREQUENCY_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -----------------------
    ! Write the end frequency
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        END_FREQUENCY_VARNAME, &
                                        SRF%End_Frequency, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//END_FREQUENCY_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! Write the integrated SRF value
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        INTEGRATED_SRF_VARNAME, &
                                        SRF%Integrated_SRF, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//INTEGRATED_SRF_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! --------------------------
    ! Write the summed SRF value
    ! --------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID,   &
                                        SUMMATION_SRF_VARNAME, &
                                        SRF%Summation_SRF, &
                                        START = (/ Channel_Index /) )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing channel '//TRIM( Channel_String )//&
                            ' '//SUMMATION_SRF_VARNAME//' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------
    ! Write the SRF response data
    ! ---------------------------

    Error_Status = Put_netCDF_Variable( NC_fileID, &
                                        TRIM( ResponseNAME ), &
                                        SRF%Response )

    IF ( Error_Status/= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( ResponseNAME )//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_SRF_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SRF_netCDF
!
! PURPOSE:
!       Function to read a selected channel's SRF data from a netCDF SRF
!       format file.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SRF_netCDF( NC_Filename,              &  ! Input  
!                                       Channel,                  &  ! Input  
!                                       SRF,                      &  ! Output 
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the netCDF SRF
!                     format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Channel:      Channel number for which the SRF data is required.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SRF:          Structure containing the requested SRF data.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the netCDF data read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Inquire_SRF_netCDF:      Function to inquire a netCDF SRF format
!                                file to obtain the number of channels
!                                and the channel list.
!
!       Allocate_SRF:            Function to allocate the pointer members
!                                of the SRF structure.
!                                SOURCE: SRF_DEFINE module
!
!       NF90_CLOSE:              Function to close a netCDF file.
!                                SOURCE: netCDF library
!
!       Get_netCDF_Dimension:    Function to return a dimension value from
!                                a netCDF file given the dimension name.
!                                SOURCE: NETCDF_UTILITY module
!                                
!       Get_netCDF_Variable:     Function to return a variable from a
!                                netCDF file given the variable name.
!                                SOURCE: NETCDF_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_SRF_netCDF( NC_Filename,  &   ! Input
                            Channel,      &   ! Input
                            SRF,          &   ! Output
                            RCS_Id,       &   ! Revision control
                            Message_Log ) &   ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: NC_Filename
    INTEGER,                  INTENT( IN )     :: Channel

    ! -- Output
    TYPE( SRF_type ),         INTENT( IN OUT ) :: SRF

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id
   
    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SRF_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NC_FileID

    INTEGER :: Allocate_Status
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: n_Channels
    INTEGER :: NCEP_Sensor_ID
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID
    INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_List
    INTEGER :: Channel_Index

    INTEGER :: n_Points
    CHARACTER(  4 ) :: Channel_String
    CHARACTER( 25 ) :: DimNAME
    CHARACTER( 25 ) :: ResponseNAME




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

    ! -----------------------------------------------
    ! Read the channel dimension and sensor Id values
    ! -----------------------------------------------

    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       n_Channels = n_Channels, &
                                       NCEP_Sensor_ID   = NCEP_Sensor_ID,   &
                                       WMO_Satellite_ID = WMO_Satellite_ID, &
                                       WMO_Sensor_ID    = WMO_Sensor_ID,    &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining channel dimension from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! Get the channel list
    ! --------------------

    ! -- Allocate the array
    ALLOCATE( Channel_List( n_Channels ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating channel list array. STAT = ", i5 )' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Read it
    Error_Status = Inquire_SRF_netCDF( NC_Filename, &
                                       Channel_List = Channel_List, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_LIST_VARNAME//' data from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      DEALLOCATE( Channel_List )
      RETURN
    END IF


    ! ---------------------------
    ! Check the SRF channel value
    ! ---------------------------

    ! -- Get the channel index
    Channel_Index = MINLOC( ABS( Channel_List - Channel ), DIM = 1 )
    IF ( ( Channel_List( Channel_Index ) - Channel ) /= 0 ) Channel_Index = -1

    ! -- Deallocate the channel list array
    DEALLOCATE( Channel_List )

    ! -- Is the channel valid?
    IF ( Channel_Index < 1 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "SRF channel ", i4, " not in channel list for ", a, "." )' ) &
                      Channel, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SRF_netCDF( TRIM( NC_Filename ), &
                                    NC_FileID, &
                                    Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SRF data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE SRF DATA --                         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------------
    ! Create the channel SRF dimension and variable names
    ! ---------------------------------------------------

    CALL Create_Variable_Names( Channel, & 
                                Channel_String, &
                                DimNAME, &
                                ResponseNAME )


    ! --------------------------
    ! Get the n_Points dimension
    ! --------------------------

    Error_Status = Get_netCDF_Dimension( NC_fileID, &
                                         TRIM( DimNAME ), &
                                         n_Points, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//TRIM( DimNAME )//' dimension.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------
    ! Allocate the output SRF structure
    ! ---------------------------------

    Error_Status = Allocate_SRF( n_Points, &
                                 SRF, &
                                 Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error allocating SRF data array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ---------------------------------------------------
    ! Get the SRF response data for the requested channel
    ! ---------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        TRIM( ResponseNAME ), &
                                        SRF%Response, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( ResponseNAME )//' data.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------
    ! Read the frequency limits
    ! -------------------------

    ! -- Begin frequency
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        SRF%Begin_Frequency, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BEGIN_FREQUENCY_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- End frequency
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        END_FREQUENCY_VARNAME, &
                                        SRF%End_Frequency, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//END_FREQUENCY_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ------------------------------
    ! Read the integrated SRF values
    ! ------------------------------

    ! -- The integrated value
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        INTEGRATED_SRF_VARNAME, &
                                        SRF%Integrated_SRF, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INTEGRATED_SRF_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -- The summation value
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        SUMMATION_SRF_VARNAME, &
                                        SRF%Summation_SRF, &
                                        START = (/ Channel_Index /), &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INTEGRATED_SRF_VARNAME//' data for channel '//&
                            TRIM( Channel_String ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! -------------------------------------------------------------
    ! All reads successful, so assign channel number and sensor Ids
    ! -------------------------------------------------------------

    SRF%Channel = Channel

    SRF%NCEP_Sensor_ID   = NCEP_Sensor_ID
    SRF%WMO_Satellite_ID = WMO_Satellite_ID
    SRF%WMO_Sensor_ID    = WMO_Sensor_ID



    !#--------------------------------------------------------------------------#
    !#                       -- CLOSE THE netCDF FILE --                        #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SRF_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SRF data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE SRF FREQUENCY GRID --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Frequency_SRF( SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing frequency grid for channel '//TRIM( Channel_String )//&
                            ' SRF from '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Read_SRF_netCDF

END MODULE SRF_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/08/15 20:51:04 $
!
! $Revision: 774 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_netCDF_IO.f90,v $
! Revision 3.9  2006/08/15 20:51:04  wd20pd
! Additional replacement of Error_Handler with Message_Handler.
!
! Revision 3.8  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 3.7  2005/08/11 17:35:28  paulv
! - Removed netCDF file closure statement in error processing for failure of
!   Frequency_SRF() call. The file is already closed.
!
! Revision 3.6  2004/08/31 20:53:49  paulv
! - Upgraded to Fortran95.
! - Removed Get_Channel_Index() function. Code now used inline since f95
!   allows the use of the DIM argument in the MINLOC intrinsic.
! - Improved the Create_Variable_Names() subroutine.
! - Added optional n_Points argument to Inquire_SRF_netCDF() function.
! - Changed error handling in Inquire_SRF_netCDF() function.
! - If an error occurs closing a netCDF file at the end of the Inquire(),
!   Read() or Write() functions, a warning *message* is issued, but the error
!   status is not set to WARNING.
! - Added structure association test to the Write_SRF_netCDF() function.
! - Changed INTENT of SRF structure in Read_SRF_netCDF() function from
!   OUT to IN OUT. Necessary to prevent memory leaks.
! - Updated header documentation.
!
! Revision 3.5  2004/06/25 17:15:57  paulv
! - Removed unused variables from type declarations.
! - Cosmetic changes.
!
! Revision 3.4  2003/11/19 15:26:28  paulv
! - Updated header documentation.
!
! Revision 3.3  2003/09/05 16:07:08  paulv
! - Added optional output arguments BEGIN_FREQUENCY and END_FREQUENCY to
!   the Inquire() function.
!
! Revision 3.2  2003/09/04 15:24:08  paulv
! - Removed the Open() and Close() netCDF file functions from the PUBLIC
!   list.
!
! Revision 3.1  2003/09/03 14:57:13  paulv
! - Corrected bug in Read() function where the sensor ID values were not
!   being read and placed into the output SRF structure.
!
! Revision 3.0  2003/08/29 18:02:30  paulv
! - New version. FileID values not returned as all open/close functionality
!   is now internal.
! - Standardisd the attribute/varaible naming.
!
! Revision 2.1  2003/03/21 22:40:09  paulv
! - Character string global attributes are now cleared before being read.
!
! Revision 2.0  2002/11/22 17:40:00  paulv
! - Added MODULE_RCS_ID parameter.
! - Removed the maximum number of channels parameter.
! - Removed the NC_SRF_dataID derived type definition.
! - Added SRF dimension and variable names as parameters.
! - Removed NC_dataID output argument from CREATE() and
!   NC_dataID input argument from WRITE() functions.
! - Altered the internals of the WRITE() function to inquire
!   the netCDF file to get the needed information. This adds
!   a little extra overhead (inquiring and allocating arrays
!   for the data reads) but eliminates the need to carry around
!   the NC_dataID structure after creation and for writing.
! - Upon exit from the CREATE() function, the netCDF file is now
!   in DATA mode. This allows file reads in any subsequent INQUIRE()
!   calls - which is not allowed if the file is in DEFINE mode.
! - Upon entry to the WRITE() function, the file must be in DATA
!   mode. Upon exit from the WRITE() function, the file is in
!   DATA mode. Previously, the requirement was for the file to
!   be in DEFINE mode.
!
! Revision 1.9  2002/06/05 19:10:55  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.8  2002/05/31 22:36:08  paulv
! - Added call to netCDF_Utility subroutine Remove_NULL_Characters() to replace
!   the null values in global attribute strings with spaces when they are
!   returned from the Inquire_SRF_netCDF() function.
!
! Revision 1.7  2002/05/31 21:49:00  paulv
! - Removed the netCDF open and close functions. These were not "data type"
!   specific so they were placed into the netCDF_Utility module and renamed
!   in the USE statement.
! - Added summation_SRF output.
!
! Revision 1.6  2002/05/20 20:00:30  paulv
! - USEing netCDF_Utility module rather than old "netcdf_utility" module.
! - Changed all instances of "get_ncdf_dimension" to "Get_netCDF_Dimension".
! - Changed all instances of "get_ncdf_Variable" to "Get_netCDF_Variable".
! - Removed comments and code for f95 syntax use of MINLOC in channel_index
!   determination.
! - Added BEGIN_FREQUENCY and END_FREQUENCY arguments to the Inquire function.
!
! Revision 1.5  2002/05/08 13:09:01  paulv
! - Added HISTORY optional argument to the CREATE and INQUIRE functions.
! - CREATE and INQUIRE functions now return a WARNING status if the global
!   attribute write/read fails and the netCDF file is *not* closed.
!   Previously, a FAILURE status was returned and the file was closed.
! - The SRF datatype is now used in the WRITE function. Previously all the
!   elements of the SRF structure were passed individually. D'oh.
!
! Revision 1.4  2002/05/07 14:22:34  paulv
! - Altered creation, write, and read functions to reflect the removal of
!   the FREQUENCY member of the SRF data structure. The frequency grid
!   is now defined solely by the begin and end frequencies and the
!   total number of points for the channel. The begin and end frequencies
!   were added to the WRITE_SRF_NETCDF() argument list and the frequency
!   array was removed.
! - Added the global attributes as optional arguments to the INQUIRE_SRF_NETCDF()
!   function.
! - Replaced call to PUT_NCDF_DEF_ATTS() with calls to NF90_PUT_ATT(). Too
!   many default attributes - only LONG_NAME and UNITS are now written.
!
! Revision 1.3  2002/05/03 19:17:31  paulv
! - Updated documentation.
!
! Revision 1.2  2002/05/03 18:49:43  paulv
! - Sync update.
!
!
!
!
!
