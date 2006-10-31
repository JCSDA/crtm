!------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_netCDF_IO
!
! PURPOSE:
!       Module containing routine to read and write netCDF format files of
!       LBLRTM output data.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LBLRTM_netCDF_IO
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       Message_Handler:    Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       netcdf:             Module supplied with the Fortran 90 version 
!                           of the netCDF libraries (at least v3.5.0).
!                           See http://www.unidata.ucar.edu/packages/netcdf
!
!       netCDF_Utility:     Module containing utility routines for
!                           netCDF file access.
!                           USEs: NETCDF_DIMENSION_UTILITY module
!                                 NETCDF_VARIABLE_UTILITY module
!
! CONTAINS:
!       Create_LBLRTM_netCDF:   Function to create a netCDF format LBLRTM
!                               data file for writing.
!
!       Inquire_LBLRTM_netCDF:  Function to inquire a netCDF format 
!                               LBLRTM file to obtain information
!                               about the data dimensions and attributes.
!
!       Write_LBLRTM_netCDF:    Function to write a layer of data to a
!                               netCDF format LBLRTM file.
!
!       Read_LBLRTM_netCDF:     Function to read a selected layer of data
!                               from a netCDF format LBLRTM file.
!
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
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
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


MODULE LBLRTM_netCDF_IO


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE netcdf
  USE netCDF_Utility,  Open_LBLRTM_netCDF =>  Open_netCDF, &
                      Close_LBLRTM_netCDF => Close_netCDF


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Create_LBLRTM_netCDF
  PUBLIC :: Inquire_LBLRTM_netCDF
  PUBLIC :: Write_LBLRTM_netCDF
  PUBLIC :: Read_LBLRTM_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: LBLRTM_netCDF_IO.f90,v 2.4 2006/07/26 21:43:58 wd20pd Exp $'

   ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME   = 'title'
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME = 'history'
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME = 'comment'
  CHARACTER( * ), PRIVATE, PARAMETER :: ID_TAG_GATTNAME  = 'id_tag' 

  ! -- Dimension names
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_DIMNAME = 'n_frequencies'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DIMNAME     = 'n_layers'

  ! -- Variable names
  CHARACTER( * ), PRIVATE, PARAMETER :: DIRECTION_VARNAME          = 'direction'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_VARNAME    = 'begin_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_VARNAME      = 'end_frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_VARNAME = 'frequency_interval'
  CHARACTER( * ), PRIVATE, PARAMETER :: TRANSMITTANCE_VARNAME      = 'transmittance'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: DIRECTION_LONGNAME          = &
    'Direction flag for transmittance calculation. Downwelling=0, Upwelling=1'
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_LONGNAME    = &
    'Begin frequency of the transmittance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_LONGNAME      = &
    'End frequency of the transmittance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_LONGNAME = &
    'Frequency interval of the transmittance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: TRANSMITTANCE_LONGNAME      = &
    'Layer -> boundary (TOA or SFC) spectral transmittance'


  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_UNITS    = &
    'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_UNITS      = &
    'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_UNITS = &
    'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: TRANSMITTANCE_UNITS      = &
    'None'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER,         PRIVATE, PARAMETER :: DIRECTION_FILLVALUE          = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_FILLVALUE    = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: END_FREQUENCY_FILLVALUE      = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_FILLVALUE = -1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TRANSMITTANCE_FILLVALUE      = -1.0_fp_kind

  ! -- Variable types
  INTEGER, PRIVATE, PARAMETER :: DIRECTION_TYPE          = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: BEGIN_FREQUENCY_TYPE    = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: END_FREQUENCY_TYPE      = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_TYPE = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: TRANSMITTANCE_TYPE      = NF90_DOUBLE


  ! -- Direction flags for transmittance calculation
  INTEGER, PRIVATE, PARAMETER :: DOWNWELLING = 0
  INTEGER, PRIVATE, PARAMETER :: UPWELLING   = 1


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_LBLRTM_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF format LBLRTM
!       data file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_GAtts( NC_Filename,              &  ! Input
!                                          NC_FileID,                &  ! Input
!                                          ID_Tag  = ID_Tag,         &  ! Optional input
!                                          Title   = Title,          &  ! Optional input
!                                          History = History,        &  ! Optional input
!                                          Comment = Comment,        &  ! Optional input
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF LBLRTM format data file to write to.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:    NetCDF file ID number.
!                     function.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a short tag used to identify the
!                     profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a succinct description of what
!                     is in the netCDF datafile.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which
!                     any messages will be logged. If not specified,
!                     or if an error occurs opening the log file, the
!                     default action is to output messages to standard
!                     output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute write was successful
!                        == WARNING an error occurred writing the global attributes
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Put_netCDF_Attribute: Function to write attribute data to a netCDF 
!                             data file.
!                             SOURCE: NETCDF_ATTRIBUTE module
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_GAtts( NC_Filename,   &  ! Input
                               NC_FileID,     &  ! Input
                               ID_Tag,        &  ! Optional input
                               Title,         &  ! Optional input
                               History,       &  ! Optional input
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
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_GAtts'

    ! -- "Internal" global attributes
    CHARACTER( * ), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER( * ), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Write_Status
    INTEGER :: NF90_Status
    CHARACTER(  8 ) :: cdate
    CHARACTER( 10 ) :: ctime
    CHARACTER(  5 ) :: czone



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#              -- WRITE THE "INTERNAL" GLOBAL ATTRIBUTES --                #
    !#--------------------------------------------------------------------------#

    ! -----------
    ! Software ID
    ! -----------

    Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                         WRITE_MODULE_HISTORY_GATTNAME, &
                                         MODULE_RCS_ID, &
                                         Message_Log = Message_Log )

    IF ( Write_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WRITE_MODULE_HISTORY_GATTNAME//&
                            ' attribute to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! -------------
    ! Creation date
    ! -------------

    CALL DATE_AND_TIME( cdate, ctime, czone )

    Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                         CREATION_DATE_AND_TIME_GATTNAME, &
                                         cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                         ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                         czone//'UTC', &
                                         Message_Log = Message_Log )

    IF ( Write_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CREATION_DATE_AND_TIME_GATTNAME//&
                            ' attribute to '//TRIM( NC_Filename ), &
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

      Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           TRIM( Title ), &
                                           Message_Log = Message_Log )

      IF ( Write_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TITLE_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! History
    ! -------

    IF ( PRESENT( History ) ) THEN

      Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           TRIM( History ), &
                                           Message_Log = Message_Log )

      IF ( Write_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//HISTORY_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -------
    ! Comment
    ! -------

    IF ( PRESENT( Comment ) ) THEN

      Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           TRIM( Comment ), &
                                           Message_Log = Message_Log )

      IF ( Write_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//COMMENT_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! ------
    ! ID Tag
    ! ------

    IF ( PRESENT( ID_Tag ) ) THEN

      Write_Status = Put_netCDF_Attribute( NC_FileID, &
                                           ID_TAG_GATTNAME, &
                                           TRIM( ID_Tag ), &
                                           Message_Log = Message_Log )

      IF ( Write_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//ID_TAG_GATTNAME//&
                              ' attribute to '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_LBLRTM_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Read_LBLRTM_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF format LBLRTM
!       data file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_GAtts( NC_Filename,              &  ! Input
!                                         NC_FileID,                &  ! Input
!                                         ID_Tag  = ID_Tag,         &  ! Optional output
!                                         Title   = Title,          &  ! Optional output
!                                         History = History,        &  ! Optional output
!                                         Comment = Comment,        &  ! Optional output
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:  Character string specifying the name of the
!                     netCDF LBLRTM format data file to read from.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:    NetCDF file ID number.
!                     function.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which
!                     any messages will be logged. If not specified,
!                     or if an error occurs opening the log file, the
!                     default action is to output messages to standard
!                     output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       ID_Tag:       Character string written into the ID_TAG global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a short tag used to identify the
!                     Profile set.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Title:        Character string written into the TITLE global
!                     attribute field of the netCDF LBLRTM file.
!                     Should contain a succinct description of what
!                     is in the netCDF datafile.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:      Character string written into the HISTORY global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:      Character string written into the COMMENT global
!                     attribute field of the netCDF LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful
!                        == WARNING an error occurred reading the global attributes
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Get_netCDF_Attribute: Function to read attribute data from a netCDF 
!                             data file.
!                             SOURCE: NETCDF_ATTRIBUTE module
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: Message_Handler module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_GAtts( NC_Filename,   &  ! Input
                              NC_FileID,     &  ! Input
                              ID_Tag,        &  ! Optional output
                              Title,         &  ! Optional output
                              History,       &  ! Optional output
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
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: Read_Status
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

      Read_Status = Get_netCDF_Attribute( NC_FileID, &
                                          TITLE_GATTNAME, &
                                          Title, &
                                          Message_Log = Message_Log )

      IF ( Read_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
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

      Read_Status = Get_netCDF_Attribute( NC_FileID, &
                                          HISTORY_GATTNAME, &
                                          History, &
                                          Message_Log = Message_Log )

      IF ( Read_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( History )

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Comment = ' '

      Read_Status = Get_netCDF_Attribute( NC_FileID, &
                                          COMMENT_GATTNAME, &
                                          Comment, &
                                          Message_Log = Message_Log )

      IF ( Read_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Comment )

    END IF


    ! ----------
    ! The ID_TAG
    ! ----------

    IF ( PRESENT( ID_Tag ) ) THEN

      ID_Tag = ' '

      Read_Status = Get_netCDF_Attribute( NC_FileID, &
                                          ID_TAG_GATTNAME, &
                                          ID_Tag, &
                                          Message_Log = Message_Log )

      IF ( Read_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//ID_TAG_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( ID_Tag )

    END IF

  END FUNCTION Read_LBLRTM_GAtts





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!S+
! NAME:
!       Create_LBLRTM_netCDF
!
! PURPOSE:
!       Function to create a netCDF LBLRTM data file for writing.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_LBLRTM_netCDF( NC_Filename,              &  ! Input
!                                            n_Frequencies,            &  ! Input
!                                            n_Layers,                 &  ! Input
!                                            Direction,                &  ! Input
!                                            Begin_Frequency,          &  ! Input
!                                            End_Frequency,            &  ! Input
!                                            Frequency_Interval,       &  ! Input
!                                            ID_Tag      = ID_Tag,     &  ! Optional input
!                                            Title       = Title,      &  ! Optional input
!                                            History     = History,    &  ! Optional input
!                                            Comment     = Comment,    &  ! Optional input
!                                            RCS_Id      = RCS_Id,     &  ! Revision control
!                                            Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF format LBLRTM data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Frequencies:      The number of spectral points in each layer of data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Layers:           The number of atmospheric layers in the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Direction:          The direction flag for the spectral data calculation.
!                           If = 0 Downwelling
!                              = 1 Upwelling
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Begin_Frequency:    The begin frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       End_Frequency:      The end frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Frequency_Interval: The frequency spacing of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!
!
! OPTIONAL INPUT ARGUMENTS:
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:              Character string written into the TITLE global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:            Character string written into the HISTORY global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
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
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the LBLRTM netCDF file creation was successful
!                              == FAILURE an unrecoverable error occured
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
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Create_LBLRTM_netCDF( NC_Filename,        &  ! Input
                                 n_Frequencies,      &  ! Input
                                 n_Layers,           &  ! Input
                                 Direction,          &  ! Input
                                 Begin_Frequency,    &  ! Input
                                 End_Frequency,      &  ! Input
                                 Frequency_Interval, &  ! Input
                                 ID_Tag,             &  ! Optional input
                                 Title,              &  ! Optional input
                                 History,            &  ! Optional input
                                 Comment,            &  ! Optional input
                                 RCS_Id,             &  ! Revision control
                                 Message_Log )       &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                INTENT( IN )  :: NC_Filename
    INTEGER,                       INTENT( IN )  :: n_Frequencies
    INTEGER,                       INTENT( IN )  :: n_Layers
    INTEGER,                       INTENT( IN )  :: Direction
    REAL( fp_kind ),               INTENT( IN )  :: Begin_Frequency
    REAL( fp_kind ),               INTENT( IN )  :: End_Frequency
    REAL( fp_kind ),               INTENT( IN )  :: Frequency_Interval

    ! -- Optional input
    CHARACTER( * ),  OPTIONAL,     INTENT( IN )  :: ID_Tag
    CHARACTER( * ),  OPTIONAL,     INTENT( IN )  :: Title
    CHARACTER( * ),  OPTIONAL,     INTENT( IN )  :: History
    CHARACTER( * ),  OPTIONAL,     INTENT( IN )  :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,     INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,     INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_LBLRTM_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: WriteGatts_Status
    INTEGER :: Longname_Status
    INTEGER :: Units_Status
    INTEGER :: FillValue_Status
    INTEGER :: Close_Status
    INTEGER :: Frequency_DimID
    INTEGER :: Layer_DimID
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
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    IF ( n_Frequencies < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of frequencies (spectral points) must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of layers must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    IF ( Direction /= DOWNWELLING .AND. Direction /= UPWELLING ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid direction flag', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )

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
    !#                         -- DEFINE THE DIMENSIONS --                      #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! The number of spectral points
    ! -----------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FREQUENCY_DIMNAME, &
                                n_Frequencies, &
                                Frequency_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//FREQUENCY_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The number of layers
    ! --------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LAYER_DIMNAME, &
                                n_Layers, &
                                Layer_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//LAYER_DIMNAME//' dimension in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE GLOBAL ATTRIBUTES --                    #
    !#--------------------------------------------------------------------------#

    WriteGAtts_Status = Write_LBLRTM_GAtts( NC_Filename,              &
                                            NC_FileID,                &
                                            ID_Tag      = ID_Tag,     &
                                            Title       = Title,      &
                                            History     = History,    &
                                            Comment     = Comment,    &
                                            Message_Log = Message_Log )

    IF ( WriteGAtts_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- DEFINE THE VARIABLES --                         #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Direction flag
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DIRECTION_VARNAME, &
                                DIRECTION_TYPE, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DIRECTION_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             DIRECTION_LONGNAME, &
                                             Variable_Name = DIRECTION_VARNAME )

    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             DIRECTION_FILLVALUE, &
                                             Variable_Name = DIRECTION_VARNAME )

    IF ( Longname_Status  /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DIRECTION_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Begin frequency
    ! ---------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                BEGIN_FREQUENCY_TYPE, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BEGIN_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             BEGIN_FREQUENCY_LONGNAME, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )

    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             BEGIN_FREQUENCY_UNITS, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )

    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             BEGIN_FREQUENCY_FILLVALUE, &
                                             Variable_Name = BEGIN_FREQUENCY_VARNAME )

    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! End frequency
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                END_FREQUENCY_VARNAME, &
                                END_FREQUENCY_TYPE, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//END_FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             END_FREQUENCY_LONGNAME, &
                                             Variable_Name = END_FREQUENCY_VARNAME )

    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             END_FREQUENCY_UNITS, &
                                             Variable_Name = END_FREQUENCY_VARNAME )

    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             END_FREQUENCY_FILLVALUE, &
                                             Variable_Name = END_FREQUENCY_VARNAME )

    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! Frequency interval
    ! ------------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_INTERVAL_VARNAME, &
                                FREQUENCY_INTERVAL_TYPE, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_INTERVAL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             FREQUENCY_INTERVAL_LONGNAME, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )

    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             FREQUENCY_INTERVAL_UNITS, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )

    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             FREQUENCY_INTERVAL_FILLVALUE, &
                                             Variable_Name = FREQUENCY_INTERVAL_VARNAME )

    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! Transmittance
    ! -------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRANSMITTANCE_VARNAME, &
                                TRANSMITTANCE_TYPE, &
                                dimids = (/ Frequency_DimID, &
                                            Layer_DimID     /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRANSMITTANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             TRANSMITTANCE_LONGNAME, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )

    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             TRANSMITTANCE_UNITS, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )

    FillValue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             TRANSMITTANCE_FILLVALUE, &
                                             Variable_Name = TRANSMITTANCE_VARNAME )

    IF ( Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         FillValue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRANSMITTANCE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- TAKE THE FILE OUT OF DEFINE MODE --                   #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_ENDDEF( NC_FileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error taking file '//TRIM( NC_Filename )// &
                            ' out of define mode - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE FREQUENCY DATA --                       #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Direction flag
    ! --------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DIRECTION_VARNAME, &
                                        Direction )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DIRECTION_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Begin frequency
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Begin_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! End frequency
    ! -------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        End_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! Frequency interval
    ! ------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Frequency_Interval )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//&
                            ' to '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_LBLRTM_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Create_LBLRTM_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_LBLRTM_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF format LBLRTM file to obtain the 
!       dimensions and frequency information.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_LBLRTM_netCDF( NC_Filename,                             &  ! Input
!                                             n_Frequencies      = n_Frequencies,      &  ! Optional output
!                                             n_Layers           = n_Layers,           &  ! Optional output
!                                             Direction          = Direction,          &  ! Optional output
!                                             Begin_Frequency    = Begin_Frequency,    &  ! Optional output
!                                             End_Frequency      = End_Frequency,      &  ! Optional output
!                                             Frequency_Interval = Frequency_Interval, &  ! Optional output
!                                             ID_Tag             = ID_Tag,             &  ! Optional output
!                                             Title              = Title,              &  ! Optional output
!                                             History            = History,            &  ! Optional output
!                                             Comment            = Comment,            &  ! Optional output
!                                             RCS_Id             = RCS_Id,             &  ! Optional output
!                                             Message_Log        = Message_Log         ) ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of
!                           the netCDF format LBLRTM data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Frequencies:      The number of spectral points in each layer of data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Layers:           The number of atmospheric layers in the netCDF file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Direction:          The direction flag for the spectral data calculation.
!                           If = 0 Downwelling
!                              = 1 Upwelling
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Begin_Frequency:    The begin frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       End_Frequency:      The end frequency of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Frequency_Interval: The frequency spacing of the spectral data.
!                           UNITS:      inverse centimetres (cm^-1)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:              Character string written into the TITLE global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:            Character string written into the HISTORY global attribute
!                           field of the netCDF LBLRTM file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF LBLRTM file.
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
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the LBLRTM netCDF file inquiry was
!                                         successful
!                              == FAILURE - an error occurred opening the
!                                           netCDF file, or
!                                         - an error occurred reading any of
!                                           the requested dimension or variable
!                                           data.
!                                         - an error occurred reading any of the
!                                           requested global file attributes
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_LBLRTM_netCDF:      Function to open an LBLRTM netCDF
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
!       Close_LBLRTM_netCDF:     Function to close an LBLRTM netCDF
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
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_LBLRTM_netCDF( NC_Filename,        &  ! Input
                                  n_Frequencies,      &  ! Optional output
                                  n_Layers,           &  ! Optional output
                                  Direction,          &  ! Optional output
                                  Begin_Frequency,    &  ! Optional output
                                  End_Frequency,      &  ! Optional output
                                  Frequency_Interval, &  ! Optional output
                                  ID_Tag,             &  ! Optional output
                                  Title,              &  ! Optional output
                                  History,            &  ! Optional output
                                  Comment,            &  ! Optional output
                                  RCS_Id,             &  ! Revision control
                                  Message_Log )       &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Frequencies
    INTEGER,         OPTIONAL, INTENT( OUT ) :: n_Layers
    INTEGER,         OPTIONAL, INTENT( OUT ) :: Direction
    REAL( fp_kind ), OPTIONAL, INTENT( OUT ) :: Begin_Frequency
    REAL( fp_kind ), OPTIONAL, INTENT( OUT ) :: End_Frequency
    REAL( fp_kind ), OPTIONAL, INTENT( OUT ) :: Frequency_Interval
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: ID_Tag
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_LBLRTM_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: NF90_Status
    INTEGER :: ReadGAtts_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID



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

    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE DIMENSIONS --                          #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! The number of spectral points
    ! -----------------------------

    IF ( PRESENT( n_Frequencies ) ) THEN

      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           FREQUENCY_DIMNAME, &
                                           n_Frequencies, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//FREQUENCY_DIMNAME//&
                              ' dimension from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF


    ! --------------------------------
    ! The number of atmospheric layers
    ! --------------------------------

    IF ( PRESENT( n_Layers ) ) THEN

      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           LAYER_DIMNAME, &
                                           n_Layers, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//LAYER_DIMNAME//&
                              ' dimension from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE DIRECTION FLAG --                        #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Direction ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          DIRECTION_VARNAME, &
                                          Direction )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//DIRECTION_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE FREQUENCIES --                         #
    !#--------------------------------------------------------------------------#

    ! ---------------
    ! Begin frequency
    ! ---------------

    IF ( PRESENT( Begin_Frequency ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          BEGIN_FREQUENCY_VARNAME, &
                                          Begin_Frequency )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//BEGIN_FREQUENCY_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF


    ! -------------
    ! End frequency
    ! -------------

    IF ( PRESENT( End_Frequency ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          END_FREQUENCY_VARNAME, &
                                          End_Frequency )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//END_FREQUENCY_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF


    ! ------------------
    ! Frequency interval
    ! ------------------

    IF ( PRESENT( Frequency_Interval ) ) THEN

      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          FREQUENCY_INTERVAL_VARNAME, &
                                          Frequency_Interval )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//FREQUENCY_INTERVAL_VARNAME//&
                              ' data from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    ReadGAtts_Status = Read_LBLRTM_GAtts( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          ID_Tag  = ID_Tag, &
                                          Title   = Title, &
                                          History = History, &
                                          Comment = Comment, &
                                          Message_Log = Message_Log )

    IF ( ReadGAtts_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_LBLRTM_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_LBLRTM_netCDF




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_LBLRTM_netCDF
!
! PURPOSE:
!       Function to write LBLRTM spectral data to a netCDF format LBLRTM file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_netCDF( NC_Filename,                   &  ! Input
!                                           Layer,                         &  ! Input
!                                           Transmittance = Transmittance, &  ! Optional input
!                                           RCS_Id        = RCS_Id,        &  ! Revision control
!                                           Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the netCDF
!                       format LBLRTM data file to write to.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:      NetCDF file ID number returned from the
!                       Create_LBLRTM_netCDF() or Open_LBLRTM_netCDF()
!                       functions.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Layer:          Layer index value into which the input data is to be
!                       "slotted" in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Transmittance:  Transmittance data to write to the netCDF LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-1, n_Frequencies
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the LBLRTM netCDF file write was successful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       Inquire_LBLRTM_netCDF:  Function to inquire a netCDF format 
!                               LBLRTM file to obtain information
!                               about the data dimensions and attributes.
!
!       Open_LBLRTM_netCDF:     Function to open a netCDF format LBLRTM file.
!
!       Close_LBLRTM_netCDF:    Function to close a netCDF format LBLRTM file.
!
!       Put_netCDF_Variable:    Function to write a netCDF file variable
!                               variable by name.
!                               SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:             Function to close a netCDF file.
!                               SOURCE: netCDF library
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: Message_Handler module
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

  FUNCTION Write_LBLRTM_netCDF( NC_Filename,   &  ! Input
                                Layer,         &  ! Input
                                Transmittance, &  ! Optional input
                                RCS_Id,        &  ! Revision control
                                Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: NC_Filename
    INTEGER,                                   INTENT( IN )  :: Layer

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Transmittance

    ! -- Revision control
    CHARACTER( * ),                  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: n_Frequencies
    INTEGER :: n_Layers



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
    !#                -- CHECK IF ANYTHING NEEDS TO BE WRITTEN --               #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( Transmittance ) ) RETURN



    !#--------------------------------------------------------------------------#
    !#         -- GET THE DIMENSION VALUES AND CHECK THE LAYER INDEX --         #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Read the dimension values
    ! -------------------------

    Error_Status = Inquire_LBLRTM_netCDF( NC_Filename,                   &
                                          n_Frequencies = n_Frequencies, &
                                          n_Layers      = n_Layers,      &
                                          Message_Log   = Message_Log    )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining LBLRTM dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! Check the layer index input
    ! ---------------------------

    IF ( Layer < 1 .OR. Layer > n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Input LAYER value invalid for specified netCDF dataset.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READWRITE' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- WRITE THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! -------------
    ! Transmittance
    ! -------------

    IF ( PRESENT( Transmittance ) ) THEN

      ! -- Check the input array size
      IF ( SIZE( Transmittance ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,    &
                              'Input TRANSMITTANCE array size different from netCDF definition.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      ! -- Write the transmittance data
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          TRANSMITTANCE_VARNAME, &
                                          Transmittance, &
                                          START = (/ 1, Layer /), &
                                          COUNT = (/ n_Frequencies, 1 /) )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing TRANSMITTANCE variable for layer #", i3, &
                          &" in ", a )' ) Layer, TRIM( NC_Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_LBLRTM_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_LBLRTM_netCDF




!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_LBLRTM_netCDF
!
! PURPOSE:
!       Function to read LBLRTM spectral data from a netCDF format LBLRTM file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_netCDF( NC_Filename,                   &  ! Input
!                                          Layer,                         &  ! Input
!                                          Transmittance = Transmittance, &  ! Optional output
!                                          RCS_Id        = RCS_Id,        &  ! Revision control
!                                          Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format LBLRTM data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Layer:          Layer index value from which the output data
!                       is to be read.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Transmittance:  Transmittance data read from the netCDF LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       REAL( fp_kind )
!                       DIMENSION:  Rank-1, n_Frequencies
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the LBLRTM netCDF file read was successful
!                          == FAILURE an unrecoverable error occurred
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       Inquire_LBLRTM_netCDF:  Function to inquire a netCDF format 
!                               LBLRTM file to obtain information
!                               about the data dimensions and attributes.
!
!       Open_LBLRTM_netCDF:     Function to open a LBLRTM netCDF
!                               format file.
!
!       Close_LBLRTM_netCDF:    Function to close a LBLRTM netCDF
!                               format file.
!
!       Get_netCDF_Variable:    Function to retrieve a netCDF file variable
!                               variable by name.
!                               SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:             Function to close a netCDF file.
!                               SOURCE: netCDF library
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: Message_Handler module
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

  FUNCTION Read_LBLRTM_netCDF( NC_Filename,   &  ! Input
                               Layer,         &  ! Input
                               Transmittance, &  ! Optional output
                               RCS_Id,        &  ! Revision control
                               Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                            INTENT( IN )  :: NC_Filename
    INTEGER,                                   INTENT( IN )  :: Layer

    ! -- Optional output
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: Transmittance

    ! -- Revision control
    CHARACTER( * ),                  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: n_Frequencies
    INTEGER :: n_Layers



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
    !#                 -- CHECK IF ANYTHING NEEDS TO BE READ --                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( Transmittance ) ) RETURN



    !#--------------------------------------------------------------------------#
    !#         -- GET THE DIMENSION VALUES AND CHECK THE LAYER INDEX --         #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Read the dimension values
    ! -------------------------

    Error_Status = Inquire_LBLRTM_netCDF( NC_Filename, &
                                          n_Frequencies = n_Frequencies, &
                                          n_Layers      = n_Layers, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining LBLRTM dimensions from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------
    ! Check the layer index input
    ! ---------------------------

    IF ( Layer < 1 .OR. Layer > n_Layers ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'Input LAYER value invalid for specified netCDF dataset.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_LBLRTM_netCDF( TRIM( NC_Filename ), &
                                       NC_FileID, &
                                       Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF LBLRTM data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                             -- READ THE DATA --                          #
    !#--------------------------------------------------------------------------#

    ! -------------
    ! Transmittance
    ! -------------

    IF ( PRESENT( Transmittance ) ) THEN

      ! -- Check the input array size
      IF ( SIZE( Transmittance ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME,    &
                              'Input TRANSMITTANCE array size different from netCDF definition.', &
                              Error_Status,    &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      ! -- Read the transmittance data
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          TRANSMITTANCE_VARNAME, &
                                          Transmittance, &
                                          START = (/ 1, Layer /), &
                                          COUNT = (/ n_Frequencies, 1 /) )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing TRANSMITTANCE variable for layer #", i3, &
                          &" in ", a )' ) Layer, TRIM( NC_Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_LBLRTM_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF LBLRTM data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_LBLRTM_netCDF

END MODULE LBLRTM_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: LBLRTM_netCDF_IO.f90,v 2.4 2006/07/26 21:43:58 wd20pd Exp $
!
! $Date: 2006/07/26 21:43:58 $
!
! $Revision: 2.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_netCDF_IO.f90,v $
! Revision 2.4  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 2.3  2005/05/08 15:19:46  paulv
! - Upgraded to Fortran-95
! - Used separate status variables where required to prevent overwriting of
!   return value Error_Status with a SUCCESS result when a previous action
!   may have returned WARNING.
! - Removed WARNING return status from Read() and Write() public functions.
!
! Revision 2.2  2003/07/22 19:58:07  paulv
! - Corrected bug in Read() function. I was trying to PUT rather than GET!
!
! Revision 2.1  2003/07/16 16:38:17  paulv
! - Added direction variable to Create() and Inquire() functions.
!
! Revision 2.0  2003/07/14 20:01:47  paulv
! - New version.
!
! Revision 1.6  2002/07/18 12:11:54  paulv
! - Removed use of NC_dataID variable in creation and write functions. Dimension
!   and variable IDs are obtained as necessary.
! - Added RCS_Id optional output argument to all functions.
! - Improved argument checking.
! - Added and updated documentation.
!
! Revision 1.5  2002/06/05 19:09:10  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
! - Removed OPEN and CLOSE functions. Replaced with generic functions from
!   netCDF_Utility module.
!
! Revision 1.4  2002/05/15 19:25:07  paulv
! - Updated header documentation.
!
! Revision 1.3  2002/05/15 17:57:42  paulv
! - Replaced variable and dimension netCDF ID data types with a derived type
!   containing both. Don't know why I kept them separate.
! - Changed creation and write function interfaces to reflect changes in the
!   netCDF data ID derived types.
! - Added WRITE_MODULE_HISTORY and CREATION_DATE_AND_TIME global attribute
!   writes to the Create_LBLRTM_netCDF() function.
! - Added HISTORY and COMMENT optional arguments to the Create_LBLRTM_netCDF()
!   function for output as global attributes.
! - Added output of variable attributes LONG_NAME and UNITS.
!
! Revision 1.2  2002/05/14 20:47:57  paulv
! - Correct bugs in output error message construction. Forgot to put the "&"
!   on the continuation lines.
!
! Revision 1.1  2002/04/26 22:57:13  paulv
! nitial checkin.
!
!
!
