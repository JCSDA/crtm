!------------------------------------------------------------------------------
!M+
! NAME:
!       Solar_netCDF_IO
!
! PURPOSE:
!       Module containing routines to read and write Solar netCDF 
!       format files.
!       
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Solar_netCDF_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Solar_Define:          Module defining the Solar data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    Message_Handler module
!
!       netcdf:                Module supplied with the Fortran 90 version 
!                              of the netCDF libraries (at least v3.5.0).
!                              See http://www.unidata.ucar.edu/packages/netcdf
!
!       netCDF_Utility:        Module containing utility routines for
!                              netCDF file access.
!                              USEs: NETCDF_DIMENSION_UTILITY module
!                                    NETCDF_VARIABLE_UTILITY module
!                                    NETCDF_ATTRIBUTE_UTILITY module
!                                    
!
! CONTAINS:
!       Inquire_Solar_netCDF:  Function to inquire a netCDF format 
!                              Solar file to obtain information
!                              about the data dimensions and attributes.
!
!       Write_Solar_netCDF:    Function to write Solar data to a
!                              netCDF format Solar file.
!
!       Read_Solar_netCDF:     Function to read Solar data from a
!                              netCDF format Solar file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jan-2002
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

MODULE Solar_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE Solar_Define

  USE netcdf
  USE netCDF_Utility,  Open_Solar_netCDF =>  Open_netCDF, &
                      Close_Solar_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_Solar_netCDF
  PUBLIC :: Write_Solar_netCDF
  PUBLIC :: Read_Solar_netCDF



  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Solar_netCDF_IO.f90,v 2.4 2006/09/21 17:59:50 wd20pd Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME     = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME   = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME   = 'comment' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SOURCE_ATTNAME     = 'source' 
  CHARACTER( * ), PRIVATE, PARAMETER :: REFERENCES_ATTNAME = 'references' 
  
  ! -- Dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'

  ! -- Variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_VARNAME       = 'Begin_Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_VARNAME         = 'End_Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_VARNAME    = 'Frequency_Interval'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_TEMPERATURE_VARNAME = 'Blackbody_Temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: RADIUS_VARNAME                = 'Radius'
  CHARACTER( * ), PRIVATE, PARAMETER :: EARTH_SUN_DISTANCE_VARNAME    = 'Earth_Sun_Disance'
  CHARACTER( * ), PRIVATE, PARAMETER :: IRRADIANCE_VARNAME            = 'Irradiance'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_IRRADIANCE_VARNAME  = 'Blackbody_Irradiance'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_LONGNAME       = &
  'Begin frequency of irradiance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_LONGNAME         = &
  'End frequency of irradiance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_LONGNAME    = &
  'Frequency interval of irradiance data'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_TEMPERATURE_LONGNAME = &
  'Solar radiative temperature used to generate the blackbody source function'
  CHARACTER( * ), PRIVATE, PARAMETER :: RADIUS_LONGNAME                = &
  'Radius of visible solar disk, or photosphere'
  CHARACTER( * ), PRIVATE, PARAMETER :: EARTH_SUN_DISTANCE_LONGNAME    = &
  'Earth-Sun distance'
  CHARACTER( * ), PRIVATE, PARAMETER :: IRRADIANCE_LONGNAME            = &
  'Extraterrestrial TOA solar irradiance'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_IRRADIANCE_LONGNAME  = &
  'Extraterrestrial TOA solar blackbody irradiance using blackbody temperature'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: BEGIN_FREQUENCY_UNITS       = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: END_FREQUENCY_UNITS         = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_UNITS    = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_TEMPERATURE_UNITS = 'Kelvin (K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: RADIUS_UNITS                = 'Metres (m)'
  CHARACTER( * ), PRIVATE, PARAMETER :: EARTH_SUN_DISTANCE_UNITS    = 'Metres (m)'
  CHARACTER( * ), PRIVATE, PARAMETER :: IRRADIANCE_UNITS            = 'mW/(m^2.cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: BLACKBODY_IRRADIANCE_UNITS  = 'mW/(m^2.cm^-1)'

  ! -- Variable netCDF datatypes
  INTEGER,        PRIVATE, PARAMETER :: BEGIN_FREQUENCY_TYPE       = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: END_FREQUENCY_TYPE         = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: FREQUENCY_INTERVAL_TYPE    = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: BLACKBODY_TEMPERATURE_TYPE = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: RADIUS_TYPE                = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: EARTH_SUN_DISTANCE_TYPE    = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: IRRADIANCE_TYPE            = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: BLACKBODY_IRRADIANCE_TYPE  = NF90_DOUBLE


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_Solar_GAtts
!
! PURPOSE:
!       Function to write the supplied attributes to a netCDF format
!       Solar data file.
!
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_Solar_GAtts( NC_Filename,              &  ! Input
!                                         NC_FileID,                &  ! Input
!                                         Title       = Title,      &  ! Optional input
!                                         History     = History,    &  ! Optional input
!                                         Comment     = Comment,    &  ! Optional input
!                                         Source      = Source,     &  ! Optional input
!                                         References  = References, &  ! Optional input
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF Solar format data to write to.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number associated with the input
!                         filename.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE
!                         variable.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE
!                         variable. 
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful
!                            == FAILURE an error occurred writing the supplied
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Put_netCDF_Attributes:  Function to write attribute data to
!                               a netCDF data file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Feb-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Solar_GAtts( NC_Filename, &  ! Input
                              NC_FileID,   &  ! Input
                              Title,       &  ! Optional input
                              History,     &  ! Optional input
                              Comment,     &  ! Optional input
                              Source,      &  ! Optional input
                              References,  &  ! Optional input
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
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Source
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: References

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Solar_GAtts'

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
                                '$Id: Solar_netCDF_IO.f90,v 2.4 2006/09/21 17:59:50 wd20pd Exp $' )


    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WRITE_MODULE_HISTORY_GATTNAME//&
                            ' attribute to '// &
                            TRIM( NC_FileNAME )//' - '// &
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
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- DEFINE THE USER ACCESSIBLE GLOBAL ATTRIBUTES --            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The TITLE
    ! ---------

    IF ( PRESENT( Title ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           TRIM( Title ) )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//TITLE_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           TRIM( History ) )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//HISTORY_GATTNAME//' attribute to '//&
                              TRIM( NC_FileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//COMMENT_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- GET THE SOLAR IRRADIANCE ATTRIBUTES --               #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! The SOURCE
    ! ----------

    IF ( PRESENT( Source ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           SOURCE_ATTNAME, &
                                           TRIM( Source ), &
                                           Variable_Name = IRRADIANCE_VARNAME )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//IRRADIANCE_VARNAME//' variable '//&
                              SOURCE_ATTNAME//' attribute to '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! --------------
    ! The REFERENCES
    ! --------------

    IF ( PRESENT( References ) ) THEN

      Error_Status = Put_netCDF_Attribute( NC_FileID, &
                                           REFERENCES_ATTNAME, &
                                           TRIM( References ), &
                                           Variable_Name = IRRADIANCE_VARNAME )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//IRRADIANCE_VARNAME//' variable '//&
                              REFERENCES_ATTNAME//' attribute to '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_Solar_GAtts





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_Solar_GAtts
!
! PURPOSE:
!       Function to read the requested attributes from a netCDF format
!       Solar data file.
!
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_Solar_GAtts( NC_Filename,              &  ! Input
!                                        NC_FileID,                &  ! Input
!                                        Title       = Title,      &  ! Optional output
!                                        History     = History,    &  ! Optional output
!                                        Comment     = Comment,    &  ! Optional output
!                                        Source      = Source,     &  ! Optional output
!                                        References  = References, &  ! Optional output
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF Solar format data to read.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number associated with the input
!                         filename.
!                         UNITS:      None
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
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE
!                         variable.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE
!                         variable. 
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful
!                            == FAILURE an error occurred reading the requested
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Get_netCDF_Attributes:  Function to read attribute data from a netCDF 
!                               data file.
!                               SOURCE: NETCDF_UTILITY module
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Feb-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Solar_GAtts( NC_Filename,  &  ! Input
                             NC_FileID,    &  ! Input
                             Title,        &  ! Optional output
                             History,      &  ! Optional output
                             Comment,      &  ! Optional output
                             Source,       &  ! Optional output
                             References,   &  ! Optional output
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
    INTEGER,                  INTENT( IN )  :: NC_FileID

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Source
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: References

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_Solar_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------



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

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           Title )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Title )

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           History )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//' attribute from '//&
                              TRIM( NC_FileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( History )

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           Comment )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//' attribute from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Comment )

    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- GET THE SOLAR IRRADIANCE ATTRIBUTES --               #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! The SOURCE
    ! ----------

    IF ( PRESENT( Source ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           SOURCE_ATTNAME, &
                                           Source, &
                                           Variable_Name = IRRADIANCE_VARNAME )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//IRRADIANCE_VARNAME//' variable '//&
                              SOURCE_ATTNAME//' attribute from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Source )

    END IF


    ! --------------
    ! The REFERENCES
    ! --------------

    IF ( PRESENT( References ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           REFERENCES_ATTNAME, &
                                           References, &
                                           Variable_Name = IRRADIANCE_VARNAME )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//IRRADIANCE_VARNAME//' variable '//&
                              REFERENCES_ATTNAME//' attribute from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( References )

    END IF

  END FUNCTION Read_Solar_GAtts





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
!       Inquire_Solar_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF format Solar source data file to obtain
!       the dimensions and attributes.
!
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_Solar_netCDF( NC_Filename,                   &  ! Input
!                                            n_Frequencies = n_Frequencies, &  ! Optional output
!                                            Title         = Title,         &  ! Optional output
!                                            History       = History,       &  ! Optional output
!                                            Comment       = Comment,       &  ! Optional output
!                                            Source        = Source,        &  ! Optional output
!                                            References    = References,    &  ! Optional output
!                                            RCS_Id        = RCS_Id,        &  ! Version control
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF
!                         format Solar data file to inquire.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Frequencies:    The number of spectral points.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF Solar file.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Source:           Character string written into the SOURCE
!                         attribute field of the IRRADIANCE
!                         variable.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       References:       Character string written into the REFERENCES
!                         attribute field of the IRRADIANCE
!                         variable. 
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the netCDF file inquiry was successful
!                            == FAILURE an error occurred reading any of the requested
!                                       dimension data.
!                            == WARNING - an error occurred reading any of the requested
!                                         global file attributes, or
!                                       - an error occurred closing the netCDF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Open_Solar_netCDF:       Function to open a netCDF format Solar
!                                data file.
!
!       Read_Solar_GAtts:        Function to read the global attributes from
!                                a netCDF format Solar data file.
!
!       Close_Solar_netCDF:      Function to close a netCDF format Solar
!                                data file with error checking.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_Solar_netCDF( NC_Filename,   &  ! Input
                                 n_Frequencies, &  ! Optional output
                                 Title,         &  ! Optional output
                                 History,       &  ! Optional output
                                 Comment,       &  ! Optional output
                                 Source,        &  ! Optional output
                                 References,    &  ! Optional output
                                 RCS_Id,        &  ! Version control
                                 Message_Log )  &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Frequencies
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Source
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: References

    ! -- Version control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_Solar_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NF90_Status
    INTEGER :: NC_FileID



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

    Error_Status = Open_Solar_netCDF( TRIM( NC_FileNAME ), &
                                      NC_FileID, &
                                      Mode = 'Read' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF Solar data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE SPECTRAL DIMENSION --                     #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Frequencies ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           FREQUENCY_DIMNAME, &
                                           n_Frequencies, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//FREQUENCY_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- GET THE ATTRIBUTES --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_Solar_GAtts( TRIM( NC_Filename ), &
                                     NC_FileID, &
                                     Title      = Title, &
                                     History    = History, &
                                     Comment    = Comment, &
                                     Source     = Source, &
                                     References = References, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_Solar_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF Solar data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_Solar_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_Solar_netCDF
!
! PURPOSE:
!       Function to write Solar data to a netCDF format Solar file.
!
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!         Error_Status = Write_Solar_netCDF( NC_Filename,               &  ! Input
!                                            Solar,                     &  ! Input
!                                            Title       = Title,       &  ! Optional input
!                                            History     = History,     &  ! Optional input
!                                            Comment     = Comment,     &  ! Optional input
!                                            Source      = Source,      &  ! Optional input
!                                            References  = References,  &  ! Optional input
!                                            RCS_Id      = RCS_Id,      &  ! Version control
!                                            Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format Solar data file to create.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Solar:           Structure containing the solar data to write to file.
!                        UNITS:      N/A
!                        TYPE:       Solar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Source:          Character string written into the SOURCE
!                        attribute field of the IRRADIANCE
!                        variable.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       References:      Character string written into the REFERENCES
!                        attribute field of the IRRADIANCE
!                        variable. 
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
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
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input Solar structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                           == WARNING an error occurred writing the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Associated_Solar:        Function to test the association status
!                                of the pointer members of a Solar
!                                structure.
!                                SOURCE: SOLAR_DEFINE module
!
!       NF90_CREATE:             Function to create a netCDF file.
!                                SOURCE: netCDF library
!
!       NF90_DEF_DIM:            Function to define a dimension in
!                                a netCDF dataset.
!                                SOURCE: netCDF library
!
!       NF90_PUT_ATT:            Function to write an attribute to
!                                a netCDF dataset.
!                                SOURCE: netCDF library
!
!       Write_Solar_GAtts:       Function to write the global attributes
!                                to the netCDF formnat Solar file.
!
!       NF90_DEF_VAR:            Function to define a variable in
!                                a netCDF dataset.
!                                SOURCE: netCDF library
!
!       NF90_ENDDEF:             Function to put a netCDF dataset into
!                                data mode.
!                                SOURCE: netCDF library
!
!       Put_netCDF_Variable:     Function to write variable data to a
!                                netCDF data file.
!                                SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:              Function to close a netCDF file.
!                                SOURCE: netCDF library
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten
!
! RESTRICTIONS:
!       None..
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Solar_netCDF( NC_Filename,  &  ! Input
                               Solar,        &  ! Input
                               Title,        &  ! Optional input
                               History,      &  ! Optional input
                               Comment,      &  ! Optional input
                               Source,       &  ! Optional input
                               References,   &  ! Optional input
                               RCS_Id,       &  ! Version control
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
    TYPE( Solar_type ),       INTENT( IN )  :: Solar

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Source
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: References

    ! -- Version control
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_Solar_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: NF90_Status1, NF90_Status2
    INTEGER :: Close_Status

    INTEGER :: NC_FileID
    INTEGER :: Frequency_DimID
    INTEGER :: VarID



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                  -- SET THE RCS ID ARGUMENT IF SUPPLIED --               #
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

    IF ( .NOT. Associated_Solar( Solar ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT Solar pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( TRIM( NC_FileNAME ), &
                               NF90_CLOBBER, &
                               NC_FileID     )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating '//TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- DEFINE THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! The number of frequencies
    ! -------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FREQUENCY_DIMNAME, &
                                Solar%n_Frequencies, &
                                Frequency_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//FREQUENCY_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- DEFINE THE VARIABLES --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------
    ! Begin frequency
    ! ---------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BEGIN_FREQUENCY_VARNAME, &
                                BEGIN_FREQUENCY_TYPE, &
                                varid = varID )

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
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 BEGIN_FREQUENCY_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 BEGIN_FREQUENCY_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
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
                                varid = varID )

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
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 END_FREQUENCY_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 END_FREQUENCY_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
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
                                varid = varID )

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
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 FREQUENCY_INTERVAL_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 FREQUENCY_INTERVAL_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! Solar blackbody temperature
    ! ---------------------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BLACKBODY_TEMPERATURE_VARNAME, &
                                BLACKBODY_TEMPERATURE_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BLACKBODY_TEMPERATURE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 BLACKBODY_TEMPERATURE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 BLACKBODY_TEMPERATURE_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BLACKBODY_TEMPERATURE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! Solar Radius
    ! ------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RADIUS_VARNAME, &
                                RADIUS_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RADIUS_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 RADIUS_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 RADIUS_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RADIUS_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! Earth-Sun distance
    ! ------------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                EARTH_SUN_DISTANCE_VARNAME, &
                                EARTH_SUN_DISTANCE_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//EARTH_SUN_DISTANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 EARTH_SUN_DISTANCE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 EARTH_SUN_DISTANCE_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//EARTH_SUN_DISTANCE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! Solar irradiance
    ! ----------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                IRRADIANCE_VARNAME, &
                                IRRADIANCE_TYPE, &
                                dimids = Frequency_DimID, &
                                varid  = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//IRRADIANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 IRRADIANCE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 IRRADIANCE_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//IRRADIANCE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------------
    ! Blackbody solar irradiance
    ! --------------------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BLACKBODY_IRRADIANCE_VARNAME, &
                                BLACKBODY_IRRADIANCE_TYPE, &
                                dimids = Frequency_DimID, &
                                varid  = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BLACKBODY_IRRADIANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 LONGNAME_ATTNAME, &
                                 BLACKBODY_IRRADIANCE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 varID, &
                                 UNITS_ATTNAME, &
                                 BLACKBODY_IRRADIANCE_UNITS )

    IF ( NF90_Status1 /= NF90_NOERR .OR. NF90_Status2 /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BLACKBODY_IRRADIANCE_VARNAME//' variable attributes to '// &
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

    Error_Status = Write_Solar_GAtts( TRIM( NC_FileNAME ), &
                                      NC_FileID, &
                                      Title      = Title, &
                                      History    = History, &
                                      Comment    = Comment, &
                                      Source     = Source, &
                                      References = References, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attribute to '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Take netCDF file out of define mode
    ! -----------------------------------

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


    ! ------------------
    ! The frequency data
    ! ------------------

    ! -- Begin frequency
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Solar%Begin_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BEGIN_FREQUENCY_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- End frequency
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        Solar%End_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//END_FREQUENCY_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Frequency interval
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Solar%Frequency_Interval )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_INTERVAL_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------------
    ! The solar temperature for the blackbody source
    ! ----------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_TEMPERATURE_VARNAME, &
                                        Solar%Blackbody_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BLACKBODY_TEMPERATURE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! The geometry
    ! ------------

    ! -- Solar radius
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RADIUS_VARNAME, &
                                        Solar%Radius )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RADIUS_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Earth-Sun distance
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        EARTH_SUN_DISTANCE_VARNAME, &
                                        Solar%Earth_Sun_Distance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//EARTH_SUN_DISTANCE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------
    ! The irradiance source functions
    ! -------------------------------

    ! -- Solar source spectrum
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        IRRADIANCE_VARNAME, &
                                        Solar%Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//IRRADIANCE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Blackbody source spectrum
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_IRRADIANCE_VARNAME, &
                                        Solar%Blackbody_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BLACKBODY_IRRADIANCE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_Solar_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF Solar data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Solar_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_Solar_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format Solar file.
!
! CATEGORY:
!       Solar
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_Solar_netCDF( NC_Filename,              &  ! Input
!                                         Solar,                    &  ! Output
!                                         Title       = Title,      &  ! Optional output
!                                         History     = History,    &  ! Optional output
!                                         Comment     = Comment,    &  ! Optional output
!                                         Source      = Source,     &  ! Optional output
!                                         References  = References, &  ! Optional output
!                                         RCS_Id      = RCS_Id,     &  ! Version control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF Solar
!                        format Solar data file to read.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Solar:           Structure to contain the Solar data read
!                        from file.
!                        UNITS:      N/A
!                        TYPE:       Solar_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF Solar file.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Source:          Character string written into the SOURCE
!                        attribute field of the IRRADIANCE
!                        variable.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       References:      Character string written into the REFERENCES
!                        attribute field of the IRRADIANCE
!                        variable. 
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      None
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE a unrecoverable read error occurred.
!                           == WARNING an error occurred reading the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Open_Solar_netCDF:     Function to open a netCDF format Solar
!                              data file.
! 
!       Inquire_Solar_netCDF:  Function to inquire a netCDF format 
!                              Solar file to obtain information
!                              about the data dimensions and attributes.
!
!       Close_Solar_netCDF:    Function to close a netCDF format Solar
!                              data file with error checking.
!
!       Allocate_Solar:        Function to allocate the pointer members
!                              of an Solar structure.
!                              SOURCE: SOLAR_DEFINE module
!
!       Get_netCDF_Variable:   Function to read variable data from a
!                              netCDF data file.
!                              SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:            Function to close a netCDF file.
!                              SOURCE: netCDF library
!
!       Display_Message:       Subroutine to output messages
!                              SOURCE: Message_Handler module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output Solar argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jan-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Solar_netCDF( NC_Filename,  &  ! Input
                              Solar,        &  ! Output
                              Title,        &  ! Optional output
                              History,      &  ! Optional output
                              Comment,      &  ! Optional output
                              Source,       &  ! Optional output
                              References,   &  ! Optional output
                              RCS_Id,       &  ! Version control
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: NC_Filename

    ! -- Output
    TYPE( Solar_type ),       INTENT( IN OUT ) :: Solar

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Source
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: References

    ! -- Version control
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_Solar_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Frequencies


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                  -- SET THE RCS ID ARGUMENT IF SUPPLIED --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- GET THE DIMENSION VALUE AND ALLOCATE THE Solar STRUCTURE --     #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Read the dimension value
    ! ------------------------

    Error_Status = Inquire_Solar_netCDF( TRIM( NC_Filename ), &
                                         n_Frequencies = n_Frequencies, &
                                         Title         = Title, &
                                         History       = History, &
                                         Comment       = Comment, &
                                         Source        = Source, &
                                         References    = References, &
                                         Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining Solar dimension/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_Solar( n_Frequencies, &
                                   Solar, &
                                   Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating Solar structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Solar_netCDF( TRIM( NC_FileNAME ), &
                                      NC_FileID, &
                                      Mode = 'Read' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF Solar data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE Solar DATA --                         #
    !#--------------------------------------------------------------------------#


    ! ------------------
    ! The frequency data
    ! ------------------

    ! -- Begin frequency
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BEGIN_FREQUENCY_VARNAME, &
                                        Solar%Begin_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BEGIN_FREQUENCY_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- End frequency
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        END_FREQUENCY_VARNAME, &
                                        Solar%End_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//END_FREQUENCY_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Frequency interval
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_INTERVAL_VARNAME, &
                                        Solar%Frequency_Interval )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FREQUENCY_INTERVAL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------------
    ! The solar temperature for the blackbody source
    ! ----------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_TEMPERATURE_VARNAME, &
                                        Solar%Blackbody_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BLACKBODY_TEMPERATURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! The geometry
    ! ------------

    ! -- Solar radius
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        RADIUS_VARNAME, &
                                        Solar%Radius )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//RADIUS_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Earth-Sun distance
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        EARTH_SUN_DISTANCE_VARNAME, &
                                        Solar%Earth_Sun_Distance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//EARTH_SUN_DISTANCE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------
    ! The irradiance source functions
    ! -------------------------------

    ! -- Solar source spectrum
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        IRRADIANCE_VARNAME, &
                                        Solar%Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//IRRADIANCE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Blackbody source spectrum
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BLACKBODY_IRRADIANCE_VARNAME, &
                                        Solar%Blackbody_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BLACKBODY_IRRADIANCE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_Solar_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF Solar data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- COMPUTE THE Solar FREQUENCY GRID --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Frequency_Solar( Solar )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing frequency grid for solar data from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Read_Solar_netCDF

END MODULE Solar_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Solar_netCDF_IO.f90,v 2.4 2006/09/21 17:59:50 wd20pd Exp $
!
! $Date: 2006/09/21 17:59:50 $
!
! $Revision: 2.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Solar_netCDF_IO.f90,v $
! Revision 2.4  2006/09/21 17:59:50  wd20pd
! Replaced all references to Error_Handler with Message_Handler.
!
! Revision 2.3  2005/08/11 17:33:14  paulv
! - Added call to Frequency_Solar() in the Read_Solar_netCDF() function.
!
! Revision 2.2  2004/08/31 18:20:27  paulv
! - Upgraded to Fortran95.
! - Added structure association test to the Write() function.
! - Changed INTENT of Solar structure in Read() function from OUT to
!   IN OUT. Necessary to prevent memory leaks.
! - If an error occurs closing a netCDF file at the end of the Read() and
!   Write() functions, a warning *message* is issued, but the error status
!   is not set to WARNING.
! - Updated header documentation.
!
! Revision 2.1  2004/06/25 17:18:30  paulv
! - Added write module history and creation time/date global attribute writes
!   to the GAtt() writer.
! - Cosmetic changes.
!
! Revision 2.0  2003/02/11 22:13:56  paulv
! - New version. Removed the Create_Solar() function. Only public entities
!   are now the Inquire, Write, and Read functions.
!
! Revision 1.2  2002/08/22 20:24:00  paulv
! - Using netCDF_Utility module function Get_netCDF_Variable() in place
!   of old get_ncdf_variable() function.
!
! Revision 1.1  2002/08/22 17:24:02  paulv
! Initial checkin.
!
!
!
!
