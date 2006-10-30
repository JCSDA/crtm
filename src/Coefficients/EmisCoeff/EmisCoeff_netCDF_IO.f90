!--------------------------------------------------------------------------------
!M+
! NAME:
!       EmisCoeff_netCDF_IO
!
! PURPOSE:
!       Module containing routines to create, inquire, read and write netCDF
!       format Spectral EmisCoeff files.
!       
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE EmisCoeff_netCDF_IO
!
! MODULES:
!       Type_Kinds:        Module containing definitions for kinds
!                          of variable types.
!
!       Message_Handler:   Module to define simple error codes and
!                          handle error conditions
!                          USEs: FILE_UTILITY module
!
!       EmisCoeff_Define:  Module defining the Spectral EmisCoeff data
!                          structure and containing routines to manipulate it.
!                          USEs: TYPE_KINDS module
!                                FILE_UTILITY module
!                                ERROR_HANDLER module
!
!       netcdf:            Module supplied with the Fortran 90 version 
!                          of the netCDF libraries (at least v3.5.0).
!                          See http://www.unidata.ucar.edu/packages/netcdf
!
!       netCDF_Utility:    Module containing utility routines for
!                          netCDF file access.
!                          USEs: NETCDF_DIMENSION_UTILITY module
!                                NETCDF_VARIABLE_UTILITY module
!                                NETCDF_ATTRIBUTE_UTILITY module
!
! CONTAINS:
!       Inquire_EmisCoeff_netCDF:  Function to inquire a netCDF format 
!                                  EmisCoeff file to obtain information
!                                  about the data dimensions and attributes.
!
!       Write_EmisCoeff_netCDF:    Function to write EmisCoeff data to a
!                                  netCDF format EmisCoeff file.
!
!       Read_EmisCoeff_netCDF:     Function to read EmisCoeff data from a
!                                  netCDF format EmisCoeff file.
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
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE EmisCoeff_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE EmisCoeff_Define

  USE netcdf
  USE netCDF_Utility,  Open_EmisCoeff_netCDF =>  Open_netCDF, &
                      Close_EmisCoeff_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_EmisCoeff_netCDF
  PUBLIC :: Write_EmisCoeff_netCDF
  PUBLIC :: Read_EmisCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: EmisCoeff_netCDF_IO.f90,v 2.1 2006/05/02 14:58:34 dgroff Exp $'

  ! -- Keyword set value
  INTEGER,        PRIVATE, PARAMETER :: UNSET = 0
  INTEGER,        PRIVATE, PARAMETER ::   SET = 1

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME   = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME = 'comment' 

  ! -- Dimension names
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_DIMNAME      = 'n_Angles'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_DIMNAME  = 'n_Frequencies'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIND_SPEED_DIMNAME = 'n_Wind_Speeds'

  ! -- Variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: RELEASE_VARNAME    = 'Release'
  CHARACTER( * ), PRIVATE, PARAMETER :: VERSION_VARNAME    = 'Version'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATA_TYPE_VARNAME  = 'Data_Type'
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_VARNAME      = 'Angle'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_VARNAME  = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIND_SPEED_VARNAME = 'Wind_Speed'
  CHARACTER( * ), PRIVATE, PARAMETER :: EMISSIVITY_VARNAME = 'Emissivity'

  ! -- Variable description attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER( * ), PRIVATE, PARAMETER :: RELEASE_DESCRIPTION    = &
    'Release number of Spectral EmisCoeff data file'
  CHARACTER( * ), PRIVATE, PARAMETER :: VERSION_DESCRIPTION    = &
    'Version number of Spectral EmisCoeff data file'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATA_TYPE_DESCRIPTION  = &
    'Flag to indicate if this EmisCoeff data is for the spectral or sensor emissivity model'
  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_DESCRIPTION      = &
    'Angle (Z) dimension values for emissivity data.'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_DESCRIPTION  = &
    'Frequency (F) dimension values for emissivity data.'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIND_SPEED_DESCRIPTION = &
    'Wind speed (V) dimension values for emissivity data.'
  CHARACTER( * ), PRIVATE, PARAMETER :: EMISSIVITY_DESCRIPTION = &
    'Spectral sea surface emissivity data.'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_LONGNAME      = 'Angle' 
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_LONGNAME  = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIND_SPEED_LONGNAME = 'Wind Speed' 
  CHARACTER( * ), PRIVATE, PARAMETER :: EMISSIVITY_LONGNAME = 'Emissivity' 

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: ANGLE_UNITS      = 'degrees from vertical'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_UNITS  = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WIND_SPEED_UNITS = 'metres per second (m.s^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: EMISSIVITY_UNITS = 'None.'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER,         PRIVATE, PARAMETER :: IP_FILLVALUE = -1
  REAL( fp_kind ), PRIVATE, PARAMETER :: FP_FILLVALUE = -999.0_fp_kind

  INTEGER,         PRIVATE, PARAMETER :: RELEASE_FILLVALUE    = IP_FILLVALUE
  INTEGER,         PRIVATE, PARAMETER :: VERSION_FILLVALUE    = IP_FILLVALUE
  INTEGER,         PRIVATE, PARAMETER :: DATA_TYPE_FILLVALUE  = IP_FILLVALUE
  REAL( fp_kind ), PRIVATE, PARAMETER :: ANGLE_FILLVALUE      = FP_FILLVALUE
  REAL( fp_kind ), PRIVATE, PARAMETER :: FREQUENCY_FILLVALUE  = FP_FILLVALUE
  REAL( fp_kind ), PRIVATE, PARAMETER :: WIND_SPEED_FILLVALUE = FP_FILLVALUE
  REAL( fp_kind ), PRIVATE, PARAMETER :: EMISSIVITY_FILLVALUE = FP_FILLVALUE
  REAL( fp_kind ), PRIVATE, PARAMETER :: DERIVATIVE_FILLVALUE = FP_FILLVALUE

  ! -- Variable netCDF datatypes
  INTEGER, PRIVATE, PARAMETER :: RELEASE_TYPE    = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: VERSION_TYPE    = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: DATA_TYPE_TYPE  = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: ANGLE_TYPE      = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: FREQUENCY_TYPE  = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: WIND_SPEED_TYPE = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: EMISSIVITY_TYPE = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: DERIVATIVE_TYPE = NF90_DOUBLE


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_EmisCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF Spectral EmisCoeff
!       data file.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisCoeff_GAtts( NC_Filename,              &  ! Input
!                                             NC_FileID,                &  ! Input
!                                             Title       = Title,      &  ! Optional input
!                                             History     = History,    &  ! Optional input
!                                             Comment     = Comment,    &  ! Optional input
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF EmisCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_EmisCoeff_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF EmisCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the global attribute write was successful
!                            == FAILURE an error occurred writing the supplied
!                                       global attribute(s).
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
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Write_EmisCoeff_GAtts( NC_Filename,  &  ! Input
                                  NC_FileID,    &  ! Input
                                  Title,        &  ! Optional input
                                  History,      &  ! Optional input
                                  Comment,      &  ! Optional input
                                  Message_Log ) &  ! Error messaging
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

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_EmisCoeff_GAtts'

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
                              TRIM( NC_FileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_EmisCoeff_GAtts





!--------------------------------------------------------------------------------
!
! NAME:
!       Read_EmisCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF Spectral EmisCoeff
!       data file.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisCoeff_GAtts( NC_Filename,                   &  ! Input
!                                            NC_FileID,                     &  ! Input
!                                            Title         = Title,         &  ! Optional output
!                                            History       = History,       &  ! Optional output
!                                            Comment       = Comment,       &  ! Optional output
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF EmisCoeff format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_EmisCoeff_netCDF() function.
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
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF EmisCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the global attribute read was successful
!                            == FAILURE an error occurred reading the requested
!                                       global attribute(s).
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Get_netCDF_Attribute: Function to read attribute data from a netCDF 
!                             data file.
!                             SOURCE: NETCDF_ATTRIBUTE module
!
!       Display_Message:      Subroutine to output messages
!                             SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Read_EmisCoeff_GAtts( NC_Filename,   &  ! Input
                                 NC_FileID,     &  ! Input
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
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_EmisCoeff_GAtts'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 10000 ) :: Long_String



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
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TITLE_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TITLE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Title = Long_String(1:MIN( LEN( Title ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------
    ! The HISTORY
    ! -----------

    IF ( PRESENT( History ) ) THEN

      History = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           HISTORY_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//HISTORY_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      History = Long_String(1:MIN( LEN( History ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------
    ! The COMMENT
    ! -----------

    IF ( PRESENT( Comment ) ) THEN

      Comment = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           COMMENT_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//COMMENT_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Comment = Long_String(1:MIN( LEN( Comment ), LEN_TRIM( Long_String ) ))

    END IF

  END FUNCTION Read_EmisCoeff_GAtts




!--------------------------------------------------------------------------------
!
! NAME:
!       Create_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to create a netCDF Spectral EmisCoeff data file for writing.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_EmisCoeff_netCDF( NC_Filename,              &  ! Input
!                                               n_Angles,                 &  ! Input
!                                               n_Frequencies,            &  ! Input
!                                               n_Wind_Speeds,            &  ! Input
!                                               NC_FileID,                &  ! Output
!                                               Title       = Title,      &  ! Optional input
!                                               History     = History,    &  ! Optional input
!                                               Comment     = Comment,    &  ! Optional input
!                                               RCS_Id      = RCS_Id,     &  ! Optional output
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF EmisCoeff format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Angles:           The angle dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Frequencies:      The frequency dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Wind_Speeds:      The wind speed dimension of the emissivity data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF EmisCoeff file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF EmisCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF EmisCoeff file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the netCDF file creation was successful
!                              == FAILURE an unrecoverable error occurred
!                              == WARNING - an error occurred writing any of the
!                                           supplied global file attributes, or
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
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output data file already exists, it is overwritten.
!       - The created netCDF file is in DATA mode upon exiting this function.
!       - If a FAILURE error occurs, the created netCDF file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!
!--------------------------------------------------------------------------------

  FUNCTION Create_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                    n_Angles,      &  ! Input
                                    n_Frequencies, &  ! Input
                                    n_Wind_Speeds, &  ! Input
                                    NC_FileID,     &  ! Output
                                    Title,         &  ! Optional input
                                    History,       &  ! Optional input
                                    Comment,       &  ! Optional input
                                    RCS_Id,        &  ! Optional output
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
    INTEGER,                  INTENT( IN )  :: n_Angles
    INTEGER,                  INTENT( IN )  :: n_Frequencies
    INTEGER,                  INTENT( IN )  :: n_Wind_Speeds

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_EmisCoeff_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: Description_Status
    INTEGER :: Longname_Status
    INTEGER :: Units_Status
    INTEGER :: Fillvalue_Status
    INTEGER :: Angle_dimID
    INTEGER :: Frequency_dimID
    INTEGER :: Wind_Speed_dimID
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Check the dimensions
    ! --------------------

    IF ( n_Angles      < 1 .OR. &
         n_Frequencies < 1 .OR. &
         n_Wind_Speeds < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'All dimensions must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( NC_Filename,  &
                               NF90_CLOBBER, &
                               NC_FileID     )

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

    ! --------------------
    ! The number of angles
    ! --------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                ANGLE_DIMNAME, &
                                n_Angles, &
                                Angle_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//ANGLE_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------
    ! The number of frequencies
    ! -------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                FREQUENCY_DIMNAME, &
                                n_Frequencies, &
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


    ! -------------------------
    ! The number of wind speeds
    ! -------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                WIND_SPEED_DIMNAME, &
                                n_Wind_Speeds, &
                                Wind_Speed_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//WIND_SPEED_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE GLOBAL ATTRIBUTES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_EmisCoeff_GAtts( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          Title       = Title, &
                                          History     = History, &
                                          Comment     = Comment, &
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
    !#                        -- DEFINE THE VARIABLES --                        #
    !#--------------------------------------------------------------------------#

    ! ------------
    ! File Release
    ! ------------

    ! -- Define the release variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                RELEASE_VARNAME, &
                                RELEASE_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//RELEASE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               RELEASE_DESCRIPTION, &
                                               Variable_Name = RELEASE_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               RELEASE_FILLVALUE, &
                                               Variable_Name = RELEASE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RELEASE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! File Version
    ! ------------

    ! -- Define the release variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                VERSION_VARNAME, &
                                VERSION_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//VERSION_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               VERSION_DESCRIPTION, &
                                               Variable_Name = VERSION_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               VERSION_FILLVALUE, &
                                               Variable_Name = VERSION_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//VERSION_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! The data type
    ! -------------

    ! -- Define the release variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DATA_TYPE_VARNAME, &
                                DATA_TYPE_TYPE, &
                                varid = varID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DATA_TYPE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               DATA_TYPE_DESCRIPTION, &
                                               Variable_Name = DATA_TYPE_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               DATA_TYPE_FILLVALUE, &
                                               Variable_Name = DATA_TYPE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATA_TYPE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----
    ! Angle
    ! -----

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ANGLE_VARNAME, &
                                ANGLE_TYPE, &
                                varid  = varID, &
                                dimids = Angle_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ANGLE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               ANGLE_DESCRIPTION, &
                                               Variable_Name = ANGLE_VARNAME )

    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               ANGLE_LONGNAME, &
                                               Variable_Name = ANGLE_VARNAME )

    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               ANGLE_UNITS, &
                                               Variable_Name = ANGLE_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               ANGLE_FILLVALUE, &
                                               Variable_Name = ANGLE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------
    ! Frequency
    ! ---------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                varid  = varID, &
                                dimids = Frequency_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               FREQUENCY_DESCRIPTION, &
                                               Variable_Name = FREQUENCY_VARNAME )

    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               FREQUENCY_LONGNAME, &
                                               Variable_Name = FREQUENCY_VARNAME )

    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               FREQUENCY_UNITS, &
                                               Variable_Name = FREQUENCY_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               FREQUENCY_FILLVALUE, &
                                               Variable_Name = FREQUENCY_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------
    ! Wind_Speed
    ! ----------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WIND_SPEED_VARNAME, &
                                WIND_SPEED_TYPE, &
                                varid  = varID, &
                                dimids = Wind_Speed_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WIND_SPEED_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               WIND_SPEED_DESCRIPTION, &
                                               Variable_Name = WIND_SPEED_VARNAME )

    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               WIND_SPEED_LONGNAME, &
                                               Variable_Name = WIND_SPEED_VARNAME )

    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               WIND_SPEED_UNITS, &
                                               Variable_Name = WIND_SPEED_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               WIND_SPEED_FILLVALUE, &
                                               Variable_Name = WIND_SPEED_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WIND_SPEED_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Emissivity data
    ! ---------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                EMISSIVITY_VARNAME, &
                                EMISSIVITY_TYPE, &
                                varid = varID, &
                                dimids = (/ Angle_DimID, &
                                            Frequency_DimID, &
                                            Wind_Speed_DimID /) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//EMISSIVITY_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Write some attributes
    Description_Status = Put_netCDF_Attribute( NC_FileID, &
                                               DESCRIPTION_ATTNAME, &
                                               EMISSIVITY_DESCRIPTION, &
                                               Variable_Name = EMISSIVITY_VARNAME )

    Longname_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               EMISSIVITY_LONGNAME, &
                                               Variable_Name = EMISSIVITY_VARNAME )

    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               EMISSIVITY_UNITS, &
                                               Variable_Name = EMISSIVITY_VARNAME )

    Fillvalue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               EMISSIVITY_FILLVALUE, &
                                               Variable_Name = EMISSIVITY_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         Longname_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         Fillvalue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//EMISSIVITY_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- TAKE THE NETCDF FILE OUT OF DEFINE MODE --             #
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

  END FUNCTION Create_EmisCoeff_netCDF





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
!       Inquire_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF Spectral EmisCoeff format file to obtain
!       the dimension values and global attributes.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                                n_Angles      = n_Angles,      &  ! Optional output
!                                                n_Frequencies = n_Frequencies, &  ! Optional output
!                                                n_Wind_Speeds = n_Wind_Speeds, &  ! Optional output
!                                                Angle         = Angle,         &  ! Optional output
!                                                Frequency     = Frequency,     &  ! Optional output
!                                                Wind_Speed    = Wind_Speed,    &  ! Optional output
!                                                Release       = Release,       &  ! Optional Output
!                                                Version       = Version,       &  ! Optional Output
!                                                Title         = Title,         &  ! Optional output
!                                                History       = History,       &  ! Optional output
!                                                Comment       = Comment,       &  ! Optional output
!                                                RCS_Id        = RCS_Id,        &  ! Revision control
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF EmisCoeff
!                         format data file. Used only for Message output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Angles:         The angle dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       n_Frequencies:    The frequency dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       n_Wind_Speeds:    The wind speed dimension of the emissivity data.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Angle:            Angle dimension values for the emissivity and
!                         derivative data. Size of argument must match the
!                         data file dimensions.
!                         UNITS:      Degrees (from vertical)
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-1 (n_Angles)
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Frequency:        Frequency dimension values for the emissivity and
!                         derivative data. Size of argument must match the
!                         data file dimensions.
!                         UNITS:      Inverse centimetres (cm^-1)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (n_Frequencies)
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Wind_Speed:       Surface wind speed dimension values for the
!                         emissivity and derivative data. Size of argument
!                         must match the data file dimensions.
!                         UNITS:      metres/second (m.s^-1)
!                         TYPE:       REAL( Double )
!                         DIMENSION:  Rank-1 (n_Wind_Speeds)
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Release:          The EmisCoeff data/file release number. Used to check
!                         for data/software mismatch.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Version:          The EmisCoeff data/file version number. Used for
!                         purposes only in identifying the dataset for
!                         a particular release.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF EmisCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:           Character string containing the Revision Control
!                         System Id field for the module.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the ERROR_HANDLER module.
!                         If == SUCCESS the netCDF file inquiry was successful
!                            == FAILURE an error occurred reading any of the requested
!                                       dimension or release/version data.
!                            == WARNING - an error occurred reading any of the requested
!                                         global file attributes, or
!                                       - an error occurred closing the netCDF file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
! CALLS:
!       Open_EmisCoeff_netCDF:   Function to open a netCDF format EmisCoeff
!                                data file.
!
!       Close_EmisCoeff_netCDF:  Function to close a netCDF format EmisCoeff
!                                data file.
!
!       Read_EmisCoeff_GAtts:    Function to read the global attributes from
!                                a netCDF format EmisCoeff data file.
!
!       Get_netCDF_Dimension:    Function to return a dimension value from
!                                a netCDF file given the dimension name.
!                                SOURCE: NETCDF_UTILITY module
!                                
!       Get_netCDF_Variable:     Function to return a variable from a
!                                netCDF file given the variable name.
!                                SOURCE: NETCDF_UTILITY module
!
!       NF90_CLOSE:              Function to close a netCDF file.
!                                SOURCE: netCDF library
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       If a FAILURE error occurs, the netCDF EmisCoeff file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Inquire_EmisCoeff_netCDF( NC_Filename,    &  ! Input
                                     n_Angles,       &  ! Optional output
                                     n_Frequencies,  &  ! Optional output
                                     n_Wind_Speeds,  &  ! Optional output
                                     Angle,          &  ! Optional output
                                     Frequency,      &  ! Optional output
                                     Wind_Speed,     &  ! Optional output
                                     Release,        &  ! Optional output
                                     Version,        &  ! Optional output
                                     Title,          &  ! Optional output
                                     History,        &  ! Optional output
                                     Comment,        &  ! Optional output
                                     RCS_Id,         &  ! Revision control
                                     Message_Log )   &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                          INTENT( IN )  :: NC_Filename

    ! -- Optional output
    INTEGER,         OPTIONAL,               INTENT( OUT ) :: n_Angles
    INTEGER,         OPTIONAL,               INTENT( OUT ) :: n_Frequencies
    INTEGER,         OPTIONAL,               INTENT( OUT ) :: n_Wind_Speeds
    REAL( fp_kind ), OPTIONAL, DIMENSION(:), INTENT( OUT ) :: Angle
    REAL( fp_kind ), OPTIONAL, DIMENSION(:), INTENT( OUT ) :: Frequency
    REAL( fp_kind ), OPTIONAL, DIMENSION(:), INTENT( OUT ) :: Wind_Speed
    INTEGER,         OPTIONAL,               INTENT( OUT ) :: Release
    INTEGER,         OPTIONAL,               INTENT( OUT ) :: Version
    CHARACTER( * ),  OPTIONAL,               INTENT( OUT ) :: Title
    CHARACTER( * ),  OPTIONAL,               INTENT( OUT ) :: History
    CHARACTER( * ),  OPTIONAL,               INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,               INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),  OPTIONAL,               INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_EmisCoeff_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: Data_Type
    INTEGER :: l, n, i



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

    Error_Status = Open_EmisCoeff_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF EmisCoeff data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CHECK THE DATA TYPE --                        #
    !#--------------------------------------------------------------------------#

    ! -- Get it
    Error_Status = Get_netCDF_Variable( NC_fileID, &
                                        DATA_TYPE_VARNAME, &
                                        Data_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//DATA_TYPE_VARNAME//&
                            ' data from '//TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF

    ! -- Check it
    IF ( Data_Type /= SPECTRAL_EMISCOEFF_TYPE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid EmisCoeff file data type in '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- GET THE DIMENSIONS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! The number of angles
    ! --------------------

    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ANGLE_DIMNAME, &
                                         i, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//ANGLE_DIMNAME//' dimension from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    IF ( PRESENT( n_Angles ) ) n_Angles = i


    ! -------------------------
    ! The number of frequencies
    ! -------------------------

    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         FREQUENCY_DIMNAME, &
                                         l, &
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

    IF ( PRESENT( n_Frequencies ) ) n_Frequencies = l


    ! -------------------------
    ! The number of wind speeds
    ! -------------------------

    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         WIND_SPEED_DIMNAME, &
                                         n, &
                                         Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining '//WIND_SPEED_DIMNAME//' dimension from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    IF ( PRESENT( n_Wind_Speeds ) ) n_Wind_Speeds = n



    !#--------------------------------------------------------------------------#
    !#                    -- GET THE DIMENSION VALUE DATA --                    #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The angle
    ! ---------

    IF ( PRESENT( Angle ) ) THEN

      ! -- Check the argument size
      IF ( SIZE( Angle ) /= i ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'ANGLE size different from netCDF file dimension.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          ANGLE_VARNAME, &
                                          Angle )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//ANGLE_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF


    ! ------------------
    ! The frequency grid
    ! ------------------

    IF ( PRESENT( Frequency ) ) THEN

      ! -- Check the argument size
      IF ( SIZE( Frequency ) /= l ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'FREQUENCY argument size different from netCDF file dimension.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          FREQUENCY_VARNAME, &
                                          Frequency )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//FREQUENCY_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF


    ! --------------
    ! The wind speed
    ! --------------

    IF ( PRESENT( Wind_Speed ) ) THEN

      ! -- Check the argument size
      IF ( SIZE( Wind_Speed ) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'WIND_SPEED size different from netCDF file dimension.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      ! -- Get the data
      Error_Status = Get_netCDF_Variable( NC_fileID, &
                                          WIND_SPEED_VARNAME, &
                                          Wind_Speed )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//WIND_SPEED_VARNAME//&
                              ' data from '//TRIM( NC_fileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_fileID )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                -- GET THE RELEASE/VERSION INFORMATION --                 #
    !#--------------------------------------------------------------------------#

    ! ------------
    ! File release
    ! ------------

    IF ( PRESENT( Release ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          RELEASE_VARNAME, &
                                          Release, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//RELEASE_VARNAME//' value from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ------------
    ! File version
    ! ------------

    IF ( PRESENT( Version ) ) THEN
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          VERSION_VARNAME, &
                                          Version, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//VERSION_VARNAME//' value from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE GLOBAL ATTRIBUTES --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_EmisCoeff_GAtts( TRIM( NC_Filename ), &
                                        NC_FileID, &
                                        Title       = Title, &
                                        History     = History, &
                                        Comment     = Comment, &
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

    Close_Status = Close_EmisCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_EmisCoeff_netCDF





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to write Spectral EmisCoeff data to a netCDF file.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                              EmisCoeff,                     &  ! Input
!                                              Title         = Title,         &  ! Optional input
!                                              History       = History,       &  ! Optional input
!                                              Comment       = Comment,       &  ! Optional input
!                                              Quiet         = Quiet,         &  ! Optional input
!                                              RCS_Id        = RCS_Id,        &  ! Revision control
!                                              Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the output
!                        netCDF EmisCoeff format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       EmisCoeff:       Structure to write to file.
!                        UNITS:      N/A
!                        TYPE:       EmisCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF EmisCoeff file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
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
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input EmisCoeff structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Associated_EmisCoeff:       Function to test the association status
!                                   of the pointer members of a EmisCoeff
!                                   structure.
!                                   SOURCE: EMISCOEFF_DEFINE module
!
!       Check_EmisCoeff_Release:    Function to check the Release value of
!                                   the EmisCoeff data.
!
!       Create_EmisCoeff_netCDF:    Function to create a netCDF format 
!                                   EmisCoeff file for writing.
!
!       Put_netCDF_Variable:        Function to write variable data to a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       If the output file exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Write_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                   EmisCoeff,     &  ! Input
                                   Title,         &  ! Optional input
                                   History,       &  ! Optional input
                                   Comment,       &  ! Optional input
                                   Quiet,         &  ! Optional input
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
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename
    TYPE( EmisCoeff_type ),   INTENT( IN )  :: EmisCoeff

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_EmisCoeff_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_EmisCoeff( EmisCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT EmisCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE EmisCoeff DATA TYPE --                    #
    !#--------------------------------------------------------------------------#

    IF ( EmisCoeff%Data_Type /= SPECTRAL_EMISCOEFF_TYPE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid EmisCoeff structure data type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK THE EmisCoeff STRUCTURE RELEASE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'EmisCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CREATE THE OUTPUT DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Create_EmisCoeff_netCDF( NC_Filename, &
                                            EmisCoeff%n_Angles, &
                                            EmisCoeff%n_Frequencies, &
                                            EmisCoeff%n_Wind_Speeds, &
                                            NC_FileID, &
                                            Title         = Title, &
                                            History       = History, &
                                            Comment       = Comment, &
                                            Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating output netCDF EmisCoeff file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! The Release and Version number
    ! ------------------------------

    ! -- Release
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RELEASE_VARNAME, &
                                        EmisCoeff%Release )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RELEASE_VARNAME//' number to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Version
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        VERSION_VARNAME, &
                                        EmisCoeff%Version )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//VERSION_VARNAME//' number to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------
    ! The EmisCoeff data type
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DATA_TYPE_VARNAME, &
                                        EmisCoeff%Data_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATA_TYPE_VARNAME//' number to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------
    ! The angles
    ! ----------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ANGLE_VARNAME, &
                                        EmisCoeff%Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ANGLE_VARNAME//' to '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The frequencies
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        EmisCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//' to '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The wind speeds
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WIND_SPEED_VARNAME, &
                                        EmisCoeff%Wind_Speed )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WIND_SPEED_VARNAME//' to '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! The emissivity data
    ! -------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        EMISSIVITY_VARNAME, &
                                        EmisCoeff%Emissivity )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//EMISSIVITY_VARNAME//' to '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_EmisCoeff_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_EmisCoeff( EmisCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_EmisCoeff_netCDF





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Read_EmisCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format Spectral EmisCoeff file.
!
! CATEGORY:
!       CRTM : Coefficients : EmisCoeff : Spectral Emissivity Model
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisCoeff_netCDF( NC_Filename,                   &  ! Input
!                                             EmisCoeff,                     &  ! Output
!                                             Quiet         = Quiet,         &  ! Optional input
!                                             Title         = Title,         &  ! Optional output
!                                             History       = History,       &  ! Optional output
!                                             Comment       = Comment,       &  ! Optional output
!                                             RCS_Id        = RCS_Id,        &  ! Revision control
!                                             Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF EmisCoeff
!                        format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       EmisCoeff:        Structure to contain the transmittance coefficient
!                        data read from the file.
!                        UNITS:      N/A
!                        TYPE:       EmisCoeff_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF EmisCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE a read error occurred.
!                           == WARNING an error occurred closing the netCDF file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Open_EmisCoeff_netCDF:      Function to open a netCDF format EmisCoeff
!                                   data file.
!
!       Inquire_EmisCoeff_netCDF:   Function to inquire a netCDF format 
!                                   EmisCoeff file to obtain information
!                                   about the data dimensions and attributes.
!
!       Check_EmisCoeff_Release:    Function to check the Release value of
!                                   the EmisCoeff data.
!
!       Close_EmisCoeff_netCDF:     Function to close a netCDF format EmisCoeff
!                                   data file with error checking.
!
!       Allocate_EmisCoeff:         Function to allocate the pointer members
!                                   of a EmisCoeff structure.
!                                   SOURCE: EMISCOEFF_DEFINE module
!
!       Get_netCDF_Variable:        Function to read variable data from a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Count_EmisCoeff_Sensors:    Subroutine to count the number of different
!                                   sensors represented in an EmisCoeff data
!                                   structure.
!                                   SOURCE: EMISCOEFF_DEFINE module
!
!       Version_EmisCoeff:          Subroutine to construct a version info
!                                   Message for output.
!                                   SOURCE: EMISCOEFF_DEFINE module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output EmisCoeff argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Read_EmisCoeff_netCDF( NC_Filename,   &  ! Input
                                  EmisCoeff,     &  ! Output
                                  Quiet,         &  ! Optional input
                                  Title,         &  ! Optional output
                                  History,       &  ! Optional output
                                  Comment,       &  ! Optional output
                                  RCS_Id,        &  ! Revision control
                                  Message_Log )  &  ! Error messaging
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
    TYPE( EmisCoeff_type ),   INTENT( IN OUT ) :: EmisCoeff

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Comment

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_EmisCoeff_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Frequencies
    INTEGER :: n_Wind_Speeds
    INTEGER :: n_Angles
    INTEGER :: Release
    INTEGER :: Version
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- CHECK THE EmisCoeff DATA TYPE --                    #
    !#--------------------------------------------------------------------------#

    IF ( EmisCoeff%Data_Type /= SPECTRAL_EMISCOEFF_TYPE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid EmisCoeff structure data type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- GET THE DIMENSIONS AND ATTRIBUTES --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = Inquire_EmisCoeff_netCDF( TRIM( NC_Filename ), &
                                             n_Angles      = n_Angles, &
                                             n_Frequencies = n_Frequencies, &
                                             n_Wind_Speeds = n_Wind_Speeds, &
                                             Release       = Release, &
                                             Version       = Version, &
                                             Title         = Title, &
                                             History       = History, &
                                             Comment       = Comment, &
                                             Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining EmisCoeff dimensions/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- ALLOCATE THE EmisCoeff STRUCTURE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_EmisCoeff( n_Angles, &
                                       n_Frequencies, &
                                       n_Wind_Speeds, &
                                       EmisCoeff, &
                                       Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating EmisCoeff structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- CHECK THE DATA RELEASE --                       #
    !#--------------------------------------------------------------------------#

    EmisCoeff%Release = Release
    EmisCoeff%Version = Version

    Error_Status = Check_EmisCoeff_Release( EmisCoeff, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'EmisCoeff Release check failed for '//TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_EmisCoeff_netCDF( TRIM( NC_FileNAME ), &
                                          NC_FileID, &
                                          Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF EmisCoeff data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE EmisCoeff DATA --                      #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! The angle
    ! ---------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ANGLE_VARNAME, &
                                        EmisCoeff%Angle )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//ANGLE_VARNAME//' from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! The frequency
    ! -------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        EmisCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FREQUENCY_VARNAME//' from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! The wind speed
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WIND_SPEED_VARNAME, &
                                        EmisCoeff%WInd_Speed )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WIND_SPEED_VARNAME//' from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! The emissivity data
    ! -------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        EMISSIVITY_VARNAME, &
                                        EmisCoeff%Emissivity )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//EMISSIVITY_VARNAME//' from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Close_EmisCoeff_netCDF( NC_FileID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF EmisCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_EmisCoeff( EmisCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_EmisCoeff_netCDF

END MODULE EmisCoeff_netCDF_IO


!---------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!---------------------------------------------------------------------------------
!
! $Id: EmisCoeff_netCDF_IO.f90,v 2.1 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 2.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: EmisCoeff_netCDF_IO.f90,v $
! Revision 2.1  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.0  2005/07/19 15:12:55  paulv
! - Update to Release 2.0. Emissivity derivative is no longer in the structure
!   and the dimension order of the emissivity data is altered to relfect the
!   order of calculation in the CRTM.
!
! Revision 1.2  2005/06/20 21:27:24  paulv
! - Changed category to CRTM Coefficients base.
! - Updated to relfect changes in EmisCoeff_Define module.
!
! Revision 1.1  2005/06/20 15:40:33  paulv
! Initial checkin.
!
!
!
!
