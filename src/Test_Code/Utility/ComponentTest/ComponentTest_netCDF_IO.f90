!------------------------------------------------------------------------------
!M+
! NAME:
!       ComponentTest_netCDF_IO
!
! PURPOSE:
!       Module containing routines to read and write netCDF format
!       ComponentTest files.
!       
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ComponentTest_netCDF_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       ComponentTest_Define:  Module defining the ComponentTest data structure
!                              and containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
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
! CONTAINS:
!       Inquire_ComponentTest_netCDF:  Function to inquire a netCDF format 
!                                   ComponentTest file to obtain information
!                                   about the data dimensions and attributes.
!
!       Write_ComponentTest_netCDF:    Function to write ComponentTest data to a
!                                   netCDF format ComponentTest file.
!
!       Read_ComponentTest_netCDF:     Function to read ComponentTest data from a
!                                   netCDF format ComponentTest file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2006 Paul van Delst
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

MODULE ComponentTest_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE ComponentTest_Define

  USE netcdf
  USE netCDF_Utility,  Open_ComponentTest_netCDF =>  Open_netCDF, &
                      Close_ComponentTest_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! -----------------------------
  ! Everything private by default
  ! -----------------------------

  PRIVATE


  ! -------------------------
  ! Inherited public entities
  ! -------------------------

  ! -- Parameters inherited from the 
  ! -- ComponentTest_Define module
  PUBLIC :: COMPONENTTEST_TESTTYPE    ! All the test types
  PUBLIC :: COMPONENTTEST_FWDTL_TESTTYPE
  PUBLIC :: COMPONENTTEST_TLAD_TESTTYPE
  PUBLIC :: COMPONENTTEST_ADK_TESTTYPE

  PUBLIC :: COMPONENTTEST_DATATYPE ! All the data types
  PUBLIC :: COMPONENTTEST_POLY_DATATYPE
  PUBLIC :: COMPONENTTEST_MONO_DATATYPE

  ! -- ComponentTest structure data type definition
  ! -- inherited from the ComponentTest_Define module
  PUBLIC :: ComponentTest_type

  ! -- ComponentTest structure routines inherited
  ! -- from the ComponentTest_Define module
  PUBLIC :: Associated_ComponentTest
  PUBLIC :: Destroy_ComponentTest
  PUBLIC :: Allocate_ComponentTest


  ! -------------------------------------
  ! Public entities in the current module
  ! -------------------------------------

  ! -- Routines in this module
  PUBLIC :: Inquire_ComponentTest_netCDF
  PUBLIC :: Read_ComponentTest_netCDF
  PUBLIC :: Write_ComponentTest_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: ComponentTest_netCDF_IO.f90,v 1.3 2006/05/02 14:58:35 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME       = 'comment' 
  CHARACTER( * ), PRIVATE, PARAMETER :: ID_TAG_GATTNAME        = 'id_tag' 
  CHARACTER( * ), PRIVATE, PARAMETER :: TESTTYPE_GATTNAME      = 'TestType'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATATYPE_GATTNAME      = 'DataType'

  ! -- Dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DIMNAME           = 'n_Layers'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLY_DATATYPE_DIMNAME   = 'n_Channels'
  CHARACTER( * ), PRIVATE, PARAMETER :: MONO_DATATYPE_DIMNAME   = 'n_Frequencies'
  CHARACTER( * ), PRIVATE, PARAMETER :: PERTURBATION_DIMNAME    = 'n_Perturbations'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_VARIABLE_DIMNAME  = 'n_Input_Variables'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_VARIABLE_DIMNAME = 'n_Output_Variables'
  CHARACTER( * ), PRIVATE, PARAMETER :: STRLEN_DIMNAME          = 'StrLen'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATASET_DIMNAME         = 'n_DataSets'

  ! -- Variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: PRESSURE_VARNAME        = 'Pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLY_DATATYPE_VARNAME   = 'Channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: MONO_DATATYPE_VARNAME   = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: PERTURBATION_VARNAME    = 'Perturbation'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_VARIABLE_VARNAME  = 'Input_Variable_Name'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_UNITS_VARNAME     = 'Input_Variable_Units'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_VARIABLE_VARNAME = 'Output_Variable_Name'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_UNITS_VARNAME    = 'Output_Variable_Units'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_FWDTL_VARNAME        = 'd_NL'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_FWDTL_VARNAME        = 'd_TL'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_TLAD_VARNAME         = 'd_TL'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_TLAD_VARNAME         = 'd_AD'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_ADK_VARNAME          = 'd_AD'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_ADK_VARNAME          = 'd_K'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATASET_NAME_VARNAME    = 'DataSet_Name'

  ! -- Variable description attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER( * ), PRIVATE, PARAMETER :: PRESSURE_DESCRIPTION = &
'Layer pressures used in the Component Test.'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLY_DATATYPE_DESCRIPTION = &
'Sensor channels used in the Component Test.'
  CHARACTER( * ), PRIVATE, PARAMETER :: MONO_DATATYPE_DESCRIPTION = &
'Frequencies used in the Component Test.'
  CHARACTER( * ), PRIVATE, PARAMETER :: PERTURBATION_DESCRIPTION = &
'List of the perturbation fractions applied to the variables in the Component Test.'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_VARIABLE_DESCRIPTION = &
'List of the input variable names in the Component Test'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_UNITS_DESCRIPTION = &
'List of the input variable units in the Component Test'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_VARIABLE_DESCRIPTION = &
'List of the output variable names in the Component Test'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_UNITS_DESCRIPTION = &
'List of the output variable units in the Component Test'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_FWDTL_DESCRIPTION = &
'The FWD model non-linear difference for the given input variable perturbation.'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_FWDTL_DESCRIPTION = &
'The TL model values for the given input variable perturbation.'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_TLAD_DESCRIPTION = &
'The TL model values for unity perturbation of the TL variables'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_TLAD_DESCRIPTION = &
'The AD model values for unity perturbation of the AD variables'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_ADK_DESCRIPTION = &
'The AD model values for unity perturbation of the AD variables'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_ADK_DESCRIPTION = &
'The K-matrix model values for unity perturbation of the AD variables'

  CHARACTER( * ), PRIVATE, PARAMETER :: DATASET_NAME_DESCRIPTION = &
'List of descriptions for each dataset in the file'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: PRESSURE_LONGNAME        = 'Pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLY_DATATYPE_LONGNAME   = 'Sensor Channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: MONO_DATATYPE_LONGNAME   = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: PERTURBATION_LONGNAME    = 'Perturbation fraction'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_VARIABLE_LONGNAME  = 'Input Variable Name'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_VARIABLE_LONGNAME = 'Output Variable Name'
  CHARACTER( * ), PRIVATE, PARAMETER :: INPUT_UNITS_LONGNAME     = 'Input Variable Units'
  CHARACTER( * ), PRIVATE, PARAMETER :: OUTPUT_UNITS_LONGNAME    = 'Output Variable Units'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_FWDTL_LONGNAME        = 'd(NL)'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_FWDTL_LONGNAME        = 'd(TL)'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_TLAD_LONGNAME         = 'd(TL)'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_TLAD_LONGNAME         = 'd(AD)'
  CHARACTER( * ), PRIVATE, PARAMETER :: D1_ADK_LONGNAME          = 'd(AD)'
  CHARACTER( * ), PRIVATE, PARAMETER :: D2_ADK_LONGNAME          = 'd(K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATASET_NAME_LONGNAME    = 'DataSet Name'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: PRESSURE_UNITS      = 'hPa'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLY_DATATYPE_UNITS = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: MONO_DATATYPE_UNITS = 'cm^-1 or GHz'
  CHARACTER( * ), PRIVATE, PARAMETER :: PERTURBATION_UNITS  = 'Variable. Input dependent.'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER,        PRIVATE, PARAMETER :: IP_FILLVALUE = -1
  REAL( Double ), PRIVATE, PARAMETER :: FP_FILLVALUE = -999.0_Double

  REAL( Double ), PRIVATE, PARAMETER :: PRESSURE_FILLVALUE     = FP_FILLVALUE
  REAL( Double ), PRIVATE, PARAMETER :: SPECTRAL_FILLVALUE     = FP_FILLVALUE
  REAL( Double ), PRIVATE, PARAMETER :: PERTURBATION_FILLVALUE = FP_FILLVALUE
  REAL( Double ), PRIVATE, PARAMETER :: D1_FILLVALUE           = FP_FILLVALUE
  REAL( Double ), PRIVATE, PARAMETER :: D2_FILLVALUE           = FP_FILLVALUE

  ! -- Variable netCDF datatypes
  INTEGER, PRIVATE, PARAMETER :: PRESSURE_TYPE        = NF90_DOUBLE 
  INTEGER, PRIVATE, PARAMETER :: SPECTRAL_TYPE        = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: PERTURBATION_TYPE    = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: INPUT_VARIABLE_TYPE  = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: OUTPUT_VARIABLE_TYPE = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: INPUT_UNITS_TYPE     = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: OUTPUT_UNITS_TYPE    = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: D1_TYPE              = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: D2_TYPE              = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: DATASET_NAME_TYPE    = NF90_CHAR


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
!       Write_ComponentTest_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF
!       ComponentTest data file.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_ComponentTest_GAtts( NC_Filename, &  ! Input
!                                                 NC_FileID,   &  ! Input
!                                                 TestType,    &  ! Input
!                                                 DataType,    &  ! Input
!                                                 Title         = Title,         &  ! Optional input
!                                                 History       = History,       &  ! Optional input
!                                                 Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                                 Platform_Name = Platform_Name, &  ! Optional input
!                                                 Comment       = Comment,       &  ! Optional input
!                                                 ID_Tag        = ID_Tag,        &  ! Optional input
!                                                 Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ComponentTest format data file to write to.
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
!       TestType:         Integer flag indicating whether the test type is
!                         FWD/TL, TL/AD, or AD/K.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       DataType:         Integer flag indicating whether the spectral dimension
!                         is polychromatic or monochromatic.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF ComponentTest
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF ComponentTest
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a short tag used to identify the
!                         data set.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_ComponentTest_GAtts( NC_Filename,   &  ! Input
                                      NC_FileID,     &  ! Input
                                      TestType,      &  ! Input
                                      DataType,      &  ! Input
                                      Title,         &  ! Optional input
                                      History,       &  ! Optional input
                                      Sensor_Name,   &  ! Optional input
                                      Platform_Name, &  ! Optional input
                                      Comment,       &  ! Optional input
                                      ID_Tag,        &  ! Optional input
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
    INTEGER,                  INTENT( IN ) :: TestType
    INTEGER,                  INTENT( IN ) :: DataType

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: ID_Tag

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ComponentTest_GAtts'

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
                              TRIM( NC_FileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! -----------
    ! Data ID tag
    ! -----------

    IF ( PRESENT( ID_Tag ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  ID_TAG_GATTNAME, &
                                  TRIM( ID_Tag ) )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//ID_TAG_GATTNAME//' attribute to '//&
                              TRIM( NC_FileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! ---------
    ! Test type
    ! ---------

    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TESTTYPE_GATTNAME, &
                                TestType )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TESTTYPE_GATTNAME//' attribute to '//&
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------
    ! Data type
    ! ---------

    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                DATATYPE_GATTNAME, &
                                DataType )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATATYPE_GATTNAME//' attribute to '//&
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_ComponentTest_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Read_ComponentTest_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF ComponentTest data file.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_ComponentTest_GAtts( NC_Filename,                   &  ! Input
!                                                NC_FileID,                     &  ! Input
!                                                Title         = Title,         &  ! Optional output
!                                                History       = History,       &  ! Optional output
!                                                Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                                Platform_Name = Platform_Name, &  ! Optional output
!                                                Comment       = Comment,       &  ! Optional output
!                                                ID_Tag        = ID_Tag,        &  ! Optional output
!                                                TestType      = TestType,      &  ! Optional output
!                                                DataType      = DataType,      &  ! Optional output
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ComponentTest format data file to read.
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
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF ComponentTest
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF ComponentTest
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ComponentTest file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF ComponentTest file.
!                         Should contain a short tag used to identify the
!                         data set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       TestType:         Integer flag indicating whether the test type is
!                         FWD/TL, TL/AD, or AD/K.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       DataType:         Integer flag indicating whether the spectral dimension
!                         is polychromatic or monochromatic.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT ), OPTIONAL
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
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_ComponentTest_GAtts( NC_Filename,   &  ! Input
                                     NC_FileID,     &  ! Input
                                     Title,         &  ! Optional output
                                     History,       &  ! Optional output
                                     Sensor_Name,   &  ! Optional output
                                     Platform_Name, &  ! Optional output
                                     Comment,       &  ! Optional output
                                     ID_Tag,        &  ! Optional output
                                     TestType,      &  ! Optional output
                                     DataType,      &  ! Optional output
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
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: ID_Tag
    INTEGER,        OPTIONAL, INTENT( OUT ) :: TestType
    INTEGER,        OPTIONAL, INTENT( OUT ) :: DataType

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ComponentTest_GAtts'


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


    ! ---------------
    ! The SENSOR_NAME
    ! ---------------

    IF ( PRESENT( Sensor_Name ) ) THEN

      Sensor_Name = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           SENSOR_NAME_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//SENSOR_NAME_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Sensor_Name = Long_String(1:MIN( LEN( Sensor_Name ), LEN_TRIM( Long_String ) ))

    END IF


    ! -----------------
    ! The PLATFORM_NAME
    ! -----------------

    IF ( PRESENT( Platform_Name ) ) THEN

      Platform_Name = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           PLATFORM_NAME_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//PLATFORM_NAME_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      Platform_Name = Long_String(1:MIN( LEN( Platform_Name ), LEN_TRIM( Long_String ) ))

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


    ! -----------
    ! The ID_TAG
    ! -----------

    IF ( PRESENT( ID_Tag ) ) THEN

      ID_Tag = ' '
      Long_String = ' '

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           ID_TAG_GATTNAME, &
                                           Long_String, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//ID_TAG_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

      CALL Remove_NULL_Characters( Long_String )

      ID_Tag = Long_String(1:MIN( LEN( ID_Tag ), LEN_TRIM( Long_String ) ))

    END IF


    ! ------------
    ! The TestType
    ! ------------

    IF ( PRESENT( TestType ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           TESTTYPE_GATTNAME, &
                                           TestType, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//TESTTYPE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF


    ! ------------
    ! The DataType
    ! ------------

    IF ( PRESENT( DataType ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           DATATYPE_GATTNAME, &
                                           DataType, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//DATATYPE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Read_ComponentTest_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Create_ComponentTest_netCDF
!
! PURPOSE:
!       Function to create a netCDF format ComponentTest file for writing
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!         Error_Status = Create_ComponentTest_netCDF( NC_Filename,                   &  ! Input
!                                                     nK,                            &  ! Input
!                                                     nL,                            &  ! Input
!                                                     nP,                            &  ! Input
!                                                     nIV,                           &  ! Input
!                                                     nOV,                           &  ! Input
!                                                     TestType,                      &  ! Input
!                                                     DataType,                      &  ! Input
!                                                     NC_FileID,                     &  ! Output
!                                                     Title         = Title,         &  ! Optional input
!                                                     History       = History,       &  ! Optional input
!                                                     Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                                     Platform_Name = Platform_Name, &  ! Optional input
!                                                     Comment       = Comment,       &  ! Optional input
!                                                     ID_Tag        = ID_Tag,        &  ! Optional input
!                                                     Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:           Character string specifying the name of the
!                              netCDF format ComponentTest data file to create.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       nK:                    The number of layers dimension of the
!                              ComponentTest data. For surface component tests,
!                              should be set to 1.
!                              Must be > 0.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       nL:                    The spectral dimension (channels/frequencies) of
!                              the ComponentTest data.
!                              Must be > 0.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       nP:                    The number of perturbations dimension of the
!                              ComponentTest data. For non-FWD/TL component tests,
!                              should be set to 1.
!                              Must be > 0.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       nIV:                   The number of input variables dimension of the
!                              ComponentTest data.
!                              Must be > 0.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       nOV:                   The number of output variables dimension of the
!                              ComponentTest data.
!                              Must be > 0.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       TestType:              Integer flag indicating whether the test type is
!                              FWD/TL, TL/AD, or AD/K.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
!       DataType:              Integer flag indicating whether the spectral dimension
!                              is polychromatic or monochromatic.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:                 Character string written into the TITLE global
!                              attribute field of the netCDF ComponentTest file.
!                              Should contain a succinct description of what
!                              is in the netCDF datafile.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:               Character string written into the HISTORY global
!                              attribute field of the netCDF ComponentTest file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:           Character string written into the SENSOR_NAME
!                              global attribute field of the netCDF ComponentTest
!                              file.
!                              ** Only written for SENSOR based data type **
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:         Character string written into the PLATFORM_NAME
!                              global attribute field of the netCDF ComponentTest
!                              file.
!                              ** Only written for SENSOR based data type **
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:               Character string written into the COMMENT global
!                              attribute field of the netCDF ComponentTest file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       ID_Tag:                Character string written into the ID_TAG global
!                              attribute field of the netCDF ComponentTest file.
!                              Should contain a short tag used to identify the
!                              profile set.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:           Character string specifying a filename in which any
!                              messages will be logged. If not specified, or if an
!                              error occurs opening the log file, the default action
!                              is to output messages to standard output.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       NC_FileID:             NetCDF file ID number.
!                              function.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error status.
!                              The error codes are defined in the ERROR_HANDLER module.
!                              If == SUCCESS the netCDF file creation was successful
!                                 == FAILURE an unrecoverable error occurred.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
! CALLS:
!       Associated_ComponentTest:   Function to test the association status
!                                   of the pointer members of a ComponentTest
!                                   structure.
!                                   SOURCE: TLMTEST_DEFINE module
!
!       Write_ComponentTest_GAtts:  Function to write the global attributes
!                                   to the netCDF formnat ComponentTest file.
!
!       Put_netCDF_Attribute:       Function to write attribute data to a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_ATTRIBUTE_UTILITY module
!
!       Put_netCDF_Variable:        Function to write variable data to a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!       NF90_CREATE:                Function to create a netCDF file.
!                                   SOURCE: netCDF library
!
!       NF90_DEF_DIM:               Function to define a dimension in
!                                   a netCDF dataset.
!                                   SOURCE: netCDF library
!
!       NF90_DEF_VAR:               Function to define a variable in
!                                   a netCDF dataset.
!                                   SOURCE: netCDF library
!
!       NF90_ENDDEF:                Function to put a netCDF dataset into
!                                   data mode.
!                                   SOURCE: netCDF library
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Upon exit from this routine, the netCDF file is in DATA mode.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Create_ComponentTest_netCDF( NC_Filename,           &  ! Input
                                        nK,                    &  ! Input
                                        nL,                    &  ! Input
                                        nP,                    &  ! Input
                                        nIV,                   &  ! Input
                                        nOV,                   &  ! Input
                                        TestType,              &  ! Input
                                        DataType,              &  ! Input
                                        NC_FileID,             &  ! Output
                                        Title,                 &  ! Optional input
                                        History,               &  ! Optional input
                                        Sensor_Name,           &  ! Optional input
                                        Platform_Name,         &  ! Optional input
                                        Comment,               &  ! Optional input
                                        ID_Tag,                &  ! Optional input
                                        Message_Log )          &  ! Error messaging
                                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER(*),               INTENT(IN)  :: NC_Filename
    INTEGER,                    INTENT(IN)  :: nK
    INTEGER,                    INTENT(IN)  :: nL
    INTEGER,                    INTENT(IN)  :: nP
    INTEGER,                    INTENT(IN)  :: nIV
    INTEGER,                    INTENT(IN)  :: nOV
    INTEGER,                    INTENT(IN)  :: TestType
    INTEGER,                    INTENT(IN)  :: DataType
 
    ! -- Output
    INTEGER,                    INTENT(OUT) :: NC_FileID

    ! -- Optional input
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: Title         
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: History       
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: Sensor_Name   
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: Platform_Name 
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: Comment       
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: ID_Tag        

    ! -- Error handler Message log
    CHARACTER(*), OPTIONAL,     INTENT(IN)  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_ComponentTest_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: Description_Status
    INTEGER :: LongName_Status
    INTEGER :: Units_Status
    INTEGER :: FillValue_Status

    INTEGER :: nK_DimID
    INTEGER :: nL_DimID
    INTEGER :: nP_DimID
    INTEGER :: nIV_DimID
    INTEGER :: nOV_DimID
    INTEGER :: StrLen_DimID
    INTEGER :: nM_DimID
    INTEGER :: varID

    CHARACTER( 256 ) :: d1_VarName    
    CHARACTER( 256 ) :: d1_Description
    CHARACTER( 256 ) :: d1_LongName   
    CHARACTER( 256 ) :: d2_VarName    
    CHARACTER( 256 ) :: d2_Description
    CHARACTER( 256 ) :: d2_LongName   

    CHARACTER( 256 ) :: Spectral_DimName
    CHARACTER( 256 ) :: Spectral_VarName
    CHARACTER( 256 ) :: Spectral_Description
    CHARACTER( 256 ) :: Spectral_LongName
    CHARACTER( 256 ) :: Spectral_Units

    TYPE( ComponentTest_type ) :: ComponentTest_Dummy



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    IF ( ALL( COMPONENTTEST_TESTTYPE /= TestType ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid TestType', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( ALL( COMPONENTTEST_DATATYPE /= DataType ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid DataType', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#         -- ASSIGN THE TEST- AND DATA-TYPE SPECIFIC ATTRIBUTES --         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! The attributes for the output variables
    ! of the different test types
    ! ---------------------------------------

    SELECT CASE ( TestType )

      CASE ( COMPONENTTEST_FWDTL_TESTTYPE )
        d1_VarName     = D1_FWDTL_VARNAME
        d1_Description = D1_FWDTL_DESCRIPTION
        d1_LongName    = D1_FWDTL_LONGNAME
        d2_VarName     = D2_FWDTL_VARNAME
        d2_Description = D2_FWDTL_DESCRIPTION
        d2_LongName    = D2_FWDTL_LONGNAME

      CASE ( COMPONENTTEST_TLAD_TESTTYPE )
        d1_VarName     = D1_TLAD_VARNAME
        d1_Description = D1_TLAD_DESCRIPTION
        d1_LongName    = D1_TLAD_LONGNAME
        d2_VarName     = D2_TLAD_VARNAME
        d2_Description = D2_TLAD_DESCRIPTION
        d2_LongName    = D2_TLAD_LONGNAME

      CASE ( COMPONENTTEST_ADK_TESTTYPE )
        d1_VarName     = D1_ADK_VARNAME
        d1_Description = D1_ADK_DESCRIPTION
        d1_LongName    = D1_ADK_LONGNAME
        d2_VarName     = D2_ADK_VARNAME
        d2_Description = D2_ADK_DESCRIPTION
        d2_LongName    = D2_ADK_LONGNAME
    END SELECT


    ! ---------------------------------------------------
    ! The attributes for the spectral data type dimension
    ! ---------------------------------------------------

    SELECT CASE ( DataType )

      CASE ( COMPONENTTEST_POLY_DATATYPE )
        Spectral_DimName     = POLY_DATATYPE_DIMNAME
        Spectral_VarName     = POLY_DATATYPE_VARNAME
        Spectral_Description = POLY_DATATYPE_DESCRIPTION
        Spectral_LongName    = POLY_DATATYPE_LONGNAME
        Spectral_Units       = POLY_DATATYPE_UNITS

      CASE ( COMPONENTTEST_MONO_DATATYPE )
        Spectral_DimName     = MONO_DATATYPE_DIMNAME
        Spectral_VarName     = MONO_DATATYPE_VARNAME
        Spectral_Description = MONO_DATATYPE_DESCRIPTION
        Spectral_LongName    = MONO_DATATYPE_LONGNAME
        Spectral_Units       = MONO_DATATYPE_UNITS
    END SELECT



    !#--------------------------------------------------------------------------#
    !#                    -- CREATE THE NETCDF DATA FILE --                     #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CREATE( TRIM( NC_FileNAME ), &
                               NF90_CLOBBER, &
                               NC_FileID )

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

    ! --------------------
    ! The number of layers
    ! --------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LAYER_DIMNAME, &
                                nK, &
                                nK_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//LAYER_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The spectral dimension
    ! ----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                TRIM(Spectral_DimName), &
                                nL, &
                                nL_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM(Spectral_DimName)//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! The number of perturbations
    ! ---------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PERTURBATION_DIMNAME, &
                                nP, &
                                nP_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//PERTURBATION_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The number of input variables
    ! -----------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                INPUT_VARIABLE_DIMNAME, &
                                nIV, &
                                nIV_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//INPUT_VARIABLE_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The number of output variables
    ! ------------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                OUTPUT_VARIABLE_DIMNAME, &
                                nOV, &
                                nOV_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//OUTPUT_VARIABLE_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The string length
    ! -----------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                STRLEN_DIMNAME, &
                                ComponentTest_Dummy%StrLen, &
                                StrLen_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//STRLEN_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------------------------------
    ! The number of datasets. Unlimited dimension.
    ! --------------------------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                DATASET_DIMNAME, &
                                NF90_UNLIMITED, &
                                nM_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//DATASET_DIMNAME//' dimension in '// &
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

    IF ( DataType == COMPONENTTEST_POLY_DATATYPE ) THEN

      Error_Status = Write_ComponentTest_GAtts( TRIM( NC_FileNAME ), &
                                                NC_FileID, &
                                                TestType, &
                                                DataType, &
                                                Title         = Title, &
                                                History       = History, &
                                                Sensor_Name   = Sensor_Name, &
                                                Platform_Name = Platform_Name, &
                                                Comment       = Comment, &
                                                ID_Tag        = ID_Tag, &
                                                Message_Log = Message_Log )

    ELSE

      Error_Status = Write_ComponentTest_GAtts( TRIM( NC_FileNAME ), &
                                                NC_FileID, &
                                                TestType, &
                                                DataType, &
                                                Title   = Title, &
                                                History = History, &
                                                Comment = Comment, &
                                                ID_Tag  = ID_Tag, &
                                                Message_Log = Message_Log )

    END IF

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attribute to '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- DEFINE THE VARIABLES --                       #
    !#--------------------------------------------------------------------------#

    ! --------
    ! Pressure
    ! --------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PRESSURE_VARNAME, &
                                PRESSURE_TYPE, &
                                dimids = nK_DimID, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PRESSURE_VARNAME//' variable in '// &
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
                                               PRESSURE_DESCRIPTION, &
                                               Variable_Name = PRESSURE_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               PRESSURE_LONGNAME, &
                                               Variable_Name = PRESSURE_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               PRESSURE_UNITS, &
                                               Variable_Name = PRESSURE_VARNAME )
    FillValue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               PRESSURE_FILLVALUE, &
                                               Variable_Name = PRESSURE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         FillValue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PRESSURE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------
    ! Spectral dimension data
    ! -----------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRIM(Spectral_VarName), &
                                SPECTRAL_TYPE, &
                                dimids = nL_DimID, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRIM(Spectral_VarName)//' variable in '// &
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
                                               TRIM(Spectral_Description), &
                                               Variable_Name = TRIM(Spectral_VarName) )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               TRIM(Spectral_LongName), &
                                               Variable_Name = TRIM(Spectral_VarName) )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               TRIM(Spectral_Units), &
                                               Variable_Name = TRIM(Spectral_VarName) )
    FillValue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               SPECTRAL_FILLVALUE, &
                                               Variable_Name = TRIM(Spectral_VarName) )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         FillValue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(Spectral_VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! Perturbation array
    ! ------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PERTURBATION_VARNAME, &
                                PERTURBATION_TYPE, &
                                dimids = nP_DimID, &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PERTURBATION_VARNAME//' variable in '// &
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
                                               PERTURBATION_DESCRIPTION, &
                                               Variable_Name = PERTURBATION_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               PERTURBATION_LONGNAME, &
                                               Variable_Name = PERTURBATION_VARNAME )
    Units_Status       = Put_netCDF_Attribute( NC_FileID, &
                                               UNITS_ATTNAME, &
                                               PERTURBATION_UNITS, &
                                               Variable_Name = PERTURBATION_VARNAME )
    FillValue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               PERTURBATION_FILLVALUE, &
                                               Variable_Name = PERTURBATION_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS .OR. &
         Units_Status       /= SUCCESS .OR. &
         FillValue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PERTURBATION_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! Input variable name
    ! -------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                INPUT_VARIABLE_VARNAME, &
                                INPUT_VARIABLE_TYPE, &
                                dimids = (/ StrLen_DimID, nIV_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//INPUT_VARIABLE_VARNAME//' variable in '// &
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
                                               INPUT_VARIABLE_DESCRIPTION, &
                                               Variable_Name = INPUT_VARIABLE_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               INPUT_VARIABLE_LONGNAME, &
                                               Variable_Name = INPUT_VARIABLE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INPUT_VARIABLE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! Input variable units
    ! --------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                INPUT_UNITS_VARNAME, &
                                INPUT_UNITS_TYPE, &
                                dimids = (/ StrLen_DimID, nIV_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//INPUT_UNITS_VARNAME//' variable in '// &
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
                                               INPUT_UNITS_DESCRIPTION, &
                                               Variable_Name = INPUT_UNITS_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               INPUT_UNITS_LONGNAME, &
                                               Variable_Name = INPUT_UNITS_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INPUT_UNITS_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! Output variable name
    ! --------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OUTPUT_VARIABLE_VARNAME, &
                                OUTPUT_VARIABLE_TYPE, &
                                dimids = (/ StrLen_DimID, nOV_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//OUTPUT_VARIABLE_VARNAME//' variable in '// &
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
                                               OUTPUT_VARIABLE_DESCRIPTION, &
                                               Variable_Name = OUTPUT_VARIABLE_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               OUTPUT_VARIABLE_LONGNAME, &
                                               Variable_Name = OUTPUT_VARIABLE_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//OUTPUT_VARIABLE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------
    ! Output variable units
    ! ---------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OUTPUT_UNITS_VARNAME, &
                                OUTPUT_UNITS_TYPE, &
                                dimids = (/ StrLen_DimID, nOV_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//OUTPUT_UNITS_VARNAME//' variable in '// &
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
                                               OUTPUT_UNITS_DESCRIPTION, &
                                               Variable_Name = OUTPUT_UNITS_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               OUTPUT_UNITS_LONGNAME, &
                                               Variable_Name = OUTPUT_UNITS_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//OUTPUT_UNITS_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------
    ! DataSet name
    ! ------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DATASET_NAME_VARNAME, &
                                DATASET_NAME_TYPE, &
                                dimids = (/ StrLen_DimID, nM_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DATASET_NAME_VARNAME//' variable in '// &
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
                                               DATASET_NAME_DESCRIPTION, &
                                               Variable_Name = DATASET_NAME_VARNAME )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               DATASET_NAME_LONGNAME, &
                                               Variable_Name = DATASET_NAME_VARNAME )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATASET_NAME_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D1 data
    ! -------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRIM(d1_VarName), &
                                D1_TYPE, &
                                dimids = (/ nK_DimID, &
                                            nL_DimID, &
                                            nP_DimID, &
                                            nIV_DimID, &
                                            nOV_DimID, &
                                            nM_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRIM(d1_VarName)//' variable in '// &
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
                                               TRIM(d1_Description), &
                                               Variable_Name = TRIM(d1_VarName) )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               TRIM( d1_LongName ), &
                                               Variable_Name = TRIM(d1_VarName) )
    FillValue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               D1_FILLVALUE, &
                                               Variable_Name = TRIM(d1_VarName) )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS .OR. &
         FillValue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(d1_VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D2 data
    ! -------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                TRIM(d2_VarName), &
                                D2_TYPE, &
                                dimids = (/ nK_DimID, &
                                            nL_DimID, &
                                            nP_DimID, &
                                            nIV_DimID, &
                                            nOV_DimID, &
                                            nM_DimID /), &
                                varid = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//TRIM(d2_VarName)//' variable in '// &
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
                                               TRIM(d2_Description), &
                                               Variable_Name = TRIM(d2_VarName) )
    LongName_Status    = Put_netCDF_Attribute( NC_FileID, &
                                               LONGNAME_ATTNAME, &
                                               TRIM( d2_LongName ), &
                                               Variable_Name = TRIM(d2_VarName) )
    FillValue_Status   = Put_netCDF_Attribute( NC_FileID, &
                                               FILLVALUE_ATTNAME, &
                                               D2_FILLVALUE, &
                                               Variable_Name = TRIM(d2_VarName) )

    IF ( Description_Status /= SUCCESS .OR. &
         LongName_Status    /= SUCCESS .OR. &
         FillValue_Status   /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(d2_VarName)//' variable attributes to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- TAKE NETCDF FILE OUT OF DEFINE MODE --                #
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

  END FUNCTION Create_ComponentTest_netCDF





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
!       Inquire_ComponentTest_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF ComponentTest format file to obtain the 
!       dimensions and global attributes.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ComponentTest_netCDF( NC_Filename,                   &  ! Input
!                                                    nK            = nK,            &  ! Optional output
!                                                    nL            = nL,            &  ! Optional output
!                                                    nP            = nP,            &  ! Optional output
!                                                    nIV           = nIV,           &  ! Optional output
!                                                    nOV           = nOV,           &  ! Optional output
!                                                    nM            = nM,            &  ! Optional output
!                                                    TestType      = TestType,      &  ! Optional output
!                                                    DataType      = DataType,      &  ! Optional output
!                                                    Title         = Title,         &  ! Optional output
!                                                    History       = History,       &  ! Optional output
!                                                    Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                                    Platform_Name = Platform_Name, &  ! Optional output
!                                                    Comment       = Comment,       &  ! Optional output
!                                                    ID_Tag        = ID_Tag,        &  ! Optional output
!                                                    RCS_Id        = RCS_Id,        &  ! Version control
!                                                    Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the netCDF
!                         format ComponentTest data file to inquire.
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
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       nK:                   The number of layers dimension of the
!                             ComponentTest data.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       nL:                   The spectral dimension (channels/frequencies) of
!                             the ComponentTest data.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       nP:                   The number of perturbations dimension of the
!                             ComponentTest data.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       nIV:                  The number of input variables dimension of the
!                             ComponentTest data.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       nOV:                  The number of output variables dimension of the
!                             ComponentTest data.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       nM:                   The number of datasets in the ComponentTest datafile.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       TestType:             Integer flag indicating whether the test type is
!                             FWD/TL, TL/AD, or AD/K.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       DataType:             Integer flag indicating whether the spectral dimension
!                             is polychromatic or monochromatic.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:                Character string written into the TITLE global
!                             attribute field of the netCDF ComponentTest file.
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:              Character string written into the HISTORY global
!                             attribute field of the netCDF ComponentTest file.
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Sensor_Name:          Character string written into the SENSOR_NAME global
!                             attribute field of the netCDF ComponentTest file.
!                             ** Only written for POLYCHROMATIC data types **
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Platform_Name:        Character string written into the PLATFORM_NAME global
!                             attribute field of the netCDF ComponentTest file.
!                             ** Only written for POLYCHROMATIC data types **
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:              Character string written into the COMMENT global
!                             attribute field of the netCDF ComponentTest file.
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       ID_Tag:               Character string written into the ID_TAG global
!                             attribute field of the netCDF ComponentTest file.
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:               Character string containing the Revision Control
!                             System Id field for the module.
!                             UNITS:      N/A
!                             TYPE:       CHARACTER( * )
!                             DIMENSION:  Scalar
!                             ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:         The return value is an integer defining the error status.
!                             The error codes are defined in the ERROR_HANDLER module.
!                             If == SUCCESS the netCDF file inquiry was successful
!                                == FAILURE an error occurred reading any of the requested
!                                           dimension or release/version data.
!                                == WARNING - an error occurred reading any of the requested
!                                             global file attributes, or
!                                           - an error occurred closing the netCDF file.
!                             UNITS:      N/A
!                             TYPE:       INTEGER
!                             DIMENSION:  Scalar
!
! CALLS:
!       Open_ComponentTest_netCDF:  Function to open a netCDF format ComponentTest
!                                   data file.
!
!       Read_ComponentTest_GAtts:   Function to read the global attributes from
!                                   a netCDF format ComponentTest data file.
!
!       Close_ComponentTest_netCDF: Function to close a netCDF format ComponentTest
!                                   data file with error checking.
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Get_netCDF_Dimension:       Function to return a dimension value from
!                                   a netCDF file given the dimension name.
!                                   SOURCE: NETCDF_UTILITY module
!                                
!       Get_netCDF_Variable:        Function to return a variable from a
!                                   netCDF file given the variable name.
!                                   SOURCE: NETCDF_UTILITY module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_ComponentTest_netCDF( NC_Filename,   &  ! Input
                                         nK,            &  ! Optional output
                                         nL,            &  ! Optional output
                                         nP,            &  ! Optional output
                                         nIV,           &  ! Optional output
                                         nOV,           &  ! Optional output
                                         nM,            &  ! Optional output
                                         TestType,      &  ! Optional output
                                         DataType,      &  ! Optional output
                                         Title,         &  ! Optional output
                                         History,       &  ! Optional output
                                         Sensor_Name,   &  ! Optional output
                                         Platform_Name, &  ! Optional output
                                         Comment,       &  ! Optional output
                                         ID_Tag,        &  ! Optional output
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
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nK
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nL
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nP
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nIV
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nOV
    INTEGER,        OPTIONAL, INTENT( OUT ) :: nM
    INTEGER,        OPTIONAL, INTENT( OUT ) :: TestType
    INTEGER,        OPTIONAL, INTENT( OUT ) :: DataType
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: ID_Tag

    ! -- Version control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_ComponentTest_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: File_DataType
    INTEGER :: nK_Status

    CHARACTER(256) :: Spectral_DimName



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

    Error_Status = Open_ComponentTest_netCDF( TRIM( NC_FileNAME ), &
                                              NC_FileID, &
                                              Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF ComponentTest data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#            -- GET THE DATATYPE FOR SPECTRAL DIMENSION NAME --            #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Read the data type
    ! ------------------

    Error_Status = Read_ComponentTest_GAtts( TRIM( NC_FileNAME ), &
                                             NC_FileID, &
                                             DataType = File_DataType, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading DataType global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------------------------------
    ! Assign the appropriate spectral dimension name
    ! ----------------------------------------------

    SELECT CASE ( File_DataType )
      CASE ( COMPONENTTEST_POLY_DATATYPE )
        Spectral_DimName = POLY_DATATYPE_DIMNAME
      CASE ( COMPONENTTEST_MONO_DATATYPE )
        Spectral_DimName = MONO_DATATYPE_DIMNAME
    END SELECT



    !#--------------------------------------------------------------------------#
    !#                         -- GET THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! The layer dimension
    ! -------------------

    IF ( PRESENT( nK ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           LAYER_DIMNAME, &
                                           nK, &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//LAYER_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! The spectral dimension
    ! ----------------------

    IF ( PRESENT( nL ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           TRIM( Spectral_DimName ), &
                                           nL, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//TRIM( Spectral_DimName )//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! --------------------------
    ! The perturbation dimension
    ! --------------------------

    IF ( PRESENT( nP ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           PERTURBATION_DIMNAME, &
                                           nP, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//PERTURBATION_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ----------------------------
    ! The input variable dimension
    ! ----------------------------

    IF ( PRESENT( nIV ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           INPUT_VARIABLE_DIMNAME, &
                                           nIV, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//INPUT_VARIABLE_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! -----------------------------
    ! The output variable dimension
    ! -----------------------------

    IF ( PRESENT( nOV ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           OUTPUT_VARIABLE_DIMNAME, &
                                           nOV, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//OUTPUT_VARIABLE_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ---------------------
    ! The dataset dimension
    ! ---------------------

    IF ( PRESENT( nM ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           DATASET_DIMNAME, &
                                           nM, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//DATASET_DIMNAME//' dimension from '//&
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

    IF ( File_DataType == COMPONENTTEST_POLY_DATATYPE ) THEN

      Error_Status = Read_ComponentTest_GAtts( TRIM( NC_Filename ),           &
                                               NC_FileID,                     &
                                               TestType      = TestType,      &
                                               DataType      = DataType,      &
                                               Title         = Title,         &
                                               History       = History,       &
                                               Sensor_Name   = Sensor_Name,   &
                                               Platform_Name = Platform_Name, &
                                               Comment       = Comment,       &
                                               ID_Tag        = ID_Tag,        &
                                               Message_Log   = Message_Log    )

    ELSE

      Error_Status = Read_ComponentTest_GAtts( TRIM( NC_Filename ),      &
                                               NC_FileID,                &
                                               TestType    = TestType,   &
                                               DataType    = DataType,   &
                                               Title       = Title,      &
                                               History     = History,    &
                                               Comment     = Comment,    &
                                               ID_Tag      = ID_Tag,     &
                                               Message_Log = Message_Log )

    END IF

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading global attributes from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_ComponentTest_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ComponentTest data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_ComponentTest_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_ComponentTest_netCDF
!
! PURPOSE:
!       Function to write ComponentTest data to a netCDF format ComponentTest file.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!         Error_Status = Write_ComponentTest_netCDF( NC_Filename,                   &  ! Input
!                                                    ComponentTest,                 &  ! Input
!                                                    New_File      = New_File,      &  ! Optional input
!                                                    Title         = Title,         &  ! Optional input
!                                                    History       = History,       &  ! Optional input
!                                                    Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                                    Platform_Name = Platform_Name, &  ! Optional input
!                                                    Comment       = Comment,       &  ! Optional input
!                                                    ID_Tag        = ID_Tag,        &  ! Optional input
!                                                    RCS_Id        = RCS_Id,        &  ! Version control
!                                                    Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format ComponentTest data file to write to.
!                        If the file does not exist, it is created.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       ComponentTest:   Structure containing the ComponentTest data to
!                        write to file.
!                        UNITS:      N/A
!                        TYPE:       ComponentTest_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       New_File:        Set this argument to write the ComponentTest structure
!                        data to a new file. Default action is to write to
!                        an existing file.
!                        If == 0, data is written to an existing file [DEFAULT]
!                           == 1, a new file is created for output.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:     Character string written into the SENSOR_NAME
!                        global attribute field of the netCDF ComponentTest
!                        file.
!                        ** Only written for POLYCHROMATIC data type **
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:   Character string written into the PLATFORM_NAME
!                        global attribute field of the netCDF ComponentTest
!                        file.
!                        ** Only written for POLYCHROMATIC data type **
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a short tag used to identify the
!                        profile set.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
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
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input ComponentTest structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                           == WARNING an error occurred writing the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Create_ComponentTest_netCDF: Function to create a ComponentTest netDF file.
!
!       Associated_ComponentTest:    Function to test the association status
!                                    of the pointer members of a ComponentTest
!                                    structure.
!                                    SOURCE: TLMTEST_DEFINE module
!
!       Open_ComponentTest_netCDF:   Function to open a ComponentTest netDF file.
!                                    SOURCE: NETCDF_UTILITY module
!
!       Close_ComponentTest_netCDF:  Function to close a netCDF format ComponentTest
!                                    data file with error checking.
!                                    SOURCE: NETCDF_UTILITY module
!
!       Put_netCDF_Variable:         Function to write variable data to a
!                                    netCDF data file.
!                                    SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
!       NF90_ENDDEF:                 Function to put a netCDF dataset into
!                                    data mode.
!                                    SOURCE: netCDF library
!
!       NF90_INQ_VARID:              Function to inquire a variable in a 
!                                    netCDF dataset base don its name.
!                                    SOURCE: netCDF library
!
!       NF90_PUT_VAR:                Function to write variable data to a
!                                    netCDF data file.
!                                    SOURCE: netCDF library
!
!       NF90_CLOSE:                  Function to close a netCDF file.
!                                    SOURCE: netCDF library
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_ComponentTest_netCDF( NC_Filename,   &  ! Input
                                       ComponentTest, &  ! Input
                                       New_File,      &  ! Optional input
                                       Title,         &  ! Optional input
                                       History,       &  ! Optional input
                                       Sensor_Name,   &  ! Optional input
                                       Platform_Name, &  ! Optional input
                                       Comment,       &  ! Optional input
                                       ID_Tag,        &  ! Optional input
                                       RCS_Id,        &  ! Version control
                                       Message_Log )  &  ! Error messaging
                                     RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),             INTENT( IN )  :: NC_Filename
    TYPE( ComponentTest_type ), INTENT( IN )  :: ComponentTest

    ! -- Optional input
    INTEGER,          OPTIONAL, INTENT( IN )  :: New_File
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Comment
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: ID_Tag

    ! -- Version control
    CHARACTER( * ),   OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),   OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_ComponentTest_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: d1_VarName
    CHARACTER( 256 ) :: d2_VarName
    CHARACTER( 256 ) :: Spectral_VarName

    LOGICAL :: Old_File
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUTS --                            #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------
    ! Check structure pointer association status
    ! ------------------------------------------

    IF ( .NOT. Associated_ComponentTest( ComponentTest ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT ComponentTest pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Check the dataset number
    ! ------------------------

    IF ( ComponentTest%nM < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'ComponentTest structure dataset number, nM, component must be > 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#                       -- CHECK OPTIONAL INPUTS --                        #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! Write to an existing file or create a new one?
    ! ----------------------------------------------

    ! -- Default is to write to an existing file....
    Old_File = .TRUE.

    ! -- ....unless the NEW optional argument is set....
    IF ( PRESENT( New_File ) ) THEN
      IF ( New_File == SET ) Old_File = .FALSE.
    END IF

    ! -- ....or if the file doesn't exist
    IF ( .NOT. File_Exists( TRIM( NC_FileNAME ) ) ) Old_File = .FALSE.



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    IF ( Old_File ) THEN


      ! -----------------------------------
      ! Open an existing ComponentTest file
      ! -----------------------------------

      Error_Status = Open_ComponentTest_netCDF( TRIM( NC_FileNAME ), &
                                                NC_FileID, &
                                                Mode = 'READWRITE' )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error opening netCDF ComponentTest data file '//&
                              TRIM( NC_FileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE


      ! -------------------------------
      ! Create a new ComponentTest file
      ! -------------------------------

      Error_Status = Create_ComponentTest_netCDF( TRIM( NC_FileNAME ),                 &  ! Input
                                                  ComponentTest%nK,                    &  ! Input
                                                  ComponentTest%nL,                    &  ! Input
                                                  ComponentTest%nP,                    &  ! Input
                                                  ComponentTest%nIV,                   &  ! Input
                                                  ComponentTest%nOV,                   &  ! Input
                                                  ComponentTest%TestType,              &  ! Input
                                                  ComponentTest%DataType,              &  ! Input
                                                  NC_FileID,                           &  ! Output
                                                  Title         = Title,               &  ! Optional input
                                                  History       = History,             &  ! Optional input
                                                  Sensor_Name   = Sensor_Name,         &  ! Optional input
                                                  Platform_Name = Platform_Name,       &  ! Optional input
                                                  Comment       = Comment,             &  ! Optional input
                                                  ID_Tag        = ID_Tag,              &  ! Optional input
                                                  Message_Log   = Message_Log          )  ! Error messaging

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error creating netCDF ComponentTest data file '//&
                              TRIM( NC_FileNAME ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#        -- ASSIGN THE TEST- AND DATA-TYPE SPECIFIC VARIABLE NAMES --      #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( ComponentTest%TestType )

      CASE ( COMPONENTTEST_FWDTL_TESTTYPE )
        d1_VarName = D1_FWDTL_VARNAME
        d2_VarName = D2_FWDTL_VARNAME

      CASE ( COMPONENTTEST_TLAD_TESTTYPE )
        d1_VarName = D1_TLAD_VARNAME
        d2_VarName = D2_TLAD_VARNAME

      CASE ( COMPONENTTEST_ADK_TESTTYPE )
        d1_VarName = D1_ADK_VARNAME
        d2_VarName = D2_ADK_VARNAME
    END SELECT


    SELECT CASE ( ComponentTest%DataType )

      CASE ( COMPONENTTEST_POLY_DATATYPE )
        Spectral_VarName = POLY_DATATYPE_VARNAME

      CASE ( COMPONENTTEST_MONO_DATATYPE )
        Spectral_VarName = MONO_DATATYPE_VARNAME
    END SELECT



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! The pressure profile
    ! --------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PRESSURE_VARNAME, &
                                        ComponentTest%Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PRESSURE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! The spectral dimension data
    ! ---------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TRIM( Spectral_VarName ), &
                                        ComponentTest%Spectral )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( Spectral_VarName )//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The perturbation array
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PERTURBATION_VARNAME, &
                                        ComponentTest%Perturbation )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PERTURBATION_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The input variable names
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        INPUT_VARIABLE_VARNAME, &
                                        ComponentTest%Input_Variable_Name )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INPUT_VARIABLE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The input variable units
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        INPUT_UNITS_VARNAME, &
                                        ComponentTest%Input_Variable_Units )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//INPUT_UNITS_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------
    ! The output variable names
    ! -------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        OUTPUT_VARIABLE_VARNAME, &
                                        ComponentTest%Output_Variable_Name )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//OUTPUT_VARIABLE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------
    ! The output variable units
    ! -------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        OUTPUT_UNITS_VARNAME, &
                                        ComponentTest%Output_Variable_Units )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//OUTPUT_UNITS_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The DataSet name
    ! ----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DATASET_NAME_VARNAME, &
                                        ComponentTest%nM_Name, &
                                        START = (/ 1, ComponentTest%nM /)  )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATASET_NAME_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D1 data
    ! -------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TRIM( d1_VarName ), &
                                        ComponentTest%d1, &
                                        START = (/ 1, 1, 1, 1, 1, ComponentTest%nM /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing data for ", a, " dataset number ", i5, " to ", a )' ) &
                      TRIM( d1_VarName ), ComponentTest%nM, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D2 data
    ! -------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        TRIM( d2_VarName ), &
                                        ComponentTest%d2, &
                                        START = (/ 1, 1, 1, 1, 1, ComponentTest%nM /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing data for ", a, " dataset number ", i5, " to ", a )' ) &
                      TRIM( d2_VarName ), ComponentTest%nM, TRIM( NC_Filename )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_ComponentTest_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ComponentTest data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_ComponentTest_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_ComponentTest_netCDF
!
! PURPOSE:
!       Function to read ComponentTest data for a specified dataset from a
!       netCDF format ComponentTest file.
!
! CATEGORY:
!       CRTM : Test : Utility
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_ComponentTest_netCDF( NC_Filename,                   &  ! Input
!                                                 nM,                            &  ! Input
!                                                 ComponentTest,                 &  ! Output
!                                                 Title         = Title,         &  ! Optional output
!                                                 History       = History,       &  ! Optional output
!                                                 Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                                 Platform_Name = Platform_Name, &  ! Optional output
!                                                 Comment       = Comment,       &  ! Optional output
!                                                 ID_Tag        = ID_Tag,        &  ! Optional output
!                                                 RCS_Id        = RCS_Id,        &  ! Revision control
!                                                 Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format ComponentTest data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       nM:              The dataset number of the ComponentTest structure to
!                        read from file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
!       ComponentTest:   Structure to contain the spectral coefficient data read
!                        from the file.
!                        UNITS:      N/A
!                        TYPE:       ComponentTest_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Sensor_Name:     Character string written into the SENSOR_NAME global
!                        attribute field of the netCDF ComponentTest file.
!                        ** Only written for POLYCHROMATIC data type **
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Platform_Name:   Character string written into the PLATFORM_NAME global
!                        attribute field of the netCDF ComponentTest file.
!                        ** Only written for POLYCHROMATIC data type **
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ComponentTest file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF ComponentTest file.
!                        Should contain a short tag used to identify the
!                        profile set.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE an unrecoverable read error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Open_ComponentTest_netCDF:     Function to open a netCDF format ComponentTest
!                                      data file.
!
!       Inquire_ComponentTest_netCDF:  Function to inquire a netCDF format 
!                                      ComponentTest file to obtain information
!                                      about the data dimensions and attributes.
!
!       Close_ComponentTest_netCDF:    Function to close a netCDF format ComponentTest
!                                      data file with error checking.
!
!       Allocate_ComponentTest:        Function to allocate the pointer members
!                                      of an ComponentTest structure.
!                                      SOURCE: TLMTEST_DEFINE module
!
!       Get_netCDF_Variable:           Function to read variable data from a
!                                      netCDF data file.
!                                      SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Display_Message:               Subroutine to output messages
!                                      SOURCE: ERROR_HANDLER module
!
!       NF90_INQ_VARID:                Function to inquire a variable in a 
!                                      netCDF dataset base don its name.
!                                      SOURCE: netCDF library
!
!       NF90_GET_VAR:                  Function to read variable data from a
!                                      netCDF data file.
!                                      SOURCE: netCDF library
!
!       NF90_CLOSE:                    Function to close a netCDF file.
!                                      SOURCE: netCDF library
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output ComponentTest argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_ComponentTest_netCDF( NC_Filename,   &  ! Input
                                      nM,            &  ! Input
                                      ComponentTest, &  ! Output
                                      Title,         &  ! Optional output
                                      History,       &  ! Optional output
                                      Sensor_Name,   &  ! Optional output
                                      Platform_Name, &  ! Optional output
                                      Comment,       &  ! Optional output
                                      ID_Tag,        &  ! Optional output
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
    CHARACTER( * ),             INTENT( IN )     :: NC_Filename
    INTEGER,                    INTENT( IN )     :: nM

    ! -- Output
    TYPE( ComponentTest_type ), INTENT( IN OUT ) :: ComponentTest

    ! -- Optional output
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: Sensor_Name
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: Platform_Name
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: Comment
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: ID_Tag

    ! -- Revision control
    CHARACTER( * ),   OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),   OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_ComponentTest_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: d1_VarName
    CHARACTER( 256 ) :: d2_VarName
    CHARACTER( 256 ) :: Spectral_VarName

    INTEGER :: Get_Status
    INTEGER :: Units_Status
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: File_TestType
    INTEGER :: File_DataType

    INTEGER :: NC_FileID
    INTEGER :: nK
    INTEGER :: nL
    INTEGER :: nP
    INTEGER :: nIV
    INTEGER :: nOV
    INTEGER :: n_DataSets
    INTEGER :: varID



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- INQUIRE THE ComponentTest DATA FILE --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Inquire_ComponentTest_netCDF( TRIM( NC_FileNAME ),           &
                                                 nK = nK,                       &
                                                 nL = nL,                       &
                                                 nP = nP,                       &
                                                 nIV = nIV,                     &
                                                 nOV = nOV,                     &
                                                 nM = n_DataSets,               &
                                                 TestType      = File_TestType, &
                                                 DataType      = File_DataType, &
                                                 Title         = Title,         &
                                                 History       = History,       &
                                                 Sensor_Name   = Sensor_Name,   &
                                                 Platform_Name = Platform_Name, &
                                                 Comment       = Comment,       &
                                                 ID_Tag        = ID_Tag,        &
                                                 Message_Log   = Message_Log    )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining ComponentTest dimension/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                          -- CHECK THE DataSet --                         #
    !#--------------------------------------------------------------------------#

    IF ( nM < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input nM data set argument must be > 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( nM > n_DataSets ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input nM data set argument is > netCDF dimension.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- ALLOCATE THE OUTPUT ComponentTest STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_ComponentTest( nK, nL, nP, nIV, nOV, &
                                           ComponentTest, &
                                           Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating ComponentTest structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------------------
    ! Set the test type, data type, and data set
    ! ------------------------------------------

    ComponentTest%TestType = File_TestType
    ComponentTest%DataType = File_DataType
    ComponentTest%nM       = nM



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_ComponentTest_netCDF( TRIM( NC_FileNAME ), &
                                           NC_FileID, &
                                           Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF ComponentTest data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- ASSIGN THE TEST- AND DATA-TYPE SPECIFIC VARIABLE NAMES --      #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( ComponentTest%TestType )

      CASE ( COMPONENTTEST_FWDTL_TESTTYPE )
        d1_VarName = D1_FWDTL_VARNAME
        d2_VarName = D2_FWDTL_VARNAME

      CASE ( COMPONENTTEST_TLAD_TESTTYPE )
        d1_VarName = D1_TLAD_VARNAME
        d2_VarName = D2_TLAD_VARNAME

      CASE ( COMPONENTTEST_ADK_TESTTYPE )
        d1_VarName = D1_ADK_VARNAME
        d2_VarName = D2_ADK_VARNAME
    END SELECT


    SELECT CASE ( ComponentTest%DataType )

      CASE ( COMPONENTTEST_POLY_DATATYPE )
        Spectral_VarName = POLY_DATATYPE_VARNAME

      CASE ( COMPONENTTEST_MONO_DATATYPE )
        Spectral_VarName = MONO_DATATYPE_VARNAME
    END SELECT



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE ComponentTest DATA --                    #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! The pressure profile
    ! --------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PRESSURE_VARNAME, &
                                        ComponentTest%Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PRESSURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The channel list or frequency
    ! -----------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TRIM( Spectral_VarName ), &
                                        ComponentTest%Spectral )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( Spectral_VarName )//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The perturbation array
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PERTURBATION_VARNAME, &
                                        ComponentTest%Perturbation )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PERTURBATION_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The input variable names
    ! ------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        INPUT_VARIABLE_VARNAME, &
                                        ComponentTest%Input_Variable_Name )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INPUT_VARIABLE_VARNAME//' and units attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The input variable units
    ! ------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        INPUT_UNITS_VARNAME, &
                                        ComponentTest%Input_Variable_Units )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//INPUT_VARIABLE_VARNAME//' and units attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------
    ! The output variable names
    ! -------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        OUTPUT_VARIABLE_VARNAME, &
                                        ComponentTest%Output_Variable_Name )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//OUTPUT_VARIABLE_VARNAME//' and units attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------
    ! The output variable units
    ! -------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        OUTPUT_UNITS_VARNAME, &
                                        ComponentTest%Output_Variable_Units )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//OUTPUT_VARIABLE_VARNAME//' and units attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The DataSet name
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        DATASET_NAME_VARNAME, &
                                        ComponentTest%nM_Name, &
                                        START = (/ 1, ComponentTest%nM /)  )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//DATASET_NAME_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D1 data
    ! -------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TRIM( d1_VarName ), &
                                        ComponentTest%d1, &
                                        START = (/ 1, 1, 1, 1, 1, nM /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading data for ", a, " data set number ", i5, "." )' ) &
                      TRIM( d1_VarName ), nM
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------
    ! D2 data
    ! -------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        TRIM( d2_VarName ), &
                                        ComponentTest%d2, &
                                        START = (/ 1, 1, 1, 1, 1, nM /) )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading data for ", a, " data set number ", i5, "." )' ) &
                      TRIM( d2_VarName ), nM
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_ComponentTest_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ComponentTest data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_ComponentTest_netCDF

END MODULE ComponentTest_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: ComponentTest_netCDF_IO.f90,v 1.3 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: ComponentTest_netCDF_IO.f90,v $
! Revision 1.3  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2006/03/06 23:11:13  paulv
! - Made all parameters inherited from ComponentTest_Define public.
! - The input and output variable units are no longer attributes of the
!   variable name variables - netCDF API doesn't allow rank-1 attributes of
!   character type. The variable unit strings are now read and written as variables
!   themselves.
! - Corrected bugs in named of inherited parameters.
!
! Revision 1.1  2006/03/06 19:24:32  paulv
! Initial checkin. Untested.
!
!
!
