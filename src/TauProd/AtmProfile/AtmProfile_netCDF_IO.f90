!------------------------------------------------------------------------------
!M+
! NAME:
!       AtmProfile_netCDF_IO
!
! PURPOSE:
!       Module containing routines to read and write AtmProfile netCDF 
!       format files.
!       
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AtmProfile_netCDF_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       AtmProfile_Define:     Module defining the AtmProfile data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
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
!
! CONTAINS:
!       Inquire_AtmProfile_netCDF:  Function to inquire a netCDF format 
!                                   AtmProfile file to obtain information
!                                   about the data dimensions and attributes.
!
!       Write_AtmProfile_netCDF:    Function to write AtmProfile data to a
!                                   netCDF format AtmProfile file.
!
!       Read_AtmProfile_netCDF:     Function to read AtmProfile data from a
!                                   netCDF format AtmProfile file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002, 2003 Paul van Delst
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

MODULE AtmProfile_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE AtmProfile_Define

  USE netcdf
  USE netCDF_Utility,  Open_AtmProfile_netCDF =>  Open_netCDF, &
                      Close_AtmProfile_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_AtmProfile_netCDF
  PUBLIC :: Write_AtmProfile_netCDF
  PUBLIC :: Read_AtmProfile_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: AtmProfile_netCDF_IO.f90,v 4.3 2006/06/30 16:47:16 dgroff Exp $'

  ! -- Keyword set value
  INTEGER,        PRIVATE, PARAMETER :: UNSET = 0
  INTEGER,        PRIVATE, PARAMETER ::   SET = 1

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME   = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME = 'comment' 
  CHARACTER( * ), PRIVATE, PARAMETER :: ID_TAG_GATTNAME  = 'id_tag' 

  ! -- Dimension names
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_DIMNAME       = 'n_levels'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DIMNAME       = 'n_layers'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_DIMNAME    = 'n_absorbers'
  CHARACTER( * ), PRIVATE, PARAMETER :: PROFILE_DIMNAME     = 'n_profiles'
  CHARACTER( * ), PRIVATE, PARAMETER :: DESCRIPTION_DIMNAME = 'pdsl'

  ! -- Variable names
  CHARACTER( * ), PRIVATE, PARAMETER :: DESCRIPTION_VARNAME        = 'profile_description'
  CHARACTER( * ), PRIVATE, PARAMETER :: CLIMATOLOGY_MODEL_VARNAME  = 'climatology_model'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATETIME_VARNAME           = 'date_time'
  CHARACTER( * ), PRIVATE, PARAMETER :: LATITUDE_VARNAME           = 'latitude'
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGITUDE_VARNAME          = 'longitude'
  CHARACTER( * ), PRIVATE, PARAMETER :: SURFACE_ALTITUDE_VARNAME   = 'surface_altitude'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_ID_VARNAME        = 'absorber_id'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_UNITS_ID_VARNAME  = 'absorber_units_id'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_PRESSURE_VARNAME     = 'level_pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_TEMPERATURE_VARNAME  = 'level_temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ABSORBER_VARNAME     = 'level_absorber'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ALTITUDE_VARNAME     = 'level_altitude'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_PRESSURE_VARNAME     = 'layer_pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_TEMPERATURE_VARNAME  = 'layer_temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_ABSORBER_VARNAME     = 'layer_absorber'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DELTA_Z_VARNAME      = 'layer_delta_z'

  ! -- Variable long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: DESCRIPTION_LONGNAME       = 'Description of profile'
  CHARACTER( * ), PRIVATE, PARAMETER :: CLIMATOLOGY_MODEL_LONGNAME = &
  'Climatology model associated with profile date/time/location.'
  CHARACTER( * ), PRIVATE, PARAMETER :: DATETIME_LONGNAME          = &
  'Date and Time in which profile was recorded(sonde) or for which it was generated(model)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LATITUDE_LONGNAME          = 'Latitude of profile location'
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGITUDE_LONGNAME         = 'Longitude of profile location'
  CHARACTER( * ), PRIVATE, PARAMETER :: SURFACE_ALTITUDE_LONGNAME  = 'Surface altitude of profile'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_ID_LONGNAME       = &
  'HITRAN/LBLRTM absorber ID number for atmospheric profile constituents'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_UNITS_ID_LONGNAME = 'LBLRTM absorber units ID number'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_PRESSURE_LONGNAME    = 'Level pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_TEMPERATURE_LONGNAME = 'Level temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ABSORBER_LONGNAME    = 'Level absorber'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ALTITUDE_LONGNAME    = 'Level altitude'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_PRESSURE_LONGNAME    = 'Layer pressure'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_TEMPERATURE_LONGNAME = 'Layer temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_ABSORBER_LONGNAME    = 'Layer absorber'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DELTA_Z_LONGNAME     = 'Layer thickness'

  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: DATETIME_UNITS          = 'YYYYMMDD.HH'
  CHARACTER( * ), PRIVATE, PARAMETER :: LATITUDE_UNITS          = 'degress North (-90->+90)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGITUDE_UNITS         = 'degress East (0->360)'
  CHARACTER( * ), PRIVATE, PARAMETER :: SURFACE_ALTITUDE_UNITS  = 'metres (m)'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_ID_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: ABSORBER_UNITS_ID_UNITS = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_PRESSURE_UNITS    = 'hectoPascals (hPa)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_TEMPERATURE_UNITS = 'Kelvin (K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ABSORBER_UNITS    = 'Variable (see Absorber_Units_ID)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LEVEL_ALTITUDE_UNITS    = 'metres (m)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_PRESSURE_UNITS    = 'hectoPascals (hPa)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_TEMPERATURE_UNITS = 'Kelvin (K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_ABSORBER_UNITS    = 'Variable (see Absorber_Units_ID)'
  CHARACTER( * ), PRIVATE, PARAMETER :: LAYER_DELTA_Z_UNITS     = 'metres (m)'


  ! -- Variable netCDF datatypes
  INTEGER,        PRIVATE, PARAMETER :: DESCRIPTION_TYPE        = NF90_CHAR
  INTEGER,        PRIVATE, PARAMETER :: CLIMATOLOGY_MODEL_TYPE  = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: DATETIME_TYPE           = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LATITUDE_TYPE           = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LONGITUDE_TYPE          = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: SURFACE_ALTITUDE_TYPE   = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: ABSORBER_ID_TYPE        = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: ABSORBER_UNITS_ID_TYPE  = NF90_INT
  INTEGER,        PRIVATE, PARAMETER :: LEVEL_PRESSURE_TYPE     = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LEVEL_TEMPERATURE_TYPE  = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LEVEL_ABSORBER_TYPE     = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LEVEL_ALTITUDE_TYPE     = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LAYER_PRESSURE_TYPE     = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LAYER_TEMPERATURE_TYPE  = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LAYER_ABSORBER_TYPE     = NF90_DOUBLE
  INTEGER,        PRIVATE, PARAMETER :: LAYER_DELTA_Z_TYPE      = NF90_DOUBLE

  ! -- Variable fill value attribute
  CHARACTER( * ), PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################



  ! --------------------------------------------------
  ! PRIVATE utility routine to convert the data in the
  ! AtmProfileDateTime structure to a double precision
  ! value of YYYYMMDD.HH
  !   where YYYY = year
  !         MM   = month
  !         DD   = day of month
  !         HH   = hour of day (0-23)
  ! The result is what is stored in the netCDF dataset
  ! --------------------------------------------------

  SUBROUTINE Convert_DateTime_to_Double( DateTime_DT, DateTime )


    ! -----------------
    ! Type declarations
    ! -----------------

    TYPE( AtmProfileDateTime_type ), DIMENSION(:), INTENT( IN )  :: DateTime_DT
    REAL( Double ),                  DIMENSION(:), INTENT( OUT ) :: DateTime

    INTEGER :: n


    ! ---------------------
    ! Convert the data type
    ! ---------------------

    DO n = 1, SIZE( DateTime_DT )
      DateTime(n) = REAL( ( DateTime_DT(n)%Year  * 10000 ) + &
                          ( DateTime_DT(n)%Month * 100   ) + &
                          ( DateTime_DT(n)%Day           ), Double ) + &
                    ( REAL( DateTime_DT(n)%Hour, Double ) / 100.0_Double )
    END DO
               
  END SUBROUTINE Convert_DateTime_to_Double



  ! ---------------------------------------------------
  ! PRIVATE utility routine to convert the data in the
  ! double precision netCDF data set date/time variable
  ! to a AtmProfileDateTime data type.
  ! ---------------------------------------------------

  SUBROUTINE Convert_DateTime_to_Type( DateTime, DateTime_DT )


    ! -----------------
    ! Type declarations
    ! -----------------

    REAL( Double ),                  DIMENSION(:), INTENT( IN )  :: DateTime
    TYPE( AtmProfileDateTime_type ), DIMENSION(:), INTENT( OUT ) :: DateTime_DT

    INTEGER( Long ) :: x
    INTEGER :: n


    ! ---------------------
    ! Convert the data type
    ! ---------------------

    DO n = 1, SIZE( DateTime )

      ! -- The year
      x = INT( DateTime(n), Long )
      DateTime_DT(n)%Year = ( x - MOD( x, 10000_Long ) ) / 10000_Long

      ! -- The month
      x = MOD( x, 10000_Long )
      DateTime_DT(n)%Month = ( x - MOD( x, 100_Long ) ) / 100_Long

      ! -- The day of the month
      DateTime_DT(n)%Day = MOD( x, 100_Long )

      ! -- The hour of the day
      DateTime_DT(n)%Hour = NINT( MOD( DateTime(n), 1.0_Double ) * 100.0_Double )

    END DO
               
  END SUBROUTINE Convert_DateTime_to_Type




!--------------------------------------------------------------------------------
!
! NAME:
!       Write_AtmProfile_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF AtmProfile data file.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_AtmProfile_GAtts( NC_Filename,              &  ! Input
!                                              NC_FileID,                &  ! Input
!                                              Title       = Title,      &  ! Optional input
!                                              History     = History,    &  ! Optional input
!                                              Comment     = Comment,    &  ! Optional input
!                                              ID_Tag      = ID_Tag,     &  ! Optional input
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_AtmProfile_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a short tag used to identify the
!                         profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:      Character string specifying a filename in which
!                         any Messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output Messages to standard
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the global attribute write was successful.
!                        == WARNING an error occurred writing the supplied
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       NF90_PUT_ATT:       Function to write attribute data to a netCDF 
!                           data file.
!                           SOURCE: netCDF library
!
!       Display_Message:    Subroutine to output Messages
!                           SOURCE: ERROR_HANDLER module
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
!
!--------------------------------------------------------------------------------

  FUNCTION Write_AtmProfile_GAtts( NC_Filename,   &  ! Input
                                   NC_FileID,     &  ! Input
                                   Title,         &  ! Optional input
                                   History,       &  ! Optional input
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

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: ID_Tag

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_AtmProfile_GAtts'

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

    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                WRITE_MODULE_HISTORY_GATTNAME, &
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


    ! ----------------------------
    ! Dependent profile set ID tag
    ! ----------------------------

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

  END FUNCTION Write_AtmProfile_GAtts





!--------------------------------------------------------------------------------
!
! NAME:
!       Read_AtmProfile_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF AtmProfile data file.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_AtmProfile_GAtts( NC_Filename,            &  ! Input
!                                             NC_FileID,              &  ! Input
!                                             Title       = Title,    &  ! Optional output
!                                             History     = History,  &  ! Optional output
!                                             Comment     = Comment,  &  ! Optional output
!                                             ID_Tag      = ID_Tag,   &  ! Optional output
!                                             Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF AtmProfile format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_ or Create_AtmProfile_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any Messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output Messages to standard
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
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF AtmProfile file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       ID_Tag:           Character string written into the ID_TAG global
!                         attribute field of the netCDF AtmProfile file.
!                         Should contain a short tag used to identify the
!                         profile set.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the global attribute read was successful.
!                        == WARNING an error occurred reading the requested
!                           global attributes.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Get_netCDF_Attribute: Function to read attribute data from a netCDF 
!                             data file.
!                             SOURCE: NETCDF_ATTRIBUTE module
!
!       Display_Message:      Subroutine to output Messages
!                             SOURCE: ERROR_HANDLER module
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
!
!--------------------------------------------------------------------------------

  FUNCTION Read_AtmProfile_GAtts( NC_Filename,  &  ! Input
                                  NC_FileID,    &  ! Input
                                  Title,        &  ! Optional output
                                  History,      &  ! Optional output
                                  Comment,      &  ! Optional output
                                  ID_Tag,       &  ! Optional output
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
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: ID_Tag

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AtmProfile_GAtts'


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

  END FUNCTION Read_AtmProfile_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Create_AtmProfile_netCDF
!
! PURPOSE:
!       Function to create a netCDF AtmProfile data file for writing.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_AtmProfile_netCDF( NC_Filename,              &  ! Input
!                                                n_Layers,                 &  ! Input
!                                                n_Absorbers,              &  ! Input
!                                                n_Profiles,               &  ! Input
!                                                NC_FileID,                &  ! Output
!                                                ID_Tag      = ID_Tag,     &  ! Optional input
!                                                Title       = Title,      &  ! Optional input
!                                                History     = History,    &  ! Optional input
!                                                Comment     = Comment,    &  ! Optional input
!                                                RCS_Id      = RCS_Id,     &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF AtmProfile format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Layers:           The number of atmospheric layers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Absorbers:        The number of molecular absorbers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Profiles:         The number of profiles contained in the netCDF
!                           dataset.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF AtmProfile file.
!                           Should contain a short tag used to identify the
!                           profile set.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AtmProfile file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any Messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output Messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       Integer
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the netCDF file creation was successful.
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred writing any of the
!                                   supplied global attributes.
!
! CALLS:
!       NF90_CREATE:             Function to create a netCDF data file and
!                                place it in DEFINE mode.
!                                SOURCE: netCDF library
!
!       NF90_DEF_DIM:            Function to define a dimension in a netCDF
!                                data file.
!                                SOURCE: netCDF library
!
!       NF90_PUT_ATT:            Function to write attribute data to a netCDF
!                                data file.
!                                SOURCE: netCDF library
!
!       NF90_DEF_VAR:            Function to define a variable in a netCDF
!                                data file.
!                                SOURCE: netCDF library
!
!       NF90_PUT_VAR:            Function to write variable data to a netCDF
!                                data file.
!                                SOURCE: netCDF library
!
!       NF90_ENDDEF:             Function to take a netCDF file out of DEFINE
!                                mode and put it in DATA mode.
!                                SOURCE: netCDF library
!
!       NF90_REDEF:              Function to put a netCDF file in DEFINE mode.
!                                SOURCE: netCDF library
!
!       NF90_CLOSE:              Function to close a netCDF file.
!                                SOURCE: netCDF library
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the file already exists, it is overwritten.
!       - If a FAILURE error occurs, the netCDF file is closed.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Create_AtmProfile_netCDF( NC_Filename,  &  ! Input
                                     n_Layers,     &  ! Input
                                     n_Absorbers,  &  ! Input
                                     n_Profiles,   &  ! Input
                                     NC_FileID,    &  ! Output
                                     ID_Tag,       &  ! Optional input
                                     Title,        &  ! Optional input
                                     History,      &  ! Optional input
                                     Comment,      &  ! Optional input
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
    INTEGER,                  INTENT( IN )  :: n_Layers
    INTEGER,                  INTENT( IN )  :: n_Absorbers
    INTEGER,                  INTENT( IN )  :: n_Profiles

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_AtmProfile_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: NF90_Status
    INTEGER :: NF90_Status1, NF90_Status2, NF90_Status3

    INTEGER :: n_Levels

    INTEGER :: Level_DimID
    INTEGER :: Layer_DimID
    INTEGER :: Absorber_DimID
    INTEGER :: Profile_DimID
    INTEGER :: PD_StrLen_DimID    

    INTEGER :: VarID
    INTEGER :: Absorber_ID_VarID
    INTEGER :: Absorber_Units_ID_VarID

    ! -- Used only to determine the profile description string length
    TYPE( AtmProfile_type ) :: APdummy



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! -----------------
    ! n_Layer dimension
    ! -----------------

    IF ( n_Layers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_LAYERS dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    n_Levels = n_Layers + 1


    ! ---------------------
    ! n_Absorbers dimension
    ! ---------------------

    ! -- Invalid?
    IF ( n_Absorbers < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_ABSORBERS dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Too many?
    IF ( n_Absorbers > ATMPROFILE_N_ABSORBERS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input number of absorbers, ", i5, &
                        &", is greater than the maximum allowed, ", i5, "." )' ) &
                      n_Absorbers, ATMPROFILE_N_ABSORBERS
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------
    ! n_Profiles dimension
    ! --------------------

    IF ( n_Profiles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_PROFILES dimension must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
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
    ! The number of levels
    ! --------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                LEVEL_DIMNAME, &
                                n_Levels, &
                                Level_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//LEVEL_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
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
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------
    ! The number of absorbers
    ! -----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                ABSORBER_DIMNAME, &
                                n_Absorbers, &
                                Absorber_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//ABSORBER_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The number of profiles
    ! ----------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                PROFILE_DIMNAME, &
                                n_Profiles, &
                                Profile_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//PROFILE_DIMNAME//' dimension in '// &
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! The profile description string length
    ! -------------------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                DESCRIPTION_DIMNAME, &
                                APdummy%PD_StrLen, &
                                PD_StrLen_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//DESCRIPTION_DIMNAME//' dimension in '// &
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

    Error_Status = Write_AtmProfile_GAtts( TRIM( NC_Filename ), &
                                           NC_FileID, &
                                           Title   = Title, &
                                           History = History, &
                                           Comment = Comment, &
                                           ID_Tag  = ID_Tag, &
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

    ! -------------------
    ! Profile description
    ! -------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DESCRIPTION_VARNAME, &
                                DESCRIPTION_TYPE, &
                                DimIDs = (/ PD_StrLen_DimID, Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//DESCRIPTION_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                DESCRIPTION_LONGNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DESCRIPTION_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Climatology model
    ! -----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                CLIMATOLOGY_MODEL_VARNAME, &
                                CLIMATOLOGY_MODEL_TYPE, &
                                DimIDs = Profile_DimID, &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//CLIMATOLOGY_MODEL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                VarID, &
                                LONGNAME_ATTNAME, &
                                CLIMATOLOGY_MODEL_LONGNAME )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------
    ! Date and time
    ! -------------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                DATETIME_VARNAME, &
                                DATETIME_TYPE, &
                                DimIDs = Profile_DimID, &
                                VarID = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//DATETIME_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write the attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 DATETIME_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 DATETIME_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. &
         NF90_Status2 /= NF90_NOERR .AND. &
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATETIME_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------
    ! Latitude
    ! --------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LATITUDE_VARNAME, &
                                LATITUDE_TYPE, &
                                DimIDs = Profile_DimID, &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LATITUDE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LATITUDE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LATITUDE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LATITUDE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------
    ! Longitude
    ! ---------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LONGITUDE_VARNAME, &
                                LONGITUDE_TYPE, &
                                DimIDs = Profile_DimID, &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LONGITUDE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LONGITUDE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LONGITUDE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LONGITUDE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! Surface Altitude
    ! ----------------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SURFACE_ALTITUDE_VARNAME, &
                                SURFACE_ALTITUDE_TYPE, &
                                DimIDs = Profile_DimID, &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SURFACE_ALTITUDE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 SURFACE_ALTITUDE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 SURFACE_ALTITUDE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SURFACE_ALTITUDE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------
    ! Absorber ID
    ! -----------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ABSORBER_ID_VARNAME, &
                                ABSORBER_ID_TYPE, &
                                DimIDs = Absorber_DimID, &
                                VarID  = Absorber_ID_VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ABSORBER_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_ID_VarID, &
                                 LONGNAME_ATTNAME, &
                                 ABSORBER_ID_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_ID_VarID, &
                                 UNITS_ATTNAME, &
                                 ABSORBER_ID_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_ID_VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_IP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ABSORBER_ID_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Absorber Units ID
    ! -----------------

    ! -- Define the variables
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ABSORBER_UNITS_ID_VARNAME, &
                                ABSORBER_UNITS_ID_TYPE, &
                                DimIDs = Absorber_DimID, &
                                VarID  = Absorber_Units_ID_VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//ABSORBER_UNITS_ID_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_Units_ID_VarID, &
                                 LONGNAME_ATTNAME, &
                                 ABSORBER_UNITS_ID_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_Units_ID_VarID, &
                                 UNITS_ATTNAME, &
                                 ABSORBER_UNITS_ID_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 Absorber_Units_ID_VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_IP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ABSORBER_UNITS_ID_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level Pressure
    ! --------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_PRESSURE_VARNAME, &
                                LEVEL_PRESSURE_TYPE, &
                                DimIDs = (/ Level_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LEVEL_PRESSURE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LEVEL_PRESSURE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LEVEL_PRESSURE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_PRESSURE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Level Temperature
    ! -----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_TEMPERATURE_VARNAME, &
                                LEVEL_TEMPERATURE_TYPE, &
                                DimIDs = (/ Level_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LEVEL_TEMPERATURE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LEVEL_TEMPERATURE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LEVEL_TEMPERATURE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_TEMPERATURE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level Absorber
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_ABSORBER_VARNAME, &
                                LEVEL_ABSORBER_TYPE, &
                                DimIDs = (/ Level_DimID, &
                                            Absorber_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LEVEL_ABSORBER_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LEVEL_ABSORBER_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LEVEL_ABSORBER_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_ABSORBER_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level Altitude
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LEVEL_ALTITUDE_VARNAME, &
                                LEVEL_ALTITUDE_TYPE, &
                                DimIDs = (/ Level_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LEVEL_ALTITUDE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LEVEL_ALTITUDE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LEVEL_ALTITUDE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_ALTITUDE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Layer Pressure
    ! --------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LAYER_PRESSURE_VARNAME, &
                                LAYER_PRESSURE_TYPE, &
                                DimIDs = (/ Layer_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LAYER_PRESSURE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LAYER_PRESSURE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LAYER_PRESSURE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_PRESSURE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Layer Temperature
    ! -----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LAYER_TEMPERATURE_VARNAME, &
                                LAYER_TEMPERATURE_TYPE, &
                                DimIDs = (/ Layer_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LAYER_TEMPERATURE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LAYER_TEMPERATURE_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LAYER_TEMPERATURE_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_TEMPERATURE_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Layer Absorber
    ! --------------

    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LAYER_ABSORBER_VARNAME, &
                                LAYER_ABSORBER_TYPE, &
                                DimIDs = (/ Layer_DimID, &
                                            Absorber_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LAYER_ABSORBER_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LAYER_ABSORBER_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LAYER_ABSORBER_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_ABSORBER_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF




    ! ---------------
    ! Layer thickness
    ! ---------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                LAYER_DELTA_Z_VARNAME, &
                                LAYER_DELTA_Z_TYPE, &
                                DimIDs = (/ Layer_DimID, &
                                            Profile_DimID /), &
                                VarID  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//LAYER_DELTA_Z_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write attributes
    NF90_Status1 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 LONGNAME_ATTNAME, &
                                 LAYER_DELTA_Z_LONGNAME )

    NF90_Status2 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 UNITS_ATTNAME, &
                                 LAYER_DELTA_Z_UNITS )

    NF90_Status3 = NF90_PUT_ATT( NC_FileID, &
                                 VarID, &
                                 FILLVALUE_ATTNAME, &
                                 ATMPROFILE_FP_INVALID )

    IF ( NF90_Status1 /= NF90_NOERR .AND. & 
         NF90_Status2 /= NF90_NOERR .AND. & 
         NF90_Status3 /= NF90_NOERR       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_DELTA_Z_VARNAME//' attributes to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF




    !#--------------------------------------------------------------------------#
    !#  -- WRITE THE ABSORBER ID AND UNITS ID DATA AND LEVEL/LAYER PRESSURES -- #
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



  END FUNCTION Create_AtmProfile_netCDF





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_AtmProfile_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF AtmProfile format file to obtain the
!       dimensions and global attributes.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_AtmProfile_netCDF( NC_Filename,               &  ! Input
!                                                 n_Layers    = n_Layers,    &  ! Optional output
!                                                 n_Absorbers = n_Absorbers, &  ! Optional output
!                                                 n_Profiles  = n_Profiles,  &  ! Optional output
!                                                 ID_Tag      = ID_Tag,      &  ! Optional output
!                                                 Title       = Title,       &  ! Optional output
!                                                 History     = History,     &  ! Optional output
!                                                 Comment     = Comment,     &  ! Optional output
!                                                 RCS_Id      = RCS_Id,      &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the netCDF
!                           format AtmProfile data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Layers:           The number of atmospheric layers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Absorbers:        The number of molecular absorbers dimension of the
!                           atmospheric profile data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Profiles:         The number of profiles contained in the netCDF
!                           dataset.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       ID_Tag:             Character string written into the ID_TAG global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF AtmProfile file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the netCDF file inquiry was successful.
!                        == FAILURE - an error occurred opening the netCDF file, or
!                                   - an error occurred reading any of the requested
!                                     dimension or variable data.
!                        == WARNING - an error occurred reading any of the requested
!                                     global file attributes, or
!                                   - an error occurred closing the netCDF file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_AtmProfile_netCDF:  Function to open an AtmProfile netCDF file.
!
!       Close_AtmProfile_netCDF: Function to close an AtmProfile netCDF file.
!
!       Read_AtmProfile_GATTs:   Function to retrieve the global attributes
!                                from the AtmProfile netCDF file.
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
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_AtmProfile_netCDF( NC_Filename,  &  ! Input
                                      n_Layers,     &  ! Optional output
                                      n_Absorbers,  &  ! Optional output
                                      n_Profiles,   &  ! Optional output
                                      ID_Tag,       &  ! Optional output
                                      Title,        &  ! Optional output
                                      History,      &  ! Optional output
                                      Comment,      &  ! Optional output
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
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
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Layers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Absorbers
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Profiles

    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: ID_Tag
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_AtmProfile_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NC_FileID
    INTEGER :: NF90_Status
    INTEGER :: Close_Status



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

    Error_Status = Open_AtmProfile_netCDF( TRIM( NC_FileNAME ), &
                                           NC_FileID, &
                                           Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF AtmProfile data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- GET THE DIMENSIONS --                          #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! The number of layers
    ! --------------------

    IF ( PRESENT( n_Layers ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           LAYER_DIMNAME, &
                                           n_Layers, &
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


    ! -----------------------
    ! The number of absorbers
    ! -----------------------

    IF ( PRESENT( n_Absorbers ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           ABSORBER_DIMNAME, &
                                           n_Absorbers, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//ABSORBER_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! ----------------------
    ! The number of profiles
    ! ----------------------

    IF ( PRESENT( n_Profiles ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           PROFILE_DIMNAME, &
                                           n_Profiles, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//PROFILE_DIMNAME//' dimension from '//&
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

    Error_Status = Read_AtmProfile_GAtts( TRIM( NC_Filename ), &
                                          NC_FileID, &
                                          Title         = Title, &
                                          History       = History, &
                                          Comment       = Comment, &
                                          ID_Tag        = ID_Tag, &
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

    Close_Status = Close_AtmProfile_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AtmProfile data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_AtmProfile_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_AtmProfile_netCDF
!
! PURPOSE:
!       Function to write AtmProfile structures to a netCDF format
!       AtmProfile file.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_AtmProfile_netCDF( NC_Filename,              &  ! Input
!                                               AtmProfile,               &  ! Input
!                                               Quiet       = Quiet,      &  ! Optional Input
!                                               Title       = Title,      &  ! Optional input
!                                               History     = History,    &  ! Optional input
!                                               Comment     = Comment,    &  ! Optional input
!                                               ID_Tag      = ID_Tag,     &  ! Optional input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        AtmProfile format data file to write to. It must
!                        have been previously created via the Create() function.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       AtmProfile:      Structure containing the atmospheric profile data
!                        to be written to the netCDF dataset.
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF AtmProfile file.
!                        Should contain a short tag used to identify the
!                        profile set.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF AtmProfile file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF AtmProfile file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF AtmProfile file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the netCDF data write was successful
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred closing the netCDF
!                                   output file after a successful write.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Create_AtmProfile_netCDF:   Function to create a netCDF format 
!                                   AtmProfile file for writing.
!
!       Close_AtmProfile_netCDF:    Function to close an AtmProfile netCDF file.
!
!       Put_netCDF_Variable:        Function to write a variable to a
!                                   netCDF file given the variable name.
!                                   SOURCE: NETCDF_UTILITY module
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Display_Message:            Subroutine to output Messages
!                                   SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_AtmProfile_netCDF( NC_Filename,  &  ! Input
                                    AtmProfile,   &  ! Input
                                    Quiet,        &  ! Optional input
                                    Title,        &  ! Optional input
                                    History,      &  ! Optional input
                                    Comment,      &  ! Optional input
                                    ID_Tag,       &  ! Optional input
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
    TYPE( AtmProfile_type ),  INTENT( IN )  :: AtmProfile

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Quiet
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: ID_Tag

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_AtmProfile_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status
    INTEGER :: NF90_Status
    INTEGER :: j
    REAL( Double ), DIMENSION( SIZE( AtmProfile%DateTime ) ) :: DateTime



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_AtmProfile( AtmProfile ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT AtmProfile pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Check for invalid Absorber info values
    ! --------------------------------------

    ! -- Absorber ID
    IF ( ANY( AtmProfile%Absorber_ID < 1 ) .OR. &
         ANY( AtmProfile%Absorber_ID > ATMPROFILE_N_ABSORBERS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid value found in input Absorber_ID array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Absorber Units ID
    IF ( ANY( AtmProfile%Absorber_Units_ID < 1 ) .OR. &
         ANY( AtmProfile%Absorber_Units_ID > ATMPROFILE_N_ABSORBER_UNITS ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid value found in input Absorber_Units_ID array.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------------
    ! Check for repeated Absorber_ID values
    ! -------------------------------------

    DO j = 1, AtmProfile%n_Absorbers
      IF ( COUNT( AtmProfile%Absorber_ID == AtmProfile%Absorber_ID( j ) ) > 1 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "More than one Absorber ID value of ", i2, " found." )' ) &
                        AtmProfile%Absorber_ID( j ) 
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END DO


    ! --------------------------------------
    ! Check for Absorber Units ID value for
    ! non-H2O dewpoint and relative humidity
    ! --------------------------------------

    DO j = 1, AtmProfile%n_Absorbers
      IF ( AtmProfile%Absorber_ID( j ) /= 1 ) THEN  ! If not H2O
        IF ( AtmProfile%Absorber_Units_ID( j ) > 5 ) THEN  ! Cannot have units of dewpoint or RH%
          Error_Status = FAILURE
          WRITE( Message, '( "Absorber #", i2, " (ID# = ", i2, ") cannot have units of ", a )' ) &
                          j, AtmProfile%Absorber_ID( j ), &
                          TRIM( ATMPROFILE_ABSORBER_UNITS_NAME( AtmProfile%Absorber_Units_ID( j ) ) )
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF
      END IF
    END DO



    !#--------------------------------------------------------------------------#
    !#                   -- CREATE THE OUTPUT DATA FILE --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = Create_AtmProfile_netCDF( TRIM( NC_Filename ), &
                                             AtmProfile%n_Layers, &
                                             AtmProfile%n_Absorbers, &
                                             AtmProfile%n_Profiles, &
                                             NC_FileID, &
                                             Title   = Title, &
                                             History = History, &
                                             Comment = Comment, &
                                             ID_Tag  = ID_Tag, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating output netCDF AtmProfile file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE ABSORBER INFO DATA --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! Write the Absorber ID
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        AtmProfile%Absorber_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ABSORBER_ID_VARNAME//' to '//&
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------
    ! Write the Absorber Units ID
    ! ---------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ABSORBER_UNITS_ID_VARNAME, &
                                        AtmProfile%Absorber_Units_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//ABSORBER_UNITS_ID_VARNAME//' to '//&
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE PROFILE "INFO" DATA --                   #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The profile description
    ! -----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DESCRIPTION_VARNAME, &
                                        AtmProfile%Description )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DESCRIPTION_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------
    ! The climatology model
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CLIMATOLOGY_MODEL_VARNAME, &
                                        AtmProfile%Climatology_Model )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CLIMATOLOGY_MODEL_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The Date and time
    ! -----------------

    ! -- Convert the Dates and Times to double precision variables
    CALL Convert_DateTime_to_Double( AtmProfile%DateTime, DateTime )

    ! -- Write the data
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        DATETIME_VARNAME, &
                                        DateTime )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//DATETIME_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------
    ! Latitude
    ! --------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LATITUDE_VARNAME, &
                                        AtmProfile%Location%Latitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LATITUDE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------
    ! Longitude
    ! ---------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LONGITUDE_VARNAME, &
                                        AtmProfile%Location%Longitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LONGITUDE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! Surface Altitude
    ! ----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SURFACE_ALTITUDE_VARNAME, &
                                        AtmProfile%Location%Surface_Altitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SURFACE_ALTITUDE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE PROFILE LEVEL DATA --                    #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Write the Level Pressure
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        AtmProfile%Level_Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_PRESSURE_VARNAME//' to '//&
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Level temperature
    ! -----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_TEMPERATURE_VARNAME, &
                                        AtmProfile%Level_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_TEMPERATURE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level absorber
    ! --------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_ABSORBER_VARNAME, &
                                        AtmProfile%Level_Absorber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_ABSORBER_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level Altitude
    ! --------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LEVEL_ALTITUDE_VARNAME, &
                                        AtmProfile%Level_Altitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LEVEL_ALTITUDE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE PROFILE LAYER DATA --                    #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Write the Layer Pressure
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LAYER_PRESSURE_VARNAME, &
                                        AtmProfile%Layer_Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_PRESSURE_VARNAME//' to '//&
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Layer temperature
    ! -----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LAYER_TEMPERATURE_VARNAME, &
                                        AtmProfile%Layer_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_TEMPERATURE_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Layer absorber
    ! --------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LAYER_ABSORBER_VARNAME, &
                                        AtmProfile%Layer_Absorber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_ABSORBER_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Layer thickness
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        LAYER_DELTA_Z_VARNAME, &
                                        AtmProfile%Layer_Delta_Z )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//LAYER_DELTA_Z_VARNAME//' to '// &
                            TRIM( NC_Filename )//'- '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_AtmProfile_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AtmProfile data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_AtmProfile( AtmProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_FileNAME )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_AtmProfile_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_AtmProfile_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format AtmProfile file.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_AtmProfile_netCDF( NC_Filename,              &  ! Input
!                                              AtmProfile,               &  ! Output
!                                              Quiet       = Quiet,      &  ! Optional Input
!                                              Reverse     = Reverse,    &  ! Optional Input
!                                              Title       = Title,      &  ! Optional output
!                                              History     = History,    &  ! Optional output
!                                              Comment     = Comment,    &  ! Optional output
!                                              ID_Tag      = ID_Tag,     &  ! Optional output
!                                              RCS_Id      = RCS_Id,     &  ! Revision control
!                                              Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF AtmProfile
!                        format data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Reverse:         Set this keyword to reverse the order of the profile data
!                        arrays in the K index (vertical) dimension.
!                        If REVERSE = 0, arrays are returned as they are stored in
!                                        the netCDF input file (DEFAULT)
!                           REVERSE = 1, arrays are returned in reverse order to how
!                                        they are stored in the netCDF input file.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       AtmProfile:      Structure containing the atmospheric profile data.
!                        UNITS:      N/A
!                        TYPE:       AtmProfile_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF AtmProfile file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF AtmProfile file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF AtmProfile file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       ID_Tag:          Character string written into the ID_TAG global
!                        attribute field of the netCDF AtmProfile file.
!                        Should contain a short tag used to identify the
!                        profile set.
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
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the netCDF data read was successful.
!                        == FAILURE an unrecoverable error occurred.
!                        == WARNING an error occurred closing the netCDF
!                                   input file after a successful read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_AtmProfile_netCDF:     Function to open an AtmProfile netCDF file.
!
!       Inquire_AtmProfile_netCDF:  Function to inquire a netCDF format 
!                                   AtmProfile file to obtain information
!                                   about the data dimensions and attributes.
!
!       Allocate_AtmProfile:        Function to allocate the pointer members
!                                   of an AtmProfile structure.
!                                   SOURCE: ATMPROFILE_DEFINE module
!
!       Get_netCDF_Variable:        Function to read variable data from a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Close_AtmProfile_netCDF:    Function to close an AtmProfile netCDF file.
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Display_Message:            Subroutine to output Messages
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
!       Note the INTENT on the output AtmProfile argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_AtmProfile_netCDF( NC_Filename,  &  ! Input
                                   AtmProfile,   &  ! Output
                                   Quiet,        &  ! Optional input
                                   Reverse,      &  ! Optional input
                                   Title,        &  ! Optional output
                                   History,      &  ! Optional output
                                   Comment,      &  ! Optional output
                                   ID_Tag,       &  ! Optional output
                                   RCS_Id,       &  ! Revision control
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
    TYPE( AtmProfile_type ),  INTENT( IN OUT ) :: AtmProfile

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,        OPTIONAL, INTENT( IN )     :: Reverse

    ! -- Optional output
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: Comment
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: ID_Tag

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_AtmProfile_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 512 ) :: Message

    LOGICAL :: Noisy
    LOGICAL :: ProfileFlip

    INTEGER :: NC_FileID

    INTEGER :: Allocate_Status
    INTEGER :: NF90_Status
    INTEGER :: Close_Status

    INTEGER :: n_Layers
    INTEGER :: n_Absorbers
    INTEGER :: n_Profiles

    INTEGER :: j, k

    REAL( Double ), DIMENSION(:), ALLOCATABLE :: DateTime



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! ----------------
    ! Profile reversal
    ! ----------------

    ! -- Do not reverse the input profile arrays....
    ProfileFlip = .FALSE.

    ! -- ....unless the REVERSE keyword is set.
    IF ( PRESENT( Reverse ) ) THEN
      IF ( Reverse == SET ) ProfileFlip = .TRUE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- READ THE DIMENSION/ATTRIBUTE VALUES --                #
    !#--------------------------------------------------------------------------#

    Error_Status = Inquire_AtmProfile_netCDF( TRIM( NC_Filename ), &
                                              n_Layers    = n_Layers, &
                                              n_Absorbers = n_Absorbers, &
                                              n_Profiles  = n_Profiles, &
                                              Title   = Title, &
                                              History = History, &
                                              Comment = Comment, &
                                              ID_Tag  = ID_Tag, &
                                              Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining AtmProfile dimensions/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- ALLOCATE THE DATA STRUCTURE --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_AtmProfile( n_Layers, &
                                        n_Absorbers, &
                                        n_Profiles, &
                                        AtmProfile, &
                                        Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating AtmProfile structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- OPEN THE netCDF FILE FOR READING --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_AtmProfile_netCDF( TRIM( NC_FileNAME ), &
                                           NC_FileID, &
                                           Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF AtmProfile data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE PROFILE "INFO" DATA --                   #
    !#--------------------------------------------------------------------------#

    ! -----------------------
    ! The profile description
    ! -----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        DESCRIPTION_VARNAME, &
                                        AtmProfile%Description )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//DESCRIPTION_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    CALL Remove_NULL_Characters( AtmProfile%Description )


    ! ---------------------
    ! The climatology model
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CLIMATOLOGY_MODEL_VARNAME, &
                                        AtmProfile%Climatology_Model )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CLIMATOLOGY_MODEL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The Date and time
    ! -----------------

    ! -- Allocate the DateTime array
    ALLOCATE( DateTime( n_Profiles ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating temporary DateTime array. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Read the data
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        DATETIME_VARNAME, &
                                        DateTime )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//DATETIME_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Convert the Date and Time into the derived type
    CALL Convert_DateTime_to_Type( DateTime, AtmProfile%DateTime )

    ! -- deallocate the DateTime array
    DEALLOCATE( DateTime, STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error deallocating temporary DateTime array. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------
    ! Latitude
    ! --------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LATITUDE_VARNAME, &
                                        AtmProfile%Location%Latitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LATITUDE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------
    ! Longitude
    ! ---------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LONGITUDE_VARNAME, &
                                        AtmProfile%Location%Longitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LONGITUDE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! Surface Altitude
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SURFACE_ALTITUDE_VARNAME, &
                                        AtmProfile%Location%Surface_Altitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SURFACE_ALTITUDE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------
    ! Absorber ID
    ! -----------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        AtmProfile%Absorber_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//ABSORBER_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Absorber Units ID
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ABSORBER_UNITS_ID_VARNAME, &
                                        AtmProfile%Absorber_Units_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//ABSORBER_UNITS_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------------------
    ! Fill the other Absorber_Units structure members
    ! -----------------------------------------------

    DO j = 1, AtmProfile%n_Absorbers
      AtmProfile%Absorber_Units_Name( j )   = ATMPROFILE_ABSORBER_UNITS_NAME( AtmProfile%Absorber_Units_ID( j ) )
      AtmProfile%Absorber_Units_LBLRTM( j ) = ATMPROFILE_ABSORBER_UNITS_CHAR( AtmProfile%Absorber_Units_ID( j ) )
    END DO



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE PROFILE LEVEL DATA --                    #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Level pressure
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_PRESSURE_VARNAME, &
                                        AtmProfile%Level_Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LEVEL_PRESSURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Level temperature
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_TEMPERATURE_VARNAME, &
                                        AtmProfile%Level_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LEVEL_TEMPERATURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level absorber
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_ABSORBER_VARNAME, &
                                        AtmProfile%Level_Absorber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LEVEL_ABSORBER_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Level Altitude
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LEVEL_ALTITUDE_VARNAME, &
                                        AtmProfile%Level_Altitude )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LEVEL_ALTITUDE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- READ THE PROFILE LAYER DATA --                    #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Layer pressure
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LAYER_PRESSURE_VARNAME, &
                                        AtmProfile%Layer_Pressure )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LAYER_PRESSURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! Layer temperature
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LAYER_TEMPERATURE_VARNAME, &
                                        AtmProfile%Layer_Temperature )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LAYER_TEMPERATURE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! Layer absorber
    ! --------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LAYER_ABSORBER_VARNAME, &
                                        AtmProfile%Layer_Absorber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LAYER_ABSORBER_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! Layer Thickness
    ! ---------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        LAYER_DELTA_Z_VARNAME, &
                                        AtmProfile%Layer_Delta_Z )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//LAYER_DELTA_Z_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#          -- REVERSE THE PROFILE DATA DIRECTION IF REQUIRED --            #
    !#--------------------------------------------------------------------------#

    IF ( ProfileFlip ) THEN


      ! ----------
      ! Level data
      ! ----------

      k = AtmProfile%n_Levels
      AtmProfile%Level_Pressure    = AtmProfile%Level_Pressure(    k:1:-1, : )
      AtmProfile%Level_Temperature = AtmProfile%Level_Temperature( k:1:-1, : )
      AtmProfile%Level_Absorber    = AtmProfile%Level_Absorber(    k:1:-1, :, : )
      AtmProfile%Level_Altitude    = AtmProfile%Level_Altitude(    k:1:-1, : )


      ! ----------
      ! Layer data
      ! ----------

      k = AtmProfile%n_Layers
      AtmProfile%Layer_Pressure    = AtmProfile%Layer_Pressure(    k:1:-1, : )
      AtmProfile%Layer_Temperature = AtmProfile%Layer_Temperature( k:1:-1, : )
      AtmProfile%Layer_Absorber    = AtmProfile%Layer_Absorber(    k:1:-1, :, : )
      AtmProfile%Layer_Delta_Z     = AtmProfile%Layer_Delta_Z(     k:1:-1, : )

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_AtmProfile_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF AtmProfile data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Information_AtmProfile( AtmProfile, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_FileNAME )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_AtmProfile_netCDF

END MODULE AtmProfile_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AtmProfile_netCDF_IO.f90,v 4.3 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 4.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile_netCDF_IO.f90,v $
! Revision 4.3  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 4.2  2005/01/03 14:39:09  paulv
! - Removed header documentation delimiters from PRIVATE routines.
!
! Revision 4.1  2004/12/13 20:41:27  paulv
! - Removed declarations of unused variables.
!
! Revision 4.0  2004/11/02 20:13:02  paulv
! - New versions for modified AtmProfile structure.
!
! Revision 3.0  2004/08/27 14:32:41  paulv
! - Updated to Fortran95
! - New versions to handle derived type initialisation.
!
! Revision 2.3  2003/12/01 19:17:30  paulv
! - Updated header documentation.
!
! Revision 2.2  2003/11/17 19:49:58  paulv
! - Added QUIET optional argument to the Read() and Write() function. Setting
!   this optional argument suppresses output of information messages.
! - Added REVERSE optional argument to the Read() function. Setting this
!   optional argument reverses the order of the AtmProfile profile arrays in
!   the vertical.
!
! Revision 2.1  2003/07/21 20:03:12  paulv
! - Fixed bug in definition of the number of levels in the Create() routine.
!
! Revision 2.0  2003/05/23 20:59:25  paulv
! - New version. Entire database is now read/written rather than a profile
!   at a time.
! - Standardised the variable/attribute/dimension naming with parameters.
!
! Revision 1.6  2003/02/25 17:54:01  paulv
! - Updated documentation.
!
! Revision 1.5  2002/07/29 15:29:33  paulv
! - Removed parameters for:
!   o Maximum number of absorbers
!   o Maximum number of absorber units
!   o The list of absorber unit IDs
!   o The list of absorber unit names
!   o The list of corresponding absorber units LBLRTM specification
!   and placed them in the AtmProfile_Define module.
!
! Revision 1.4  2002/07/17 14:37:31  paulv
! - Added ID_TAG optional argument to the Create() and Inquire() functions.
!
! Revision 1.3  2002/07/12 19:02:19  paulv
! - Added Level_Pressure input argument and Layer_Pressure optional input
!   argument to the Create_AtmProfile_netCDF() function. Previously, the
!   pressure profiles were written in the Write_AtmProfile_netCDF() function
!   but since this dataset is designed for same-level profiles, writing the
!   same pressures for every profile is unnecessary. Note that if the
!   Layer_Pressure argument is not supplied, it is calculated from the input
!   Level_Pressure argument.
! - Added Level_Pressure and Layer_Pressure optional output arguments to the
!   Inquire_AtmProfile_netCDF() function.
! - Added RCS_Id optional output arguments to public functions.
! - Replaced NF90_INQ_VarID() and NF90_GET_VAR() function calls in the
!   Read_AtmProfile_netCDF() function with calls to the Get_NetCDF_Variable()
!   function...EXCEPT for the profile description as the Get() function is
!   not overloaded for character arguments.
! - Removed AtmProfile dimension check in Read_AtmProfile_netCDF(). The
!   AtmProfile structure should be empty on entry so the check will always
!   fail.
! - Corrected bug in referencing the Location and DateTime sub-structures
!   of the AtmProfile structure.
!
! Revision 1.2  2002/07/11 15:05:33  paulv
! - Finished modifying - all routines completed but untested.
!
! Revision 1.1  2002/07/09 21:08:16  paulv
! Initial checkin. Incomplete.
!
!
!
!
