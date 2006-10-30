!------------------------------------------------------------------------------
!M+
! NAME:
!       SpcCoeff_netCDF_IO
!
! PURPOSE:
!       Module containing routines to read and write netCDF format SpcCoeff
!       files.
!       
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SpcCoeff_netCDF_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       SpcCoeff_Define:       Module defining the SpcCoeff data structure and
!                              containing routines to manipulate it.
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
!
! CONTAINS:
!       Inquire_SpcCoeff_netCDF:  Function to inquire a netCDF format 
!                                 SpcCoeff file to obtain information
!                                 about the data dimensions and attributes.
!
!       Write_SpcCoeff_netCDF:    Function to write SpcCoeff data to a
!                                 netCDF format SpcCoeff file.
!
!       Read_SpcCoeff_netCDF:     Function to read SpcCoeff data from a
!                                 netCDF format SpcCoeff file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Dec-2002
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

MODULE SpcCoeff_netCDF_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE Message_Handler

  USE SpcCoeff_Define

  USE netcdf
  USE netCDF_Utility,  Open_SpcCoeff_netCDF =>  Open_netCDF, &
                      Close_SpcCoeff_netCDF => Close_netCDF


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_SpcCoeff_netCDF
  PUBLIC :: Write_SpcCoeff_netCDF
  PUBLIC :: Read_SpcCoeff_netCDF


  ! -------------------
  ! Procedure overloads
  ! -------------------

  INTERFACE Write_SpcCoeff_netCDF
    MODULE PROCEDURE Write_Spectral
    MODULE PROCEDURE Write_Sensor
  END INTERFACE Write_SpcCoeff_netCDF

  INTERFACE Read_SpcCoeff_netCDF
    MODULE PROCEDURE Read_Spectral
    MODULE PROCEDURE Read_Sensor
  END INTERFACE Read_SpcCoeff_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: SpcCoeff_netCDF_IO.f90,v 6.6 2006/05/02 16:58:02 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Invalid dimension value
  INTEGER, PRIVATE, PARAMETER :: INVALID_DIMENSION_VALUE = -1

  ! -- Global attribute names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: TITLE_GATTNAME         = 'title' 
  CHARACTER( * ), PRIVATE, PARAMETER :: HISTORY_GATTNAME       = 'history' 
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_NAME_GATTNAME   = 'sensor_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: PLATFORM_NAME_GATTNAME = 'platform_name' 
  CHARACTER( * ), PRIVATE, PARAMETER :: COMMENT_GATTNAME       = 'comment' 
  CHARACTER( * ), PRIVATE, PARAMETER :: FILE_TYPE_GATTNAME     = 'File_Type'

  ! -- Dimension names. Case sensitive
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_DIMNAME              = 'n_channels'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_DIMNAME                 = 'n_nodes'
  CHARACTER( * ), PRIVATE, PARAMETER :: MAX_CHANNEL_PER_NODE_DIMNAME = 'max_channels_per_node'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_DESCRIPTOR_DIMNAME    = 'sdsl'

  ! -- Variable names. Case sensitive.
  CHARACTER( * ), PRIVATE, PARAMETER :: RELEASE_VARNAME             = 'Release'
  CHARACTER( * ), PRIVATE, PARAMETER :: VERSION_VARNAME             = 'Version'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_DESCRIPTOR_VARNAME   = 'Sensor_Descriptor'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_TYPE_VARNAME         = 'Sensor_Type'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_VARNAME      = 'NCEP_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_VARNAME    = 'WMO_Satellite_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_VARNAME       = 'WMO_Sensor_ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_CHANNEL_VARNAME      = 'Sensor_Channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_VARNAME           = 'frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: WAVENUMBER_VARNAME          = 'wavenumber'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C1_VARNAME           = 'planck_c1'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C2_VARNAME           = 'planck_c2'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C1_VARNAME             = 'band_c1'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C2_VARNAME             = 'band_c2'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLARIZATION_VARNAME        = 'polarization'
  CHARACTER( * ), PRIVATE, PARAMETER :: CBR_VARNAME                 = 'cosmic_background_radiance'
  CHARACTER( * ), PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_VARNAME    = 'is_solar_channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_VARNAME    = 'solar_irradiance'

  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_VARNAME = 'MW_and_IR_Channel_Index'
  CHARACTER( * ), PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_VARNAME = 'n_Channels_per_Node'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_VARNAME    = 'Channel_Node_Map'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_VARNAME    = 'MW_and_IR_Node_Index'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_FREQUENCY_VARNAME      = 'Node_Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_WAVENUMBER_VARNAME     = 'Node_Wavenumber'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C1_VARNAME      = 'Node_Planck_C1'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C2_VARNAME      = 'Node_Planck_C2'


  ! -- Description attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: DESC_ATTNAME = 'description'

  CHARACTER( * ), PRIVATE, PARAMETER :: RELEASE_DESC              = &
'Release number of SpcCoeff data file'
  CHARACTER( * ), PRIVATE, PARAMETER :: VERSION_DESC              = &
'Version number of SpcCoeff data file'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_DESCRIPTOR_DESC    = &
'Short text string containing the sensor/satellite description'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_TYPE_DESC          = &
'Sensor type flag. Used to identify microwave, infrared, and visible sensor channels'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_DESC       = &
'ID used at NOAA/NCEP/EMC to identify a satellite/sensor (-1 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_DESC     = &
'WMO code for identifying satellite platforms (1023 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_DESC        = &
'WMO code for identifying a satellite sensor (2047 == none available)'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_CHANNEL_DESC       = &
'List of sensor channel numbers associated with the SpcCoeff data'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_DESC            = &
'Channel central frequency, f'
  CHARACTER( * ), PRIVATE, PARAMETER :: WAVENUMBER_DESC           = &
'Channel central wavenumber, v'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C1_DESC            = &
'First Planck coefficient, c1.v^3, for channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C2_DESC            = &
'Second Planck coefficient, c2.v, for channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C1_DESC              = &
'Polychromatic band correction offset'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C2_DESC              = &
'Polychromatic band correction slope'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLARIZATION_DESC         = &
'Polarization type flag. Describes the channel polarization.'
  CHARACTER( * ), PRIVATE, PARAMETER :: CBR_DESC                  = &
'Planck radiance for the cosmic background temperature'
  CHARACTER( * ), PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_DESC     = &
'Flag indicating if a channel is affected by solar source (0=no, 1=yes)'
  CHARACTER( * ), PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_DESC     = &
'TOA solar irradiance using Kurucz spectrum'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_DESC  = &
'The microwave and infrared channel indices used internally by the OSS MW and IR modules'
  CHARACTER( * ), PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_DESC  = &
'Number of channels represented by a node'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_DESC     = &
'Indexing array to map monochromatic nodes to sensor channels'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_DESC     = &
'The microwave and infrared node indices used internally by the OSS MW and IR modules'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_FREQUENCY_DESC       = &
'Node frequency, f'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_WAVENUMBER_DESC      = &
'Node wavenumber, v'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C1_DESC       = &
'First Planck coefficient, c1.v^3, for node'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C2_DESC       = &
'Second Planck coefficient, c2.v, for node'


  ! -- Long name attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_DESCRIPTOR_LONGNAME    = 'Sensor Descriptor'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_TYPE_LONGNAME          = 'Sensor Type'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_LONGNAME       = 'NCEP Sensor ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_LONGNAME     = 'WMO Satellite ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_LONGNAME        = 'WMO Sensor ID'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_CHANNEL_LONGNAME       = 'Sensor Channel'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_LONGNAME            = 'Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: WAVENUMBER_LONGNAME           = 'Wavenumber'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C1_LONGNAME            = 'Planck C1'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C2_LONGNAME            = 'Planck C2'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C1_LONGNAME              = 'Band C1'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C2_LONGNAME              = 'Band C2'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLARIZATION_LONGNAME         = 'Polarization type flag'
  CHARACTER( * ), PRIVATE, PARAMETER :: CBR_LONGNAME                  = 'Cosmic Background Radiance'
  CHARACTER( * ), PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_LONGNAME     = 'Solar channel flag'
  CHARACTER( * ), PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_LONGNAME     = 'Kurucz Solar Irradiance'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_LONGNAME  = 'OSS internal MW and IR Channel Index'
  CHARACTER( * ), PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_LONGNAME  = 'Number of channels per Node'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_LONGNAME     = 'Channel<->Node indexing map'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_LONGNAME     = 'OSS internal MW and IR Node Index'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_FREQUENCY_LONGNAME       = 'Node Frequency'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_WAVENUMBER_LONGNAME      = 'Node Wavenumber'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C1_LONGNAME       = 'Node Planck C1'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C2_LONGNAME       = 'Node Planck C2'



  ! -- Variable units attribute.
  CHARACTER( * ), PRIVATE, PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_TYPE_UNITS          = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_UNITS        = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: SENSOR_CHANNEL_UNITS       = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: FREQUENCY_UNITS            = 'Gigahertz (GHz)'
  CHARACTER( * ), PRIVATE, PARAMETER :: WAVENUMBER_UNITS           = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C1_UNITS            = 'mW/(m^2.sr.cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: PLANCK_C2_UNITS            = 'Kelvin (K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C1_UNITS              = 'Kelvin (K)'
  CHARACTER( * ), PRIVATE, PARAMETER :: BAND_C2_UNITS              = 'K/K'
  CHARACTER( * ), PRIVATE, PARAMETER :: POLARIZATION_UNITS         = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: CBR_UNITS                  = 'mW/(m^2.sr.cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_UNITS     = 'mW/(m^2.cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_UNITS  = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_UNITS  = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_UNITS     = 'N/A'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_FREQUENCY_UNITS       = 'Gigahertz (GHz)'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_WAVENUMBER_UNITS      = 'Inverse centimetres (cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C1_UNITS       = 'mW/(m^2.sr.cm^-1)'
  CHARACTER( * ), PRIVATE, PARAMETER :: NODE_PLANCK_C2_UNITS       = 'Kelvin (K)'

  ! -- Variable _FillValue attribute.
  CHARACTER( * ),  PRIVATE, PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER,         PRIVATE, PARAMETER :: IP_FILLVALUE = -1
  REAL( Double ),  PRIVATE, PARAMETER :: FP_FILLVALUE = -1.0_Double

  INTEGER( Long ), PRIVATE, PARAMETER :: SENSOR_TYPE_FILLVALUE         = INVALID_SENSOR
  INTEGER( Long ), PRIVATE, PARAMETER :: NCEP_SENSOR_ID_FILLVALUE      = INVALID_NCEP_SENSOR_ID      
  INTEGER( Long ), PRIVATE, PARAMETER :: WMO_SATELLITE_ID_FILLVALUE    = INVALID_WMO_SATELLITE_ID    
  INTEGER( Long ), PRIVATE, PARAMETER :: WMO_SENSOR_ID_FILLVALUE       = INVALID_WMO_SENSOR_ID       
  INTEGER( Long ), PRIVATE, PARAMETER :: SENSOR_CHANNEL_FILLVALUE      = IP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: FREQUENCY_FILLVALUE           = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: WAVENUMBER_FILLVALUE          = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: PLANCK_C1_FILLVALUE           = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: PLANCK_C2_FILLVALUE           = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: BAND_C1_FILLVALUE             = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: BAND_C2_FILLVALUE             = FP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: POLARIZATION_FILLVALUE        = INVALID_POLARIZATION        
  REAL( Double ),  PRIVATE, PARAMETER :: CBR_FILLVALUE                 = FP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_FILLVALUE    = IP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_FILLVALUE    = FP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_FILLVALUE = IP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_FILLVALUE = IP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_FILLVALUE    = IP_FILLVALUE
  INTEGER( Long ), PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_FILLVALUE    = IP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: NODE_FREQUENCY_FILLVALUE      = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: NODE_WAVENUMBER_FILLVALUE     = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: NODE_PLANCK_C1_FILLVALUE      = FP_FILLVALUE
  REAL( Double ),  PRIVATE, PARAMETER :: NODE_PLANCK_C2_FILLVALUE      = FP_FILLVALUE


  ! -- Variable netCDF datatypes
  INTEGER, PRIVATE, PARAMETER :: RELEASE_TYPE              = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: VERSION_TYPE              = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: SENSOR_DESCRIPTOR_TYPE    = NF90_CHAR
  INTEGER, PRIVATE, PARAMETER :: SENSOR_TYPE_TYPE          = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: NCEP_SENSOR_ID_TYPE       = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: WMO_SATELLITE_ID_TYPE     = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: WMO_SENSOR_ID_TYPE        = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: SENSOR_CHANNEL_TYPE       = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: FREQUENCY_TYPE            = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: WAVENUMBER_TYPE           = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: PLANCK_C1_TYPE            = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: PLANCK_C2_TYPE            = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: BAND_C1_TYPE              = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: BAND_C2_TYPE              = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: POLARIZATION_TYPE         = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: CBR_TYPE                  = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: IS_SOLAR_CHANNEL_TYPE     = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: SOLAR_IRRADIANCE_TYPE     = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: MW_IR_CHANNEL_INDEX_TYPE  = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: N_CHANNELS_PER_NODE_TYPE  = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: CHANNEL_NODE_MAP_TYPE     = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: MW_IR_NODE_INDEX_TYPE     = NF90_INT
  INTEGER, PRIVATE, PARAMETER :: NODE_FREQUENCY_TYPE       = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: NODE_WAVENUMBER_TYPE      = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: NODE_PLANCK_C1_TYPE       = NF90_DOUBLE
  INTEGER, PRIVATE, PARAMETER :: NODE_PLANCK_C2_TYPE       = NF90_DOUBLE


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
!       Write_SpcCoeff_GAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF SpcCoeff data file.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SpcCoeff_GAtts( NC_Filename,                   &  ! Input
!                                            NC_FileID,                     &  ! Input
!                                            Title         = Title,         &  ! Optional input
!                                            History       = History,       &  ! Optional input
!                                            Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                            Platform_Name = Platform_Name, &  ! Optional input
!                                            Comment       = Comment,       &  ! Optional input
!                                            File_Type     = File_Type,     &  ! Optional input
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SpcCoeff format data file to write to.
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
!                         attribute field of the netCDF SpcCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF SpcCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF SpcCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       File_Type:        An integer flag identifying the type of SpcCoeff
!                         (sensor or spectral) data in the file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
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
!       None.
!
! COMMENTS:
!       The netCDF file remains in DEFINE mode upon exiting this function.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Write_SpcCoeff_GAtts( NC_Filename,   &  ! Input
                                 NC_FileID,     &  ! Input
                                 Title,         &  ! Optional input
                                 History,       &  ! Optional input
                                 Sensor_Name,   &  ! Optional input
                                 Platform_Name, &  ! Optional input
                                 Comment,       &  ! Optional input
                                 File_Type,     &  ! Optional input
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
    INTEGER,        OPTIONAL, INTENT( IN ) :: File_Type

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_GAtts'

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


    ! -------------
    ! The file type
    ! -------------

    IF ( PRESENT( File_Type ) ) THEN

      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  FILE_TYPE_GATTNAME, &
                                  File_Type )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error writing '//FILE_TYPE_GATTNAME//' attribute to '//&
                              TRIM( NC_Filename )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Write_SpcCoeff_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Read_SpcCoeff_GAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF SpcCoeff data file.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SpcCoeff_GAtts( NC_Filename,                   &  ! Input
!                                           NC_FileID,                     &  ! Input
!                                           Title         = Title,         &  ! Optional output
!                                           History       = History,       &  ! Optional output
!                                           Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                           Platform_Name = Platform_Name, &  ! Optional output
!                                           Comment       = Comment,       &  ! Optional output
!                                           File_Type     = File_Type,     &  ! Optional output
!                                           Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF SpcCoeff format data file to read.
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
!                         attribute field of the netCDF SpcCoeff file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Sensor_Name:      Character string written into the SENSOR_NAME
!                         global attribute field of the netCDF SpcCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Platform_Name:    Character string written into the PLATFORM_NAME
!                         global attribute field of the netCDF SpcCoeff
!                         file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF SpcCoeff file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       File_Type:        An integer flag identifying the type of SpcCoeff
!                         (sensor or spectral) data in the file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
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
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Read_SpcCoeff_GAtts( NC_Filename,   &  ! Input
                                NC_FileID,     &  ! Input
                                Title,         &  ! Optional output
                                History,       &  ! Optional output
                                Sensor_Name,   &  ! Optional output
                                Platform_Name, &  ! Optional output
                                Comment,       &  ! Optional output
                                File_Type,     &  ! Optional output
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
    INTEGER,        OPTIONAL, INTENT( OUT ) :: File_Type

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_GAtts'


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


    ! ---------
    ! File type
    ! ---------

    IF ( PRESENT( File_Type ) ) THEN

      Error_Status = Get_netCDF_Attribute( NC_FileID, &
                                           FILE_TYPE_GATTNAME, &
                                           File_Type, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = WARNING
        CALL Display_Message( ROUTINE_NAME, &
                              'Error reading '//FILE_TYPE_GATTNAME//&
                              ' attribute from '//TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF

    END IF

  END FUNCTION Read_SpcCoeff_GAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Create_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to create the netCDF format SpcCoeff file
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Create_SpcCoeff_netCDF( &
!                        NC_Filename,                                   &  ! Input
!                        Release,                                       &  ! Input
!                        Version,                                       &  ! Input
!                        n_Channels,                                    &  ! Input
!                        n_Nodes,              = n_Nodes,               &  ! Optional input
!                        Max_Channels_per_Node = Max_Channels_per_Node, &  ! Optional input
!                        Title                 = Title,                 &  ! Optional input
!                        History               = History,               &  ! Optional input
!                        Sensor_Name           = Sensor_Name,           &  ! Optional input
!                        Platform_Name         = Platform_Name,         &  ! Optional input
!                        Comment               = Comment,               &  ! Optional input
!                        Message_Log           = Message_Log            )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:            Character string specifying the name of the
!                               netCDF SpcCoeff format data file to write to.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       Release:                The file release number.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       Version:                The file version number
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       n_Channels:             The number of sensor channels dimension
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       n_Nodes:                The number of monochromatic nodes dimension.
!                               Along with the Max_Channels_per_Node optional
!                               argument, this argument is used to specify that
!                               the output SpcCoeff file will be a SPECTRAL file,
!                               rather than a SENSOR file (default).
!                               ** NOTE: An error is flagged if this argument is
!                               present but the Max_Channels_per_Node argument
!                               is not.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       Max_Channels_per_Node:  The number of monochromatic nodes dimension.
!                               Along with the n_Nodes optional argument,
!                               this argument is used to specify that the
!                               output SpcCoeff file will be a SPECTRAL file,
!                               rather than a SENSOR file (default).
!                               ** NOTE: An error is flagged if this argument
!                               is present but the n_Nodes argument is not.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN )
!
!       Title:                  Character string written into the TITLE global
!                               attribute field of the netCDF SpcCoeff file.
!                               Should contain a succinct description of what
!                               is in the netCDF datafile.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:                Character string written into the HISTORY global
!                               attribute field of the netCDF SpcCoeff file.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:            Character string written into the SENSOR_NAME
!                               global attribute field of the netCDF SpcCoeff
!                               file.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:          Character string written into the PLATFORM_NAME
!                               global attribute field of the netCDF SpcCoeff
!                               file.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:                Character string written into the COMMENT global
!                               attribute field of the netCDF SpcCoeff file.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       File_Type:              An integer flag identifying the type of SpcCoeff
!                               (sensor or spectral) data in the file.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:            Character string specifying a filename in which
!                               any messages will be logged. If not specified,
!                               or if an error occurs opening the log file, the
!                               default action is to output messages to standard
!                               output.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the error status.
!                               The error codes are defined in the ERROR_HANDLER module.
!                               If == SUCCESS the file creation was successful
!                                  == FAILURE an unreoverable error occurred.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! CALLS:
!       Write_SpcCoeff_GAtts:  Function to write the global attributes to
!                              a netCDF format SpcCoeff data file.
!
!       Close_SpcCoeff_netCDF: Function to close a netCDF format SpcCoeff
!                              data file with error checking.
!
!       NF90_CREATE:           Function to create a netCDF data file and
!                              place it in DEFINE mode.
!                              SOURCE: netCDF library
!
!       NF90_DEF_DIM:          Function to define a dimension in a netCDF
!                              data file.
!                              SOURCE: netCDF library
!
!       NF90_PUT_ATT:          Function to write attribute data to a netCDF
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
!       NF90_CLOSE:            Function to close a netCDF file.
!                              SOURCE: netCDF library
!
!       Put_netCDF_Variable:   Function to write variable data to a
!                              netCDF data file.
!                              SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Display_Message:       Subroutine to output messages
!                              SOURCE: ERROR_HANDLER module
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
!       If the output file already exists, it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Create_SpcCoeff_netCDF( NC_Filename,           &  ! Input
                                   Release,               &  ! Input
                                   Version,               &  ! Input
                                   n_Channels,            &  ! Input
                                   n_Nodes,               &  ! Optional input
                                   Max_Channels_per_Node, &  ! Optional input
                                   Title,                 &  ! Optional input
                                   History,               &  ! Optional input
                                   Sensor_Name,           &  ! Optional input
                                   Platform_Name,         &  ! Optional input
                                   Comment,               &  ! Optional input
                                   RCS_Id,                &  ! Version control
                                   Message_Log )          &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN )  :: NC_Filename
    INTEGER,                      INTENT( IN )  :: Release
    INTEGER,                      INTENT( IN )  :: Version
    INTEGER,                      INTENT( IN )  :: n_Channels

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )  :: n_Nodes
    INTEGER,            OPTIONAL, INTENT( IN )  :: Max_Channels_per_Node
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Comment

    ! -- Version control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Create_SpcCoeff_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    TYPE( SpcCoeff_Sensor_type ) :: Dummy

    INTEGER :: File_Type
    INTEGER :: NF90_Status
    INTEGER :: Desc_Status
    INTEGER :: Longname_Status  
    INTEGER :: Units_Status     
    INTEGER :: Fillvalue_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: Channel_DimID
    INTEGER :: Node_DimID
    INTEGER :: Max_Channels_per_Node_DimID
    INTEGER :: StrLen_DimID
    INTEGER :: varID

    INTEGER :: Sensor_Type_DimID  
    INTEGER :: Frequency_DimID    
    INTEGER :: Wavenumber_DimID   
    INTEGER :: Planck_C1_DimID    
    INTEGER :: Planck_C2_DimID    
    INTEGER :: Polarization_DimID 
    INTEGER :: CBR_DimID          
    INTEGER :: SI_DimID          



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- DETERMINE THE OUTPUT DATA FILE TYPE --                 #
    !#--------------------------------------------------------------------------#

    ! -- Create a Sensor file...
    File_Type = SENSOR_FILE_TYPE

    ! -- ....unless the n_Nodes and Max_Channels_per_Node
    ! -- ....arguments are passed
    IF ( PRESENT( n_Nodes ) .AND. PRESENT( Max_Channels_per_Node ) ) THEN
      File_Type = SPECTRAL_FILE_TYPE
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------------
    ! Double check that BOTH n_Nodes and Max_Channels_per_Node
    ! have been specified or NEITHER of them has been.
    ! --------------------------------------------------------

    IF ( PRESENT( n_Nodes ) .NEQV. PRESENT( Max_Channels_per_Node ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'For the n_Nodes and Max_Channels_per_Node optional '//&
                            'arguments, BOTH or NEITHER of them must be passed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------------------
    ! The channel dimension (common to sensor and spectral files)
    ! -----------------------------------------------------------

    IF ( n_Channels < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'N_CHANNELS dimension must be > 0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------
    ! The spectral dimensions
    ! -----------------------

    IF ( File_Type == SPECTRAL_FILE_TYPE ) THEN

      IF ( n_Nodes < 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'N_NODES dimension must be > 0', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      IF ( Max_Channels_per_Node < 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'MAX_CHANNELS_PER_NODE dimension must be > 0', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


    END IF



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
                            TRIM( NC_FileNAME )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------
    ! The spectral dimensions
    ! -----------------------

    IF ( File_Type == SPECTRAL_FILE_TYPE ) THEN

      NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                  NODE_DIMNAME, &
                                  n_Nodes, &
                                  Node_DimID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining the '//NODE_DIMNAME//' dimension in '// &
                              TRIM( NC_FileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

      NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                  MAX_CHANNEL_PER_NODE_DIMNAME, &
                                  Max_Channels_per_Node, &
                                  Max_Channels_per_Node_DimID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error defining the '//MAX_CHANNEL_PER_NODE_DIMNAME//&
                              ' dimension in '//TRIM( NC_FileNAME )//' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF

    END IF


    ! ------------------------------------
    ! The sensor description string length
    ! ------------------------------------

    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                SENSOR_DESCRIPTOR_DIMNAME, &
                                Dummy%Sensor_Descriptor_StrLen, &
                                StrLen_DimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//SENSOR_DESCRIPTOR_DIMNAME//' dimension in '// &
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

    Error_Status = Write_SpcCoeff_GAtts( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Title         = Title, &
                                         History       = History, &
                                         Sensor_Name   = Sensor_Name, &
                                         Platform_Name = Platform_Name, &
                                         Comment       = Comment, &
                                         File_Type     = File_Type, &
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
    !#               -- DEFINE THE RELEASE AND VERSION VARIABLES --             #
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
    Desc_Status = Put_netCDF_Attribute( NC_FileID, &
                                        DESC_ATTNAME, &
                                        RELEASE_DESC, &
                                        Variable_Name = RELEASE_VARNAME )

    IF ( Desc_Status /= SUCCESS ) THEN
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

    ! -- Define the version variable
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
    Desc_Status = Put_netCDF_Attribute( NC_FileID, &
                                        DESC_ATTNAME, &
                                        VERSION_DESC, &
                                        Variable_Name = VERSION_VARNAME )

    IF ( Desc_Status /= SUCCESS ) THEN
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



    !#--------------------------------------------------------------------------#
    !#             -- DEFINE THE SENSOR/SPECTRAL COMMON VARIABLES --            #
    !#--------------------------------------------------------------------------#

    ! -----------------
    ! Sensor descriptor
    ! -----------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_DESCRIPTOR_VARNAME, &
                                SENSOR_DESCRIPTOR_TYPE, &
                                dimids = (/ StrLen_DimID, Channel_DimID /), &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SENSOR_DESCRIPTOR_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status     = Put_netCDF_Attribute( NC_FileID, &
                                            DESC_ATTNAME, &
                                            SENSOR_DESCRIPTOR_DESC, &
                                            Variable_Name = SENSOR_DESCRIPTOR_VARNAME )
    Longname_Status = Put_netCDF_Attribute( NC_FileID, &
                                            LONGNAME_ATTNAME, &
                                            SENSOR_DESCRIPTOR_LONGNAME, &
                                            Variable_Name = SENSOR_DESCRIPTOR_VARNAME )

    IF ( Desc_Status     /= SUCCESS .OR. &
         Longname_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_DESCRIPTOR_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------
    ! Sensor type
    ! -----------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_TYPE_VARNAME, &
                                SENSOR_TYPE_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SENSOR_TYPE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             SENSOR_TYPE_DESC, &
                                             Variable_Name = SENSOR_TYPE_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             SENSOR_TYPE_LONGNAME, &
                                             Variable_Name = SENSOR_TYPE_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             SENSOR_TYPE_UNITS, &
                                             Variable_Name = SENSOR_TYPE_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             SENSOR_TYPE_FILLVALUE, &
                                             Variable_Name = SENSOR_TYPE_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_TYPE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------
    ! NCEP Sensor ID
    ! --------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                NCEP_SENSOR_ID_VARNAME, &
                                NCEP_SENSOR_ID_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

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
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             NCEP_SENSOR_ID_DESC, &
                                             Variable_Name = NCEP_SENSOR_ID_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             NCEP_SENSOR_ID_LONGNAME, &
                                             Variable_Name = NCEP_SENSOR_ID_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             NCEP_SENSOR_ID_UNITS, &
                                             Variable_Name = NCEP_SENSOR_ID_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             NCEP_SENSOR_ID_FILLVALUE, &
                                             Variable_Name = NCEP_SENSOR_ID_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
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
                                dimids = Channel_DimID, &
                                varid  = VarID )

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
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             WMO_SATELLITE_ID_DESC, &
                                             Variable_Name = WMO_SATELLITE_ID_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             WMO_SATELLITE_ID_LONGNAME, &
                                             Variable_Name = WMO_SATELLITE_ID_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             WMO_SATELLITE_ID_UNITS, &
                                             Variable_Name = WMO_SATELLITE_ID_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             WMO_SATELLITE_ID_FILLVALUE, &
                                             Variable_Name = WMO_SATELLITE_ID_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
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
                                dimids = Channel_DimID, &
                                varid  = VarID )

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
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             WMO_SENSOR_ID_DESC, &
                                             Variable_Name = WMO_SENSOR_ID_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             WMO_SENSOR_ID_LONGNAME, &
                                             Variable_Name = WMO_SENSOR_ID_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             WMO_SENSOR_ID_UNITS, &
                                             Variable_Name = WMO_SENSOR_ID_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             WMO_SENSOR_ID_FILLVALUE, &
                                             Variable_Name = WMO_SENSOR_ID_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! Sensor channel list
    ! -------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_CHANNEL_VARNAME, &
                                SENSOR_CHANNEL_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             SENSOR_CHANNEL_DESC, &
                                             Variable_Name = SENSOR_CHANNEL_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             SENSOR_CHANNEL_LONGNAME, &
                                             Variable_Name = SENSOR_CHANNEL_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             SENSOR_CHANNEL_UNITS, &
                                             Variable_Name = SENSOR_CHANNEL_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             SENSOR_CHANNEL_FILLVALUE, &
                                             Variable_Name = SENSOR_CHANNEL_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------
    ! The channel centre frequency
    ! ----------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                FREQUENCY_VARNAME, &
                                FREQUENCY_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//FREQUENCY_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             FREQUENCY_DESC, &
                                             Variable_Name = FREQUENCY_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             FREQUENCY_LONGNAME, &
                                             Variable_Name = FREQUENCY_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             FREQUENCY_UNITS, &
                                             Variable_Name = FREQUENCY_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             FREQUENCY_FILLVALUE, &
                                             Variable_Name = FREQUENCY_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The channel centre wavenumber
    ! -----------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                WAVENUMBER_VARNAME, &
                                WAVENUMBER_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//WAVENUMBER_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             WAVENUMBER_DESC, &
                                             Variable_Name = WAVENUMBER_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             WAVENUMBER_LONGNAME, &
                                             Variable_Name = WAVENUMBER_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             WAVENUMBER_UNITS, &
                                             Variable_Name = WAVENUMBER_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             WAVENUMBER_FILLVALUE, &
                                             Variable_Name = WAVENUMBER_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WAVENUMBER_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------------
    ! The channel first Planck function coefficient
    ! ---------------------------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PLANCK_C1_VARNAME, &
                                PLANCK_C1_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PLANCK_C1_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             PLANCK_C1_DESC, &
                                             Variable_Name = PLANCK_C1_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             PLANCK_C1_LONGNAME, &
                                             Variable_Name = PLANCK_C1_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             PLANCK_C1_UNITS, &
                                             Variable_Name = PLANCK_C1_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             PLANCK_C1_FILLVALUE, &
                                             Variable_Name = PLANCK_C1_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C1_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------------
    ! The channel second Planck function coefficient
    ! ----------------------------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PLANCK_C2_VARNAME, &
                                PLANCK_C2_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//PLANCK_C2_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             PLANCK_C2_DESC, &
                                             Variable_Name = PLANCK_C2_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             PLANCK_C2_LONGNAME, &
                                             Variable_Name = PLANCK_C2_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             PLANCK_C2_UNITS, &
                                             Variable_Name = PLANCK_C2_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             PLANCK_C2_FILLVALUE, &
                                             Variable_Name = PLANCK_C2_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C2_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------------
    ! Polychromatic band correction offset
    ! ------------------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BAND_C1_VARNAME, &
                                BAND_C1_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BAND_C1_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             BAND_C1_DESC, &
                                             Variable_Name = BAND_C1_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             BAND_C1_LONGNAME, &
                                             Variable_Name = BAND_C1_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             BAND_C1_UNITS, &
                                             Variable_Name = BAND_C1_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             BAND_C1_FILLVALUE, &
                                             Variable_Name = BAND_C1_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C1_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------
    ! Polychromatic band correction slope
    ! -----------------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                BAND_C2_VARNAME, &
                                BAND_C2_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//BAND_C2_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             BAND_C2_DESC, &
                                             Variable_Name = BAND_C2_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             BAND_C2_LONGNAME, &
                                             Variable_Name = BAND_C2_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             BAND_C2_UNITS, &
                                             Variable_Name = BAND_C2_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             BAND_C2_FILLVALUE, &
                                             Variable_Name = BAND_C2_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C2_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel polarization
    ! ------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                POLARIZATION_VARNAME, &
                                POLARIZATION_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//POLARIZATION_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             POLARIZATION_DESC, &
                                             Variable_Name = POLARIZATION_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             POLARIZATION_LONGNAME, &
                                             Variable_Name = POLARIZATION_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             POLARIZATION_UNITS, &
                                             Variable_Name = POLARIZATION_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             POLARIZATION_FILLVALUE, &
                                             Variable_Name = POLARIZATION_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//POLARIZATION_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! Solar channel flag
    ! ------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                IS_SOLAR_CHANNEL_VARNAME, &
                                IS_SOLAR_CHANNEL_TYPE, &
                                dimids = Channel_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//IS_SOLAR_CHANNEL_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             IS_SOLAR_CHANNEL_DESC, &
                                             Variable_Name = IS_SOLAR_CHANNEL_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             IS_SOLAR_CHANNEL_LONGNAME, &
                                             Variable_Name = IS_SOLAR_CHANNEL_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             IS_SOLAR_CHANNEL_UNITS, &
                                             Variable_Name = IS_SOLAR_CHANNEL_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             IS_SOLAR_CHANNEL_FILLVALUE, &
                                             Variable_Name = IS_SOLAR_CHANNEL_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//IS_SOLAR_CHANNEL_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#         -- DEFINE THE SENSOR/SPECTRAL SPECIFIC DIMENSION IDs --          #
    !#         --          AND THE SPECTRAL-ONLY VARIABLES          --          #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( File_Type )

      CASE ( SENSOR_FILE_TYPE )

        ! --------------------------------
        ! Define the CHANNEL dimension for
        ! the cosmic background radiance
        ! and solar irradiance
        ! --------------------------------

        CBR_DimID = Channel_DimID
        SI_DimID  = Channel_DimID

      CASE ( SPECTRAL_FILE_TYPE )

        ! ------------------------------
        ! Define the NODE dimension for
        ! the cosmic background radiance
        ! and solar irradiance
        ! ------------------------------

        CBR_DimID = Node_DimID
        SI_DimID  = Node_DimID


        ! -------------------------------------
        ! Define the OSS internal microwave and
        ! infrared channel index array
        ! -------------------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    MW_IR_CHANNEL_INDEX_VARNAME, &
                                    MW_IR_CHANNEL_INDEX_TYPE, &
                                    dimids = Channel_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//MW_IR_CHANNEL_INDEX_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 MW_IR_CHANNEL_INDEX_DESC, &
                                                 Variable_Name = MW_IR_CHANNEL_INDEX_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 MW_IR_CHANNEL_INDEX_LONGNAME, &
                                                 Variable_Name = MW_IR_CHANNEL_INDEX_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 MW_IR_CHANNEL_INDEX_UNITS, &
                                                 Variable_Name = MW_IR_CHANNEL_INDEX_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 MW_IR_CHANNEL_INDEX_FILLVALUE, &
                                                 Variable_Name = MW_IR_CHANNEL_INDEX_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//MW_IR_CHANNEL_INDEX_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -----------------------------------------------
        ! Define the number of channels per node variable
        ! -----------------------------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    N_CHANNELS_PER_NODE_VARNAME, &
                                    N_CHANNELS_PER_NODE_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//N_CHANNELS_PER_NODE_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 N_CHANNELS_PER_NODE_DESC, &
                                                 Variable_Name = N_CHANNELS_PER_NODE_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 N_CHANNELS_PER_NODE_LONGNAME, &
                                                 Variable_Name = N_CHANNELS_PER_NODE_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 N_CHANNELS_PER_NODE_UNITS, &
                                                 Variable_Name = N_CHANNELS_PER_NODE_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 N_CHANNELS_PER_NODE_FILLVALUE, &
                                                 Variable_Name = N_CHANNELS_PER_NODE_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//N_CHANNELS_PER_NODE_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! -----------------------------
        ! Define the channel<->node map
        ! -----------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    CHANNEL_NODE_MAP_VARNAME, &
                                    CHANNEL_NODE_MAP_TYPE, &
                                    dimids = (/ Max_Channels_per_Node_DimID, &
                                                Node_DimID /), &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//CHANNEL_NODE_MAP_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 CHANNEL_NODE_MAP_DESC, &
                                                 Variable_Name = CHANNEL_NODE_MAP_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 CHANNEL_NODE_MAP_LONGNAME, &
                                                 Variable_Name = CHANNEL_NODE_MAP_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 CHANNEL_NODE_MAP_UNITS, &
                                                 Variable_Name = CHANNEL_NODE_MAP_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 CHANNEL_NODE_MAP_FILLVALUE, &
                                                 Variable_Name = CHANNEL_NODE_MAP_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//CHANNEL_NODE_MAP_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! -------------------------------------
        ! Define the OSS internal microwave and
        ! infrared node index array
        ! -------------------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    MW_IR_NODE_INDEX_VARNAME, &
                                    MW_IR_NODE_INDEX_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//MW_IR_NODE_INDEX_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 MW_IR_NODE_INDEX_DESC, &
                                                 Variable_Name = MW_IR_NODE_INDEX_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 MW_IR_NODE_INDEX_LONGNAME, &
                                                 Variable_Name = MW_IR_NODE_INDEX_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 MW_IR_NODE_INDEX_UNITS, &
                                                 Variable_Name = MW_IR_NODE_INDEX_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 MW_IR_NODE_INDEX_FILLVALUE, &
                                                 Variable_Name = MW_IR_NODE_INDEX_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//MW_IR_NODE_INDEX_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! ------------------
        ! The node frequency
        ! ------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    NODE_FREQUENCY_VARNAME, &
                                    NODE_FREQUENCY_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//NODE_FREQUENCY_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 NODE_FREQUENCY_DESC, &
                                                 Variable_Name = NODE_FREQUENCY_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 NODE_FREQUENCY_LONGNAME, &
                                                 Variable_Name = NODE_FREQUENCY_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 NODE_FREQUENCY_UNITS, &
                                                 Variable_Name = NODE_FREQUENCY_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 NODE_FREQUENCY_FILLVALUE, &
                                                 Variable_Name = NODE_FREQUENCY_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//NODE_FREQUENCY_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! -------------------
        ! The node wavenumber
        ! -------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    NODE_WAVENUMBER_VARNAME, &
                                    NODE_WAVENUMBER_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//NODE_WAVENUMBER_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 NODE_WAVENUMBER_DESC, &
                                                 Variable_Name = NODE_WAVENUMBER_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 NODE_WAVENUMBER_LONGNAME, &
                                                 Variable_Name = NODE_WAVENUMBER_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 NODE_WAVENUMBER_UNITS, &
                                                 Variable_Name = NODE_WAVENUMBER_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 NODE_WAVENUMBER_FILLVALUE, &
                                                 Variable_Name = NODE_WAVENUMBER_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//NODE_WAVENUMBER_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! ------------------------------------------
        ! The node first Planck function coefficient
        ! ------------------------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    NODE_PLANCK_C1_VARNAME, &
                                    NODE_PLANCK_C1_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//NODE_PLANCK_C1_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 NODE_PLANCK_C1_DESC, &
                                                 Variable_Name = NODE_PLANCK_C1_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 NODE_PLANCK_C1_LONGNAME, &
                                                 Variable_Name = NODE_PLANCK_C1_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 NODE_PLANCK_C1_UNITS, &
                                                 Variable_Name = NODE_PLANCK_C1_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 NODE_PLANCK_C1_FILLVALUE, &
                                                 Variable_Name = NODE_PLANCK_C1_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//NODE_PLANCK_C1_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF


        ! -------------------------------------------
        ! The node second Planck function coefficient
        ! -------------------------------------------

        ! -- Define the variable
        NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                    NODE_PLANCK_C2_VARNAME, &
                                    NODE_PLANCK_C2_TYPE, &
                                    dimids = Node_DimID, &
                                    varid  = VarID )

        IF ( NF90_Status /= NF90_NOERR ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error defining '//NODE_PLANCK_C2_VARNAME//&
                                ' variable in '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

        ! -- Write some attributes
        Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                                 DESC_ATTNAME, &
                                                 NODE_PLANCK_C2_DESC, &
                                                 Variable_Name = NODE_PLANCK_C2_VARNAME )
        Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                                 LONGNAME_ATTNAME, &
                                                 NODE_PLANCK_C2_LONGNAME, &
                                                 Variable_Name = NODE_PLANCK_C2_VARNAME )
        Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                                 UNITS_ATTNAME, &
                                                 NODE_PLANCK_C2_UNITS, &
                                                 Variable_Name = NODE_PLANCK_C2_VARNAME )
        Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                                 FILLVALUE_ATTNAME, &
                                                 NODE_PLANCK_C2_FILLVALUE, &
                                                 Variable_Name = NODE_PLANCK_C2_VARNAME )

        IF ( Desc_Status      /= SUCCESS .OR. &
             Longname_Status  /= SUCCESS .OR. &
             Units_Status     /= SUCCESS .OR. &
             Fillvalue_Status /= SUCCESS      ) THEN
          Error_Status = FAILURE
          CALL Display_Message( ROUTINE_NAME, &
                                'Error writing '//NODE_PLANCK_C2_VARNAME//&
                                ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF

    END SELECT



    !#--------------------------------------------------------------------------#
    !#    -- DEFINE THE COSMIC BASCKGROUND RADIANCE AND SOLAR IRRADIANCE --     #
    !#    --          WITH THE SPECIFIC FILE TYPE DIMENSION ID           --     #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! The cosmic background radiance
    ! ------------------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                CBR_VARNAME, &
                                CBR_TYPE, &
                                dimids = CBR_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//CBR_VARNAME//&
                            ' variable in '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             CBR_DESC, &
                                             Variable_Name = CBR_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             CBR_LONGNAME, &
                                             Variable_Name = CBR_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             CBR_UNITS, &
                                             Variable_Name = CBR_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             CBR_FILLVALUE, &
                                             Variable_Name = CBR_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CBR_VARNAME//&
                            ' variable attributes to '//TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! TOA Solar irradiance
    ! --------------------

    ! -- Define the variable
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SOLAR_IRRADIANCE_VARNAME, &
                                SOLAR_IRRADIANCE_TYPE, &
                                dimids = SI_DimID, &
                                varid  = VarID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining '//SOLAR_IRRADIANCE_VARNAME//' variable in '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF

    ! -- Write some attributes
    Desc_Status      = Put_netCDF_Attribute( NC_FileID, &
                                             DESC_ATTNAME, &
                                             SOLAR_IRRADIANCE_DESC, &
                                             Variable_Name = SOLAR_IRRADIANCE_VARNAME )
    Longname_Status  = Put_netCDF_Attribute( NC_FileID, &
                                             LONGNAME_ATTNAME, &
                                             SOLAR_IRRADIANCE_LONGNAME, &
                                             Variable_Name = SOLAR_IRRADIANCE_VARNAME )
    Units_Status     = Put_netCDF_Attribute( NC_FileID, &
                                             UNITS_ATTNAME, &
                                             SOLAR_IRRADIANCE_UNITS, &
                                             Variable_Name = SOLAR_IRRADIANCE_VARNAME )
    Fillvalue_Status = Put_netCDF_Attribute( NC_FileID, &
                                             FILLVALUE_ATTNAME, &
                                             SOLAR_IRRADIANCE_FILLVALUE, &
                                             Variable_Name = SOLAR_IRRADIANCE_VARNAME )

    IF ( Desc_Status      /= SUCCESS .OR. &
         Longname_Status  /= SUCCESS .OR. &
         Units_Status     /= SUCCESS .OR. &
         Fillvalue_Status /= SUCCESS      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SOLAR_IRRADIANCE_VARNAME//' variable attributes to '// &
                            TRIM( NC_Filename )//' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF





    !#--------------------------------------------------------------------------#
    !#                 -- WRITE THE RELEASE AND VERSION DATA --                 #
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


    ! ------------------------------
    ! The Release and Version number
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        RELEASE_VARNAME, &
                                        Release )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//RELEASE_VARNAME//' number to '// &
                            TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -- Version
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        VERSION_VARNAME, &
                                        Version )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//VERSION_VARNAME//' number to '// &
                            TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_Filename ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Create_SpcCoeff_netCDF





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
!       Inquire_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF SpcCoeff format file to obtain the
!       dimensions and global attributes.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_SpcCoeff_netCDF( NC_Filename,                                   &  ! Input
!                                               n_Channels            = n_Channels,            &  ! Optional output
!                                               n_Nodes               = n_Nodes,               &  ! Optional output
!                                               Max_Channels_per_Node = Max_Channels_per_Node, &  ! Optional output
!                                               Release               = Release,               &  ! Optional Output
!                                               Version               = Version,               &  ! Optional Output
!                                               Title                 = Title,                 &  ! Optional output
!                                               History               = History,               &  ! Optional output
!                                               Sensor_Name           = Sensor_Name,           &  ! Optional output
!                                               Platform_Name         = Platform_Name,         &  ! Optional output
!                                               Comment               = Comment,               &  ! Optional output
!                                               RCS_Id                = RCS_Id,                &  ! Version control
!                                               Message_Log           = Message_Log            )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:           Character string specifying the name of the netCDF
!                              format SpcCoeff data file to inquire.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
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
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Channels:            The number of channels dimension of the
!                              SpcCoeff data.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       n_Nodes:               The number of nodes dimension of the
!                              SpcCoeff data.
!                              ** NOTE:  This argument is ignored if
!                              the SpcCoeff data file contains only
!                              sensor data. 
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Max_Channels_per_Node: The maximum number of channels per
!                              node dimension of the SpcCoeff data.
!                              ** NOTE:  This argument is ignored if
!                              the SpcCoeff data file contains only
!                              sensor data. 
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Release:               The coefficient file release number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Version:               The coefficient file version number.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Title:                 Character string written into the TITLE global
!                              attribute field of the netCDF SpcCoeff file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       History:               Character string written into the HISTORY global
!                              attribute field of the netCDF SpcCoeff file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Sensor_Name:           Character string written into the SENSOR_NAME global
!                              attribute field of the netCDF SpcCoeff file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Platform_Name:         Character string written into the PLATFORM_NAME global
!                              attribute field of the netCDF SpcCoeff file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Comment:               Character string written into the COMMENT global
!                              attribute field of the netCDF SpcCoeff file.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:                Character string containing the Revision Control
!                              System Id field for the module.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error status.
!                              The error codes are defined in the ERROR_HANDLER module.
!                              If == SUCCESS the netCDF file inquiry was successful
!                                 == FAILURE an error occurred reading any of the requested
!                                            dimension or release/version data.
!                                 == WARNING - an error occurred reading any of the requested
!                                              global file attributes, or
!                                            - an error occurred closing the netCDF file.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
! CALLS:
!       Open_SpcCoeff_netCDF:    Function to open a netCDF format SpcCoeff
!                                data file.
!
!       Read_SpcCoeff_GAtts:     Function to read the global attributes from
!                                a netCDF format SpcCoeff data file.
!
!       Close_SpcCoeff_netCDF:   Function to close a netCDF format SpcCoeff
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
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_SpcCoeff_netCDF( NC_Filename,           &  ! Input
                                    n_Channels,            &  ! Optional output
                                    n_Nodes,               &  ! Optional output
                                    Max_Channels_per_Node, &  ! Optional output
                                    Release,               &  ! Optional output
                                    Version,               &  ! Optional output
                                    Title,                 &  ! Optional output
                                    History,               &  ! Optional output
                                    Sensor_Name,           &  ! Optional output
                                    Platform_Name,         &  ! Optional output
                                    Comment,               &  ! Optional output
                                    RCS_Id,                &  ! Version control
                                    Message_Log )          &  ! Error messaging
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
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Channels
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Nodes
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Max_Channels_per_Node
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Release
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Version
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Title
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: History
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Sensor_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Platform_Name
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: Comment

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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_SpcCoeff_netCDF'


    ! ------------------
    ! Function variables
    ! ------------------

    INTEGER :: NF90_Status
    INTEGER :: File_Type
    INTEGER :: Close_Status
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

    Error_Status = Open_SpcCoeff_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SpcCoeff data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- GET THE SpcCoeff DATA TYPE --                     #
    !#--------------------------------------------------------------------------#

    Error_Status = Read_SpcCoeff_GAtts( NC_Filename, &
                                        NC_FileID, &
                                        File_Type = File_Type, &
                                        Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FILE_TYPE_GATTNAME//' global attribute from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- GET THE DIMENSIONS --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! The number of sensor channels
    ! -----------------------------

    IF ( PRESENT( n_Channels ) ) THEN
      Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                           CHANNEL_DIMNAME, &
                                           n_Channels, &
                                           Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//&
                              TRIM( NC_Filename ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        NF90_Status = NF90_CLOSE( NC_FileID )
        RETURN
      END IF
    END IF


    ! -----------------------------
    ! The other spectral dimensions
    ! -----------------------------

    Spectral_Dimensions: IF ( File_Type == SPECTRAL_FILE_TYPE ) THEN

      ! -- The number of nodes
      IF ( PRESENT( n_Nodes ) ) THEN
        Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                             NODE_DIMNAME, &
                                             n_Nodes, &
                                             Message_Log = Message_Log )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( ROUTINE_NAME, &
                                'Error obtaining '//NODE_DIMNAME//' dimension from '//&
                                TRIM( NC_Filename ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF
      END IF


      ! -- The maximum number of channels per node
      IF ( PRESENT( Max_Channels_per_Node ) ) THEN
        Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                             MAX_CHANNEL_PER_NODE_DIMNAME, &
                                             Max_Channels_per_Node, &
                                             Message_Log = Message_Log )

        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( ROUTINE_NAME, &
                                'Error obtaining '//MAX_CHANNEL_PER_NODE_DIMNAME//&
                                ' dimension from '//TRIM( NC_Filename ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          NF90_Status = NF90_CLOSE( NC_FileID )
          RETURN
        END IF
      END IF

    END IF Spectral_Dimensions



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

    Error_Status = Read_SpcCoeff_GAtts( TRIM( NC_FileNAME ), &
                                        NC_FileID, &
                                        Title         = Title, &
                                        History       = History, &
                                        Sensor_Name   = Sensor_Name, &
                                        Platform_Name = Platform_Name, &
                                        Comment       = Comment, &
                                        Message_Log = Message_Log )

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

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Inquire_SpcCoeff_netCDF





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to write SpcCoeff data to a netCDF format SpcCoeff file.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!         Error_Status = Write_SpcCoeff_netCDF( NC_Filename,                   &  ! Input
!                                               SpcCoeff,                      &  ! Input
!                                               Title         = Title,         &  ! Optional input
!                                               History       = History,       &  ! Optional input
!                                               Sensor_Name   = Sensor_Name,   &  ! Optional input
!                                               Platform_Name = Platform_Name, &  ! Optional input
!                                               Comment       = Comment,       &  ! Optional input
!                                               Quiet         = Quiet,         &  ! Optional input
!                                               RCS_Id        = RCS_Id,        &  ! Version control
!                                               Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the
!                        netCDF format SpcCoeff data file to create.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       SpcCoeff:        Structure containing the spectral coefficient data
!                        to write to file.
!                        UNITS:      N/A
!                        TYPE:       SpcCoeff_Sensor_type
!                                      OR
!                                    SpcCoeff_Spectral_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF SpcCoeff file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Sensor_Name:     Character string written into the SENSOR_NAME
!                        global attribute field of the netCDF SpcCoeff
!                        file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Platform_Name:   Character string written into the PLATFORM_NAME
!                        global attribute field of the netCDF SpcCoeff
!                        file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the Message_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input SpcCoeff structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                           == WARNING an error occurred writing the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Associated_SpcCoeff:        Function to test the association status
!                                   of the pointer members of a SpcCoeff
!                                   structure.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       Check_SpcCoeff_Release:     Function to check the validity of the
!                                   SpcCoeff Release value.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       Create_SpcCoeff_netCDF:     Function to create a netCDF format SpcCoeff
!                                   datafile.
!
!       Open_SpcCoeff_netCDF:       Function to open a netCDF format SpcCoeff
!                                   datafile.
!
!       Put_netCDF_Variable:        Function to write variable data to a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       Close_SpcCoeff_netCDF:      Function to close a netCDF format SpcCoeff
!                                   datafile.
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Version_SpcCoeff:           Subroutine to construct a version info
!                                   message for output.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       Display_Message:            Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Sensor( NC_Filename,   &  ! Input
                         SpcCoeff,      &  ! Input
                         Title,         &  ! Optional input
                         History,       &  ! Optional input
                         Sensor_Name,   &  ! Optional input
                         Platform_Name, &  ! Optional input
                         Comment,       &  ! Optional input
                         Quiet,         &  ! Optional input
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
    CHARACTER( * ),               INTENT( IN )  :: NC_Filename
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional input
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Comment
    INTEGER,            OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Version control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),     OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_netCDF(Sensor)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID



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
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- CHECK THE SpcCoeff STRUCTURE RELEASE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- CREATE THE OUTPUT DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Create_SpcCoeff_netCDF( TRIM( NC_Filename ),           &  ! Input
                                           SpcCoeff%Release,              &  ! Input
                                           SpcCoeff%Version,              &  ! Input
                                           SpcCoeff%n_Channels,           &  ! Input
                                           Title         = Title,         &  ! Optional input
                                           History       = History,       &  ! Optional input
                                           Sensor_Name   = Sensor_Name,   &  ! Optional input
                                           Platform_Name = Platform_Name, &  ! Optional input
                                           Comment       = Comment,       &  ! Optional input
                                           Message_Log   = Message_Log    )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating netCDF sensor SpcCoeff data file '//&
                            TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Mode = 'READWRITE' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF Sensor SpcCoeff data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The sensor descriptor
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        SpcCoeff%Sensor_Descriptor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_DESCRIPTOR_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The sensor type
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        SpcCoeff%Sensor_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_TYPE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_SENSOR_ID
    ! ------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        SpcCoeff%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_SATELLITE_ID
    ! --------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        SpcCoeff%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_SENSOR_ID
    ! -----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        SpcCoeff%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The channel list
    ! ----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        SpcCoeff%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_CHANNEL_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The channel central frequency
    ! -----------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        SpcCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The channel central wavenumber
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WAVENUMBER_VARNAME, &
                                        SpcCoeff%Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WAVENUMBER_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------
    ! The first Planck coefficient
    ! ----------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PLANCK_C1_VARNAME, &
                                        SpcCoeff%Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C1_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The second Planck coefficient
    ! -----------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PLANCK_C2_VARNAME, &
                                        SpcCoeff%Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C2_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! The polychromatic band correction offset
    ! ----------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BAND_C1_VARNAME, &
                                        SpcCoeff%Band_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C1_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The polychromatic band correction slope
    ! ---------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BAND_C2_VARNAME, &
                                        SpcCoeff%Band_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C2_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel polarisation
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        POLARIZATION_VARNAME, &
                                        SpcCoeff%Polarization )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//POLARIZATION_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The cosmic background radiance
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CBR_VARNAME, &
                                        SpcCoeff%Cosmic_Background_Radiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CBR_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The solar channel flag
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        IS_SOLAR_CHANNEL_VARNAME, &
                                        SpcCoeff%Is_Solar_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//IS_SOLAR_CHANNEL_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------------------
    ! The Kurucz TOA solar irradiance source function
    ! -----------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SOLAR_IRRADIANCE_VARNAME, &
                                        SpcCoeff%Solar_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SOLAR_IRRADIANCE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Sensor


  FUNCTION Write_Spectral( NC_Filename,   &  ! Input
                           SpcCoeff,      &  ! Input
                           Title,         &  ! Optional input
                           History,       &  ! Optional input
                           Sensor_Name,   &  ! Optional input
                           Platform_Name, &  ! Optional input
                           Comment,       &  ! Optional input
                           Quiet,         &  ! Optional input
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
    CHARACTER( * ),                 INTENT( IN )  :: NC_Filename
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN )  :: SpcCoeff

    ! -- Optional input
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Title
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: History
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Sensor_Name
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Platform_Name
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Comment
    INTEGER,              OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Version control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),       OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SpcCoeff_netCDF(Spectral)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID



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
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SpcCoeff( SpcCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SpcCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- CHECK THE SpcCoeff STRUCTURE RELEASE --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- CREATE THE OUTPUT DATA FILE --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Create_SpcCoeff_netCDF( &
                     TRIM( NC_Filename ),                                    &  ! Input
                     SpcCoeff%Release,                                       &  ! Input
                     SpcCoeff%Version,                                       &  ! Input
                     SpcCoeff%n_Channels,                                    &  ! Input
                     n_Nodes               = SpcCoeff%n_Nodes,               &  ! Optional input
                     Max_Channels_per_Node = SpcCoeff%Max_Channels_per_Node, &  ! Optional input
                     Title                 = Title,                          &  ! Optional input
                     History               = History,                        &  ! Optional input
                     Sensor_Name           = Sensor_Name,                    &  ! Optional input
                     Platform_Name         = Platform_Name,                  &  ! Optional input
                     Comment               = Comment,                        &  ! Optional input
                     Message_Log           = Message_Log                     )  ! Error messaging

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error creating netCDF spectral SpcCoeff data file '//&
                            TRIM( NC_fileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                         NC_FileID, &
                                         Mode = 'READWRITE' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF Sensor SpcCoeff data file '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The sensor descriptor
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        SpcCoeff%Sensor_Descriptor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_DESCRIPTOR_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The sensor type
    ! ---------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        SpcCoeff%Sensor_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_TYPE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_SENSOR_ID
    ! ------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        SpcCoeff%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NCEP_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_SATELLITE_ID
    ! --------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        SpcCoeff%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SATELLITE_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_SENSOR_ID
    ! -----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        SpcCoeff%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WMO_SENSOR_ID_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The channel list
    ! ----------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        SpcCoeff%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SENSOR_CHANNEL_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------
    ! The channel frequency
    ! ---------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        SpcCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//FREQUENCY_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The channel wavenumber
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        WAVENUMBER_VARNAME, &
                                        SpcCoeff%Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//WAVENUMBER_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------------
    ! The channel first Planck coefficient
    ! ------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PLANCK_C1_VARNAME, &
                                        SpcCoeff%Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C1_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! The channel second Planck coefficient
    ! -------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PLANCK_C2_VARNAME, &
                                        SpcCoeff%Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//PLANCK_C2_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! The polychromatic band correction offset
    ! ----------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BAND_C1_VARNAME, &
                                        SpcCoeff%Band_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C1_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The polychromatic band correction slope
    ! ---------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        BAND_C2_VARNAME, &
                                        SpcCoeff%Band_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//BAND_C2_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel polarisation
    ! ------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        POLARIZATION_VARNAME, &
                                        SpcCoeff%Polarization )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//POLARIZATION_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The solar channel flag
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        IS_SOLAR_CHANNEL_VARNAME, &
                                        SpcCoeff%Is_Solar_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//IS_SOLAR_CHANNEL_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The internal OSS microwave and infrared channel indices
    ! -------------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        MW_IR_CHANNEL_INDEX_VARNAME, &
                                        SpcCoeff%MW_and_IR_Channel_Index )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MW_IR_CHANNEL_INDEX_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------
    ! The number of channels per node
    ! -------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        N_CHANNELS_PER_NODE_VARNAME, &
                                        SpcCoeff%n_Channels_per_Node )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//N_CHANNELS_PER_NODE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The channel<->node map
    ! ----------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CHANNEL_NODE_MAP_VARNAME, &
                                        SpcCoeff%Channel_Node_Map )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CHANNEL_NODE_MAP_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------------------
    ! The internal OSS microwave and infrared node indices
    ! ----------------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        MW_IR_NODE_INDEX_VARNAME, &
                                        SpcCoeff%MW_and_IR_Node_Index )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//MW_IR_NODE_INDEX_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The node frequency
    ! ------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NODE_FREQUENCY_VARNAME, &
                                        SpcCoeff%Node_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NODE_FREQUENCY_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! The node wavenumber
    ! -------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NODE_WAVENUMBER_VARNAME, &
                                        SpcCoeff%Node_Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NODE_WAVENUMBER_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------
    ! The node first Planck coefficient
    ! ---------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NODE_PLANCK_C1_VARNAME, &
                                        SpcCoeff%Node_Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NODE_PLANCK_C1_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------
    ! The node second Planck coefficient
    ! ----------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        NODE_PLANCK_C2_VARNAME, &
                                        SpcCoeff%Node_Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//NODE_PLANCK_C2_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The cosmic background radiance
    ! ------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        CBR_VARNAME, &
                                        SpcCoeff%Cosmic_Background_Radiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//CBR_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------------------
    ! The Kurucz TOA solar irradiance source function
    ! -----------------------------------------------

    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SOLAR_IRRADIANCE_VARNAME, &
                                        SpcCoeff%Solar_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//SOLAR_IRRADIANCE_VARNAME//' to '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Spectral



!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SpcCoeff_netCDF
!
! PURPOSE:
!       Function to read data from a netCDF format SpcCoeff file.
!
! CATEGORY:
!       Instrument Information : SpcCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SpcCoeff_netCDF( NC_Filename,                   &  ! Input
!                                            SpcCoeff,                      &  ! Output
!                                            Quiet         = Quiet,         &  ! Optional input
!                                            Title         = Title,         &  ! Optional output
!                                            History       = History,       &  ! Optional output
!                                            Sensor_Name   = Sensor_Name,   &  ! Optional output
!                                            Platform_Name = Platform_Name, &  ! Optional output
!                                            Comment       = Comment,       &  ! Optional output
!                                            RCS_Id        = RCS_Id,        &  ! Revision control
!                                            Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF
!                        format SpcCoeff data file to read.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the Message log file if
!                        the Message_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
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
!       SpcCoeff:        Structure to contain the spectral coefficient data read
!                        from the file.
!                        UNITS:      N/A
!                        TYPE:       SpcCoeff_Sensor_type
!                                      OR
!                                    SpcCoeff_Spectral_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Sensor_Name:     Character string written into the SENSOR_NAME global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Platform_Name:   Character string written into the PLATFORM_NAME global
!                        attribute field of the netCDF SpcCoeff file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF SpcCoeff file.
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
!                           == FAILURE an unrecoverable read error occurred.
!                           == WARNING an error occurred reading the global
!                                      attributes.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Open_SpcCoeff_netCDF:       Function to open a netCDF format SpcCoeff
!                                   data file.
!
!       Inquire_SpcCoeff_netCDF:    Function to inquire a netCDF format 
!                                   SpcCoeff file to obtain information
!                                   about the data dimensions and attributes.
!
!       Close_SpcCoeff_netCDF:      Function to close a netCDF format SpcCoeff
!                                   data file with error checking.
!
!       Allocate_SpcCoeff:          Function to allocate the pointer members
!                                   of an SpcCoeff structure.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       Get_netCDF_Variable:        Function to read variable data from a
!                                   netCDF data file.
!                                   SOURCE: NETCDF_VARIABLE_UTILITY module
!
!       NF90_CLOSE:                 Function to close a netCDF file.
!                                   SOURCE: netCDF library
!
!       Count_SpcCoeff_Sensors:     Subroutine to count the number of different
!                                   sensors represented in an SpcCoeff data
!                                   structure.
!                                   SOURCE: SPCCOEFF_DEFINE module
!
!       Version_SpcCoeff:           Subroutine to construct a version info
!                                   message for output.
!                                   SOURCE: SPCCOEFF_DEFINE module
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
!       Note the INTENT on the output SpcCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Dec-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Sensor( NC_Filename,   &   ! Input
                        SpcCoeff,      &   ! Output
                        Quiet,         &   ! Optional input
                        Title,         &   ! Optional output
                        History,       &   ! Optional output
                        Sensor_Name,   &   ! Optional output
                        Platform_Name, &   ! Optional output
                        Comment,       &   ! Optional output
                        RCS_Id,        &   ! Revision control
                        Message_Log )  &   ! Error messaging
                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN )     :: NC_Filename

    ! -- Output
    TYPE( SpcCoeff_Sensor_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Optional output
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: Sensor_Name
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: Platform_Name
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: Comment

    ! -- Revision control
    CHARACTER( * ),     OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_netCDF(Sensor)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Channels



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
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#     -- GET THE DIMENSION VALUE AND ALLOCATE THE SpcCoeff STRUCTURE --    #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Read the dimension value
    ! ------------------------

    Error_Status = Inquire_SpcCoeff_netCDF( TRIM( NC_FileNAME ), &
                                            n_Channels    = n_Channels, &
                                            Release       = SpcCoeff%Release, &
                                            Version       = SpcCoeff%Version, &
                                            Title         = Title, &
                                            History       = History, &
                                            Sensor_Name   = Sensor_Name, &
                                            Platform_Name = Platform_Name, &
                                            Comment       = Comment, &
                                            Message_Log   = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining SpcCoeff dimension/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed for '//TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      SpcCoeff, &
                                      Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating SpcCoeff structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SpcCoeff_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SpcCoeff data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SpcCoeff DATA --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The sensor descriptor
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        SpcCoeff%Sensor_Descriptor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_DESCRIPTOR_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The sensor type
    ! ---------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        SpcCoeff%Sensor_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_TYPE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_SENSOR_ID
    ! ------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        SpcCoeff%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NCEP_SENSOR_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_SATELLITE_ID
    ! --------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        SpcCoeff%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SATELLITE_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_SENSOR_ID
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        SpcCoeff%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SENSOR_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The channel list
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        SpcCoeff%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_CHANNEL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The channel central frequency
    ! -----------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        SpcCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FREQUENCY_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The channel central wavenumber
    ! ------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WAVENUMBER_VARNAME, &
                                        SpcCoeff%Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WAVENUMBER_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------
    ! The first Planck coefficient
    ! ----------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PLANCK_C1_VARNAME, &
                                        SpcCoeff%Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PLANCK_C1_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------
    ! The second Planck coefficient
    ! -----------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PLANCK_C2_VARNAME, &
                                        SpcCoeff%Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PLANCK_C2_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! The polychromatic band correction offset
    ! ----------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BAND_C1_VARNAME, &
                                        SpcCoeff%Band_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BAND_C1_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The polychromatic band correction slope
    ! ---------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BAND_C2_VARNAME, &
                                        SpcCoeff%Band_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BAND_C2_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel polarisation
    ! ------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        POLARIZATION_VARNAME, &
                                        SpcCoeff%Polarization )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//POLARIZATION_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The cosmic background radiance
    ! ------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CBR_VARNAME, &
                                        SpcCoeff%Cosmic_Background_Radiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CBR_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The solar channel flag
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        IS_SOLAR_CHANNEL_VARNAME, &
                                        SpcCoeff%Is_Solar_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//IS_SOLAR_CHANNEL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------------------
    ! The Kurucz TOA solar irradiance source function
    ! -----------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SOLAR_IRRADIANCE_VARNAME, &
                                        SpcCoeff%Solar_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SOLAR_IRRADIANCE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_Sensor

  FUNCTION Read_Spectral( NC_Filename,   &   ! Input
                          SpcCoeff,      &   ! Output
                          Quiet,         &   ! Optional input
                          Title,         &   ! Optional output
                          History,       &   ! Optional output
                          Sensor_Name,   &   ! Optional output
                          Platform_Name, &   ! Optional output
                          Comment,       &   ! Optional output
                          RCS_Id,        &   ! Revision control
                          Message_Log )  &   ! Error messaging
                        RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                 INTENT( IN )     :: NC_Filename

    ! -- Output
    TYPE( SpcCoeff_Spectral_type ), INTENT( IN OUT ) :: SpcCoeff

    ! -- Optional input
    INTEGER,              OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Optional output
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: Title
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: History
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: Sensor_Name
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: Platform_Name
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: Comment

    ! -- Revision control
    CHARACTER( * ),       OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),       OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SpcCoeff_netCDF(Spectral)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: NF90_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Channels
    INTEGER :: n_Nodes
    INTEGER :: Max_Channels_per_Node



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
    ! Info Message output
    ! -------------------

    ! -- Output informational messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#     -- GET THE DIMENSION VALUE AND ALLOCATE THE SpcCoeff STRUCTURE --    #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Read the dimension value
    ! ------------------------

    Error_Status = Inquire_SpcCoeff_netCDF( TRIM( NC_FileNAME ), &
                                            n_Channels            = n_Channels, &
                                            n_Nodes               = n_Nodes,               &
                                            Max_Channels_per_Node = Max_Channels_per_Node, &
                                            Release               = SpcCoeff%Release, &
                                            Version               = SpcCoeff%Version, &
                                            Title                 = Title, &           
                                            History               = History, &         
                                            Sensor_Name           = Sensor_Name, &     
                                            Platform_Name         = Platform_Name, &   
                                            Comment               = Comment, &         
                                            Message_Log           = Message_Log )      

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining SpcCoeff dimension/attributes from '//&
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_SpcCoeff_Release( SpcCoeff, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'SpcCoeff Release check failed for '//TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_fileID )
      RETURN
    END IF


    ! ----------------------
    ! Allocate the structure
    ! ----------------------

    Error_Status = Allocate_SpcCoeff( n_Channels, &
                                      n_Nodes, &
                                      Max_Channels_per_Node, &
                                      SpcCoeff, &
                                      Message_Log = Message_Log )
                                        
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred allocating SpcCoeff structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- OPEN THE netCDF FILE --                       #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_SpcCoeff_netCDF( TRIM( NC_FileNAME ), &
                                         NC_FileID, &
                                         Mode = 'READ' )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening netCDF SpcCoeff data file '//&
                            TRIM( NC_FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE SpcCoeff DATA --                       #
    !#--------------------------------------------------------------------------#

    ! ---------------------
    ! The sensor descriptor
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_DESCRIPTOR_VARNAME, &
                                        SpcCoeff%Sensor_Descriptor )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_DESCRIPTOR_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------
    ! The sensor type
    ! ---------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        SpcCoeff%Sensor_Type )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_TYPE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The NCEP_SENSOR_ID
    ! ------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NCEP_SENSOR_ID_VARNAME, &
                                        SpcCoeff%NCEP_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NCEP_SENSOR_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! --------------------
    ! The WMO_SATELLITE_ID
    ! --------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SATELLITE_ID_VARNAME, &
                                        SpcCoeff%WMO_Satellite_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SATELLITE_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------
    ! The WMO_SENSOR_ID
    ! -----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WMO_SENSOR_ID_VARNAME, &
                                        SpcCoeff%WMO_Sensor_ID )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WMO_SENSOR_ID_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------
    ! The channel list
    ! ----------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        SpcCoeff%Sensor_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SENSOR_CHANNEL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------
    ! The channel frequency
    ! ---------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        FREQUENCY_VARNAME, &
                                        SpcCoeff%Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//FREQUENCY_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The channel wavenumber
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        WAVENUMBER_VARNAME, &
                                        SpcCoeff%Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//WAVENUMBER_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------------
    ! The channel first Planck coefficient
    ! ------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PLANCK_C1_VARNAME, &
                                        SpcCoeff%Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PLANCK_C1_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! The channel second Planck coefficient
    ! -------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PLANCK_C2_VARNAME, &
                                        SpcCoeff%Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//PLANCK_C2_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------
    ! The polychromatic band correction offset
    ! ----------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BAND_C1_VARNAME, &
                                        SpcCoeff%Band_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BAND_C1_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! The polychromatic band correction slope
    ! ---------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        BAND_C2_VARNAME, &
                                        SpcCoeff%Band_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//BAND_C2_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------
    ! The channel polarisation
    ! ------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        POLARIZATION_VARNAME, &
                                        SpcCoeff%Polarization )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//POLARIZATION_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The solar channel flag
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        IS_SOLAR_CHANNEL_VARNAME, &
                                        SpcCoeff%Is_Solar_Channel )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//IS_SOLAR_CHANNEL_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------------------------------
    ! The internal OSS microwave and infrared channel indices
    ! -------------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        MW_IR_CHANNEL_INDEX_VARNAME, &
                                        SpcCoeff%MW_and_IR_Channel_Index )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MW_IR_CHANNEL_INDEX_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------------------
    ! The number of channels per node
    ! -------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        N_CHANNELS_PER_NODE_VARNAME, &
                                        SpcCoeff%n_Channels_per_Node )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//N_CHANNELS_PER_NODE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------
    ! The channel<->node map
    ! ----------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CHANNEL_NODE_MAP_VARNAME, &
                                        SpcCoeff%Channel_Node_Map )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CHANNEL_NODE_MAP_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------------------------
    ! The internal OSS microwave and infrared node indices
    ! ----------------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        MW_IR_NODE_INDEX_VARNAME, &
                                        SpcCoeff%MW_and_IR_Node_Index )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//MW_IR_NODE_INDEX_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------
    ! The node frequency
    ! ------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NODE_FREQUENCY_VARNAME, &
                                        SpcCoeff%Node_Frequency )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NODE_FREQUENCY_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -------------------
    ! The node wavenumber
    ! -------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NODE_WAVENUMBER_VARNAME, &
                                        SpcCoeff%Node_Wavenumber )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NODE_WAVENUMBER_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ---------------------------------
    ! The node first Planck coefficient
    ! ---------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NODE_PLANCK_C1_VARNAME, &
                                        SpcCoeff%Node_Planck_C1 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NODE_PLANCK_C1_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ----------------------------------
    ! The node second Planck coefficient
    ! ----------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        NODE_PLANCK_C2_VARNAME, &
                                        SpcCoeff%Node_Planck_C2 )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//NODE_PLANCK_C2_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! ------------------------------
    ! The cosmic background radiance
    ! ------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        CBR_VARNAME, &
                                        SpcCoeff%Cosmic_Background_Radiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//CBR_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF


    ! -----------------------------------------------
    ! The Kurucz TOA solar irradiance source function
    ! -----------------------------------------------

    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SOLAR_IRRADIANCE_VARNAME, &
                                        SpcCoeff%Solar_Irradiance )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//SOLAR_IRRADIANCE_VARNAME//' from '// &
                            TRIM( NC_Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CLOSE THE netCDF FILE --                         #
    !#--------------------------------------------------------------------------#

    Close_Status = Close_SpcCoeff_netCDF( NC_FileID )

    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF SpcCoeff data file '// &
                            TRIM( NC_FileNAME ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COUNT THE NUMBER OF SENSORS --                      #
    !#--------------------------------------------------------------------------#

    CALL Count_SpcCoeff_Sensors( SpcCoeff, Use_WMO_ID = SET )



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_SpcCoeff( SpcCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( NC_Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_Spectral

END MODULE SpcCoeff_netCDF_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SpcCoeff_netCDF_IO.f90,v 6.6 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 6.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SpcCoeff_netCDF_IO.f90,v $
! Revision 6.6  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 6.5  2005/08/18 16:02:23  paulv
! - Updated to reflect changes in SpcCoeff Spectral structure definition.
!   TOA solar irradiance is now a node-based array in the Spectral structure.
!
! Revision 6.4  2005/07/12 14:50:40  paulv
! - Corrected bug in the Create() function. The local variables Frequency_Desc
!   and Wavenumber_Desc were still declared and overrode the module parameters
!   of the same name. The local variables were deleted.
!
! Revision 6.3  2005/07/05 23:55:49  paulv
! - Corrected bug in Create_SpcCoeff_netCDF() function where the node
!   Planck C2 variable dimension was incorrect.
!
! Revision 6.2  2005/07/05 22:30:54  paulv
! - Fixed some minor variable and argument bugs.
!
! Revision 6.1  2005/07/05 21:38:04  paulv
! - Added some additional components to the Spectral SpcCoeff structure
!   definition.
! - Moved some node-based components back to being channel-based in the
!   Spectral SpcCoeff structure definition.
! - Still untested.
!
! Revision 6.0  2005/07/05 11:39:34  paulv
! - Major update. There are now two SpcCoeff data structures; a Sensor based
!   one (the old, regular SpcCoeff) and a Spectral based one (for use with the
!   monomchromatic OSS code). Definition and I/O modules have been modified
!   accordingly. Untested.
!
! Revision 5.2  2005/04/01 18:01:51  paulv
! - Renamed all IS_MICROWAVE_CHANNEL_xxx parameter names to IS_MW_CHANNEL_xxx
!   to avoid going beyond the 31 character limit.
!
! Revision 5.1  2005/03/31 21:25:40  paulv
! - Completed update for new SpcCoeff_Define module. Untested.
!
! Revision 5.0  2005/03/30 22:24:25  paulv
! - New Version. Incomplete.
! - Partial update for changes made to SpcCoeff_Define module.
!
! Revision 4.3  2004/09/17 17:04:36  paulv
! - Now using Get_netCDF_Attribute in the global attribute read function.
! - Global attributes read using a very long string and then truncated accordingly
!   to prevent problems when the supplied dummy argument is not long enough to hold
!   the attribute string.
!
! Revision 4.2  2004/08/31 21:14:02  paulv
! - Minor documentation updates.
! - Replaced Error_Status result on Close_SpcCoeff_netCDF() function to
!   Close_Status to prevent propagaton of a failure on close.
!
! Revision 4.1  2004/08/23 14:43:46  paulv
! - Upgraded to Fortran95.
! - Added structure association test to the Write() function.
! - Changed INTENT of SpcCoeff structure in Read() function from OUT to
!   IN OUT. Necessary to prevent memory leaks.
! - If an error occurs closing a netCDF file at the end of the Read() and
!   Write() functions, a warning *message* is issued, but the error status
!   is not set to WARNING.
! - Updated header documentation.
!
! Revision 4.0  2004/06/25 19:46:05  paulv
! - Dummy checkin to update version number branch to 4.X.
!
! Revision 3.2  2004/06/25 19:41:13  paulv
! - Upgraded definition and I/O of polarization component from a scalar
!   value to a full Stokes vector.
!
! Revision 3.1  2004/06/25 17:12:07  paulv
! - Removed unused variables from type declarations.
! - Cosmetic changes.
!
! Revision 3.0  2004/05/17 17:40:48  paulv
! - Added Sensor_Descriptor component to SpcCoeff structure. Modified the
!   netCDF and Binary I/O modules to handle the new component. New SpcCoeff
!   release number is 3.
!
! Revision 2.9  2004/03/09 17:24:37  paulv
! - Mostly cosmetic changes. Some fixes to eliminate possibilities of
!   exceeding string lengths.
!
! Revision 2.8  2003/11/13 19:35:47  paulv
! - Updated header documentation.
!
! Revision 2.7  2003/10/24 18:18:14  paulv
! - Code category changed from
!     NCEP RTM : Coefficients : SpcCoeff
!   to
!     Instrument Information : SpcCoeff
!
! Revision 2.6  2003/08/18 13:35:27  paulv
! - Corrected bug in reading the band_c1 coefficient value. The band_c2 value
!   was always being read in.
!
! Revision 2.5  2003/06/19 21:37:09  paulv
! - Now using the Check_SpcCoeff_Release() function to determine if a passed
!   SpcCoeff structure can be read/written.
!
! Revision 2.4  2003/04/14 19:56:15  paulv
! - Added global attributes as optional arguments to READ() function.
!
! Revision 2.3  2003/02/13 20:00:45  paulv
! - Made open mode in Open_netCDF() function all uppercase as required.
!
! Revision 2.2  2003/02/13 17:36:49  paulv
! - Modified all calls that pass filename character strings to pass the TRIM()
!   version of the argument.
!
! Revision 2.1  2003/02/12 15:22:46  paulv
! - Corrected bug in writing the variable attributes.
! - Moved the "internal" attribute write to the Write_SpcCoeff_GAtts()
!   function.
!
! Revision 2.0  2003/02/10 22:56:07  paulv
! - New version. Create() funciton removed. Only public accessible functions
!   are the Inquire(), Write() and Read() functions.
!
! Revision 1.5  2002/12/27 19:35:38  paulv
! - Corrected bug with Release and Version attribute error checking.
! - Ensured that all strings were trimmed prior to use.
!
! Revision 1.4  2002/12/26 17:35:56  paulv
! - Added call to Count_SpcCoeff_Sensors() in the Read_SpcCoeff_netCDF()
!   function.
! - Now using call to Version_SpcCoeff() to construct the INFORMATION
!   message output.
!
! Revision 1.3  2002/12/24 00:14:57  paulv
! - Using Put_netCDF_Variable() function in Write() function.
! - Updated documentation.
!
! Revision 1.2  2002/12/23 12:51:26  paulv
! - Completed new versions. Untested.
!
! Revision 1.1  2002/12/20 22:23:03  paulv
! Initial checkin. Incomplete.
!
!
