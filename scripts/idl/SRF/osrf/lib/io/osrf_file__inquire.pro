;+
; NAME:
;       OSRF_File::Inquire
;
; PURPOSE:
;       The OSRF_File::Inquire procedure method inquires an OSRF_File
;       file for information.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Inquire, $
;         Debug            = Debug           , $  ; Input keyword
;         n_Channels       = n_Channels      , $  ; Output keyword
;         Version          = Version         , $  ; Output keyword
;         Sensor_ID        = Sensor_ID       , $  ; Output keyword
;         WMO_Satellite_Id = WMO_Satellite_Id, $  ; Output keyword
;         WMO_Sensor_Id    = WMO_Sensor_Id   , $  ; Output keyword
;         Sensor_Type      = Sensor_Type     , $  ; Output keyword
;         Title            = Title           , $  ; Output keyword
;         History          = History         , $  ; Output keyword
;         Comment          = Comment              ; Output keyword
;
; INPUT KEYWORDS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Regular output. (DEFAULT)
;                              SET     => Information about all currently compiled
;                                         routines and their arguments are output.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       n_Channels:         The number of channels dimension of the
;                           SRF data data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Version:            The version number of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_ID:          A character string identifying the sensor and
;                           satellite platform used to contruct filenames.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       WMO_Sensor_ID:      The WMO code used to identify sensors.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Title:              Character string written into the TITLE global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       History:            Character string written into the HISTORY global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Comment:            Character string written into the COMMENT global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;-

PRO OSRF_File::Inquire, $
  Debug            = Debug           , $  ; Input keyword
  n_Channels       = n_Channels      , $  ; Output keyword
  Version          = Version         , $  ; Output keyword
  Sensor_ID        = Sensor_ID       , $  ; Output keyword
  WMO_Satellite_Id = WMO_Satellite_Id, $  ; Output keyword
  WMO_Sensor_Id    = WMO_Sensor_Id   , $  ; Output keyword
  Sensor_Type      = Sensor_Type     , $  ; Output keyword
  Title            = Title           , $  ; Output keyword
  History          = History         , $  ; Output keyword
  Comment          = Comment              ; Output keyword

  ; Set up
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Get the dimensions and data
  ; ...Open the netCDF SRF file
  fid = NCDF_OPEN( self.filename, /NOWRITE )
  NCDF_CONTROL, fid, /VERBOSE
  ; ...Get the number of channels dimension
  DimID = NCDF_DIMID( fid, CHANNEL_DIMNAME )
  NCDF_DIMINQ, fid, DimID, DimName, n_Channels
  self.n_Channels = n_Channels
  ; ...Done
  NCDF_CLOSE, fid

  
  ; Get the global attributes
  self->Read_GAtts, Debug = Debug
  Version          = self.Version
  Sensor_Id        = self.Sensor_Id
  WMO_Satellite_Id = self.WMO_Satellite_Id
  WMO_Sensor_Id    = self.WMO_Sensor_Id
  Sensor_Type      = self.Sensor_Type
  Title            = self.Title
  History          = self.History
  Comment          = self.Comment


  ; Done
  CATCH, /CANCEL

END ; PRO OSRF_File::Inquire
