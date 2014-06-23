;+
; NAME:
;       OSRF_File::Get_Property
;
; PURPOSE:
;       The OSRF_File::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Get_Property, $
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
;       Sensor_ID:          Character string sensor/platform identifier.
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

PRO OSRF_File::Get_Property, $
  Debug            = debug           , $  ; Input keyword
  n_Channels       = n_channels      , $  ; Output keyword
  Version          = version         , $  ; Output keyword
  Sensor_ID        = sensor_id       , $  ; Output keyword
  WMO_Satellite_Id = wmo_satellite_id, $  ; Output keyword
  WMO_Sensor_Id    = wmo_sensor_id   , $  ; Output keyword
  Sensor_Type      = sensor_type     , $  ; Output keyword
  Sensor_Channel   = sensor_channel  , $  ; Output leyword
  Title            = title           , $  ; Output keyword
  History          = history         , $  ; Output keyword
  Comment          = comment              ; Output keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Get data
  IF ( ARG_PRESENT(n_channels      ) ) THEN n_channels       = self.n_Channels
  IF ( ARG_PRESENT(version         ) ) THEN version          = self.Version         
  IF ( ARG_PRESENT(wmo_satellite_id) ) THEN wmo_satellite_id = self.WMO_Satellite_ID
  IF ( ARG_PRESENT(wmo_sensor_id   ) ) THEN wmo_sensor_id    = self.WMO_Sensor_ID   
  IF ( ARG_PRESENT(sensor_type     ) ) THEN sensor_type      = self.Sensor_Type     
  IF ( ARG_PRESENT(sensor_id       ) ) THEN sensor_id        = self.Sensor_Id
  IF ( ARG_PRESENT(title           ) ) THEN title            = self.Title    
  IF ( ARG_PRESENT(history         ) ) THEN history          = self.History  
  IF ( ARG_PRESENT(comment         ) ) THEN comment          = self.Comment

  IF ( ARG_PRESENT(sensor_channel) ) THEN BEGIN
    ; Get references for all OSRF objects in list
    osrf = self->IDL_Container::Get(/ALL, ISA='OSRF', COUNT=n)
    ; Loop over all OSRF objects to build the sensor channel list
    Sensor_Channel = LONARR(n)
    FOR i = 0L, n-1L DO BEGIN
      osrf[i]->Get_Property, Channel=ch
      sensor_channel[i] = ch
    ENDFOR
  ENDIF

  ; Done
  CATCH, /CANCEL

END ; PRO OSRF_File::Get_Property
