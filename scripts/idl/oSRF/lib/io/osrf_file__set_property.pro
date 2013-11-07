;
; NAME:
;       OSRF_File::Set_Property
;
; PURPOSE:
;       The OSRF_File::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Set_Property, $
;         Debug            = Debug           , $  ; Input keyword
;         Version          = Version         , $  ; Input keyword
;         Sensor_Id        = Sensor_Id       , $  ; Input keyword
;         WMO_Satellite_Id = WMO_Satellite_Id, $  ; Input keyword
;         WMO_Sensor_Id    = WMO_Sensor_Id   , $  ; Input keyword
;         Sensor_Type      = Sensor_Type     , $  ; Input keyword
;         Title            = Title           , $  ; Input keyword
;         History          = History         , $  ; Input keyword
;         Comment          = Comment           )  ; Input keyword
;
; INPUT KEYWORDS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Error handler is enabled. (DEFAULT)
;                              SET     => Error handler is disabled; Routine
;                                         traceback output is enabled.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Version:            The version number of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Id:          Character string sensor/platform identifier.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       WMO_Sensor_Id:      The WMO code used to identify sensors.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Title:              Character string written into the TITLE global
;                           attribute field of the netCDF SRF file.
;                           Should contain a succinct description of what
;                           is in the netCDF datafile.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       History:            Character string written into the HISTORY global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Comment:            Character string written into the COMMENT global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Set_Property, $
  Debug = Debug, $
  Version          = Version         , $
  Sensor_Id        = Sensor_Id       , $
  WMO_Satellite_Id = WMO_Satellite_Id, $
  WMO_Sensor_Id    = WMO_Sensor_Id   , $
  Sensor_Type      = Sensor_Type     , $
  Title            = Title           , $
  History          = History         , $
  Comment          = Comment         


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF_File parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Set data
  IF ( N_ELEMENTS(Version         ) GT 0 ) THEN self.Version          = Version         
  IF ( N_ELEMENTS(WMO_Satellite_ID) GT 0 ) THEN self.WMO_Satellite_ID = WMO_Satellite_ID
  IF ( N_ELEMENTS(WMO_Sensor_ID   ) GT 0 ) THEN self.WMO_Sensor_ID    = WMO_Sensor_ID   
  IF ( N_ELEMENTS(Sensor_Type     ) GT 0 ) THEN self.Sensor_Type      = Sensor_Type     

  IF ( Valid_String(Sensor_Id) ) THEN self.Sensor_Id = Sensor_Id       
  IF ( Valid_String(Title    ) ) THEN self.Title     = Title         
  IF ( Valid_String(History  ) ) THEN self.History   = History       
  IF ( Valid_String(Comment  ) ) THEN self.Comment   = Comment       
 
 
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF_File::Set_Property
