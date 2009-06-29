;+
; NAME:
;       SensorInfo_List::Write
;
; PURPOSE:
;       The SensorInfo_List::Write procedure method writes an
;       ASCII SensorInfo file.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Write, $
;         [Filename, $]     ; Optional input
;         Force = Force, $  ; Input keyword
;         Debug = Debug     ; Input keyword
;
; ARGUMENTS:
;       Filename:    The name of the SensorInfo file to read. If not specified,
;                    the file read is defined by self.Filename.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT)
;
; KEYWORDS:
;       Force:       If the output file already exists, set this keyword to
;                    force the file to be overwritten. If the output file
;                    already exists then,
;                    If NOT SET => No data is written to file. (DEFAULT)
;                       SET     => Existing file is overwritten.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; EXAMPLE:
;       Given a SensorInfo_List object,
;
;         IDL> help, list
;         LIST            OBJREF    = <ObjHeapVar8(SENSORINFO_LIST)>
;
;       it can be written to a SensorInfo file, named "si.dat", like so,
;
;         IDL> list->Write('si.dat')
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 29-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo_List::Write, $
  Filename     , $  ; Optional input
  Force = Force, $  ; Input keyword
  Debug = Debug     ; Input keyword

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler
  ; ...Check filename argument
  IF ( Valid_String(Filename) ) THEN $
    _Filename = Filename $
  ELSE $
    _Filename = self.Filename
  ; ...Check if file exists
  fInfo = FILE_INFO(_Filename)
  IF ( fInfo.EXISTS ) THEN BEGIN
    IF ( KEYWORD_SET(Force) ) THEN BEGIN
      MESSAGE, STRTRIM(_Filename,2) + ' will be overwritten', $
               /INFORMATIONAL
    ENDIF ELSE BEGIN
      MESSAGE, STRTRIM(_Filename,2) + ' already exists and FORCE keyword not set. Nothing done.', $
               /INFORMATIONAL
      CATCH, /CANCEL
      RETURN
    ENDELSE
  ENDIF
  

  ; Open the file for writing
  GET_LUN, fid
  OPENW, fid, _Filename

  
  ; Get all the SensorInfo object references
  si = self->Get(/ALL, ISA='SensorInfo', COUNT = n_Sensors )
  
  
  ; Begin loop over sensors
  FOR n = 0L, n_Sensors-1L DO BEGIN
    ; ...Get the SensorInfo properties
    si[n]->SensorInfo::Get_Property, $
      Debug            = Debug           , $
      Sensor_Name      = Sensor_Name     , $
      Satellite_Name   = Satellite_Name  , $
      Sensor_Id        = Sensor_Id       , $
      WMO_Satellite_ID = WMO_Satellite_ID, $
      WMO_Sensor_ID    = WMO_Sensor_ID   , $
      Sensor_Type      = Sensor_Type     , $
      Sensor_Channel   = Sensor_Channel  , $
      Use_Flag         = Use_Flag        , $
      Noise            = Noise           
    n_Channels = N_ELEMENTS(Sensor_Channel)
    ; ...Write the SensorInfo header line
    PRINTF, fid, FORMAT=SENSORINFO_FORMAT, Sensor_Name     , $
                                           Satellite_Name  , $
                                           Sensor_Id       , $
                                           Sensor_Type     , $
                                           WMO_Sensor_ID   , $
                                           WMO_Satellite_ID, $
                                           n_Channels      
    ; ...Write the channel information
    FOR i = 0L, n_Channels-1L DO BEGIN
      PRINTF, fid, FORMAT=CHANNELINFO_FORMAT, Sensor_Channel[i], $
                                              Use_Flag[i], $
                                              Noise[i]
    ENDFOR
    
  ENDFOR
  FREE_LUN, fid

  
  ; Output an info message
  MESSAGE, 'FILE: ' + STRTRIM(_Filename,2) + ', N_SENSORS=' + STRTRIM(n_Sensors,2), $
    /INFORMATIONAL

     
  ; Done
  CATCH, /CANCEL

END ; PRO SensorInfo_List::Write
