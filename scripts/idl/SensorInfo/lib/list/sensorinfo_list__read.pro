;+
; NAME:
;       SensorInfo_List::Read
;
; PURPOSE:
;       The SensorInfo_List::Read procedure method reads a SensorInfo_List
;       file and fills the container with all the SensorInfo objects in
;       the file.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo_List::]Read, $
;         [Filename, $]  ; Optional input
;         Debug = Debug  ;  Input keyword
;
; ARGUMENTS:
;       Filename:    The name of the SensorInfo file to read. If not specified,
;                    the file read is defined by self.Filename.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; EXAMPLE:
;       After creating a SensorInfo_List object,
;
;         IDL> list = OBJ_NEW('SensorInfo_List')
;
;       it can be populated with data from a SensorInfo file,
;       named "si.dat", like so,
;
;         IDL> list->Read, 'si.dat'
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION Is_A_CommentLine, Line
  RETURN, STRMID(STRTRIM(Line,1),0,1) EQ '!'
END ; FUNCTION Is_A_CommentLine


FUNCTION Is_A_BlankLine, Line
  RETURN, STRLEN(STRTRIM(Line,2)) EQ 0
END ; FUNCTION Is_A_BlankLine


PRO SensorInfo_List::Read, $
  Filename, $    ; Optional input
  Debug = Debug  ; Input keyword

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler
  ; ...Check filename argument
  IF ( Valid_String(Filename) ) THEN self.Filename = Filename


  ; Check the file exists
  fInfo = FILE_INFO(self.Filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, self.Filename+' not found', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file
  GET_LUN, fid
  OPENR, fid, self.Filename


  ; Initialise variables for reading
  n_Channels       = 0L
  Sensor_Name      = ''
  Satellite_Name   = ''
  Sensor_Id        = ''
  WMO_Satellite_ID = 0L
  WMO_Sensor_ID    = 0L
  Sensor_Type      = 0L


  ; Loop over comment lines
  Line = ''
  REPEAT BEGIN
    ; ...Save the current file position
    POINT_LUN, -fid, fptr
    ; ...Read a line from the file
    READF, fid, Line
    ; ...Exit loop if this is NOT a comment or blank line
  ENDREP UNTIL ( (NOT Is_A_CommentLine(Line)) AND (NOT Is_A_BlankLine(Line)))


  ; Restore file position to before the previous read
  POINT_LUN, fid, fptr
  

  ; Begin loop over sensors
  WHILE ( NOT EOF(fid) ) DO BEGIN
    ; ...Read a line from the file
    READF, fid, Line
    ; ...Cycle loop if this is a comment or blank line
    IF ( Is_A_CommentLine(Line) OR Is_A_BlankLine(Line) ) THEN CONTINUE
    ; Read the SensorInfo line into variables
    READS, FORMAT=SENSORINFO_FORMAT, Line, Sensor_Name     , $
                                           Satellite_Name  , $
                                           Sensor_Id       , $
                                           Sensor_Type     , $
                                           WMO_Sensor_ID   , $
                                           WMO_Satellite_ID, $
                                           n_Channels      
    ; ...Create and allocate a SensorInfo object
    si = OBJ_NEW('SensorInfo', Debug=Debug)
    si->Allocate, n_Channels, Debug=Debug
    ; ...Read the channel information
    ChannelInfo = DBLARR(3,n_Channels)
    READF, fid, ChannelInfo
    Sensor_Channel = LONG(REFORM(ChannelInfo[0,*]))
    Use_Flag       = LONG(REFORM(ChannelInfo[1,*]))
    Noise          = REFORM(ChannelInfo[2,*])
    ; ...Set the SensorInfo properties
    si->SensorInfo::Set_Property, $
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
    ; ...Add the SensorInfo object to the list
    self->Add, si, Debug=Debug

  ENDWHILE
  FREE_LUN, fid

  
  ; Output an info message
  MESSAGE, 'FILE: ' + STRTRIM(self.Filename,2) + ', N_SENSORS=' + STRTRIM(self.n_Sensors,2), $
    /INFORMATIONAL

     
  ; Done
  CATCH, /CANCEL

END ; PRO SensorInfo_List::Read
