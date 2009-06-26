;+
; NAME:
;       SensorInfo_List::Write
;
; PURPOSE:
;       The SensorInfo_List::Write function method write ASCII format
;       SensorInfo file data from a SensorInfo linked list
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo_List::]Write( Filename   , $  ; Input
;                                              Debug=Debug  )  ; Input keyword
;
; INPUT ARGUMENTS:
;       Filename:    The name of the SensorInfo file to write.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORD PARAMETERS:
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
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the file write was successful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       error_codes:           Include file containing error code definitions.
;
; EXAMPLE:
;       Given a SensorInfo_List object,
;
;         IDL> help, list
;         LIST            OBJREF    = <ObjHeapVar8(SENSORINFO_LIST)>
;
;       it can be written to a SensorInfo file, named "si.dat", like so,
;
;         IDL> Result = list->Write('si.dat')
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 29-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Write, Filename, $  ; Input
                                 Debug=Debug  ; Input keyword

  ; Set up
  ; ------
  ; Include the SensorInfo parameters
  @sensorinfo_parameters
  
  ; error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      IF ( N_ELEMENTS(File_Id) GT 0 ) THEN FREE_LUN, File_Id
      RETURN, FAILURE
    ENDIF
    MsgSwitch = 1
  ENDELSE



  ; Get Initialise variables for reading
  ; --------------------------------
  n_Channels      = 0L
  Sensor_Name     = ''
  Satellite_Name  = ''
  Sensor_Id       = ''
  WMO_Satellite_ID= 0L
  WMO_Sensor_ID   = 0L
  Microwave_Flag  = 0L
  Sensor_Type     = 0L

  
  ; Create a local SensorInfo object
  ; --------------------------------
  si = OBJ_NEW('SensorInfo',Debug=Debug)

  
  ; Open the file
  ; -------------
  GET_LUN, File_Id
  OPENR, File_Id, Filename

  
  ; Loop over comment lines
  ; -----------------------
  Line_Buffer = ' '
  REPEAT BEGIN
    ; Save the current file position
    POINT_LUN, -File_Id, File_Ptr
    
    ; Read a line from the file
    READF, File_Id, Line_Buffer
    
    ; Exit loop if this is NOT a comment or blank line
  ENDREP UNTIL (STRMID(Line_Buffer,0,1) NE '!' AND STRLEN(Line_Buffer) NE 0)


  ; Restore file position to before the previous read
  ; -------------------------------------------------
  POINT_LUN, File_Id, File_Ptr
  
  
  ; Begin loop over sensors
  ; -----------------------
  n_Sensors = 0L
  WHILE ( NOT EOF(File_Id) ) DO BEGIN
  
    ; Read a line from the file
    READF, File_Id, Line_Buffer

    ; Cycle loop if this is a blank line
    IF ( STRLEN(Line_Buffer) EQ 0 ) THEN CONTINUE
    
    ; Increment sensor counter
    ++n_Sensors
    
    ; Read the SensorInfo line into variables
    READS, FORMAT=SENSORINFO_FORMAT, Line_Buffer, Sensor_Name     , $
                                                  Satellite_Name  , $
                                                  Sensor_Id       , $
                                                  Microwave_Flag  , $
                                                  WMO_Sensor_ID   , $
                                                  WMO_Satellite_ID, $
                                                  n_Channels      
    
    ; Allocate the SensorInfo object
    Result = si->SensorInfo::Allocate(n_Channels,Debug=Debug)
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error allocating SensorInfo structure for '+STRTRIM(Sensor_Id,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch

    ; Read the channel information
    ChannelInfo = DBLARR(3,n_Channels)
    READF, File_Id, ChannelInfo
    Sensor_Channel = LONG(REFORM(ChannelInfo[0,*]))
    Use_Flag       = LONG(REFORM(ChannelInfo[1,*]))
    Noise          = REFORM(ChannelInfo[2,*])
    
    ; Set the values in the SensorInfo structure
    Result = si->SensorInfo::Set( Sensor_Name      = Sensor_Name     , $
                                  Satellite_Name   = Satellite_Name  , $
                                  Sensor_Id        = Sensor_Id       , $
                                  WMO_Satellite_ID = WMO_Satellite_ID, $
                                  WMO_Sensor_ID    = WMO_Sensor_ID   , $
                                  Microwave_Flag   = Microwave_Flag  , $
                                  Sensor_Type      = Sensor_Type     , $
                                  Sensor_Channel   = Sensor_Channel  , $
                                  Use_Flag         = Use_Flag        , $
                                  Noise            = Noise           , $
                                  Debug=Debug )
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error setting SensorInfo structure values for '+STRTRIM(Sensor_Id,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch

    ; Add the SensorInfo object to the list
    Result = self->Add_Node(si,Debug=Debug)    
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error adding SensorInfo node to list for '+STRTRIM(Sensor_Id,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
               
    ; Destroy the SensorInfo object for the next read
    Result = si->SensorInfo::Destroy(Debug=Debug)
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error destroying SensorInfo structure for '+STRTRIM(Sensor_Id,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ENDWHILE

  
  ; Clean up
  ; --------
  FREE_LUN, File_Id
  OBJ_DESTROY, si, Debug=Debug

  
  ; Output an info message
  ; ----------------------
  MESSAGE, 'FILE: '+STRTRIM(Filename,2)+', N_SENSORS='+STRTRIM(n_Sensors,2), /INFORMATIONAL

     
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION SensorInfo_List::Read
