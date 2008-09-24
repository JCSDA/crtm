;
; NAME:
;       Create_SRF_netCDF
;
; PURPOSE:
;       Function to create a netCDF SRF data file for writing.
;
; CALLING SEQUENCE:
;       Error_Status = Create_SRF_netCDF( NC_Filename                      , $  ; Input
;                                         Sensor_Type                      , $  ; Input
;                                         Sensor_Channel                   , $  ; Input
;                                         Version         =Version         , $  ; Optional input
;                                         Sensor_Id       =Sensor_Id       , $  ; Optional input
;                                         WMO_Satellite_Id=WMO_Satellite_Id, $  ; Optional input
;                                         WMO_Sensor_Id   =WMO_Sensor_Id   , $  ; Optional input
;                                         Title           =Title           , $  ; Optional input
;                                         History         =History         , $  ; Optional input
;                                         Comment         =Comment           )  ; Optional input
;
; INPUT ARGUMENTS:
;       NC_Filename:        Character string specifying the name of the
;                           netCDF SRF format data file to create.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
;       Sensor_Channel:     The list of channel numbers to be written
;                           to the SRF file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1
;                           ATTRIBUTES: INTENT(IN)
;
; OPTIONAL INPUT ARGUMENTS:
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
;       Message_Log:        Character string specifying a filename in which
;                           any messages will be logged. If not specified,
;                           or if an error occurs opening the log file, the
;                           default action is to output messages to standard
;                           output.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Error_Status:       The return value is an integer defining the error status.
;                           The error codes are defined in the error_codes include file.
;                           If == SUCCESS the SRF netCDF file creation was successful
;                              == FAILURE an unrecoverable error occurred 
;                              == WARNING - an error occurred writing any of the
;                                           supplied global attributes.
;                                         - an error occurred closing the netCDF file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;
;-

FUNCTION Create_SRF_netCDF, NC_Filename                      , $  ; Input
                            Sensor_Type                      , $  ; Input
                            Sensor_Channel                   , $  ; Input
                            Version         =Version         , $  ; Optional input
                            Sensor_Id       =Sensor_Id       , $  ; Optional input
                            WMO_Satellite_Id=WMO_Satellite_Id, $  ; Optional input
                            WMO_Sensor_Id   =WMO_Sensor_Id   , $  ; Optional input
                            Title           =Title           , $  ; Optional input
                            History         =History         , $  ; Optional input
                            Comment         =Comment              ; Optional input
                            
  ; Set up
  ; ------
  ; Set compilation options
  COMPILE_OPT STRICTARR
  ; Compile the SRF methods by
  ; creating a dummy structure
  x = {SRF}
  Undefine, x
  ; Error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(NC_FileId) GT 0 ) THEN NCDF_CONTROL, NC_FileId, /ABORT
    RETURN, FAILURE
  ENDIF    
  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters


  ; Check input
  ; -----------
  ; Check the sensor type
  IF ( Sensor_Type LT 1 OR Sensor_Type GT N_SENSOR_TYPES ) THEN $
    MESSAGE, 'Invalid SENSOR_TYPE input.', /NONAME, /NOPRINT
  ; Check channel input
  n_Channels = N_ELEMENTS(Sensor_Channel)
  IF ( n_Channels LT 1 ) THEN $
    MESSAGE, 'SENSOR_CHANNEL array must be non-zero size.', /NONAME, /NOPRINT
  loc = WHERE(Sensor_Channel LT 1, count)
  IF ( count GT 1 ) THEN $
    MESSAGE, 'Invalid SENSOR_CHANNEL value found.',/ NONAME, /NOPRINT


  ; Create the data file
  ; --------------------
  NC_FileId = NCDF_CREATE(NC_Filename, /CLOBBER)
  NCDF_CONTROL, NC_FileId, /VERBOSE

  
  ; Define the dimensions
  ; ----------------------
  ; The number of channels
  DimId = NCDF_DIMDEF(NC_FileId,CHANNEL_DIMNAME,n_Channels)

 
  ; Write the global attributes
  ; ---------------------------
  Result = WriteGAtts_SRF_netCDF( NC_Filename                      , $
                                  NC_FileId                        , $
                                  Version         =Version         , $
                                  Sensor_Id       =Sensor_Id       , $
                                  WMO_Satellite_Id=WMO_Satellite_Id, $
                                  WMO_Sensor_Id   =WMO_Sensor_Id   , $
                                  Title           =Title           , $
                                  History         =History         , $
                                  Comment         =Comment           )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error writing global attributes to '+NC_Filename, /NONAME, /NOPRINT
    
    
  ; Define the sensor type scalar variable
  ; --------------------------------------
  Sensor_Type_VarId = NCDF_VARDEF(NC_FileId,SENSOR_TYPE_VARNAME,/LONG)
  NCDF_ATTPUT,NC_FileId,Sensor_Type_VarId,LONGNAME_ATTNAME,SENSOR_TYPE_LONGNAME 
  NCDF_ATTPUT,NC_FileId,Sensor_Type_VarId,DESCRIPTION_ATTNAME,SENSOR_TYPE_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,Sensor_Type_VarId,UNITS_ATTNAME,SENSOR_TYPE_UNITS
  NCDF_ATTPUT,NC_FileId,Sensor_Type_VarId,FILLVALUE_ATTNAME,SENSOR_TYPE_FILLVALUE
  
  ; Define the channel-dimensioned variables and attributes
  ; -------------------------------------------------------
  ; The Sensor_Channel
  Sensor_Channel_VarId = NCDF_VARDEF(NC_FileId,SENSOR_CHANNEL_VARNAME,DimId,/LONG)
  NCDF_ATTPUT,NC_FileId,Sensor_Channel_VarId,LONGNAME_ATTNAME,SENSOR_CHANNEL_LONGNAME 
  NCDF_ATTPUT,NC_FileId,Sensor_Channel_VarId,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,Sensor_Channel_VarId,UNITS_ATTNAME,SENSOR_CHANNEL_UNITS
  NCDF_ATTPUT,NC_FileId,Sensor_Channel_VarId,FILLVALUE_ATTNAME,SENSOR_CHANNEL_FILLVALUE
  ; The Integrated_SRF
  VarId = NCDF_VARDEF(NC_FileId,INTEGRATED_SRF_VARNAME,DimId,/DOUBLE)
  NCDF_ATTPUT,NC_FileId,VarId,LONGNAME_ATTNAME,INTEGRATED_SRF_LONGNAME 
  NCDF_ATTPUT,NC_FileId,VarId,DESCRIPTION_ATTNAME,INTEGRATED_SRF_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,VarId,UNITS_ATTNAME,INTEGRAL_SRF_UNITS
  NCDF_ATTPUT,NC_FileId,VarId,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE
  ; The Summation_SRF
  VarId = NCDF_VARDEF(NC_FileId,SUMMATION_SRF_VARNAME,DimId,/DOUBLE)
  NCDF_ATTPUT,NC_FileId,VarId,LONGNAME_ATTNAME,SUMMATION_SRF_LONGNAME 
  NCDF_ATTPUT,NC_FileId,VarId,DESCRIPTION_ATTNAME,SUMMATION_SRF_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,VarId,UNITS_ATTNAME,INTEGRAL_SRF_UNITS
  NCDF_ATTPUT,NC_FileId,VarId,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE


  ; Take netCDF file out of define mode
  ; -----------------------------------
  NCDF_CONTROL,NC_FileId,/ENDEF


  ; Write the defining data
  ; -----------------------
  NCDF_VARPUT,NC_FileId,Sensor_Type_VarID,Sensor_Type
  NCDF_VARPUT,NC_FileId,Sensor_Channel_VarID,Sensor_Channel


  ; Done
  ; ----
  NCDF_CLOSE, NC_FileId
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Create_SRF_netCDF

