;+
; NAME:
;       Write_SRF_netCDF
;
; PURPOSE:
;       Function to write data in an SRF structure to a netCDF format
;       SRF file.
;
; CALLING SEQUENCE:
;       Error_Status = Write_SRF_netCDF( NC_Filename, $  ; Input
;                                        SRF        , )  ; Input
;
; INPUT ARGUMENTS:
;       NC_Filename:     Character string specifying the name of the netCDF
;                        format SRF data file to write to.
;                        UNITS:      N/A
;                        TYPE:       CHARACTER(*)
;                        DIMENSION:  Scalar
;                        ATTRIBUTES: INTENT(IN)
;
;       SRF:             Structure containing the SRF data to write to file.
;                        UNITS:      N/A
;                        TYPE:       {SRF}
;                        DIMENSION:  Scalar
;                        ATTRIBUTES: INTENT(IN)
;
; FUNCTION RESULT:
;       Error_Status:    The return value is an integer defining the error
;                        status. The error codes are defined in the
;                        error_codes include file.
;                        If == SUCCESS the netCDF data write was successful
;                           == FAILURE an unrecoverable error occurred
;                        UNITS:      N/A
;                        TYPE:       INTEGER
;                        DIMENSION:  Scalar
;
;-

FUNCTION Write_SRF_netCDF, NC_Filename, $  ; Input
                           SRF             ; Input

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
  IF ( NOT Valid_String( NC_Filename ) ) THEN $
    MESSAGE, 'Input NC_Filename argument not defined;', $
             /NONAME, /NOPRINT
  fInfo = FILE_INFO(NC_Filename)
  IF ( fInfo.EXISTS EQ FALSE ) THEN $
    MESSAGE, 'Input NC_Filename not found.', $
             /NONAME, /NOPRINT
  ; Check the structure
  IF ( Is_A_SRF_Structure( SRF, /Quiet ) EQ FALSE ) THEN $
    MESSAGE, 'Input SRF argument is not an {SRF} structure', $
             /NONAME, /NOPRINT
  ; ALL *input* pointers must be associated
  IF ( Associated_SRF( SRF ) EQ FALSE ) THEN $
    MESSAGE, 'Some or all SRF pointer members are NOT associated.', $
             /NONAME, /NOPRINT
  ; Check SRF channel is valid
  IF ( SRF.Channel LT 1 ) THEN $
    MESSAGE, 'Invalid SRF channel, '+STRTRIM(SRF.Channel,2)+'. Must be > 0.', $
             /NONAME, /NOPRINT
  ; Check SRF array sizes
  IF ( SRF.n_Points LT 1 ) THEN $
    MESSAGE, 'Invalid no of SRF points, '+STRTRIM(SRF.n_Points,2)+'. Must be > 0.', $
             /NONAME, /NOPRINT
  IF ( SRF.n_Bands LT 1 ) THEN $
    MESSAGE, 'Invalid no of SRF bands, '+STRTRIM(SRF.n_Bands,2)+'. Must be > 0.', $
             /NONAME, /NOPRINT

  ; Select the frequency units string
  CASE SRF.Sensor_Type OF
    MICROWAVE_SENSOR  : Frequency_Units = 'Gigahertz (GHz)'
    INFRARED_SENSOR   : Frequency_Units = 'Inverse centimetres (cm^-1)'
    VISIBLE_SENSOR    : Frequency_Units = 'Inverse centimetres (cm^-1)'
    ULTRAVIOLET_SENSOR: Frequency_Units = 'Inverse centimetres (cm^-1)' 
    ELSE: MESSAGE, 'Invalid sensor type', /NONAME, /NOPRINT
  ENDCASE


  ; Check that the SRF sensor type is consistent for the file
  ; ---------------------------------------------------------
  ; Get the sensor type
  Result = Inquire_SRF_netCDF( NC_Filename,Sensor_Type=Sensor_Type )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE,'Error obtaining '+SENSOR_TYPE_VARNAME+' from '+NC_Filename, /NONAME, /NOPRINT
  ; Check if it's the same
  IF ( SRF.Sensor_Type NE Sensor_Type ) THEN $
    MESSAGE, 'File and structure sensor type flags are different!', /NONAME, /NOPRINT
    

  ; Check that the SRF channel is valid for the file
  ; ------------------------------------------------
  ; Read the sensor channel list
  Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE,'Error reading  '+SENSOR_CHANNEL_VARNAME+' data from '+NC_Filename, /NONAME, /NOPRINT
  ; Check if the SRF channel is in the list at all, or more than once
  Channel_Idx = WHERE(Sensor_Channel EQ SRF.Channel, count)
  IF ( count LT 1 ) THEN $
    MESSAGE, 'SRF channel '+STRTRIM(SRF.Channel,2)+' is not in the sensor channel list for '+$
             NC_Filename, /NONAME, /NOPRINT
  IF ( count GT 1 ) THEN $
    MESSAGE, 'Check '+NC_Filename+' file; SRF channel '+STRTRIM(SRF.Channel,2)+$
             ' occurs multiple times in the sensor channel list', /NONAME, /NOPRINT


  ; Create the SRF dimension and variable names for the current channel
  ; -------------------------------------------------------------------
  CreateNames_SRF_netCDF, SRF.Channel, $
                          Channel_Name     =Channel_Name     , $
                          Point_DimName    =Point_DimName    , $
                          Band_DimName     =Band_DimName     , $
                          Response_VarName =Response_VarName , $
                          f1_Band_VarName  =f1_Band_VarName  , $
                          f2_Band_VarName  =f2_Band_VarName  , $
                          npts_Band_VarName=npts_Band_VarName


  ; Open the file and put it in define mode
  ; ---------------------------------------
  NC_FileId = NCDF_OPEN(NC_Filename,/WRITE)
  NCDF_CONTROL, NC_FileId, /VERBOSE
  NCDF_CONTROL, NC_FileId, /REDEF

  
  ; Define the dimensions for this channel
  ; --------------------------------------
  n_Bands_DimId  = NCDF_DIMDEF(NC_FileId,Band_DimName,SRF.n_Bands)
  n_Points_DimId = NCDF_DIMDEF(NC_FileId,Point_DimName,SRF.n_Points)

  
  ; Define the band variables
  ; -------------------------
  ; The band begin frequency variable
  f1_Band_VarId = NCDF_VARDEF(NC_FileId,f1_Band_VarName,n_Bands_DimId,/DOUBLE)
  NCDF_ATTPUT,NC_FileId,f1_Band_VarId,LONGNAME_ATTNAME,F1_BAND_LONGNAME 
  NCDF_ATTPUT,NC_FileId,f1_Band_VarId,DESCRIPTION_ATTNAME,F1_BAND_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,f1_Band_VarId,UNITS_ATTNAME,Frequency_Units
  NCDF_ATTPUT,NC_FileId,f1_Band_VarId,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE
  ; The band end frequency variable
  f2_Band_VarId = NCDF_VARDEF(NC_FileId,f2_Band_VarName,n_Bands_DimId,/DOUBLE)
  NCDF_ATTPUT,NC_FileId,f2_Band_VarId,LONGNAME_ATTNAME,F2_BAND_LONGNAME 
  NCDF_ATTPUT,NC_FileId,f2_Band_VarId,DESCRIPTION_ATTNAME,F2_BAND_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,f2_Band_VarId,UNITS_ATTNAME,Frequency_Units
  NCDF_ATTPUT,NC_FileId,f2_Band_VarId,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE
  ; The band npts variable
  npts_Band_VarId = NCDF_VARDEF(NC_FileId,npts_Band_VarName,n_Bands_DimId,/LONG)
  NCDF_ATTPUT,NC_FileId,npts_Band_VarId,LONGNAME_ATTNAME,NPTS_BAND_LONGNAME 
  NCDF_ATTPUT,NC_FileId,npts_Band_VarId,DESCRIPTION_ATTNAME,NPTS_BAND_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,npts_Band_VarId,UNITS_ATTNAME,NPTS_BAND_UNITS
  NCDF_ATTPUT,NC_FileId,npts_Band_VarId,FILLVALUE_ATTNAME,NPTS_BAND_FILLVALUE


  ; Define the response variable
  ; ----------------------------
  Response_VarId = NCDF_VARDEF(NC_FileId,Response_VarName,n_Points_DimId,/DOUBLE)
  NCDF_ATTPUT,NC_FileId,Response_VarId,LONGNAME_ATTNAME,RESPONSE_LONGNAME 
  NCDF_ATTPUT,NC_FileId,Response_VarId,DESCRIPTION_ATTNAME,RESPONSE_DESCRIPTION
  NCDF_ATTPUT,NC_FileId,Response_VarId,UNITS_ATTNAME,FREQUENCY_UNITS
  NCDF_ATTPUT,NC_FileId,Response_VarId,FILLVALUE_ATTNAME,RESPONSE_FILLVALUE

  
  ; Put the file into data mode 
  ; ---------------------------
  NCDF_CONTROL, NC_FileId, /ENDEF


  ; Write the channel dependent data
  ; --------------------------------
  ; The integrated SRF value
  VarId = NCDF_VARID(NC_FileId,INTEGRATED_SRF_VARNAME)
  NCDF_VARPUT,NC_FileId,VarId,SRF.Integrated_SRF,OFFSET=Channel_Idx
  ; The summed SRF value
  VarId = NCDF_VARID(NC_FileId,SUMMATION_SRF_VARNAME)
  NCDF_VARPUT,NC_FileId,VarId,SRF.Summation_SRF,OFFSET=Channel_Idx


  ; Write the band dependent data
  ; -----------------------------
  NCDF_VARPUT,NC_FileId,f1_Band_VarId,*SRF.f1_Band
  NCDF_VARPUT,NC_FileId,f2_Band_VarId,*SRF.f2_Band
  NCDF_VARPUT,NC_FileId,npts_Band_VarId,*SRF.npts_Band


  ; Write the SRF response
  ; ----------------------
  NCDF_VARPUT,NC_FileId,Response_VarId,*SRF.Response


  ; Done
  ; ----
  NCDF_CLOSE, NC_FileId
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Write_SRF_netCDF
