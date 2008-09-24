;+
; NAME:
;       Read_SRF_netCDF
;
; PURPOSE:
;       Function to read a selected channel's SRF data from a netCDF SRF
;       format file.
;
; CALLING SEQUENCE:
;       Error_Status = Read_SRF_netCDF( NC_Filename, $  ! Input  
;                                       Channel    , $  ! Input  
;                                       SRF          )  ! Output 
;
; INPUT ARGUMENTS:
;       NC_Filename:  Character string specifying the name of the netCDF SRF
;                     format data file to read.
;                     UNITS:      N/A
;                     TYPE:       CHARACTER(*)
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT( IN )
;
;       Channel:      Channel number for which the SRF data is required.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT( IN )
;
; OUTPUT ARGUMENTS:
;       SRF:          Structure containing the requested SRF data.
;                     UNITS:      N/A
;                     TYPE:       {SRF}
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT( OUT )
;
; FUNCTION RESULT:
;       Error_Status: The return value is an integer defining the error
;                     status. The error codes are defined in the
;                     error_codes include file.
;                     If == SUCCESS the netCDF data read was successful
;                        == FAILURE an unrecoverable error occurred
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;
;-

FUNCTION Read_SRF_netCDF, NC_Filename, $ ; Input
                          Channel    , $ ; Input
                          SRF            ; Output

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
    MESSAGE, 'Input NC_Filename argument not defined!', $
             /NONAME, /NOPRINT
  ; Destroy the SRF structure if required
  IF ( N_ELEMENTS(SRF) GT 0 ) THEN BEGIN
    IF ( Associated_SRF( SRF, /ANY_Test ) ) THEN BEGIN
      Result = Destroy_SRF( SRF )
      IF ( Result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying SRF structure.', $
                 /NONAME, /NOPRINT
    ENDIF
    Undefine, SRF
  ENDIF
  SRF = {SRF}


  ; Check that SRF channel is valid
  ; -------------------------------
  ; Get the sensor channel list
  Result = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error inquiring SRF file '+NC_Filename+' for sensor channel list', $
             /NONAME, /NOPRINT
  ; Check if the requested channel is in the list at all, or more than once
  Channel_Idx = (WHERE( Sensor_Channel EQ Channel, n ))[0]
  IF ( n LT 1 ) THEN $
    MESSAGE, 'SRF channel '+STRTRIM(Channel,2)+' is not in the sensor channel list for '+NC_Filename, $
             /NONAME, /NOPRINT
  IF ( n GT 1 ) THEN $
    MESSAGE, 'Check '+NC_Filename+' file! SRF channel '+STRTRIM(Channel,2)+$
             ' occurs multiple times in the sensor channel list', $
             /NONAME, /NOPRINT

  ; Read some of the global attributes
  ; ----------------------------------
  Result = Inquire_SRF_netCDF( NC_Filename                        , $  ; Input
                               Version          = Version         , $  ; Optional output
                               Sensor_ID        = Sensor_ID       , $  ; Optional output
                               WMO_Satellite_ID = WMO_Satellite_ID, $  ; Optional output
                               WMO_Sensor_ID    = WMO_Sensor_ID   , $  ; Optional output
                               Sensor_Type      = Sensor_Type       )  ; Optional output
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error occurred reading global attributes from '+NC_Filename, $
             /NONAME, /NOPRINT
  
  
  ; Create the SRF dimension and variable names for the current channel
  ; -------------------------------------------------------------------
  CreateNames_SRF_netCDF, Channel, $
                          Point_DimName    =Point_DimName    , $ 
                          Band_DimName     =Band_DimName     , $ 
                          Response_VarName =Response_VarName , $ 
                          f1_Band_VarName  =f1_Band_VarName  , $ 
                          f2_Band_VarName  =f2_Band_VarName  , $ 
                          npts_Band_VarName=npts_Band_VarName 


  ; Open the netCDF SRF file
  ; ------------------------
  NC_FileId = NCDF_OPEN( NC_Filename, /NOWRITE )


  ; Retrieve channel dimension values
  ; ---------------------------------
  ; Retrieve the number of points dimension value
  DimID = NCDF_DIMID( NC_FileId, Point_DimName )
  NCDF_DIMINQ, NC_FileId, DimID, DimName, n_Points
  ; Retrieve the number of bandss dimension value
  DimID = NCDF_DIMID( NC_FileId, Band_DimName )
  NCDF_DIMINQ, NC_FileId, DimID, DimName, n_Bands

  
  ; Allocate the output SRF structure
  ; ---------------------------------
  Result = Allocate_SRF( n_Points,SRF,n_Bands=n_Bands )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error occurred allocating SRF structure.', $
             /NONAME, /NOPRINT

  ; Set the sensor and channel values
  ; ---------------------------------
  SRF.Version          = Version
  SRF.Sensor_ID        = Sensor_ID 
  SRF.WMO_Satellite_ID = WMO_Satellite_ID
  SRF.WMO_Sensor_ID    = WMO_Sensor_ID   
  SRF.Sensor_Type      = Sensor_Type
  SRF.Channel          = Channel


  ; Read the channel dependent data
  ; -------------------------------
  ; The integrated SRF value
  VarID = NCDF_VARID( NC_FileId, INTEGRATED_SRF_VARNAME )
  NCDF_VARGET, NC_FileId, VarID, x
  SRF.Integrated_SRF = x[Channel_Idx]
  ; The summed SRF value
  VarID = NCDF_VARID( NC_FileId, SUMMATION_SRF_VARNAME )
  NCDF_VARGET, NC_FileId, VarID, x
  SRF.Summation_SRF = x[Channel_Idx]


  ; Read the band dependent data
  ; ----------------------------
  ; The band begin frequencies
  VarID = NCDF_VARID( NC_FileId, f1_Band_VarName )
  NCDF_VARGET, NC_FileId, VarID, *SRF.f1_Band
  ; The band end frequencies
  VarID = NCDF_VARID( NC_FileId, f2_Band_VarName )
  NCDF_VARGET, NC_FileId, VarID, *SRF.f2_Band
  ; The number of band points
  VarID = NCDF_VARID( NC_FileId, npts_Band_VarName )
  NCDF_VARGET, NC_FileId, VarID, *SRF.npts_Band


  ; Read the SRF response
  ; ---------------------
  VarID = NCDF_VARID( NC_FileId, Response_VarName )
  NCDF_VARGET, NC_FileId, VarID, *SRF.Response

  
  ; Compute the SRF frequency grid 
  ; ------------------------------
  Result = Frequency_SRF( SRF )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error computing frequency grid for channel '+STRTRIM(Channel,2)+ $
             ' SRF from '+NC_Filename, $
             /NONAME, /NOPRINT

  ; Done
  ; ----
  NCDF_CLOSE, NC_FileId
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Read_SRF_netCDF
