;+
; NAME:
;       Inquire_SRF_netCDF
;
; PURPOSE:
;       Function to inquire a netCDF SRF format file to obtain the dimensions,
;       channel list, sensor IDs, and global attributes.
;
; CALLING SEQUENCE:
;       Error_Status = Inquire_SRF_netCDF( NC_Filename                        , $  ; Input
;                                          n_Channels       = n_Channels      , $  ; Optional output
;                                          n_Points         = n_Points        , $  ; Optional output
;                                          n_Bands          = n_Bands         , $  ; Optional output
;                                          Sensor_Type      = Sensor_Type     , $  ; Optional output
;                                          Sensor_Channel   = Sensor_Channel  , $  ; Optional output
;                                          Begin_Frequency  = Begin_Frequency , $  ; Optional output
;                                          End_Frequency    = End_Frequency   , $  ; Optional output
;                                          Version          = Version         , $  ; Optional output
;                                          Sensor_ID        = Sensor_ID       , $  ; Optional output
;                                          Title            = Title           , $  ; Optional output
;                                          History          = History         , $  ; Optional output
;                                          Sensor_Name      = Sensor_Name     , $  ; Optional output
;                                          Platform_Name    = Platform_Name   , $  ; Optional output
;                                          Comment          = Comment           )  ; Optional output
;
; INPUT ARGUMENTS:
;       NC_Filename:        Character string specifying the name of the
;                           SRF netCDF format data file to inquire.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN)
;
; OPTIONAL OUTPUT ARGUMENTS:
;       n_Channels:         The number of channels dimension of the
;                           SRF data data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Points:           The number of spectral points used to represent the
;                           SRF for each channel.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Bands:            The number of bands used to represent the
;                           SRF for each channel.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Channel:     The list of channel numbers present in the netCDF
;                           SRF file. The list may not necessarily
;                           start at 1 or contain contiguous values.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Begin_Frequency:    The list of the begin frequency limits for
;                           each channel's SRF.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL(fp)
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       End_Frequency:      The list of the end frequency limits for
;                           each channel's SRF.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL(fp)
;                           DIMENSION:  Rank-1, n_Channels
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
; FUNCTION RESULT:
;       Error_Status:       The return value is an integer defining the error status.
;                           The error codes are defined in the error_codes file.
;                           If == SUCCESS the netCDF file inquiry was successful
;                              == FAILURE - Invalid input filename
;                                         - Error reading the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 25-Jan-2006
;                       paul.vandelst@ssec.wisc.edu
;-

FUNCTION Inquire_SRF_netCDF, NC_Filename,                         $  ; Input
                             n_Channels       = n_Channels      , $  ; Optional output
                             n_Points         = n_Points        , $  ; Optional output
                             n_Bands          = n_Bands         , $  ; Optional output
                             Sensor_Type      = Sensor_Type     , $  ; Optional output
                             Sensor_Channel   = Sensor_Channel  , $  ; Optional output
                             Begin_Frequency  = Begin_Frequency , $  ; Optional output
                             End_Frequency    = End_Frequency   , $  ; Optional output
                             Version          = Version         , $  ; Optional output
                             Sensor_ID        = Sensor_ID       , $  ; Optional output
                             WMO_Satellite_Id = WMO_Satellite_Id, $  ; Optional output
                             WMO_Sensor_Id    = WMO_Sensor_Id   , $  ; Optional output
                             Title            = Title           , $  ; Optional output
                             History          = History         , $  ; Optional output
                             Comment          = Comment              ; Optional output

  ; Set up
  ; ------
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    IF ( N_ELEMENTS(NC_FileId) GT 0 ) THEN NCDF_CONTROL, NC_FileId, /ABORT
    RETURN, FAILURE
  ENDIF    
  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters


  ; Open the netCDF SRF file
  ; ------------------------
  NC_FileId = NCDF_OPEN( NC_Filename, /NOWRITE )
  NCDF_CONTROL, NC_FileId, /VERBOSE


  ; Check input
  ; -----------
  IF ( NOT Valid_String( NC_Filename ) ) THEN $
    MESSAGE, 'Input NC_Filename argument not defined!', $
             /NONAME, /NOPRINT


  ; Get the number of channels dimension
  ; ------------------------------------
  DimID = NCDF_DIMID( NC_FileId, CHANNEL_DIMNAME )
  NCDF_DIMINQ, NC_FileId, DimID, DimName, n_Channels
  
  
  ; Get the sensor type
  ; -------------------
  VarID = NCDF_VARID( NC_FileId, SENSOR_TYPE_VARNAME )
  NCDF_VARGET, NC_FileId, VarID, Sensor_Type
  
  
  ; Get the sensor channel data
  ; ---------------------------
  VarID = NCDF_VARID( NC_FileId, SENSOR_CHANNEL_VARNAME )
  NCDF_VARGET, NC_FileId, VarID, Sensor_Channel

  
  ; Get the channel specific data
  ; -----------------------------
  IF ( ARG_PRESENT(n_Points       ) AND $
       ARG_PRESENT(n_Bands        ) AND $
       ARG_PRESENT(Begin_Frequency) AND $
       ARG_PRESENT(End_Frequency  )     ) THEN BEGIN
    ; Create return arrays
    n_Points        = LONARR(n_Channels)
    n_Bands         = LONARR(n_Channels)
    Begin_Frequency = LONARR(n_Channels)
    End_Frequency   = LONARR(n_Channels)
    ; Loop over channels
    FOR i = 0L, n_Channels-1L DO BEGIN
      ; Create the various dim and var names for this channel
      CreateNames_SRF_netCDF, Sensor_Channel[i]              , $
                              Point_DimName  =Point_DimName  , $
                              Band_DimName   =Band_DimName   , $
                              f1_Band_VarName=f1_Band_VarName, $
                              f2_Band_VarName=f2_Band_VarName   
      ; Retrieve the number of points dimension value
      DimID = NCDF_DIMID( NC_FileId, Point_DimName )
      NCDF_DIMINQ, NC_FileId, DimID, DimName, n
      n_Points[i] = n
      ; Retrieve the number of bands dimension value
      DimID = NCDF_DIMID( NC_FileId, Band_DimName )
      NCDF_DIMINQ, NC_FileId, DimID, DimName, n
      n_Bands[i] = n
      ; Retrieve the begin frequency
      VarID = NCDF_VARID( NC_FileId, f1_Band_VarName )
      NCDF_VARGET, NC_FileId, VarID, f_Band
      Begin_Frequency[i] = f_Band[0]
      ; Retrieve the end frequency
      VarID = NCDF_VARID( NC_FileId, f2_Band_VarName )
      NCDF_VARGET, NC_FileId, VarID, f_Band
      End_Frequency[i] = f_Band[n_Bands[i]-1]
    ENDFOR
  ENDIF
  
  
  ; Get the global attributes
  ; -------------------------
  result = ReadGAtts_SRF_netCDF( NC_Filename                      , $  ; Input
                                 NC_FileId                        , $  ; Input
                                 Release         =Release         , $  ; Optional output
                                 Version         =Version         , $  ; Optional output
                                 Sensor_Id       =Sensor_Id       , $  ; Optional output
                                 WMO_Satellite_Id=WMO_Satellite_Id, $  ; Optional output
                                 WMO_Sensor_Id   =WMO_Sensor_Id   , $  ; Optional output
                                 Title           =Title           , $  ; Optional output
                                 History         =History         , $  ; Optional output
                                 Comment         =Comment           )  ; Optional output
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading global attributes from '+STRTRIM(NC_Filename,2), $
             /NONAME, /NOPRINT


  ; Done
  ; ----
  NCDF_CLOSE, NC_FileId
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Inquire_SRF_netCDF
