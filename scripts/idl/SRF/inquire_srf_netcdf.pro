;+
; NAME:
;       Inquire_SRF_netCDF
;
; PURPOSE:
;       Function to inquire a netCDF SRF format file to obtain the number of
;       channels, channel list, sensor IDs, and global attributes.
;
; CALLING SEQUENCE:
;       Error_Status = Inquire_SRF_netCDF( NC_Filename,                         $  ! Input
;                                          n_Channels       = n_Channels,       $  ! Optional output
;                                          n_Points         = n_Points,         $  ! Optional output
;                                          Channel_List     = Channel_List,     $  ! Optional output
;                                          Begin_Frequency  = Begin_Frequency,  $  ! Optional output
;                                          End_Frequency    = End_Frequency,    $  ! Optional output
;                                          WMO_Satellite_ID = WMO_Satellite_ID, $  ! Optional output
;                                          WMO_Sensor_ID    = WMO_Sensor_ID,    $  ! Optional output
;                                          Title            = Title,            $  ! Optional output
;                                          History          = History,          $  ! Optional output
;                                          Sensor_Name      = Sensor_Name,      $  ! Optional output
;                                          Platform_Name    = Platform_Name,    $  ! Optional output
;                                          Comment          = Comment           )  ! Optional output
;
; INPUT ARGUMENTS:
;       NC_Filename:        Character string specifying the name of the netCDF
;                           format SRF data file to inquire.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER( * )
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( IN )
;
; OPTIONAL OUTPUT ARGUMENTS:
;       n_Channels:         The number of channels dimension of the
;                           SRF data data.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       n_Points:           The number of spectral points used to represent the
;                           SRF for each channel.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Channel_List:       The list of channel numbers present in the netCDF
;                           SRF file. The list may not necessarily
;                           start at 1 or contain contiguous values.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Begin_Frequency:    The list of the begin frequency limits for
;                           each channel's SRF.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL( fp_kind )
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       End_Frequency:      The list of the end frequency limits for
;                           each channel's SRF.
;                           UNITS:      Inverse centimetres (cm^-1)
;                           TYPE:       REAL( fp_kind )
;                           DIMENSION:  Rank-1, n_Channels
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       WMO_Satellite_ID:   The WMO code for identifying satellite
;                           platforms. Taken from the WMO common
;                           code tables at:
;                             http://www.wmo.ch/web/ddbs/Code-tables.html
;                           The Satellite ID is from Common Code
;                           table C-5, or code table 0 01 007 in BUFR
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       WMO_Sensor_ID:      The WMO code for identifying a satelite
;                           sensor. Taken from the WMO common
;                           code tables at:
;                             http://www.wmo.ch/web/ddbs/Code-tables.html
;                           The Sensor ID is from Common Code
;                           table C-8, or code table 0 02 019 in BUFR
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Title:              Character string written into the TITLE global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       History:            Character string written into the HISTORY global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Sensor_Name:        Character string written into the SENSOR_NAME global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Platform_Name:      Character string written into the PLATFORM_NAME global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
;       Comment:            Character string written into the COMMENT global
;                           attribute field of the netCDF SRF file.
;                           UNITS:      N/A
;                           TYPE:       CHARACTER(*)
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
;
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

FUNCTION Inquire_SRF_netCDF, SRF_Filename,                        $  ; Input
                             n_Channels       = n_Channels,       $  ; Optional output
                             n_Points         = n_Points,         $  ; Optional output
                             Channel_List     = Channel_List,     $  ; Optional output
                             Begin_Frequency  = Begin_Frequency,  $  ; Optional output
                             End_Frequency    = End_Frequency,    $  ; Optional output
                             WMO_Satellite_ID = WMO_Satellite_ID, $  ; Optional output
                             WMO_Sensor_ID    = WMO_Sensor_ID,    $  ; Optional output
                             Title            = Title,            $  ; Optional output
                             History          = History,          $  ; Optional output
                             Sensor_Name      = Sensor_Name,      $  ; Optional output
                             Platform_Name    = Platform_Name,    $  ; Optional output
                             Comment          = Comment              ; Optional output


  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG + $
             '; Error inquiring netCDF SRF file ' + $
             STRTRIM( SRF_FileNAME, 2 ), $
             /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters

  ; Check input
  n_Arguments = 1
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
  IF ( NOT Valid_String( SRF_FileNAME ) ) THEN $
    MESSAGE, 'Input SRF_Filename argument not defined!', $
             /NONAME, /NOPRINT

  ; Open the netCDF SRF file
  FileID = NCDF_OPEN( SRF_Filename, /NOWRITE )


  ; Get the dimensional variables
  ;
  ; The channel list
  VarID = NCDF_VARID( FileID, CHANNEL_LIST_VARNAME )
  NCDF_VARGET, FileID, VarID, Channel_List  
  ; The frequency limits
  VarID = NCDF_VARID( FileID, BEGIN_FREQUENCY_VARNAME )
  NCDF_VARGET, FileID, VarID, Begin_Frequency
  VarID = NCDF_VARID( FileID, END_FREQUENCY_VARNAME )
  NCDF_VARGET, FileID, VarID, End_Frequency
  ; The sensor IDs
  VarID = NCDF_VARID( FileID, WMO_SATELLITE_ID_VARNAME )
  NCDF_VARGET, FileID, VarID, WMO_Satellite_ID
  VarID = NCDF_VARID( FileID, WMO_SENSOR_ID_VARNAME )
  NCDF_VARGET, FileID, VarID, WMO_Sensor_ID


  ; Get the dimensions
  ;
  ; The channel dimension
  DimID = NCDF_DIMID( FileID, CHANNEL_DIMNAME )
  NCDF_DIMINQ, FileID, DimID, DimName, n_Channels
  ; The number of points dimension
  n_Points = LONARR( n_Channels )
  FOR l = 0, n_Channels-1L DO BEGIN
    DimName = 'channel_'+STRTRIM(Channel_List[l],2)+'_n_points'
    DimID = NCDF_DIMID( FileID, DimName )
    NCDF_DIMINQ, FileID, DimID, Dummy, n
    n_Points[l] = n
  ENDFOR


  ; Get the global attributes
  ;
  ; The title
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, TITLE_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute ' + TITLE_GATTNAME + ' not found. Skipping...', $
             /INFORMATIONAL
    Title = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, TITLE_GATTNAME, Title
    Title = STRING( Title )
  ENDELSE

  ; The history
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, HISTORY_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute ' + HISTORY_GATTNAME + ' not found. Skipping...', $
             /INFORMATIONAL
    History = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, HISTORY_GATTNAME, History
    History = STRING( History )
  ENDELSE

  ; The sensor name
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, SENSOR_NAME_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute ' + SENSOR_NAME_GATTNAME + ' not found. Skipping...', $
             /INFORMATIONAL
    Sensor_Name = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, SENSOR_NAME_GATTNAME, Sensor_Name
    Sensor_Name = STRING( Sensor_Name )
  ENDELSE

  ; The platform name
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, PLATFORM_NAME_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute ' + PLATFORM_NAME_GATTNAME + ' not found. Skipping...', $
             /INFORMATIONAL
    Platform_Name = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, PLATFORM_NAME_GATTNAME, Platform_Name
    Platform_Name = STRING( Platform_Name )
  ENDELSE

  ; The comment
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, COMMENT_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute ' + COMMENT_GATTNAME + ' not found. Skipping...', $
             /INFORMATIONAL
    Comment = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, COMMENT_GATTNAME, Comment
    Comment = STRING( Comment )
  ENDELSE

  NCDF_CLOSE, FileID


  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Inquire_SRF_netCDF
