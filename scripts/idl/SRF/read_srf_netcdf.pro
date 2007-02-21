;+
; NAME:
;       Read_SRF_netCDF
;
; PURPOSE:
;       Function to read a selected channel's SRF data from a netCDF SRF
;       format file.
;
; CALLING SEQUENCE:
;       Error_Status = Read_SRF_netCDF( SRF_Filename, $  ! Input  
;                                       Channel,      $  ! Input  
;                                       SRF           )  ! Output 
;
; INPUT ARGUMENTS:
;       SRF_Filename: Character string specifying the name of the netCDF SRF
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
;                     TYPE:       SRF_type
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT( OUT )
;
; FUNCTION RESULT:
;       Error_Status: The return value is an integer defining the error
;                     status. The error codes are defined in the
;                     ERROR_HANDLER module.
;                     If == SUCCESS the netCDF data read was successful
;                        == FAILURE an unrecoverable error occurred
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;
; COMMENTS:
;       Note the INTENT on the output SRF argument is IN OUT rather
;       than just OUT. This is necessary because the argument may be defined on
;       input. To prevent memory leaks, the IN OUT INTENT is a must.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst; CIMSS/SSEC 02-Sep-2003
;                       paul.vandelst@ssec.wisc.edu
;
;-

FUNCTION Read_SRF_netCDF, SRF_Filename, $ ; Input
                          Channel,      $ ; Input
                          SRF             ; Output

  ; Set compilation options
  COMPILE_OPT STRICTARR

  ; Compile the SRF methods by
  ; creating a dummy structure
  x = { SRF }
  Undefine, x

  ; Set up error handler
  @error_codes
  CATCH, error_status
  IF ( error_status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters

  ; Check input
  n_arguments = 3
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
  ; Check that required arguments are defined
  IF ( NOT Valid_String( SRF_Filename ) ) THEN $
    MESSAGE, 'Input SRF_Filename argument not defined!', $
             /NONAME, /NOPRINT

  ; Destroy the SRF structure if required
  IF ( N_ELEMENTS( SRF ) GT 0 ) THEN BEGIN
    IF ( Associated_SRF( SRF, /ANY_Test ) ) THEN BEGIN
      Result = Destroy_SRF( SRF )
      IF ( Result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying SRF structure.', $
                 /NONAME, /NOPRINT
    ENDIF
    Undefine, SRF
  ENDIF
  SRF = { SRF }


  ; Check that SRF channel is valid
  ;
  ; Read the channel dimension and sensor ID values
  Result = Inquire_SRF_netCDF( SRF_Filename, $
                               n_Points         = n_Points        , $
                               Channel_List     = Channel_List    , $
                               Sensor_Name      = Sensor_Name     , $
                               Platform_Name    = Platform_Name   , $
                               Begin_Frequency  = Begin_Frequency , $
                               End_Frequency    = End_Frequency   , $
                               WMO_Satellite_ID = WMO_Satellite_ID, $
                               WMO_Sensor_ID    = WMO_Sensor_ID     )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error inquiring SRF file '+SRF_Filename, $
             /NONAME, /NOPRINT

  Idx = (WHERE( Channel_List EQ Channel, Count ))[0]
  IF ( Count EQ 0 ) THEN $
    MESSAGE, 'SRF channel '+STRTRIM(Channel,2)+' not in channel list for '+SRF_Filename, $
             /NONAME, /NOPRINT


  ; Allocate the SRF structure
  Result = Allocate_SRF( n_Points[Idx], SRF )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating SRF structure.', $
             /NONAME, /NOPRINT


  ; Open the netCDF SRF file
  FileID = NCDF_OPEN( SRF_Filename, /NOWRITE )

  ; Read the SRF data
  VarName = 'channel_'+STRTRIM(Channel,2)+'_response'
  VarID   = NCDF_VARID( FileID, VarName )
  NCDF_VARGET, FileID, VarID, *SRF.Response

  ; Read the integrated data
  VarID = NCDF_VARID( FileID, INTEGRATED_SRF_VARNAME )
  NCDF_VARGET, FileID, VarID, Integrated_SRF
  VarID = NCDF_VARID( FileID, SUMMATION_SRF_VARNAME )
  NCDF_VARGET, FileID, VarID, Summation_SRF
  
  ; Assign the SRF structure components
  SRF.Sensor_Name      = Sensor_Name
  SRF.Platform_Name    = Platform_Name
  SRF.WMO_Satellite_ID = WMO_Satellite_ID
  SRF.WMO_Sensor_ID    = WMO_Sensor_ID
  SRF.Channel          = Channel
  SRF.Begin_Frequency  = Begin_Frequency[Idx]
  SRF.End_Frequency    = End_Frequency[Idx]
  SRF.Integrated_SRF   = Integrated_SRF[Idx]
  SRF.Summation_SRF    = Summation_SRF[Idx]

  ; Compute the frequency grid
  Result = Frequency_SRF( SRF )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error computing SRF frequency grid.', $
             /NONAME, /NOPRINT

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END
