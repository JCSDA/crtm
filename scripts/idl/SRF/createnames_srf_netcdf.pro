;+
; Procedure to create channel-based dimension and variable names
; for use in accessing netCDF format SRF datafiles.
;
PRO CreateNames_SRF_netCDF, Channel                            , $  ; Input
                            Channel_Name     =Channel_Name     , $  ; Output
                            Point_DimName    =Point_DimName    , $  ; Output
                            Band_DimName     =Band_DimName     , $  ; Output
                            Response_VarName =Response_VarName , $  ; Output
                            f1_Band_VarName  =f1_Band_VarName  , $  ; Output
                            f2_Band_VarName  =f2_Band_VarName  , $  ; Output
                            npts_Band_VarName=npts_Band_VarName     ; Output
;-
  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
  ENDIF

  ; Create the channel string
  c = STRTRIM(Channel,2)

  ; Construct all the other names
  Channel_Name      = c
  Point_DimName     = 'channel_'+c+'_n_points'
  Band_DimName      = 'channel_'+c+'_n_bands'
  Response_VarName  = 'channel_'+c+'_response'
  f1_Band_VarName   = 'channel_'+c+'_f1_band'
  f2_Band_VarName   = 'channel_'+c+'_f2_band'
  npts_Band_VarName = 'channel_'+c+'_npts_band'

  ; Done
  CATCH, /CANCEL
 
END ; PRO CreateNames_SRF_netCDF
