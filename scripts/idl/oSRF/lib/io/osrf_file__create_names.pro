;+
; NAME:
;       OSRF_File::Create_Names
;
; PURPOSE:
;       The OSRF_File::Create_Names procedure method constructs
;       dimension and variable names based on OSRF channel number and
;       the number of bands.
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF_File methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Create_Names, $
;         Channel                              , $  ; Input argument          
;         n_Bands                              , $  ; Optional input argument 
;         Debug             = Debug            , $  ; Input keyword
;         n_Bands_DimName   = n_Bands_DimName  , $  ; Output keyword
;         n_Points_DimName  = n_Points_DimName , $  ; Output keyword
;         f1_VarName        = f1_VarName       , $  ; Output keyword
;         f2_VarName        = f2_VarName       , $  ; Output keyword
;         Frequency_VarName = Frequency_VarName, $  ; Output keyword
;         Response_VarName  = Response_VarName      ; Output keyword
;
; INPUT ARGUMENTS:
;       Channel:           The sensor channel number.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN)
;
; OPTIONAL INPUT ARGUMENTS:
;       n_Bands:           The number of bands for the sensor channel SRF.
;                          If not specified, the default value is 1.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INPUT KEYWORDS:
;       Debug:             Set this keyword for debugging.
;                          If NOT SET => Error handler is enabled. (DEFAULT)
;                             SET     => Error handler is disabled; Routine
;                                        traceback output is enabled.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       n_Bands_DimName:   The dimension name for the number of SRF bands
;                          for the specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Points_DimName:  The dimension names for the number of points used
;                          to specfiy the SRF data for the band(s) for the 
;                          specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Rank-1 (n_Bands)
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f1_VarName:        The variable name for the band begin frequencies
;                          of the specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       f2_VarName:        The variable name for the band end frequencies
;                          of the specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Frequency_VarName: The variable names for the SRF frequency arrays
;                          of the band(s) for the specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Rank-1 (n_Bands)
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Response_VarName:  The variable names for the SRF response arrays
;                          of the band(s) for the specified sensor channel.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Rank-1 (n_Bands)
;                          ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Create_Names, $
  Channel, $  ; Input argument
  n_Bands, $  ; Optional input argument
  Debug                  = debug                 , $  ; Input keyword
  n_Bands_DimName        = n_bands_dimname       , $  ; Output keyword
  n_Temperatures_DimName = n_temperatures_dimname, $  ; Output keyword
  n_Points_DimName       = n_points_dimname      , $  ; Output keyword
  f1_VarName             = f1_varname            , $  ; Output keyword
  f2_VarName             = f2_varname            , $  ; Output keyword
  Frequency_VarName      = frequency_varname     , $  ; Output keyword
  Response_VarName       = response_varname      , $  ; Output keyword
  T_VarName              = t_varname             , $  ; Output keyword
  Teff_VarName           = teff_varname          , $  ; Output keyword
  Tfit_VarName           = tfit_varname               ; Output keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check n_Bands argument
  _n_bands = 1L  ; Default
  IF ( N_ELEMENTS(n_Bands) GT 0 ) THEN BEGIN
    _n_bands = LONG(n_Bands[0])
    IF ( _n_bands LT 1 ) THEN $
      MESSAGE, 'Invalid number of bands specified.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
  
  
  ; Construct the dimension and variable names
  ; ...Channel only
  ch = STRTRIM(Channel,2)
  n_bands_dimname        = 'ch'+ch+'_n_Bands'
  n_temperatures_dimname = 'ch'+ch+'_n_Temperatures'
  f1_varname             = 'ch'+ch+'_f1'
  f2_varname             = 'ch'+ch+'_f2'
  t_varname              = 'ch'+ch+'_T'
  teff_varname           = 'ch'+ch+'_Teff'
  tfit_varname           = 'ch'+ch+'_Tfit'
  ; ...Channel and band
  b  = STRTRIM(LINDGEN(_n_bands)+1,2)
  n_points_dimname  = 'ch'+ch+'_b'+b+'_n_Points'
  frequency_varname = 'ch'+ch+'_b'+b+'_Frequency'
  response_varname  = 'ch'+ch+'_b'+b+'_Response'

END
