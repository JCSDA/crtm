;
; NAME:
;       Read_hamsr_Raw_SRF
;
; PURPOSE:
;       This (private) helper procedure reads individual channel data
;       files and loads the hamsr SRF data into arrays and lists.
;
; CALLING SEQUENCE:
;       Read_hamsr_Raw_SRF, $
;         Filename     , $  ; Input
;         Channel      , $  ; Input
;         n_bands      , $  ; Input
;         n_points     , $  ; Output
;         frequency    , $  ; Output
;         response     , $  ; Output
;         Debug = debug     ; Input keyword
;
; INPUTS:
;       Filename:    The filename conmtaining the SRF data to be read.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;      
;       Channel:     The channel number being processed
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
;       n_bands:     The channel number being processed
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; OUTPUTS:
;       n_points:    Array containing the number of SRF data points
;                    for each band. NOTE: Can be scalar if only one
;                    band present.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar or rank-1
;                    ATTRIBUTES: INTENT(OUT)
;                    
;       frequency:   List containing the frequency grid information
;                    for the SRF data points for each band.
;                    UNITS:      Inverse centimetres (cm^-1)
;                    TYPE:       LIST
;                    DIMENSION:  N/A
;                    ATTRIBUTES: INTENT(OUT)
;
;       response:    List containing the relative response SRF data
;                    for each band.
;                    UNITS:      N/A
;                    TYPE:       LIST
;                    DIMENSION:  N/A
;                    ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_pro_err_handler: Error handler code for oSRF procedures.
;
;

PRO Read_hamsr_Raw_SRF, $
  Filename     , $  ; Input
  Channel      , $  ; Input
  n_bands      , $  ; Input
  n_points     , $  ; Output
  frequency    , $  ; Output
  response     , $  ; Output
  Debug = debug     ; Input keyword

  COMPILE_OPT HIDDEN

  ; Set up
  @osrf_pro_err_handler
  ; ...Lists for the SRF data
  frequency = LIST()
  response  = LIST()
  
  ; spectral and response indices
  IDX_SPECTRAL = 0 ; Index of wavenumber
  IDX_RESPONSE = 1 ; Index of response
  
  ; Read the data
  n_lines = FILE_LINES(Filename)
  data    = STRARR(n_lines)
  n       = INDGEN(n_lines)
  
  OPENR, fid, Filename, /GET_LUN
  READF, fid, data
  FREE_LUN, fid
  
  n_band_boundaries = n_bands*2L
  
  bound_indices = INTARR(n_band_boundaries)
  bound_indices = WHERE((data[n] - data[n-1] EQ data[n]) OR &
                        (data[n] - data[n+1] EQ data[n]))

  ; Extract the data from the string array
  f = DBLARR(n_bands, n_lines)
  r = DBLARR(n_bands, n_lines)
  FOR i = 0L, n_lines-1L DO BEGIN
    elements = STRSPLIT(data[i], /EXTRACT)
    f[i] = DOUBLE(elements[IDX_SPECTRAL])
    r[i] = DOUBLE(elements[IDX_RESPONSE])
  ENDFOR

  idx = UNIQ(f, SORT(f))
  f = f[idx]
  r = r[idx]
  
  n_points = INTARR(n_bands)
  c1 = 0L
  c2 = 1L
  FOR b = 0L, n_bands-1L DO BEGIN
    n_points[b] = N_ELEMENTS(f[bound_indices[c1]:bound_indices[c2]])
    frequency.Add, f[bound_indices[c1]], f[bound_indices[c2]] /NO_COPY
    response.Add, r[bound_indices[c1]], r[bound_indices[c2]] /NO_COPY
    c1 = c1 + 1L
    c2 = c2 + 1L
  ENDFOR  

END
;+
;
; NAME:
;       oSRF::Load_hamsr
;
; PURPOSE:
;       This procedure loads oSRF objects with hamsr SRF data.
;
; CALLING SEQUENCE:
;       obj->[oSRF::]Load_hamsr, $
;         Sensor_Id        , $ ; Input
;         Channel          , $ ; Input
;         n_bands          , $ ; Input
;         Path    = Path   , $ ; Input keyword.
;         Debug   = Debug  , $ ; Input keyword.
;         History = HISTORY    ; Output keyword.
;
; INPUTS:
;       Sensor_Id:   The sensor id of the sensor for which SRF data is to be
;                    loaded. This id is used to construct the filename containing
;                    SRF data.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
;       Channel:     The sensor channel number for which SRF data is to be
;                    loaded. This value is used to construct the filename
;                    containing SRF data.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
;       n_bands:     The number of bands for the channel being processed
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Path:        Set this keyword to the directory path of the input files.
;                    If not specified, the default is the current directory, "./"
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       History:     Specify this keyword to return the version id of the loader
;                    procedure. This is used in setting the history global attribute
;                    in the SRF netCDF datafile.
;                    UNITS:      N/A
;                    TYPE:       CHARACTER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(OUT), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; CREATION HISTORY:
;       Written by:     Joe Bloggs, DD-Mon-YYYY
;                       joe.bloggs@domain
;
;-

PRO oSRF::Load_hamsr, $
  Sensor_Id        , $ ; Input
  Channel          , $ ; Input
  n_bands          , $ ; Input
  Path    = Path   , $ ; Input keyword. If not specified, default is "./"
  Debug   = Debug  , $ ; Input keyword. Passed onto all oSRF methods
  History = HISTORY    ; Output keyword of version id.


  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check keywords
  Check_Threshold = N_ELEMENTS(Response_Threshold) GT 0 ? TRUE : FALSE
  Path            = Valid_String(Path) ? Path : "./"

  ; Parameters
  HISTORY = "$Id$"

  ; Construct file name
  ch = STRING(Channel,FORMAT='(i2.2)')
  filename = Path+PATH_SEP()+Sensor_Id+'-'+ch+'.inp'
  ; ...Check it exists
  fInfo = FILE_INFO(filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, 'Datafile '+filename+' not found....', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Read the file
  Read_hamsr_Raw_SRF, filename, channel, n_bands, n_points, frequency, response

  ; Load the SRF data into the oSRF object
  ; ...Allocate
  self->Allocate, n_points, Debug=Debug
  ; ...Clear flags
  self->Clear_Flag, $
    Debug=Debug, $
    /All
  ; ...Set the data values
  n_bands = N_ELEMENTS(n_points)
  FOR i = 0, n_bands-1 DO BEGIN
    band = i+1
    self->Set_Property, $
      band, $
      Debug=Debug, $
      Sensor_Id = Sensor_Id, $
      Channel   = Channel, $
      Frequency = frequency[i], $
      Response  = response[i]
  ENDFOR

END

