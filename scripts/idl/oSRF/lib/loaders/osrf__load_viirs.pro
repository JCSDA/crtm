;
; NAME:
;       Read_VIIRS_Raw_SRF
;
; PURPOSE:
;       This (private) helper procedure reads individual channel data
;       files and loads the VIIRS SRF data into arrays and lists.
;
; CALLING SEQUENCE:
;       Read_VIIRS_Raw_SRF, $
;         Filename     , $  ; Input
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

PRO Read_VIIRS_Raw_SRF, $
  Filename     , $  ; Input
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


  ; Specific sensor file parameters
  HDR_ID       = '#' ; Header comment indicator
  IDX_SPECTRAL = 3   ; Index of wavelength in input array
  IDX_RESPONSE = 4   ; Index of response in input array

  
  ; Read the file(s) into a data buffer
  ; (need to do this since the band name contains characters)
  n_lines = FILE_LINES(Filename)
  data    = STRARR(n_lines)
  OPENR, fid, Filename, /GET_LUN
  READF, fid, data
  FREE_LUN, fid   


  ; Separate the header comments and data
  hdr_flag = STRMID(data,0,1)
  hdr_loc = WHERE( hdr_flag EQ HDR_ID, $
                   hdr_count, $
                   COMPLEMENT=data_loc, $
                   NCOMPLEMENT=data_count )
  IF ( hdr_count EQ 0 OR data_count EQ 0 ) THEN $
    MESSAGE, 'Header and data counts are zero!'
  hdr  = data[hdr_loc]
  data = TEMPORARY(data[data_loc])


  ; Extract the data from the string array
  wavelength = DBLARR(data_count)
  r          = DBLARR(data_count)
  FOR i = 0L, data_count-1L DO BEGIN
    elements = STRSPLIT(data[i], /EXTRACT)
    wavelength[i] = DOUBLE(elements[IDX_SPECTRAL])
    r[i]          = DOUBLE(elements[IDX_RESPONSE])
  ENDFOR


  ; Units conversions
  ; ...Convert wavelength from nm to um
  wavelength = wavelength / 1000.0d0
  ; ...Convert wavelength in um to frequency in cm^-1
  f = 10000.0d0/wavelength 


  ; Only keep the unique values
  idx = UNIQ(f, SORT(f))
  f = f[idx]
  r = r[idx]


  ; Assign data to return argument
  n_points = N_ELEMENTS(f)
  frequency.Add, f, /NO_COPY
  response.Add,  r, /NO_COPY
  
END


;+
;
; NAME:
;       oSRF::Load_VIIRS
;
; PURPOSE:
;       This procedure loads oSRF objects with VIIRS SRF data.
;
; CALLING SEQUENCE:
;       obj->[oSRF::]Load_VIIRS, $
;         Sensor_Id        , $ ; Input
;         Channel          , $ ; Input
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
;       Written by:     Paul van Delst, 06-Dec.-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO oSRF::Load_VIIRS, $
  Sensor_Id        , $ ; Input
  Channel          , $ ; Input
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
  Read_VIIRS_Raw_SRF, filename, n_points, frequency, response


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

