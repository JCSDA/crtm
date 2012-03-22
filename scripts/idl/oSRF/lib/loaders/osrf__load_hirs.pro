;
; NAME:
;       Read_hirs_Raw_SRF
;
; PURPOSE:
;       This (private) helper procedure reads individual channel data
;       files and loads the hirs SRF data into arrays and lists.
;
; CALLING SEQUENCE:
;       Read_hirs_Raw_SRF, $
;         Filename     , $  ; Input
;         Platform     , $  ; Input
;         Channel      , $  ; Input
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
; Frequency_Shift:   Array of frequency shifts to be applied.
;                    UNITS:      N/A
;                    TYPE:       LIST
;                    DIMENSION:  rank-1
;                    ATTRIBUTES: INTENT(OUT)
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
; 
; INPUT KEYWORD PARAMETERS:
;
;     F_Shift:       Set this keyword to apply a frequency
;                    shift to data read from file
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
; INCLUDE FILES:
;       osrf_pro_err_handler: Error handler code for oSRF procedures.
;
;

PRO Read_hirs_Raw_SRF, $
  Filename       , $  ; Input
  Platform       , $  ; Input
  Channel        , $  ; Input
  n_points       , $  ; Output
  frequency      , $  ; Output
  response       , $  ; Output
  F_Shift=F_Shift, $  ; Input
  Debug = debug     ; Input keyword

  COMPILE_OPT HIDDEN

  ; Set up
  @osrf_pro_err_handler
  ; ...Lists for the SRF data
  frequency = LIST()
  response  = LIST()
  
  ; Read the data channel by channel
  OPENR, FileID, Filename, /GET_LUN
  WHILE NOT EOF(FileID) DO BEGIN

    ; Read a chunk of data for current channeld
    READF, FileID, Channel_read, n_points    
    Data = DBLARR(5,n_points)
    READF, FileID, Data
    
    IF ( Channel_read NE Channel ) THEN CONTINUE

    ; Split out data in ascending frequency order
    Freq = REFORM(Data[0,*])
    FilterTransmittance = REFORM(Data[1,*])
    Response_NoFilter   = REFORM(Data[2,*])
    Resp                = REFORM(Data[3,*])
    NormalisedResponse  = REFORM(Data[4,*])

  ENDWHILE
  FREE_LUN, FileID
  
  ; Correct for ch 9
  ; problem where necessary
  IF ( Channel EQ 9L ) THEN BEGIN
    Loc = WHERE( Freq GE 1000.0d0 )
    CASE Platform OF
      'n15'     : Freq[Loc] = Freq[Loc] -  0.03d0
      'n18'     : Freq[Loc] = Freq[Loc] +  0.03d0
      'metop-a' : Freq[Loc] = Freq[Loc] +  0.03d0
      ELSE      : Freq[Loc] = Freq[Loc]
    ENDCASE
  ENDIF
  
  ; Apply frequency shift
  ; where necessary
  IF ( KEYWORD_SET(F_Shift) ) THEN BEGIN
    Freq = Freq + F_Shift
  ENDIF  

  ; Sort and only keep
  ; the unique values
  idx = UNIQ(Freq, SORT(Freq))
  f = Freq[idx]
  r = Resp[idx]
  
  frequency.Add, f, /NO_COPY  
  response.Add , r, /NO_COPY  

END

;+
;
; NAME:
;       oSRF::Load_hirs
;
; PURPOSE:
;       This procedure loads oSRF objects with hirs SRF data.
;
; CALLING SEQUENCE:
;       obj->[oSRF::]Load_hirs, $
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
;    F_Shift:        When applied this keyword applies a frequency shift 
;                    UNITS:      N/A
;                    TYPE:       DOUBLE
;                    DIMENSION:  Rank-1
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
PRO oSRF::Load_hirs, $
  Sensor_Id                , $ ; Input
  Channel                  , $ ; Input
  F_Shift = F_Shift        , $ ; Input. Shifts SRF data
  Path    = Path           , $ ; Input keyword. If not specified, default is "./"
  Debug   = Debug          , $ ; Input keyword. Passed onto all oSRF methods
  History = HISTORY            ; Output keyword of version id.

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

  ; Extract platform from sensor id
  sensor_id_array = STRSPLIT(Sensor_Id,'_',/EXTRACT)
  platform = sensor_id_array[1]
  
  ; Construct file name
  ch = STRING(Channel,FORMAT='(i2.2)')
  filename = Path+PATH_SEP()+Sensor_Id+'.inp'
  ; ...Check it exists
  fInfo = FILE_INFO(filename)
  IF ( NOT fInfo.EXISTS ) THEN $
    MESSAGE, 'Datafile '+filename+' not found....', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Read the file
  IF ( KEYWORD_SET(F_Shift) ) THEN BEGIN
    Read_hirs_Raw_SRF, filename, $
                       platform, $
                        Channel, $
                       n_points, $
                      frequency, $
                       response, $
                     F_Shift=F_Shift
  ENDIF ELSE BEGIN
    Read_hirs_Raw_SRF, filename, $
                       platform, $
                        Channel, $
                       n_points, $
                      frequency, $
                       response
  ENDELSE
                     
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

