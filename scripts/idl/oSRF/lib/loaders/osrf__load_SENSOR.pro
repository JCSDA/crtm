;+
; ***TEMPLATE***
;
; oSRF method to load SRF data for a specific <sensor> channel
; and detector
;
PRO oSRF__Load_<sensor>, $
  Sensor_Id, $ ; Input
  Channel  , $ ; Input
  Path    = Path   , $ ; Input keyword. If not specified, default is "./"
  Debug   = Debug  , $ ; Input keyword. Passed onto all oSRF methods
  History = HISTORY    ; Output keyword of version id.
;-

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
  <This will be specific to each file>
  <Return data in x[0:1,*] array>
  <Return n_Points dimension>


  ; Assign frequency and response
  ; to individual arrays
  f = REFORM(x[0,*])
  r = REFORM(x[1,*])
  ; ...Only keep the unique values
  idx = UNIQ(f, SORT(f))
  IF ( N_ELEMENTS(idx) NE n_Points ) THEN $
    MESSAGE, 'Non-Unique frequencies found', /INFORMATIONAL
  f = f[idx]
  r = r[idx]


  ; Load the SRF data into the oSRF object
  ; ...Allocate
  self->Allocate, N_ELEMENTS(f), Debug=Debug
  ; ...Clear flags
  self->Clear_Flag, $
    Debug=Debug, $
    /All
  ; ...Set the data values
  self->Set_Property, $
    Channel = Channel, $
    Frequency = f, $
    Response  = r

END

