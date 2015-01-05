;+
; NAME:
;       IRwaterCoeff::ReadFile
;
; PURPOSE:
;       The IRwaterCoeff::ReadFile procedure method reads an IRwaterCoeff object
;       from a data file.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]ReadFile, $
;         Filename         , $
;         Quiet   = quiet  , $
;         Debug   = debug  , $
;         Title   = title  , $
;         History = history, $
;         Comment = comment
;
; INPUTS:
;       Filename:       The name of the IRwaterCoeff object data file to read.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Quiet:          Set this keyword to disable informational output
;                       If NOT SET => Record information is output. (DEFAULT)
;                          SET     => Record information is NOT output.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:          Set this keyword for debugging.
;                       If NOT SET => Error handler is enabled. (DEFAULT)
;                          SET     => Error handler is disabled; Routine
;                                     traceback output is enabled.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
; OUTPUT KEYWORDS:
;       Title:          Character string containing a succinct description
;                       of what is in the dataset.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       History:        Character string containing dataset creation
;                       history.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Comment:        Character string containing any comments about
;                       the dataset.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;-

PRO IRwaterCoeff::ReadFile, $
  filename         , $  ; Input
  Quiet   = quiet  , $  ; Optional input
  Debug   = debug  , $  ; Optional input
  Title   = title  , $  ; Optional output
  History = history, $  ; Optional output
  Comment = comment     ; Optional output      

  ; Set up
  @irwatercoeff_parameters
  @irwatercoeff_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)


  ; Process input
  ; ...Destroy the object
  self->Destroy, Debug = debug
  ; ...Check filename
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Open the file
  fid = Open_Binary_File(filename, Debug = debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the release and version
  release = 0L
  version = 0L
  READU, fid, release, version


  ; Read the dimensions
  n_angles      = 0L
  n_frequencies = 0L
  n_wind_speeds = 0L
  READU, fid, n_angles     , $
              n_frequencies, $
              n_wind_speeds
  ; ...Create the object
  self->Create, n_angles     , $
                n_frequencies, $
                n_wind_speeds, $
                Debug = debug
  IF ( ~ self->Associated(Debug = debug) ) THEN $
    MESSAGE, 'IRwaterCoeff object allocation failed.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Explicitly assign the version number
  self->Set_Property, Version = version, $
                      Debug   = debug


  ; Read the global attributes
  ReadGAtts_Binary_File, $
    fid, $
    Title   = title  , $
    History = history, $
    Comment = comment


  ; Read the coefficient data
  ; ...Read the dimensional vectors
  self->Get_Property, $
    Angle      = angle     , $
    Frequency  = frequency , $
    Wind_Speed = wind_speed, $
    /Remove, $
    Debug = debug
  READU, fid, angle     , $
              frequency , $
              wind_speed
  self->Set_Property, $
    Angle      = angle     , $
    Frequency  = frequency , $
    Wind_Speed = wind_speed, $
    Debug = debug
  ; ...Read the emissivity data
  self->Get_Property, $
    Emissivity = emissivity, $
    /Remove, $
    Debug = debug
  READU, fid, emissivity
  self->Set_Property, $
    Emissivity = emissivity, $
    Debug = debug


  ; Close the file
  FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'FILE: '+STRTRIM(filename,2)+'; '+self->Info(Debug=debug), /INFORMATIONAL

END
