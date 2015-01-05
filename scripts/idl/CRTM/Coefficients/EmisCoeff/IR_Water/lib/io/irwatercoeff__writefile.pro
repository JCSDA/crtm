;+
; NAME:
;       IRwaterCoeff::WriteFile
;
; PURPOSE:
;       The IRwaterCoeff::WriteFile procedure method writes an IRwaterCoeff object
;       to a data file.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]WriteFile, $
;         Filename         , $
;         Swap    = swap   , $
;         Quiet   = quiet  , $
;         Title   = title  , $
;         History = history, $
;         Comment = comment, $
;         Debug   = debug
;
; INPUTS:
;       Filename:       The name of the IRwaterCoeff object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       Swap:           Set this keyword parameter to byte swap data written
;                       to the file if it opened by this procedure.
;                       If NOT SET => data subsequently written to a file is written
;                                     in the native platform endian format. (DEFAULT)
;                          SET     => data subsequently written to a file is byte
;                                     swapped compared to the native platform byte
;                                     endian format.
;                       This keyword is ignored if the file is already open for write access.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Quiet:          Set this keyword to disable informational output
;                       If NOT SET => Record information is output. (DEFAULT)
;                          SET     => Record information is NOT output.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Title:          Character string containing a succinct description
;                       of what is in the dataset.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       History:        Character string containing dataset creation
;                       history.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Comment:        Character string containing any comments about
;                       the dataset.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
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
;-

PRO IRwaterCoeff_List::WriteFile, $
  filename         , $  ; Input
  Swap    = swap   , $  ; Optional input
  Quiet   = quiet  , $  ; Optional input
  Title   = title  , $  ; Optional input
  History = history, $  ; Optional input
  Comment = comment, $  ; Optional input      
  Debug   = debug       ; Optional input

  ; Set up
  MODULE_VERSION_ID = '$Id$'
  @irwatercoeff_parameters
  @irwatercoeff_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)
  

  ; Process input
  ; ...If no data, do nothing
  IF ( ~ self->Associated() ) THEN RETURN
  ; ...Check filename
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  
  ; Open the file
  fid = Open_Binary_File(filename, /Write, Swap = swap, Debug = debug)
  IF ( fid < 0 ) THEN $
    MESSAGE, 'Error opening file '+filename, $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Write the release and version
  self->Get_Property, $
    Release = release, $
    Version = version, $
    Debug = debug
  WRITEU, fid, release, version


  ; Write the dimensions
  self->Get_Property, $
    n_Angles      = n_angles     , $
    n_Frequencies = n_frequencies, $
    n_Wind_Speeds = n_wind_speeds, $
    Debug = debug
  WRITEU, fid, n_angles     , $
               n_frequencies, $
               n_wind_speeds


  ; Write the global attributes
  WriteGAtts_Binary_File, $
    fid, $
    Write_Module = MODULE_VERSION_ID, $
    Title        = title  , $
    History      = history, $
    Comment      = comment
    

  ; Write the coefficient data
  ; ...Write the dimensional vectors
  self->Get_Property, $
    Angle      = angle     , $
    Frequency  = frequency , $
    Wind_Speed = wind_speed, $
    Debug = debug
  WRITEU, fid, angle     , $
               frequency , $
               wind_speed
  ; ...Write the emissivity data
  self->Get_Property, $
    Emissivity = emissivity, $
    Debug = debug
  WRITEU, fid, emissivity


  ; Close the file
  FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'FILE: '+STRTRIM(filename,2)+'; '+self->Info(Debug=debug), /INFORMATIONAL

END
