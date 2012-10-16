;+
; NAME:
;       Cloud_List::WriteFile
;
; PURPOSE:
;       The Cloud_List::WriteFile procedure method writes a list of cloud objects
;       to a data file.
;
; CALLING SEQUENCE:
;       Obj->[Cloud_List::]WriteFile, $
;         Filename       , $
;         FileId = FileId, $
;         Swap   = Swap  , $
;         Quiet  = Quiet , $
;         Debug  = Debug
;
; INPUTS:
;       Filename:       The name of the Cloud object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       FileId:         The unit number for the Cloud object data file
;                       to write.
;                       - If this keyword is passed in AND corresponds to the
;                         filename AND is already opened for write access, then
;                         it is assumed the file is an Atmosphere object file
;                         of which the Cloud object data is a component.
;                       - For all other cases, it is assumed the file must be
;                         opened and is a standalone Cloud object file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
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
;       Debug:          Set this keyword for debugging.
;                       If NOT SET => Error handler is enabled. (DEFAULT)
;                          SET     => Error handler is disabled; Routine
;                                     traceback output is enabled.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO Cloud_List::WriteFile, $
  filename       , $  ; Input
  FileId = fileid, $  ; Optional input
  Swap   = swap  , $  ; Optional input
  Quiet  = quiet , $  ; Optional input
  Debug  = debug      ; Optional input

  ; Set up
  @cloud_parameters
  @cloud_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)
  

  ; Process input
  ; ...If no clouds, do nothing
  IF ( self.IsEmpty() ) THEN RETURN
  ; ...Check cloud list
  IF ( ~ self->HasOnly_Clouds(Debug = debug) ) THEN $
    MESSAGE, 'Cloud list contains non-cloud objects!', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check filename
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  
  ; Check if file needs to be opened
  open_file = TRUE
  IF ( N_ELEMENTS(fileid) GT 0 ) THEN BEGIN
    fid   = fileid[0]
    finfo = FSTAT(fid)
    IF ( (finfo.NAME EQ filename) && finfo.OPEN && finfo.WRITE ) THEN open_file = FALSE
  ENDIF


  ; Open the file if necessary
  IF ( open_file ) THEN BEGIN
    fid = Open_Binary_File(filename, /Write, Swap = swap, Debug = debug)
    IF ( fid < 0 ) THEN $
      MESSAGE, 'Error opening file '+filename, $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF


  ; Write the file dimensions
  n_clouds = LONG(self.Count())
  WRITEU, fid, n_clouds


  ; Loop over the number of clouds
  FOR nc = 0L, n_clouds-1L DO BEGIN


    ; Write the current cloud data dimensions
    self[nc]->Cloud::Get_Property, $
      n_Layers = n_layers, $
      Debug = debug
    WRITEU, fid, n_layers


    ; Write the cloud data
    self[nc]->Cloud::Get_Property, $
      Type               = type              , $
      Effective_Radius   = effective_radius  , $
      Effective_Variance = effective_variance, $
      Water_Content      = water_content     , $
      Debug = debug
    WRITEU, fid, $
      type, $
      effective_radius, $
      effective_variance, $
      water_content

  ENDFOR


  ; Close the file if necessary
  IF ( open_file ) THEN FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of clouds written to '+filename+' : '+STRTRIM(n_clouds,2), /INFORMATIONAL

END
