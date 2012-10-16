;+
; NAME:
;       Aerosol_List::WriteFile
;
; PURPOSE:
;       The Aerosol_List::WriteFile procedure method writes a list of Aerosol objects
;       to a data file.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol_List::]WriteFile, $
;         Filename       , $
;         FileId = FileId, $
;         Swap   = Swap  , $
;         Quiet  = Quiet , $
;         Debug  = Debug
;
; INPUTS:
;       Filename:       The name of the Aerosol object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORDS:
;       FileId:         The unit number for the Aerosol object data file
;                       to write.
;                       - If this keyword is passed in AND corresponds to the
;                         filename AND is already opened for write access, then
;                         it is assumed the file is an Atmosphere object file
;                         of which the Aerosol object data is a component.
;                       - For all other cases, it is assumed the file must be
;                         opened and is a standalone Aerosol object file.
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

PRO Aerosol_List::WriteFile, $
  filename       , $  ; Input
  FileId = fileid, $  ; Optional input
  Swap   = swap  , $  ; Optional input
  Quiet  = quiet , $  ; Optional input
  Debug  = debug      ; Optional input

  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet)) || KEYWORD_SET(debug)
  

  ; Process input
  ; ...If no aerosols, do nothing
  IF ( self.IsEmpty() ) THEN RETURN
  ; ...Check aerosol list
  IF ( ~ self->HasOnly_Aerosols(Debug = debug) ) THEN $
    MESSAGE, 'Aerosol list contains non-aerosol objects!', $
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
  n_aerosols = LONG(self.Count())
  WRITEU, fid, n_aerosols


  ; Loop over the number of aerosols
  FOR na = 0L, n_aerosols-1L DO BEGIN

    ; Write the current aerosol data dimensions
    self[na]->Aerosol::Get_Property, $
      n_Layers = n_layers, $
      Debug = debug
    WRITEU, fid, n_layers


    ; Write the aerosol data
    self[na]->Aerosol::Get_Property, $
      Type             = type              , $
      Effective_Radius = effective_radius  , $
      concentration    = concentration     , $
      Debug=Debug
    WRITEU, fid, $
      type, $
      effective_radius, $
      concentration

  ENDFOR


  ; Close the file if necessary
  IF ( open_file ) THEN FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of aerosols written to '+filename+' : '+STRTRIM(n_aerosols,2), /INFORMATIONAL

END
