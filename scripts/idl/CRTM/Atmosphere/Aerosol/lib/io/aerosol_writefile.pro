;+
; NAME:
;       Aerosol_WriteFile
;
; PURPOSE:
;       The Aerosol_WriteFile procedure writes a list of aerosol objects
;       to a data file.
;
; CALLING SEQUENCE:
;       Aerosol_WriteFile, $
;         Filename       , $
;         Aerosols       , $
;         FileId = FileId, $
;         Swap   = Swap  , $
;         Quiet  = Quiet , $
;         Debug  = Debug   )
;
; INPUTS:
;       Filename:       The name of the Aerosol object data file to write.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
;       Aerosols:         List containing the Aerosol objects to write to file.
;                       UNITS:      N/A
;                       TYPE:       LIST
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

PRO Aerosol_WriteFile, $
  Filename       , $  ; Input
  Aerosols       , $  ; Input
  FileId = FileId, $  ; Optional input
  Swap   = Swap  , $  ; Optional input
  Quiet  = Quiet , $  ; Optional input
  Debug  = Debug      ; Optional input

  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))
  

  ; Process input
  ; ...Check filename
  IF ( NOT Valid_String(Filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check aerosol list
  IF ( ~ OBJ_VALID(Aerosols) ) THEN $
    MESSAGE, 'Aerosols input is not a valid object', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( ~ OBJ_ISA(Aerosols,'LIST') ) THEN $
    MESSAGE, 'Aerosols input is not a LIST object', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  

  ; If no aerosols, do nothing
  IF ( Aerosols.IsEmpty() ) THEN RETURN
  
  
  ; Check if file needs to be opened
  open_file = TRUE
  IF ( N_ELEMENTS(FileId) GT 0 ) THEN BEGIN
    fid   = FileId[0]
    finfo = FSTAT(fid)
    IF ( (finfo.NAME EQ Filename) AND finfo.OPEN AND finfo.WRITE ) THEN open_file = FALSE
  ENDIF


  ; Open the file if necessary
  IF ( open_file ) THEN BEGIN
    fid = Open_Binary_File(Filename, /Write, Swap=Swap, Debug=Debug)
    IF ( fid < 0 ) THEN $
      MESSAGE, 'Error opening file '+Filename, $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF


  ; Write the file dimensions
  n_aerosols = LONG(Aerosols.Count())
  WRITEU, fid, n_aerosols


  ; Loop over the number of aerosols
  FOR nc = 0L, n_aerosols-1L DO BEGIN

    ; Check that current list element is actually an aerosol object
    aerosol = Aerosols[nc]
    IF ( ~ OBJ_VALID(aerosol) ) THEN $
      MESSAGE, 'List element '+STRTRIM(nc,2)+' is not a valid object', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    IF ( ~ OBJ_ISA(aerosol,'AEROSOL') ) THEN $
      MESSAGE, 'List element '+STRTRIM(nc,2)+' is not a AEROSOL object', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    

    ; Write the current aerosol data dimensions
    aerosol->Aerosol::Get_Property, $
      n_Layers=n_layers, $
      Debug=Debug
    WRITEU, fid, n_layers


    ; Write the aerosol data
    aerosol->Aerosol::Get_Property, $
      Type              =type              , $
      Effective_Radius  =effective_radius  , $
      concentration     =concentration     , $
      Debug=Debug
    WRITEU, fid, type, $
                 effective_radius, $
                 concentration

  ENDFOR


  ; Close the file if necessary
  IF ( open_file ) THEN FREE_LUN, fid


  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of aerosols written to '+Filename+' : '+STRTRIM(nc,2), /INFORMATIONAL

END
