;+
; NAME:
;       Aerosol_ReadFile
;
; PURPOSE:
;       The Aerosol_ReadFile procedure reads aerosol object data files
;       returning a list object of the Aerosol data in the file
;
; CALLING SEQUENCE:
;       Aerosol_ReadFile, $
;         Filename       , $
;         Aerosols       , $
;         FileId = FileId, $
;         Quiet  = Quiet , $
;         Debug  = Debug   )
;
; INPUTS:
;       Filename:       The name of the Aerosol object data file to read.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; OUTPUTS:
;       Aerosols:         List containing the Aerosol objects read from file.
;                       UNITS:      N/A
;                       TYPE:       LIST
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORDS:
;       FileId:         The unit number for the Aerosol object data file
;                       to be read.
;                       - If this keyword is passed in AND corresponds to the
;                         filename AND is already opened for read access, then
;                         it is assumed the file is an Atmosphere object file
;                         of which the Aerosol object data is a component.
;                       - For all other cases, it is assumed the file must be
;                         opened and is a standalone Aerosol object file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
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
;
; OUTPUT KEYWORDS:
;       Count:          The number of aerosol objects read from file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO Aerosol_ReadFile, $
  Filename       , $  ; Input
  Aerosols       , $  ; Output
  FileId = FileId, $  ; Optional input
  Quiet  = Quiet , $  ; Optional input
  Debug  = Debug , $  ; Optional input
  Count  = Count      ; Optional output

  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))
  Count = 0L


  ; Process input
  IF ( NOT Valid_String(Filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check if file needs to be opened
  open_file = TRUE
  IF ( N_ELEMENTS(FileId) GT 0 ) THEN BEGIN
    fid   = FileId[0]
    finfo = FSTAT(fid)
    IF ( (finfo.NAME EQ Filename) AND finfo.OPEN AND finfo.READ ) THEN open_file = FALSE
  ENDIF


  ; Open the file if necessary
  IF ( open_file ) THEN BEGIN
    fid = Open_Binary_File(Filename, Debug = Debug)
    IF ( fid < 0 ) THEN $
      MESSAGE, 'Error opening file '+Filename, $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF


  ; Read the file dimensions
  n_aerosols = 0L
  READU, fid, n_aerosols


  ; Create the aerosol list
  Aerosols = LIST()
  
  
  ; Loop over the number of aerosols
  FOR nc = 0L, n_aerosols-1L DO BEGIN


    ; Create the current Aerosol object
    aerosol = OBJ_NEW('Aerosol', Debug=Debug)


    ; Read the current aerosol data dimensions
    n_layers = 0L
    READU, fid, n_layers


    ; Create the structure
    aerosol->Create, n_layers, Debug = Debug
    IF ( ~aerosol->Associated(Debug = Debug) ) THEN $
      MESSAGE, 'Aerosol object allocation failed.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch


    ; Read the aerosol data
    type               = 0L
    effective_radius   = DBLARR(n_layers)
    concentration      = DBLARR(n_layers)
    READU, fid, type, $
                effective_radius, $
                concentration


    ; Load the aerosol data into the object
    aerosol->Set_Property, Debug = Debug, $
      Type               = type              , $
      Effective_Radius   = effective_radius  , $
      concentration      = concentration


    ; Add the current aerosol to the file object
    Aerosols.Add, aerosol

  ENDFOR


  ; Close the file if necessary
  IF ( open_file ) THEN FREE_LUN, fid


  ; Set the output keywords
  Count = Aerosols.Count()
  

  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of aerosols read from '+Filename+' : '+STRTRIM(Count,2), /INFORMATIONAL

END
