;+
; NAME:
;       Cloud_ReadFile
;
; PURPOSE:
;       The Cloud_ReadFile procedure reads cloud object data files
;       returning a list object of the Cloud data in the file
;
; CALLING SEQUENCE:
;       Cloud_ReadFile, $
;         Filename       , $
;         Clouds         , $
;         FileId = FileId, $
;         Quiet  = Quiet , $
;         Debug  = Debug   )
;
; INPUTS:
;       Filename:       The name of the Cloud object data file to read.
;                       UNITS:      N/A
;                       TYPE:       CHARACTER(*)
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
; OUTPUTS:
;       Clouds:         List containing the Cloud objects read from file.
;                       UNITS:      N/A
;                       TYPE:       LIST
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT)
;
; INPUT KEYWORDS:
;       FileId:         The unit number for the Cloud object data file
;                       to be read.
;                       - If this keyword is passed in AND corresponds to the
;                         filename AND is already opened for read access, then
;                         it is assumed the file is an Atmosphere object file
;                         of which the Cloud object data is a component.
;                       - For all other cases, it is assumed the file must be
;                         opened and is a standalone Cloud object file.
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
;       Count:          The number of cloud objects read from file.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(OUT), OPTIONAL
;-

PRO Cloud_ReadFile, $
  Filename       , $  ; Input
  Clouds         , $  ; Output
  FileId = FileId, $  ; Optional input
  Quiet  = Quiet , $  ; Optional input
  Debug  = Debug , $  ; Optional input
  Count  = Count      ; Optional output

  ; Set up
  @cloud_parameters
  @cloud_pro_err_handler
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
  n_clouds = 0L
  READU, fid, n_clouds


  ; Create the cloud list
  Clouds = LIST()
  
  
  ; Loop over the number of clouds
  FOR nc = 0L, n_clouds-1L DO BEGIN


    ; Create the current Cloud object
    cloud = OBJ_NEW('Cloud', Debug=Debug)


    ; Read the current cloud data dimensions
    n_layers = 0L
    READU, fid, n_layers


    ; Create the structure
    cloud->Create, n_layers, Debug = Debug
    IF ( ~cloud->Associated(Debug = Debug) ) THEN $
      MESSAGE, 'Cloud object allocation failed.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch


    ; Read the cloud data
    type               = 0L
    effective_radius   = DBLARR(n_layers)
    effective_variance = DBLARR(n_layers)
    water_content      = DBLARR(n_layers)
    READU, fid, type, $
                effective_radius, $
                effective_variance, $
                water_content


    ; Load the cloud data into the object
    cloud->Set_Property, Debug = Debug, $
      Type               = type              , $
      Effective_Radius   = effective_radius  , $
      Effective_Variance = effective_variance, $
      Water_Content      = water_content


    ; Add the current cloud to the file object
    Clouds.Add, cloud

  ENDFOR


  ; Close the file if necessary
  IF ( open_file ) THEN FREE_LUN, fid


  ; Set the output keywords
  Count = Clouds.Count()
  

  ; Output an info message
  IF ( noisy ) THEN $
    MESSAGE, 'Number of clouds read from '+Filename+' : '+STRTRIM(Count,2), /INFORMATIONAL

END
