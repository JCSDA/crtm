;+
; NAME:
;       Cloud::Read_Record
;
; PURPOSE:
;       The Cloud::Read_Record procedure method reads a cloud record from file.
;
;       NOTE: This is considered a PRIVATE method not to be called outside
;             the context of reading a cloud, or atmosphere, datafile.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Read_Record, $
;         FileId       , $  ; Input
;         Quiet = Quiet     ; Input keyword
;         Debug = Debug     ; Input keyword
;
; INPUTS:
;       FileId:         The unit number for the already opened file from which
;                       the cloud record is to be read.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
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
; INCLUDE FILES:
;       cloud_parameters: Include file for cloud specific parameters.
;
;       cloud_pro_err_handler: Include file for error handling.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 30-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Cloud::Read_Record, $
  FileId       , $  ; Input
  Quiet = Quiet, $  ; Optional input
  Debug = Debug     ; Optional input
  

  ; Set up
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))


  ; Read the data dimensions and check
  self->Get_Property, n_Layers = n_layers, Debug = Debug
  n_cloud_layers = n_layers
  READU, FileID, n_cloud_layers
  IF ( n_Layers NE n_cloud_layers ) THEN $
    MESSAGE, 'Cloud record data dimensions, ' + $
             STRTRIM(n_cloud_layers,2)+ $
             ', are inconsistent with object definition, ' + $
             STRTRIM(n_layers,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the cloud data
  type               = 0L
  effective_radius   = DBLARR(n_layers)  
  effective_variance = DBLARR(n_layers)  
  water_content      = DBLARR(n_layers)  
  READU, FileId, type, $
                 effective_radius, $
                 effective_variance, $
                 water_content
  
  
  ; Load the cloud data into the object
  self->Set_Property, Debug = Debug, $
    Type               = type              , $
    Effective_Radius   = effective_radius  , $
    Effective_Variance = effective_variance, $
    Water_Content      = water_content       
    
  
  ; Output info message
  IF ( Noisy ) THEN BEGIN
    MESSAGE, '  Cloud record n_Layers='+STRTRIM(n_layers,2)+$
             '; Type='+CLOUD_TYPE_NAME[type], $
             /INFORMATIONAL
  ENDIF
  
END
