;+
; NAME:
;       Cloud::Write_Record
;
; PURPOSE:
;       The Cloud::Write_Record procedure method writes a cloud record to file.
;
;       NOTE: This is considered a PRIVATE method not to be called outside
;             the context of writing a cloud, or atmosphere, datafile.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Write_Record, $
;         FileId       , $  ; Input
;         Quiet = Quiet     ; Input keyword
;         Debug = Debug     ; Input keyword
;
; INPUTS:
;       FileId:         The unit number for the already opened file to which
;                       the cloud record is to be written.
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

PRO Cloud::Write_Record, $
  FileId       , $  ; Input
  Quiet = Quiet, $  ; Optional input
  Debug = Debug     ; Optional input
  

  ; Set up
  COMPILE_OPT HIDDEN
  @cloud_parameters
  @cloud_pro_err_handler
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(Quiet))


  ; Retrieve the cloud object data
  self->Get_Property, Debug = Debug, $
    n_Layers           = n_layers          , $
    Type               = type              , $
    Effective_Radius   = effective_radius  , $
    Effective_Variance = effective_variance, $
    Water_Content      = water_content       


  ; Write to file
  WRITEU, FileID, n_layers
  WRITEU, FileId, type, $
                  effective_radius, $
                  effective_variance, $
                  water_content
  
  
  ; Output info message
  IF ( Noisy ) THEN BEGIN
    MESSAGE, '  Cloud record n_Layers='+STRTRIM(n_layers,2)+$
             '; Type='+CLOUD_TYPE_NAME[type], $
             /INFORMATIONAL
  ENDIF
  
END
