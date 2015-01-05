;+
; NAME:
;       IRwaterCoeff::Create
;
; PURPOSE:
;       The IRwaterCoeff::Create procedure method creates an allocated
;       instance of a IRwaterCoeff object. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[IRwaterCoeff::]Create, $
;         n_Angles     , $
;         n_Frequencies, $
;         n_Wind_Speeds, $
;         Debug = Debug   
;
; INPUTS:
;       n_Angles:       Number of angles dimension.
;                       Must be > 0.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
;       n_Frequencies:  Number of frequencies dimension.
;                       Must be > 0.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;
;       n_Wind_Speeds:  Number of wind speeds dimension.
;                       Must be > 0.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN)
;                  
; INPUT KEYWORDS:
;       Debug:          Set this keyword for debugging.
;                       If NOT SET => Error handler is enabled. (DEFAULT)
;                          SET     => Error handler is disabled; Routine
;                                     traceback output is enabled.
;                       UNITS:      N/A
;                       TYPE:       INTEGER
;                       DIMENSION:  Scalar
;                       ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO IRwaterCoeff::Create, $
  n_Angles     , $ ; Input
  n_Frequencies, $ ; Input
  n_Wind_Speeds, $ ; Input
  Debug = Debug    ; Input keyword

  ; Set up
  @irwatercoeff_pro_err_handler
 
 
  ; Check input
  IF ( n_Angles LT 1 ) THEN $
    MESSAGE, 'n_Angles dimension must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( n_Frequencies LT 1 ) THEN $
    MESSAGE, 'n_Frequencies dimension must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( n_Wind_Speeds LT 1 ) THEN $
    MESSAGE, 'n_Wind_Speeds dimension must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
   

  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Create the empty arrays
  (self.Angle).Add,      DBLARR(n_Angles)
  (self.Frequency).Add,  DBLARR(n_Frequencies)
  (self.Wind_Speed).Add, DBLARR(n_Wind_Speeds)
  (self.Emissivity).Add, DBLARR(n_Angles,n_Frequencies,n_Wind_Speeds)

  
  ; Assign the dimension
  self.n_Angles      = n_Angles     
  self.n_Frequencies = n_Frequencies
  self.n_Wind_Speeds = n_Wind_Speeds
 

  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
