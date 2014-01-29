;+
; NAME:
;       TauProfile::Create
;
; PURPOSE:
;       The TauProfile::Create procedure method creates an allocated
;       instance of a TauProfile object. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]Create, $
;         n_Layers       , $ ; Input
;         n_Channels     , $ ; Input
;         n_Angles       , $ ; Input
;         n_Profiles     , $ ; Input
;         n_Molecule_Sets, $ ; Input
;         Debug = Debug      ; Input keyword
;
; INPUTS:
;       n_Layers:         The number of layers dimension of the
;                         TauProfile data.
;                         Must be > 0.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;
;       n_Channels:       The number of sensor channels dimension
;                         of the TauProfile data.
;                         Must be > 0.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;                  
;       n_Angles:         The number of view angles dimension of the
;                         TauProfile data.
;                         Must be > 0.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;                  
;       n_Profiles:       The number of atmospheric profiles dimension
;                         of the TauProfile data.
;                         Must be > 0.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;                  
;       n_Molecule_Sets:  The number of molecular data sets dimension
;                         of the TauProfile data.
;                         Must be > 0.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN)
;                  
; INPUT KEYWORDS:
;       Debug:            Set this keyword for debugging. If set then:
;                         - the error handler for this procedure is disabled
;                           so that execution halts where the error occurs,
;                         - more verbose output is produced.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO TauProfile::Create, $
  n_Layers       , $ ; Input
  n_Channels     , $ ; Input
  n_Angles       , $ ; Input
  n_Profiles     , $ ; Input
  n_Molecule_Sets, $ ; Input
  Debug = Debug      ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  @tauprofile_pro_err_handler


  ; Check input
  n_arguments = 5
  IF ( N_PARAMS() LT n_arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check that required arguments are defined
  IF ( N_ELEMENTS(n_Layers       ) EQ 0 OR $
       N_ELEMENTS(n_Channels     ) EQ 0 OR $
       N_ELEMENTS(n_Angles       ) EQ 0 OR $
       N_ELEMENTS(n_Profiles     ) EQ 0 OR $
       N_ELEMENTS(n_Molecule_Sets) EQ 0    ) THEN $
    MESSAGE, 'Input TauProfile dimensions are not defined.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check that dimensions are valid
  IF ( n_Layers        LT 1 OR $
       n_Channels      LT 1 OR $
       n_Angles        LT 1 OR $
       n_Profiles      LT 1 OR $
       n_Molecule_Sets LT 1    ) THEN $
    MESSAGE, 'Input TauProfile dimensions must all be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Assign the dimensions
  self.n_Layers        = n_Layers       
  self.n_Channels      = n_Channels     
  self.n_Angles        = n_Angles       
  self.n_Profiles      = n_Profiles     
  self.n_Molecule_Sets = n_Molecule_Sets
 

  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
