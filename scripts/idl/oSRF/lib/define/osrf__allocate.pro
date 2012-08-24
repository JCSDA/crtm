;+
; NAME:
;       OSRF::Allocate
;
; PURPOSE:
;       The OSRF::Allocate procedure method allocates the SRF
;       object data arrays.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Allocate, $
;         n_Points       , $  ; Input
;         Debug=Debug         ; Input keyword
;
; INPUTS:
;       n_Points:    The number of SRF data points to which the
;                    data arrays are to be allocated. Can be a
;                    scalar or vector.
;                    If SCALAR:  n_Bands == 1
;                       VECTOR:  n_Bands == N_ELEMENTS(n_Points)
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar or Rank-1
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       After creating a OSRF object,
;
;         IDL> x = OBJ_NEW('OSRF')
;
;       it can be allocated to the required number of points and bands,
;       in this example 4:
;
;         IDL> n_Points = [100,80,81,97]  ; Quadruple band SRF
;         IDL> x->Allocate, n_Points
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Allocate, $
  n_Points       , $  ; Input
  Debug=Debug         ; Input keyword

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check dimension input
  n_Bands = N_ELEMENTS(n_Points)
  IF ( n_Bands EQ 0 ) THEN $
    MESSAGE, 'Must specify N_POINTS argument.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  FOR i = 0, n_Bands-1 DO BEGIN
    IF ( n_Points[i] LT 1 ) THEN $
      MESSAGE, 'Input N_POINTS for band '+STRTRIM(i+1,2)+'must be at least > 0.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDFOR
  ; ...Destroy object if already allocated
  IF ( self->Associated(Debug=Debug) ) THEN self->Destroy, Debug=Debug
 
 
  ; Perform the allocations 
  self.f1        = PTR_NEW(DBLARR(n_Bands))
  self.f2        = PTR_NEW(DBLARR(n_Bands))
  self.n_Points  = PTR_NEW(DBLARR(n_Bands))
  self.Frequency = PTR_NEW(PTRARR(n_Bands))
  self.Response  = PTR_NEW(PTRARR(n_Bands))
  self.Radiance  = PTR_NEW(PTRARR(n_Bands))
  FOR i = 0, n_Bands-1 DO BEGIN
    (*self.Frequency)[i] = PTR_NEW(DBLARR(n_Points[i]))
    (*self.Response)[i]  = PTR_NEW(DBLARR(n_Points[i]))
    (*self.Radiance)[i]  = PTR_NEW(DBLARR(n_Points[i]))
  ENDFOR
  ; ...The plotting array
  self.Plot_Reference = PTR_NEW(OBJARR(n_Bands))


  ; Assign the dimensions
  self.n_Bands   = n_Bands
  *self.n_Points = n_Points
 
 
  ; Set the allocation indicator
  self.Is_Allocated = TRUE
 
END ; PRO OSRF::Allocate
