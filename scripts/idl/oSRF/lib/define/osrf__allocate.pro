;+
; NAME:
;       OSRF::Allocate
;
; PURPOSE:
;       The OSRF::Allocate procedure method sets the size of 
;       the object data arrays.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Allocate, $
;         n_Points       , $  ; Input
;         Debug=Debug         ; Input keyword
;
; INPUTS:
;       n_Points:    The number of SRF data points for each passband.
;                    Can be a scalar or vector.
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
;         IDL> x.Allocate, n_Points
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
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  
  ; Check dimension input
  n_bands = N_ELEMENTS(n_Points)
  ; ...Were ANY specified?
  IF ( n_bands EQ 0 ) THEN $
    MESSAGE, "Must specify N_POINTS argument.", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Was a valid number of bands specified?
  IF ( (n_bands NE 1) AND $
       (n_bands NE 2) AND $
       (n_bands NE 4)     ) THEN $
    MESSAGE, "Invalid number of bands, "+STRTRIM(n_bands,2)+", specified. Only 1, 2, or 4 is valid.", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Are the number-of-points/band meaningful?
  FOR i = 0, n_bands-1 DO BEGIN
    IF ( n_Points[i] LT 1 ) THEN $
      MESSAGE, "Input N_POINTS for band "+STRTRIM(i+1,2)+" must be at least > 0.", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDFOR
  
  
  ; Destroy object if already allocated
  IF ( self.Associated(Debug=Debug) ) THEN self.Destroy, Debug=Debug
 
 
  ; Set the hash
  self.n_Points = HASH(LINDGEN(n_bands)+1L, n_Points)


  ; Assign the dimensions
  self.n_Bands = n_Bands
 
 
  ; Set the allocation indicator
  self.Is_Allocated = TRUE
 
END
