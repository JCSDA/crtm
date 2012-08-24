;+
; NAME:
;       OSRF::Convolve
;
; PURPOSE:
;       The OSRF::Convolve function method convolves the response data
;       with user supplied data.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Convolve( ptr_array, Debug = Debug )
;
; INPUTS:
;       ptr_array:             Pointer array where each element is a pointer
;                              to an array of band data with which the OSRF
;                              data is be convolved. Up to four bands can
;                              be specified, e.g. as for quadruple bandpass
;                              microwave channels. The structure of this
;                              array can be schematically described like 
;                              below for the 4-passband case:
;
;                                 -------------------------
;                                 |  1  |  2  |  3  |  4  |
;                                 -------------------------
;                                    |     |     |     |
;                                    |     |     |     |
;                                   \|/   \|/   \|/   \|/
;                                    *     *     *     *
;                                   ---   ---   ---   ---
;                                   | |   | |   | |   | |
;                                   ---   ---   ---   ---
;                                   | |   | |   | |   | |
;                                   ---   |.|   |.|   ---
;                                   | |   |.|   |.|   | |
;                                   |.|   |.|   |.|   |.|
;                                   |.|   | |   | |   |.|
;                                   |.|   ---   ---   |.|
;                                   | |   | |   | |   | |
;                                   ---   ---   ---   ---
;                                   | |               | |
;                                   ---               ---
;
;                              For IR and VIS channels, the number of passbands is
;                              always one
;                              UNITS:      N/A
;                              TYPE:       PTRARR
;                              DIMENSION:  n_Bands
;                              ATTRIBUTES: INTENT(IN)
;
; INPUT KEYWORD PARAMETERS:
;       Debug:                 Set this keyword for debugging.
;                              If NOT SET => Error handler is enabled. (DEFAULT)
;                                 SET     => Error handler is disabled; Routine
;                                            traceback output is enabled.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:                The return value is the convolved quantity.
;                              UNITS:      Variable
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       Let's say we have a double passband microwave channel, where each passband
;       is described by 128 points. The structure of the input "ptr_array" argument,
;       which is to be convolved with the OSRF response data, would look like:
;
;         IDL> HELP, ptr_array
;         <PtrHeapVar151> POINTER   = Array[2]
;         IDL> n = N_ELEMENTS(ptr_array)
;         IDL> FOR i = 0, n-1 DO HELP, *ptr_array[i]
;         <PtrHeapVar154> DOUBLE    = Array[128]
;         <PtrHeapVar157> DOUBLE    = Array[128]
;
;       The convolution of these data with the channel OSRF object, x, is then achieved via:
;
;         IDL> result = x->Convolve( ptr_array )
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Convolve, $
  ptr_array, $  ; Input
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_func_err_handler


  ; Check if object has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the number of bands
  self->Get_Property, $
    n_Bands = n_Bands, $
    Debug   = Debug


  ; Sum up band integrals
  y = ZERO
  FOR i = 0L, n_Bands-1L DO BEGIN
    IF ( ~ self->Flag_Is_Set(INTEGRATED_FLAG) ) THEN self->Integrate, Debug=Debug
    ; Get band data
    Band = i+1
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response  = r, $
      Debug=Debug
    ; Integrate
    Sum = Integral(f, (*ptr_array[i])*r)
    IF ( Sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(Band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    y = y + Sum
  ENDFOR
  self->Get_Property, $
    Integral = IntSum, $
    Debug    = Debug
  y = y / IntSum


  ; Done
  RETURN, y
 
END ; FUNCTION OSRF::Convolve
