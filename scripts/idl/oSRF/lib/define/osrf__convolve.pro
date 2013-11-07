;+
; NAME:
;       OSRF::Convolve
;
; PURPOSE:
;       The OSRF::Convolve function method convolves the response data
;       with user supplied data.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Convolve( band_data, Debug = Debug )
;
; INPUTS:
;       band_data:             Has where each member is an array of band
;                              data with which the OSRF data is be convolved.
;                              One, two, or four bands can be specified, e.g.
;                              as for quadruple bandpass microwave channels.
;                              For IR and VIS channels, the number of passbands is
;                              always one
;                              UNITS:      N/A
;                              TYPE:       HASH
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
;       is described by 128 points. The structure of the input "band_data" argument,
;       which is to be convolved with the OSRF response data, would look like:
;
;         IDL> HELP, band_data
;         BAND_DATA       LIST  <ID=10  NELEMENTS=2>
;         IDL> FOR i = 0, band_data.Count()-1 DO HELP, band_data[i]
;         <Expression>    DOUBLE    = Array[128]
;         <Expression>    DOUBLE    = Array[128]
;
;       The convolution of these data with the channel OSRF object, x, is then achieved via:
;
;         IDL> result = x->Convolve( band_data )
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 17-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Convolve, $
  Band_Data, $  ; Input
  Debug=Debug

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_func_err_handler


  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the number of bands
  self.Get_Property, n_Bands=n_bands, Debug=Debug
  
  
  ; Check that the number of oSRF bands agrees with the input
  IF ( band_data.Count() NE n_bands ) THEN $
    MESSAGE, "Number of input band_data hash elements, "+STRTRIM(band_data.Count(),2)+$
             ", is different from the number of oSRF bands, "+STRTRIM(n_bands,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check the SRF is integrated
  IF ( ~ self.Flag_Is_Set(IS_INTEGRATED_FLAG, Debug=Debug) ) THEN self.Integrate, Debug=Debug


  ; Sum up band integrals
  y = ZERO
  FOR i = 0L, n_bands-1L DO BEGIN
    ; Get current band
    band = i+1
    self->Get_Property, $
      band, $
      Frequency = f, $
      Response  = r, $
      Debug=Debug

    ; Perform the band convolution
    sum = ABS(Integral(f, (Band_Data[band])*r))
    IF ( sum LE ZERO ) THEN $
      MESSAGE, "SRF integration for band #"+STRTRIM(band,2)+" is < zero", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; Accumulate
    y = y + sum
  ENDFOR
  
  
  ; Normalise the result
  self.Get_Property, Integral=intsum, Debug=Debug
  y = y / intsum


  ; Done
  RETURN, y
 
END
