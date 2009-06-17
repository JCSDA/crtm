;+
; NAME:
;       OSRF::Clear_Flags
;
; PURPOSE:
;       The OSRF::Clear_Flags procedure method clears the bit flag component.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Clear_Flags, $
;         Debug                = Debug               , $  ; Input keyword
;         Interpolated         = Interpolated        , $  ; Input keyword
;         Integrated           = Integrated          , $  ; Input keyword
;         f0_Computed          = f0_Computed         , $  ; Input keyword
;         Frequency_Units      = Frequency_Units     , $  ; Input keyword
;         Interpolation_Method = Interpolation_Method, $  ; Input keyword
;         Integration_Method   = Integration_Method  , $  ; Input keyword
;         All                  = All                      ; Input keyword
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
;       Interpolated:          Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF response data
;                              has NOT been interpolated.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Integrated:            Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF response data
;                              has NOT been integrated.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       f0_Computed:           Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF central frequency
;                              has NOT been computed.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Frequency_Units:       Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF frequency data
;                              units are inverse centimetres (cm-1)
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Interpolation_Method:  Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF interpolation
;                              method used is spline interpolation.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Integration_Method:    Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF integration
;                              method used is Simpson's Rule..
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       All:                   Set this keyword to clear ALL the OSRF bit flags.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       Given a valid, allocated, OSRF object, x, the various bit flags can
;       be cleared individually like so,
;
;         IDL> x->Clear_Flags, /Interpolated        , $
;                              /Integrated          , $
;                              /f0_Computed         , $
;                              /Frequency_Units     , $
;                              /Interpolation_Method, $
;                              /Integration_Method
;
;       Alternatively, the ALL keyword can be used to clear all the flags,
;
;         IDL> x->Clear_Flags, /All
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Clear_Flags, $
  Debug                = Debug               , $  ; Input keyword
  Interpolated         = Interpolated        , $  ; Input keyword
  Integrated           = Integrated          , $  ; Input keyword
  f0_Computed          = f0_Computed         , $  ; Input keyword
  Frequency_Units      = Frequency_Units     , $  ; Input keyword
  Interpolation_Method = Interpolation_Method, $  ; Input keyword
  Integration_Method   = Integration_Method  , $  ; Input keyword
  All                  = All                      ; Input keyword


  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'OSRF structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Clear flags
  IF ( KEYWORD_SET(Interpolated        ) ) THEN self.Flags = self.Flags AND ( NOT INTERPOLATED_FLAG         )
  IF ( KEYWORD_SET(Integrated          ) ) THEN self.Flags = self.Flags AND ( NOT INTEGRATED_FLAG           )
  IF ( KEYWORD_SET(f0_Computed         ) ) THEN self.Flags = self.Flags AND ( NOT F0_COMPUTED_FLAG          )
  IF ( KEYWORD_SET(Frequency_Units     ) ) THEN self.Flags = self.Flags AND ( NOT FREQUENCY_UNITS_FLAG      )
  IF ( KEYWORD_SET(Interpolation_Method) ) THEN self.Flags = self.Flags AND ( NOT INTERPOLATION_METHOD_FLAG )
  IF ( KEYWORD_SET(Integration_Method  ) ) THEN self.Flags = self.Flags AND ( NOT INTEGRATION_METHOD_FLAG   )
  IF ( KEYWORD_SET(All                 ) ) THEN self.Flags = 0L
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Clear_Flags
