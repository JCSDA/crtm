;+
; NAME:
;       OSRF::Set_Flags
;
; PURPOSE:
;       The OSRF::Set_Flags procedure method sets the bit flag component.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Set_Flags, $
;         Debug                = Debug               , $  ; Input keyword
;         Interpolated         = Interpolated        , $  ; Input keyword
;         Integrated           = Integrated          , $  ; Input keyword
;         f0_Computed          = f0_Computed         , $  ; Input keyword
;         Frequency_Units      = Frequency_Units     , $  ; Input keyword
;         Interpolation_Method = Interpolation_Method, $  ; Input keyword
;         Integration_Method   = Integration_Method       ; Input keyword
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
;       Interpolated:          Set this keyword to indicate the SRF response
;                              data has been interpolated.
;                              If NOT SET => SRF has NOT been interpolated. [DEFAULT]
;                                 SET     => SRF has been interpolated. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Integrated:            Set this keyword to indicate the SRF response
;                              data has been integrated.
;                              If NOT SET => SRF has NOT been integrated. [DEFAULT]
;                                 SET     => SRF has been integrated. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       f0_Computed:           Set this keyword to indicate the SRF central frequency
;                              has been computed.
;                              If NOT SET => SRF f0 has NOT been computed. [DEFAULT]
;                                 SET     => SRF f0 has been computed.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Frequency_Units:       Set this keyword to indicate the SRF frequency units.
;                              If NOT SET => Units are inverse centimetres (cm-1) [DEFAULT]
;                                 SET     => Units are Gigahertz (GHz). 
;                              Alternatively, the keyword can be set using the
;                              parameterised values,
;                              If OSRF_INVERSE_CM => Units are inverse centimetres (cm-1) [DEFAULT]
;                                 OSRF_GHZ        => Units are Gigahertz (GHz).
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Interpolation_Method:  Set this keyword to indicate the SRF interpolation method.
;                              If NOT SET => Spline interpolation is used [DEFAULT]
;                                 SET     => Linear interpolation is used. 
;                              Alternatively, the keyword can be set using the
;                              parameterised values,
;                              If OSRF_SPLINE => Spline interpolation is used [DEFAULT]
;                                 OSRF_LINEAR => Linear interpolation is used. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Integration_Method:    Set this keyword to indicate the SRF integration method.
;                              If NOT SET => Simpson's rule is used [DEFAULT]
;                                 SET     => Gaussian integration is used. 
;                              Alternatively, the keyword can be set using the
;                              parameterised values,
;                              If OSRF_SIMPSON  => Simpson's rule is used [DEFAULT]
;                                 OSRF_GAUSSIAN => Gaussian integration is used. 
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
;       be set like so,
;
;         IDL> x->Set_Flags, /Interpolated        , $
;                            /Integrated          , $
;                            /Frequency_Units     , $
;                            /Interpolation_Method, $
;                            /Integration_Method
;
;       Alternatively, for those flags that are used to switch between different
;       functionality, the parameterised forms can be used,
;
;         IDL> @srf_parameters
;         IDL> x->Set_Flags, Frequency_Units      = OSRF_GHZ     , $
;                            Interpolation_Method = OSRF_LINEAR  , $
;                            Integration_Method   = OSRF_GAUSSIAN
;
;       where the parameter values are defined in the srf_parameters include file.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Set_Flags, $
  Debug                = Debug               , $  ; Input keyword
  Interpolated         = Interpolated        , $  ; Input keyword
  Integrated           = Integrated          , $  ; Input keyword
  f0_Computed          = f0_Computed         , $  ; Input keyword
  Frequency_Units      = Frequency_Units     , $  ; Input keyword
  Interpolation_Method = Interpolation_Method, $  ; Input keyword
  Integration_Method   = Integration_Method       ; Input keyword


  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Set flags
  IF ( KEYWORD_SET(Interpolated        ) ) THEN self.Flags = self.Flags OR INTERPOLATED_FLAG        
  IF ( KEYWORD_SET(Integrated          ) ) THEN self.Flags = self.Flags OR INTEGRATED_FLAG          
  IF ( KEYWORD_SET(f0_Computed         ) ) THEN self.Flags = self.Flags OR F0_COMPUTED_FLAG          
  IF ( KEYWORD_SET(Frequency_Units     ) ) THEN self.Flags = self.Flags OR FREQUENCY_UNITS_FLAG     
  IF ( KEYWORD_SET(Interpolation_Method) ) THEN self.Flags = self.Flags OR INTERPOLATION_METHOD_FLAG
  IF ( KEYWORD_SET(Integration_Method  ) ) THEN self.Flags = self.Flags OR INTEGRATION_METHOD_FLAG  

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Set_Flags
