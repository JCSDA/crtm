;+
; NAME:
;       OSRF::Set_Flag
;
; PURPOSE:
;       The OSRF::Set_Flag procedure method sets the bit value(s) of the
;       flags property for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Set_Flag, $
;         Debug                = Debug               , $  ; Input keyword
;         Is_Interpolated      = Is_Interpolated     , $  ; Input keyword
;         Is_Integrated        = Is_Integrated       , $  ; Input keyword
;         f0_Computed          = f0_Computed         , $  ; Input keyword
;         Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
;         Is_Difference        = Is_Difference       , $  ; Input keyword
;         Is_HiRes             = Is_HiRes                 ; Input keyword
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
;       Is_Interpolated:       Set this keyword to indicate the SRF response
;                              data has been interpolated.
;                              If NOT SET => SRF has NOT been interpolated. [DEFAULT]
;                                 SET     => SRF has been interpolated. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_Integrated:         Set this keyword to indicate the SRF response
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
;       Linear_Interpolation:  Set this keyword to indicate the SRF was/should be linearly
;                              interpolated.
;                              If NOT SET => Spline interpolation is used [DEFAULT]
;                                 SET     => Linear interpolation is used. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_Difference:         Set this keyword to indicate the SRF response data is a 
;                              difference comparison.
;                              If NOT SET => Response data is a regular SRF. [DEFAULT]
;                                 SET     => Response data is a difference comparison. 
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_HiRes:              Set this keyword to indicate the SRF data was interpolated
;                              to a high resolution frequency grid.
;                              If NOT SET => Interpolation frequency grid is LOW RESOLUTION. [DEFAULT]
;                                 SET     => Interpolation frequency grid is HIGH RESOLUTION. 
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
;         IDL> x->Set_Flag, /Is_Interpolated     , $
;                           /Is_Integrated       , $
;                           /Frequency_GHz       , $
;                           /Linear_Interpolation
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Set_Flag, $
  Debug                = Debug               , $  ; Input keyword
  Is_Interpolated      = Is_Interpolated     , $  ; Input keyword
  Is_Integrated        = Is_Integrated       , $  ; Input keyword
  f0_Computed          = f0_Computed         , $  ; Input keyword
  Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
  Is_Difference        = Is_Difference       , $  ; Input keyword
  Is_HiRes             = iS_HiRes                 ; Input keyword


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 

  ; Set flags
  IF ( KEYWORD_SET(Is_Interpolated     ) ) THEN self.Flags = self.Flags OR IS_INTERPOLATED_FLAG.position
  IF ( KEYWORD_SET(Is_Integrated       ) ) THEN self.Flags = self.Flags OR IS_INTEGRATED_FLAG.position          
  IF ( KEYWORD_SET(f0_Computed         ) ) THEN self.Flags = self.Flags OR F0_COMPUTED_FLAG.position          
  IF ( KEYWORD_SET(Linear_Interpolation) ) THEN self.Flags = self.Flags OR LINEAR_INTERPOLATION_FLAG.position
  IF ( KEYWORD_SET(Is_Difference       ) ) THEN self.Flags = self.Flags OR IS_DIFFERENCE_FLAG.position          
  IF ( KEYWORD_SET(Is_HiRes            ) ) THEN self.Flags = self.Flags OR IS_HIRES_FLAG.position          



  ; Output debug information
  IF ( KEYWORD_SET(Debug) ) THEN self.Display_Flags

END
