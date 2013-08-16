;+
; NAME:
;       OSRF::Clear_Flag
;
; PURPOSE:
;       The OSRF::Clear_Flag procedure method clears the bit value(s) of the
;       flags property for this object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Clear_Flag, $
;         Debug                = Debug               , $  ; Input keyword
;         Is_Interpolated      = Is_Interpolated     , $  ; Input keyword
;         Is_Integrated        = Is_Integrated       , $  ; Input keyword
;         f0_Computed          = f0_Computed         , $  ; Input keyword
;         Frequency_GHz        = Frequency_GHz       , $  ; Input keyword
;         Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
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
;       Is_Interpolated:       Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF response data
;                              has NOT been interpolated.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_Integrated:         Set this keyword to clear the corresponding flag.
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
;       Frequency_GHz:         Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF frequency data
;                              units are inverse centimetres (cm-1)
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Linear_Interpolation:  Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF interpolation
;                              method used is spline interpolation.
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
;         IDL> x->Clear_Flag, /Is_Interpolated     , $
;                             /Is_Integrated       , $
;                             /f0_Computed         , $
;                             /Frequency_GHz       , $
;                             /Linear_Interpolation
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

PRO OSRF::Clear_Flag, $
  Debug                = Debug               , $  ; Input keyword
  Is_Interpolated      = Is_Interpolated     , $  ; Input keyword
  Is_Integrated        = Is_Integrated       , $  ; Input keyword
  f0_Computed          = f0_Computed         , $  ; Input keyword
  Frequency_GHz        = Frequency_GHz       , $  ; Input keyword
  Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
  All                  = All                      ; Input keyword


  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Clear flags
  IF ( KEYWORD_SET(Is_Interpolated     ) ) THEN self.Flags = self.Flags AND ( NOT INTERPOLATED_FLAG.position         )
  IF ( KEYWORD_SET(Is_Integrated       ) ) THEN self.Flags = self.Flags AND ( NOT INTEGRATED_FLAG.position           )
  IF ( KEYWORD_SET(f0_Computed         ) ) THEN self.Flags = self.Flags AND ( NOT F0_COMPUTED_FLAG.position          )
  IF ( KEYWORD_SET(Frequency_GHz       ) ) THEN self.Flags = self.Flags AND ( NOT FREQUENCY_GHZ_FLAG.position        )
  IF ( KEYWORD_SET(Linear_Interpolation) ) THEN self.Flags = self.Flags AND ( NOT INTERPOLATION_METHOD_FLAG.position )
  IF ( KEYWORD_SET(All                 ) ) THEN self.Flags = 0L
  
END
