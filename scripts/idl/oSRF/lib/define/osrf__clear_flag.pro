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
;         Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
;         Is_Difference        = Is_Difference       , $  ; Input keyword
;         Is_HiRes             = Is_HiRes            , $  ; Input keyword
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
;       Linear_Interpolation:  Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF interpolation
;                              method used is spline interpolation.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_Difference:         Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF response data
;                              is NOT a difference comparison.
;                              UNITS:      N/A
;                              TYPE:       INTEGER
;                              DIMENSION:  Scalar
;                              ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Is_HiRes:              Set this keyword to clear the corresponding flag.
;                              Clearing this flag indicates the SRF data was
;                              interpolated to a low resolution frequency grid.
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

PRO oSRF::Clear_Flag, $
  Debug                = Debug               , $  ; Input keyword
  Is_Interpolated      = Is_Interpolated     , $  ; Input keyword
  Is_Integrated        = Is_Integrated       , $  ; Input keyword
  f0_Computed          = f0_Computed         , $  ; Input keyword
  Linear_Interpolation = Linear_Interpolation, $  ; Input keyword
  Is_Difference        = Is_Difference       , $  ; Input keyword
  Is_HiRes             = Is_HiRes            , $  ; Input keyword
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
  IF ( KEYWORD_SET(Is_Interpolated     ) ) THEN self.Flags = self.Flags AND ( NOT IS_INTERPOLATED_FLAG.position      )
  IF ( KEYWORD_SET(Is_Integrated       ) ) THEN self.Flags = self.Flags AND ( NOT IS_INTEGRATED_FLAG.position        )
  IF ( KEYWORD_SET(f0_Computed         ) ) THEN self.Flags = self.Flags AND ( NOT F0_COMPUTED_FLAG.position          )
  IF ( KEYWORD_SET(Linear_Interpolation) ) THEN self.Flags = self.Flags AND ( NOT LINEAR_INTERPOLATION_FLAG.position )
  IF ( KEYWORD_SET(Is_Difference       ) ) THEN self.Flags = self.Flags AND ( NOT IS_DIFFERENCE_FLAG.position        )
  IF ( KEYWORD_SET(Is_HiRes            ) ) THEN self.Flags = self.Flags AND ( NOT IS_HIRES_FLAG.position             )
  IF ( KEYWORD_SET(All                 ) ) THEN self.Flags = 0L
  
END
