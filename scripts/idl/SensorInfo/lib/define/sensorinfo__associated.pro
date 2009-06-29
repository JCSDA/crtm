;+
; NAME:
;       SensorInfo::Associated
;
; PURPOSE:
;       The SensorInfo::Associated function determies if the pointer 
;       components of a SensorInfo object are associated with a target.
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo::]Associated( $
;                  ANY_Test=ANY_Test, $  ; Input keyword
;                  Debug=Debug        )  ; Input keyword
;
; KEYWORDS:
;       ANY_Test:    Set this keyword to test if ANY of the
;                    SensorInfo pointer components are associated.
;                    The default is to test if ALL the components
;                    are associated.
;                    If NOT SET => test if ALL the components
;                                  are associated.  (DEFAULT)
;                       SET,    => test if ANY of the components
;                                  are associated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == TRUE the object pointer components are associated.
;                       == FALSE the object pointer components are NOT associated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; EXAMPLE:
;       After creating a SensorInfo object,
;
;         IDL> x = OBJ_NEW('SensorInfo')
;
;       the association status can be tested,
;
;         IDL> IF ( x->Associated() ) THEN PRINT,'Associated' ELSE PRINT,'Not Associated'
;         Not Associated
;
;       Once the object pointer components have been allocated, the associated
;       status should be "true",
;
;         IDL> x->Allocate,10
;         IDL> IF ( x->Associated() ) THEN PRINT,'Associated' ELSE PRINT,'Not Associated'
;         Associated
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo::Associated, $
  ANY_Test=ANY_Test, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
  ENDELSE
 
  ; Test association status
  Association_Status = FALSE
  IF ( KEYWORD_SET( ANY_Test ) ) THEN BEGIN
    IF ( PTR_VALID( self.Sensor_Channel ) AND $
         PTR_VALID( self.Use_Flag       ) AND $
         PTR_VALID( self.Noise          )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( self.Sensor_Channel ) OR $
         PTR_VALID( self.Use_Flag       ) OR $
         PTR_VALID( self.Noise          )    ) THEN Association_Status = TRUE
  ENDELSE
 
  RETURN, Association_Status
 
END ; FUNCTION SensorInfo::Associated
