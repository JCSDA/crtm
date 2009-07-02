;+
; NAME:
;       AtmProfile::Associated
;
; PURPOSE:
;       The AtmProfile::Associated function determies if the pointer 
;       components of a AtmProfile object are associated with a target.
;
; CALLING SEQUENCE:
;       Result = Obj->[AtmProfile::]Associated( $
;                  ANY_Test=ANY_Test, $  ; Input keyword
;                  Debug=Debug        )  ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       ANY_Test:    Set this keyword to test if ANY of the
;                    AtmProfile pointer components are associated.
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
;       error_codes: Include file containing error code definitions.
;
; EXAMPLE:
;       After creating a AtmProfile object,
;
;         IDL> x = OBJ_NEW('AtmProfile')
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
;       Written by:     Paul van Delst, 01-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION AtmProfile::Associated, $
  ANY_Test=ANY_Test, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up
  ; ...error handler
  @error_codes
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    MESSAGE, '--> Entered.', /INFORMATIONAL
    MsgSwitch = 0
  ENDIF ELSE BEGIN
    CATCH, Error_Status
    IF ( Error_Status NE 0 ) THEN BEGIN
      CATCH, /CANCEL
      MESSAGE, !ERROR_STATE.MSG, /CONTINUE
      RETURN, FALSE
    ENDIF
    MsgSwitch = 1
  ENDELSE
  ; ...Check keyword
  ALL_Test = KEYWORD_SET(ANY_Test) ? FALSE : TRUE


  ; Test association status
  Association_Status = FALSE
  IF ( ALL_Test ) THEN BEGIN
    IF ( PTR_VALID( self.Absorber_ID           ) AND $
         PTR_VALID( self.Absorber_Units_ID     ) AND $
         PTR_VALID( self.Absorber_Units_Name   ) AND $
         PTR_VALID( self.Absorber_Units_LBLRTM ) AND $
         PTR_VALID( self.Level_Pressure        ) AND $
         PTR_VALID( self.Level_Temperature     ) AND $
         PTR_VALID( self.Level_Absorber        ) AND $
         PTR_VALID( self.Level_Altitude        ) AND $
         PTR_VALID( self.Layer_Pressure        ) AND $
         PTR_VALID( self.Layer_Temperature     ) AND $
         PTR_VALID( self.Layer_Absorber        ) AND $
         PTR_VALID( self.Layer_Delta_Z         ) AND $
         PTR_VALID( self.xsysvar               ) AND $
         PTR_VALID( self.ysysvar               ) AND $
         PTR_VALID( self.psysvar               )     ) THEN Association_Status = TRUE
  ENDIF ELSE BEGIN
    IF ( PTR_VALID( self.Absorber_ID           ) OR $
         PTR_VALID( self.Absorber_Units_ID     ) OR $
         PTR_VALID( self.Absorber_Units_Name   ) OR $
         PTR_VALID( self.Absorber_Units_LBLRTM ) OR $
         PTR_VALID( self.Level_Pressure        ) OR $
         PTR_VALID( self.Level_Temperature     ) OR $
         PTR_VALID( self.Level_Absorber        ) OR $
         PTR_VALID( self.Level_Altitude        ) OR $
         PTR_VALID( self.Layer_Pressure        ) OR $
         PTR_VALID( self.Layer_Temperature     ) OR $
         PTR_VALID( self.Layer_Absorber        ) OR $
         PTR_VALID( self.Layer_Delta_Z         ) OR $
         PTR_VALID( self.xsysvar               ) OR $
         PTR_VALID( self.ysysvar               ) OR $
         PTR_VALID( self.psysvar               )    ) THEN Association_Status = TRUE
  ENDELSE
 
  RETURN, Association_Status
 
END ; FUNCTION AtmProfile::Associated
