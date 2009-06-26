;+
; NAME:
;       SensorInfo_List::Is_Empty
;
; PURPOSE:
;       The SensorInfo_List::Is_Empty function method determines if there 
;       is anything in a SensorInfo linked list
;
; CALLING SEQUENCE:
;       Result = Obj->[SensorInfo_List::]Is_Empty()
;
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the logical
;                    true or false if the list is empty. The integer
;                    codes are defined in the error_codes include file.
;                    If == TRUE  the list is empty
;                       == FALSE the list contains data
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       error_codes:   Include file containing error code definitions.
;
; EXAMPLE:
;       To test if a SensorInfo_List object, list, is empty or not,
;
;         IDL> IF ( list->Is_Empty() ) THEN PRINT, 'List is empty'
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 02-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION SensorInfo_List::Is_Empty

  ; Setup
  @error_codes
  
  ; Is there a valid first node?
  IF ( NOT PTR_VALID(self.First) ) THEN RETURN, TRUE
  IF ( NOT PTR_VALID((*self.First).Next) ) THEN RETURN, TRUE

  ; Yes, there is.
  RETURN, FALSE
  
END ; FUNCTION SensorInfo_List::Is_Empty
