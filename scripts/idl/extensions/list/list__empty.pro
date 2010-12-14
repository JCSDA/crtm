;+
; NAME:
;       List::Empty
;
; PURPOSE:
;       The List::Empty function method return TRUE if a list contains
;       no elements, FALSE otherwise.
;
; CALLING SEQUENCE:
;       result = Obj->[List::]Empty()
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 14-Dec-2010
;                       paul.vandelst@noaa.gov
;-

FUNCTION List::Empty
  RETURN, N_ELEMENTS(self) EQ 0
END
