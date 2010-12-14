;+
; NAME:
;       List::Length
;
; PURPOSE:
;       The List::Length function method returns the number of
;       elements in a list.
;
; CALLING SEQUENCE:
;       result = Obj->[List::]Length()
;
; EXAMPLE:
;       IDL> x = list(1, 'f', !null, 3.14, !null, "bananas")
;       IDL> help, x
;       X               LIST  <ID=2  NELEMENTS=6>
;       IDL> print, x.length()
;                 6
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 14-Dec-2010
;                       paul.vandelst@noaa.gov
;-

FUNCTION List::Length
  RETURN, N_ELEMENTS(self)
END
