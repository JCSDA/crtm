;+
; NAME:
;       List::Clear
;
; PURPOSE:
;       The List::Clear procedure method removes all elements
;       from a list.
;
; CALLING SEQUENCE:
;       Obj->[List::]Clear
;
; EXAMPLE:
;       IDL> x = list(1, 'f', !null, 3.14, !null, "bananas")
;       IDL> help, x
;       X               LIST  <ID=2  NELEMENTS=6>
;       IDL> x.clear
;       IDL> help, x
;       X               LIST  <ID=2  NELEMENTS=0>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 14-Dec-2010
;                       paul.vandelst@noaa.gov
;-

PRO List::Clear
  self.Remove, /ALL
END
