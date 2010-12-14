;+
; NAME:
;       List::n_Items
;
; PURPOSE:
;       The List::n_Items function method returns the number of
;       non-null elements in a list.
;
; CALLING SEQUENCE:
;       result = Obj->[List::]n_Items()
;
; EXAMPLE:
;       IDL> x = list(1, 'f', !null, 3.14, !null, "bananas")
;       IDL> help, x
;       X               LIST  <ID=2  NELEMENTS=6>
;       IDL> print, x.n_items()
;                 4
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 14-Dec-2010
;                       paul.vandelst@noaa.gov
;-

FUNCTION List::n_Items
  idx = WHERE(self NE !NULL, count)
  RETURN, count
END
