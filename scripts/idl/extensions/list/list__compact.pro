;+
; NAME:
;       List::Compact
;
; PURPOSE:
;       The List::Compact function method returns a copy of a list
;       with all nil elements removed.
;
; CALLING SEQUENCE:
;       result = Obj->[List::]Compact()
;
; EXAMPLE:
;       IDL> x = list(1, 'f', !null, 3.14, !null, "bananas")
;       IDL> help, x
;       X               LIST  <ID=17  NELEMENTS=6>
;       IDL> y = x.compact()
;       IDL> help, y
;       Y               LIST  <ID=31  NELEMENTS=4>
;       IDL> print, y
;                  1
;       f
;             3.14000
;       bananas
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 14-Dec-2010
;                       paul.vandelst@noaa.gov
;-

FUNCTION List::Compact
  idx = WHERE(self NE !NULL, count)
  RETURN, ( count GT 0 ) ? self[idx] : LIST()
END
