; docformat = 'rst'

;+
; :Todo:
;    not implemented
; 
; :Params:
;    olist : in, required, type=object
;       subclass of `MGcoAbstractList`; what about the type of elements?
;-
pro mg_sort_objects, olist
  compile_opt strictarr

  if (~obj_isa(olist, 'mgcoabstractlist')) then begin
    message, 'olist parameter must be be an MGAbstractList'
  endif
end
