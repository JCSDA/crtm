; docformat = 'rst'

;+
; String substitution routine which substitutes values into a given string
; using the correspondences found in the provided hashtable.
;
; :Returns:
;    string
; 
; :Params:
;    str : in, optional, type=string
;       string to substitute into
;    hashtable : in, required, type=object
;       MGcoHashtable object (string->string) which contains the substitutions
;-
function mg_subs, str, hashtable
  compile_opt strictarr

  _str = str
  
  keys = hashtable->keys(count=nkeys)
  for k = 0L, nkeys - 1L do begin
    _str = mg_streplace(_str, keys[k], hashtable->get(keys[k]))
  endfor
  
  return, _str
end


; main-level example program

hashtable = obj_new('MGcoHashtable', key_type=7, value_type=7)
hashtable->put, '%name', 'XDAP'
hashtable->put, '%current_doc', 'http://quartic.txcorp.com/test.h5'

pattern = '%name - %current_doc'
print, pattern, mg_subs(pattern, hashtable), format='(%"\"%s\" -> \"%s\"")'

obj_destroy, hashtable

end