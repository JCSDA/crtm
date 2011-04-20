; docformat = 'rst'

;+
; Index containing all directories, files, routines, and arguments.
;-

; need:
;   - a way to partition given a number of items per page
;   - a way to iterate through the results

;+
; Returns an array of first letters of the names of the items in the index.
;
; :Returns: 
;    strarr
;    
; :Keywords: 
;    count : out, optional, type=long
;       number of first letters for items in the index
;-
function doctreeindex::getFirstLetters, count=count
  compile_opt strictarr, hidden
  
  ind = where(self.letters, count)
  if (count gt 0) then begin
    return, string(reform(byte(ind), 1, count))
  endif else begin
    return, -1L
  endelse
end


;+
; Add the item to the index.
;
; :Params:
;    item : in, required, type=object
;       item to add to the index; any object with 'index_name', 'index_url',
;       and 'index_type' available from getVariable can be added
;- 
pro doctreeindex::add, item
  compile_opt strictarr, hidden

  item->getProperty, name=name
  self.items->put, { name: name, item: item }
  ++self.letters[byte(strupcase(strmid(name, 0, 1)))]
end


;+
; Free resources of index.
;-
pro doctreeindex::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, self.items
end


;+
; Create the index.
; 
; :Returns: 1 for success, 0 for failure
;-
function doctreeindex::init
  compile_opt strictarr, hidden

  ; strings -> objects
  self.items = obj_new('MGcoArraylist', example={ name:'', item: obj_new() }, $
                       block_size=100)
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    items 
;       hash table of strings -> objects (index names to DOCtree* 
;       objects)
;    letters 
;       histogram of letter usage
;-
pro doctreeindex__define
  compile_opt strictarr, hidden

  define = { DOCtreeIndex, $
             items: obj_new(), $
             letters: lonarr(256) $
           }
end
