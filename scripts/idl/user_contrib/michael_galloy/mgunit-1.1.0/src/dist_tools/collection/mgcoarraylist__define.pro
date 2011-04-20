; docformat = 'rst'

;+
; An array list is a way to have an arbitrary length list of any particular
; IDL variable (but all elements must be the same type). An MGcoArrayList 
; implements the same interface as IDL_Container, but can contain any IDL 
; type.
;
; :Author: Michael Galloy
; :Version: 1.0
;
; :Properties:
;    type 
;       type code as in SIZE function to specify the type of elements in the 
;       list; TYPE or EXAMPLE keyword must be used when initializing the array
;       list
;    block_size 
;       initial size of the data array; defaults to 1000 if not specified
;    example 
;       type defined by an example instead of a type code (required for array 
;       lists of structures)
;    count 
;       number of elements in the array list
;    _ref_extra 
;       keywords to MGcoAbstractList::getProperty
;
; :Examples:
;    For example::
;
;       a = obj_new('MGcoArrayList', type=7)
;
;       a->add, 'a'
;       a->add, ['b', 'c', 'd']
;
;       print, a->count()
;       print, a->get(/all)
;       print, a->get(position=1)
;
;       obj_destroy, a
;-


;+
; Get properties of the list.
;-
pro mgcoarraylist::getProperty, type=type, block_size=blockSize, $
                                example=example, count=count, _ref_extra=e
  compile_opt strictarr, logical_predicate

  if (arg_present(type)) then type = self.type
  if (arg_present(blockSize)) then blockSize = self.blockSize
  if (arg_present(example) && ptr_valid(self.pExample)) then begin
    example = *self.pExample
  endif
  if (arg_present(count)) then count = self.nUsed
  
  if (n_elements(e) gt 0) then begin
    self->mgcoabstractlist::getProperty, _strict_extra=e
  endif
end


;+
; Set properties of the list.
;-
pro mgcoarraylist::setProperty, type=type, block_size=blockSize
  compile_opt strictarr
  on_error, 2
  
  if (n_elements(type) gt 0 && ((type eq 8) ne (self.type eq 8))) then begin
    message, 'Cannot convert between structures and other types'
  endif
  
  if (n_elements(blockSize) gt 0 && (blockSize lt self.nUsed)) then begin
    message, 'Cannot set the blockSize to less than number of elements in list'
  endif
  
  if (n_elements(type) eq 0 && n_elements(blockSize) eq 0) then return
  
  self.version++
  
  self.type = n_elements(type) eq 0 ? self.type : type 
  self.blockSize = n_elements(blockSize) eq 0 ? self.blockSize : blockSize
  
  if (self.type eq 8) then begin
    newData = replicate(*self.pExample, self.blockSize) 
  endif else begin
    newData = make_array(self.blockSize, type=self.type, /nozero)
  endelse
  
  newData[0] = (*self.pData)[0:self.nUsed-1L]
  *self.pData = newData
end


;+
; Remove specified elements from the list.
; 
; :Params:
;    elements : in, optional, type=type of list 
;       elements of the list to remove
;
; :Keywords:
;    position : in, optional, type=long 
;       set to a scalar or vector array of indices to remove from the list
;    all : in, optional, type=boolean 
;       set to remove all elements of the list
;-
pro mgcoarraylist::remove, elements, position=position, all=all
  compile_opt strictarr
  on_error, 2

  self.version++

  ; nothing to remove
  if (self.nUsed eq 0L) then return

  ; handle ALL keyword
  if (keyword_set(all)) then begin
    self.nUsed = 0L
    return
  endif

  ; handle POSITION keyword
  case n_elements(position) of
    0 :
    1 : begin 
      pos = position[0] 
      if (pos ge self.nUsed or pos lt 0) then begin
        message, 'Position value out of range'
      endif
      if (pos ne self.nUsed - 1L) then begin 
        (*self.pData)[pos] = (*self.pData)[pos+1L:self.nUsed-1L]
      endif
      self.nUsed--
      return
    end
    else : message, 'Position must be a scalar or 1-element array'
  endcase

  ; remove first element in the list
  if (n_elements(elements) eq 0L) then begin
    if (self.nUsed ne 1) then begin
      (*self.pData)[0L] = (*self.pData)[1L:self.nUsed-1L]
    endif
    self.nUsed--
    return
  endif

  ; remove specified elements in the list
  for i = 0L, n_elements(elements) - 1L do begin 
    keepIndices = where((*self.pData)[0L:self.nUsed-1L] ne elements[i], $
                        nKeep)
    if (nKeep gt 0L) then begin
      self.nUsed = nKeep
      (*self.pData)[0L] = (*self.pData)[keepIndices]
    endif else begin
      self.nUsed = 0L 
      break
    endelse
  endfor
end


;+
; Move an element of the list to another position.
; 
; :Params:
;    source : in, required, type=long 
;       index of the element to move
;    destination : in, required, type=long
;       index of position to move element
;-
pro mgcoarraylist::move, source, destination
  compile_opt strictarr, logical_predicate

  self.version++

  ; bounds checking on source and destination
  if (source lt 0 || source ge self.nUsed) then begin
    message, 'Source index out of bounds'
  endif
  if (destination lt 0 || destination ge self.nUsed) then begin
    message, 'Destination index out of bounds'
  endif

  sourceElement = (*self.pData)[source]
  if (source lt destination) then begin
    (*self.pData)[source] =  (*self.pData)[source+1L:destination]
  endif else begin 
    (*self.pData)[destination+1L] = (*self.pData)[destination:source-1L]
  endelse
  (*self.pData)[destination] = sourceElement
end


;+
; Determines whether a list contains specified elements.
;
; :Returns: 1B if contained or 0B if otherwise
;
; :Params:
;    elements : in, required, type=type of list 
;       scalar or vector of elements of the same type as the list
;
; :Keywords:
;    position : out, optional, type=long
;       set to a named variable that will return the position of the first 
;       instance of the corresponding element of the specified elements
;-
function mgcoarraylist::isContained, elements, position=position
  compile_opt strictarr, logical_predicate

  n = n_elements(elements)
  position = lonarr(n)

  isContained = n gt 0 ? bytarr(n) : 0B
  for i = 0L, n - 1L do begin
    ind = where(*self.pData eq elements[i], nFound)
    isContained[i] = nFound gt 0L
    position[i] = ind[0]
  endfor

  return, isContained
end


;+
; Add elements to the list.
;
; :Params:
;    elements : in, required, type=list type
;       scalar or vector array of the same type as the list
;
; :Keywords:
;    position : in, optional, type=long/lonarr, default=end of list 
;       index or index array to insert elements at; if array, must match 
;       number of elements
;-
pro mgcoarraylist::add, elements, position=position
  compile_opt strictarr

  self.version++
  nNew = n_elements(elements)

  ; double the size of the list until there is enough room
  if (self.nUsed + nNew gt self.blockSize) then begin
    self.blockSize *= 2L  
    while (self.nUsed + nNew gt self.blockSize) do self.blockSize *= 2L
    if (self.type eq 8) then begin
      newData = replicate(*self.pExample, self.blockSize)
    endif else begin
      newData = make_array(self.blockSize, type=self.type)
    endelse
    newData[0] = *self.pData
    *self.pData = temporary(newData)
  endif
  
  ; add the elements
  case n_elements(position) of
    0 : begin
      (*self.pData)[self.nUsed] = elements
      self.nUsed += nNew
    end
    1 : begin
      ; shift down any elements to the right of position
      if (position lt self.nUsed) then begin
        (*self.pData)[position+nNew] = (*self.pData)[position:self.nUsed-1L]
      endif
      (*self.pData)[position] = elements
      self.nUsed += nNew
    end
    else : begin
      for el = 0L, nNew - 1L do begin
        self->add, elements[el], position=position[el]
      endfor
    end
  endcase
end


;+
; Private method to screen for given class(es). Indices returned are indices 
; POSITION (or data array if ALL is set).
; 
; :Private:
;
; :Returns: index array or -1L if none
;
; :Keywords:
;    position : in, optional, type=lonarr 
;       indices of elements to check
;    isa : in, required, type=string/strarr 
;       classes to check objects for
;    count : out, optional, type=long
;       number of matched items
;    all : in, optional, type=boolean 
;       screen from all elements
;-
function mgcoarraylist::isaGet, position=position, isa=isa, all=all, $
                                count=count
  compile_opt strictarr
  
  ; handle the /ALL case separately because I don't want to create a large
  ; index array for POSITION
  if (keyword_set(all)) then begin
    good = bytarr(self.blocksize)
    for i = 0L, n_elements(isa) - 1L do begin
      good or= obj_isa(*self.pData, isa[i])
    endfor
    return, where(good, count)
  endif
  
  nPos = n_elements(position)
  good = bytarr(nPos)
  for i = 0L, n_elements(isa) - 1L do begin
    good or= obj_isa((*self.pData)[position], isa[i])
  endfor
  
  return, where(good, count)
end


;+
; Get elements of the list. 
;
; :Returns: element(s) of the list or -1L if no elements to return
;
; :Keywords:
;    all : in, optional, type=boolean 
;       set to return all elements
;    position : in, optional, type=long/lonarr 
;       set to an index or an index array of elements to return; defaults to 0 
;       if ALL keyword not set
;    count : out, optional, type=integer 
;       set to a named variable to get the number of elements returned by this 
;       function
;    isa : in, optional, type=string/strarr 
;       classname(s) of objects to return; only allowable if list type is 
;       object
;-
function mgcoarraylist::get, all=all, position=position, count=count, isa=isa
  compile_opt strictarr
  on_error, 2

  ; return -1L if no elements
  if (self.nUsed eq 0) then begin
    count = 0L
    return, -1L
  endif

  ; return all the elements
  if (keyword_set(all)) then begin
    count = self.nUsed
    if (self.type eq 11 && n_elements(isa) gt 0) then begin
      ind = self->isaGet(all=all, isa=isa, count=count)
      if (count eq 0) then return, -1L
      return, (*self.pData)[ind]
    endif 
    return, (*self.pData)[0:self.nUsed-1L]
  endif

  ; return first element if ALL or POSITION are not present
  if (n_elements(position) eq 0) then begin
    count = 1L
    if (self.type eq 11 && n_elements(isa) gt 0) then begin
      ind = self->isaGet(position=0, isa=isa, count=count)
      if (count eq 0) then return, -1L
      return, (*self.pData)[ind]
    endif 
    return, (*self.pData)[0]
  endif

  ; make sure POSITION keyword is in valid range
  badInd = where(position lt 0 or position gt (self.nUsed - 1L), nOutOfBounds)
  if (nOutOfBounds gt 0) then begin
    message, 'Position value out of range'
  endif

  ; return elements selected by POSITION keyword
  count = n_elements(position)
  if (self.type eq 11 && n_elements(isa) gt 0) then begin
    ind = self->isaGet(position=position, isa=isa, count=count)
    if (count eq 0) then return, -1L
    return, (*self.pData)[position[ind]]
  endif 
  return, (*self.pData)[position]
end


;+
; Returns the number of elements in the list.
;
; :Returns: long integer
;-
function mgcoarraylist::count
  compile_opt strictarr

  return, self.nUsed
end


;+
; Creates an iterator to iterate through the elements of the ArrayList. The 
; destruction of the iterator is the responsibility of the caller of this 
; method.
;
; :Returns: MGcoArrayListIterator object
;-
function mgcoarraylist::iterator
  compile_opt strictarr

  return, obj_new('MGcoArrayListIterator', self)
end


;+
; Cleanup list resources.
;-
pro mgcoarraylist::cleanup
  compile_opt strictarr

  ; if data is objects, free them
  if (self.type eq 11) then obj_destroy, *self.pData
  
  ptr_free, self.pExample, self.pData
end


;+
; Create a list.
;
; :Returns: 1B for succes, 0B otherwise
;-
function mgcoarraylist::init, type=type, example=example, block_size=blockSize
  compile_opt strictarr
  on_error, 2

  self.nUsed = 0L

  ; set type
  self.type = n_elements(type) eq 0 ? size(example, /type) : type
  if (self.type eq 0) then message, 'List type is undefined'

  ; set blockSize
  self.blockSize = n_elements(blockSize) eq 0 ? 1000L : blockSize
  if (self.blockSize le 0) then message, 'List size must be positive'

  ; create the list elements -- structures are special
  if (self.type eq 8) then begin 
    if (n_elements(example) eq 0) then begin
      message, 'Structure lists must specify type with EXAMPLE keyword'
    endif
    data = replicate(example, self.blockSize)
    self.pExample = ptr_new(example)
  endif else begin
    data = make_array(self.blockSize, type=self.type)
  endelse
  
  self.pData = ptr_new(data, /no_copy)

  return, 1B
end


;+
; Define member variables.
; 
; :Categories: object, collection
; 
; :Fields:
;    pData 
;       pointer to the data array
;    nUsed 
;       number of elements of the list actually in use
;    type 
;       SIZE type code of the data array
;    blockSize 
;       size of the data array
;    pExample 
;       used if list of structures to specify the structure
;-
pro mgcoarraylist__define
  compile_opt strictarr

  define = { MGcoArrayList, inherits MGcoAbstractList, $
             pData: ptr_new(), $
             nUsed: 0L, $
             type: 0L, $
             blockSize: 0L, $
             pExample: ptr_new() $
           }
end
