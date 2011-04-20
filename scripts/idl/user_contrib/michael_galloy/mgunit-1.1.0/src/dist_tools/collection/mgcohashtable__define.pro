; docformat = 'rst'

;+
; A hash table which can hash any kind of IDL variables. To hash objects, 
; simply make sure each object implements the hashCode method. See the help 
; for the calcHashCode method for details.
;
; :Categories:
;    collection
;
; :Author:
;    Michael D. Galloy
;
; :Examples:
;    Try the main-level example program at the end of this file::
;
;       IDL> .run mgcohashtable__define
;-

;+
; Returns an array with the same number of elements as the hash array.  The
; value each element of the array is the number of elements in that "bin" of
; the mgcohashtable. This could be useful in determining the effectiveness of 
; the hash code calculations.
;
; :Private:
;
; :Returns: 
;    long array
;-
function mgcohashtable::_getHistogram
  compile_opt strictarr
  on_error, 2

  hist = lonarr(self.arraySize)
  for i = 0L, self.arraySize - 1L do begin
    hist[i] = obj_valid((*self.keyArray)[i]) $
                ? (*self.keyArray)[i]->count() $
                : 0L
  endfor

  return, hist
end


;+
; Prints keys and values to a given LUN. Prints to STDOUT if LUN not given.
;
; :Params:
;    lun : in, optional, type=LUN, default=-1
;       logical unit number for output
;-
pro mgcohashtable::print, lun
  compile_opt strictarr

  myLun = n_elements(lun) eq 0 ? -1L : lun

  keys = self->keys(count=count)
  values = self->values()

  if (count gt 0) then begin
    for i = 0L, count - 1L do begin
      printf, myLun, keys[i] + ' -> ' + values[i]
    endfor
  endif
end


;+
; Returns an array of the keys of the hash table.
;
; :Returns: 
;    an array of the keys of the hash table or -1 if no keys
;
; :Keywords:
;    count : out, optional, type=integral
;       number of keys in the hash table
;-
function mgcohashtable::keys, count=count
  compile_opt strictarr
  on_error, 2

  count = self->count()
  if (count le 0L) then return, -1L
  keyArray = make_array(type=self.keyType, count, /nozero)
  idx = 0L

  for i = 0L, self.arraySize - 1L do begin
    list = (*self.keyArray)[i]
    if (obj_valid(list)) then begin
      keyArray[idx] = list->get(/all)
      idx += list->count()
    endif
  endfor

  return, keyArray
end


;+
; Returns an array of the values of the hash table.
;
; :Returns: 
;    an array of the values of the hash table or -1 if no values
;
; :Keywords:
;    count : out, optional, type=integral
;       number of values in the hash table
;-
function mgcohashtable::values, count=count
  compile_opt strictarr
  on_error, 2

  count = self->count()
  if (count le 0L) then return, -1L
  valueArray = make_array(type=self.valueType, count, /nozero)
  idx = 0L

  for i = 0, self.arraySize - 1 do begin
      list = (*self.valueArray)[i]
      if (obj_valid(list)) then begin
        valueArray[idx] = list->get(/all)
        idx += list->count()
      endif
  endfor

  return, valueArray
end


;+
; Calculates the hash code of the given key.  The index of the array element
; the key's value will be stored in will be the hash code value MOD the array
; size.
;
; If a hash tbale of object references is desired, then the objects should
; implement the hashCode method.  This function should accept no parameters 
; and return an unsigned long.
;
; This method should not normally be called directly.
;
; If the given default hash function is not doing well (use the _getHistogram 
; method to find out how well it's spreading out the keys), subclass this 
; class and implement a more appropriate hash function.
;
; :Returns: 
;    hash code (unsigned long integer); 0 if null pointer or object, undefined 
;    variable; or an object that does not implement hashCode
;
; :Params:
;    key : in, type=key type
;       key to find hash code of
;-
function mgcohashtable::_calcHashCode, key
  compile_opt strictarr
  on_error, 2

  ; handles the case if key is an object, but without the hash_code method
  errorNumber = 0L
  catch, errorNumber
  if (errorNumber) then begin
    catch, /cancel
    return, 0UL
  endif

  type = size(key, /type)
  case type of
  0 : return, 0UL
  1 : return, ulong(key)
  2 : return, ulong(key)
  3 : return, ulong(key)
  4 : return, ulong(key)
  5 : return, ulong(key)
  6 : return, ulong(abs(key))
  7 : begin   ; strings
        ascii = ulong(byte(key))
        total = 0UL
        for i = 0, n_elements(ascii) - 1 do begin
            ; 37UL is a magic number suggested by the literature
            total = total * 37UL + ascii[i]
        endfor
        return, total
  end
  8 : begin   ; structures
        hashCode = 0UL
        for tagIndex = 0, n_tags(key) - 1 do begin
          hashCode +=self->_calcHashCode(key.(tagIndex))
        endfor
        return, hashCode
  end
  9 : return, ulong(abs(key))
  10 : return, ptr_valid(key) ? self->_calcHashCode(*key) : 0UL
  11 : return, obj_valid(key) ? key->hashCode() : 0UL
  12 : return, ulong(key)
  13 : return, ulong(key)
  14 : return, ulong(key)
  15 : return, ulong(key)
  endcase
end


;+
; Finds the value associated with the given key.
;
; :Returns: 
;    the value of the associated key or -1L if not found
;
; :Params:
;    key : in, type=key type
;       key to look up
;
; :Keywords:
;    found : out, optional, type=boolean
;       true if value found for given key
;-
function mgcohashtable::get, key, found=found
  compile_opt strictarr
  on_error, 2

  hcode = self->_calcHashCode(key) mod self.arraySize

  found = 0B
  keyIndex = 0L
  if (~obj_valid((*self.keyArray)[hcode])) then begin
    found = 0B
    return, -1L
  endif
  
  iter = (*self.keyArray)[hcode]->iterator()
  while (iter->hasNext()) do begin
    element = iter->next()
    if (element eq key) then begin
      found = 1B
      break
    endif
    keyIndex++
  endwhile

  obj_destroy, iter
  return, found ? (*self.valueArray)[hcode]->get(position=keyIndex) : -1L
end


;+
; Removes the value associated with the given key.
;
; :Params:
;    key : in, type=key type
;       key to look up
;
; :Keywords:
;    found : out, optional, type=boolean
;       true if value found for given key
;-
pro mgcohashtable::remove, key, found=found
  compile_opt strictarr
  on_error, 2

  hcode = self->_calcHashCode(key) mod self.arraySize

  found = 0B
  keyIndex = 0L
  if (~obj_valid((*self.keyArray)[hcode])) then begin
    found = 0B
    return
  endif
  
  iter = (*self.keyArray)[hcode]->iterator()
  while (iter->hasNext()) do begin
    element = iter->next()
    if (element eq key) then begin
      found = 1B
      break
    endif
    keyIndex++
  endwhile

  obj_destroy, iter
  
  if (found) then begin
    (*self.valueArray)[hcode]->remove, position=keyIndex
  endif
end


;+
; Puts the key-value pair into the hash table or updates the value for the key
; if it is already in the hash table.
;
; :Params:
;    key : in, required, type=key type
;       key to place in the table
;    value : in, required, type=value type
;       value to place in the table
;
; :Keywords:
;    found : out, optional, type=boolean
;       pass a named variable that is set to true if the key was already in 
;       the table and is updated
;-
pro mgcohashtable::put, key, value, found=found
  compile_opt strictarr
  on_error, 2

  if (n_params() ne 2L) then message, 'put method requires key and value'
  
  hcode = self->_calcHashCode(key) mod self.arraySize
  if (obj_valid((*self.keyArray)[hcode])) then begin
    found = 0B
    keyIndex = 0L
    iter = (*self.keyArray)[hcode]->iterator()
    while (iter->hasNext()) do begin
      el = iter->next()
      if (el eq key) then begin
        found = 1
        break
      endif
      keyIndex++
    endwhile
    obj_destroy, iter

    if (found) then begin
      (*self.valueArray)[hcode]->remove, position=keyIndex
      (*self.valueArray)[hcode]->add, value, position=keyIndex
    endif else begin
      (*self.keyArray)[hcode]->add, key
      (*self.valueArray)[hcode]->add, value
    endelse
  endif else begin
    found = 0

    if (self.keyType eq 8) then begin  ; type 8 = structure
      if (~ptr_valid(self.keyExample)) then self.keyExample = ptr_new(key)
      (*self.keyArray)[hcode] $
        = obj_new('MGcoArrayList', example=*self.keyExample, block_size=5)
    endif else begin
      (*self.keyArray)[hcode] $
        = obj_new('MGcoArrayList', type=self.keyType, block_size=5)
    endelse
    (*self.keyArray)[hcode]->add, key

    if (self.valueType eq 8) then begin ; type 8 = structure
      if (~ptr_valid(self.valueExample)) then self.valueExample = ptr_new(value)
      (*self.valueArray)[hcode] $
        = obj_new('MGcoArrayList', example=*self.valueExample, block_size=5)
    endif else begin
      (*self.valueArray)[hcode] $
        = obj_new('MGcoArrayList', type=self.valueType, block_size=5)
    endelse
    (*self.valueArray)[hcode]->add, value
  endelse
end


;+
; Find the number of key-value pairs in the hash table
;
; :Returns: 
;    the number of key-value pairs in the hash table
;-
function mgcohashtable::count
  compile_opt strictarr
  on_error, 2

  ; check size of key array only
  size = 0L
  for keyIndex = 0L, n_elements(*self.keyArray) - 1L do begin
    size += obj_valid((*self.keyArray)[keyIndex]) $
              ? (*self.keyArray)[keyIndex]->count() $
              : 0L
  endfor

  return, size
end


;+
; Determines if the hash table is empty.
;
; :Returns: 
;    0 if the table is empty, 1 if it contains any key-value pairs
;-
function mgcohashtable::isEmpty
  compile_opt strictarr
  on_error, 2

  ; search key array only
  for keyIndex = 0L, n_elements(*self.keyArray) - 1L do begin
    testList = (*self.keyArray)[keyIndex]
    if (obj_valid(testList)) then begin
      if (testList->count() gt 0) then return, 0
    endif
  endfor

  return, 1B
end


;+
; Frees hash table resources, but the resources contained by the hash table.
;-
pro mgcohashtable::cleanup
  compile_opt strictarr
  on_error, 2

  obj_destroy, [*self.keyArray, *self.valueArray]
  ptr_free, self.keyExample, self.valueExample, self.keyArray, self.valueArray
end


;+
; Create a hash table.
;
; :Returns: 
;    1 if successful; 0 otherwise
;
; :Keywords:
;    array_size : in, optional, type=integral, default=101
;       the size of the hash table; generally a prime is a good choice
;    key_type : in, type=0-15
;       type code for keys; key_type or key_example must be present
;    value_type : in, type=0-15
;       type code for values; value_type or key_example must be present
;    key_example : in, type=key type
;       example of key type; key_type or key_example must be present
;    value_example : in, type=value type
;       example of value type; value_type or value_example must be present
;-
function mgcohashtable::init, array_size=arraySize, $
                              key_type=keyType, $
                              value_type=valueType, $
                              key_example=keyExample, $
                              value_example=valueExample
  compile_opt strictarr
  on_error, 2

  self.arraySize = n_elements(arraySize) eq 0 ? 101L : arraySize

  if (n_elements(keyType) eq 0) then begin
    if (n_elements(keyExample) eq 0) then begin
      message, 'type of key must be defined with KEY_TYPE or KEY_EXAMPLE'
    endif else begin
      self.keyType = size(keyExample, /type)
    endelse
  endif else begin
    self.keyType = keyType
  endelse

  if (n_elements(valueType) eq 0) then begin
    if (n_elements(valueExample) eq 0) then begin
      message, 'type of value must be defined with VALUE_TYPE or VALUE_EXAMPLE'
    endif else begin
      self.valueType = size(valueExample, /type)
    endelse
  endif else begin
    self.valueType = valueType
  endelse

  self.keyArray = ptr_new(objarr(self.arraySize))
  self.valueArray = ptr_new(objarr(self.arraySize))

  return, 1
end


;+
; Hash table implementation.
;
; :Fields:
;    keyArray 
;       pointer to array of keys; type of array is specified by KEY_TYPE field 
;       for non-structures and by KEY_EXAMPLE field for structures
;    valueArray 
;       pointer to array of values; type of array is specified VALUE_TYPE 
;       field for non-structures and by VALUE_EXAMPLE field for structures
;    arraySize 
;       size of the key and value arrays
;    keyType 
;       SIZE type of keys; if 8 (structures), examine KEY_EXAMPLE to find type 
;       of structure
;    valueType 
;       SIZE type of keys; if 8 (structures), examine VALUE_EXAMPLE to find 
;       type of structure
;    keyExample 
;       pointer to example structure defining the key type
;    valueExample 
;       pointer to example structure defining the value type
;-
pro mgcohashtable__define
  compile_opt strictarr

  define = { MGcoHashTable, $
             keyArray: ptr_new(), $
             valueArray: ptr_new(), $
             arraySize: 0L, $
             keyType: 0L, $
             valueType: 0L, $
             keyExample: ptr_new(), $    ; used for structures
             valueExample: ptr_new() $   ; used for structures
           }
end


; main-level example program

h = obj_new('MGcoHashTable', key_type=7, value_type=3)

h->put, 'Boulder', 80303
h->put, 'Lafayette', 80026
h->put, 'La Porte', 46350

print, h->keys()
print, h->values()
print, h->get('Boulder')

obj_destroy, h

end
