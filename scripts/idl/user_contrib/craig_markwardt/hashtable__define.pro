;+
; CLASS_NAME:
;	HASHTABLE
;
; PURPOSE:
;	A hash table class which associates key strings with arbitrary values
;
; CATEGORY:
;	Data Structures
;
; SUPERCLASSES:
;       None.
;
; SUBCLASSES:
;       This class has no subclasses.
;
; CREATION:
;       See HASHTABLE::INIT
;
; DESCRIPTION:
;
;       This is a hash table class.  With this data structure, users
;       can associate arbitrary values (scalars, arrays, structures,
;       objects, etc) with a scalar string "key."  The hash table is a
;       collection of (key,value) associations.  Users may dynamically
;       add and remove entries.
;
;       Upon initialization, users may choose the size of the hash
;       table.  This size should be larger than the expected number of
;       entries in the table.  Regardless of the size of the table, an
;       essentially unlimited number of entries may be stored.
;
;       Duplicate keys may be allowed or disallowed, depending on the
;       NO_DUPLICATES keyword to the initialization method.
;
;
; METHODS:
;       Intrinsic Methods
;       This class has the following methods:
;
;       HASHTABLE::CLEANUP removes an existing hash table object
;       HASHTABLE::INIT    initializes a new hash table object
;       HASHTABLE::ADD     adds a new entry to an existing hash table object
;       HASHTABLE::COUNT   returns the number of entries in a hash table
;       HASHTABLE::REMOVE  removes an entry from an existing hash table
;       HASHTABLE::ISCONTAINED is a KEYNAME contained within a hash table?
;       HASHTABLE::GET     returns value associated with KEYNAME in hash table
;       HASHTABLE::KEYS    returns all the keys in an existing hash table
;       HASHTABLE::STRUCT  returns hash table, converted to a structure
;
;
; MODIFICATION HISTORY:
; 	Written and documented, Nov 2003, CM
;       Adjusted ::STRHASHVAL to accomodate possible overflow
;         exceptions, Apr 2004, CM
;       Enhanced ::STRHASHVAL to accept empty strings, 03 Jul 2006, CM
;         (thanks to William Dieckmann)
;       "Fixed" the empty-string problem yet again, 23 Oct 2006, CM
;         (thanks again to William Dieckmann)
;       Decrement COUNT variable after deleting keys, 09 Mar 2007, CM
;       Make ::REMOVE more efficient by using WHERE(COMPLEMENT=), 
;         12 Jun 2007, CM
;       Change array notation to square brackets and enforce with
;         compiler option, 15 Jun 2007, CM
;       Add user-defined "null" value for missing elements, 
;         15 Jun 2007, CM
;       Convert to [] array index notation, 20 Jun 2007, CM
;       Change the two WHERE's in ::REMOVE to a single WHERE
;         with COMPLEMENT, 20 Jun 2007, CM
;       Fix glaring bug in ::REMOVE when an entry still exists,
;         in a bucket, 27 Jun 2007, CM
;       Clean up the new NULL_VALUE pointer when destroying object,
;         (thanks to I. Zimine) 30 Jul 2007, CM
;       Fix case where user stores many identical keys (more than
;         LENGTH), 09 Aug 2007, CM
;       Add POSITION keyword to ::REMOVE, 12 Nov 2008, CM
; 
;  $Id$
;-
; Copyright (C) 2003, 2004, 2006, 2007, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-


;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::INIT
;
; PURPOSE:
;       Creates a hash table object.
;
; CALLING SEQUENCE:
;
;       result = obj_new('hashtable', [LENGTH=length,] [NULL_VALUE=null])
;
; DESCRIPTION:
;
;       The INIT method creates a new hash table object and
;       initializes it.  The user can chose the initial size of the
;       hashtable, which should be comparable to the number of entries
;       expected.
;
; OPTIONAL INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       LENGTH - The number of "buckets" in the hash table, i.e. then
;                number of unique hash values.  This size is fixed
;                once the table is created, however since a bucket can
;                contain more than one entry, this is not a
;                fundamental limitation.
;
;       NO_DUPLICATES - If set, then duplicate entries are not allowed
;                       in the hash table.
;
;       NULL_VALUE - a custom "null" value which is returned when
;                    GET() does not find an entry.
;                    Default: 0L
;
; RETURNS:
;       A new hash table object.
;
; EXAMPLE:
;       ht = obj_new('hashtable')
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-


function hashtable::init, length=length0, no_duplicates=nodups, $
                  null_value=null0,_EXTRA=extra

  COMPILE_OPT strictarr
  if n_elements(length0) EQ 0 then length = 1229L $
  else length = floor(length0[0])
  if n_elements(null0) EQ 0 then null = 0L $
  else null = null0[0]

  self.length = length
  self.count = 0
  self.table = ptr_new(ptrarr(length))
  self.flags = 0L
  self.free_keys = 0
  self.free_values = 0
  self.null_value = ptr_new(null)

  self.flags = (self.flags OR (keyword_set(nodups)*1L))

  return, 1
end

pro hashent__define
  COMPILE_OPT strictarr
  he = {hashent, $
        hashval: 0UL, $
        length: 0l, $
        key: '', $
        value: ptr_new()}
  return
end


;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::CLEANUP
;
; PURPOSE:
;       De-allocates storage and cleans up a hash table object.
;
; CALLING SEQUENCE:
;
;       OBJ_DESTROY, ht
;       
; DESCRIPTION:
;
;       This procedure performs all clean-up required to remove the
;       object.  All hash table entries are freed.  However, if any of
;       the contained objects are heap data or objects, the user is
;       responsible for freeing those pointers or objects.
;
; OPTIONAL INPUTS:
;       None.
;       
;
; KEYWORD PARAMETERS:
;       None.
;       
;
; EXAMPLE:
;       OBJ_DESTROY, ht
;       
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

pro hashtable::cleanup
  COMPILE_OPT strictarr
  ht = self.table
  if ptr_valid(ht) then begin
      ht = *ht
      sz = size(ht)
      if sz[sz[0]+1] EQ 10 then begin
          wh = where(ptr_valid(ht) EQ 1, ct)
          if ct GT 0 then begin
              for i = 0L, ct-1 do begin
                  ptr_free, (*ht[wh[i]]).value
              endfor
          endif
          ptr_free, ht
      endif
      ptr_free, self.table
      self.table = ptr_new()
  endif

  ;; Free the null value
  if ptr_valid(self.null_value) then ptr_free, self.null_value
  self.null_value = ptr_new()

  return
end

function hashtable::bucket, hashval
  COMPILE_OPT strictarr
  return, hashval MOD (self.length)
end

function hashtable::strhashval, str, radix=k0

  COMPILE_OPT strictarr
  ;; Just picked a nice big prime number
  if n_elements(k0) EQ 0 then k = 17293UL else k = ulong(floor(k0[0])>1)

  b = ulong(byte(str))  ;; Convert string(s) to integer

  nstr = n_elements(str) ;; Number of strings
  len = strlen(str) > 1  ;; Lengths of strings
  nchar = n_elements(b[*,0]) ;; Max number of chars per string

  kn = k^(nchar-1-lindgen(nchar))  ;; factor raised to kth power

  hashval = ulonarr(nstr)
  mask32 = (ishft(1ULL,+32)-1)
  for i = 0L, nstr-1 do begin
      vec = kn[nchar-len[i]:*]*b[*,i]
      hashval[i] = ulong64(total(vec, /double)) AND mask32
  endfor

  sz = size(str)
  if sz[0] EQ 0 then return, hashval[0]
  return, hashval
end


;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::ADD
;
; PURPOSE:
;       Add an entry to a hash table.
;
; CALLING SEQUENCE:
;       HT->ADD, KEYNAME, VALUE, HASHVAL=HASHVAL
;       
; DESCRIPTION:
;
;       This method adds a new hash association to an existing hash
;       table.  The hash table associates VALUE with the scalar string
;       KEYNAME.
;
; INPUTS:
;
;       KEYNAME - a scalar string which identifies the value.
;
; KEYWORD PARAMETERS:
;
;       HASHVAL - Use for performance.  If defined upon input,
;                 specifies the hash value for this KEYNAME.  If not
;                 defined upon input, the hash value is computed
;                 internally.  Upon output, the hash value used is
;                 returned in this variable.
;
; EXAMPLE:
;
;       HT->ADD, 'X', 1
;       HT->ADD, 'Y', 2
;       struct = {psym: 3, xtitle: 'Time', ytitle: 'Value'}
;       HT->ADD, 'extra', struct
;
;       Adds the ('X',1), ('Y',2) and ('extra', STRUCT) pairs to the
;       HT hash table.
;       
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

pro hashtable::add, key, value, hashval=hashval, position=position0, $
             replace=replace, status=status, errmsg=errmsg

  COMPILE_OPT strictarr
  status = 0

  ;; Compute hash value of key, if it wasn't already computed
  if n_elements(hashval) EQ 0 then hashval = self->strhashval(key)
  nodups = (self.flags AND 1) NE 0
  
  he = {hashent}
  he.hashval = hashval
  he.length = strlen(key)
  he.key = key
  if n_elements(value) GT 0 then $
    he.value = ptr_new(value) $
  else $
    he.value = ptr_new()
  
  bucket = self->bucket(hashval)
  if bucket[0] LT 0 OR bucket[0] GT self.length then $
    message, 'ERROR: hash value is out of bounds'
  
  list = (*self.table)[bucket]
  if ptr_valid(list) then begin
      list = *list
      if (keyword_set(replace) OR nodups) then begin
          ;; Replace the entry if found
          wh = where(key EQ list.key, ct)

          ;; Check against unwanted duplicates
          if ct GT 0 AND nodups AND keyword_set(replace) EQ 0 then begin
              message, 'ERROR: could not add duplicate hash entry'
              return
          endif

          if ct EQ 0 then begin ;; Nope not found...
              ;; ... add the entry to the top of the bucket
              *(*self.table)[bucket] = [he, list]
          endif else begin
              *(list[wh[0]].value) = value
              ptr_free, he.value
          endelse
      endif else begin
          ;; Add the entry to the top of the bucket
          *(*self.table)[bucket] = [he, list]
      endelse
  endif else begin
      (*self.table)[bucket] = ptr_new([he])
  endelse

  self.count = self.count + 1
  status = 1

  return
end

;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::COUNT
;
; PURPOSE:
;       Returns number of entries in the hash table.
;       
;
; CALLING SEQUENCE:
;       CT = HT->COUNT()
;
; KEYWORD PARAMETERS:
;       None.
;
; RETURNS:
;       The number of entries.
;
; EXAMPLE:
;       CT = HT->COUNT()
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

function hashtable::count
  COMPILE_OPT strictarr
  return, self.count
end

;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::REMOVE
;
; PURPOSE:
;       Removes a hash table entry from an existing hash table object.
;
; CALLING SEQUENCE:
;       HT->REMOVE, KEYNAME
;
; DESCRIPTION:
;
;       This method removes one or more hash entries from an existing
;       hash table.  Entries whose key matches KEYNAME are removed.
;
;       If KEYNAME does not exist, then REMOVE returns silently.
;
;       If multiple entries with the same KEYNAME exist, then they are
;       all deleted by default, unless the POSITION keyword is set.
;       After deleting some entries, positions of the remaining
;       entries may shift.
;
; INPUTS:
;       KEYNAME - a scalar string to be removed from the hash table.
;
; KEYWORD PARAMETERS:
;
;       HASHVAL - Use for performance.  If defined upon input,
;                 specifies the hash value for this KEYNAME.  If not
;                 defined upon input, the hash value is computed
;                 internally.  Upon output, the hash value used is
;                 returned in this variable.
;
;       COUNT - The number of hash entries removed.
;
;       POSITION - if more than one entry was found, then POSITION is
;                  a list of indices to delete (indices start at 0).
;                  IMPORTANT NOTE: out of bounds values are allowed,
;                  and will be rounded to in-bounds values.
;
; EXAMPLE:
;       HT->REMOVE, 'X'
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

pro hashtable::remove, key, hashval=hashval, count=ct, all=all, $
             position=position

  COMPILE_OPT strictarr
  ;; Compute hash value of key, if it wasn't already computed
  if n_elements(hashval) EQ 0 then hashval = self->strhashval(key)

  ct = 0L

  bucket = self->bucket(hashval)

  list = (*self.table)[bucket]
  if ptr_valid(list) EQ 0 then return

  list = *list

  hashvals = list.hashval
  nkeys = n_elements(hashvals)
  ;; WH contains the indices of all entries that match HASHVAL
  ;;   (and therefore should be deleted)
  ;; WHG contains the indices of all entries that don't match HASHVAL
  ;;   (and therefore should be kept)
  wh = where(hashval EQ hashvals AND key EQ list.key, ct, $
             complement=whg, ncomplement=ctg)
  if ct EQ 0 then return

  ;; Filter by POSITION
  if n_elements(position) GT 0 then begin
      mask = bytarr(ct)
      mask[position] = 1
      wh1 = where(mask, ct1, complement=whg1, ncomplement=ctg1)
      if ct1 EQ 0 then return

      ;; If our position filter was a partial hit, then
      ;; sweep the unhit ones to the "keep" list.
      if ctg1 GT 0 then begin
          whg = [whg, wh[whg1]]  ;; Add to keep list
          ctg = n_elements(whg)
          wh = wh[wh1]           ;; Remove from delete list
          ct = n_elements(wh)
      endif
  end

  if ctg EQ 0 then begin
      ;; No good entries left; clear the bucket
      ptr_free, list.value
      ptr_free, (*self.table)[bucket]
      (*self.table)[bucket] = ptr_new()
  endif else begin
      ;; Remove the entry from this bucket's list
      ptr_free, list[wh].value
      *((*self.table)[bucket]) = list[whg]
  endelse

  self.count = self.count - ct
  
  return
end


;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::ISCONTAINED
;
; PURPOSE:
;       Is a hash entry KEYNAME is contained by the hash table?
;
; CALLING SEQUENCE:
;       INSIDE = HT->ISCONTAINED(KEYNAME, COUNT=count, HASHVAL=HASHVAL,
;                                VALUE=value, POSITION=position)
;       
; DESCRIPTION:
;
;      This method determines whether a key is contained within the
;      hash table.  A return value of 1 indicates YES, 0 indicates NO.
;
;      If the key is found, then the value associated with that key
;      can be returned in the VALUE keyword.  If more than one entry
;      with the same key are found, then POSITION determines which
;      value is returned.
;      
; INPUTS:
;       KEYNAME - a scalar string, the key name to be searched for.
;
; KEYWORD PARAMETERS:
;
;       COUNT - upon return, the number of hash entries which match
;               KEYNAME.
;
;       VALUE - upon return, if KEYNAME was found, the value
;               associated with that key.  If more than one keys
;               match, then by default the first entry is returned,
;               unless POSITION is specified.  If the key is not
;               found, then VALUE is undefined.
;
;       POSITION - if KEYNAME was found, and more than one entry was
;                  found, then the POSITION'th entry is returned in
;                  VALUE (the index starts at 0).
;
;       HASHVAL - Use for performance.  If defined upon input,
;                 specifies the hash value for this KEYNAME.  If not
;                 defined upon input, the hash value is computed
;                 internally.  Upon output, the hash value used is
;                 returned in this variable.
;
; RETURNS:
;       Is the key contained within the table?  (Scalar integer:
;          1=YES, 0=NO)
;       
; EXAMPLE:
;       if HT->ISCONTAINED('X') EQ 1 then print, 'X found'
;
;       if HT->ISCONTAINED('X', VALUE=xvalue) then begin
;           oplot, xvalue
;       endif
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

function hashtable::iscontained, key, value=value, $
                  hashval=hashval, errmsg=errmsg, $
                  count=ct, position=index0

  COMPILE_OPT strictarr
  ;; Compute hash value of key, if it wasn't already computed
  if n_elements(hashval) EQ 0 then hashval = self->strhashval(key)

  ct = 0L
  value = 0 & dummy = temporary(value)

  bucket = self->bucket(hashval)

  if bucket LT 0 OR bucket GT self.length then return, 0

  list = (*self.table)[bucket]
  if ptr_valid(list) EQ 0 then return, 0

  list = *list

  hashvals = list.hashval
  wh = where(key EQ list.key, ct)
  if ct EQ 0 then return, 0

  if ct EQ 1 then begin
      he = list[wh[0]]
      if arg_present(value) then value = *(he.value)
  endif else begin
      if n_elements(index0) EQ 0 then index = 0L $
      else index = floor(index0[0])

      index = index > 0 & index = index < (ct-1)

      if arg_present(value) then value = *(list[wh[index]].value)
  endelse
  
  return, 1
end

;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::GET
;
; PURPOSE:
;       Retrieves a value associated with a key from the hash table.
;       
;
; CALLING SEQUENCE:
;       VALUE = HT->GET('X', COUNT=count, POSITION=position, HASHVAL=hashval)
;       
; DESCRIPTION:
;
;       This method searches for the requested key in the hash table,
;       and returns the value associated with that key.
;
;       If more than one entry with the same key are found, then
;       POSITION determines which value is returned.
;
;       If the key is not found, then COUNT is set to zero, and the
;       returned value is undefined.
;
;
; KEYWORD PARAMETERS:
;
;       COUNT - upon return, the number of hash entries which match
;               KEYNAME.
;
;       POSITION - if KEYNAME was found, and more than one entry was
;                  found, then the POSITION'th entry is returned in
;                  VALUE (the index starts at 0).
;
;                  WARNING: if the hash table has been changed using
;                  ADD or REMOVE, then the order of elements in the
;                  table may shift.
;
;       HASHVAL - Use for performance.  If defined upon input,
;                 specifies the hash value for this KEYNAME.  If not
;                 defined upon input, the hash value is computed
;                 internally.  Upon output, the hash value used is
;                 returned in this variable.
;
; OUTPUTS:
;
;       The value associated with KEYNAME is returned.  If KEYNAME was
;       not found, then COUNT is zero and the return value is
;       set to the "null" value (see ::INIT).
;       
;
; EXAMPLE:
;
;       X = HT->GET('X')
;       
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

function hashtable::get, key, hashval=hashval, $
                  count=ct, position=index0

  COMPILE_OPT strictarr
  ct = 0L
  
  keyfound = self->iscontained(key, hashval=hashval, value=value, $
                               count=ct, position=index0)
  if keyfound EQ 0 then return, *(self.null_value)
  
  return, value
end

;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::KEYS
;
; PURPOSE:
;       Retrieves all the keys of the hash tables
;       
;
; CALLING SEQUENCE:
;       KEYS = HT->KEYS()
;       
; DESCRIPTION:
;
;       This method returns all of the keys in the hash table.  If
;       duplicate hash entries are present, then a key may appear more
;       than once.
;
;       The order of the keys is undefined.
;
; KEYWORD PARAMETERS:
;       None.
;
; RETURNS:
;
;       A string array containing the keys of this hash table.
;
; EXAMPLE:
;
;       KEYS = HT->KEYS()
;       for i = 0, n_elements(keys)-1 do print, ht->get(keys(i))
;       
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

function hashtable::keys, count=ct, _EXTRA=extra

  COMPILE_OPT strictarr
  table = *(self.table)
  wh = where(ptr_valid(table), ct)
  if ct EQ 0 then return, 0

  keys = ['']
  for i = 0L, ct-1 do begin
      bucket = *(table[wh[i]])
      keys = [keys, bucket.key]
  endfor

  keys = keys[1:*]
  
  return, keys
end

;+
; =============================================================
;
; METHODNAME:
;       HASHTABLE::STRUCT
;
; PURPOSE:
;       Converts the hash table to an equivalent IDL structure
;       
;
; CALLING SEQUENCE:
;       STRUCT = HT->STRUCT()
;       
; DESCRIPTION:
;
;       This method converts the hash table into an equivalent IDL
;       structure.  One structure tag appears for each hash entry.
;
;       WARNING: (1) the hash keys must be valid IDL structure names;
;       (2) there must be no duplicate keys in the hash table.
;
;       The order of the keys is undefined.
;
; KEYWORD PARAMETERS:
;       None.
;
; RETURNS:
;
;       A structure.
;
; EXAMPLE:
;
;       HTSTRUCT = HT->STRUCT()
;       help, keys, htstruct
;       
;
; MODIFICATION HISTORY:
;       Written and documented, Nov 2003, CM
; 	
;-

function hashtable::struct, count=ct, _EXTRA=extra
  
  COMPILE_OPT strictarr
  table = *(self.table)
  wh = where(ptr_valid(table), ct)
  if ct EQ 0 then return, 0

  for i = 0L, ct-1 do begin
      bucket = *(table[wh[i]])
      for j = 0, n_elements(bucket.key)-1 do begin
          if n_elements(str) EQ 0 then $
            str = create_struct(bucket[j].key, *(bucket[j].value)) $
          else $
            str = create_struct(str, bucket[j].key, *(bucket[j].value))
      endfor
  endfor

  return, str
end

; =============================================================
; METHODNAME: HASHTABLE__DEFINE
;  internal method: defines hash table data structure
pro hashtable__define

  COMPILE_OPT strictarr
  struct = {hashtable, $
            table: ptr_new(), $  ;; Table of HASHENT structures
            length: 0L, $        ;; Number of buckets in table
            count: 0L, $         ;; Number of entries in table
            flags: 0L, $         ;; Flags
            free_keys: 0L, $
            free_values: 0L, $
            null_value: ptr_new() $
           }
  return
end


