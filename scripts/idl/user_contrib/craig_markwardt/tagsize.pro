;+
; NAME:
;   TAGSIZE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute SIZE descriptors for each tag in a structure
;
; CALLING SEQUENCE:
;   SIZES = TAGSIZE(STRUCT, N_TAGS=ntags, TAG_NAMES=tagnames,
;                   STRUCTURE_NAME=structname, STATUS=status, LENGTH=length)
;
; DESCRIPTION: 
;
;   The TAGSIZE function determines the types and sizes of each tag in
;   a structure.  This is not as easy as it may seem, because IDL
;   makes it very, *very* difficult to find out the true dimensions of
;   a structure element.
;
;   Here is a brief explanation.  It is known that IDL drops trailing
;   dimensions of size 1 in many situations.  Also, for structures
;   only, arrays of any dimensionality which have only one element are
;   RETURNED AS SCALARS.  Thus, if you are doing any heavy duty work
;   with structures, copying and querying individual elements between
;   structures, etc., you will find that you will lose some crucial
;   dimensions which you can't normally regain.
;
;   TAGSIZE attempts to work around all of these limitations to
;   present the true dimensions of all elements in a structure.
;
;   It returns an 11xNTAGS array, which contains a SIZE-style vector
;   for each element in the structure.  Eleven elements is the largest
;   array size needed to describe any IDL data type using SIZE.  Thus,
;   to extract information about the second tag in structure X
;   (element number 1 starting from zero), you would use the following
;   code:
;
;     SIZES = TAGSIZE(X)  ;; Extract type information from structure X
;     SIZE_1 = SIZES(*,1) ;; Extract type information about the 2nd element
;
;     SIZE_1 = SIZE_1(0:SIZE_1(0)+2) ;; Trim the array if desired
;
;   The last command is optional, but trims the resulting array to be
;   a true SIZE-style result.
;     
;   TAGSIZE also has several convenience keywords to extract other
;   relevant information about a structure.
;
;
; INPUTS:
;
;   STRUCTURE - any structure to examine.  If the value is not a
;               structure then an error is reported.
;
; KEYWORDS:
;
;   N_TAGS - upon return, the number of tags in the structure is
;            stored in this keyword.
;
;   TAG_NAMES - upon return, the names of each tag are stored in this
;               keyword, as an array of strings.
;
;   STRUCTURE_NAME - upon return, the name of the structure is stored
;                    in this keyword.  If the structure is anonymous
;                    then the empty string ('') is returned.
;
;   LENGTH - upon return, the size in bytes of each tag element in the
;            structure is stored in this keyword, as an array of
;            integers.
;
;   STATUS - upon return, the status is stored in this keyword.  A
;            value of 1 indicates success, 0 indicates failure.
;
;
; RETURNS:
;
;   A two dimensional array, with dimensions LONARR(11,NTAGS),
;   containing the size information of all tag elements in the
;   structure.  SIZES(*,i) is the SIZE-style vector for tag element i.
;
; EXAMPLE:
;
;   Compute the sizes of the elements in X, defined here.
;     IDL> x = {a: [1], b: intarr(2,2), c: reform(strarr(2,1),2,1)}
;     IDL> help, /struct, x
;     ** Structure <818c8b4>, 3 tags, length=28, refs=1:
;        A               INT       Array[1]
;        B               INT       Array[2, 2]
;        C               STRING    Array[2, 1]
;     IDL> print, byte(tagsize(x))
;        1  [1]  2   1   0   0   0   0   0   0   0
;        2  [2   2]  2   4   0   0   0   0   0   0
;        2  [2   1]  7   2   0   0   0   0   0   0
;     [ Array dimensions are emphasized with brackets ]
;
;   Compare this to the type information returned by HELP, which is
;   incorrect for tags A and C.
;     IDL> help, x.a, x.b, x.c
;     <Expression>    INT       =        1
;     <Expression>    INT       = Array[2, 2]
;     <Expression>    STRING    = Array[2]
;
; SEE ALSO:
;
;   TAG_NAMES, N_TAGS, SIZE, HELP, INPUTFORM, HELPFORM
;
; MODIFICATION HISTORY:
;   Written, CM, 13 May 2000
;   Documented, 05 Jul 2000
;   Small documentation changes, CM, 31 Aug 2000
;   Signficant cleanup of HELP parsing, CM, 04 Dec 2000
;   Added case for array of structures with new parsing, CM 12 Jan
;     2001
;
;  $Id$
;
;-
; Copyright (C) 2000-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function tagsize, structure, n_tags=ntags, tag_names=tnames, $
                  structure_name=sname, status=status, nocatch=nocatch, $
                  length=length

  ;; Error checking
  status = 0
  on_error, 2
  if n_elements(structure) EQ 0 then $
    message, 'ERROR: parameter must be of type STRUCTURE'
  sz = size(structure)
  if sz(sz(0)+1) NE 8 then $
    message, 'ERROR: parameter must be of type STRUCTURE'

  ;; Take only the first element, if STRUCTURE is an array
  st = structure(0)
  ;; Extract "help" style output 
  if float(!version.release) GE 5.0 then begin
      help, st, /struct, output=hh
      hh = strtrim(hh, 2)+' '
  endif

  ;; Get some generic information our users are too lazy to get
  ;; themselves.  We need some of it anyway.
  tnames = tag_names(st)
  ntags  = n_elements(tnames)
  sname  = tag_names(st, /structure_name)
  
  ;; Output array
  ssz = reform(lonarr(11, ntags), 11, ntags, /overwrite)
  length = lonarr(ntags)

  nhelp = n_elements(hh)
  ;; j is the index into hh, the HELP output
  j = 0L  
  for i = 0L, ntags-1 do begin
      ;; Get some basic information
      sz = size(st.(i))
      ndims = sz(0)
      ;; Cheap way to get the number of bytes of element i
      len = n_tags({test:st.(i)}, /length)  
      
      ;; Arghh, this is bad!  Arrays with trailing dimensions of
      ;; length 1 are hard to detect.  This almost does it.  What this
      ;; tries to do is double the dimensions of a value, which breaks
      ;; the trailing-ones degeneracy problem.  The error checking is
      ;; for when we have an 8-dimensional array (!).  Unfortunately,
      ;; it doesn't help if st.(i) has only one element, as evidence
      ;; by the next load of crap.
      catcherr = 0
      if NOT keyword_set(nocatch) then catch, catcherr
      if catcherr EQ 0 then xsz = size(st([0,0]).(i)) $
      else                  xsz = [9L]
      catch, /cancel
      ndims = xsz(0) - 1

      ;; Check for structures, always an array
      if sz(sz(0)+1) EQ 8 AND ndims EQ 0 then ndims = 1
      if sz(sz(0)+2) GT 1 AND ndims EQ 0 then ndims = 1

      ;; Arrgggghhhh!  It gets even worse, since for some reason
      ;; one-element array tags are converted to scalars when
      ;; extracted.  We then parse the output of HELP, which only
      ;; works for IDL 5.
      if ndims EQ 0 AND nhelp GT 0 then begin

          ;; Scan to find the name of the variable of interest
          wh = (where(strpos(hh, tnames(i)+' ') EQ 0, ct))(0)
          if ct EQ 0 then $
            message, 'ERROR: structure composition was inconsistent (1)'

          ;; Combine the next two lines, removing = signs
          h1 = hh(wh) + hh((wh+1)<(nhelp-1))
          while strpos(h1, '=') GE 0 do strput, h1, ' ', strpos(h1, '=')
          h2 = str_sep(strcompress(h1), ' ')
          n = n_elements(h2)

          ;; Locate the tag in the help output
          if n LT 3 then $
            message, 'ERROR: structure tag '+tnames(i)+' not found'

          ;; Extract the dimensions
          sdims = ''
          ii = 2
          if strupcase(strmid(h2(ii),0,5)) EQ 'ARRAY' then begin
              DO_ARRAY:
              slen = strlen(h2(ii)) - 7   ;; Remove "array[]"
              sdims = strmid(h2(ii),6,slen)
              if sdims EQ '' then goto, DONE_HELP

              dims = 0 & dummy = temporary(dims)
              if execute('dims = ['+sdims+']') EQ 0 then goto, DONE_HELP

              ;; Sanity check
              if sz(sz(0)+2) NE total(dims) then $
                message, 'ERROR: structure composition was inconsistent (2)'
              
              ndims = n_elements(dims)
              goto, DONE_HELP
          endif else if h2(2) EQ '->' then begin
              ii = 4
              goto, DO_ARRAY
          endif
            
      endif
      DONE_HELP:
      
      ;; Finally, FINALLY, assemble this information into a SIZE-style
      ;; vector.
      ssz(0,i) = ndims
      if ndims GT 0 then ssz(1:ndims,i) = 1     ;; Pre-fill with ones
      if sz(0) GT 0 then ssz(1,i) = sz(1:sz(0)) ;; Put non-one dimensions
      ssz(ndims+1,i) = sz(sz(0)+1)              ;; Put variable type
      ssz(ndims+2,i) = sz(sz(0)+2)              ;; Put n_elements
      length(i) = len                           ;; Length of tag in bytes
  endfor

  status = 1
  return, ssz
end
