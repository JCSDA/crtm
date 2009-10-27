;+
; NAME:
;   PROFREE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Free the pointers associated with an PRODIS abstract syntax tree
;
; CALLING SEQUENCE:
;   PROFREE, TREE
;   
; DESCRIPTION: 
;
;   PROFREE frees the memory and pointers associated with an abstract
;   syntax tree, as returned by PRODIS.  Users should use this
;   procedure when they are finished with an abstract syntax tree and
;   want to release its resources.  The procedure frees all pointers
;   in the tree recursively.
;
; INPUTS:
;
;   TREE - the abstract syntax tree to be freed.  Upon return the
;          contents of TREE will be undefined.
;  
;
; SEE ALSO:
;
;   PRODIS, PROREND, CMSAVEDIR, CMSVLIB
;
; MODIFICATION HISTORY:
;   Written, 2000-2002, CM
;   Documented, 19 Mar 2002, CM
;   
;
; $Id$
;
;-
; Copyright (C) 2000-2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro profree, tree
  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, '  PROFREE, TREE', /info
      return
  endif

  if n_elements(tree) EQ 0 then return

  sz = size(tree)
  if sz(sz(0)+1) NE 8 then return

  for i = 0, n_elements(tree)-1 do begin
      if tag_names(tree(i), /structure_name) EQ 'PDS_NODE' then begin
          for j = 0, 2 do begin
              if ptr_valid(tree(i).operands(j)) then $
                if n_elements(*tree(i).operands(j)) GT 0 then $
                profree, *tree(i).operands(j)
          endfor
          ptr_free, tree(i).operands
      endif
  endfor
  tree = 0
  dummy = temporary(tree)
end
