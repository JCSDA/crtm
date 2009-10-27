;+
; NAME:
;   DXCOMMON
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Defines DEBUG_LEVEL common block (INTERNAL)
;
; DESCRIPTION: 
;
;   This code fragment defines the DEBUG_LEVEL common block.  This
;   common is internal to the debugging procedures.
;
;  $Id$
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;
; Define function names just to be sure
;
forward_function routine_names, routine_info

;
; Define DEBUG_LEVEL common block
;
common debug_level, dblevel, dbtraceback

;
; Set common block values 
;
if n_elements(dblevel) EQ 0 then begin
  dblevel = 0L
  dbtraceback = ['']
endif
