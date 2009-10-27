;+
; NAME:
;   FXGCLOSE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Closes a generic resource
;
; MAJOR TOPICS:
;   File I/O, Pipes, URLs, FITS
;
; CALLING SEQUENCE:
;   FXGCLOSE, UNIT
;
; DESCRIPTION:
;
;   FXGCLOSE closes a generic resource originally opened by FXGOPEN.
;   All associated system resources are freed.
;
;   You must use the specialized 'FXG' style functions to read, write
;   and seek on file units opened with FXGOPEN:
;
;     FXGOPEN  - open resource
;     FXGCLOSE - close resource
;     FXGREAD  - read from resource
;     FXGWRITE - write to resource
;     FXGSEEK  - seek on resource (i.e., perform POINT_LUN)
;
;     FXGFILTERED - determine if resource is a normal file.
;
; INPUTS:
;
;   UNIT - the unit number of the currently open resource.  The unit
;          must have been previously opened by FXGOPEN.
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 02 Oct 1999, CM
;   Changed copyright notice, 21 Sep 2000, CM
;
;  $Id$
;
;-
; Copyright (C) 1999-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
PRO FXGCLOSE, UNIT

  on_error, 2

  if n_params() NE 1 then begin
      message, 'USAGE: FXGCLOSE, UNIT', /info
      return
  endif
  if n_elements(unit) EQ 0 then $
    message, 'ERROR: UNIT is not defined'

@fxfilter
  if unit LT 0 OR unit GE FXFILTER_MAX_LUN then $
    message, 'ERROR: UNIT is not a valid file unit'

  free_lun = 'FREE_LUN'
  if filterflag(unit) AND 1  then $
    if close_cmd(unit) NE '' then free_lun = close_cmd(unit)
  
  call_procedure, free_lun, unit

  FILTERFLAG(UNIT) = 0   ;; Zero out the dispatch entries
  SEEK_CMD(UNIT)   = ''
  READ_CMD(UNIT)   = ''
  WRITE_CMD(UNIT)  = ''
  CLOSE_CMD(UNIT)  = ''

  return
end
