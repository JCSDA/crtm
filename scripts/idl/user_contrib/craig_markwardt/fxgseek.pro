;+
; NAME:
;   FXGSEEK
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Perform a seek operation on a generic resource.
;
; MAJOR TOPICS:
;   File I/O, Pipes, URLs, FITS
;
; CALLING SEQUENCE:
;   FXGSEEK,  UNIT, POSITION   ;; Sets the current file position
;   FXGSEEK, -UNIT, POSITION   ;; Queries the current file position
;
; DESCRIPTION:
;
;   FXGSEEK performs a seek on the selected resource.  Depending on
;   the sign of UNIT, the current file position is either queried or
;   set, in much the same manner as the built-in IDL procedure
;   POINT_LUN.
;
;   If the resource is a stream, the seek operation does not
;   necessarily force a read until FXGREAD is called (i.e., reads are
;   "lazy").
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
;   UNIT - the unit number to operate on.  The unit must have been
;          previously opened by FXGOPEN.  The operation of FXGSEEK
;          depends on the sign of UNIT.  If UNIT is positive, then the
;          current file position of file UNIT is set to POSITION.  If
;          UNIT is negative, then the current file position of file
;          |UNIT| is placed in the variable POSITION.
;
;   POSITION - Depending on the sign of UNIT, the behavior is
;              different.  When UNIT is positive, POSITION is an input
;              variable containing the new file position.  When UNIT
;              is negative, POSITION is an output variable to contain
;              the file's current file position.
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
PRO FXGSEEK, UNIT, POSITION

  on_error, 2

  if n_params() EQ 0 then begin
      message, 'USAGE: FXGSEEK, UNIT, POSITION', /info
      return
  endif
  if n_elements(unit) EQ 0 then $
    message, 'ERROR: UNIT is not defined'
  if unit GT 0 AND n_elements(position) EQ 0 then $
    message, 'ERROR: POSITION is not defined'

@fxfilter
  if unit LT -FXFILTER_MAX_LUN OR unit GE FXFILTER_MAX_LUN then $
    message, 'ERROR: UNIT is not a valid file unit'
  unit0 = abs(unit(0))

  point_lun = 'POINT_LUN'
  if filterflag(unit0) AND 1  then $
    if seek_cmd(unit0) NE '' then point_lun = seek_cmd(unit0)
  if point_lun EQ '-' then begin
      errmsg = string(unit, $
               format='("ERROR: Resource unit ",I0," does not support seeking.")')
      message, errmsg
  endif
  
  call_procedure, point_lun, unit, position

  return
end
