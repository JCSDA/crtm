;+
; NAME:
;   FXPSEEK
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Internal routine to perform seek on a Unix Pipe.
;
; DESCRIPTION:
;
;   See the following procedures for appropriate documentation.
;
;     FXGOPEN  - open resource
;     FXGCLOSE - close resource
;     FXGREAD  - read from resource
;     FXGWRITE - write to resource
;     FXGSEEK  - seek on resource (i.e., perform POINT_LUN)
;
;     FXGFILTERED - determine if resource is a normal file.
;
; PARAMETERS
;
;   unit - the pipe LUN.  If positive, then the file pointer is moved
;          to POSITION in the pipe output.  [ In reality the file
;          pointer is moved in the cache file. ]  If negative, then
;          the file pointer of the file unit -LUN is returned in
;          POSITION.
;
;   position - the file pointer, either passed or returned as defined
;              by UNIT.
;
; Side Effects
;
;   The file pointer may be updated.
;   Actual file accesses are postponed until needed (ie, when FXPREAD
;   is called).
;
; MODIFICATION HISTORY:
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

PRO FXPSEEK, UNIT, POSITION

@fxpcommn

  IF UNIT GT 0 THEN BEGIN
      POINT_LUN, CACHE_UNIT(UNIT), POSITION
      POINTER(UNIT) = POSITION
  ENDIF ELSE BEGIN
      POINT_LUN, -CACHE_UNIT(-UNIT), POSITION
      POINTER(-UNIT) = POSITION
  ENDELSE

  RETURN
END
