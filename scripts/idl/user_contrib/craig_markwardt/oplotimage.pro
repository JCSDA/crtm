;+
; NAME:
;   OPLOTIMAGE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Overlays an image on an existing plot.
;
; CALLING SEQUENCE:
;   OPLOTIMAGE, img
;
; DESCRIPTION: 
;
;   OPLOTIMAGE overlays an image on an already-existing set of plot
;   axes.  It should not matter what plot elements have already be
;   displayed, but at least one command is needed to set up the plot
;   axes.
;
;   Only the IMGXRANGE and IMGYRANGE keywords, specifying the extent
;   of the image, can be given in a call to OPLOTIMAGE.
;
;   See PLOTIMAGE for more detailed information.
;
; INPUTS:
;
;   IMG - A byte array to be displayed.  An image declared as
;         ARRAY(M,N) will be M pixels in the x-direction and N pixels
;         in the y-direction.  The image is resampled via
;         interpolation to fill the desired display region.
; 
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   IMGXRANGE, IMGYRANGE - Each is a two component vector that
;                          describes the X and Y position of the first
;                          and last pixels.
;                          Default: the size of the image in pixels
;
; OUTPUTS:
;   NONE
;
; PROCEDURE:
;
; EXAMPLE:
;
;   This example first constructs an image whose values are found by
;       z(x,y) = cos(x) * sin(y)
;   and x and y are in the range [-2,2] and [4,8], respectively.
;   The image is then plotted in the range [-10, 10] in both x and
;   y directions.
;   
;   x = findgen(20)/5. - 2.
;   y = findgen(20)/5. + 4.
;   zz = cos(x) # sin(y)
;   imgxrange = [min(x), max(x)]
;   imgyrange = [min(y), max(y)]
;   xr=[-10.,10]
;   yr=[-10.,10]
;   plotimage, bytscl(zz), imgxrange=imgxrange, imgyrange=imgyrange
;
;   Now for the overlay.  A new image is created in the ranges between
;   -10 and 0:
;      z(x,y) = x y
;
;   x = findgen(20)/2 - 10.
;   y = findgen(20)/2 - 10.
;   imgxrange = [min(x), max(x)]
;   imgyrange = [min(y), max(y)]
;   zz = x # y
;   oplotimage, bytscl(zz), imgxrange=imgxrange, imgyrange=imgyrange
;
; SEE ALSO:
;
;   PLOTIMAGE, BYTSCL
;
; EXTERNAL SUBROUTINES:
;
;   SUBCELL, DEFSUBCELL, TVIMAGE
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Removed BYTE requirement, added ON_ERROR, CM 19 Apr 2000
;   Added copyright notice, CM 25 Mar 2001
;
;   $Id$
;
;-
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro oplotimage, img, $
                imgxrange=imgxrange, imgyrange=imgyrange, $
                _EXTRA=extra

  ;; Return to user upon encountering an error
  on_error, 2

  ;; Usage message
  if n_params() EQ 0 then begin
      message, 'OPLOTIMAGE, image, imgxrange=, imgyrange=,...', /info
      return
  endif

  sysposition = fltarr(4)
  sysposition([0,2]) = !x.window
  sysposition([1,3]) = !y.window
  sysxrange   = !x.range
  if sysxrange(0) EQ 0. AND sysxrange(1) EQ 0. then sysxrange = !x.crange
  sysyrange   = !y.range
  if sysyrange(0) EQ 0. AND sysyrange(1) EQ 0. then sysyrange = !y.crange
  if (sysxrange(0) EQ 0. AND sysxrange(1) EQ 0.) OR $
    (sysyrange(0) EQ 0. AND sysyrange(1) EQ 0.) then begin
      message, 'ERROR: you must first sent the X- and Y-RANGE'
  endif

  plotimage, img, xrange=sysxrange, yrange=sysyrange, imgxrange=imgxrange, $
    imgyrange=imgyrange, /noerase, position=sysposition, $
    /noaxes, _EXTRA=extra

  return
end

