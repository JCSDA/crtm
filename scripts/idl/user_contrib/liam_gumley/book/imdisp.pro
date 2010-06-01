PRO IMDISP, IMAGE, POSITION=POSITION, OUT_POS=OUT_POS, $
  BOTTOM=BOTTOM, NCOLORS=NCOLORS, NOSCALE=NOSCALE, $
  MARGIN=MARGIN, ORDER=ORDER, _EXTRA=EXTRA_KEYWORDS

;- Check arguments
if (n_params() ne 1) then message, 'Usage: IMDISP, IMAGE'
if (n_elements(image) eq 0) then $
  message, 'Argument IMAGE is undefined'
if (n_elements(margin) eq 0) then begin
  case 1 of
    (max(!p.multi) gt 0)        : margin = 0.025
    (n_elements(position) eq 4) : margin = 0.0
    else                        : margin = 0.1
  endcase
endif
if (n_elements(order) eq 0) then order = 0

;- Check the image dimensions
result = size(image)
ndims = result[0]
if (ndims lt 2) or (ndims gt 3) then $
  message, 'IMAGE must be a 2D or 3D array'
dims = result[1 : ndims]

;- Check that 3D image array is in valid true color form
true = 0
if (ndims eq 3) then begin
  index = where(dims eq 3L, count)
  if (count lt 1) then $
    message, 'IMAGE must be of the form ' + $
    '[3,NX,NY], [NX,3,NY], or [NX,NY,3]'
  true = 1
  truedim = index[0]
endif

;- Get color table information
if ((!d.flags and 256) ne 0) and $
  (!d.window lt 0) then begin
  window, /free, /pixmap
  wdelete, !d.window
endif
if (n_elements(bottom) eq 0) then bottom = 0
if (n_elements(ncolors) eq 0) then $
  ncolors = !d.table_size - bottom

;- Check for IDL 5.2 if printer device is selected
version = float(!version.release)
if (version lt 5.2) and (!d.name eq 'PRINTER') then $
  message, 'IDL 5.2 or higher is required ' + $
  'for PRINTER device support'

;- Get red, green, and blue components of true color image
if (true eq 1) then begin
    case truedim of
    0 : begin
          red = image[0, *, *]
          grn = image[1, *, *]
          blu = image[2, *, *]
        end
    1 : begin
          red = image[*, 0, *]
          grn = image[*, 1, *]
          blu = image[*, 2, *]
        end
    2 : begin
          red = image[*, *, 0]
          grn = image[*, *, 1]
          blu = image[*, *, 2]
        end
  endcase
  red = reform(red)
  grn = reform(grn)
  blu = reform(blu)
endif

;- Establish image position
if (n_elements(position) eq 0) then begin
  if (max(!p.multi) eq 0) then begin
    position = [0.0, 0.0, 1.0, 1.0]
  endif else begin
    plot, [0], /nodata, xstyle=4, ystyle=4, $
      xmargin=[0, 0], ymargin=[0, 0]
    position = [!x.window[0], !y.window[0], $
                !x.window[1], !y.window[1]]
  endelse
endif

;- Compute size of displayed image
position_value = position
case true of
  0 : imsize, image, x0, y0, xsize, ysize, $
        margin=margin, position=position_value, $
        _extra=extra_keywords
  1 : imsize, red, x0, y0, xsize, ysize, $
        margin=margin, position=position_value, $
        _extra=extra_keywords
endcase
out_pos = position_value

;- Choose whether to scale the image or not
if (keyword_set(noscale) eq 1) then begin

  ;- Don't scale the image
  case true of
    0 : scaled = image
    1 : begin
          scaled_dims = (size(red))[1:2]
          scaled = replicate(red[0], $
            scaled_dims[0], scaled_dims[1], 3)
          scaled[*, *, 0] = red
          scaled[*, *, 1] = grn
          scaled[*, *, 2] = blu
        end
  endcase

endif else begin

  ;- Scale the image
  case true of
    0 : scaled = imscale(image, $
          bottom=bottom, ncolors=ncolors)
    1 : begin
          scaled_dims = (size(red))[1:2]
          scaled = bytarr(scaled_dims[0], scaled_dims[1], 3)
          scaled[*, *, 0] = imscale(red, $
            bottom=0, ncolors=256)
          scaled[*, *, 1] = imscale(grn, $
            bottom=0, ncolors=256)
          scaled[*, *, 2] = imscale(blu, $
            bottom=0, ncolors=256)
        end
  endcase

endelse

;- Display the image on the printer device
if (!d.name eq 'PRINTER') then begin
  case true of
    0 : begin
          device, /index_color
          tv, scaled, x0, y0, $
            xsize=xsize, ysize=ysize, order=order
        end
    1 : begin
          device, /true_color
          tv, scaled, x0, y0, $
            xsize=xsize, ysize=ysize, order=order, true=3
        end
  endcase
  return
endif

;- Display the image on devices with scaleable pixels
;- (including PostScript)
if ((!d.flags and 1) ne 0) then begin
  case true of
    0 : tv, scaled, x0, y0, $
          xsize=xsize, ysize=ysize, order=order
    1 : begin
          tvlct, r, g, b, /get
          loadct, 0, /silent
          tv, scaled, x0, y0, $
            xsize=xsize, ysize=ysize, order=order, true=3
          tvlct, r, g, b
        end
  endcase
  return
endif

;- Get display depth
depth = 8
if (!d.name eq 'WIN') or $
   (!d.name eq 'MAC') or $
   (!d.name eq 'X') then begin
  if (version ge 5.1) then begin
    device, get_visual_depth=depth
  endif else begin
    if (!d.n_colors gt 256) then depth = 24
  endelse
endif

;- If the display is 8-bit and image is TrueColor,
;- convert image to PseudoColor
if (depth le 8) and (true eq 1) then begin
  scaled = congrid(temporary(scaled), xsize, ysize, 3)
  scaled = color_quan(temporary(scaled), 3, r, g, b, $
    /dither, colors=ncolors) + byte(bottom)
  tvlct, r, g, b, bottom
  true = 0
endif

;- Set decomposed color mode
if (depth gt 8) and (true eq 1) then begin
  device, decomposed=1
endif else begin
  device, decomposed=0
endelse

;- Display the image
case true of
  0 : tv, congrid(scaled, xsize, ysize), x0, y0, $
        order=order
  1 : tv, congrid(scaled, xsize, ysize, 3), x0, y0, $
        order=order, true=3
endcase

;- Turn decomposed color off if required
if (depth gt 8) and (true eq 1) then device, decomposed=0

END
