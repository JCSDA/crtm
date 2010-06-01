PRO IMGUI_DRAW, EVENT

;- Get state information
imgui_get_state, event, info, /no_copy

;- Convert event device coordinates to normal coordinates
xn = float(event.x) / float(info.draw_xsize)
yn = float(event.y) / float(info.draw_ysize)
xn = (xn > 0.0) < 1.0
yn = (yn > 0.0) < 1.0

;- Convert to fraction of image size
xf = (xn - info.out_pos[0]) / $
  (info.out_pos[2] - info.out_pos[0])
yf = (yn - info.out_pos[1]) / $
  (info.out_pos[3] - info.out_pos[1])

;- Update image value
if (xf gt 0.0) and (xf lt 1.0) and $
   (yf gt 0.0) and (yf lt 1.0) then begin

  ;- Get image size
  dims = size(info.image, /dimensions)
  case info.truedim of
    -1 : dims = dims
     0 : dims = [dims[1], dims[2]]
     1 : dims = [dims[0], dims[2]]
     2 : dims = [dims[0], dims[1]]
  endcase

  ;- Convert fractional image size to array indices
  xi = floor(xf * dims[0])
  yi = floor(yf * dims[1])
  xi = (xi > 0L) < (dims[0] - 1L)
  yi = (yi > 0L) < (dims[1] - 1L)

  ;- Get image data
  case info.truedim of
    -1 : data = info.image[xi, yi]
     0 : data = info.image[*, xi, yi]
     1 : data = info.image[xi, *, yi]
     2 : data = info.image[xi, yi, *]
  endcase

  ;- Update label widget
  if (size(data, /tname) eq 'BYTE') then data = fix(data)
  label = string('X:', strcompress(xi), $
    ', Y:', strcompress(yi), ', Data:')
  for index = 0L, n_elements(data) - 1L do $
    label = label + strcompress(data[index])
  widget_control, info.label_id, set_value=label

endif

;- Restore state information
imgui_set_state, event, info, /no_copy

END
