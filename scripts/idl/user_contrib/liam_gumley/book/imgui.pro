PRO IMGUI, IMAGE, BOTTOM=BOTTOM, NCOLORS=NCOLORS, $
  XSIZE=XSIZE, YSIZE=YSIZE, TITLE=TITLE, TABLE=TABLE, $
  DEBUG=DEBUG

;- Display an image in a GUI window

;- Check arguments
if (n_elements(image) eq 0) then $
  message, 'Argument IMAGE is undefined'
if (n_elements(xsize) eq 0) then xsize = 640
if (n_elements(ysize) eq 0) then ysize = 512
if (n_elements(title) eq 0) then title = 'IMGUI Window'
if (n_elements(table) eq 0) then table = 0
if (n_elements(debug) eq 0) then debug = 0

;- Check image dimensions
imageinfo = size(image)
ndim = imageinfo[0]
if (ndim ne 2) and (ndim ne 3) then $
  message, 'IMAGE must be a 2D or 3D array'
dims = imageinfo[1 : ndim]
if (ndim eq 3) then begin
  index = where(dims eq 3, count)
  if (count eq 0) then $
    message, 'RGB images must have one dimension of size 3'
  truedim = index[0]
endif else begin
  truedim = -1
endelse

;- Check for widget support
if ((!d.flags and 65536) eq 0) then $
  message, 'Widgets are not supported on this device'

;- Limit draw widget size to 90% of screen size
device, get_screen_size=screen_size
draw_xsize = xsize < (0.9 * screen_size[0])
draw_ysize = ysize < (0.9 * screen_size[1])

;- Create widgets
tlb = widget_base(column=1, mbar=mbar, title=title, $
  /tlb_size_events)
fmenu = widget_button(mbar, value='File')
butt1 = widget_button(fmenu, value='Save JPEG...', $
  uvalue='JPEG', event_pro='imgui_saveimage')
butt2 = widget_button(fmenu, value='Save TIFF...', $
  uvalue='TIFF', event_pro='imgui_saveimage')
butt3 = widget_button(fmenu, value='Save PostScript...', $
  uvalue='PostScript', event_pro='imgui_postscript')
butt4 = widget_button(fmenu, value='Exit', $
  /separator, event_pro='imgui_exit')
imenu = widget_button(mbar, value='Image')
butt5 = widget_button(imenu, value='Refresh', $
  uvalue='Refresh', event_pro='imgui_refresh')
butt6 = widget_button(imenu, value='Colors...', $
  uvalue='Colors', event_pro='imgui_colors')
if (ndim eq 3) then widget_control, butt6, sensitive=0
draw_id = widget_draw(tlb, $
  xsize=draw_xsize, ysize=draw_ysize, $
  uvalue='Draw', event_pro='imgui_draw', /motion_events)
base = widget_base(tlb, row=1, /align_center)
label_id = widget_label(base, value=' ', $
  /align_left, /dynamic_resize)
widget_control, tlb, /realize

;- Get window id, device name, and top level base size
widget_control, draw_id, get_value=draw_window
wset, draw_window
device = !d.name
widget_control, tlb, tlb_get_size=base_size

;- Get color information
version = float(!version.release)
if (version ge 5.1) then begin
  device, get_visual_depth=depth
endif else begin
  if (!d.n_colors le 256) then depth = 8 else depth = 24
endelse
if (n_elements(bottom) eq 0) then bottom = 0
if (n_elements(ncolors) eq 0) then $
  ncolors = !d.table_size - bottom

;- Create state information structure
info = {image:image, truedim:truedim, $
  bottom:bottom, ncolors:ncolors, debug:debug, $
  draw_xsize:draw_xsize, draw_ysize:draw_ysize, $
  tlb:tlb, draw_id:draw_id, draw_window:draw_window, $
  label_id:label_id, device:device, base_size:base_size, $
  version:version, depth:depth, $
  red:bytarr(ncolors), $
  grn:bytarr(ncolors), $
  blu:bytarr(ncolors), $
  out_pos:fltarr(4)}

;- Load entry color table and display image
loadct, table, bottom=bottom, ncolors=ncolors, /silent
imgui_getcolors, info
imgui_display, info

;- Store state information
infoptr = ptr_new(info)
widget_control, tlb, set_uvalue=infoptr

;- Start event manager
xmanager, 'imgui', tlb, $
  event_handler='imgui_tlb', cleanup='imgui_cleanup', $
  /no_block
if debug then print, 'IMGUI startup is done'

END
