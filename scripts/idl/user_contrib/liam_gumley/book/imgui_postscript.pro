PRO IMGUI_POSTSCRIPT, EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_POSTSCRIPT'

;- Save a PostScript rendering of the image
filename = dialog_pickfile(/write, group=event.top)
if (filename ne '') then begin
  widget_control, event.top, hourglass=1
  pson, filename=filename, margin=1.0, /quiet
  imgui_display, info
  psoff, /quiet
  widget_control, event.top, hourglass=0
endif

END
