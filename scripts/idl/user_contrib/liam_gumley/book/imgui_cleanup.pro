PRO IMGUI_CLEANUP, ID

;- Get information structure
widget_control, id, get_uvalue=infoptr
info = *infoptr

if info.debug then print, 'IMGUI_CLEANUP'

;- Free the information pointer
ptr_free, infoptr

END
