PRO IMGUI_XLOADCT, DATA=EVENT

;- Get state information
imgui_get_state, event, info

if info.debug then print, 'IMGUI_XLOADCT'

;- Get color table and display image
imgui_getcolors, info
if (info.depth gt 8) then imgui_display, info

;- Set state information
imgui_set_state, event, info

END
