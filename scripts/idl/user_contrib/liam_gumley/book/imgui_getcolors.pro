PRO IMGUI_GETCOLORS, INFO

if info.debug then print, 'IMGUI_GETCOLORS'

;- Get the current color table
tvlct, red, grn, blu, info.bottom, /get
info.red = red[0 : (info.ncolors - 1)]
info.grn = grn[0 : (info.ncolors - 1)]
info.blu = blu[0 : (info.ncolors - 1)]

END
