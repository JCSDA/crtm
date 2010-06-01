function get_ncolors

;- save initial graphics device name

olddevice = !d.name

;- set to 8 bit graphics device
        
thisos = !version.os_family
thisos = strmid(thisos, 0, 3)
thisos = strupcase(thisos)
case thisos of
  'MAC': set_plot, thisos
  'WIN': set_plot, thisos
  else: set_plot, 'X'
endcase
device, pseudo=8
      
;- open a window (to make sure !d.n_colors is accurate)
         
window, /free, /pixmap, xsize=10, ysize=10
wdelete, !d.window
      
;- retrieve the actual number of colors
         
ncolors = !d.n_colors

;- switch back to the initial graphics device

set_plot, olddevice

return, ncolors

end
