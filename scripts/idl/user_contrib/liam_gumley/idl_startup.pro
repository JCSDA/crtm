device, retain = 2, pseudo = 8
window, /free, /pixmap, colors = -5
plot, [ 0 ]
wdelete, !d.window
device, set_character_size = [ 6, 9 ]
widget_control, default_font = '9x15'
print, 'Number of colors is ', !d.n_colors
