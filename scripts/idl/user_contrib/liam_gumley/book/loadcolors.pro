PRO LOADCOLORS, BOTTOM=BOTTOM, NAMES=NAMES

;- Check arguments
if (n_elements(bottom) eq 0) then bottom = 0

;- Load McIDAS graphics colors
red = [  0, 255,   0, 255,   0, 255,   0, 255, $
         0, 255, 255, 112, 219, 127,   0, 255]
grn = [  0,   0, 255, 255, 255,   0,   0, 255, $
         0, 187, 127, 219, 112, 127, 163, 171]
blu = [  0, 255, 255,   0,   0,   0, 255, 255, $
       115,   0, 127, 147, 219, 127, 255, 127]
tvlct, red, grn, blu, bottom

;- Set color names
names = [ 'Black', 'Magenta', 'Cyan', 'Yellow', $
          'Green', 'Red', 'Blue', 'White', $
          'Navy', 'Gold', 'Pink', 'Aquamarine', $
          'Orchid', 'Gray', 'Sky', 'Beige' ]

END
