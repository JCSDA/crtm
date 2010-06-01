PRO BUTTON_EXAMPLE

;- Create column-aligned non-resizeable top level base
;- with menu bar
tlb = widget_base(column=1, mbar=mbar, $
  title='Button Example', tlb_frame_attr=1)

;- Create menu buttons in menu bar base
filemenu = widget_button(mbar, value='File')
fileopt1 = widget_button(filemenu, value='Open')
fileopt2 = widget_button(filemenu, value='Save')
fileopt3 = widget_button(filemenu, value='Exit', /separator)
editmenu = widget_button(mbar, value='Edit')
helpmenu = widget_button(mbar, value='Help')

;- Create bitmap buttons in row-aligned base
iconbase = widget_base(tlb, row=1, /frame)
subdir   = 'resource/bitmaps'
mapfile  = filepath('open.bmp', subdir=subdir)
iconopt1 = widget_button(iconbase, value=mapfile, /bitmap)
mapfile  = filepath('save.bmp', subdir=subdir)
iconopt2 = widget_button(iconbase, value=mapfile, /bitmap)
mapfile  = filepath('print1.bmp', subdir=subdir)
iconopt3 = widget_button(iconbase, value=mapfile, /bitmap)

;- Create text buttons
buttbase = widget_base(tlb, row=1)
buttsize = 60
buttopt1 = widget_button(buttbase, value='OK', $
  xsize=buttsize)
buttopt2 = widget_button(buttbase, value='Cancel', $
  xsize=buttsize)
buttopt3 = widget_button(buttbase, value='Help', $
  xsize=buttsize)

;- Create exclusive and non-exclusive radio buttons
buttbase = widget_base(tlb, row=1, /exclusive)
buttopt1 = widget_button(buttbase, value='On')
buttopt2 = widget_button(buttbase, value='Off')
buttbase = widget_base(tlb, row=1, /nonexclusive)
buttopt1 = widget_button(buttbase, value='Lettuce')
buttopt2 = widget_button(buttbase, value='Tomato')
buttopt3 = widget_button(buttbase, value='Mustard')

;- Realize widgets
widget_control, tlb, /realize

END
