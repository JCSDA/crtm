PRO WGOODLAYOUT

;- Create top level base
tlb = widget_base(column=1, title='Read Image File', $
  tlb_frame_attr=1)

;- Create base to hold everything except buttons
main = widget_base(tlb, column=1, frame=1)

;- Create file widgets
fbase = widget_base(main, row=1, /base_align_center)
label = widget_label(fbase, value='Filename:')
text  = widget_text(fbase, /editable, xsize=20)
butt  = widget_button(fbase, value='Browse...')

;- Create array size widgets
abase = widget_base(main, row=3, $
  /grid_layout, /base_align_center)
label = widget_label(abase, value='Header bytes to skip:')
text  = widget_text(abase, value='0', /editable, xsize=8)
label = widget_label(abase, value='Number of columns:')
text  = widget_text(abase, /editable, xsize=8)
label = widget_label(abase, value='Number of rows:')
text  = widget_text(abase, /editable, xsize=8)

;- Create data type and order droplists
dbase = widget_base(main, row=1, /align_center)
label = widget_label(dbase, value='Data type:')
drop  = widget_droplist(dbase, value=['Byte', 'Integer', $
  'Long', 'Float', 'Double'])
label = widget_label(dbase, value='Byte order: ')
drop  = widget_droplist(dbase, value=['Host', 'Swap', 'XDR'])

;- Create ok and cancel buttons
buttsize = 75
bbase = widget_base(tlb, row=1, /align_center)
butt  = widget_button(bbase, value='OK', xsize=buttsize)
butt  = widget_button(bbase, value='Cancel', xsize=buttsize)
butt  = widget_button(bbase, value='Help', xsize=buttsize)

;- Realize widgets
widget_control, tlb, /realize

END
