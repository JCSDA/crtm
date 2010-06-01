PRO WBADLAYOUT

;- Create widgets
tlb = widget_base(xsize=350, ysize=300, $
  xoffset=400, yoffset=300)

label = widget_label(tlb, value='File name:', $
  xoffset=20, yoffset=20)
text = widget_text(tlb, xsize=30, $
  xoffset=100, yoffset=20, /editable)

text = widget_text(tlb, xsize=10, $
  xoffset=25, yoffset=50, /editable)
label = widget_label(tlb, value='Header size (bytes)', $
  xoffset=125, yoffset=50)

text = widget_text(tlb, xsize=10, $
  xoffset=25, yoffset=100, /editable)
label = widget_label(tlb, value='Number of columns', $
  xoffset=125, yoffset=100)

text = widget_text(tlb, xsize=10, $
  xoffset=25, yoffset=150, /editable)
label = widget_label(tlb, value='Number of rows', $
  xoffset=125, yoffset=150)

drop = widget_droplist(tlb, xoffset=75, yoffset=200, $
  value=['BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE'])

bbase = widget_base(tlb, xsize=350, yoffset=230, column=1)
buttb = widget_button(bbase, value='Abandon')
butta = widget_button(bbase, value='Accept')

;- Realize widgets
widget_control, tlb, /realize

END
