PRO TABLE_EXAMPLE

;- Create column-aligned non-resizeable top level base
tlb = widget_base(column=1, title='Table Example', $
  tlb_frame_attr=1)

;- Create label widget
label = widget_label(tlb, value='2D array')

;- Create table widget for a 2D array
data = dist(32)
table1 = widget_table(tlb, value=data, $
  x_scroll_size=4, y_scroll_size=4, /editable)

;- Create label widget
label = widget_label(tlb, value='Vector of structures')

;- Create table widget for a vector of structures
record = {lat:45.0, lon:-89.0, $
  altitude:20500.0, heading:245.0}
data = replicate(record, 100)
names = tag_names(data)
table2 = widget_table(tlb, value=data, $
  x_scroll_size=4, y_scroll_size=4, $
  column_labels=names, /editable)

;- Realize widgets
widget_control, tlb, /realize

END
