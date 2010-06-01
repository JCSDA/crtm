PRO DROPLIST_EXAMPLE

;- Create column-aligned non-resizeable top level base
base = widget_base(column=1, title='Droplist Example', $
  tlb_frame_attr=1)

;- Create droplist widget with title
selections = ['Selection 1', 'Selection 2', 'Selection 3']
drop = widget_droplist(base, value=selections, $
  title='Selections')

;- Realize widgets
widget_control, base, /realize

END
