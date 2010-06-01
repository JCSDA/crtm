;-------------------------------------------------------------------------------
;+
; NAME:
;       CW_GETFIELDS
;
; PURPOSE:
;       This compound widget function allows the user to edit a
;       user-defined list of fields, and returns the values entered
;       in the field dialog boxes.
;
; CATEGORY:
;       Widgets.
;
; CALLING SEQUENCE:
;       Result = CW_GETFIELDS( DEFAULTS )
;
; INPUTS:
;       DEFAULTS:  Array of default field values.
;
; KEYWORD PARAMETERS:
;
;       TITLE:     Label for widget dialog box.
;
;       NAMES:     String array of labels for field dialog boxes.
;
;       FLOAT:     Accept only floating point values in field dialog boxes,
;                  and return result array as FLOAT type.
;                  Default is return result array as STRING type.
;
;       LONG :     Accept only long integer values in field dialog boxes,
;                  and return result array as LONG type.
;                  Default is to return result array as STRING type.
;
;       INTEGER:   Accept only integer values in field dialog boxes,
;                  and return result array as INTEGER type.
;                  Default is to return result array as STRING type.
;
;       GROUP:     The widget ID of the widget that calls CW_GETFIELDS.
;                  When this ID is specified, a death of the caller results
;                  in the death of CW_GETFIELDS.
;       
; OUTPUTS:
;       CW_GETFIELDS returns a STRING array containing the values
;       entered by the user in the field dialog boxes. If no changes
;       are made by the user, then the default values (DEFAULTS) are returned.

; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       This function initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;       None known.
;
; PROCEDURE:
;       Create and register the widget and then exit, returning the
;       field values as a string array.
;
; EXAMPLES:
;
;;      Input from 5 fields
;;      (Return string array)
;
; result = cw_getfields( indgen( 10 ) )
; print, strcompress( result )
;
;;      Size of a byte image to be read from disk
;;      (Return long integer array)
;
; defaults = [ 512, 512, 0 ]
; names = [ 'Number of columns', 'Number of rows', 'Header bytes to skip' ]
; result = cw_getfields( defaults, title = 'Image Size', names = names, /long )
; print, result
;
;;      Mixed text and numeric input
;;      (Return string array containing text and numeric entries)
;
; defaults = [ 'input.dat', '100', 'y', '5,5' ]
; names = [ 'File to process', 'Records', 'Flip bytes (y/n)', 'Offset (x,y)' ]
; result = cw_getfields( defaults, title = 'Input Parameters', names = names )
; filename = result( 0 )
; nrecs = fix( result( 1 ) )
; answer = result( 2 )
; reads, result( 3 ), xoff, yoff
; print, filename, nrecs, answer, xoff, yoff, format = '(a20,i6,1x,a1,i6,i6)'
;
; MODIFICATION HISTORY:
;       Written by:     Liam Gumley, CIMSS/SSEC, 9 April 1996
;-
;-------------------------------------------------------------------------------

pro cw_getfields_event, event

;- get event value

widget_control, event.id, get_uvalue = ev

;- get handle id from first child of root widget (i.e. the extra base)

extra = widget_info( event.handler, /child )
widget_control, extra, get_uvalue = id

;- get handle information

handle_value, id, result

;- check field event values and set return value

for i = 0, n_elements( result ) - 1 do begin

  string = 'field' + strcompress( i, /remove_all )
  if ev eq string then begin
    widget_control, event.id, get_value = value
    result( i ) = string( value )
    handle_value, id, result, /set
  endif
  
endfor

;- check done button

if ev eq 'button' then begin
  widget_control, event.top, /destroy
endif

end

;-------------------------------------------------------------------------------

function cw_getfields, defaults, title = title, names = names, $
  float = float, long = long, integer = integer, group = group

;- check keywords

if n_elements( defaults ) eq 0 then $
  message, 'Input argument DEFAULTS not defined'
if keyword_set( names ) and $
  ( n_elements( names ) ne n_elements( defaults ) ) then $
  message, 'DEFAULTS and NAMES are not the same size'
if not keyword_set( float ) then float = 0
if not keyword_set( long ) then long = 0
if not keyword_set( integer ) then integer = 0
if not keyword_set( group ) then group = 0
    
;- create parent top base,
;- and main base to hold information passed to event handler

top = widget_base( title = ' ' )
main = widget_base( top, /column, /map )

;- create handle and store id in user value of extra base

id = handle_create()
widget_control, main, set_uvalue = id

;- add label if defined

if keyword_set( title ) then result = widget_label( main, value = title )

;- add fields

for i = 0, n_elements( defaults ) - 1 do begin

  value = 'Field ' + strcompress( i, /remove_all )
  if keyword_set( names ) then value = names( i )
  result = cw_field( main, /all_events, /column, /frame, $
    title = value, value = defaults( i ), $
    uvalue = 'field' + strcompress( i, /remove_all ), $
    float = float, long = long, integer = integer )
  
endfor

;- add done button

result = widget_button( main, uvalue = 'button', value = 'Done' )

;- store information for event handler

result = string( defaults )
handle_value, id, result, /set

;- realize widgets and invoke xmanager

widget_control, main, /realize
xmanager, 'cw_getfields', top, group_leader = group

;- get information returned from event handler

handle_value, id, result

;- convert to correct data type, if type is specified

if float then result = float( result )
if long then result = long( result )
if integer then result = fix( result )

;- return result array

return, result

end

;-------------------------------------------------------------------------------
