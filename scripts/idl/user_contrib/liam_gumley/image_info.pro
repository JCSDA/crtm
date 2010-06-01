;+
; NAME:
;    IMAGE_INFO
;
; PURPOSE:
;    This function allows the user to interactively define the data type and
;    size of a multiband input image.
;    A single band image is implied if the number of bands is 1.
;
; CATEGORY:
;    Widgets.
;
; CALLING SEQUENCE:
;    Result = IMAGE_INFO()
;
; INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    GROUP  The widget ID of the widget that calls IMAGE_INFO.  When this
;           ID is specified, a death of the caller results in the death of
;           the IMAGE_INFO widget application.
;
; OUTPUTS:
;    IMAGE_INFO returns a structure with elements defined as follows:
;    TYPE       Data type (0=BYTE, 1=SHORT, 2=LONG, 3=FLOAT, default=0)
;    SKIP       Number of bytes to skip before reading the image (default=0)
;    COLS       Number of columns (X) in the image (default=1)
;    ROWS       Number of rows (Y) in the image (default=1)
;    NBANDS     Total number of bands in this image (default=1)
;    INTERLEAVE Interleave type (0=Pixel, 1=Line, 2=Band, meaningful if NBANDS GT 1) 
;    FAIL       Fail flag (0=Ok button hit, 1=Cancel button hit)
;
; COMMON BLOCKS:
;    None.
;
; SIDE EFFECTS:
;    This function initiates the XMANAGER if it is not already running.
;
; RESTRICTIONS:
;    None.
;
; PROCEDURE:
;    Create and register the widget and then exit, returning the image
;    type and size information selected by the user.
;
; EXAMPLE:
; Create an IMAGE_INFO widget that lets the user select the data type
; and size of an image to be read.
;
; INFO = IMAGE_INFO()
; PRINT, INFO
;
; MODIFICATION HISTORY:
;    Written by:     Liam Gumley, CIMSS/SSEC, 29-JUNE-1995
;-

; event handler for image_info

pro image_info_event, event

; get handle id stored in user value of first child of root widget,
; and get info structure

main = widget_info( event.handler, /child )
widget_control, main, get_uvalue = value
id = value( 0 )
droplist21 = value( 1 )

; get return value structure

handle_value, id, info

; get the user value of the widget which caused this event,
; and check user values

widget_control, event.id, get_uvalue = ev
done = 0

case ev of 

  ; data type

  'droplist11' : info.type = widget_info( event.id, /droplist_select )

  ; bytes to skip
    
  'field21' : begin
    widget_control, event.id, get_value = skip
    info.skip = skip
  end
  
  ; number of columns  
  
  'field22' : begin
    widget_control, event.id, get_value = cols
    info.cols = cols
  end
  
  ; number of rows  
  
  'field23' : begin
    widget_control, event.id, get_value = rows
    info.rows = rows
  end
  
  ; total bands
  
  'field24' : begin
    widget_control, event.id, get_value = nbands
    info.nbands = nbands
    if( info.nbands gt 1 ) then begin
      widget_control, droplist21, sensitive = 1
    endif
  end
  
  ; interleave type
  
  'droplist21' : info.interleave = widget_info( event.id, /droplist_select )
  
  ; ok button  
  
  'button31': begin
    
    ; check values to be returned
    
    if( info.skip lt 0 ) then begin
    
      result = widget_message( $
        'Error: Bytes to skip must be GE 0', $
        /error, title = 'image_info error' )
        
    endif else if( info.cols lt 1 or info.cols lt 1 ) then begin
    
      result = widget_message( $
        'Error: Columns and Rows must be GE 1', $
        /error, title = 'image_info error' )
    
    endif else if( info.nbands lt 1 ) then begin
      
      result = widget_message( $
        'Error: Number of bands must be GE 1', $
        /error, title = 'image_info error' )
        
    endif else begin
    
      ; everything is ok
      
      info.fail = 0
      done = 1
      
    endelse
    
  end
  
  ; cancel button  
  
  'button32': begin
  
    info.fail = 1
    done = 1
    
  end
  
  ; help button  
  
  'button33': result = widget_message( [ $
    "You need to select the data type and size of the image.", " ", $
    "The data types available are:",$
    "  Byte           (8 bits): values range from 0 to 255,",$
    "  Short Integer (16 bits): values range from -32768 to 32768,",$
    "  Long Integer  (32 bits): values range from -2**31 to 2**31-1,",$
    "  IEEE Real     (32 bits): absolute values range from 1e-38 to 3e38,", " ",$
    "You must also define the image size, by entering:",$
    "  the number of bytes to skip at the beginning of the file,",$
    "  the number of columns across the image (x coordinate),",$
    "  the number of rows down the image (y coordinate).", " ", $
    "If this is a multiband image, you must specify the total number", $
    "number of bands, and the interleave type.", $
    "Interleave types are:", $
    "  Band (data stored as columns(X), rows(Y), bands)", $
    "  Line (data stored as columns(X), bands, rows(Y))", $
    "  Pixel (data stored as bands, columns(X), rows(Y))" ], $
    /information, title = 'Image Information Help' )
       
endcase

; reset return value structure, and destroy widget if done

handle_value, id, info, /set
if( done ) then widget_control, event.top, /destroy

end

;--------------------------------------------------------------------------------

function image_info, group = group

if n_elements( group ) eq 0 then group = 0

; create and store return value structure

info = { type:0L, skip:0L, cols:1L, rows:1L, nbands:1L, interleave:0L, fail:0L }
id = handle_create()
handle_value, id, info, /set

; create root base, and interpose extra base to hold state/return information

root = widget_base()
main = widget_base( root, column = 1 )

; create data type droplist

base1 = widget_base( main, column = 1, map = 1, uvalue = 'mainbase' )
value = [ $
  'Byte (8 bits)', $
  'Short Integer (16 bits)', $
  'Long Integer (32 bits)', $
  'IEEE Real (32 bits)' ]
droplist11 = widget_droplist( base1, value = value, uvalue= 'droplist11', $
  title = 'Data type' )

; create fields for bytes to skip, columns, rows

base2 = widget_base( main, column = 1, map = 1, uvalue = 'base2' )
field21 = cw_field( base2, value = info.skip, row = 1, long = 1, $
  all_events = 1, uvalue = 'field21', title = '  Bytes to skip' )
field22 = cw_field( base2, value = info.cols, row = 1, long = 1, $
  all_events = 1, uvalue = 'field22', title = '    Columns (X)' )
field23 = cw_field( base2, value = info.rows, row = 1, long = 1, $
  all_events = 1, uvalue = 'field23', title = '       Rows (Y)' )
field24 = cw_field( base2, value = info.nbands, row = 1, long = 1, $
  all_events = 1, uvalue = 'field24', title = 'Number of bands' )

; create interleave type droplist

value = [ $
  'Band', $
  'Line', $
  'Pixel' ]
droplist21 = widget_droplist( base2, value = value, uvalue= 'droplist21', $
  title = 'Interleave type' )

; desensitize interleave droplist until number of bands GT 1

widget_control, droplist21, sensitive = 0

; create buttons for OK, cancel, help

base3 = widget_base( main, row = 1, map = 1, uvalue='base3' )
button31 = widget_button( base3, uvalue = 'button31', value = '    OK    ' )
label31 = widget_label( base3, value = '  ' )
button32 = widget_button( base3, uvalue = 'button32', value = '  Cancel  ' )
label32 = widget_label( base3, value = '  ' )
button33 = widget_button( base3, uvalue = 'button33', value = '   Help   ' )

; store handle id in user value of first child of root

value = [ id, droplist21 ]
widget_control, main, set_uvalue = value 

; realize widgets

widget_control, root, /realize
xmanager, 'image_info', root, group_leader = group

; get return value
 
handle_value, id, info
return, info

end
