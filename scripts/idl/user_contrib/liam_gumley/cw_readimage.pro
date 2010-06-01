;+
; NAME:
;    CW_READIMAGE
;
; PURPOSE:
;    Read an image of user-specified type and size. Widget dialogs are used
;    to get the image file name, type, and size.
;
; CATEGORY:
;    Data I/O, Widgets.
;
; CALLING SEQUENCE:
;    CW_READIMAGE, DATA
;
; INPUTS:
;    DATA    Named variable to hold returned image array.
;
; KEYWORD PARAMETERS:
;    GROUP   The widget ID of the widget that calls CW_READIMAGE.  When this
;            ID is specified, a death of the caller results in the death of
;            the CW_READIMAGE widget application.
;
; OUTPUTS:
;    DATA    Image array of the requested type and size.
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
;    Get the file name, and type/size information. Check that the size of
;    the image requested by the user does not exceed the total file size,
;    and if it does not, read and return an array of the requested type
;    and size.
;
; EXAMPLE:
; Create, then read a 256 x 256 BYTE image from image.dat.
;
; A = BYTSCL( DIST( 256 ) )
; OPENW, LUN, 'image.dat', /GET_LUN
; WRITEU, LUN, A
; FREE_LUN, LUN
; CW_READIMAGE, DATA
; TVSCL, DATA
;
; MODIFICATION HISTORY:
;    Written by:     Liam Gumley, CIMSS/SSEC, 28-JUNE-1995
;-

pro cw_readimage, data, group = group

if not keyword_set( group ) then group = 0

;- get file name

name = pickfile( /read, group = group )
if( name eq '' ) then goto, finish

;- get image information

get_info:
info = image_info( group = group )
if( info.fail eq 1 ) then goto, finish
type = info.type
skip = info.skip
cols = info.cols
rows = info.rows
nbands = info.nbands
interleave = info.interleave

;- open file and get stats

openr, lun, name, /get_lun
a = fstat( lun )

;- check file size, and read if ok

f = [ 1L, 2L, 4L, 4L ]
size = ( long( skip ) + long( cols ) * long( rows ) * long( nbands ) ) * f( type )

if( size gt a.size ) then begin

  ;- print error message and return error flag
  
  sz1 = strcompress( size, /remove_all )
  sz2 = strcompress( a.size, /remove_all )
  result = widget_message( $
    [ ( "Error: The image size you requested (" + sz1 + " bytes)" ), $
    ( "is larger than the actual file size (" + sz2 + " bytes)." ) ], $
    /error, title = 'image_read error' )
    goto, get_info
  
endif else begin

  ;- create data array and read

  if( nbands eq 1 ) then begin
  
    ;- single band image
    
    case type of
      0: data = bytarr( cols, rows, /nozero )    
      1: data = intarr( cols, rows, /nozero )
      2: data = lonarr( cols, rows, /nozero )
      3: data = fltarr( cols, rows, /nozero )
    end
  
  endif else begin
  
    ;- multiple band image - select interleave type
    
    case interleave of
  
      0: begin
      case type of
        0: data = bytarr( cols, rows, nbands, /nozero )    
        1: data = intarr( cols, rows, nbands, /nozero )
        2: data = lonarr( cols, rows, nbands, /nozero )
        3: data = fltarr( cols, rows, nbands, /nozero )
      end
      end
      
      1: begin
      case type of
        0: data = bytarr( cols, nbands, rows, /nozero )    
        1: data = intarr( cols, nbands, rows, /nozero )
        2: data = lonarr( cols, nbands, rows, /nozero )
        3: data = fltarr( cols, nbands, rows, /nozero )
      end
      end
      
      2: begin
      case type of
        0: data = bytarr( nbands, cols, rows, /nozero )    
        1: data = intarr( nbands, cols, rows, /nozero )
        2: data = lonarr( nbands, cols, rows, /nozero )
        3: data = fltarr( nbands, cols, rows, /nozero )
      end
      end
      
    end
    
  endelse

  ;- read data
        
  point_lun, lun, skip * f( type )
  readu, lun, data
  free_lun, lun
  
endelse

finish:
  
end
