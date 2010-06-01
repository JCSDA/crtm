;-------------------------------------------------------------------------------

function hdf_get_info, file, names = names, labels = labels, $
  units = units, ndims = ndims

;- this function returns information about all SDS in a HDF file

;- set return value (number of SDS)

nsds = -1

;- check that file exists, and is HDF

result = hdf_open( file, /read )
if result lt 1 then goto, finish
hdf_close, result

;- open file and get number of SDS

sd_id = hdf_sd_start( file, /read )
hdf_sd_fileinfo, sd_id, nsds, ngatt
if nsds eq 0 then goto, finish

;- get information on SDS

names = strarr( nsds )
labels = strarr( nsds )
units = strarr( nsds )
ndims = lonarr( nsds )
    
for i = 0, nsds - 1 do begin

  sds_id = hdf_sd_select( sd_id, i )
  hdf_sd_getinfo, sds_id, name = na, label = la, unit = un, ndim = nd
  names( i ) = na
  labels( i ) = la
  units( i ) = un
  ndims( i ) = nd

endfor

finish:

;- close file if necessary

if nsds ge 0 then begin
  hdf_sd_endaccess, sds_id
  hdf_sd_end, sd_id
endif

;- return number of SDS

return, nsds

end

;-------------------------------------------------------------------------------

function sds_get_info, file, sds_name, dim_name = dim_name, $
  dim_size = dim_size, att_name = att_name

;- this function returns information about a specified SDS in a HDF file

;- set return value (number of dimensions)

ndim = -1

;- check that file exists, and is HDF

result = hdf_open( file, /read )
if result lt 1 then goto, finish
hdf_close, result

;- open file, select SDS, and get number of dimensions and attributes

sd_id = hdf_sd_start( file, /read )
sds_id = hdf_sd_select( sd_id, hdf_sd_nametoindex( sd_id, sds_name ) )
hdf_sd_getinfo, sds_id, ndims = ndim, natts = natt, dims = dim_size

;- get dimension info

if ndim gt 0 then begin
  dim_name = strarr( ndim )
  for i = 0, ndim - 1 do begin
    dim_id = hdf_sd_dimgetid( sds_id, i )
    hdf_sd_dimget, dim_id, name = name
    dim_name( i ) = name
  endfor
endif

;- get attribute info

if natt gt 0 then begin
  att_name = strarr( natt )
  for i = 0, natt - 1 do begin
    hdf_sd_attrinfo, sds_id, i, name = name
    att_name( i ) = name
  endfor
endif

finish:

;- close file if necessary

if ndim ge 0 then begin
  hdf_sd_endaccess, sds_id
  hdf_sd_end, sd_id
endif

;- return number of dimensions

return, ndim
    
end

;-------------------------------------------------------------------------------

pro sds_read, file, data, $
  sds = sds, info = info, start = start, count = count, $
  long_name = long_name, units = units, scale = scale, offset = offset, $
  read_all = read_all

;+
; NAME:
;       SDS_READ
;
; PURPOSE:
;       SDS_READ provides a point-and-click interface for reading a single SDS
;       from a HDF3.3r4 file. The user interactively selects which portion of
;       the SDS is to be read.
;
; CATEGORY:
;       HDF.
;
; CALLING SEQUENCE:
;       SDS_READ, FILE, DATA, $
;         SDS = SDS, INFO = INFO, START = START, COUNT = COUNT, $
;         LONG_NAME = LONG_NAME, UNITS = UNITS, SCALE = SCALE, OFFSET = OFFSET, $
;         READ_ALL = READ_ALL
;
; INPUTS:
;       FILE:      Name of HDF file to read.
;
;       DATA:      Named variable to hold returned SDS array data.
;
; KEYWORD PARAMETERS:
;       SDS:       Set this keyword to the name of a specific SDS to read.
;                  The default is for the user to select a SDS name from a
;                  widget list.
;
;       INFO:      Set this keyword to print SDS information - no data is read.
;                  The default is to not print any SDS information.
;
;       START:     Set this keyword to a vector containing the start position
;                  in the array to be read. The default is [0,0,...,0].
;
;       COUNT:     Set this keyword to a vector containing the number of items
;                  in the array to be read. The default is to read all items.
;
;       LONG_NAME: Set this keyword to a named variable in which the SDS
;                  long_name attribute is returned. The SDS name is returned
;                  if the long_name attribute is not defined.
;
;       UNITS:     Set this keyword to a named variable in which the SDS
;                  units attribute is returned. "no units" is returned
;                  if the units attribute is not defined.
;
;       SCALE:     Set this keyword to a named variable in which the SDS
;                  scale attribute is returned. 1.0 is returned if the scale
;                  attribute is undefined.
;
;       OFFSET:    Set this keyword to a named variable in which the SDS
;                  offset attribute is returned. 0.0 is returned if the offset
;                  attribute is undefined.
;
;       READ_ALL:  Read all the data in the SDS (START and COUNT not required).
;
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
;       Get SDS name, then read SDS.
;
; LIBRARY CALLS:
;       XLIST        (JHU APL library, ftp://fermi.jhuapl.edu/pub/idl)
;       CW_GETFIELDS (Liam Gumley, liam.gumley@ssec.wisc.edu)
;       HDF_GET_INFO (Liam Gumley, liam.gumley@ssec.wisc.edu)
;       SDS_GET_INFO (Liam Gumley, liam.gumley@ssec.wisc.edu)
;
; EXAMPLES:
;;     Create a simple HDF SDS file (256x256 float array)
;
;sd_id = hdf_sd_start( 'image.hdf', /create )
;sds_id = hdf_sd_create( sd_id, 'image', [256,256], /float )
;hdf_sd_adddata, sds_id, dist(256)
;hdf_sd_endaccess, sds_id
;hdf_sd_end, sd_id
;
;;     Read the image in interactive mode and display it
;
;sds_read, pickfile(), image
;help, image
;tvscl, image
;
;;     Read rows 128-257 of the image in non-interactive mode
;
;sds_read, 'image.hdf', image, sds = 'image', $
;  start = [ 0, 128 ], count = [ 256, 128 ]
;help, image
;
; MODIFICATION HISTORY:
;       Written by:     Liam Gumley, CIMSS/SSEC, 1 July 1996
;                       Liam Gumley, CIMSS/SSEC, 7 August 1996
;                       - added non-interactive read option via SDS keyword
;                       - added START and COUNT keywords
;                       - added print information option via INFO keyword
;                       Liam Gumley, CIMSS/SSEC, 27 September 1996
;                       - added READ_ALL keyword to read whole SDS
;                       Liam Gumley, CIMSS/SSEC, 03 December 1996
;                       - fixed problem with START and COUNT keywords
;-

;- is the file parameter present?

if n_elements( file ) eq 0 then message, 'Input parameter FILE is missing'

;- is the file parameter a string variable?

sz = size( file )
if sz( 0 ) ne 0 or sz( 1 ) ne 7 or sz( 2 ) ne 1 then $
  message, 'Input parameter FILE must be a string'

;- can the file be opened?

openr, lun, file, /get_lun, error = status
if status ne 0 then message, 'Input parameter FILE could not be opened'
free_lun, lun

;- is the file HDF?

if not hdf_ishdf( file ) then message, 'Input parameter FILE is not HDF'

;- check that input parameter DATA was passed (except when INFO keyword is set)

if n_elements( info ) eq 0 then info = 0
if ( n_params() lt 2 ) and ( info eq 0 ) then $
  message, 'Input parameter DATA is missing'

;- set return values

data = -1
name = ''
units = ''
scale = 1.0
offset = 0.0

;- get SDS information

nsds = hdf_get_info( file, names = names, labels = labels, $
  units = units_array, ndims = ndims )

;- check number of SDS found

if nsds lt 1 then message, 'No SDS were found in this file'

;- if labels (attribute "long_name") are missing, substitute SDS names
  
loc = where( labels eq '', nloc )
if nloc gt 0 then labels( loc ) = names( loc )

;- if units (attribute "units") are missing, substitute 'no units'

loc = where( units_array eq '', nloc )
if nloc gt 0 then units_array( loc ) = 'no units'

;- add units to labels

labels_units = labels + ' (' + units_array + ')'

;- select a SDS using widget list,
;- or the name supplied in the SDS keyword

if n_elements( sds ) eq 0 then begin

  ;- select an SDS using widget list of SDS names
  
  result = xlist( labels_units, title = 'Select a HDF dataset' )
  if result eq '' then goto, finish
  loc = where( labels_units eq result )

endif else begin

  ;- select an SDS using keyword
  
  loc = where( sds eq names, nloc )
  if nloc ne 1 then message, 'SDS ' + sds + ' was not found'

endelse

;- set index of selected SDS

index = loc( 0 )

;- get information on the selected dataset

ndim = sds_get_info( file, names( index ), dim_name = dim_name, $
  dim_size = dim_size, att_name = att_name )

;- if INFO keyword is set, then print information and quit

if info ne 0 then begin

  print, 'Information for SDS ' + names( index )

  print, 'Dimensions'
  for i = 0, n_elements( dim_name ) - 1 do begin
    print, "  ", dim_name( i ), ":", strcompress( dim_size( i ) )
  endfor

  print, 'Attributes'
  sd_id = hdf_sd_start( file, /read )
  sds_id = hdf_sd_select( sd_id, hdf_sd_nametoindex( sd_id, names( index ) ) )
  for i = 0, n_elements( att_name ) - 1 do begin
    att_id = hdf_sd_attrfind( sds_id, att_name( i ) )
    hdf_sd_attrinfo, sds_id, att_id, data = att_val
    print, "  ", att_name( i ), ": ", strcompress( att_val )
  endfor
  hdf_sd_endaccess, sds_id
  hdf_sd_end, sd_id

  goto, finish

endif

;- check number of dimensions

if ndim lt 1 then message, 'No dimensions found for SDS ' + names( index )

;- get start and count vectors using either widget dialog,
;- or START and COUNT keywords (must define both if used)

if keyword_set( read_all ) then goto, read_sds

if ( not keyword_set( start ) ) and ( not keyword_set( count ) ) then begin

  ;- get start and count vectors using widget dialog

  field_defaults = [ lonarr( ndim ), dim_size ]
  field_names = [ 'Start index for dimension [' + dim_name + ']', $
    'Number of values for dimension [' + dim_name + ']' ]
  field_title = labels( index )  
  result = cw_getfields( field_defaults, names = field_names, $
    title = field_title, /long )
  start = result( 0 : ndim - 1 )
  count = result( ndim  : * )

endif else begin

  ;- check that both START and COUNT keywords are defined
  
  if n_elements( start ) eq 0 or n_elements( count ) eq 0 then message, $
    'START and COUNT keywords must both be defined'
  
  ;- check that START and COUNT have the correct number of dimensions
  
  if n_elements( start ) ne ndim then message, $
    'START must have ' + strcompress( ndim ) + ' elements for SDS ' + $
    names( index )
  if n_elements( count ) ne ndim then message, $
    'COUNT must have ' + strcompress( ndim ) + ' elements for SDS ' + $
    names( index )

endelse

;- ensure that START and COUNT are valid

data_start = ( start > 0 ) < ( dim_size - 1 )
data_count = ( count > 1 ) < ( dim_size - start )
      
;- read the SDS

read_sds:
sd_id = hdf_sd_start( file, /read )
sds_id = hdf_sd_select( sd_id, hdf_sd_nametoindex( sd_id, names( index ) ) )
if keyword_set( read_all ) then begin
  hdf_sd_getdata, sds_id, data
endif else begin
  hdf_sd_getdata, sds_id, data, start = data_start, count = data_count
end

;- store long_name and units string

long_name = labels( index )
units = units_array( index )

;- get scale and offset if they exist

scale_count = 0
scale = 1.0
if n_elements( att_name ) gt 0 then $
  loc = where( att_name eq 'scale_factor', scale_count )
if scale_count eq 1 then begin
  att_id = hdf_sd_attrfind( sds_id, 'scale_factor' )
  hdf_sd_attrinfo, sds_id, att_id, data = scale
  sz = size( scale )
  if sz( n_elements( sz ) - 2 ) ne 7 then begin
    scale = float( scale )
  endif else begin
    scale = fltarr( sz( 1 ) )
    scale( * ) = 1.0
  endelse
endif

offset_count = 0
offset = 1.0
if n_elements( att_name ) gt 0 then $
  loc = where( att_name eq 'add_offset', offset_count )
if offset_count eq 1 then begin
  att_id = hdf_sd_attrfind( sds_id, 'add_offset' )
  hdf_sd_attrinfo, sds_id, att_id, data = offset
  sz = size( offset )
  if sz( n_elements( sz ) - 2 ) ne 7 then begin
    offset = float( offset )
  endif else begin
    offset = fltarr( sz( 1 ) )
    offset( * ) = 0.0
  endelse
endif

;- end access to SDS and close file

hdf_sd_endaccess, sds_id
hdf_sd_end, sd_id

finish:

end
