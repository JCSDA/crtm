pro masview, file, band, $
  image = image, nodisplay = nodisplay, raw = raw, radiance = radiance, $
  temperature = temperature, micron = micron, scale = scale, range = range, $
  ps = ps, landscape = landscape, custom = custom, gif = gif, outfile = outfile, $
  scan = scan, noaxis = noaxis, hisfile = hisfile, equal = equal, $
  time = time, histime = histime, date = date, skip = skip, $
  difference = difference, record = record, reverse = reverse, $
  help = help, charsize = charsize, ncolors = ncolors, bottom = bottom

;-------------------------------------------------------------------------------
;+
; NAME:
;       MASVIEW
; 
; PURPOSE:
;       Extract and display a single band of MAS imagery.
;       (1) Exabyte format as recorded in-flight (57344 bytes per block),
;       (2) UW-Intermediate format (74200 bytes per block),
;       (3) HDF Level 1B format,
;       (4) UW-Intermediate Quicklook format.
; 
; CATEGORY:
;       MAS visualization.
; 
; CALLING SEQUENCE:
;       MASVIEW, FILE, BAND
; 
; INPUTS:
;       FILE:         Name of MAS 50 channel UW intermediate format file.
;       BAND:         Band number to extract/display.
; 
; OPTIONAL KEYWORD PARAMETERS:
;       IMAGE         2D array in which the band data will be returned,
;                     (default units are gain corrected counts,
;                     but /RAW, /RADIANCE, or /TEMPERATURE may be used).
;       /NODISPLAY    Do not display image (useful when used with IMAGE keyword),
;                     (default is to display image).
;       /RAW          Display raw counts,
;                     (default is to display gain corrected counts).
;       /RADIANCE     Display data in radiance units of mW/m2/sr/cm-1,
;                     (default is to display gain corrected counts).
;       /TEMPERATURE  Display data in brightness temperature units of Kelvin,
;                     (default is to display gain corrected counts).
;       /MICRON       When /RADIANCE is also set, display radiance in units of
;                     W/m2/sr/micron, (default is mW/m2/sr/cm-1).
;       /SCALE        Display a vertical color scale to the right of the image,
;                     (default is do not display color scale).
;       /RANGE        2 or 3 element vector indicating indicating physical range
;                     over which to map the color scale. The third element of RANGE,
;                     if specified, sets the step interval of the displayed color
;                     scale.
;                     (default range is minimum to maximum value of band data).
;       /PS           Send display to a Postscript output file,
;                     (default is to send display to current graphics window,
;                     default output file name is masview.ps unless OUTFILE is set).
;       /LANDSCAPE    Use landscape orientation for Postscript output,
;                     (default is portrait).
;       /CUSTOM       Customize Postscript output using popup dialog box.
;       /GIF          Copy display from graphics window to a GIF file,
;                     (default output file name is masview.gif unless OUTFILE is set).
;       OUTFILE       Output file name for use with PS of GIF keywords,
;                     (default output file name is masview.ps unless OUTFILE is set).
;       /SCAN         Draw image axes in scan (Y) and pixel (X) coordinates,
;                     (default is time (Y) and scan angle (X) coordinates).
;       /NOAXIS       Do not draw axes around image,
;                     (default is to draw axes).
;       HISFILE       Name of HIS spectrum data file. A box representing the HIS
;                     footprint will be printed at each HIS spectrum time,
;                     (default is do not draw HIS footprints).
;       /EQUAL        Histogram equalize the resulting image,
;                     (default is do not histogram equalize the image).
;                     Be careful if you use the IMAGE keyword to return the
;                     image data - it will be histogram equalized.
;       TIME          1D array in which MAS GPS time (UTC hours) is returned,
;                     (default is do not return MAS time).
;       HISTIME       1D array in which HIS GPS time (UTC hours) is returned
;                     when HISFILE keyword is also set,
;                     (default is do not return HIS time).
;       DATE          1D array in which date is specified [DAY,MONTH,YEAR]
;                     (default is to obtain date from data).
;       /SKIP         Skip every second scanline
;                     (default is to use every scanline).
;       DIFFERENCE    Two element vector of the form [BAND1,BAND2], where
;                     BAND1 and BAND2 are MAS band numbers between
;                     26 and 50. The brightness temperature difference (K)
;                     (BAND1-BAND2) will be displayed.
;       RECORD        A two element vector containing the first and last
;                     record numbers to be read, e.g. [2000,3000]
;                     (default is to read all records). Currently works with
;                     with UW-Intermediate format and HDF input files.
;       /REVERSE      Reverse the color table (useful for IR bands).
;       /HELP         If set, print help text.
;       CHARSIZE      Scalar float value indicating character scaling factor
;                     (default is 1.0).
;       NCOLORS       Scalar integer value indicating number of colors in
;                     color table to use (default is use all colors).
;       BOTTOM        Scalar integer value indicating bottom color in color
;                     table to use (default is zero).
; 
; OUTPUTS:
;       None.
;  
; COMMON BLOCKS:
;       None.
; 
; SIDE EFFECTS:
;       None.
; 
; RESTRICTIONS:
;       Requires ESRG IDL library routines.
;       Requires Liam Gumley user library routines.
; 
; PROCEDURE:
;       Read data from file, calibrate if necessary, and display.
; 
; EXAMPLES:
; 
; ;Display IR band 45 (11 micron ) as gain corrected counts
; file = 'apr21_s82160_s82860.uw'
; masview, file, 45
; 
; ;Display visible band 3 as raw counts with color scale
; masview, file, 3, /scale
; 
; ;Display IR band 31 (3.7 micron) as brightness temperature from 220 to 320 K
; masview, file, 31, /temp, /scale, range = [ 220, 320 ]
; 
; ;Display IR band 46 as radiance, return the band data, and save as a GIF file
; masview, file, 46, /rad, image = image, /gif, outfile = 'band46.gif'
; help, image
; 
; ;Display IR band 45 (11 micron) as brightness temperature, save Postscript
; loadct, 39
; masview, file, 45, /temp, /scale, /ps
; 
; ;Display IR band 45 (11 micron) as brightness temperature, custom Postscript
; loadct, 39
; masview, file, 45, /temp, /scale, /ps, /custom
; 
; MODIFICATION HISTORY:
;       Written by:     Liam Gumley, CIMSS/SSEC, 13 May 1996
;       Revised by:     Liam Gumley, CIMSS/SSEC, 30 Sep 1996
;                       Now reads MAS Exabyte, UW, or HDF formats.
;                       Many other enhancements.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 16 Oct 1996
;                       Brightness temperature differences can now be
;                       displayed using the DIFFERENCE keyword.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 27 Nov 1996
;                       Uses updated/fixed version of MAS_READ_UW.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 03 Dec 1996
;                       Added RECORD keyword, uses new version of MAS_READ_HDF.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 03 Mar 1997
;                       Added REVERSE keyword.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 06 Mar 1997
;                       Now uses JHU_APL library routine CBAR to color bar at
;                       the bottom of the image.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 13 Mar 1997
;                       Fixed color postscript output problem when number of
;                       colors is less than 256, added TABLE keyword.     
;       Revised by:     Liam Gumley, CIMSS/SSEC, 26 Mar 1997
;                       Removed ESRG library TOGGLE routine (V3.5 was screwy).
;                       Removed COLOR keyword (color postscript is now default).
;                       Removed TABLE keyword (now handled internally).
;                       Added CUSTOM keyword to allow custom Postscript setup.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 07 Apr 1997
;                       RECORD keyword now works with UW Intermediate format.
;                       UW Intermediate Quicklook format can now be read.
;       Revised by:     Liam Gumley, CIMSS/SSEC, 15 Apr 1997
;                       Now uses TVIMAGE instead of TVIM to display image,
;                       which allows the use of split color tables.
;                       Added NCOLORS and BOTTOM keywords for split color tables.
;                       Added CHARSIZE keyword.  A default vector font size
;                       of 6x9 is assumed. If your graphics text looks too big,
;                       do DEVICE,SET_CHARACTER_SIZE=[6,9] *after* a window is
;                       created.
;-
;-------------------------------------------------------------------------------

;- print help if requested

if keyword_set ( help ) then begin
  print, "NAME:
  print, "      MASVIEW
  print, "
  print, "PURPOSE:
  print, "      Extract and display a single band of MAS imagery.
  print, "      (1) Exabyte format as recorded in-flight (57344 bytes per block),
  print, "      (2) UW-Intermediate format (74200 bytes per block),
  print, "      (3) HDF Level 1B format,
  print, "      (4) UW-Intermediate Quicklook format.
  print, "
  print, "CATEGORY:
  print, "      MAS visualization.
  print, "
  print, "CALLING SEQUENCE:
  print, "      MASVIEW, FILE, BAND
  print, "
  print, "INPUTS:
  print, "      FILE:         Name of MAS 50 channel UW intermediate format file.
  print, "      BAND:         Band number to extract/display.
  print, "
  print, "OPTIONAL KEYWORD PARAMETERS:
  print, "      IMAGE         2D array in which the band data will be returned,
  print, "                    (default units are gain corrected counts,
  print, "                    but /RAW, /RADIANCE, or /TEMPERATURE may be used).
  print, "      /NODISPLAY    Do not display image (useful when used with IMAGE keyword),
  print, "                    (default is to display image).
  print, "      /RAW          Display raw counts,
  print, "                    (default is to display gain corrected counts).
  print, "      /RADIANCE     Display data in radiance units of mW/m2/sr/cm-1,
  print, "                    (default is to display gain corrected counts).
  print, "      /TEMPERATURE  Display data in brightness temperature units of Kelvin,
  print, "                    (default is to display gain corrected counts).
  print, "      /MICRON       When /RADIANCE is also set, display radiance in units of
  print, "                    W/m2/sr/micron, (default is mW/m2/sr/cm-1).
  print, "      /SCALE        Display a vertical color scale to the right of the image,
  print, "                    (default is do not display color scale).
  print, "      /RANGE        2 or 3 element vector indicating indicating physical range
  print, "                    over which to map the color scale. The third element of RANGE,
  print, "                    if specified, sets the step interval of the displayed color
  print, "                    scale.
  print, "                    (default range is minimum to maximum value of band data).
  print, "      /PS           Send display to a Postscript output file,
  print, "                    (default is to send display to current graphics window,
  print, "                    default output file name is masview.ps unless OUTFILE is set).
  print, "      /LANDSCAPE    Use landscape orientation for Postscript output,
  print, "                    (default is portrait).
  print, "      /CUSTOM       Customize Postscript output using popup dialog box.
  print, "      /GIF          Copy display from graphics window to a GIF file,
  print, "                    (default output file name is masview.gif unless OUTFILE is set).
  print, "      OUTFILE       Output file name for use with PS of GIF keywords,
  print, "                    (default output file name is masview.ps unless OUTFILE is set).
  print, "      /SCAN         Draw image axes in scan (Y) and pixel (X) coordinates,
  print, "                    (default is time (Y) and scan angle (X) coordinates).
  print, "      /NOAXIS       Do not draw axes around image,
  print, "                    (default is to draw axes).
  print, "      HISFILE       Name of HIS spectrum data file. A box representing the HIS
  print, "                    footprint will be printed at each HIS spectrum time,
  print, "                    (default is do not draw HIS footprints).
  print, "      /EQUAL        Histogram equalize the resulting image,
  print, "                    (default is do not histogram equalize the image).
  print, "                    Be careful if you use the IMAGE keyword to return the
  print, "                    image data - it will be histogram equalized.
  print, "      TIME          1D array in which MAS GPS time (UTC hours) is returned,
  print, "                    (default is do not return MAS time).
  print, "      HISTIME       1D array in which HIS GPS time (UTC hours) is returned
  print, "                    when HISFILE keyword is also set,
  print, "                    (default is do not return HIS time).
  print, "      DATE          1D array in which date is specified [DAY,MONTH,YEAR]
  print, "                    (default is to obtain date from data).
  print, "      /SKIP         Skip every second scanline
  print, "                    (default is to use every scanline).
  print, "      DIFFERENCE    Two element vector of the form [BAND1,BAND2], where
  print, "                    BAND1 and BAND2 are MAS band numbers between
  print, "                    26 and 50. The brightness temperature difference (K)
  print, "                    (BAND1-BAND2) will be displayed.
  print, "      RECORD        A two element vector containing the first and last
  print, "                    record numbers to be read, e.g. [2000,3000]
  print, "                    (default is to read all records). Currently works with
  print, "                    with UW-Intermediate format and HDF input files.
  print, "      /REVERSE      Reverse the color table (useful for IR bands).
  print, "      /HELP         If set, print help text.
  print, "      CHARSIZE      Scalar float value indicating character scaling factor
  print, "                    (default is 1.0).
  print, "      NCOLORS       Scalar integer value indicating number of colors in
  print, "                    color table to use (default is use all colors).
  print, "      BOTTOM        Scalar integer value indicating bottom color in color
  print, "                    table to use (default is zero).
  print, "
  print, "OUTPUTS:
  print, "      None.
  print, " 
  print, "COMMON BLOCKS:
  print, "      None.
  print, "
  print, "SIDE EFFECTS:
  print, "      None.
  print, "
  print, "RESTRICTIONS:
  print, "      Requires ESRG IDL library routines.
  print, "      Requires Liam Gumley user library routines.
  print, "
  print, "PROCEDURE:
  print, "      Read data from file, calibrate if necessary, and display.
  print, "
  print, "EXAMPLES:
  print, "
  print, ";Display IR band 45 (11 micron ) as gain corrected counts
  print, "file = 'apr21_s82160_s82860.uw'
  print, "masview, file, 45
  print, "
  print, ";Display visible band 3 as raw counts with color scale
  print, "masview, file, 3, /scale
  print, "
  print, ";Display IR band 31 (3.7 micron) as brightness temperature from 220 to 320 K
  print, "masview, file, 31, /temp, /scale, range = [ 220, 320 ]
  print, "
  print, ";Display IR band 46 as radiance, return the band data, and save as a GIF file
  print, "masview, file, 46, /rad, image = image, /gif, outfile = 'band46.gif'
  print, "help, image
  print, "
  print, ";Display IR band 45 (11 micron) as brightness temperature, save Postscript
  print, "loadct, 39
  print, "masview, file, 45, /temp, /scale, /ps
  print, "
  print, ";Display IR band 45 (11 micron) as brightness temperature, custom Postscript
  print, "loadct, 39
  print, "masview, file, 45, /temp, /scale, /ps, /custom
  print, "
  print, "MODIFICATION HISTORY:
  print, "      Written by:     Liam Gumley, CIMSS/SSEC, 13 May 1996
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 30 Sep 1996
  print, "                      Now reads MAS Exabyte, UW, or HDF formats.
  print, "                      Many other enhancements.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 16 Oct 1996
  print, "                      Brightness temperature differences can now be
  print, "                      displayed using the DIFFERENCE keyword.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 27 Nov 1996
  print, "                      Uses updated/fixed version of MAS_READ_UW.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 03 Dec 1996
  print, "                      Added RECORD keyword, uses new version of MAS_READ_HDF.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 03 Mar 1997
  print, "                      Added REVERSE keyword.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 06 Mar 1997
  print, "                      Now uses JHU_APL library routine CBAR to color bar at
  print, "                      the bottom of the image.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 13 Mar 1997
  print, "                      Fixed color postscript output problem when number of
  print, "                      colors is less than 256, added TABLE keyword.     
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 26 Mar 1997
  print, "                      Removed ESRG library TOGGLE routine (V3.5 was screwy).
  print, "                      Removed COLOR keyword (color postscript is now default).
  print, "                      Removed TABLE keyword (now handled internally).
  print, "                      Added CUSTOM keyword to allow custom Postscript setup.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 07 Apr 1997
  print, "                      RECORD keyword now works with UW Intermediate format.
  print, "                      UW Intermediate Quicklook format can now be read.
  print, "      Revised by:     Liam Gumley, CIMSS/SSEC, 15 Apr 1997
  print, "                      Now uses TVIMAGE instead of TVIM to display image,
  print, "                      which allows the use of split color tables.
  print, "                      Added NCOLORS and BOTTOM keywords for split color tables.
  print, "                      Added CHARSIZE keyword.  A default vector font size
  print, "                      of 6x9 is assumed. If your graphics text looks too big,
  print, "                      do DEVICE,SET_CHARACTER_SIZE=[6,9] *after* a window is
  print, "                      created.
  goto, done
endif

;- set margins

xmargin0 = !x.margin
ymargin0 = !y.margin
!x.margin = [ 11, 2 ]
!y.margin = [ 5, 7 ]
if keyword_set( scale ) then !y.margin(0) = 12
if keyword_set( noaxis ) then begin
  !x.margin = [ 2, 2 ]
  !y.margin = [ !y.margin(0) - 3, 7 ]
endif

;- set character scaling

if not keyword_set( charsize ) then charsize = 1.0

;- approximate central wavelengths for all 50 MAS bands (microns)

wvl = ['0.55', '0.66', '0.70', '0.75', '0.79', $ 
       '0.83', '0.87', '0.91', '0.95', '1.61', $
       '1.66', '1.72', '1.78', '1.83', '1.88', $ 
       '1.93', '1.98', '2.03', '2.08', '2.13', $
       '2.18', '2.23', '2.28', '2.33', '2.38', $ 
       '2.96', '3.12', '3.26', '3.44', '3.57', $
       '3.74', '3.89', '4.06', '4.16', '4.40', $ 
       '4.51', '4.65', '4.82', '4.99', '5.14', $ 
       '5.28', '8.54', '9.70', '10.48', '10.98', $
       '11.93', '12.80', '13.19', '13.66', '14.13'  ]

;- check difference keyword

if n_elements( difference ) gt 0 then begin
  if n_elements( difference ) eq 2 then begin
    band1 = difference( 0 )
    band2 = difference( 1 )
    if band1 lt 26 or band1 gt 50 or band2 lt 26 or band2 gt 50 then $
      message, 'BAND1 and BAND2 must be in the range 26-50'
    band = band1
  endif else begin
    message, 'DIFFERENCE must be a 2 element vector of the form [BAND1,BAND2]'
  endelse  
endif

;- check record keyword

if not keyword_set( record ) then record = 0

;- identify format of file

type = mas_file_type( file )

;- read data from file according to file type

case 1 of

  ; - Exabyte format
  
  type eq 1 : begin
    mas_read_exabyte, file, band, image, bb1c, bb2c, bb1t, bb2t, tback, $
      hr, mn, sc, scanline
    y = 0
    m = 0
    d = 0
  end

  ;- UW intermediate format

  type eq 2 or type eq 4 : begin
    mas_read_uw, file, band, image, bb1c, bb2c, bb1t, bb2t, tback, $
      hr, mn, sc, scanline, y, m, d, $
      heading, altitude, lat, lon, record = record
  end
  
  ;- HDF format
  
  type eq 3 : begin
    mas_read_hdf, file, band, image, hr, mn, sc, scanline, y, m, d, $
      record = record
  end
     
endcase

;- skip every other scanline if requested

if keyword_set( skip ) then begin
  sz = size( image )
  nx = sz( 1 )
  ny = sz( 2 )
  image = congrid( temporary( image ), nx, ny/2 )
  hr = congrid( hr, ny/2 )
  mn = congrid( mn, ny/2 )
  sc = congrid( sc, ny/2 )
  scanline = congrid( scanline, ny/2 )
  if type eq 1 or type eq 2 then begin
    bb1c = congrid( bb1c, ny/2 )
    bb2c = congrid( bb2c, ny/2 )
    bb1t = congrid( bb1t, ny/2 )
    bb2t = congrid( bb2t, ny/2 )
    tback = congrid( tback, ny/2 )
  endif
endif

;- put image in correct orientation

image = rotate( rotate( temporary( image ), 7 ), 2 )
sz = size( image )
nx = sz( 1 )
ny = sz( 2 )

;- create time array and title

time = float( hr ) + float( mn ) / 60.0 + float( sc ) / 3600.0
if n_elements( date ) eq 3 then begin
  d = date( 0 )
  m = date( 1 )
  y = date( 2 ) 
endif
title = string( m, d, y, hr( ny / 2 ), mn( ny / 2 ), sc( ny / 2 ), $
  band, strcompress( wvl( band - 1 ), /remove_all ), $
  format = '( "MAS ",' + $
  'i2.2, "/", i2.2, "/", i4, 1x, i2.2, ":", i2.2, ":", i2.2, " UTC!C",' + $
  '"Band", 1x, i2.2, 1x, "(", f5.2, " micron)!C" )' )
if n_elements( difference ) eq 2 then begin
  band1 = difference( 0 )
  band2 = difference( 1 )
  title = string( m, d, y, hr( ny / 2 ), mn( ny / 2 ), sc( ny / 2 ), $
    band1, band2, $
    strcompress( wvl( band1 - 1 ), /remove_all ), strcompress( wvl( band2 - 1 ), /remove_all ), $ 
    format = '( "MAS ",' + $
    'i2.2, "/", i2.2, "/", i4, 1x, i2.2, ":", i2.2, ":", i2.2, " UTC!C",' + $
    '"Bands", 1x, i2.2, "-", i2.2, 1x, "(", f5.2, "-", f5.2, " micron)!C" )' )
endif
title = strcompress( title )

;- get unique time codes, and chop off first and last entries

loc = uniq( time )
loc = loc( 1 : n_elements( loc ) - 2 )

;- do linear fit on time vs. scanline and create new time array

fit = poly_fit( scanline( loc ), time( loc ), 1 )
time = scanline * fit( 1 ) + fit( 0 )
time = reform( temporary( time ), ny )

;- if file is HDF, we are nearly ready to display

if type eq 3 then begin

  if keyword_set( temperature ) or n_elements( difference ) eq 2 then begin
    image = masbright( temporary( image ), band, /micron )
    title = title + 'Brightness Temperature (Kelvin)'
  endif else begin
    title = title + 'Radiance (W/m!U2!N/sr/micron)'
  endelse

  if n_elements( difference ) eq 2 then begin
    if not keyword_set( skip ) then skip = 0
    masview, file, difference( 1 ), /temperature, /nodisplay, skip = skip, $
      image = band2, record = record
    image = temporary( image ) - band2
  endif
  
  goto, display_image

endif

;- check if this is a port 1 or 2 band, or if raw counts were requested

if band le 25 then begin
  title = title + 'Gain Corrected Counts'
  goto, display_image
endif
if keyword_set( raw ) then begin
  title = title + 'Raw Counts'
  goto, display_image
endif

;- remove gain variation in IR band by subtracting blackbody counts

if ( not keyword_set( radiance ) ) and $
   ( not keyword_set( temperature ) ) and $
   ( n_elements( difference ) ne 2 ) then begin
  image = temporary( image ) - ( rebin( rotate( bb1c, 1 ), nx, ny ) - 3000 )
  title = title + 'Gain Corrected Counts'
  goto, display_image
endif

;- compute calibration slope and intercept

mascal, band, bb1c, bb2c, bb1t, bb2t, tback, eslope, eincpt

;- create calibrated radiance image

image = ( temporary( image ) * rebin( rotate( eslope, 1 ), nx, ny, /sample ) ) + $
  rebin( rotate( eincpt, 1 ), nx, ny, /sample )

if keyword_set( radiance ) and n_elements( difference ) ne 2 then begin

  ;- check if conversion to radiance units of W/m2/sr/micron was requested

  if keyword_set( micron ) then begin
    image = masplanck( masbright( temporary( image ), band ), band, /micron )
    title = title + 'Radiance (W/m!U2!N/sr/micron)'
  endif else begin 
    title = title + 'Radiance (mW/m!U2!N/sr/cm!U-1!N)'
  endelse
  
  goto, display_image

endif

;- convert radiance to brightness temperature

image = masbright( temporary( image ), band )
title = title + 'Brightness Temperature (Kelvin)'

;- check if difference image is required

if n_elements( difference ) eq 2 then begin
  masview, file, difference( 1 ), /temperature, /nodisplay, skip = skip, $
    image = band2
  image = temporary( image ) - band2
endif

display_image:

;- check nodisplay keyword

if keyword_set( nodisplay ) then goto, finish

;- check for color scale keyword

if keyword_set( scale ) then begin
  scale = 1
endif else begin
  scale = 0
endelse

;- turn on postscript if requested
; (ps_form is from David Fanning, http://www.dfanning.com)

if keyword_set( ps ) then begin

  ;- set defaults for ps_form

  defaults = { PS_FORM_INFO, $
               xsize:7.0, $                  ; The x size of the plot
               xoff:0.75, $                   ; The x offset of the plot
               ysize:9.5, $                  ; The y size of the plot
               yoff:0.75, $                    ; The y offset of the plot
               filename:'masview.ps', $      ; The name of the output file
               inches:1, $                    ; Inches
               color:1, $                    ; Color on
               bits_per_pixel:8, $           ; Bits per image pixel
               encapsulated:0,$              ; Encapsulated mode off
               landscape:0 }                 ; Portrait mode

  ;- reset defaults for landscape
  
  if keyword_set( landscape ) then begin
    defaults.xsize = 9.5
    defaults.xoff = 0.75
    defaults.ysize = 7.0
    defaults.yoff = 10.25
    defaults.landscape = 1
  endif

  ;- save current color table
  
  tvlct, red, green, blue, /get

  ;- call ps_form to set postscript parameters
  
  initialize = 1
  if keyword_set( custom ) then initialize = 0
  cancelbutton = 0
  forminfo = ps_form( local = defaults, cancel = cancelbutton, $
    initialize = initialize )
  
  ;- if cancel button was not hit in custom mode, then switch to postscript
  
  if not cancelbutton then begin 
    olddevice = !d.name 
    set_plot, 'PS', /interpolate
    device, /helvetica, font_size = 10, _extra=forminfo
    print, 'Creating Postscript output file ', forminfo.filename
  endif 
  
endif

;- check histogram equalization keyword

if keyword_set( equal ) then begin
  image = temporary( hist_equal( image, $
    minv = min( image ), maxv = max( image ) ) )
  range = [ 0, 255 ]
  scale = 0
  title = title + " (Histogram Equalized)"
endif

;- check range keyword

if keyword_set( range ) then begin
  imin = range( 0 )
  imax = range( 1 )
endif else begin
  imin = min( image, max = imax )
  range = [ imin, imax ]
endelse
if keyword_set( reverse ) then range = [ imax, imin ]

;- check color index keywords

if not keyword_set( ncolors ) then ncolors = !d.n_colors - 1
if not keyword_set( bottom ) then bottom = 1

;- display the image

sz = size( image )
nx = sz( 1 )
ny = sz( 2 )
pos = boxpos( aspect = float( nx ) / float( ny ) )
plot, [0], /nodata, xstyle = 4, ystyle = 4, pos = pos, title = title, $
  charsize = 1.25 * charsize
byte_image = bytscl( image, min = imin, max = imax, top = ncolors - 1 ) + 1B
if keyword_set( reverse ) then byte_image = ncolors - 1 - temporary( byte_image )
tvimage, byte_image, pos = pos, ncolors = ncolors, bottom = bottom

;- display color scale if requested

if keyword_set( scale ) then begin
  cmin = bottom
  cmax = bottom + ncolors - 1
  if keyword_set( ps ) then begin
    cmin = 0
    cmax = 255
  endif
  if keyword_set( reverse ) then begin
    cmin = bottom + ncolors - 1
    cmax = bottom
    if keyword_set( ps ) then begin
      cmin = 255
      cmax = 0
    endif
  endif
  y0 = 2.0 * float(!d.y_ch_size) / float(!d.y_vsize)
  y1 = y0 + 0.03
  print, y0, y1
  cbar, pos = [ pos( 0 ), y0, pos( 2 ), y1 ], $
    xticklen = 1.0, cmin = cmin , cmax = cmax, $
    charsize = 1.25 * charsize, $
    xrange = [ imin, imax ]
endif  

;- check for scan keyword

if keyword_set( scan ) then begin
  xrange = [ 0, nx - 1 ]
  xtitle = 'Pixel'
  yrange = [ scanline( 0 ), scanline( ny - 1 ) ]
  ytitle = 'Scan'
  ytickformat = '( i6 )'
endif else begin
  xrange = [ -42.96, 42.96 ]
  xtitle = 'Scan Angle (degrees)'
  yrange = [ time( 0 ), time( ny - 1 ) ]
  ytitle = 'Time (UTC hours)'
  ytickformat = '( f6.3 )'
endelse

;- draw axes

if not keyword_set( noaxis ) then begin
  axis, xaxis = 0, xtitle = xtitle, xrange = xrange, xstyle = 1, /save, $
    charsize = 1.25 * charsize
  axis, yaxis = 0, ytitle = ytitle, yrange = yrange, ystyle = 1, $
    ytickformat = ytickformat, /save, charsize = 1.25 * charsize
  axis, xaxis = 1, xrange = xrange, xstyle = 1, $
    xtickname = make_array( 30, /string, value = ' ' )
  axis, yaxis = 1, yrange = yrange, ystyle = 1, $
    ytickname = make_array( 30, /string, value = ' ' )
endif

;- overplot HIS times

if keyword_set( hisfile ) then begin

  ;- get HIS time
  
  get_his_time, hisfile, hour, minute, second
  histime = hour + minute / 60.0 + second / 3600.0
  
  ;- find HIS times within MAS time range
  
  loc = where( ( histime ge time( 0 ) ) and ( histime le time( ny - 1 ) ) )
  
  ;- plot HIS time label next to HIS FOV markers

  for i = 0, n_elements( histime( loc ) ) - 1 do begin
    label = string( hour( loc( i ) ), minute( loc( i ) ), second( loc( i ) ), $
    format = '( "HIS ", i2.2, ":", i2.2, ":", i2.2 )' )
    xyouts, 4.0, histime( loc( i ) ) - 1.1 / 3600.0, label, $
      color = 1, charsize = 1.25 * charsize
    plots, [ 0.0 ], [ histime( loc( i ) ) ], psym = 1, color = 1
  endfor
  
  ;- only return the HIS times within the MAS time window
  
  if keyword_set( histime ) then histime = histime( loc )
    
endif

;- end postscript output if needed
  
if keyword_set( ps ) then begin
  if not cancelbutton then begin
    device, /close
    set_plot, olddevice
    tvlct, red, green, blue
  endif
endif

;- write gif image if requested

if keyword_set( gif ) then begin
  if not keyword_set( outfile ) then outfile = 'masview.gif'
  tvlct, r, g, b, /get
  write_gif, outfile, tvrd(), r, g, b
endif

finish:

;- reset original margins

!x.margin = xmargin0
!y.margin = ymargin0
if not keyword_set( image ) then image = 0B

done:

end
