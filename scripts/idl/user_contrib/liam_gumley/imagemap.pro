;$Id$
;$Log: imagemap.pro,v $
;Revision 1.1  2006/05/19 19:19:32  paulv
;Initial checkin
;
;Revision 1.2  1998/06/16 15:08:16  gumley
;Added BASEIMAGE keyword allowing overlay of multiple images.
;
;Revision 1.1  1998/05/27 14:30:17  gumley
;Initial revision
;

PRO IMAGEMAP, Image, Latitude, Longitude, _EXTRA = Extra, $
  CURRENT = Current, MISSING = Missing, NEWIMAGE = NewImage, $
  XOFFSET = Xoffset, YOFFSET = Yoffset, XSIZE = Xsize, YSIZE = Ysize, $
  BASEIMAGE = BaseImage, NODISPLAY = NoDisplay, $
  RANGE = Range, BOTTOM = Bottom, NCOLORS = Ncolors

;-------------------------------------------------------------------------------
;    CHECK INPUT
;-------------------------------------------------------------------------------

;- Check number of arguments

IF N_PARAMS() NE 3 THEN MESSAGE, 'Syntax is IMAGEMAP, Image, Latitude, Longitude'

;- Check that each argument is defined

IF N_ELEMENTS( Image ) EQ 0 THEN MESSAGE, 'Argument 1 (IMAGE) is undefined'
IF N_ELEMENTS( Latitude ) EQ 0 THEN MESSAGE, 'Argument 2 (LATITUDE) is undefined'
IF N_ELEMENTS( Longitude ) EQ 0 THEN MESSAGE, 'Argument 3 (LONGITUDE) is undefined'


;- Check that arguments have identical dimensions

ImageInfo = SIZE( Image, /STRUCTURE )
LatitudeInfo = SIZE( Latitude, /STRUCTURE )
LongitudeInfo = SIZE( Longitude, /STRUCTURE )

IF ( ImageInfo.N_DIMENSIONS    NE LatitudeInfo.N_DIMENSIONS  OR $
     ImageInfo.N_DIMENSIONS    NE LongitudeInfo.N_DIMENSIONS OR $
     LatitudeInfo.N_DIMENSIONS NE LongitudeInfo.N_DIMENSIONS    ) THEN BEGIN
  HELP, Image, Latitude, Longitude
  MESSAGE, 'Arguments IMAGE, LATITUDE, LONGITUDE do not have identical dimensionality'
ENDIF

FOR i = 0, ImageInfo.N_DIMENSIONS - 1 DO BEGIN
  IF ( ImageInfo.DIMENSIONS[i]    NE LatitudeInfo.DIMENSIONS[i]  OR $
       ImageInfo.DIMENSIONS[i]    NE LongitudeInfo.DIMENSIONS[i] OR $
       LatitudeInfo.DIMENSIONS[i] NE LongitudeInfo.DIMENSIONS[i]    ) THEN BEGIN
    HELP, Image, Latitude, Longitude
    MESSAGE, 'Arguments IMAGE, LATITUDE, LONGITUDE do not have identical dimensions'
  ENDIF
ENDFOR


;- Check range of data in Latitude and Longitude arguments

Location = WHERE( Latitude LT -90.0 OR Latitude GT 90.0, Count )
IF Count GE 1 THEN MESSAGE, 'LATITUDE must be in the range [-90.0,90.0] degrees'
Location = WHERE( Longitude LT -180.0 OR Longitude GT 180.0, Count )
IF Count GE 1 THEN MESSAGE, 'LONGITUDE must be in the range [-180.0,180.0] degrees'

;- Check keyword flags

IF NOT KEYWORD_SET( Current ) THEN Current = 0
IF NOT KEYWORD_SET( NoDisplay ) THEN NoDisplay = 0

;- Check keyword values

IF N_ELEMENTS( Missing ) NE 1 THEN Missing = 0B
IF N_ELEMENTS( Range ) NE 2 THEN BEGIN
  Minv = MIN( Image, MAX = Maxv )
  Range = [ Minv, Maxv ]
ENDIF
IF N_ELEMENTS( Bottom ) NE 1 THEN Bottom = 0B
IF N_ELEMENTS( Ncolors ) NE 1 THEN Ncolors = !D.TABLE_SIZE - Bottom

;-------------------------------------------------------------------------------
;    COMPUTE MAP LIMITS
;-------------------------------------------------------------------------------

LatitudeMin = MIN( Latitude, MAX = LatitudeMax )
LongitudeMin = MIN( Longitude, MAX = LongitudeMax )
Limit = [ LatitudeMin, LongitudeMin, LatitudeMax, LongitudeMax ]

;- Check if LIMIT is defined in EXTRA keywords

IF N_ELEMENTS( Extra ) GT 0 THEN BEGIN

  Index = WHERE( STRUPCASE( STRMID( TAG_NAMES( Extra ), 0, 3 ) ) EQ 'LIM', Count )

  ;- If LIMIT keyword was found, check it for validity

  IF Count GT 0 THEN BEGIN
    Limit = Extra.Limit
    IF N_ELEMENTS( Limit ) NE 4 THEN $
	MESSAGE, 'Keyword LIMIT must be a 4 element vector of the form [LATMIN,LONMIN,LATMAX,LONMAX]'
    FOR i = 0, 2, 2 DO BEGIN
	IF Limit( i ) LT -90.0 OR Limit( i ) GT 90.0 THEN $
        MESSAGE, 'Keyword LIMIT latitude must be in the range [-90.0,90.0] degrees'
	IF Limit( i + 1 ) LT -180.0 OR Limit( i + 1 ) GT 180.0 THEN $
        MESSAGE, 'Keyword LIMIT longitude must be in the range [-180.0,180.0] degrees'
    ENDFOR
    IF Limit( 0 ) GT Limit( 2 ) THEN $
	MESSAGE, 'Keyword LIMIT latitude minimum must be less than maximum'
    IF Limit( 1 ) GT Limit( 3 ) THEN $
	MESSAGE, 'Keyword LIMIT longitude minimum must be less than maximum'
  ENDIF

ENDIF

;-------------------------------------------------------------------------------
;    RESAMPLE IMAGE TO MAP PROJECTION
;-------------------------------------------------------------------------------

;- Create map projection unless CURRENT keyword is set

IF NOT Current THEN BEGIN
  LatitudeCenter = 0.0
  LongitudeCenter = 0.5 * ( Limit( 1 ) + Limit( 3 ) )
  Rotation = 0.0
  MAP_SET, LatitudeCenter, LongitudeCenter, Rotation, $
    LIMIT = Limit, _Extra = Extra, /NOBORDER
ENDIF ELSE BEGIN
  IF !MAP.Projection LT 1 THEN MESSAGE, 'Map projection not established'
ENDELSE

;- Set number of samples and lines for warped image

NumSamples = !D.X_Size
NumLines = !D.Y_Size
IF ( !D.Name EQ 'PS' ) THEN BEGIN
  NumSamples = 640L
  NumLines = LONG( FLOAT( NumSamples ) * FLOAT( !D.Y_Size ) / FLOAT( !D.X_Size ) )
ENDIF

;- Create resampled byte scaled image

NewImage = REPLICATE( 0B, NumSamples, NumLines )
NormalCoords = CONVERT_COORD( Longitude, Latitude, /DATA, /TO_NORMAL )
NewSamples = NormalCoords( 0, * ) * ( NumSamples )
NewLines = NormalCoords( 1, * ) * ( NumLines )
NewImage( NewSamples, NewLines ) = $
  BYTSCL( Image, TOP = Ncolors - 1, MIN = Range( 0 ), MAX = Range( 1 ) ) + BYTE( Bottom ) + 1B

;- Extract portion of image which fits within map boundaries

Xlimit = !X.Window * ( NumSamples - 1 )
Ylimit = !Y.Window * ( NumLines - 1 )
NewImage = TEMPORARY( NewImage( Xlimit( 0 ) : Xlimit( 1 ), Ylimit( 0 ) : Ylimit( 1 ) ) )

;- Compute image offset and size in device coordinates

Edge = CONVERT_COORD( [ Xlimit(0), Xlimit(1) ] / FLOAT( NumSamples ), $
  [ Ylimit(0), Ylimit(1) ] / FLOAT( NumLines ), /NORMAL, /TO_DEVICE )
Xoffset = Edge( 0, 0 )
Yoffset = Edge( 1, 0 )
Xsize = Edge( 0, 1 ) - Edge( 0, 0 )
Ysize = Edge( 1, 1 ) - Edge( 1, 0 )

;- Fill holes in resampled image

Fill = DILATE( NewImage, REPLICATE( 1, 2, 2 ), /GRAY )
Location = WHERE( ( Fill GE 1B ) AND ( NewImage EQ 0B ), Count )
IF Count GE 1 THEN NewImage( Location ) = Fill( Location )

;- Fill remaining undefined areas of image with the missing value

Location = WHERE( NewImage EQ 0B, count )
IF ( count GE 1 ) AND ( Missing GT 0B) THEN NewImage( Location ) = Missing

;- Overlay new image on base image if required

IF N_Elements( BaseImage ) NE 0 THEN BEGIN
  Location = WHERE( NewImage EQ Missing, count )
  IF count ge 1 THEN Newimage( Location ) = BaseImage( Location )
ENDIF

;- Display image

IF NOT NoDisplay THEN TV, Newimage, Xoffset, Yoffset, XSIZE = Xsize, YSIZE = Ysize

;- Re-display map projection unless CURRENT keyword is set

IF NOT Current THEN BEGIN
  LatitudeCenter = 0.0
  LongitudeCenter = 0.5 * ( Limit( 1 ) + Limit( 3 ) )
  Rotation = 0.0
  MAP_SET, LatitudeCenter, LongitudeCenter, Rotation, $
    LIMIT = Limit, _Extra = Extra, /NOERASE
ENDIF

END
