;+
; NAME:
;       MAP_VECTOR__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for the CAT_ARROW routine in IDL. It provides a simple 
;       way to draw arrows or vectors on images which use a MAPCOORD object to set up the map 
;       projection space. A map coordinate space must be in effect at the time the 
;       Draw method of this object is used. 
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Graphics
;
; CALLING SEQUENCE:
;
;       mapCoord = Obj_New('MapCoord', 111, CENTER_LON=0, CENTER_LAT=90)
;       vectorObject = Obj_New('Map_Vector', lon, lat, u, v, mapCoord)
;       vectorObject -> Draw
;       ObjDestroy, mapCoord, vectorObject
;       
; AUGUMENTS:
; 
;       lon:           A vector (or scalar) containing the longitude location of the (x0,y0) end of a vector.
;       
;       lat:           A vector (or scalar) containing the latitude location of the (x0,y0) end of a vector.
;
;       u:             A vector (or scalar) containing the horizontal (X or longitude) component of the vector.
;
;       v:             A vector (or scalar) containing the vertical (Y or latitude) component of the vector.
;
;       mapCoordObj:   A map coordinate object which can return a map structure for converting coordinates
;                      to/from lat/lon coordinates to XY coordinates. Typically, a MAPCOORD object. An
;                      alternative way of specifying a map coordinate object is to use the MAP_OBJECT
;                      keyword. But don't do both. Note, this object is *not* destroyed when the MAP_VECTOR
;                      object is destroyed. You are responsible for destroying the map coordinate object.
;                      A map coordinate object is REQUIRED. So, if you don't specify this parameter, use the
;                      MAP_OBJECT keyword to pass this object into the program.
;
; KEYWORDS:
;     
;  All of the following INIT keywords can be set and obtained using the SETPROPERTY and GETPROPERTY methods.
;  
;      CLIP:            The coordinates of a rectangle used to clip the graphics output. 
;                       The rectangle is specified as a vector of the form [X0, Y0, X1, Y1], 
;                       giving coordinates of the lower left and upper right corners, 
;                       respectively. The default clipping rectangle is the plot window set
;                       up by the MAPCOORD object.
; 
;      COLOR:           The name of the color to draw the arrow or vector in. Default: "white".
;
;      HSIZE:           The value of this keyword sets the length of the arrowhead. See the documenation
;                       for the ARROW command for further explanation. Default is -0.35.
;
;      LENGTH:          The U and V vectors are mutiplied by LENGTH before they are used
;                       to calculate the (x1,y1) endpoint of the vector. By default, the length is set
;                       to 1/100th of the XRANGE of the MapCoord object. This means that the maximum
;                       length of a vector will be approximately LENGTH * SQRT(2).
;      
;      LINESTYLE:       Set this keyword to the type of linestyle desired. See Graphics Keywords in
;                       the on-line help for additional information. Default is 0, solid line.
; 
;      MAP_OBJECT:      A MAPCOORD object or equivalent which had the ability to provide a map
;                       structure with a GetMapStructure method. Don't use this keyword if you have
;                       passed a map coordinate object as a positional parameter. 
;      
;      NOCLIP:          Set this keyword to surpress clipping of the arrow. Set to 0 by default.
;
;      PARENT:          An object reference to an object that will be the parent of this object.
;      
;      SOLID:           Set this keyword to make a solid arrow, using polygon fills.
;      
;      THICK:           The thickness of the lines. Set to 1.0 by default.
;        
;      UVCOORDS:        Set this keyword if the LON and LAT arrays are specified in UV coordinates, rather than
;                       longitude and latitude coordinates.
;
; DEPENDENCIES:
;
;       The following programs (at least) are required from the Coyote Library:
;
;                     http://www.idlcoyote.com/programs/error_message.pro
;                     http://www.idlcoyote.com/programs/cgcolor.pro
;                     http://www.idlcoyote.com/programs/cgsymcat.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 14 June 2010.
;-
;
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO Map_Vector::Draw, NOMAPDRAW=nomapdraw, _EXTRA=extrakeywords

    ; Normally, when the DRAW method is called, the DRAW method of the MapCoord object is
    ; called first, so the map projection space can be set properly. If the map projection
    ; space is already set up, there is no need to call the DRAW method of the MapCoord 
    ; object. In this case, it would be appropriate to set the NOMAPDRAW keyword on this
    ; DRAW method.

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; You have to have data to plot. If not exit quietly.
    IF (N_Elements(*self.lon) EQ 0) OR (N_Elements(*self.lat) EQ 0) THEN RETURN
    IF (N_Elements(*self.u) EQ 0) OR (N_Elements(*self.v) EQ 0) THEN RETURN
    
    ; If the vectors don't all have the same number of elements, there is an error.
    IF N_Elements(*self.lon) NE N_Elements(*self.lat) THEN BEGIN
        Message, 'The number of elements in the latitude and longitude arrays must be the same.'
    ENDIF
    IF N_Elements(*self.u) NE N_Elements(*self.v) THEN BEGIN
        Message, 'The number of elements in the U and V arrays must be the same.'
    ENDIF
    IF N_Elements(*self.lon) NE N_Elements(*self.v) THEN BEGIN
        Message, 'The number of elements in the lon, lat, u, and v arrays must be the same.'
    ENDIF
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.map_object) THEN BEGIN
        mapStruct = self.map_object -> GetMapStructure() 
        IF ~Keyword_Set(nomapdraw) THEN self.map_object -> Draw
    ENDIF ELSE Message, 'There is no valid map object from which a map structure can be obtained.'

    ; If you have a map structure, then determine if the locations to plot
    ; are in lat/lon or UV coordinate space. The MapCoord object sets up
    ; a UV coordinate space. The locations to be plotted here are in lat/lon
    ; space, so they have to be converted to XY space to be plotted.
    IF N_Elements(mapStruct) NE 0 THEN BEGIN
    
        ; If the "lons and lats" are already in UVCOORDS, leave them alone.
        IF self.uvcoords THEN BEGIN
            lon = *self.lon
            lat = *self.lat
        ENDIF ELSE BEGIN
        
            ; Otherwise, convert them, since the map is *always* in UVCoords.
            uv = MAP_PROJ_FORWARD(*self.lon, *self.lat, MAP_STRUCTURE=mapStruct)
            lon = Reform(uv[0,*])
            lat = Reform(uv[1,*])
        ENDELSE
     ENDIF ELSE BEGIN
        lon = *self.lon
        lat = *self.lat
    ENDELSE
    
    ; Do we have to assign a value to length?
    IF ~Finite(self.length) THEN BEGIN
        self.map_object -> GetProperty, XRANGE=xr
        length = ABS(xr[1] - xr[0]) / 100.0
    ENDIF ELSE length = self.length
    
    ; Scale the U and V values by the length.
    uscaled = *self.u * length ;Scale_Vector(*self.u, 0, length)
    vscaled = *self.v * length ;Scale_Vector(*self.v, 0, length)
    
    ; If clip is not defined, then set it here.
    IF Total(self.clip) EQ 0 $
        THEN clip = [!X.CRange[0], !Y.CRange[0], !X.CRange[1], !Y.CRange[1]] $
        ELSE clip = self.clip
        
    ; Calculate the endpoint of the arrow and draw it.
    FOR j=0L,N_Elements(*self.u)-1 DO BEGIN
        x0 = (*self.lon)[j]
        y0 = (*self.lat)[j]
        x1 = x0 + uscaled[j]
        y1 = y0 + vscaled[j]
        Cat_IDLArrow, x0, y0, x1, y1, HSIZE=self.hsize, CLIP=clip, THICK=self.thick, $
           HTHICK=self.thick, LENGTH=length, COLOR=self.color, SOLID=self.solid, $
           _EXTRA=extrakeywords, /DATA, LINESTYLE=self.linestyle
    ENDFOR

    ; Draw children?
    self -> CatAtom::Draw, _EXTRA=extrakeywords

END ; -------------------------------------------------------------------------------------

    
PRO Map_Vector::GetProperty, $
    LON=lon, $
    LAT=lat, $
    U=U, $
    V=V, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    NOCLIP=noclip, $
    SOLID=solid, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    _REF_EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    clip = self.clip
    color = self.color
    hsize = self.hsize
    length = self.length
    linestyle = self.linestyle
    noclip = self.noclip
    solid  = self.solid
    thick = self.thick
    uvcoords = self.uvcoords
    lats = *self.lats
    lons = *self.lons
    u = *self.u
    v = *self.v
    map_object = self.map_object
    
    IF N_Elements(extra) NE 0 THEN self -> CATATOM::GetProperty, _EXTRA=extra
    
END ; -------------------------------------------------------------------------------------

    
PRO Map_Vector::SetProperty, $
    LON=lon, $
    LAT=lat, $
    U=U, $
    V=V, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    NOCLIP=noclip, $
    SOLID=solid, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    IF N_Elements(lon) NE 0 THEN BEGIN
    ;print, 'lon:', lon
        *self.lons = lon
    ENDIF
    IF N_Elements(lat) NE 0 THEN BEGIN
        *self.lat = lat
    ;print, 'lat:',  lats
    ENDIF
    IF N_Elements(u) NE 0 THEN BEGIN
    ;print, 'u:', u
        *self.u = u
    ENDIF
    IF N_Elements(v) NE 0 THEN BEGIN
        *self.v = v
    ;print, 'v',  v
    ENDIF

    IF N_Elements(clip) NE 0 THEN self.clip = clip
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(hsize) NE 0 THEN self.hsize = hsize
    IF N_Elements(length) NE 0 THEN self.length = length
    IF N_Elements(map_object) NE 0 THEN self.map_object = map_object
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(noclip) NE 0 THEN self.noclip = noclip
    IF N_Elements(solid) NE 0 THEN self.solid = Keyword_Set(solid)
    IF N_Elements(thick) NE 0 THEN self.thick = thick

    IF N_Elements(extra) NE 0 THEN self -> CATATOM::SetProperty, _EXTRA=extra

END ; -------------------------------------------------------------------------------------


PRO Map_Vector::CLEANUP

    Ptr_Free, self.lon
    Ptr_Free, self.lat
    Ptr_Free, self.u
    Ptr_Free, self.v
    
    self -> CatAtom::CLEANUP
END ; -------------------------------------------------------------------------------------


FUNCTION Map_Vector::INIT, lon, lat, u, v, mapCoordObj, $
    CLIP=clip, $
    COLOR=color, $
    HSIZE=hsize, $
    LENGTH=length, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    NOCLIP=noclip, $
    PARENT=parent, $
    SOLID=solid, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    _EXTRA=extra
    
    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Initialize superclass object,
     ok = self -> CatAtom::INIT(parent, _EXTRA=extra) 
     IF ~ok THEN RETURN, 0

    ; Default values.
    SetDefaultValue, color, 'White'
    SetDefaultValue, hsize, -0.35
    SetDefaultValue, linestyle, 0
    SetDefaultValue, noclip, 1
    SetDefaultValue, solid, 0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, uvcoords, 0B
    SetDefaultValue, length, !Values.F_NAN

    IF N_Elements(clip) NE 0 THEN self.clip = clip
    self.color = color
    self.hsize = hsize
    self.length = length
    self.linestyle = linestyle
    self.noclip = noclip
    self.solid = solid
    self.thick = thick
    self.uvcoords = uvcoords
    
    IF N_Elements(lon) EQ 0 $
        THEN self.lon = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lon = Ptr_New(lon)
    IF N_Elements(lat) EQ 0 $
        THEN self.lat = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lat = Ptr_New(lat)
        
    IF N_Elements(u) EQ 0 $
        THEN self.u = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.u = Ptr_New(u)
        
    IF N_Elements(v) EQ 0 $
        THEN self.v = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.v = Ptr_New(v)
        
    ; If a map object exists, simply put it in the right place. Do NOT
    ; make yourself a parent, because this object might contain YOU!
    IF Obj_Valid(mapCoordObj) THEN self.map_object = mapCoordObj
    IF Obj_Valid(map_object) AND Obj_Valid(self.map_object) THEN $
        Message, 'Cannot use both mapCoordObj parameter and MAP_OBJECT keyword.'
    IF Obj_Valid(map_object) AND ~Obj_Valid(self.map_object) THEN $
        self.map_object = map_object
        
    ; Make sure you have a valid map object at this point.
    IF ~Obj_Valid(self.map_object) THEN Message, 'A valid map object is required to create a MAP_VECTOR object.'
    
    RETURN, 1
    
END ; -------------------------------------------------------------------------------------


PRO Map_Vector__DEFINE, class

    class = { MAP_VECTOR, $
              lon: Ptr_New(), $      
              lat: Ptr_New(), $ 
              u: Ptr_New(), $
              v: Ptr_New(), $  
              length: 0.0D, $
              clip: DblArr(4),$   
              color: "", $
              hsize: 0.0, $
              linestyle: 0, $
              noclip: 0B, $
              solid: 0, $
              thick: 0, $
              uvcoords: 0B, $
              map_object: Obj_New(), $
              INHERITS CatAtom $
            }

END ; -------------------------------------------------------------------------------------