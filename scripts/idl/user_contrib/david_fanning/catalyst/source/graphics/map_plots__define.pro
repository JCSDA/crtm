;+
; NAME:
;       MAP_PLOTS__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for the PLOTS routine in IDL. It provides a simple 
;       way to draw polygons or lines on images which use a MAPCOORD object to set up the map 
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
;       mapCoordObj = Obj_New('MapCoord', 'Lambert Azimuthal', CENTER_LAT=90, CENTER_LON=0)
;       plotObject = Obj_New('Map_PlotS', lons, lats, mapCoordObj)
;       mapCoordObj -> Draw
;       plotObject -> Draw
;       
;       
; AUGUMENTS:
; 
;       lons:           A vector (or scalar) of longitude values to plot.
;       
;       lats:           A vector (or scalar) of latitude values to plot.
;
;       mapCoordObj:   A map coordinate object which can return a map structure for converting coordinates
;                      from lon/lat coordinates to XY coordinates. Typically, a MAPCOORD object. An
;                      alternative way of specifying a map coordinate object is to use the MAP_OBJECT
;                      keyword. But don't do both. Note, this object is *not* destroyed when the MAP_PLOTS
;                      object is destroyed. You are responsible for destroying the map coordinate object.
;                      A map coordinate object is REQUIRED. So, if you don't specify a this argument, use the
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
;      COLOR:           The name of the color to draw the lines or symbols in. Default: "white".
;      
;      LINESTYLE:       Set this keyword to the type of linestyle desired. See Graphics Keywords in
;                       the on-line help for additional information. Default is 0, solid line.
; 
;      MAP_OBJECT:      A MAPCOORD object or equivalent which had the ability to provide a map
;                       structure with a GetMapStructure method. Don't use this keyword if you have
;                       passed a map coordinate object as a positional parameter. 
;      
;      NOCLIP:          Set this keyword to supress clipping of the plot. Set to 0 by default.
;      
;      PARENT:          An object reference to an object that will be the parent of this object.
;      
;      PSYM:            The plotting symbol to use for the plot. Can use any symbol available in
;                       the Coyote Library routine cgSYMCAT. Set to 0 by default.
;                       
;      SYMSIZE:         Set this keyword to the size of symbols. Default is 1.0.
;      
;      T3D:             Set this graphics keyword if you wish to draw using the T3D transformation matrix.
;      
;      THICK:           The thickness of the lines. Set to 1.0 by default.
;        
;      ZVALUE:          Set this keyword to the ZVALUE where the output should be drawn. Set to 0 by default.
;        
;      UVCOORDS:        Set this keyword if the LONS and LATS are specified in UV (XY) coordinates, rather than
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
;       Written by David W. Fanning, 9 January 2009.
;       Added MAP_OBJECT keyword and depreciated MAP_STRUCTURE keyword. 30 May 2009. DWF.
;       Fixed a problem in setting MAP_OBJECT in SetProperty method. 12 June 2009. DWF.
;       Fixed a problem with converting lat/lon to UV coordinates in the DRAW method. 13 June 2009. DWF.
;       Circular parent references when passed a MAP_OBJECT was fixed, preventing memory
;          leakage. 30 August 2009. DWF.
;       Removed the MAP_STRUCTURE keyword, which caused MASSIVE problems and added a mapCoordObj
;          parameter. 9 November 2009. DWF.
;-
;
;******************************************************************************************;
;  Copyright (c) 2009, by Fanning Software Consulting, Inc.                                ;
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
PRO Map_PlotS::Draw, NOMAPDRAW=nomapdraw, _EXTRA=extrakeywords

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
    IF (N_Elements(*self.lons) EQ 0) OR (N_Elements(*self.lats) EQ 0) THEN RETURN
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.map_object) THEN BEGIN
        mapStruct = self.map_object -> GetMapStructure() 
        IF ~Keyword_Set(nomapdraw) THEN self.map_object -> Draw
    ENDIF ELSE Message, 'There is no valid map object from which a map structure can be obtained.'

    ; If you have a map structure, then determine if the points to plot
    ; are in lat/lon or UV coordinate space. The MapCoord object sets up
    ; a UV coordinate space. The values to be plotted here are in lat/lon
    ; space, so they have to be converted to XY space to be plotted.
    IF N_Elements(mapStruct) NE 0 THEN BEGIN
    
        ; If the "lons and lats" are already in UVCOORDS, leave them alone.
        IF self.uvcoords THEN BEGIN
            lons = *self.lons
            lats = *self.lats
        ENDIF ELSE BEGIN
        
            ; Otherwise, convert them, since the map is *always* in UVCoords.
            uv = MAP_PROJ_FORWARD(*self.lons, *self.lats, MAP_STRUCTURE=mapStruct)
            lons = Reform(uv[0,*])
            lats = Reform(uv[1,*])
        ENDELSE
     ENDIF ELSE BEGIN
        lons = *self.lons
        lats = *self.lats
    ENDELSE
    
    ; Accommodate cgSYMCAT symbols
    IF self.psym GE 0 THEN psym = cgSymCat(self.psym) ELSE psym = (-1) * cgSymCat(Abs(self.psym))
    
    ; If clip is not defined, then set it here.
    IF Total(self.clip) EQ 0 $
        THEN clip = [!X.CRange[0], !Y.CRange[0], !X.CRange[1], !Y.CRange[1]] $
        ELSE clip = self.clip

    ; Draw the lines or symbols.
    PlotS, lons, lats,  $
        CLIP=clip, $
        COLOR=cgColor(self.color), $
        LINESTYLE=self.linestyle, $
        NOCLIP=self.noclip, $
        PSYM=psym, $
        SYMSIZE=self.symsize, $
        T3D=self.t3d, $
        THICK=self.thick, $
        Z=self.zvalue

    ; Draw children?
    self -> CatAtom::Draw, _EXTRA=extrakeywords

END ; -------------------------------------------------------------------------------------

    
PRO Map_PlotS::GetProperty, $
    LONS=lons, $
    LATS=lats, $
    CLIP=clip, $
    COLOR=color, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    NOCLIP=noclip, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    ZVALUE=zvalue, $
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
    linestyle = self.linestyle
    noclip = self.noclip
    psym = self.psym
    symsize = self.symsize
    t3d = self.t3d
    thick = self.thick
    uvcoords = self.uvcoords
    zvalue = self.zvalue
    lats = *self.lats
    lons = *self.lons
    map_object = self.map_object
    
    IF N_Elements(extra) NE 0 THEN self -> CATATOM::GetProperty, _EXTRA=extra
    
END ; -------------------------------------------------------------------------------------

    
PRO Map_PlotS::SetProperty, $
    LONS=lons, $
    LATS=lats, $
    CLIP=clip, $
    COLOR=color, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    NOCLIP=noclip, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    ZVALUE=zvalue, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    IF N_Elements(lons) NE 0 THEN BEGIN
    ;print, 'lons:', lons
        *self.lons = lons
    ENDIF
    IF N_Elements(lats) NE 0 THEN BEGIN
        *self.lats = lats
    ;print, 'lats:',  lats
    ENDIF

    IF N_Elements(clip) NE 0 THEN self.clip = clip
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(map_object) NE 0 THEN self.map_object = map_object
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(noclip) NE 0 THEN self.noclip = noclip
    IF N_Elements(psym) NE 0 THEN self.psym = psym
    IF N_Elements(symsize) NE 0 THEN self.symsize = symsize
    IF N_Elements(t3d) NE 0 THEN self.t3d = t3d
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue

    IF N_Elements(extra) NE 0 THEN self -> CATATOM::SetProperty, _EXTRA=extra

END ; -------------------------------------------------------------------------------------


PRO Map_PlotS::CLEANUP

    Ptr_Free, self.lons
    Ptr_Free, self.lats
    
    self -> CatAtom::CLEANUP
END ; -------------------------------------------------------------------------------------


FUNCTION Map_PlotS::INIT, lons, lats, mapCoordObj, $
    CLIP=clip, $
    COLOR=color, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    NOCLIP=noclip, $
    PARENT=parent, $
    PSYM=psym, $
    SYMSIZE=symsize, $
    T3D=t3d, $
    THICK=thick, $
    UVCOORDS=uvcoords, $
    ZVALUE=zvalue, $
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
    SetDefaultValue, linestyle, 0
    SetDefaultValue, noclip, 1
    SetDefaultValue, psym, 0
    SetDefaultValue, symsize, 1.0
    SetDefaultValue, t3d, 0
    SetDefaultValue, thick, 1.0
    SetDefaultValue, uvcoords, 0B
    SetDefaultValue, zvalue, 0.0
    IF N_Elements(clip) NE 0 THEN self.clip = clip
    self.color = color
    self.linestyle = linestyle
    self.noclip = noclip
    self.psym = psym
    self.symsize = symsize
    self.t3d = t3d
    self.thick = thick
    self.uvcoords = uvcoords
    self.zvalue = zvalue
    
    IF N_Elements(lons) EQ 0 $
        THEN self.lons = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lons = Ptr_New(lons)
    
    IF N_Elements(lats) EQ 0 $
        THEN self.lats = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lats = Ptr_New(lats)
        
    ; If a map object exists, simply put it in the right place. Do NOT
    ; make yourself a parent, because this object might contain YOU!
    IF Obj_Valid(mapCoordObj) THEN self.map_object = mapCoordObj
    IF Obj_Valid(map_object) AND Obj_Valid(self.map_object) THEN $
        Message, 'Cannot use both mapCoordObj parameter and MAP_OBJECT keyword.'
    IF Obj_Valid(map_object) AND ~Obj_Valid(self.map_object) THEN $
        self.map_object = map_object
        
    ; Make sure you have a valid map object at this point.
    IF ~Obj_Valid(self.map_object) THEN Message, 'A valid map object is required to create a MAP_PLOTS object.'

    RETURN, 1
    
END ; -------------------------------------------------------------------------------------


PRO Map_PlotS__DEFINE, class

    class = { MAP_PLOTS, $
              lons: Ptr_New(), $      
              lats: Ptr_New(), $   
              clip: DblArr(4),$   
              color: "", $
              linestyle: 0, $
              noclip: 0B, $
              symsize: 0.0, $
              thick: 0, $
              psym: 0, $
              t3d: 0B, $
              zvalue: 0.0, $
              uvcoords: 0B, $
              map_object: Obj_New(), $
              INHERITS CatAtom $
            }

END ; -------------------------------------------------------------------------------------