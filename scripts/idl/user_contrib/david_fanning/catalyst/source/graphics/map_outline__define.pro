;+
; NAME:
;       MAP_OUTLINE__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for either MAP_CONTINENTS or MAP_GSHHS_SHORELINE.
;       It provides a simple way to allow map overlays on images which use a MAPCOORD
;       object to set up the map projection space. A map coordinate space must be in
;       effect at the time the Draw method of this object is used. Map outlines can be
;       provided from built-in IDL continental databases, or the GSHHS Shoreline
;       data base can be used. (For information on the GSHHS Shoreline data base, see
;       http://www.dfanning.com/map_tips/gshhs.html.)
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
;       outlineObject = Obj_New('Map_Outline')
;       Map_Set, /CYLINDRICAL
;       outlineObject -> Draw
;       
; AUGUMENTS:
; 
;       parent:        The parent object.
;
; COMMON KEYWORDS (used with either MAP_CONTINENTS of MAP_GSHHS_SHORELINE):
;
;       COLOR:         The name of a color (used with FSC_COLOR) to draw the output in.
;       
;       FILL:          Set this keyword to create a filled polygon output, rather than an outline.
;       
;       HIRES:         If this keyword is set, the high resolution dataset supplied with IDL is used
;                      with MAP_CONTINENTS. If this keyword is set, and the GSHHS keyword is set, and
;                      the FILENAME keyword is NOT used, then the default filename is "gshhs_h.b". Note
;                      that the high resolution dataset must be installed to be used.
;                    
;      MAP_OBJECT:      A MAPCOORD object or equivalent which had the ability to provide a map
;                       structure with a MAP_STRUCTURE keyword to the SetProperty method. If this
;                       object is supplied, it is always used in preference to MAP_STRUCTURE to 
;                       obtain the necessarey map structure for coordinate conversions.
;      
;      MAP_STRUCTURE:   Keyword depreciated in favor of MAP_OBJECT.
;                       
;                       A map projection structure (e.g., from MAP_PROJ_INIT) that allows the
;                       the coordinate conversion from Cartisian coordinates (otherwise known as
;                       UV coordinates) to longitude/latitude coordinates and visa versa. The problem
;                       with passing a MAP_STRUCTURE is that the structure is absolutely dependent on
;                       the *last* MAP_PROJ_INIT call made (from anywhere!) prior to the map structure
;                       being used. In other words, it is *exceedingly* easy for a map structure to go
;                       "stale". If you only have one map projection, and it never changes, and nothing
;                       else in your IDL session will use a map projection, you can get away with this.
;                       But it is FAR better to use the MAP_OBJECT keyword to provide a MAPCOORD object
;                       from which to obtain the map structure when it is needed. This is because in
;                       obtaining the map structure from a MAPCOORD object the MAP_PROJ_INIT method is
;                       invoked just prior to the map structure being returned. So, this keyword is
;                       depreciated in favor or the MAP_OBJECT keyword.
;
; MAP_CONTINENTS KEYWORDS (apply only if you are using MAP_CONTINENTS to draw outlines):
; 
;        COASTS:       Set this keyword if you want coasts to be drawn.
;        
;        CONTINENTS:   Set this keyword if you want continental outlines to be drawn. This will be
;                      set automatically if COASTS, COUNTRIES, RIVERS, AND USA keywords are all set
;                      to zero.
;                      
;        LINESTYLE:    Set to the type of linestyle in drawing outlines. Set to 0 or solid lines by default.
;        
;        RIVERS:       Set this keyword if you wish to draw rivers.
;        
;        T3D:          Set this graphics keyword if you wish to draw outlines use the T3D transformation matrix.
;        
;        USA:          Set this keyword if you wish do draw United States state boundaries.
;        
;        ZVALUE:       Set this keyword to the ZVALUE where the outlines should be drawn. Set to 0 by default.
;        
; MAP_GSHHS_SHORELINE KEYWORDS (apply only if you are using MAP_GSHHS_SHORELINE to draw outlines):
; 
;        FILENAME:     The root name of the GSHHS file to open. By default, "gshhs_l.b" unless the HIRES
;                      keyword is selected, in which case it will be "gshhs_h.b". The GSHHS file must be
;                      in a "resource" directory or in one of the directories on your IDL path.
;                      
;        GSHHS:        Set this keyword to use the GSHHS Shoreline data. The default is to use IDL's
;                      built-in database.  
;                      
;        LAND_COLOR:   The name of a color to be used for "land". Used with filled polygons 
;                      (e.g., the FILL keyword). By default, 'INDIAN RED'.
;        
;        LEVEL:        The polygon LEVEL. All polygons less than or equal to this value
;                      are drawn. 1-land, 2-lakes, 3-island in lake, 4-pond in island.
;                      By default, 2 (land and lake outlines).
;
;        MINAREA:      The minimum feature area. By default, 500 km^2. Polygons with areas less
;                      than this are not drawn.
;
;        OUTLINE:      Set this keyword to draw shorelines. Set by default if FILL=0.
;
;        WATER_COLOR:  The name of the water color. By default, "SKY BLUE".
;
; DEPENDENCIES:
;
;       The following programs (at least) are required from the Coyote Library:
;
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/find_resource_file.pro
;                     http://www.dfanning.com/programs/fsc_color.pro
;                     http://www.dfanning.com/programs/map_gshhs_shoreline.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 3 January 2009.
;       Fixed a problem in setting MAP_OBJECT in SetProperty method. 12 June 2009. DWF.
;       Fixed a problem when drawing filled outlines using IDL's map database when the
;           LAND_COLOR is different from the outline COLOR. 30 July 2009. DWF.
;       Previous problem introduced another. If LAND_COLOR is undefined, it is now set
;           to the same color as COLOR. 10 August 2009. DWF.
;       Circular parent references when passed a MAP_OBJECT was fixed, preventing memory
;          leakage. 30 August 2009. DWF.
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
PRO Map_Outline::Draw, _EXTRA=extrakeywords

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.map_object) $
        THEN self.map_object -> GetProperty, Map_Structure=mapStruct $
        ELSE mapStruct = *self.map_structure
    IF N_Elements(mapStruct) EQ 0 THEN Message, 'There is no valid map structure to use in producing output.'
    
    ; Do this in decomposed color, if possible.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
       DEVICE, GET_DECOMPOSED=theState
       Device, DECOMPOSED=1
    ENDIF
    
    ; Draw the appropriate map outline.
    IF self.gshhs THEN BEGIN
        rootName = File_Basename(self.filename)
        gshhsFileName = Find_Resource_File(rootName, SUCCESS=success)
        IF ~success THEN Message, 'Cannot file requested GSHHS Shoreline Data File.'
        Map_GSHHS_Shoreline, gshhsFileName, $ 
           COLOR=self.color, $                    
           FILL=self.fill, $                      
           LAND_COLOR=self.land_color, $          
           LEVEL=self.level, $                    
           MAP_PROJECTION=mapStruct, $  
           MINAREA=self.minarea, $                
           OUTLINE=self.outline, $                
           WATER_COLOR=self.water_color
    ENDIF ELSE BEGIN
        IF self.fill AND (self.color NE self.land_color) THEN BEGIN
            MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=FSC_COLOR(self.land_color), $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=1, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
            MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=FSC_COLOR(self.color), $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=0, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
        ENDIF ELSE BEGIN
            MAP_CONTINENTS, $
                COASTS=self.coasts, $
                COLOR=FSC_COLOR(self.color), $
                CONTINENTS=self.continents, $
                COUNTRIES=self.countries, $
                FILL=self.fill, $
                HIRES=self.hires, $
                MAP_STRUCTURE=mapStruct, $
                LINESTYLE=self.linestyle, $
                THICK=self.thick, $
                RIVERS=self.rivers, $
                USA=self.usa, $
                T3D=self.t3d, $
                ZVALUE=self.zvalue
         ENDELSE
    ENDELSE
    IF (!D.Flags AND 256) NE 0 THEN DEVICE, DECOMPOSED=theState
    
    ; Draw children?
    self -> CatAtom::Draw, _EXTRA=extrakeywords
END ; --------------------------------------------------------------------------------------------


PRO Map_Outline::GetProperty, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
    ZVALUE=zvalue, $
    _REF_EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get the properties.
    coasts = self.coasts
    color = self.color 
    continents = self.continents
    filename = self.filename
    fill = self.fill
    gshhs = self.gshhs
    hires = self.hires 
    land_color = self.land_color
    level = self.level
    linestyle = self.linestyle 
    IF Obj_Valid(self.map_object) THEN map_object = self.map_object
    IF N_Elements(*self.map_structure) NE 0 THEN BEGIN
        map_structure = *self.map_structure
    ENDIF ELSE BEGIN
        IF Obj_Valid(self.map_object) THEN self.map_object -> GetProperty, MAP_STRUCTURE=map_structure
    ENDELSE
    minarea = self.minarea
    outline = self.outline
    thick = self.thick
    rivers = self. rivers 
    usa = self.usa
    t3d = self.t3d 
    water_color = self.water_color
    zvalue = self.zvalue 
    
    IF N_Elements(extra) NE 0 THEN self -> CATATOM::GetProperty, _EXTRA=extra
    
END ; --------------------------------------------------------------------------------------------



PRO Map_Outline::SetProperty, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
    ZVALUE=zvalue, $
    _EXTRA=extra

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Set the properties.
    IF N_Elements(coasts) NE 0 THEN self.coasts = Keyword_Set(coasts)
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(continents) NE 0 THEN self.continents = Keyword_Set(continents)
    IF N_Elements(filename) NE 0 THEN self.filename = filename
    IF N_Elements(fill) NE 0 THEN self.fill = Keyword_Set(fill)
    IF N_Elements(gshhs) NE 0 THEN self.gshhs = Keyword_Set(gshhs)    
    IF N_Elements(hires) NE 0 THEN self.hires = Keyword_Set(hires)
    IF N_Elements(land_color) NE 0 THEN self.land_color = land_color
    IF N_Elements(level) NE 0 THEN self.level = level
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(map_object) NE 0 THEN self.map_object = map_object
    IF N_Elements(map_structure) NE 0 THEN *self.map_structure = map_structure 
    IF N_Elements(minarea) NE 0 THEN self.minarea = minarea
    IF N_Elements(outline) NE 0 THEN self.outline = outline
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(rivers) NE 0 THEN self. rivers = Keyword_Set(rivers)
    IF N_Elements(usa) NE 0 THEN self.usa = Keyword_Set(usa)
    IF N_Elements(t3d) NE 0 THEN self.t3d = Keyword_Set(t3d)
    IF N_Elements(water_color) NE 0 THEN self.water_color = water_color
    IF N_Elements(zvalue) NE 0 THEN self.zvalue = zvalue

    IF N_Elements(extra) NE 0 THEN self -> CATATOM::SetProperty, _EXTRA=extra

END ; --------------------------------------------------------------------------------------------


PRO Map_Outline::CLEANUP

    Ptr_Free, self.map_structure
    self -> CatAtom::CLEANUP
    IF Obj_Valid(self.map_object) THEN self.map_object -> RemoveParent, self
    
END ; --------------------------------------------------------------------------------------------


FUNCTION Map_Outline::INIT, parent, $
    COASTS=coasts, $
    COLOR=color, $
    CONTINENTS=continents, $
    COUNTRIES=countries, $
    FILENAME=filename, $
    FILL=fill, $
    GSHHS=gshhs, $
    HIRES=hires, $
    LAND_COLOR=land_color, $
    LEVEL=level, $
    LINESTYLE=linestyle, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    MINAREA=minarea, $
    OUTLINE=outline, $
    RIVERS=rivers, $
    T3D=t3d, $
    THICK=thick, $
    USA=usa, $
    WATER_COLOR=water_color, $
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
    IF (Keyword_Set(coasts) AND Keyword_Set(countries) AND Keyword_Set(rivers) $
        AND Keyword_Set(usa)) EQ 0 THEN continents = 1
    coasts = Keyword_Set(coasts)
    countries = Keyword_Set(countries)
    IF Keyword_Set(color) EQ 0 THEN color = 'white'
    IF N_Elements(land_color) EQ 0 THEN land_color = color
    continents = Keyword_Set(continents)
    fill = Keyword_Set(fill)
    gshhs = Keyword_set(gshhs)
    hires = Keyword_Set(hires)    
    IF N_Elements(level) EQ 0 THEN level = 2 ELSE level = 1 > level < 4
    IF Keyword_Set(linestyle) EQ 0 THEN linestyle = 0
    IF N_Elements(minArea) EQ 0 THEN minArea = 500.0 ; square kilometers.
    IF Keyword_Set(thick) EQ 0 THEN thick = 1
    rivers = Keyword_Set(rivers)
    usa = Keyword_Set(usa)
    t3d = Keyword_Set(t3d)
    IF N_Elements(water_color) EQ 0 THEN water_color = 'SKY BLUE'
    IF Keyword_Set(zvalue) EQ 0 THEN zvalue = 0.0
    
    IF gshhs AND N_Elements(filename) EQ 0 THEN BEGIN
        IF hires THEN filename = 'gshhs_h.b' ELSE filename = 'gshhs_l.b'
    ENDIF
    
    IF N_Elements(map_structure) NE 0 $
        THEN self.map_structure = Ptr_New(map_structure) $
        ELSE self.map_structure = Ptr_New(/ALLOCATE_HEAP)
    self.coasts = coasts
    self.color = color
    self.continents = continents
    self.countries = countries
    self.fill = fill
    IF N_Elements(filename) NE 0 THEN self.filename = filename
    self.hires = hires
    self.gshhs = gshhs
    self.land_color = land_color
    self.level = level
    self.linestyle = linestyle
    self.minArea = minarea
    self. rivers = rivers
    self.t3d = t3d
    self.thick = thick
    self.usa = usa
    self.water_color = water_color
    self.zvalue = zvalue
    
    ; If a map object exists, register interest in it and add yourself as an overlay.
    IF Obj_Valid(map_object) THEN self.map_object = map_object

    RETURN, 1
END ; --------------------------------------------------------------------------------------------


PRO MAP_OUTLINE__DEFINE, class

    class = { Map_Outline, $
              coasts: 0B, $                ; A flag that indicates coasts, island, lakes are drawn.
              continents: 0B, $            ; A flat that indicates continental outlines should be drawn.
              color: "", $                 ; The name of the color to draw outlines in.
              countries: 0B, $             ; A flag to indicate continental boundaries should be drawn.
              fill: 0B, $                  ; Set to do polygon fill instead of outline.
              filename: "", $              ; The root name of a GSHHS file to read from, eg. gshhs_h.b.
              hires: 0B, $                 ; Set to use high resolution maps rather than low resoluton maps.
              gshhs: 0B, $                 ; A flag to signify that a GSHHS file should be used.
              map_object: Obj_New(), $
              map_structure: Ptr_New(), $  ; A map structure to use.
              land_color: "", $            ; The name of the land color (for color fill).
              level: 0, $                  ; The level of GSHHS file to draw.
              linestyle: 0, $              ; The linestyle of the outline. By default, solid.
              minarea: 0.0D, $             ; Polygons less than the minimum area are not drawn.
              outline: 0B, $               ; Set if you want coastal outlines drawn.
              rivers: 0B, $                ; Set this keyword to draw rivers.
              t3d: 0B, $                   ; Set this keyword to draw in 3D space using T3D matrix.
              thick: 0, $                  ; The thickness of the outline
              usa: 0B, $                   ; Set this keyword to draw USA boundaries.
              water_color: "", $           ; The name of the water color.
              zvalue: 0.0D, $              ; The Z value. Default is 0.
              INHERITS CatAtom $
             }
            
END ; --------------------------------------------------------------------------------------------
 