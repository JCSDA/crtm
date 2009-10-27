;+
; NAME:
;       MAP_GRID__DEFINE
;
; PURPOSE:
;
;       This object is a wrapper for the MAP_GRID routine in IDL. It provides a simple 
;       way to allow map grids on images which use a MAPCOORD object to set up the map 
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
;       gridObject = Obj_New('Map_Grid')
;       Map_Set, /CYLINDRICAL
;       gridObject -> Draw
;       
; AUGUMENTS:
; 
;       parent:        The parent object.
;
; KEYWORDS:
; 
;  All of the following INIT keywords can be set and obtained using the SETPROPERTY and GETPROPERTY methods.
; 
;      BOX_AXES:        Set this keyword to draw a box-style axes around the map.
;      
;      CHARSIZE:        Set this keyword to the size of characters used for the labels. Default is 1.0.
;      
;      CLIP_TEXT:       Set this keyword to a zero value to turn off clipping of text labels. 
;                       By default, text labels are clipped. This keyword is ignored if the 
;                       BOX_AXES keyword is set. 
;                      
;      COLOR:           The name of the color to draw the grid lines in. Default: "charcoal".
;      
;      FILL_HORIZON:    Set this keyword to fill the current map horizon.
;      
;      HORIZON:         Set this keyword to draw the current map horizon.
;      
;      INCREMENT:       Set this keyword to the spacing between the graticle points.
;      
;      LABEL:           Set this keyword to an integer, n, that labels every n parallels and meridians.
;                       For example, LABEL=3 will label every 3rd line. Default is 1.
;                       
;      LATALIGN:        This keyword controls the alignment of the text baseline for latitude 
;                       labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;                       it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;                      
;      LATDEL:          Set this keyword equal to the spacing (in degrees) between parallels of 
;                       latitude in the grid. If this keyword is not set, a suitable value is 
;                       determined from the current map projection.
;                       
;      LATLAB:          The longitude at which to place latitude labels. The default is the center 
;                       longitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;                       
;      LATNAMES:        Set this keyword equal to an array specifying the names to be used for the 
;                       latitude labels. By default, this array is automatically generated in units 
;                       of degrees. The LATNAMES array can be either type string or any single numeric 
;                       type, but should not be of mixed type.When LATNAMES is specified, the LATS 
;                       keyword must also be specified.
;      
;      LATS:            Set this keyword equal to a one or more element vector of latitudes for which 
;                       lines will be drawn (and optionally labeled). If LATS is omitted, appropriate 
;                       latitudes will be generated based on the value of the (optional) LATDEL keyword. 
;                       If LATS is set to a single value, that latitude and a series of automatically 
;                       generated latitudes will be drawn (and optionally labeled).
;      
;      LINESTYLE:       Set this keyword to the type of linestyle desired. See Graphics Keywords in
;                       the on-line help for additional information.
; 
;      LONALIGN:        This keyword controls the alignment of the text baseline for longitude 
;                       labels. A value of 0.0 left justifies the label, 1.0 right justifies 
;                       it, and 0.5 centers it. This keyword is ignored if the BOX_AXES keyword is set.
;                      
;      LONDEL:          Set this keyword equal to the spacing (in degrees) between parallels of 
;                       longitude in the grid. If this keyword is not set, a suitable value is 
;                       determined from the current map projection.
;                       
;      LONTLAB:         The latitude at which to place longitude labels. The default is the center 
;                       latitude on the map. This keyword is ignored if the BOX_AXES keyword is set.
;                       
;      LONNAMES:        Set this keyword equal to an array specifying the names to be used for the 
;                       longitude labels. By default, this array is automatically generated in units 
;                       of degrees. The LONNAMES array can be either type string or any single numeric 
;                       type, but should not be of mixed type.When LONNAMES is specified, the LONS 
;                       keyword must also be specified.
;                       
;      LONS:            Set this keyword equal to a one or more element vector of longitudes for which 
;                       lines will be drawn (and optionally labeled). If LONS is omitted, appropriate 
;                       longitudes will be generated based on the value of the (optional) LONDEL keyword. 
;                       If LONS is set to a single value, that longitudes and a series of automatically 
;                       generated longitudes will be drawn (and optionally labeled).
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
; DEPENDENCIES:
;
;       The following programs (at least) are required from the Coyote Library:
;
;                     http://www.dfanning.com/programs/error_message.pro
;                     http://www.dfanning.com/programs/fsc_color.pro
;
; MODIFICATION HISTORY:
;
;       Written by David W. Fanning, 3 January 2009.
;       Added MAP_OBJECT keyword and depreciated MAP_STRUCTURE keyword. 30 May 2009. DWF.
;       Fixed a problem in setting MAP_OBJECT in SetProperty method. 12 June 2009. DWF.
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
PRO Map_Grid::Draw, _EXTRA=extrakeywords

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Find a map structure, if you can.
    IF Obj_Valid(self.map_object) $
        THEN self.map_object -> GetProperty, Map_Structure=mapStruct $
        ELSE mapStruct = *self.map_structure
    IF N_Elements(mapStruct) EQ 0 THEN Message, 'There is no valid map structure to use in producing output.'
    
    ; Draw the map grid.
    Map_Grid, $
        BOX_AXES=self.box_axes, $
        CLIP_TEXT=self.clip_text, $
        CHARSIZE=self.charsize, $
        COLOR=FSC_COLOR(self.color), $
        FILL_HORIZON=self.fill_horizon, $
        GLINESTYLE=self.linestyle, $
        GLINETHICK=self.thick, $
        HORIZON=self.horizon, $
        INCREMENT=*self.increment, $
        LABEL=self.label, $
        LATDEL=*self.latdel, $
        LATLAB=*self.latlab, $
        LATNAMES=*self.latnames, $
        LATS=*self.lats, $
        LONDEL=*self.londel, $
        LONLAB=*self.lonlab, $
        LONNAMES=*self.lonnames, $
        LONS=*self.lons, $
        MAP_STRUCTURE=mapStruct

    ; Draw children?
    self -> CatAtom::Draw, _EXTRA=extrakeywords
    
END ; -------------------------------------------------------------------------------------

    
PRO Map_Grid::GetProperty, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    _REF_EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    box_axes = self.box_axes
    clip_text = self.clip_text
    charsize = self.charsize
    color = self.color
    fill_horizon = self.fill_horizon
    linestyle = self.linestyle
    thick = self.thick
    horizon = self.horizon
    increment = self.increment
    label = self.label
    latdel = self.latdel
    latlab = self.latlab
    latnames = self.latnames
    lats = self.lats
    londel = self.londel
    lonlab = self.lonlab
    lonnames = self.lonnames
    lons = self.lons
    IF N_Elements(*self.map_structure) NE 0 THEN BEGIN
        map_structure = *self.map_structure
    ENDIF ELSE BEGIN
        IF Obj_Valid(self.map_object) THEN self.map_object -> GetProperty, MAP_STRUCTURE=map_structure
    ENDELSE
    IF Obj_Valid(self.map_object) THEN map_object = self.map_object
    
    IF N_Elements(extra) NE 0 THEN self -> CATATOM::GetProperty, _EXTRA=extra
    
END ; -------------------------------------------------------------------------------------

    
PRO Map_Grid::SetProperty, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
    _EXTRA=extra

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        void = Error_Message()
        RETURN
    ENDIF
    
    IF N_Elements(box_axes) NE 0 THEN self.box_axes = Keyword_Set(box_axes)
    IF N_Elements(clip_text) NE 0 THEN self.clip_text = clip_text
    IF N_Elements(charsize) NE 0 THEN self.charsize = charsize
    IF N_Elements(color) NE 0 THEN self.color = color
    IF N_Elements(fill_horizon) NE 0 THEN self.fill_horizon = Keyword_Set(fill_horizon)
    IF N_Elements(linestyle) NE 0 THEN self.linestyle = linestyle
    IF N_Elements(thick) NE 0 THEN self.thick = thick
    IF N_Elements(horizon) NE 0 THEN self.horizon = Keyword_Set(horizon)
    IF N_Elements(increment) NE 0 THEN *self.increment = increment
    IF N_Elements(label) NE 0 THEN self.label = label
    IF N_Elements(latdel) NE 0 THEN *self.latdel = latdel
    IF N_Elements(latlab) NE 0 THEN *self.latlab = latlab
    IF N_Elements(latnames) NE 0 THEN *self.latnames = latnames
    IF N_Elements(lats) NE 0 THEN *self.lats = lats
    IF N_Elements(londel) NE 0 THEN *self.londel = londel
    IF N_Elements(lonlab) NE 0 THEN *self.lonlab = lonlab
    IF N_Elements(lonnames) NE 0 THEN *self.lonnames = lonnames
    IF N_Elements(lons) NE 0 THEN *self.lons = lons
    IF N_Elements(map_structure) NE 0 THEN *self.map_structure = map_structure
    IF N_Elements(map_object) NE 0 THEN self.map_object = map_object

    IF N_Elements(extra) NE 0 THEN self -> CATATOM::SetProperty, _EXTRA=extra

END ; -------------------------------------------------------------------------------------


PRO Map_Grid::CLEANUP

    Ptr_Free, self.increment
    Ptr_Free, self.latdel
    Ptr_Free, self.latlab
    Ptr_Free, self.latnames
    Ptr_Free, self.lats
    Ptr_Free, self.londel
    Ptr_Free, self.lonlab
    Ptr_Free, self.lonnames
    Ptr_Free, self.lons
    Ptr_Free, self.map_structure
    IF Obj_Valid(self.map_object) THEN self.map_object -> RemoveParent, self
    
    self -> CatAtom::CLEANUP
END ; -------------------------------------------------------------------------------------


FUNCTION Map_Grid::INIT, parent, $
    BOX_AXES=box_axes, $
    CLIP_TEXT=clip_text, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FILL_HORIZON=fill_horizon, $
    LINESTYLE=linestyle, $
    THICK=thick, $
    HORIZON=horizon, $
    INCREMENT=increment, $
    LABEL=label, $
    LATDEL=latdel, $
    LATLAB=latlab, $
    LATNAMES=latnames, $
    LATS=lats, $
    LONDEL=londel, $
    LONLAB=lonlab, $
    LONNAMES=lonnames, $
    LONS=lons, $
    MAP_OBJECT=map_object, $
    MAP_STRUCTURE=map_structure, $
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
    self.box_axes = Keyword_Set(box_axes)
    self.fill_horizon = Keyword_Set(fill_horizon)
    self.horizon = Keyword_Set(horizon)
    
    SetDefaultValue, clip_text, 1
    SetDefaultValue, charsize, 1.0
    SetDefaultValue, color, 'White'
    SetDefaultValue, label, 1
    SetDefaultValue, linestyle, 1
    SetDefaultValue, thick, 1.0
    SetDefaultValue, latdel, 10.0
    SetDefaultValue, londel, 36.0
    self.clip_text = clip_text
    self.charsize = charsize
    self.color = color
    self.label = label
    self.linestyle = linestyle
    self.thick = thick
    
    IF N_Elements(increment) EQ 0 $
        THEN self.increment = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.increment = Ptr_New(increment)
    IF N_Elements(latdel) EQ 0 $
        THEN self.latdel = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.latdel = Ptr_New(latdel)
    IF N_Elements(londel) EQ 0 $
        THEN self.londel = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.londel = Ptr_New(londel)
    IF N_Elements(latlab) EQ 0 $
        THEN self.latlab = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.latlab = Ptr_New(latlab)
    IF N_Elements(lonlab) EQ 0 $
        THEN self.lonlab = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lonlab = Ptr_New(lonlab)
    IF N_Elements(latnames) EQ 0 $
        THEN self.latnames = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.latnames = Ptr_New(latnames)
    IF N_Elements(lonnames) EQ 0 $
        THEN self.lonnames = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lonnames = Ptr_New(lonnames)
    IF N_Elements(lats) EQ 0 $
        THEN self.lats = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lats = Ptr_New(lats)
    IF N_Elements(lons) EQ 0 $
        THEN self.lons = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.lons = Ptr_New(lons)
    IF N_Elements(map_structure) EQ 0 $
        THEN self.map_structure = Ptr_New(/ALLOCATE_HEAP) $
        ELSE self.map_structure = Ptr_New(map_structure)
    
    ; If a map object exists, simply put it in the right place. Do NOT
    ; make yourself a parent, because this object might contain YOU!
    IF Obj_Valid(map_object)THEN self.map_object = map_object
    
    RETURN, 1
    
END ; -------------------------------------------------------------------------------------


PRO Map_Grid__DEFINE, class

    class = { MAP_GRID, $
              box_axes: 0B, $              
              clip_text: 0B, $
              charsize: 0.0, $
              color: "", $
              fill_horizon: 0B, $
              linestyle: 0, $
              thick: 0, $
              horizon: 0B, $
              increment: Ptr_New(), $
              label: 0, $
              latdel: Ptr_New(), $
              latlab: Ptr_New(), $
              latnames: Ptr_New(), $
              lats: Ptr_New(), $
              londel: Ptr_New(), $
              lonlab: Ptr_New(), $
              lonnames: Ptr_New(), $
              lons: Ptr_New(), $
              map_structure: Ptr_New(), $
              map_object: Obj_New(), $
              INHERITS CatAtom $
            }

END ; -------------------------------------------------------------------------------------