;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD__DEFINE
;
; PURPOSE:
;
;       The purpose of this object is to set up a map coordinate space for
;       for other objects. The program assumes you will use MAP_PROJ_INIT
;       to set up the map structure that is the basis for the map projection
;       space. Additionally, the program can accommodate up to 20 map overlay
;       objects. These are Catalyst objects that draw graphics in a map coordinate
;       data space (examples are MAP_OUTLINE and MAP_GRID in the Catalyst "graphics"
;       directory). The MAPCOORD object cannot, of course, draw map overlays. But,
;       the CatImage object is written in such a way that if a MAPCOORD object
;       is used to set up the data coordinate space, then if overlays are present
;       in the map object, each map overlay object will have its DRAW method called
;       in turn, in the order in which they are specified in the map overlay array.
;       See the keywords SET_MAP_OVERLAY and SET_MO_POSITION for details.
;
; AUTHORS:
;
;        FANNING SOFTWARE CONSULTING   BURRIDGE COMPUTING
;        1645 Sheely Drive             18 The Green South
;        Fort Collins                  Warborough, Oxon
;        CO 80526 USA                  OX10 7DN, ENGLAND
;        Phone: 970-221-0438           Phone: +44 (0)1865 858279
;        E-mail: davidf@dfanning.com   E-mail: davidb@burridgecomputing.co.uk
;
; CATEGORY:
;
;       Objects, Coordinates
;
; SYNTAX:
;
;       theObject = Obj_New("MAPCOORD")
;
; ARGUMENTS:
;
;       map_projection     The name or the reference number of a valid CGTP map projection. See
;                          the on-line documentation for MAP_PROJ_INIT for details. Default is 111,
;                          Lambert Azimuthal with spherical datum.
;
; KEYWORDS:
; 
;       CENTER_LATITUDE:   The center latitude of the map projection.
;       
;       CENTER_LONGITUDE:  The center longitude of the map projection.
;       
;       DATUM:             The name or index number of the DATUM. "Sphere" by default. See
;                          the on-line documentation for MAP_PROJ_INIT for details.
;                          
;       SPHERE_RADIUS:     The radius, in meters, of the sphere used as the DATUM.
;       
;       SEMIMAJOR_AXIS:    The distance, in meters, of the semi-major axis of the reference ellipsoid.
;       
;       SEMIMINOR_AXIS:    The distance, in meters, of the semi-minor axis of the reference ellipsoid.
;       
;       LIMIT:             A vector of limits for the map projection: [latmin, lonmin, latmax, lonmax].
;       
;       ZONE:              The zone of UTM and State Plane projections.
;       
;       Any additional keywords defined for MAP_PROJ_INIT are allowed. And, in addition to those, the following:
; 
;       GRID_OBJECT:       An overlay object, such as MAP_GRID, for drawing map grid lines. The MAPCOORD 
;                          object cannot draw grids, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 1.
;                       
;       LATLON_RANGES:     If this keyword is set, the XRANGE and YRANGE keywords are assumed to
;                          be in units of longitude and latitude, respectively. The map structure returned
;                          from Map_Proj_Init will be used to convert these values to the appropriate UV
;                          coordinates for internal storage.
;                       
;       OUTLINE_OBJECT:    An overlay object, such as MAP_OUTLINE, for drawing map outlines. The MAPCOORD 
;                          object cannot draw outlines, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 0.
; 
;       PARENT:            An object reference of the parent object. If provided, the MAPCOORD object
;                          will add itself to the parent with the COORDS_OBJECT keyword to SETPROPERTY.
;                          The parent should be a subclassed CATDATAATOM object.
;
;       POSITION:          A four-element array representing the position of the plot in the window.
;                          Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1]. The
;                          default is [0,0,1,1].
;                       
;       MAP_OVERLAY:       A 20-element object array of overlay objects. An overlay object
;                          is an object that draws graphics in a map coordinate data space.
;                          
;                          Note: Overlay objects must be written with a DRAW method, and they must
;                          have a MAP_OBJECT keyword in a SetProperty method. When they are added
;                          to the MAPCOORD object, the MAPCOORD object will be made their parent
;                          (so they are not accidentally destroyed) and the SetProperty method will
;                          be called with the MAP_STRUCTURE keyword set equal to the MAPCOORD map structure.
;       
;       OVERLAY_POSITION:  A  scalar or vector with the same number of elements as MAP_OVERLAY.
;                          This keyword is used to tell IDL where to store the object in the overlay
;                          object array. It should be a number in the range of 0 to 19. If undefined,
;                          the overlay object array is searched for invalid objects, and the overlay
;                          object is stored at the lowest index containing an invalid object.
;                          
;       XRANGE:            A two-element array representing the X range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the longitude range of -180 to 180 is used with the map structure
;                          to create the XRANGE array.
;
;       YRANGE:            A two-element array representing the Y range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the latitude range of -90 to 90 is used with the map structure
;                          to create the YRANGE array.
;
;       _EXTRA:            Any keywords appropriate for superclass INIT methods.
;
; SUPERCLASSES:
;
;       CATCOORD
;       CATATOM
;       CATCONTAINER IDLITCOMPONENT
;       IDL_CONTAINER
;
; CLASS_STRUCTURE:
;
;   class = { MAPCOORD, $
;             overlays: ObjArr(20), $                ; A storage location for map overlays.
;             map_projection_keywords: Ptr_New(), $  ; A storage location for MAP_PROJ_INIT keywords.
;             center_latitude: 0.0D, $               ; The latitude at the center of the map projection.
;             center_longitude:0.0D, $               ; The lontigude at the center of the map projection.
;             limit: Ptr_New(), $                    ; The limit of the map projection.
;             zone: 0, $                             ; The UTM zone of the map projection.
;             theDatums: Ptr_New(), $                ; Information about available map datums.
;             thisDatum: datumStruct, $              ; The particular datum structure for this map projection.
;             theProjections: Ptr_New(), $           ; Information about available map projections.
;             thisProjection: mapStruct, $           ; The particular map projection structure for this map projection.
;             INHERITS CATCOORD $                    ; The superclass object.
;           }
;
; MESSAGES:
;
;   None.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 28 December 2008.
;       Modified the way the map overlays were handled to be more flexible. 9 January 2009. DWF.
;       Reworked the program to make it more flexible with a wide variety of GTCP map projections. 2 June 2009.
;       Fixed some typos in the word "position". 10 June 2009. DWF.
;       Had to make some modifications to accommodate UTM and State Plane projections. 28 August 2009. DWF.
;       Removed the POSTITION, XRANGE, and YRANGE keywords from the SetProjection method, where they
;           didn't really belong. 22 November 2009. DWF.
;       Removed more vestiges of the MAP_STRUCTURE keyword in INIT method. 14 December 2009. DWF.
;       Added MAP_PROJECTION keyword to GetProperty and SetProperty methods. 16 December 2009. DWF
;       Added MAP_PROJ_KEYWORDS keyword to GetProperty and SetProperty methods. 22 December 2009. DWF.
;       If LIMIT is changed in the SetProperty method AND XRANGE and YRANGE keywords are not used, then
;          the xrange and yrange are updated to reflect the new limits. 28 Dec 2009. DWF.
;       The map projection was not being set correctly if the map projection was passed as
;          string value. 9 Feb 2010. DWF.
;       Setting the SPHERE_RADIUS keyword on some methods accessed a non-existent field "datum'
;          instead of the field "thisDatum". Fixed. 9 Feb 2010. DWF.
;       Mis-spelled SEMIMINOR_AXES keyword in the GetProperty method. Fixed. 10 March 2010. DWF.
;       Added ADD_GRID, ADD_OUTLINE, and DRAW_OVERLAYS keywords. 12 April 2010. DWF.
;       Modified the way map overlays are added with MAP_OVERLAY keyword to INIT and SetProperty
;           methods. 15 June 2010. DWF.
;       Added ZONE keyword to the GetProperty method. 22 June 2010. DWF.
;       Switch UTM datum from WGS84 to WALBECK to avoid UTM projection bug in all versions
;            of IDL prior to IDL 8.2, when it is suppose to be fixed. For more information,
;            see this article: http://www.idlcoyote.com/map_tips/utmwrong.php. 31 Oct 2011. DWF.
;-
;*******************************************************************************************
;* Copyright (c) 2008-2010, jointly by Fanning Software Consulting, Inc.                   *
;* and Burridge Computing. All rights reserved.                                            *
;*                                                                                         *
;* Redistribution and use in source and binary forms, with or without                      *
;* modification, are permitted provided that the following conditions are met:             *
;*     * Redistributions of source code must retain the above copyright                    *
;*       notice, this list of conditions and the following disclaimer.                     *
;*     * Redistributions in binary form must reproduce the above copyright                 *
;*       notice, this list of conditions and the following disclaimer in the               *
;*       documentation and/or other materials provided with the distribution.              *
;*     * Neither the name of Fanning Software Consulting, Inc. or Burridge Computing       *
;*       nor the names of its contributors may be used to endorse or promote products      *
;*       derived from this software without specific prior written permission.             *
;*                                                                                         *
;* THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. AND BURRIDGE COMPUTING   *
;* ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     *
;* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE          *
;* DISCLAIMED. IN NO EVENT SHALL FANNING SOFTWARE CONSULTING, INC. OR BURRIDGE COMPUTING   *
;* BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    *
;* DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;    *
;* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             *
;* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              *
;* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           *
;* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            *
;*******************************************************************************************
;
;+
; NAME:
;       MAPCOORD::DRAW
;
; PURPOSE:
;
;       This method sets up the map coordinate system.
;
; SYNTAX:
;
;       theObject -> Draw
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;       OVERLAYS:       If this keyword is set, any overlays in the object are drawn in order.
;-
;*****************************************************************************************************
PRO MapCoord::Draw, OVERLAYS=overlays, _EXTRA=extra
 
    mapStruct = self -> SetMapProjection()
    self -> CATCoord::Draw, _EXTRA=extra
    IF Keyword_Set(overlays) OR (self.draw_overlays EQ 1) THEN overlays = 1 ELSE overlays = 0
    IF overlays THEN BEGIN
        indices = Where(Obj_Valid(self.overlays), count)
        FOR j=0,count-1 DO self.overlays[indices[j]] -> Draw, /NOMAPDRAW
    ENDIF
    
END


;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::GETMAPSTRUCTURE
;
; PURPOSE:
;
;       This method allows the user to obtain the map projection structure.
;
; SYNTAX:
;
;       mapStruct = theObject -> GetMapStructure()
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
FUNCTION MapCoord::GetMapStructure
    RETURN, self -> SetMapProjection()
END


;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain MAPCOORD properties. Be sure
;       you ALWAYS call the superclass GETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> GetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;       CENTER_LATITUDE:   The center latitude of the map projection.
;       
;       CENTER_LONGITUDE:  The center longitude of the map projection.
;       
;       DATUM:          The name of the datum used for this map projection.
;
;       DRAW_OVERLAYS:  A flag that indicates if overlays are being drawn.
;       
;       LIMIT:             A vector of limits for the map projection: [latmin, lonmin, latmax, lonmax].
;       
;       GRID_OBJECT:    An object, such as MAP_GRID, for drawing map grid lines. This keyword is depreciated
;                       in favor of MAP_OVERLAY. If it is used, and if OVERLAY_POSITION is undefined,
;                       then OVERLAY_POSITION=1.
;       
;       LATLON_RANGES:  If this keyword is set, the XRANGE and YRANGE keywords are reported in units 
;                       of longitude and latitude, respectively, rather than the default UV coordinates.
;                       
;       MAP_OVERLAY:    Set this keyword to a named variable to return the object array containing map
;                       overlay objects. If you wish to return a specific map overlay object, then use the
;                       OVERLAY_POSITION keyword to select which object you wish to return.
;                      
;       MAP_PROJ_KEYWORDS: A structure containing the current map projection keywords and values.
;
;       MAP_PROJECTION: The name of the map projection.
; 
;       MAP_STRUCTURE:  A map projection structure (e.g., from MAP_PROJ_INIT) that allows the
;                       the coordinate conversion from Cartisian coordinates (otherwise known as
;                       UV coordinates) to longitude/latitude coordinates and visa versa.   
;
;       OUTLINE_OBJECT: An object, such as MAP_OUTLINE, for drawing map outlines. This keyword is depreciated
;                       in favor of MAP_OVERLAY. If it is used, and if OVERLAY_POSITION is undefined,
;                       then OVERLAY_POSITION=0.
;                       
;       OVERLAY_POSITION: Normally, if the MAP_OVERLAY keyword is used to obtain the map overlays, the
;                       entire map overlay array is returned. However, if you wish to obtain just one
;                       map overlay object, you can set this keyword to the index number of the object
;                       to return.
; 
;       POSITION:       A four-element array representing the position of the plot in the window.
;                       Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1]. The
;                       default is [0,0,1,1].
;                       
;       SPHERE_RADIUS:  The length (in meters) of the radius of the reference sphere.
;       
;       SEMINMAJOR_AXIS: The length (in meters) of the semi-major axis of the reference ellipsoid.
;
;       SEMINMINOR_AXIS: The length (in meters) of the semi-minor axis of the reference ellipsoid.
;
;       XRANGE:         A two-element array representing the X range of the map projection in a 2D
;                       Cartesian (x,y) coordinate system, unless the LATLON_RANGES keyword is set.
;
;       YRANGE:         A two-element array representing the Y range of the map projection in a 2D
;                       Cartesian (x,y) coordinate system, unless the LATLON_RANGES keyword is set.
;
;     _REF_EXTRA:       Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO MapCoord::GetProperty, $
    DRAW_OVERLAYS=draw_overlays, $
    GRID_OBJECT=grid_object, $
    LATLON_RANGES=latlon_ranges, $
    MAP_STRUCTURE=mapStruct, $
    OUTLINE_OBJECT=outline_object, $
    POSITION=position, $
    MAP_OVERLAY=map_overlay, $
    MAP_PROJ_KEYWORDS=map_proj_keywords, $
    MAP_PROJECTION=map_projection, $
    OVERLAY_POSITION=overlay_position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    DATUM=datum, $
    SPHERE_RADIUS=sphere_radius, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    LIMIT=limit, $
    ZONE=zone, $
    _REF_EXTRA=extraKeywords

   @cat_pro_error_handler
   
   ; Make sure the map structure is up to date by always calculating it in real-time.
   mapStruct = self -> SetMapProjection() 
   
   draw_overlays = self.draw_overlays
   position = self._position
   IF Arg_Present(xrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._xrange, self._yrange, MAP_STRUCTURE=mapStruct)
            xrange = Reform(llcoords[0,*])
      ENDIF ELSE xrange = self._xrange
   ENDIF
   IF Arg_Present(yrange) THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
            llcoords = Map_Proj_Inverse(self._xrange, self._yrange, MAP_STRUCTURE=mapStruct)
            yrange = Reform(llcoords[1,*])
      ENDIF ELSE yrange = self._yrange
   ENDIF
   IF Arg_Present(grid_object) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN grid_object = self.overlays[1] $
            ELSE grid_object = self.overlays[overlay_position]
   ENDIF
   IF Arg_Present(outline_object) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN outline_object = self.overlays[0] $
            ELSE outline_object = self.overlays[overlay_position]
   ENDIF
   IF Arg_Present(map_overlay) THEN BEGIN
        IF N_Elements(overlay_position) EQ 0 $
            THEN map_overlay = self.overlays $
            ELSE map_overlay = self.overlays[overlay_position]
   ENDIF
   
   ; Other keywords.
   center_latitude = self.center_latitude
   center_longitude = self.center_longitude
   IF N_Elements(*self.limit) NE 0 THEN limit = *self.limit
   map_projection = self.thisProjection.name
   IF Arg_Present(map_proj_keywords) THEN BEGIN
        IF Ptr_Valid(self.map_projection_keywords) THEN BEGIN
            IF N_Elements(*self.map_projection_keywords) NE 0 THEN $
                map_proj_keywords = *self.map_projection_keywords
        ENDIF
   ENDIF
   datum = self.thisDatum.name
   sphere_radius = self.thisDatum.semimajor_axis
   semimajor_axis = self.thisDatum.semimajor_axis
   semiminor_axis = self.thisDatum.semiminor_axis
   zone = self.zone
   
   ; Superclass keywords.
   IF (N_ELEMENTS (extraKeywords) GT 0) THEN self -> CATCOORD::GetProperty, _EXTRA=extraKeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::MapInfo
;
; PURPOSE:
;
;       The purpose of this routine is to return information about the current map
;       projection in an IDL structure variable. Fields of the structure will reflect
;       values that are used in MAP_PROJ_INIT to create a map structure.
;
; SYNTAX:
;
;       information = object -> MapInfo()
;
; ARGUMENTS:
;
;       None.
;       
; KEYWORDS: 
;       
;       None.
;
;-
;*****************************************************************************************************
FUNCTION MapCoord::MapInfo

   @cat_func_error_handler
   
   map_keywords = Create_Struct( $
       'projection', self.thisProjection.name, $
       'datum', self.thisDatum.name, $
       'gctp', 1, $
       'center_latitude', self.center_latitude, $
       'center_longitude', self.center_longitude )
   IF N_Elements(*self.limit) NE 0 THEN map_keywords = Create_Struct(map_keywords, 'limit', *self.limit)
   IF self.thisProjection.sphereOnly THEN BEGIN
       map_keywords = Create_Struct(map_keywords, 'sphere_radius', self.thisDatum.semimajor_axis)
   ENDIF ELSE BEGIN
       map_keywords = Create_Struct(map_keywords, $
            'semimajor_axis', self.thisDatum.semimajor_axis, $
            'semiminor_axis', self.thisDatum.semiminor_axis)        
   ENDELSE
   IF N_Elements(*self.map_projection_keywords) NE 0 THEN BEGIN
        keywords = *self.map_projection_keywords
        fields = Tag_Names(keywords)
        FOR j=0,N_Elements(fields) DO BEGIN
           map_keywords = Create_Struct(map_keywords, fields[j], keywords[j])
        ENDFOR
   ENDIF   
   RETURN, map_keywords
   
END



;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set the MAPCOORD object's properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra keywords!
;
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; KEYWORDS:
; 
;       DATUM:             The name or index number of the DATUM. "Sphere" by default. See
;                          the on-line documentation for MAP_PROJ_INIT for details.
;                          
;       DRAW_OVERLAYS:     If this keyword is set, any map overlays that are included within the mapCoord object
;                          are drawn at the time the DRAW method is called.
; 
;       CENTER_LATITUDE:   The center latitude of the map projection.
;       
;       CENTER_LONGITUDE:  The center longitude of the map projection.
;       
;       LIMIT:             A vector of limits for the map projection: [latmin, lonmin, latmax, lonmax].
;       
;       GRID_OBJECT:       An overlay object, such as MAP_GRID, for drawing map grid lines. The MAPCOORD 
;                          object cannot draw grids, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 1.
;                       
;       LATLON_RANGES:     If this keyword is set, the XRANGE and YRANGE keywords are assumed to
;                          be in units of longitude and latitude, respectively. The map structure returned
;                          from Map_Proj_Init will be used to convert these values to the appropriate UV
;                          coordinates for internal storage.
;                       
;       OUTLINE_OBJECT:    An overlay object, such as MAP_OUTLINE, for drawing map outlines. The MAPCOORD 
;                          object cannot draw outlines, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 0.
; 
;       PARENT:            An object reference of the parent object. If provided, the MAPCOORD object
;                          will add itself to the parent with the COORDS_OBJECT keyword to SETPROPERTY.
;                          The parent should be a subclassed CATDATAATOM object.
;
;       POSITION:          A four-element array representing the position of the plot in the window.
;                          Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1]. The
;                          default is [0,0,1,1].
;
;       MAP_PROJ_KEYWORDS: Some map projections do not support some keywords. This is a way to specify
;                          the map projection keywords you wish to use for a particular map projection.
;                          For example, the keywords TRUE_SCALE_LATITUDE or STANDARD_PAR1 are keywords
;                          like this. The MAP_PROJ_KEYWORDS can take an anonymous structure of map
;                          projection keyword:value pairs that will be passed to MAP_PROJ_INIT when
;                          a map structure is created in the program. If the fields are found in the
;                          current map keyword structure, their values are changed, otherwise the keyword:value
;                          pair is added to the map keyword structure. To eliminate all current keywords,
;                          pass a structure defined with a NULL field set to 1 (eg. {NULL:1}.
;                          
;       MAP_PROJECTION:    The name or the reference number of a valid CGTP map projection. See
;                          the on-line documentation for MAP_PROJ_INIT for details.
;                       
;       MAP_OVERLAY:       An object or object array of up to 20 overlay objects. An overlay object
;                          is an object that draws graphics in a map coordinate data space.
;                          
;                          Note: Overlay objects must be written with a DRAW method, and they must
;                          accept a MapCoord object which can provide a map structure for converting
;                          lat/lon values to XY values. Overlay objects always draw into an XY coordinate
;                          space.
;       
;       OVERLAY_POSITION:  A  scalar or vector with the same number of elements as MAP_OVERLAY.
;                          This keyword is used to tell IDL where to store the object in the overlay
;                          object array. It should be a number in the range of 0 to 19. If undefined,
;                          the overlay object array is searched for invalid objects, and the overlay
;                          object is stored at the lowest index containing an invalid object.
;
;       SPHERE_RADIUS:     The radius, in meters, of the sphere used as the DATUM.
;       
;       SEMIMAJOR_AXIS:    The distance, in meters, of the semi-major axis of the reference ellipsoid.
;       
;       SEMIMINOR_AXIS:    The distance, in meters, of the semi-minor axis of the reference ellipsoid.
;
;       XRANGE:            A two-element array representing the X range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the longitude range of -180 to 180 is used with the map structure
;                          to create the XRANGE array.
;
;       YRANGE:            A two-element array representing the Y range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the latitude range of -90 to 90 is used with the map structure
;                          to create the YRANGE array.
;
;     _EXTRA:              Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO MapCoord::SetProperty, $
    DRAW_OVERLAYS=draw_overlays, $
    GRID_OBJECT=grid_object, $
    LATLON_RANGES=latlon_ranges, $
    OUTLINE_OBJECT=outline_object, $
    PARENT=parent, $
    POSITION=position, $
    MAP_OVERLAY=map_overlay, $
    MAP_PROJ_KEYWORDS=map_proj_keywords, $
    MAP_PROJECTION=map_projection, $
    OVERLAY_POSITION=overlay_position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    DATUM=datum, $
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    LIMIT=limit, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    _EXTRA=extraKeywords
    
   @cat_pro_error_handler
   
   ; Are we changing the map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        projections = *self.theProjections
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) THEN BEGIN
            index = Where(projections.index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF
        self.thisProjection = projections[index]
   ENDIF
   
   ; Are we changing the map datum.
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self.theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self.theDatums)[index]
        ENDIF ELSE thisDatum = (*self.theDatums)[0 > datum < 19]
        self.thisDatum = thisDatum
   ENDIF
   
   ; Are there map projection keywords to deal with?
   IF N_Elements(map_proj_keywords) NE 0 THEN BEGIN
   
        ; Make the pointer a valid pointer, if necessary.
        IF ~Ptr_Valid(self.map_projection_keywords) THEN self.map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        
        ; Is there a NULL field in the current structure that means erase what is currently in the pointer?
        index = Where(Tag_Names(map_proj_keywords) EQ 'NULL', count)
        IF count GT 0 THEN BEGIN
            IF map_proj_keywords.(index) EQ 1 THEN self.map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
        ENDIF
        
        ; Add these fields to the structure, or modify the tag value if it is already present.
        IF N_Elements(*self.map_projection_keywords) GT 0 THEN BEGIN
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               index = Where(Tag_Names(*self.map_projection_keywords) EQ thisTag, count)
               IF count GT 0 THEN BEGIN
                   (*self.map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   *self.map_projection_keywords = Create_Struct(*self.map_projection_keywords, thisTag, map_proj_keywords.(j))
               ENDELSE
            ENDFOR
        ENDIF ELSE BEGIN
            
            ; Add all the tags, except for NULL tags
            ntags = N_Tags(map_proj_keywords)
            tags = Tag_Names(map_proj_keywords)
            FOR j=0,ntags-1 DO BEGIN
               thisTag = tags[j]
               IF thisTag EQ 'NULL' THEN Continue
               IF N_Elements(*self.map_projection_keywords) EQ 0 THEN BEGIN
                    count = 0
               ENDIF ELSE BEGIN
                    index = Where(Tag_Names(*self.map_projection_keywords) EQ thisTag, count)
               ENDELSE
               IF count GT 0 THEN BEGIN
                   (*self.map_projection_keywords).(index) = map_proj_keywords.(j)
               ENDIF ELSE BEGIN
                   IF N_Elements(*self.map_projection_keywords) EQ 0 THEN BEGIN
                        *self.map_projection_keywords = Create_Struct(thisTag, map_proj_keywords.(j))
                   ENDIF ELSE BEGIN
                        *self.map_projection_keywords = Create_Struct(*self.map_projection_keywords, $
                            thisTag, map_proj_keywords.(j))
                   ENDELSE
               ENDELSE
            ENDFOR
        ENDELSE
        
        ; For debugging purposes.
        ;Help, *self.map_projection_keywords, /Structure
   ENDIF
   
   IF N_Elements(center_latitude) NE 0 THEN self.center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self.center_longitude = center_longitude
   
   ; If you change the limit, you really also need to change the XRANGE and YRANGE.
   changedLimit = 0
   IF N_Elements(limit) NE 0 THEN BEGIN
        *self.limit = limit
        changedLimit = 1
   ENDIF
   IF N_Elements(draw_overlays) NE 0 THEN self.draw_overlays = Keyword_Set(draw_overlays)
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self.thisDatum.semimajor_axis = sphere_radius
      self.thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self.thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self.thisDatum.semiminor_axis = semiminor_axis
   
   ; Make sure the map structure is up to date.
   map_structure = self -> SetMapProjection() 

   IF N_Elements(parent) NE 0 THEN self -> CATCOORD::SetProperty, PARENT=parent
   IF N_Elements(position) NE 0 THEN self -> CATCOORD::SetProperty, POSITION=position
   IF N_Elements(xrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward(xrange, [-5000,5000], MAP_STRUCTURE=map_structure)
        xrange = Reform(uvcoords[0,*])   
      ENDIF
      self -> CATCOORD::SetProperty, XRANGE=xrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            xrange = map_structure.uv_box[[0,2]]
            self -> CATCOORD::SetProperty, XRANGE=xrange
      ENDIF
   ENDELSE
   IF N_Elements(yrange) NE 0 THEN BEGIN
      IF Keyword_Set(latlon_ranges) THEN BEGIN
        uvcoords = Map_Proj_Forward([-5000,5000], yrange, MAP_STRUCTURE=map_structure)
        yrange = Reform(uvcoords[1,*])     
      ENDIF
      self -> CATCOORD::SetProperty, YRANGE=yrange
   ENDIF ELSE BEGIN
      IF changedLimit THEN BEGIN
            yrange = map_structure.uv_box[[1,3]]
            self -> CATCOORD::SetProperty, YRANGE=yrange
      ENDIF
   ENDELSE
   IF N_Elements(outline_object) NE 0 THEN BEGIN
   
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 0 ELSE thisPosition = overlay_position
        
        ; Parent will be added to object in SetOverlay method.
        self -> SetOverlay, outline_object, thisPosition, /OVERWRITE
                
   ENDIF 
   IF N_Elements(grid_object) NE 0 THEN BEGIN
   
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 1 ELSE thisPosition = overlay_position

        ; Parent will be added to object in SetOverlay method.
        self -> SetOverlay, grid_object, thisPosition, /OVERWRITE
        
   ENDIF 
   IF N_Elements(map_overlay) NE 0 THEN BEGIN
   
        count = N_Elements(map_overlay)
        IF N_Elements(overlay_position) EQ 0 THEN BEGIN
            FOR j=0,count-1 DO BEGIN
                thisOverlay = map_overlay[j]
                IF Obj_Valid(thisOverlay) THEN self -> SetOverlay, thisOverlay
            ENDFOR
        ENDIF ELSE BEGIN
            FOR j=0,count-1 DO BEGIN
                self -> SetOverlay, map_overlay[j], overlay_position[j]
            ENDFOR
        ENDELSE
   ENDIF
   
   IF (N_ELEMENTS(extraKeywords) GT 0) THEN self -> CATCOORD::SetProperty,  _EXTRA=extraKeywords
   
   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::SetMapProjection
;
; PURPOSE:
;
;       The purpose of this routine is to define a GCTP map projection that can be used to
;       produce a valid map structure. (Map structures are normally ephemeral in nature and
;       are valid only until the next MAP_PROJ_INIT call is made. (See the article at
;       http://www.dfanning.com/map_tips/ephemeral.html for more information.)
;
; SYNTAX:
;
;       mapStruct = self -> SetMapProjection()
;
; ARGUMENTS:
;
;       map_projection     The name or the reference number of a valid CGTP map projection. See
;                          the on-line documentation for MAP_PROJ_INIT for details. 
;
; KEYWORDS:
; 
;       CENTER_LATITUDE:   The center latitude of the map projection.
;       
;       CENTER_LONGITUDE:  The center longitude of the map projection.
;       
;       DATUM:             The name or index number of the DATUM. 
;                          
;       LATLON_RANGES:     If this keyword is set, the XRANGE and YRANGE keywords are assumed to
;                          be in units of longitude and latitude, respectively. The map structure returned
;                          from Map_Proj_Init will be used to convert these values to the appropriate UV
;                          coordinates for internal storage.
;                       
;       LIMIT:             A vector of limits for the map projection: [latmin, lonmin, latmax, lonmax].
;       
;       SEMIMAJOR_AXIS:    The distance, in meters, of the semi-major axis of the reference ellipsoid.
;       
;       SEMIMINOR_AXIS:    The distance, in meters, of the semi-minor axis of the reference ellipsoid.
;       
;       SPHERE_RADIUS:     The radius, in meters, of the sphere used as the DATUM.
;       
;       ZONE:              UTM or State Plane zone.
;-
;*****************************************************************************************************
FUNCTION MapCoord::SetMapProjection, map_projection, $
    LATLON_RANGES=latlon_ranges, $
    POSITION=position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    DATUM=datum, $
    LIMIT=limit, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    ZONE=zone, $
    _EXTRA=extraKeywords

   @cat_func_error_handler
   
   ; Need a new map projection?
   IF N_Elements(map_projection) NE 0 THEN BEGIN
        ; Find the map projection.
        IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self.theProjections).name) EQ StrUpCase(map_projection))
            IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
        ENDIF
        IF (N_Elements(index) EQ 0) AND (N_Elements(map_projection) NE 0) THEN BEGIN
            index = Where((*self.theProjections).index EQ map_projection, count)
            IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
        ENDIF 
        thisProjection = (*self.theProjections)[index]
        self.thisProjection = thisProjection
   ENDIF
   
   ; Need a new datum?
   IF N_Elements(datum) NE 0 THEN BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase((*self.theDatums).name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = (*self.theDatums)[index]
        ENDIF ELSE thisDatum = (*self.theDatums)[0 > datum < 19]
        self.thisDatum = thisDatum
   ENDIF
   
   ; Other map keywords?
   IF N_Elements(center_latitude) NE 0 THEN self.center_latitude = center_latitude
   IF N_Elements(center_longitude) NE 0 THEN self.center_longitude = center_longitude
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
      self.thisDatum.semimajor_axis = sphere_radius
      self.thisDatum.semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN self.thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN self.thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(limit) NE 0 THEN *self.limit = limit
   IF N_Elements(zone) NE 0 THEN self.zone = zone
   IF N_Elements(extrakeywords) NE 0 THEN *self.map_projection_keywords = extrakeywords
   
   ; Extract the values you need to call MAP_PROJ_INIT.
   thisProjection = self.thisProjection.name
   sphereOnly = self.thisProjection.sphereOnly
   thisDatum = self.thisDatum.name
   semimajor_axis = self.thisDatum.semimajor_axis 
   semiminor_axis = self.thisDatum.semiminor_axis
   center_lon = self.center_longitude
   center_lat = self.center_latitude
   IF N_Elements(*self.limit) NE 0 THEN limit = *self.limit
   zone = self.zone
   IF N_Elements(*self.map_projection_keywords) NE 0 THEN keywords = *self.map_projection_keywords
   
   ; Center latitudes are not allowed in some projections. Here are the ones where
   ; they are prohibited.
   centerlatOK = 1
   badprojstr = ['GOODES HOMOLOSINE', 'STATE PLANE', 'MERCATOR', 'SINUSOIDAL', 'EQUIRECTANGULAR', $
      'MILLER CYLINDRICAL', 'ROBINSON', 'SPACE OBLIQUE MERCATOR A', 'SPACE OBLIQUE MERCATOR B', $
      'ALASKA CONFORMAL', 'INTERRUPTED GOODE', 'MOLLWEIDE', 'INTERRUPED MOLLWEIDE', 'HAMMER', $
      'WAGNER IV', 'WAGNER VII', 'INTEGERIZED SINUSOIDAL']
   void = Where(badprojstr EQ StrUpCase(thisProjection), count)
   IF count GT 0 THEN centerlatOK = 0
    
    ; UTM and State Plane projections have to be handled differently.
    IF (StrUpCase(thisProjection) EQ 'UTM') OR (StrUpCase(thisProjection) EQ 'STATE PLANE') THEN BEGIN
    
        CASE StrUpCase(thisProjection) OF
            'UTM': BEGIN
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self.thisDatum.(0), /GCTP, $
                    CENTER_LATITUDE=center_lat, CENTER_LONGITUDE=center_lon, ZONE=zone)
                END
            'STATE PLANE': BEGIN
                mapStruct = Map_Proj_Init(thisProjection, DATUM=self.thisDatum.(0), /GCTP, $
                    ZONE=zone)
                END
        ENDCASE
        
    ENDIF ELSE BEGIN
;print, 'Using the following limit in MapCoord::SetMapProjection: ', limit
        ; Call MAP_PROJ_INIT to get the map projection structure.
        CASE 1 OF
        
            centerLatOK AND sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, $
                    _EXTRA=keywords)
                END
                
            ~centerLatOK AND sphereOnly: BEGIN

                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SPHERE_RADIUS=semimajor_axis, $
                    LIMIT=limit, $
                    _EXTRA=keywords)
                END
                
            ~centerLatOK AND ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, $
                    _EXTRA=keywords)
                END
    
            centerLatOK AND ~sphereOnly: BEGIN
                mapStruct = Map_Proj_Init(thisProjection, /GCTP, $
                    CENTER_LATITUDE=center_lat, $
                    CENTER_LONGITUDE=center_lon, $
                    SEMIMAJOR_AXIS=semimajor_axis, $
                    SEMIMINOR_AXIS=semiminor_axis, $
                    LIMIT=limit, $
                    _EXTRA=keywords)
                END
        ENDCASE
   ENDELSE
        
    RETURN, mapStruct
END



;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::SetOverlay
;
; PURPOSE:
;
;       This method is used to set a map overlay object into the storage array.
;
; SYNTAX:
;
;       self -> SetOverlay, overlayObject, overlayPosition
;
; ARGUMENTS:
;
;       overlayObject:     A map overlay object. (Required)
;       
;       overlayPostition:  The position of the overlay object in the array. (Optional)
;                          If undefined, the map overlay object is stored in the first open
;                          position in the overlay array.
;
; KEYWORDS:
;
;       OVERWRITE:         The default behavior is to throw an error if the user asks to store
;                          an overlay object in a location that already contains a valid overlay
;                          object. If this keyword is set, however, the object will be stored at
;                          the requested location without an error being generated.
;-
;*****************************************************************************************************
PRO MapCoord::SetOverlay, theObject, thePosition, OVERWRITE=overwrite

   @cat_pro_error_handler

   ; Required parameters.
   IF N_Elements(theObject) EQ 0 THEN Message, 'A map overlay object is a required parameter.'
   IF Obj_Isa(theObject, 'CATATOM') EQ 0 THEN Message, 'The map overlay object must be a subclassed CATATOM object.'
   
   ; Check for valid storage locations.
   validLocations = Where(Obj_Valid(self.overlays) EQ 0, count)
   IF N_Elements(thePosition) EQ 0 THEN BEGIN
      IF count EQ 0 THEN Message, 'The are no more map overlay positions available.'
      thePosition = validLocations[0]
   ENDIF
   
   ; Confine the position to valid values.
   thisPosition = 0 > thePosition < (N_Elements(self.overlays) - 1)
   
   ; Is the requested position an open position?
   void = Where(validLocations EQ thisPosition, posOpen)
   IF posOpen GT 0 THEN BEGIN
       self.overlays[thisPosition] = theObject
   ENDIF ELSE BEGIN
       IF Keyword_Set(overwrite) THEN BEGIN
           self.overlays[thisPosition] -> RemoveParent, self
           self.overlays[thisPosition] = theObject
       ENDIF ELSE Message, 'The requested overlay position ' + StrTrim(thisPosition) + ' is already taken.'
   ENDELSE
   
   ; Add self as parent to this object.
   theObject -> AddParent, self
   
   ; Make this map structure the map structure for the overlay object.
   ; We really need to do this only for those objects that have map in their name.
   CASE Obj_Class(theObject) OF
        "MAP_OVERLAY": theObject -> SetProperty, MAP_OBJECT=self
        "MAP_GRID": theObject -> SetProperty, MAP_OBJECT=self
        "MAP_PLOTS": theObject -> SetProperty, MAP_OBJECT=self
        ELSE:
   ENDCASE
   
   
END



;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::CLEANUP
;
; PURPOSE:
;
;       This is the MAPCOORD object class destructor method.
;
; SYNTAX:
;
;       Called automatically when the object is destroyed.
;
; ARGUMENTS:
;
;       None.
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO MapCoord::CLEANUP

   @cat_pro_error_handler
   
   Ptr_Free, self.limit
   Ptr_Free, self.map_projection_keywords
   Ptr_Free, self.theDatums
   Ptr_Free, self.theProjections
   FOR j=0,19 DO BEGIN
     thisObject = self.overlays[j]
     IF Obj_Valid(thisObject) THEN thisObject -> RemoveParent, self
   ENDFOR
   Obj_Destroy, self.overlays
   
   self -> CATCOORD::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       MAPCOORD::INIT
;
; PURPOSE:
;
;       This is the MAPCOORD object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;       map_projection     The name or the reference number of a valid CGTP map projection. See
;                          the on-line documentation for MAP_PROJ_INIT for details. Default is 111,
;                          Lambert Azimuthal with spherical datum.
;
; KEYWORDS:
;
;       ADD_GRID:          If this keyword is set, a Map_Grid object is added to the mapCoord object.
;                          The Map_Grid object is configured with AUTODRAW on and the color "sky blue".
;
;       ADD_OUTLINE:       If this keyword is set, a Map_Outline object is added to the mapCoord object.
;                          The Map_Outline object is drawn with the color "indian red".
; 
;       CENTER_LATITUDE:   The center latitude of the map projection.
;       
;       CENTER_LONGITUDE:  The center longitude of the map projection.
;       
;       DATUM:             The name or index number of the DATUM. "Sphere" by default. See
;                          the on-line documentation for MAP_PROJ_INIT for details.
;                          
;       SPHERE_RADIUS:     The radius, in meters, of the sphere used as the DATUM.
;       
;       SEMIMAJOR_AXIS:    The distance, in meters, of the semi-major axis of the reference ellipsoid.
;       
;       SEMIMINOR_AXIS:    The distance, in meters, of the semi-minor axis of the reference ellipsoid.
;       
;       LIMIT:             A vector of limits for the map projection: [latmin, lonmin, latmax, lonmax].
;       
;       ZONE:              The zone of UTM and State Plane projections.
;       
;       Any additional keywords defined for MAP_PROJ_INIT are allowed. And, in addition to those, the following:
;
;       DRAW_OVERLAYS:     If this keyword is set, any map overlays that are included within the mapCoord object
;                          are drawn at the time the DRAW method is called.
; 
;       GRID_OBJECT:       An overlay object, such as MAP_GRID, for drawing map grid lines. The MAPCOORD 
;                          object cannot draw grids, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 1.
;                       
;       LATLON_RANGES:     If this keyword is set, the XRANGE and YRANGE keywords are assumed to
;                          be in units of longitude and latitude, respectively. The map structure returned
;                          from Map_Proj_Init will be used to convert these values to the appropriate UV
;                          coordinates for internal storage.
;                       
;       OUTLINE_OBJECT:    An overlay object, such as MAP_OUTLINE, for drawing map outlines. The MAPCOORD 
;                          object cannot draw outlines, but provides a logical place to store such
;                          an object. One advantage of storing the object here is that cleanup of
;                          the object is possible without the user having to do it themselves.
;                          This keyword is depreciated in favor of MAP_OVERLAY. If it is used,
;                          and the OVERLAY_POSITION keyword is not defined, then OVERLAY_POSITION
;                          is set to 0.
; 
;       PARENT:            An object reference of the parent object. If provided, the MAPCOORD object
;                          will add itself to the parent with the COORDS_OBJECT keyword to SETPROPERTY.
;                          The parent should be a subclassed CATDATAATOM object.
;
;       POSITION:          A four-element array representing the position of the plot in the window.
;                          Use normalized coordinates (0 to 1) in this order: [x0, y0, x1, y1]. The
;                          default is [0,0,1,1].
;                       
;       MAP_OVERLAY:       An object or object array of up to 20 overlay objects. An overlay object
;                          is an object that draws graphics in a map coordinate data space.
;                          
;                          Note: Overlay objects must be written with a DRAW method, and they must
;                          accept a MapCoord object which can provide a map structure for converting
;                          lat/lon values to XY values. Overlay objects always draw into an XY coordinate
;                          space.
;                          
;       OVERLAY_POSITION:  A  scalar or vector with the same number of elements as MAP_OVERLAY.
;                          This keyword is used to tell IDL where to store the object in the overlay
;                          object array. It should be a number in the range of 0 to 19. If undefined,
;                          the overlay object array is searched for invalid objects, and the overlay
;                          object is stored at the lowest index containing an invalid object. Note
;                          that overlay positions 0 and 1 are normally reserved for map outlines and
;                          map grids, respectively.
;                          
;       XRANGE:            A two-element array representing the X range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the longitude range of -180 to 180 is used with the map structure
;                          to create the XRANGE array.
;
;       YRANGE:            A two-element array representing the Y range of the map projection in a 2D
;                          Cartesian (x,y) coordinate system. These are sometimes called UV coordinates.
;                          If undefined, the latitude range of -90 to 90 is used with the map structure
;                          to create the YRANGE array.
;
;       _EXTRA:            Any keywords appropriate for superclass INIT methods.
;-
;*****************************************************************************************************
FUNCTION MapCoord::INIT, map_projection, $
    ADD_GRID=add_grid, $
    ADD_OUTLINE=add_outline, $
    DRAW_OVERLAYS=draw_overlays, $
    GRID_OBJECT=grid_object, $
    LATLON_RANGES=latlon_ranges, $
    OUTLINE_OBJECT=outline_object, $
    PARENT=parent, $
    POSITION=position, $
    MAP_OVERLAY=map_overlay, $
    OVERLAY_POSITION=overlay_position, $
    XRANGE=xrange, $
    YRANGE=yrange, $
    ; MAP_PROJ_INIT keywords (partial list)
    CENTER_LATITUDE=center_latitude, $
    CENTER_LONGITUDE=center_longitude, $
    DATUM=datum, $
    LIMIT=limit, $
    SEMIMAJOR_AXIS=semimajor_axis, $
    SEMIMINOR_AXIS=semiminor_axis, $
    SPHERE_RADIUS=sphere_radius, $
    ZONE=zone, $
    ; Superclass keywords that must be defined here because of the way I have had to use _EXTRA
    ; keywords to pick up map projection keywords.
    AUTO_DESTROY=auto_destroy, $
    INDEXED=indexed, $
    MEMORY_MANAGEMENT=memory_management, $
    NAME=name, $
    NO_COPY=no_copy, $
    UVALUE=uvalue, $
    _EXTRA=extraKeywords

   ; Set up error handler and call superclass INIT method
   @cat_func_error_handler
   
   ; Structures used in the object.
   datumStruct = { MAPCOORD_DATUM, index:0, name:"", semimajor_axis:0.0D, semiminor_axis:0.0D }
   void = { MAPCOORD_PROJECTION, name:"", index:0, sphereOnly:0 }

   ; Default map projection.
   IF N_Elements(map_projection) EQ 0 THEN BEGIN
        this_map_projection = 'Lambert Azimuthal'
        IF N_Elements(datum) EQ 0 THEN datum = 'SPHERE'
        map_projection = 111
   ENDIF
   
    projections=[ {MAPCOORD_PROJECTION, 'UTM', 101, 0 }, $  ; GCTP 101
                  {MAPCOORD_PROJECTION, 'State Plane', 102, 0 }, $  ; GCTP 102
                  {MAPCOORD_PROJECTION, 'Albers Equal Area', 103, 0 }, $  ; GCTP 103
                  {MAPCOORD_PROJECTION, 'Lambert Conformal Conic', 104, 0 }, $  ; GCTP 104
                  {MAPCOORD_PROJECTION, 'Mercator', 105, 0 }, $  ; GCTP 105
                  {MAPCOORD_PROJECTION, 'Polar Stereographic', 106, 0 }, $  ; GCTP 106
                  {MAPCOORD_PROJECTION, 'Polyconic', 107, 0 }, $  ; GCTP 107
                  {MAPCOORD_PROJECTION, 'Equidistant Conic A', 108, 0 }, $  ; GCTP 108
                  {MAPCOORD_PROJECTION, 'Transverse Mercator', 109, 0 }, $  ; GCTP 109
                  {MAPCOORD_PROJECTION, 'Stereographic', 110, 1 }, $  ; GCTP 110
                  {MAPCOORD_PROJECTION, 'Lambert Azimuthal', 111, 1 }, $  ; GCTP 111
                  {MAPCOORD_PROJECTION, 'Azimuthal', 112, 1 }, $  ; GCTP 112
                  {MAPCOORD_PROJECTION, 'Gnomonic', 113, 1 }, $  ; GCTP 113
                  {MAPCOORD_PROJECTION, 'Orthographic', 114, 1 }, $  ; GCTP 114
                  {MAPCOORD_PROJECTION, 'Near Side Perspective', 115, 1 }, $  ; GCTP 115
                  {MAPCOORD_PROJECTION, 'Sinusoidal', 116, 1 }, $  ; GCTP 116
                  {MAPCOORD_PROJECTION, 'Equirectangular', 117, 1 }, $  ; GCTP 117
                  {MAPCOORD_PROJECTION, 'Miller Cylindrical', 118, 1 }, $  ; GCTP 118
                  {MAPCOORD_PROJECTION, 'Van der Grinten', 119, 1 }, $  ; GCTP 119
                  {MAPCOORD_PROJECTION, 'Hotine Oblique Mercator A', 120, 0 }, $ ; GCTP 120
                  {MAPCOORD_PROJECTION, 'Robinson', 121, 1 }, $ ; GCTP 121
                  {MAPCOORD_PROJECTION, 'Space Oblique Mercator A', 122, 0 }, $ ; GCTP 122
                  {MAPCOORD_PROJECTION, 'Alaska Conformal', 123, 0 }, $ ; GCTP 123
                  {MAPCOORD_PROJECTION, 'Interrupted Goode', 124, 1 }, $  ; GCTP 124
                  {MAPCOORD_PROJECTION, 'Mollweide', 125, 1 }, $ ; GCTP 125
                  {MAPCOORD_PROJECTION, 'Interrupted Mollweide', 126, 1 }, $ ; GCTP 126
                  {MAPCOORD_PROJECTION, 'Hammer', 127, 1 }, $  ; GCTP 127
                  {MAPCOORD_PROJECTION, 'Wagner IV', 128, 1 }, $ ; GCTP 128
                  {MAPCOORD_PROJECTION, 'Wagner VII', 129, 1 }, $ ; GCTP 129
                  {MAPCOORD_PROJECTION, 'Integerized Sinusoidal', 131, 1 }, $ ; GCTP 131
                  {MAPCOORD_PROJECTION, 'Equidistant Conic B', 208, 0 }, $ ; GCTP 208
                  {MAPCOORD_PROJECTION, 'Hotine Oblique Mercator B', 220, 0 }, $ ; GCTP 220
                  {MAPCOORD_PROJECTION, 'Space Oblique Mercator B', 222, 0 }] ; GCTP 222

    ; Find the map projection.
    IF Size(map_projection, /TNAME) EQ 'STRING' THEN BEGIN
        index = Where(StrUpCase(projections.name[*]) EQ StrUpCase(map_projection))
        IF index[0] EQ -1 THEN Message, 'Cannot find map projection ' + map_projection + ' in the projection list.'
    ENDIF ELSE BEGIN
        index = Where(projections.index EQ map_projection, count)
        IF count EQ 0 THEN Message, 'Cannot find map projection index ' + StrTrim(map_projection,2) + ' in projection list.' 
    ENDELSE
    this_map_projection = projections[index]
   
   ; Find the datum.
   theDatums = Replicate(datumStruct, 20)
   theDatums[0] =  { MAPCOORD_DATUM, 0, 'Clark 1866', 6378206.4 , 6356583.8  }
   theDatums[1] =  { MAPCOORD_DATUM, 1, 'Clark 1880', 6378249.145, 6356514.86955  }
   theDatums[2] =  { MAPCOORD_DATUM, 2, 'Bessel', 6377397.155, 6356078.96284 }
   theDatums[3] =  { MAPCOORD_DATUM, 3, 'International 1967', 6378157.5, 6356772.2 }
   theDatums[4] =  { MAPCOORD_DATUM, 4, 'International 1909', 6378388.0, 6356911.94613  }
   theDatums[5] =  { MAPCOORD_DATUM, 5, 'WGS 72', 6378135.0, 6356750.519915  }
   theDatums[6] =  { MAPCOORD_DATUM, 6, 'Everst', 6377276.3452 , 6356075.4133 }
   theDatums[7] =  { MAPCOORD_DATUM, 7, 'WGS 66', 6378145.0 , 6356759.769356  }
   theDatums[8] =  { MAPCOORD_DATUM, 8, 'WGS 84', 6378137.0, 6356752.31414 }
   theDatums[9] =  { MAPCOORD_DATUM, 9, 'Airy', 6377563.396, 6356256.91  }
   theDatums[10] = { MAPCOORD_DATUM, 10, 'Modified Everest', 6377304.063, 6356103.039 }
   theDatums[11] = { MAPCOORD_DATUM, 11, 'Modified Airy', 6377340.189, 6356034.448  }
   theDatums[12] = { MAPCOORD_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   theDatums[13] = { MAPCOORD_DATUM, 13, 'Southeast Asia', 6378155.0, 6356773.3205 }
   theDatums[14] = { MAPCOORD_DATUM, 14, 'Australian National', 6378160.0, 6356774.719 }
   theDatums[15] = { MAPCOORD_DATUM, 15, 'Krassovsky', 6378245.0, 6356863.0188 }
   theDatums[16] = { MAPCOORD_DATUM, 16, 'Hough', 6378270.0 , 6356794.343479  }
   theDatums[17] = { MAPCOORD_DATUM, 17, 'Mercury 1960', 6378166.0, 6356784.283666  }
   theDatums[18] = { MAPCOORD_DATUM, 18, 'Modified Mercury 1968', 6378150.0, 6356768.337303 }
   theDatums[19] = { MAPCOORD_DATUM, 19, 'Sphere', 6370997.0, 6370997.0 }
   
   IF N_Elements(datum) EQ 0 THEN BEGIN
        thisDatum = theDatums[19] 
   ENDIF ELSE BEGIN
        IF Size(datum, /TNAME) EQ 'STRING' THEN BEGIN
            index = Where(StrUpCase(theDatums.name) EQ StrUpCase(datum))
            IF index[0] EQ -1 THEN Message, 'Cannot find datum ' + datum + ' in datum list.' 
            thisDatum = theDatums[index]
        ENDIF ELSE thisDatum = theDatums[0 > datum < 19]
   ENDELSE
   
   ; There is a bug in all versions of IDL up to IDL 8.1 apparently that
   ; produces the wrong result when a UTM projection is used in conjunction
   ; with a WGS84 datum (the most common datum used in this projection). Here
   ; we substitute the WALBECK datum, which is nearly identical to WGS84 are
   ; results in position errors of less than a meter typically.
   IF (StrUpCase(thisDatum.Name) EQ 'WGS 84') && $
      (StrUpCase(this_map_projection.Name) EQ 'UTM') && $
      (Float(!version.release) LE 8.2) THEN BEGIN
          Print, 'Switching UTM datum from WGS84 to WALBECK to avoid UTM projection bug.'
          thisDatum = { MAPCOORD_DATUM, 12, 'Walbeck', 6378137.0, 6356752.314245 }
   ENDIF
   
   ; Modify the radii?
   IF N_Elements(sphere_radius) NE 0 THEN BEGIN
        semimajor_axis = sphere_radius
        semiminor_axis = sphere_radius
   ENDIF
   IF N_Elements(semimajor_axis) NE 0 THEN thisDatum.semimajor_axis = semimajor_axis
   IF N_Elements(semiminor_axis) NE 0 THEN thisDatum.semiminor_axis = semiminor_axis
   IF N_Elements(zone) EQ 0 THEN zone = 1
   
      ; Default MAP_PROJ_INIT keywords.
   IF N_Elements(center_latitude) EQ 0 THEN self.center_latitude = 0 ELSE self.center_latitude = center_latitude
   IF N_Elements(center_longitude) EQ 0 THEN self.center_longitude = 0 ELSE self.center_longitude = center_longitude
   IF N_Elements(limit) NE 0 THEN self.limit = Ptr_New(limit) ELSE self.limit = Ptr_New(/ALLOCATE_HEAP)
   IF N_Elements(extraKeywords) NE 0 $
        THEN self.map_projection_keywords = Ptr_New(extraKeywords) $
        ELSE self.map_projection_keywords = Ptr_New(/ALLOCATE_HEAP)
   
   ; Load the object.
   self.theDatums = Ptr_New(theDatums)
   self.theProjections = Ptr_New(projections)
   self.thisDatum = thisDatum
   self.thisProjection = this_map_projection
   self.zone = zone
   
   ; Get the map structure.
   mapStruct = self -> SetMapProjection()
   
   ; Need ranges?
   IF N_Elements(xrange) EQ 0 THEN xrange = mapStruct.uv_box[[0,2]]
   IF N_Elements(yrange) EQ 0 THEN yrange = mapStruct.uv_box[[1,3]]
   
   ; Are the ranges in lat/lon space?
   IF Keyword_Set(latlon_ranges) THEN BEGIN
      uvcoords = Map_Proj_Forward(xrange, yrange, MAP_STRUCTURE=mapStruct)
      xrange = Reform(uvcoords[0,*])
      yrange = Reform(uvcoords[1,*])
   ENDIF

      ; Call the SUPERCLASS object INIT method.
   ok = self -> CATCOORD::INIT ($
        PARENT=parent, $
        POSITION=position, $
        XRANGE=xrange, $
        YRANGE=yrange, $
        AUTO_DESTROY=auto_destroy, $
        INDEXED=indexed, $
        MEMORY_MANAGEMENT=memory_management, $
        NAME=name, $
        NO_COPY=no_copy, $
        UVALUE=uvalue)
        
   IF ~ok THEN RETURN, 0
   
   ; Populate the object.
   IF Obj_Valid(outline_object) THEN BEGIN
        outline_object -> AddParent, self
        outline_object -> SetProperty, MAP_STRUCTURE=mapStruct
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 0 ELSE thisPosition = overlay_position
        self -> SetOverlay, outline_object, thisPosition
   ENDIF 

   IF Obj_Valid(grid_object) THEN BEGIN
        grid_object -> AddParent, self
        grid_object -> SetProperty, MAP_STRUCTURE=mapStruct
        IF N_Elements(overlay_position) EQ 0 THEN thisPosition = 1 ELSE thisPosition = overlay_position
        IF Obj_Valid(self.overlays[thisPosition]) $
            THEN self -> SetOverlay, grid_object $
            ELSE self -> SetOverlay, grid_object, thisPosition
   ENDIF 
   
   IF N_Elements(map_overlay) NE 0 THEN BEGIN
   
        count = N_Elements(map_overlay)
        IF N_Elements(overlay_position) EQ 0 THEN BEGIN
            FOR j=0,count-1 DO BEGIN
                thisOverlay = map_overlay[j]
                IF Obj_Valid(thisOverlay) THEN self -> SetOverlay, thisOverlay
            ENDFOR
        ENDIF ELSE BEGIN
            FOR j=0,count-1 DO BEGIN
                self -> SetOverlay, map_overlay[j], overlay_position[j]
            ENDFOR
        ENDELSE
   ENDIF
   
   ; Draw overlays?
   self.draw_overlays = Keyword_Set(draw_overlays)
   
   ; Add a Map_Grid object?
   IF Keyword_Set(add_grid) THEN BEGIN
       grid = Obj_New('Map_Grid', /AUTODRAW, MAP_OBJECT=self, COLOR='sky blue', PARENT=self)
       self -> SetProperty, GRID_OBJECT=grid
   ENDIF
   
   ; Add a Map_Outline object.
   IF Keyword_Set(add_outline) THEN BEGIN
       outline = Obj_New('Map_Outline', MAP_OBJECT=self, COLOR='Indian Red', PARENT=self)
       self -> SetProperty, OUTLINE_OBJECT=outline
   ENDIF
   
   ; Finished.
   self -> Report, /Completed
   RETURN, 1

END


;*****************************************************************************************************
;
; NAME:
;       MAPCOORD CLASS DEFINITION
;
; PURPOSE:
;
;       This is the structure definition code for the MAPCOORD object.
;
;*****************************************************************************************************
PRO MapCoord__DEFINE, class

   ; Structures used in the object.
   datumStruct = { MAPCOORD_DATUM, index:0, name:"", semimajor_axis:0.0D, semiminor_axis:0.0D }
   mapStruct =   { MAPCOORD_PROJECTION, name:"", index:0, sphereOnly:0 }

   class = { MAPCOORD, $
             draw_overlays: 0B, $                   ; A flag that indicates map overlays should be drawn.
             overlays: ObjArr(20), $                ; A storage location for map overlays.
             map_projection_keywords: Ptr_New(), $  ; A storage location for MAP_PROJ_INIT keywords.
             center_latitude: 0.0D, $               ; The latitude at the center of the map projection.
             center_longitude:0.0D, $               ; The lontigude at the center of the map projection.
             limit: Ptr_New(), $                    ; The limit of the map projection.
             zone: 0, $                             ; The UTM zone of the map projection.
             theDatums: Ptr_New(), $                ; Information about available map datums.
             thisDatum: datumStruct, $              ; The particular datum structure for this map projection.
             theProjections: Ptr_New(), $           ; Information about available map projections.
             thisProjection: mapStruct, $           ; The particular map projection structure for this map projection.
             INHERITS CATCOORD $                    ; The superclass object.
           }

END


