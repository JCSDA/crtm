;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER
;
; PURPOSE:
;
;       The purpose of this program is to allow the user to view and construct
;       gpd map files for NSIDC image manipulation.
;
; AUTHORS:
;
;       David W. Fanning, Ph.D
;       National Snow and Ice Data Center (NSIDC)
;       NSIDC/CIRES University of Colorado
;       Boulder, CO 80309
;       E-Mail: fanning@nsidc.org
;
; CATEGORY:
;
;       Visualization, Map Projections.
;
; SYNTAX:
;
;       GPD_Viewer
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 14 January 2009.
;-
;*****************************************************************************************************
PRO GPD_Viewer::Update_Map, event

    @cat_pro_error_handler


    self.map_scaleID -> GetProperty, Value=mapScale
    self.map_ref_latID -> GetProperty, VALUE=lat
    self.map_ref_lonID -> GetProperty, VALUE=lon
    self.map_base_obj -> SetProperty, CENTER_LATITUDE=lat, CENTER_LONGITUDE=lon
    self -> Update_GPD_Box
    self -> Draw

END

PRO GPD_Viewer::Update_Map_Datum, event

    @cat_pro_error_handler


    self.datumID -> GetProperty, SELECTION=selected_datum
    CASE selected_datum OF
        'Sphere' : BEGIN
            semimajor_axis = '6370997.00'
            semiminor_axis = '6370997.00'
            datum = 'Sphere'
            END
        'EASE-Grid Sphere' : BEGIN
            semimajor_axis = '6371228.00'
            semiminor_axis = '6371228.00'
            datum = 'Sphere'
            END
        'Clarke 1866' : BEGIN
            semimajor_axis = '6378206.40'
            semiminor_axis = '6356583.80'
            datum = 'Clark 1866'
            END
        'WGS 84' : BEGIN
            semimajor_axis = '6378137.00'
            semiminor_axis = '6356752.31'
             datum = 'WGS 84'
            END
        'Hough' : BEGIN
            semimajor_axis = '6378270.00'
            semiminor_axis = '6356794.34'
             datum = 'Hough'
            END
    ENDCASE

    self.map_gpd_obj -> SetProperty, SEMIMAJOR_AXIS=semimajor_axis, SEMIMINOR_AXIS=semiminor_axis, DATUM=datum
    self.map_base_obj -> SetProperty, SEMIMAJOR_AXIS=semimajor_axis, SEMIMINOR_AXIS=semiminor_axis, DATUM=datum
    self -> Update_GPD_Box
    self -> Draw

END


PRO GPD_Viewer::Update_GPD_Box

    @cat_pro_error_handler

    self.mapUnitsPerGridID -> GetProperty, Value=mapUnitsPerGrid
    self.xgridSizeID -> GetProperty, Value=xgridSize
    self.ygridSizeID -> GetProperty, Value=ygridSize
    self.map_scaleID -> GetProperty, Value=mapScale
    self.grid_ref_latID -> GetProperty, Value=glat
    self.grid_ref_lonID -> GetProperty, Value=glon
    
    
    ; Calculate the new location of the grid center with respect to 
    ; the map projection.
    self.map_base_obj -> GetProperty, MAP_STRUCTURE=bmap
    uvcenter = MAP_PROJ_FORWARD(glon, glat, MAP_STRUCTURE=bmap)

    ; Calculate the locations of the box, with respect to this
    ; new grid center.
    xorigin = (xgridSize/2) * mapUnitsPerGrid * (-1) * mapScale
    yorigin = (ygridSize/2) * mapUnitsPerGrid * (-1) * mapScale
    xorigin = xorigin + uvcenter[0]
    yorigin = yorigin + uvcenter[1]
    xend = xorigin + (xgridSize * mapUnitsPerGrid * mapScale)
    yend = yorigin + (ygridSize * mapUnitsPerGrid * mapScale)
    xrange = [xorigin, xend]
    yrange = [yorigin, yend]
    x = [xrange[0], xrange[0], xrange[1], xrange[1], xrange[0]]
    y = [yrange[0], yrange[1], yrange[1], yrange[0], yrange[0]]
;    print, 'UV lons: ', x
;    print, 'UV lats: ', y

    ; These are the lat/lon values in the gpd's map projection.
    self.map_gpd_obj -> GetProperty, MAP_STRUCTURE=map
    ll = Map_Proj_Inverse(x, y, MAP_STRUCTURE=map)
    self.gpd_box -> SetProperty, LONS=ll[0,*], LATS=ll[1,*]
    
    ; These are the lat/lon values in the map's map projection.
    ; These should be what is displayed in the GUI.
    self.map_base_obj -> GetProperty, MAP_STRUCTURE=bmap
    ll = Map_Proj_Inverse(x, y, MAP_STRUCTURE=bmap)
    
    ; Display the information on the GUI.
    self.lon_ul -> SetProperty, Value=String(ll[0,1], Format='(F0.3)')
    self.lon_ur -> SetProperty, Value=String(ll[0,2], Format='(F0.3)')
    self.lon_lr -> SetProperty, Value=String(ll[0,0], Format='(F0.3)')
    self.lon_ll -> SetProperty, Value=String(ll[0,3], Format='(F0.3)')
    
    self.lat_ul -> SetProperty, Value=String(ll[1,1], Format='(F0.3)')
    self.lat_ur -> SetProperty, Value=String(ll[1,2], Format='(F0.3)')
    self.lat_lr -> SetProperty, Value=String(ll[1,0], Format='(F0.3)')
    self.lat_ll -> SetProperty, Value=String(ll[1,3], Format='(F0.3)')
    
END

PRO GPD_Viewer::Update_Grid, event

    @cat_pro_error_handler

     CASE event.name OF
     
         'DRAW_WIDGET': BEGIN
             IF event.type NE 1 THEN RETURN
             self.map_gpd_obj -> Draw             
             uv = Convert_Coord(event.x, event.y, /DEVICE, /TO_DATA)
             self.map_gpd_obj -> GetProperty, MAP_STRUCTURE=mapStruct
             ll = MAP_PROJ_INVERSE(uv[0], uv[1], MAP_STRUCTURE=mapStruct)
             self.map_gpd_obj -> SetProperty, CENTER_LONGITUDE=ll[0], CENTER_LATITUDE=ll[1]
             
             self.map_base_obj -> Draw             
             uv = Convert_Coord(event.x, event.y, /DEVICE, /TO_DATA)
             self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
             ll = MAP_PROJ_INVERSE(uv[0], uv[1], MAP_STRUCTURE=mapStruct)
             self.grid_ref_latID -> SetProperty, VALUE=ll[1]
             self.grid_ref_lonID -> SetProperty, VALUE=ll[0]
             END
             
          'GRID_CENTER_LATITUDE': BEGIN
             self.grid_ref_latID -> GetProperty, VALUE=lat
             self.grid_ref_lonID -> GetProperty, VALUE=lon
             self.map_gpd_obj -> SetProperty, CENTER_LONGITUDE=lon, CENTER_LATITUDE=lat
             END
        
          'GRID_CENTER_LONGITUDE': BEGIN
             self.grid_ref_latID -> GetProperty, VALUE=lat
             self.grid_ref_lonID -> GetProperty, VALUE=lon
             self.map_gpd_obj -> SetProperty, CENTER_LONGITUDE=lon, CENTER_LATITUDE=lat
             END

          ELSE:  
     ENDCASE
     
     self.map_base_obj -> SetProperty, LIMIT=self.limit
     self -> Update_GPD_Box
     self.drawID -> Draw
     
END 


;*****************************************************************************************************
; NAME:
;        GPD_VIEWER::EVENT_HANDLER
;
; PURPOSE:
;
;        This method is the event handler for the GPD_VIEWER object. It will typically
;        be used to respond to events from widget objects created in application.
;
; SYNTAX:
;
;        This method is called automatically by the event handling mechanism.
;
; ARGUMENTS:
;
;       event: The event structure as described in the IDL help files, except
;              that the ID, TOP and HANDLER tags will be object references.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
PRO GPD_Viewer::EventHandler, event

   @cat_pro_error_handler

   ; Branch on the object name.
   CASE StrUpCase(event.name) OF

      ; Draw widget events.
      'DRAW_WIDGET': self -> Update_Grid, event
      
      ; Grid manipulations
      'GRID_XSIZE': self -> Update_Grid, event
      'GRID_YSIZE':  self -> Update_Grid, event
      'GRID_CENTER_LATITUDE': self -> Update_Grid, event
      'GRID_CENTER_LONGITUDE': self -> Update_Grid, event
      'NUMBER_MAP_UNITS_IN_GRID': self -> Update_Grid, event
      
      ; Map manipulations.
      'MAP_PROJECTION': self -> SetMapProjection, event
      'MAP_CENTER_LATITUDE': self -> Update_Map, event
      'MAP_CENTER_LONGITUDE': self -> Update_Map, event
      'MAP_TRUESCALE_LATITUDE': self -> Update_Map, event
      'MAP_SCALE': self -> Update_Map, event
      'MAP_UNITS': self -> Update_Map, event
      'ZOOM_IN': self -> ZoomMap, event
      'ZOOM_OUT': self -> ZoomMap, event
      
      
      ; Miscellaneous operations
      'DEFAULT_GPD_BOX': self -> SetMapDefaults, event
      'OPEN_GPD_FILE': BEGIN
            filename = Dialog_Pickfile(TITLE='Select GPD File...', FILTER='*.gpd')
            IF filename EQ "" THEN RETURN
            self -> Open_GPD_File, filename
            END 
      'REFRESH_GPD_BOX': BEGIN
            self -> UpDate_GPD_Box
            self -> Draw
            END
      'WRITE_GPD_FILE': self -> Write_GPD_File, event
      'MAP_DATUM': self -> Update_Map_Datum, event
      'EXIT': OBJ_DESTROY, self

       ELSE : 
       
   ENDCASE

   IF Obj_Valid(self) THEN self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::GETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to obtain GPD_VIEWER properties. Be sure
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
;     _REF_EXTRA:     Any keywords appropriate for the superclass GetProperty method.
;-
;*****************************************************************************************************
PRO GPD_Viewer::GetProperty,  _Ref_Extra=extrakeywords

   @cat_pro_error_handler

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::GetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::GUI
;
; PURPOSE:
;
;       This method is where the graphical user interface elements of the program
;       are created.
;
; SYNTAX:
;
;       theObject -> GUI
;
; ARGUMENTS:
;
;     menuBarID:      The identifier of the menuBar widget, if any.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::GUI, menubarID

     @cat_pro_error_handler
     
     fileID = Obj_New('ButtonWidget', menubarID, VALUE='File')
     button = Obj_New('ButtonWidget', fileID, Value='Open GPD File', NAME='OPEN_GPD_FILE')
     button = Obj_New('ButtonWidget', fileID, Value='Write GPD File', NAME='WRITE_GPD_FILE')     
     button = Obj_New('ButtonWidget', fileID, Value='Exit', NAME='EXIT', /Separator)
 
     
     mainbase = Obj_New('BaseWidget', self, XPAD=0, YPAD=0, ROW=1, FRAME=1)
     controlBase = Obj_New('BaseWidget', mainbase, XPAD=3, YPAD=3, COLUMN=1)
     rightBase = Obj_New('BaseWidget', mainbase, COLUMN=1, XPAD=0, YPAD=0)
     drawBase = Obj_New('BaseWidget', rightBase, COLUMN=1)
     
     buttonBase = Obj_New('BaseWidget', self, ROW=1)
     mapBase = Obj_New('BaseWidget', controlBase, XPAD=0, YPAD=0, COLUMN=1, /Frame)
     gridBase = Obj_New('BaseWidget', controlBase, XPAD=0, YPAD=0, COLUMN=1, /Frame)
     
     map_projections =     [ 'Azimuthal Equal-Area (North Pole)', $
                             'Azimuthal Equal-Area (South Pole)', $
                             'Polar Stereographic (North Pole)', $
                             'Polar Stereographic (South Pole)', $
                             'Orthographic', $
                             'Cylindrical Equirectangular (Global)', $
                             'Mercator (Global)']
                         
     map_datums = [ 'Sphere', $
                    'EASE-Grid Sphere', $
                    'WGS 84', $
                    'Clarke 1866', $
                    'Hough']
                    
     map_units = [ 'Meters' ]
                    
     labelsize = 200
     
     
     self.projectionID = Obj_New('DroplistWidget', mapBase, VALUE=map_projections, $
                             TITLE='Map Projection: ', LABEL_SIZE=labelsize, SPACES=[0,5], $
                             NAME='MAP_PROJECTION')
     self.datumID = Obj_New('DroplistWidget', mapBase, VALUE=map_datums, $
                             TITLE='Map Datum: ', LABEL_SIZE=labelsize, SPACES=[0,5], $
                             NAME='MAP_DATUM')
     self.map_ref_latID = Obj_New('FieldWidget', mapBase, VALUE = 90.0, DECIMAL=4, $
                             TITLE='Map Origin Latitude: ', LABELSIZE=labelsize, $
                             NAME='MAP_CENTER_LATITUDE', /CR_EVENTS, SENSITIVE=0)
     self.map_ref_lonID = Obj_New('FieldWidget', mapBase, VALUE = 0.0, DECIMAL=4, $
                             TITLE='Map Origin Longitude: ', LABELSIZE=labelsize, $
                             NAME='MAP_CENTER_LONGITUDE', /CR_EVENTS)
     self.map_truescaleID = Obj_New('FieldWidget', mapBase, VALUE = 70.0, DECIMAL=4, $
                             TITLE='Latitude of True Scale: ', LABELSIZE=labelsize, $
                             NAME='MAP_TRUESCALE_LATITUDE', /CR_EVENTS, /LABEL_LEFT)
     self.map_truescaleID -> SetProperty, SENSITIVE=0
     self.map_scaleID = Obj_New('FieldWidget', mapBase, VALUE = 1.0, DECIMAL=4, $
                             TITLE='Map Scale: ', LABELSIZE=labelsize, $
                             NAME='MAP_SCALE', /CR_EVENTS, /LABEL_LEFT)        
     self.map_unitsID = Obj_New('DroplistWidget', mapBase, VALUE=map_units, $
                             TITLE='Map Units: ', LABEL_SIZE=labelsize, SPACES=[0,5], $
                             NAME='MAP_UNITS')        
     self.mapUnitsPerGridID = Obj_New('FieldWidget', gridBase, VALUE = 5000.0, DECIMAL=4, $
                             TITLE='Map Units per Grid Cell: ', LABELSIZE=labelsize, $
                             NAME='NUMBER_MAP_UNITS_IN_GRID', /CR_EVENTS, /LABEL_LEFT)        
     self.xgridSizeID = Obj_New('FieldWidget', gridBase, VALUE = 1000, DIGITS=5, $
                             TITLE='Number X Grid Cells: ', LABELSIZE=labelsize, $
                             NAME='GRID_XSIZE', /CR_EVENTS, /LABEL_LEFT)
     self.ygridSizeID = Obj_New('FieldWidget', gridBase, VALUE = 1000, DIGITS=5, $
                             TITLE='Number Y Grid Cells: ', LABELSIZE=labelsize, $
                             NAME='GRID_YSIZE', /CR_EVENTS, /LABEL_LEFT)
     self.grid_ref_latID = Obj_New('FieldWidget', gridBase, VALUE = 90.0, DECIMAL=4, $
                             TITLE='Grid Center Latitude: ', LABELSIZE=labelsize, $
                             NAME='GRID_CENTER_LATITUDE', /CR_EVENTS)
     self.grid_ref_lonID = Obj_New('FieldWidget', gridBase, VALUE = 0.0, DECIMAL=4, $
                             TITLE='Grid Center Longitude: ', LABELSIZE=labelsize, $
                             NAME='GRID_CENTER_LONGITUDE', /CR_EVENTS)
                             
     latlonBase = Obj_New('BaseWidget', rightBase, /GRID_LAYOUT, ROW=3)
     child = Obj_New('LabelWidget', latlonBase, Value='Location', /ALIGN_LEFT)
     child = Obj_New('LabelWidget', latlonBase, Value='UL', /ALIGN_CENTER)
     child = Obj_New('LabelWidget', latlonBase, Value='UR', /ALIGN_CENTER)
     child = Obj_New('LabelWidget', latlonBase, Value='LR', /ALIGN_CENTER)
     child = Obj_New('LabelWidget', latlonBase, Value='LL', /ALIGN_CENTER)
     child = Obj_New('LabelWidget', latlonBase, Value='Longitude', /ALIGN_LEFT)
     self.lon_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     child = Obj_New('LabelWidget', latlonBase, Value='Latitude', /ALIGN_LEFT)
     self.lat_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     
     ; Map zoom buttons.
     zoomBase = Obj_New('BaseWidget', rightBase, SPACE=10, ROW=1)
     self.zoomIn = Obj_New('ButtonWidget', zoomBase, Value='Zoom Map In', NAME='ZOOM_IN')
     self.zoomOut = Obj_New('ButtonWidget', zoomBase, Value='Zoom Map Out', NAME='ZOOM_OUT')
     
     
     
     commentbase = Obj_New('BaseWidget', controlBase, ROW=1)
     label = Obj_New('LabelWidget', commentBase, VALUE='File Comments:', SCR_XSIZE=labelsize)
     self.commentID = Obj_New('TextWidget', commentBase, YSIZE=5, /EDITABLE)
     controlBase -> GetProperty, GEOMETRY=geo
     self.commentID -> SetProperty, SCR_XSIZE=geo.scr_xsize - labelsize - 14
     button = Obj_New('ButtonWidget', buttonBase, Value='Open GPD File', NAME='OPEN_GPD_FILE')
     button = Obj_New('ButtonWidget', buttonBase, Value='Refresh GPD Box', NAME='REFRESH_GPD_BOX')
     button = Obj_New('ButtonWidget', buttonBase, Value='Set Default GPD Box', NAME='DEFAULT_GPD_BOX')
     button = Obj_New('ButtonWidget', buttonBase, Value='Write GPD File', NAME='WRITE_GPD_FILE')
     self.limit = [0, -180, 90, 180]
     
     ; Set up the initial map projection space.
     self.map_base_obj = Obj_New('MapCoord', 111, /GCTP,  CENTER_LATITUDE=90, $
                               CENTER_LONGITUDE=0, LIMIT=self.limit, NAME='BASEMAP')
     self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
     self.map_base_obj -> SetProperty, $
        XRANGE=mapStruct.uv_box[[0,2]], $
        YRANGE=mapStruct.uv_box[[1,3]]                   
     
     self.map_outline = Obj_New('Map_Outline', MAP_OBJECT=self.map_base_obj, COLOR='burlywood', /FILL)
     self.map_grid = Obj_New('Map_Grid', MAP_OBJECT=self.map_base_obj, COLOR='charcoal', $
        CHARSIZE=0.75, LONLAB=60)     
     
     ; Make a map coordinate system for the grid.
     self.map_gpd_obj = Obj_New('MapCoord', 111, /GCTP,  $
        CENTER_LATITUDE=90, $
        CENTER_LONGITUDE=0, LIMIT=self.limit, NAME="GRIDMAP")
     xorigin = (1000./2) * 5000.0 * (-1) * 1.0
     yorigin = (1000./2) * 5000.0 * (-1) * 1.0
     xend = xorigin + (1000.0 * 5000.0 * 1.0)
     yend = yorigin + (1000.0 * 5000.0 * 1.0)
     xrange = [xorigin, xend]
     yrange = [yorigin, yend]
     x = [xrange[0], xrange[0], xrange[1], xrange[1], xrange[0]]
     y = [yrange[0], yrange[1], yrange[1], yrange[0], yrange[0]]
     self.map_gpd_obj -> GetProperty, MAP_STRUCTURE=map
     ll = Map_Proj_Inverse(x, y, MAP_STRUCTURE=map)
     self.gpd_box = Obj_New('Map_Plots', COLOR='crimson', ll[0,*], ll[1,*], MAP_OBJECT=self.map_gpd_obj)                   
          
     IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
        self.drawID = Obj_New('DrawWidget', drawBase, XSIZE=350, YSIZE=350, COORD_OBJECT=self.map_base_obj, $
                       INITIAL_COLOR='Ivory', ERASE_WINDOW=1, BUTTON_EVENTS=1, NAME='DRAW_WIDGET')
     ENDIF ELSE BEGIN
      self.drawID = Obj_New('DrawWidget', drawBase, XSIZE=450, YSIZE=450, COORD_OBJECT=self.map_base_obj, $
                       INITIAL_COLOR='Ivory', ERASE_WINDOW=1, BUTTON_EVENTS=1, NAME='DRAW_WIDGET')
     ENDELSE                  
     self.drawID -> Add, self.map_outline, POSITION=0
     self.drawID -> Add, self.map_grid, POSITION=1
     self.drawID -> Add, self.gpd_box, POSITION=2
     
     ; Keep draw widgets from becoming active windows.
     Widget_Control, self->getID(), /Managed

     ; Display the entire application in the window.
     self -> Draw, /Center

     self.map_projections = Ptr_New(map_projections, /NO_COPY)
     self.map_datums = Ptr_New(map_datums, /NO_COPY)

     ; Call an event to set things up.
     pseudoEvent = {TOP:self, ID:self.projectionID, HANDLER:self}
     self -> SetMapProjection, pseudoEvent
END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::OPEN_GPD_FILE
;
; PURPOSE:
;
;       This method will open and display the contents of a GPD file written with the GPD_VIEWER.
;
; SYNTAX:
;
;     theObject -> Open_GPD_File, filename
;
; ARGUMENTS:
;
;     filename:      The GPD file to open.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::Open_GPD_File, filename

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(lun) NE 0 THEN Free_Lun, lun
    ENDIF
    
    ; Open the file and read the contents.
    OpenR, lun, filename, /GET_LUN
    rows = File_Lines(filename)
    text = StrArr(rows)
    ReadF, lun, text
    Free_Lun, lun
    
    ; Parse the GPD file
    upcaseText = StrUpCase(text)
    
    ; Find the comment lines in the file.
    j = 0
    comment = ""
    WHILE StrMid(text[j], 0, 1) EQ ';' DO BEGIN
        IF comment EQ "" THEN comment=StrMid(text[j], 2) ELSE comment = [comment, StrMid(text[j], 2)]
        j = j+ 1
    ENDWHILE
    IF comment[0] EQ "" THEN Undefine, comment
    
    loc = StrPos(upcaseText, 'GPD PROJECTION:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        map_projection = ''
        ReadS, thisLine, map_projection, FORMAT ='(30x, A45)'
        map_projection = StrTrim(map_projection,2)
    ENDIF
    loc = StrPos(upcaseText, 'MAP REFERENCE LATITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        lat0 = 0.0
        ReadS, thisLine, lat0, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upcaseText, 'MAP REFERENCE LONGITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        lon0 = 0.0
        ReadS, thisLine, lon0, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upcaseText, 'MAP SECOND REFERENCE LATITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        truescale = 0.0
        ReadS, thisLine, truescale, FORMAT ='(30x, F0)'
    ENDIF ELSE truescale = 0.0
    loc = StrPos(upcaseText, 'MAP SCALE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        mapScale = 0.0
        ReadS, thisLine, mapScale, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID MAP UNITS PER CELL:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        mapUnitsPerGrid = 0.0
        ReadS, thisLine, mapUnitsPerGrid, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID WIDTH:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        xgridSize = 0L
        ReadS, thisLine, xgridSize, FORMAT ='(30x, I10)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID HEIGHT:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        ygridSize = 0L
        ReadS, thisLine, ygridSize, FORMAT ='(30x, I10)'
    ENDIF         
    loc = StrPos(upcaseText, 'MAP DATUM:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        selected_datum = ''
        ReadS, thisLine, selected_datum, FORMAT ='(30x, A45)'
        selected_datum = StrTrim(selected_datum,2)
    ENDIF
    loc = StrPos(upcaseText, 'MAP ORIGIN X:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        xorigin = 0.0D
        ReadS, thisLine, xorigin, FORMAT ='(30x, F0.6)'
    ENDIF
    loc = StrPos(upcaseText, 'MAP ORIGIN Y:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        yorigin = 0.0D
        ReadS, thisLine, yorigin, FORMAT ='(30x, F0.6)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID CENTER LATITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lat = 0.0D
        ReadS, thisLine, grid_center_lat, FORMAT ='(30x, F0)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID CENTER LONGITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lon = 0.0D
        ReadS, thisLine, grid_center_lon, FORMAT ='(30x, F0)'
    ENDIF
   
    ; Set up map projection information.
    self.projectionID -> SetProperty, SELECTION=map_projection
    self -> SetMapProjection
    
    ; Set the information to the display.
    self.datumID -> SetProperty, SELECTION=selected_datum
    self.map_ref_latID -> SetProperty, Value=lat0
    self.map_ref_lonID -> SetProperty, Value=lon0
    self.mapUnitsPerGridID -> SetProperty, Value=mapUnitsPerGrid
    self.xgridSizeID -> SetProperty, Value=xgridSize
    self.ygridSizeID -> SetProperty, Value=ygridSize
    self.map_scaleID -> SetProperty, Value=mapScale
    self.map_trueScaleID -> SetProperty, Value=truescale
    self.commentID -> SetProperty, Value=comment
    
    ; Locate the center of the map grid.
    xcenter = xorigin + (xgridSize * mapUnitsPerGrid * mapScale) / 2.0
    ycenter = yorigin - (ygridSize * mapUnitsPerGrid * mapScale) / 2.0
    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
    ll = Map_Proj_Inverse(xcenter, ycenter, MAP_STRUCTURE=mapStruct)
    
    self.map_gpd_obj -> SetProperty, CENTER_LATITUDE=grid_center_lat, CENTER_LONGITUDE=grid_center_lon
    self.grid_ref_latID -> SetProperty, Value=grid_center_lat
    self.grid_ref_lonID -> SetProperty, Value=grid_center_lon
    
    self -> Update_Map
    
END

;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SETMAPDEFAULTS
;
; PURPOSE:
;
;       This method is an event handler that sets the initial map projection defaults.
;
; SYNTAX:
;
;       theObject -> SetMapDefaults, event
;
; ARGUMENTS:
;
;     event:      The event structure.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::SetMapDefaults, event

    
    self.projectionID -> GetProperty, SELECTION=thisProjection
    CASE thisProjection OF
    
        'Azimuthal Equal-Area (North Pole)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=90.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=70.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=5000.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=-90.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=-70.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=5000.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
            
        'Cylindrical Equirectangular (Global)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=0.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=0.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.mapUnitsPerGridID -> SetProperty, VALUE=7500.0
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END

        'Mercator (Global)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=0.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=0.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=7500.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
            
        'Orthographic': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=45.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=0.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=5000.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
    
         'Polar Stereographic (North Pole)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=90.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=70.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=5000.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
            
        'Polar Stereographic (South Pole)': BEGIN
            self.map_ref_latID -> SetProperty, VALUE=-90.0
            self.map_ref_lonID -> SetProperty, VALUE=0.0
            self.map_truescaleID -> SetProperty, VALUE=-70.0
            self.map_scaleID -> SetProperty, VALUE= 1.0
            self.mapUnitsPerGridID -> SetProperty, VALUE=5000.0
            self.map_UnitsID -> SetProperty, SELECTION='Meters'
            self.xgridSizeID -> SetProperty, Value=1000
            self.ygridSizeID -> SetProperty, Value=1000
            END
                        
        ENDCASE
        
        ; Call an event to set things up.
        pseudoEvent = {TOP:self, ID:self.projectionID, HANDLER:self}
        self -> SetMapProjection, pseudoEvent
END

;
;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SETMAPPROJECTION
;
; PURPOSE:
;
;       This method is an event handler that sets the initial map projection and
;       responds to the Map Projection droplist widget.
;
; SYNTAX:
;
;       theObject -> SetMapProjection, event
;
; ARGUMENTS:
;
;     event:      The event structure.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::SetMapProjection, event

    ; Get the current settings on the display.
    self.mapUnitsPerGridID -> GetProperty, Value=mapunits
    self.xgridSizeID -> GetProperty, Value=xgridSize
    self.ygridSizeID -> GetProperty, Value=ygridSize
    self.map_scaleID -> GetProperty, Value=mapScale
    self.map_trueScaleID -> GetProperty, Value=trueScale
    
    IF N_Elements(event) EQ 0 $
        THEN self.projectionID -> GetProperty, SELECTION=thisProjection $
        ELSE event.id -> GetProperty, SELECTION=thisProjection
        
    self.datumID -> GetProperty, SELECTION=selected_datum
    
    CASE selected_datum OF
        'Sphere' : BEGIN
            semimajor_axis = '6370997.00'
            semiminor_axis = '6370997.00'
            END
        'EASE-Grid Sphere' : BEGIN
            semimajor_axis = '6371228.00'
            semiminor_axis = '6371228.00'
            END
        'Clarke 1866' : BEGIN
            semimajor_axis = '6378206.40'
            semiminor_axis = '6356583.80'
            END
        'WGS 84' : BEGIN
            semimajor_axis = '6378137.00'
            semiminor_axis = '6356752.31'
            END
        'Hough' : BEGIN
            semimajor_axis = '6378270.00'
            semiminor_axis = '6356794.34'
            END
    ENDCASE
    
    CASE thisProjection OF
    
        'Azimuthal Equal-Area (North Pole)': BEGIN
            map_projection = 111
            self.map_ref_latID -> SetProperty, Value=90, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = 90 & lon0 = 0
            self.limit = [15, -180, 90, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj, THICK=2
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=1
            self.zoomout -> SetProperty, Sensitive=1
            self.drawID -> Draw  
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
            map_projection = 111
            self.map_ref_latID -> SetProperty, Value=-90, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = -90 & lon0 = 0
            self.limit = [-90, -180, -15, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=1
            self.zoomout -> SetProperty, Sensitive=1
            self.drawID -> Draw  
            END
            

            
        'Cylindrical Equirectangular (Global)': BEGIN
            map_projection = 117
            self.map_ref_latID -> SetProperty, Value=0, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = 0 & lon0 = 0
            self.limit = [-90, -180, 90, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                TRUE_SCALE_LATITUDE=40, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                TRUE_SCALE_LATITUDE=40, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj 
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0.0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.25, 1.0, 0.75]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.25, 1.0, 0.75]
            self.map_grid -> SetProperty, LonLab = -10, LATS=(Indgen(9)*20) - 80.0
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=0
            self.zoomout -> SetProperty, Sensitive=0
            self.drawID -> Draw  
            END

        'Mercator (Global)': BEGIN
            map_projection = 105
            self.map_ref_latID -> SetProperty, Value=0, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = 0 & lon0 = 0
            self.limit = [-80, -180, 80,  180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj 
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0.0     
            self.map_base_obj -> SetProperty, Position=[0, 0.25, 1.0, 0.75]
            self.map_gpd_obj -> SetProperty, Position=[0, 0.25, 1.0, 0.75]
            self.map_grid -> SetProperty, LonLab = 0, LATS=(Indgen(9)*20) -80
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=0
            self.zoomout -> SetProperty, Sensitive=0
            self.drawID -> Draw  
            END
            
        'Orthographic': BEGIN
            map_projection = 114
            self.map_ref_latID -> SetProperty, Value=45, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=-90, SENSITIVE=1
            lat0 = 45 & lon0 = -90
            self.limit = [-90, -180, 90, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj 
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(9)*20 - 80
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=0
            self.zoomout -> SetProperty, Sensitive=0
            self.drawID -> Draw  
            END
    
         'Polar Stereographic (North Pole)': BEGIN
            map_projection = 106
            self.map_ref_latID -> SetProperty, Value=90, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = 90 & lon0 = 0
            self.limit = [15, -180, 90, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=1, VALUE=70.0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=1
            self.zoomout -> SetProperty, Sensitive=1
            self.drawID -> Draw  
            END
            
        'Polar Stereographic (South Pole)': BEGIN
            map_projection = 106
            self.map_ref_latID -> SetProperty, Value=-90, SENSITIVE=1
            self.map_ref_lonID -> SetProperty, Value=0, SENSITIVE=1
            lat0 = -90 & lon0 = 0
            self.limit = [-90, -180, -45, 180]
            Obj_Destroy, self.map_base_obj
            Obj_Destroy, self.map_gpd_obj
            self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                NAME='BASEMAP', $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                LIMIT=self.limit, $
                /GCTP)
            self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
            self.map_base_obj -> SetProperty, $
                XRANGE=mapStruct.uv_box[[0,2]], $
                YRANGE=mapStruct.uv_box[[1,3]]                   
            self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                SEMIMAJOR_AXIS=semimajor_axis, $
                SEMIMINOR_AXIS=semiminor_axis, $
                CENTER_LATITUDE=lat0, $
                CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
            self.grid_ref_latID -> SetProperty, Value=lat0
            self.grid_ref_lonID -> SetProperty, Value=lon0
            self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
            self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
            self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
            self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
            self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
            self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
            self -> Update_GPD_Box
            self.zoomin -> SetProperty, Sensitive=1
            self.zoomout -> SetProperty, Sensitive=1
            self.drawID -> Draw  
            END
            
        ENDCASE
END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SETPROPERTY
;
; PURPOSE:
;
;       This method allows the user to set GPD_VIEWER properties. Be sure
;       you ALWAYS call the superclass SETPROPERTY method if you have extra
;       keywords!
;
; SYNTAX:
;
;       theObject -> SetProperty ...
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     _EXTRA:     Any keywords appropriate for the superclass SetProperty method.
;-
;*****************************************************************************************************
PRO GPD_Viewer::SetProperty,  _Extra=extrakeywords

   @cat_pro_error_handler

   IF N_Elements(extrakeywords) NE 0 THEN self -> TOPLEVELBASE::SetProperty, _Extra=extrakeywords

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::WRITE_GPD_FILE
;
; PURPOSE:
;
;       This method will write the GPD_FILE corresponding to the information in the GUI.
;       It is an event handler method.
;
; SYNTAX:
;
;     theObject -> Write_GPD_File, event
;
; ARGUMENTS:
;
;     event:      The event structure passed to it via XManager.
;
; KEYWORDS:
;
;     None.
;-
PRO GPD_Viewer::Write_GPD_File, event

    @cat_pro_error_handler
    
    ; What filename should we use?
    filename = Dialog_Pickfile(TITLE='Output GPD Filename...', /WRITE, FILE='gpd_viewer.gpd')
    IF filename EQ "" THEN RETURN
    
    ; Get the information from the display.
    self.projectionID -> GetProperty, SELECTION=selected_projection
    self.datumID -> GetProperty, SELECTION=selected_datum
    
    CASE selected_datum OF
        'Sphere' : BEGIN
            map_equatorial_radius = '6370997.00'
            map_polar_radius      = '6370997.00'
            END
        'EASE-Grid Sphere' : BEGIN
            map_equatorial_radius = '6371228.00'
            map_polar_radius      = '6371228.00'
            END
        'Clarke 1866' : BEGIN
            map_equatorial_radius = '6378206.40'
            map_polar_radius      = '6356583.80'
            END
        'WGS 84' : BEGIN
            map_equatorial_radius = '6378137.00'
            map_polar_radius      = '6356752.31'
            END
        'Hough' : BEGIN
            map_equatorial_radius = '6378270.00'
            map_polar_radius      = '6356794.34'
            END
    ENDCASE
    

    CASE selected_projection OF
        'Azimuthal Equal-Area (North Pole)': map_projection = 'Azimuthal Equal-Area'
            
        'Azimuthal Equal-Area (South Pole)': map_projection = 'Azimuthal Equal-Area'
            
        'Polar Stereographic (North Pole)': map_projection = 'Polar Stereographic'
            
        'Polar Stereographic (South Pole)': map_projection = 'Polar Stereographic'
            
        'Orthographic': map_projection = 'Orthographic'
        
        'Cylindrical Equirectangular (Global)': map_projection = 'Cylindrical Equidistant'
                    
        'Mercator (Global)': map_projection = 'Mercator'
        
     ENDCASE
    
    ; Get the data ranges from the gridbox and convert to UV coords.
    self.gpd_box -> GetProperty, LONS=lons, LATS=lats
    self.map_gpd_obj -> GetProperty, MAP_STRUCTURE=map
    xy = Map_Proj_Forward(lons, lats, Map_Structure=map)
    
    self.mapUnitsPerGridID -> GetProperty, Value=mapunits
    self.xgridSizeID -> GetProperty, Value=xgridSize
    self.ygridSizeID -> GetProperty, Value=ygridSize
    self.map_scaleID -> GetProperty, Value=mapScale
    self.map_trueScaleID -> GetProperty, Value=truescale
    self.commentID -> GetProperty, Value=theComment
    self.map_ref_latID -> GetProperty, Value=lat0
    self.map_ref_lonID -> GetProperty, Value=lon0
    self.grid_ref_latID -> GetProperty, Value=grid_center_lat
    self.grid_ref_lonID -> GetProperty, Value=grid_center_lon
    self.map_truescaleID -> GetProperty, Value=truescale
    
    map_reference_latitude = StrTrim(lat0, 2)
    map_reference_longitude = StrTrim(lon0, 2)
    map_second_reference_latitude = StrTrim(truescale, 2)
    map_units = StrTrim(mapunits, 2)
    map_scale = StrTrim(mapScale, 2)
    grid_width = StrTrim(xgridSize, 2)
    grid_height = StrTrim(ygridSize, 2)
    grid_center_lat = StrTrim(grid_center_lat, 2)
    grid_center_lon = StrTrim(grid_center_lon, 2)
        
    xorigin = (xy[0,*])[1]
    yorigin = (xy[1,*])[1]
    map_origin_x = StrTrim(String(xorigin, Format='(F12.2)'), 2)
    map_origin_y = StrTrim(String(yorigin, Format='(F12.2)'), 2)
    
     
     OpenW, lun, filename, /GET_LUN
     IF theComment[0] EQ "" THEN BEGIN
        PrintF, lun, '; GPD File Created with GPD_Viewer' 
     ENDIF ELSE BEGIN
        FOR j=0,N_Elements(theComment)-1 DO BEGIN
           thisComment = theComment[j]
           IF StrMid(thisComment,0,1) NE ';' THEN thisComment = '; ' + thisComment
           PrintF, lun, thisComment
        ENDFOR
     ENDELSE
     PrintF, lun, 'Map Projection:', map_projection, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Reference Latitude:', map_reference_latitude, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Reference Longitude:', map_reference_longitude, FORMAT='(A-30,x,A-45)'
     IF (StrPos(map_projection, 'Polar Stereographic') NE 0) OR $
        (StrPos(map_projection, 'Cylindrical Equal-Area') NE 0) THEN $
        PrintF, lun, 'Map Second Reference Latitude:', map_second_reference_latitude, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Scale:', map_scale, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Equatorial Radius:', map_equatorial_radius, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Polar Radius:', map_polar_radius, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Origin X:', map_origin_x, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Origin Y:', map_origin_y, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Units per Cell:', map_units, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Origin Column:', '-0.5', FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Origin Row:', '-0.5', FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Width:', grid_width, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Height:', grid_height, FORMAT='(A-30,x,A-45)'
     PrintF, lun, ''
     PrintF, lun, '; The following lines are added ONLY to allow easy access to GPD'
     PrintF, lun, '; information by the GPD_Viewer when reading GPD files. Do not remove'
     PrintF, lun, '; them if you wish to read this GPD file in the GPD_Viewer.'
     PrintF, lun, ''
     PrintF, lun, '; GPD Projection:', selected_projection, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Map Datum:', selected_datum, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Grid Center Latitude:', grid_center_lat, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Grid Center Longitude:', grid_center_lon, FORMAT='(A-30,x,A-45)'
     Free_Lun, lun
END

;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::KILL_NOTIFY
;
; PURPOSE:
;
;       This method is called automatically when the application is destroyed.
;       It's primary purpose is to clean up any objects that have been created
;       to run the Catalyst system. It might also destroy any Catalyst infrastructure
;       by calling CatDestroyDefaults.
;
; SYNTAX:
;
;     None.
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
PRO GPD_Viewer::Kill_Notify

END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER:ZOOMMAP
;
; PURPOSE:
;
;       This method allows the user to zoom in and out of the base map.
;
; SYNTAX:
;
;       Called from event handler when Zoom In or Zoom Out buttons are pressed
;
; ARGUMENTS:
;
;       event:         The event structure
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::ZoomMap, event

    ; Basically, change the map limits by 5 degrees, recognizing some reasonable min or max.

    self.projectionID -> GetProperty, SELECTION=thisProjection
    
    self.datumID -> GetProperty, SELECTION=selected_datum
    CASE selected_datum OF
        'Sphere' : BEGIN
            semimajor_axis = '6370997.00'
            semiminor_axis = '6370997.00'
            END
        'EASE-Grid Sphere' : BEGIN
            semimajor_axis = '6371228.00'
            semiminor_axis = '6371228.00'
            END
        'Clarke 1866' : BEGIN
            semimajor_axis = '6378206.40'
            semiminor_axis = '6356583.80'
            END
        'WGS 84' : BEGIN
            semimajor_axis = '6378137.00'
            semiminor_axis = '6356752.31'
            END
        'Hough' : BEGIN
            semimajor_axis = '6378270.00'
            semiminor_axis = '6356794.34'
            END
    ENDCASE
    Print, 'In GPD_Viewer ZOOMMAP, LIMIT=', self.limit
    CASE event.name OF
    
        'ZOOM_IN': BEGIN
        
           CASE thisProjection OF
            
                'Azimuthal Equal-Area (North Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[0]+5) < (70)
                    self.limit = [newlat, -180, 90, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                     self.grid_ref_latID -> GetProperty, Value=lat0
                    self.grid_ref_lonID -> GetProperty, Value=lon0
                     self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
                'Azimuthal Equal-Area (South Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[2]-5) > (-60)
                    self.limit = [-90, -180, newlat, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                    self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.grid_ref_latID -> SetProperty, Value=lat0
                    self.grid_ref_lonID -> SetProperty, Value=lon0
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
               'Cylindrical Equirectangular (Global)': BEGIN
                    END
        
                'Mercator (Global)': BEGIN
                    END
                    
                'Orthographic': BEGIN
                    END
            
                 'Polar Stereographic (North Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[0]+5) < (70)
                    self.limit = [newlat, -180, 90, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                     self.grid_ref_latID -> GetProperty, Value=lat0
                    self.grid_ref_lonID -> GetProperty, Value=lon0
                     self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
                'Polar Stereographic (South Pole)': BEGIN
                     self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[2]-5) > (-60)
                    self.limit = [-90, -180, newlat, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                    self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.grid_ref_latID -> SetProperty, Value=lat0
                    self.grid_ref_lonID -> SetProperty, Value=lon0
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                   END
                    
            ENDCASE
            
            END
            
         'ZOOM_OUT': BEGIN
         
           CASE thisProjection OF
            
                'Azimuthal Equal-Area (North Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[0]-5) > (-15)
                    self.limit = [newlat, -180, 90, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                     self.grid_ref_latID -> GetProperty, Value=lat0
                    self.grid_ref_lonID -> GetProperty, Value=lon0
                     self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
                'Azimuthal Equal-Area (South Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[2]+5) < (15)
                    self.limit = [-90, -180, newlat, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                    self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.grid_ref_latID -> SetProperty, Value=lat0
                    self.grid_ref_lonID -> SetProperty, Value=lon0
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
               'Cylindrical Equirectangular (Global)': BEGIN
                    minLon = self.limit[1]
                    minLat = self.limit[0]
                    maxLon = self.limit[3]
                    maxLat = self.limit[2]
                    tminLon = minLon - 5
                    tminLat = minLat - 5
                    tmaxLon = maxLon - 5
                    tmaxLat = maxLat -5
                    IF (tmaxlon - tminlon) GT 20 THEN BEGIN
                        self.limit[1] = tminLon
                        self.limit[3] = tmaxLon
                    ENDIF
                    IF (tmaxlat - tminlat) GT 20 THEN BEGIN
                        self.limit[0] = tminLat
                        self.limit[2] = tmaxLat
                    ENDIF
                    END
        
                'Mercator (Global)': BEGIN
                    END
                    
                'Orthographic': BEGIN
                    END
            
                 'Polar Stereographic (North Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[0]-5) > (-15)
                    self.limit = [newlat, -180, 90, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                     self.grid_ref_latID -> GetProperty, Value=lat0
                    self.grid_ref_lonID -> GetProperty, Value=lon0
                     self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP,  $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = 55, LATS=Indgen(5)*20
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
                'Polar Stereographic (South Pole)': BEGIN
                    self.map_ref_latID -> GetProperty, Value=lat0
                    self.map_ref_lonID -> GetProperty, Value=lon0
                    newlat = (self.limit[2]+5) < (15)
                    self.limit = [-90, -180, newlat, 180]
                    Obj_Destroy, self.map_base_obj
                    Obj_Destroy, self.map_gpd_obj
                    self.map_base_obj = Obj_New('MAPCOORD', map_projection, $
                        NAME='BASEMAP', $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        LIMIT=self.limit, $
                        /GCTP)
                    self.map_base_obj -> GetProperty, MAP_STRUCTURE=mapStruct
                    self.map_base_obj -> SetProperty, $
                        XRANGE=mapStruct.uv_box[[0,2]], $
                        YRANGE=mapStruct.uv_box[[1,3]]                   
                    self.map_gpd_obj = Obj_New('MapCoord', map_projection, /GCTP, $
                        SEMIMAJOR_AXIS=semimajor_axis, $
                        SEMIMINOR_AXIS=semiminor_axis, $
                        CENTER_LATITUDE=lat0, $
                        CENTER_LONGITUDE=lon0, LIMIT=self.limit, NAME="GRIDMAP")
                    self.grid_ref_latID -> SetProperty, Value=lat0
                    self.grid_ref_lonID -> SetProperty, Value=lon0
                    self.drawID -> SetProperty, COORD_OBJECT=self.map_base_obj
                    self.map_outline -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.map_grid -> SetProperty, MAP_OBJECT=self.map_base_obj
                    self.gpd_box -> SetProperty, MAP_OBJECT=self.map_gpd_obj
                    self.map_truescaleID -> SetProperty, SENSITIVE=0, VALUE=0     
                    self.map_base_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_gpd_obj -> SetProperty, Position=[0.0, 0.0, 1.0, 1.0]
                    self.map_grid -> SetProperty, LonLab = -55, LATS=(Indgen(5)*20) * (-1)
                    self -> Update_GPD_Box
                    self.drawID -> Draw  
                    END
                    
            ENDCASE
            END
    
    ENDCASE
    
    
    ; Update the display
    self.drawID -> Draw
    
END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::CLEANUP
;
; PURPOSE:
;
;       This is the GPD_VIEWER object class destructor method.
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
PRO GPD_Viewer::CLEANUP

   @cat_pro_error_handler
   
   Ptr_Free, self.map_projections
   Ptr_Free, self.map_datums
   Obj_Destroy, self.map_base_obj
   Obj_Destroy, self.map_gpd_obj
 
   self -> TOPLEVELBASE::CLEANUP ; Don't forget this line or memory leakage WILL occur!!!

   self -> Report, /Completed

END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::INIT
;
; PURPOSE:
;
;       This is the GPD_VIEWER object class initialization method
;
; SYNTAX:
;
;       Called automatically when the object is created.
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     __REF_EXTRA:     Any keywords appropriate for the superclass INIT method.
;                      In this case you MUST use _REF_EXTRA instead of _EXTRA because
;                      you need to return the MENUBAR object reference.
;-
;*****************************************************************************************************
FUNCTION GPD_Viewer::INIT, _Ref_Extra=extra

   @cat_func_error_handler

   ; The application is contained in a top-level base widget.
   ok = self->TOPLEVELBASE::INIT(_Extra=extra, XPAD=0, YPAD=0, SPACE=10, $
        BASE_ALIGN_CENTER=1, TITLE='GPD Viewer and Writer')

   IF (ok) THEN self -> Report, /Completed $
   ELSE self -> Report, /Failed
   RETURN, ok

END


;*****************************************************************************************************
;
; NAME:
;       GPD_VIEWER Class Definition
;
; PURPOSE:
;
;       This is the structure definition code for the GPD_VIEWER object. 
;
;*****************************************************************************************************
PRO GPD_Viewer__Define, class

   class = { GPD_Viewer, $
             projectionID: Obj_New(), $
             datumID: Obj_New(), $
             map_ref_latID: Obj_New(), $
             map_ref_lonID: Obj_New(), $
             map_scaleID: Obj_New(), $
             map_UnitsID: Obj_New(), $
             mapUnitsPerGridID: Obj_New(), $
             xgridSizeID: Obj_New(), $
             ygridSizeID: Obj_New(), $
             lon_ur: Obj_New(), $
             lon_ul: Obj_New(), $
             lon_lr: Obj_New(), $
             lon_ll: Obj_New(), $
             lat_ur: Obj_New(), $
             lat_ul: Obj_New(), $
             lat_lr: Obj_New(), $
             lat_ll: Obj_New(), $
             grid_ref_latID: Obj_New(), $
             grid_ref_lonID: Obj_New(), $
             commentID: Obj_New(), $
             map_outline: Obj_New(), $
             map_grid: Obj_New(), $
             map_truescaleID: Obj_New(), $
             gpd_box: Obj_New(), $
             drawID: Obj_New(), $
             map_projections: Ptr_New(), $
             map_datums: Ptr_New(), $
             map_base_obj: Obj_New(), $
             map_gpd_obj: Obj_New(), $
             limit: FltArr(4), $
             zoomin: Obj_New(), $
             zoomout: Obj_New(), $
             INHERITS TopLevelBase }

END


;*****************************************************************************************************
;
; NAME:
;       GPD_VIEWER Driver Program
;
; PURPOSE:
;
;       A procedural interface to the GPD_Viewer object.
;
;*****************************************************************************************************
PRO GPD_Viewer, tlb

   @cat_pro_error_handler

   ; Create the widgets that make up the application. Widgets that generate
   ; events should be named explicitly and should be children (or grandchildren)
   ; of the TLB.
   tlb = OBJ_NEW('GPD_Viewer', COLUMN=1, /Kill_Notify, NAME='TLB', SIZE_EVENTS=1, MBAR=mbar)
   tlb -> GUI, mbar


END
;*****************************************************************************************************
