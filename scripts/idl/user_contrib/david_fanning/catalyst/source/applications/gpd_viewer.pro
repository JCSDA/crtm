;*****************************************************************************************************
;
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
; NOTES: 
; 
;       NSIDC EASE-Grid global gpd files use a Behrmann Cylindrical Equal-Area map projection
;       with a 30 degree latitude of true scale. IDL does not have such a map projection, so
;       this particular map projection is faked by using an Albers Equal-Area conic projection 
;       with standard parallels of approximately 60 degrees. On a global scale, the values for
;       the grid are quite close, but not exact. Additional information can be found here:
;       http://www.dfanning.com/map_tips/fakeproj.html.
;       
;       Latitude and longitude arrays are written in files as double precision values.
;       XGrid and YGrid vectors are written into files as double precision values. The
;       xgrid has column number of elements, while the ygrid vector has row number of 
;       elements.
;
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning, 14 January 2009.
;       Added more error checking and more ability to set properties of the map projection. 31 Oct 2009. DWF.
;       Completely gutted and re-written from scratch. Same functionality, but easier to maintain
;          and giving same results as NSIDC software. 16 December 2009. DWF.
;       Added ability to output latitude and longitude arrays for the grid. 17 December 2009. DWF.
;       Added ability to read some Polar Stereo GPD files from NSIDC archives. 20 December 2009. DWF.
;       Now allowing true scale latitude values to be changed. (Required updated MapCoord object.) 
;          22 December 2009. DWF.
;       Added Equal-Area Cylindrical map projection. And now keeping aspect ratio of
;          global maps correctly. 23 December 2009. DWF.
;       Added ability to read and visualize 10 common NSIDC gpd files. 24 December 2009. DWF.
;       Added ability to output MapCoord object to the main IDL level. 24 December 2009. DWF.
;       If you changed the map origin, then clicked on the map, the map origin would revert to
;          its default values. Fixed. 27 January 2010. DWF.
;       There was a sign problem when selecting the grid center with the GUI. This caused the
;          Y location to be in the wrong location. Fixed. 27 January 2010. DWF.
;       Added a SKIP_LATLON_UPDATE keyword to both the SetMapProjection and MapLabel_to_MapProj 
;          methods. This allows me to control when the default map center latitude and longitude
;          should be changed to their default values and when they should be left alone. 
;          27 January 2010. DWF
;       Added a FIXED_MAP_GRID keyword to the MAP_GRID object call. 20 February 2010. DWF.
;       Fixed a problem that caused the grid location to change when changing the grid scale. 
;          28 Feb 2010. DWF.
;       Fixed a problem with selecting a grid center when map is zoomed. Previously the map
;          would zoom back out. Now it stays in zoomed mode. 28 Feb 2010. DWF.
;       Fixed a problem with alignment of NSIDC gpd file formats. 10 March 2010. DWF.
;       Had semi-major and semi-minor axes reversed in output GPD files. 10 March 2010. DWF.
;       Small problem with locating the center of the map projection in latitude and longitude,
;          when fixing the center by column and row. 30 March 2010. DWF.
;       Small typo was interfering with the program using the semimajor and semiminor axes
;          values in the map projection. This resulted in latitude and longitude vectors being
;          slightly misplaced. 31 March 2010. DWF.
;       Added ability to write and export XY grid vectors. These are the XGrid and YGrid coordinate vectors
;          that describe the centers of the grid pixels in meters. 31 March 2010. DWF.
;       Modified program to position the grid with upper-left X and Y values. 6 April 2010. DWF.
;       Fixed a problem when going from a North Pole projection to a South Pole projection. 28 July 2010. DWF.
;-
;*****************************************************************************************************
; NAME:
;        GPD_VIEWER::CheckKnownFileList
;
; PURPOSE:
;
;        This method is used to open known NSIDC gpd files and process them correctly
;        If the "known" file is not on the list, it is not processed and the filename 
;        is returned to the caller.
;
; SYNTAX:
;
;        wasProcessed = self -> CheckKnownFileList(filename)
;        
; RETURN_VALUE:
; 
;       wasProcessed:   A 1 if the file was known and processed correctly. A 0 otherwise.
;
; ARGUMENTS:
;
;       filename:   The name of a GPD file to process.
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION GPD_Viewer::CheckKnownFileList, filename

    ; Error handling
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        IF N_Elements(lun) NE 0 THEN Free_Lun, lun
        RETURN, 0
    ENDIF

    Compile_Opt idl2
    
    ; We are interested in the root filename.
    rootname = StrUpCase(File_Basename(filename))
    
    ; If you know it, process it. If you don't leave.
    CASE rootname OF
    
        'N3A.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=-45.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6378273.0D
            self.semiminor_axisID -> SetProperty, Value=6356889.5D
            self.metersPerGridCellID -> SetProperty, Value=12500.0D
            self.gridColsID -> SetProperty, Value=608
            self.gridRowsID -> SetProperty, Value=896
            self.grid_ref_xID -> SetProperty, Value=307.5
            self.grid_ref_yID -> SetProperty, Value=467.5
            self.grid_ref_latID -> SetProperty, Value=87.64684278D, UValue=87.64684278D
            self.grid_ref_lonID -> SetProperty, Value=146.30993247D, UValue=146.30993247D
            self.projectionID -> SetProperty, Selection='Polar Stereographic (North Pole)'
            self.datumID -> SetProperty, Selection='Hughes'
            mapInfo = self -> MapLabel_to_MapProj('Polar Stereographic (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'N3B.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=-45.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6378273.0D
            self.semiminor_axisID -> SetProperty, Value=6356889.5D
            self.metersPerGridCellID -> SetProperty, Value=25000.0D
            self.gridColsID -> SetProperty, Value=304
            self.gridRowsID -> SetProperty, Value=448
            self.grid_ref_xID -> SetProperty, Value=153.5
            self.grid_ref_yID -> SetProperty, Value=233.5
            self.grid_ref_latID -> SetProperty, Value=87.64684278D, UValue=87.64684278D
            self.grid_ref_lonID -> SetProperty, Value=146.30993247D, UValue=146.30993247D
            self.projectionID -> SetProperty, Selection='Polar Stereographic (North Pole)'
            self.datumID -> SetProperty, Selection='Hughes'
            mapInfo = self -> MapLabel_to_MapProj('Polar Stereographic (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        'NA25.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/8.0
            self.gridColsID -> SetProperty, Value=361
            self.gridRowsID -> SetProperty, Value=361
            self.grid_ref_xID -> SetProperty, Value=180.0
            self.grid_ref_yID -> SetProperty, Value=180.0
            self.grid_ref_latID -> SetProperty, Value=90.0D, UValue=90.0D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (North Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'NL.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/8.0
            self.gridColsID -> SetProperty, Value=721
            self.gridRowsID -> SetProperty, Value=721
            self.grid_ref_xID -> SetProperty, Value=360.0
            self.grid_ref_yID -> SetProperty, Value=360.0
            self.grid_ref_latID -> SetProperty, Value=90.0D, UValue=90.0D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (North Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'NH.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/16.0
            self.gridColsID -> SetProperty, Value=1441
            self.gridRowsID -> SetProperty, Value=1441
            self.grid_ref_xID -> SetProperty, Value=720.0
            self.grid_ref_yID -> SetProperty, Value=720.0
            self.grid_ref_latID -> SetProperty, Value=90.0D, UValue=90.0D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (North Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        'NA5.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/40.0
            self.gridColsID -> SetProperty, Value=1805
            self.gridRowsID -> SetProperty, Value=1805
            self.grid_ref_xID -> SetProperty, Value=902.0
            self.grid_ref_yID -> SetProperty, Value=902.0
            self.grid_ref_latID -> SetProperty, Value=90.0D, UValue=90.0D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (North Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        'NA1.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/160.0
            self.gridColsID -> SetProperty, Value=7220
            self.gridRowsID -> SetProperty, Value=7220
            self.grid_ref_xID -> SetProperty, Value=3610.0
            self.grid_ref_yID -> SetProperty, Value=3610.0
            self.grid_ref_latID -> SetProperty, Value=90.0D, UValue=90.0D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (North Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (North Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'S3A.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=-45.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6378273.0D
            self.semiminor_axisID -> SetProperty, Value=6356889.5D
            self.metersPerGridCellID -> SetProperty, Value=12500.0D
            self.gridColsID -> SetProperty, Value=632
            self.gridRowsID -> SetProperty, Value=664
            self.grid_ref_xID -> SetProperty, Value=315.5
            self.grid_ref_yID -> SetProperty, Value=347.5
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Polar Stereographic (South Pole)'
            self.datumID -> SetProperty, Selection='Hughes'
            mapInfo = self -> MapLabel_to_MapProj('Polar Stereographic (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        'S3B.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=-45.0
            self.map_truescaleID -> SetProperty, Value=70.0
            self.semimajor_axisID -> SetProperty, Value=6378273.0D
            self.semiminor_axisID -> SetProperty, Value=6356889.5D
            self.metersPerGridCellID -> SetProperty, Value=25000.d
            self.gridColsID -> SetProperty, Value=316
            self.gridRowsID -> SetProperty, Value=332
            self.grid_ref_xID -> SetProperty, Value=157.5
            self.grid_ref_yID -> SetProperty, Value=173.5
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Polar Stereographic (South Pole)'
            self.datumID -> SetProperty, Selection='Hughes'
            mapInfo = self -> MapLabel_to_MapProj('Polar Stereographic (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        'SA25.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=-70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/8.0
            self.gridColsID -> SetProperty, Value=361
            self.gridRowsID -> SetProperty, Value=361
            self.grid_ref_xID -> SetProperty, Value=180.0
            self.grid_ref_yID -> SetProperty, Value=180.0
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (South Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'SL.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=-70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/8.0
            self.gridColsID -> SetProperty, Value=721
            self.gridRowsID -> SetProperty, Value=721
            self.grid_ref_xID -> SetProperty, Value=360.0
            self.grid_ref_yID -> SetProperty, Value=360.0
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (South Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'SH.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=-70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/16.0
            self.gridColsID -> SetProperty, Value=1441
            self.gridRowsID -> SetProperty, Value=1441
            self.grid_ref_xID -> SetProperty, Value=720.0
            self.grid_ref_yID -> SetProperty, Value=720.0
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (South Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'SA5.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=-70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/40.0
            self.gridColsID -> SetProperty, Value=1605
            self.gridRowsID -> SetProperty, Value=1605
            self.grid_ref_xID -> SetProperty, Value=802.0
            self.grid_ref_yID -> SetProperty, Value=802.0
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (South Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END
            
        'SA1.GPD': BEGIN
            self.map_ref_latID -> SetProperty, Value=-90.0
            self.map_ref_lonID -> SetProperty, Value=0.0
            self.map_truescaleID -> SetProperty, Value=-70.0
            self.semimajor_axisID -> SetProperty, Value=6371228.0D
            self.semiminor_axisID -> SetProperty, Value=6371228.0D
            self.metersPerGridCellID -> SetProperty, Value=200540.2D/160.0
            self.gridColsID -> SetProperty, Value=6420
            self.gridRowsID -> SetProperty, Value=6420
            self.grid_ref_xID -> SetProperty, Value=3210
            self.grid_ref_yID -> SetProperty, Value=3210
            self.grid_ref_latID -> SetProperty, Value=-90D, UValue=-90D
            self.grid_ref_lonID -> SetProperty, Value=0.0D, UValue=0.0D
            self.projectionID -> SetProperty, Selection='Azimuthal Equal-Area (South Pole)'
            self.datumID -> SetProperty, Selection='EASE-Grid Sphere'
            mapInfo = self -> MapLabel_to_MapProj('Azimuthal Equal-Area (South Pole)')
            self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
            self -> UpdateMapDatum
            self -> UpdateMapProjection
            RETURN, 1
            END

        ; Assume failure.
        ELSE: RETURN, 0
    
    ENDCASE

END

;*****************************************************************************************************
; NAME:
;        GPD_VIEWER::ConstructFilename
;
; PURPOSE:
;
;        This method is used to construct a filename from the current GUI setup.
;
; SYNTAX:
;
;        filename = self -> ConstructFilename(fileType)
;        
; RETURN_VALUE:
; 
;       filename:   The constructed filename.
;
; ARGUMENTS:
;
;       fileType:   A string indicating what type of file is desired. Values are "LATITUDE",
;                   "LONGITUDE", or "GPD".
;
; KEYWORDS:
;
;       None.
;
;-
;*****************************************************************************************************
FUNCTION GPD_Viewer::ConstructFilename, fileType

    @cat_func_error_handler
    
    ; Assume a GPD file.
    IF N_Elements(fileType) EQ 0 THEN fileType = ''
    
    ; Root name starts with map projection.
    self.projectionID -> GetProperty, SELECTION=thisProjection 
    CASE thisProjection OF
        'Azimuthal Equal-Area (North Pole)': root = 'azimuthal_np_'
        'Azimuthal Equal-Area (South Pole)': root = 'azimuthal_sp_'
        'Polar Stereographic (North Pole)': root = 'polarstereo_np_'
        'Polar Stereographic (South Pole)': root = 'polarstereo_sp_'
        'Orthographic': root = 'ortho_'
        'Cylindrical Equirectangular (Global)': root = 'cylindrical_
        'Cylindrical Equal-Area (Global)': root = 'cylindrical_ea_
        'Mercator (Global)': root = 'mercator_'
    ENDCASE
    
    ; Add the size of the grid.
    self.gridcolsID -> GetProperty, Value=col
    self.gridrowsID -> GetProperty, Value=row
    root = root + String(col, FORMAT='(I0)') + 'x' + String(row, FORMAT='(I0)')
    
    ; Add meter per grid cell.
    self.metersPerGridCellID -> GetProperty, Value=metersPerGridCell
    m = String(metersPerGridCell, FORMAT='(I0)')
    root = root + '_' + m + 'm'
    
    ; Finish with file extension.
    CASE fileType OF
        'GPD':       filename = root + '.gpd'
        'LATITUDE':  filename = root + '.lat'
        'LONGITUDE': filename = root + '.lon'
        ELSE:        filename = root
    ENDCASE
    
    RETURN, filename
    
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
      'DRAW_WIDGET': self -> SelectGridCenter, event
      
      ; Grid manipulations
      'GRID_XSIZE': self -> UpdateMapProjection
      'GRID_YSIZE':  self -> UpdateMapProjection
      'GRID_CENTER_LATITUDE': self -> UpdateGridCenterLatLon
      'GRID_CENTER_LONGITUDE': self -> UpdateGridCenterLatLon
      'GRID_X_CORNER': self -> UpdateGridCorner
      'GRID_Y_CORNER': self -> UpdateGridCorner
      'GRID_POSITION_SELECTOR': BEGIN
            event.id -> GetProperty, VALUE=buttonValue
            IF buttonValue EQ 'Lat/Lon' THEN BEGIN
                self.grid_ref_latID -> SetProperty, Sensitive = 1
                self.grid_ref_lonID -> SetProperty, Sensitive = 1
                self.grid_ref_yID -> SetProperty, Sensitive = 0
                self.grid_ref_xID -> SetProperty, Sensitive = 0
            ENDIF ELSE BEGIN
                self.grid_ref_latID -> SetProperty, Sensitive = 0
                self.grid_ref_lonID -> SetProperty, Sensitive = 0
                self.grid_ref_yID -> SetProperty, Sensitive = 1
                self.grid_ref_xID -> SetProperty, Sensitive = 1            
            ENDELSE
            END      
      ; Map manipulations.
      'MAP_PROJECTION': self -> SetMapProjection
      'MAP_DATUM': self -> UpdateMapDatum
      'MAP_CENTER_LATITUDE': self -> UpdateMapProjection
      'MAP_CENTER_LONGITUDE': self -> UpdateMapProjection
      'MAP_TRUESCALE_LATITUDE': self -> UpdateMapProjection
      'SEMIMAJOR_AXIS': self -> UpdateMapProjection
      'SEMIMINOR_AXIS': self -> UpdateMapProjection
      'METERS_PER_GRID_CELL': self -> UpdateMapProjection
      'ZOOM_IN': self -> ZoomMap, event
      'ZOOM_OUT': self -> ZoomMap, event
      'DEFAULTS': BEGIN
            self -> SetDefaultGPDGrid
            self -> SetMapProjection
            END
      
      ; Miscellaneous operations
      'DEFAULT_GPD_GRID': self -> SetDefaultGPDGrid
      'OPEN_GPD_FILE': self -> Open_GPD_File
      'WRITE_GPD_FILE': self -> Write_GPD_File
      'WRITE_LATLON_FILES': self -> Write_LatLon_Files
      'WRITE_XYGRID_FILES': self -> Write_XYGrid_Files
      'EXPORT_LATLON': self -> Write_LatLon_Files, /EXPORT
      'EXPORT_XYGRID': self -> Write_XYGrid_Files, /EXPORT
      'NSIDC_GPD': BEGIN
            event.id -> GetProperty, VALUE=selection
            ok = self -> CheckKnownFileList(selection)
            END
      'EXPORT_MAPCOORD_OBJECT': BEGIN
      
            varName = TextBox(CANCEL=cancelled, LABEL='Variable Name: ', $
                TITLE='Export MapCoord Object', VALUE='mapCoord', GROUP_LEADER=self->GetID())
            IF cancelled THEN RETURN

             ; Create the variable at the main IDL level.
             (Scope_VarFetch(varName, LEVEL=1, /ENTER)) = self.mapCoord
              Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
            
            END
      'EXIT': OBJ_DESTROY, self

       ELSE: ; Fall though.
       
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
     
     fancyfont = (!Version.OS_Family EQ 'Windows') ? 'Times*16*Italic*Bold' : '7x14bold'
     
     fileID = Obj_New('ButtonWidget', menubarID, VALUE='File')
     loadGPD = Obj_New('ButtonWidget', fileID, Value='Load NSIDC GPD File', /MENU)
     button = Obj_New('ButtonWidget', loadGPD, Value='N3A.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='N3B.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Na25.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Na5.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Na1.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='S3A.gpd', NAME='NSIDC_GPD', /SEPARATOR)
     button = Obj_New('ButtonWidget', loadGPD, Value='S3B.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Sa25.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Sa5.gpd', NAME='NSIDC_GPD')
     button = Obj_New('ButtonWidget', loadGPD, Value='Sa1.gpd', NAME='NSIDC_GPD')
     
     button = Obj_New('ButtonWidget', fileID, Value='Open GPD File', NAME='OPEN_GPD_FILE')
     button = Obj_New('ButtonWidget', fileID, Value='Write GPD File', NAME='WRITE_GPD_FILE')     
     button = Obj_New('ButtonWidget', fileID, Value='Set Default GPD Grid', $
        NAME='DEFAULT_GPD_GRID', /SEPARATOR)
     button = Obj_New('ButtonWidget', fileID, Value='Write Grid Lat/Lon Files', $
        NAME='WRITE_LATLON_FILES', /SEPARATOR)
     button = Obj_New('ButtonWidget', fileID, Value='Write XGrid/YGrid Files', $
        NAME='WRITE_XYGRID_FILES')
     button = Obj_New('ButtonWidget', fileID, Value='Export MapCoord Object to IDL', $
        NAME='EXPORT_MAPCOORD_OBJECT', /SEPARATOR)
     button = Obj_New('ButtonWidget', fileID, Value='Export Latitude and Longitude Arrays to IDL', $
        NAME='EXPORT_LATLON')
     button = Obj_New('ButtonWidget', fileID, Value='Export XGrid and YGrid Vectors to IDL', $
        NAME='EXPORT_XYGRID')
     button = Obj_New('ButtonWidget', fileID, Value='Exit', NAME='EXIT', /SEPARATOR) 
     
     mainbase = Obj_New('BaseWidget', self, XPAD=0, YPAD=0, ROW=1)
     controlBase = Obj_New('BaseWidget', mainbase, XPAD=3, YPAD=3, COLUMN=1, SPACE=10)
     rightBase = Obj_New('BaseWidget', mainbase, COLUMN=1, XPAD=0, YPAD=0)
     drawBase = Obj_New('BaseWidget', rightBase, COLUMN=1)
     
     mapLabelBase = Obj_New('BaseWidget', controlBase, XPAD=0, YPAD=0)
     label = Obj_New('LabelWidget', mapLabelBase, XOFFSET=5, Value='   Map Parameters   ', $
        FONT=fancyfont)
     labelGeometry = Widget_Info(label->GetID(), /Geometry)
     labelYSize=labelGeometry.ysize
     mapBase = Obj_New('BaseWidget', mapLabelBase, XPAD=0, YPAD=5, COLUMN=1, $
        /Frame, YOFFSET=labelYSize/2)
     label = Obj_New('LabelWidget', mapLabelBase, XOFFSET=5, Value='   Map Parameters   ', $
        FONT=fancyfont)

     gridLabelBase = Obj_New('BaseWidget', controlBase, XPAD=0, YPAD=0)
     label = Obj_New('LabelWidget', gridLabelBase, XOFFSET=5, Value='   Grid Parameters   ', $
        FONT=fancyfont)
     labelGeometry = Widget_Info(label->GetID(), /Geometry)
     labelYSize=labelGeometry.ysize
     gridBase = Obj_New('BaseWidget', gridLabelBase, XPAD=0, YPAD=5, COLUMN=1, /Frame, $
        YOFFSET=labelYSize/2)
     label = Obj_New('LabelWidget', mapLabelBase, XOFFSET=5, Value='   Grid Parameters   ', $
        FONT=fancyfont)
     
     map_projections =     [ 'Azimuthal Equal-Area (North Pole)', $
                             'Azimuthal Equal-Area (South Pole)', $
                             'Polar Stereographic (North Pole)', $
                             'Polar Stereographic (South Pole)', $
                             'Orthographic', $
                             'Cylindrical Equirectangular (Global)', $
                             'Cylindrical Equal-Area (Global)', $
                             'Mercator (Global)']
                         
     map_datums = [ 'Sphere', $
                    'EASE-Grid Sphere', $
                    'WGS 84', $
                    'Clarke 1866', $
                    'Hough', $
                    'Hughes']
             
     ; Align things to this size.               
     labelsize = 200
      
     self.projectionID = Obj_New('DroplistWidget', mapBase, VALUE=map_projections, $
                             TITLE='Map Projection: ', LABEL_SIZE=labelsize, SPACES=[0,5], $
                             NAME='MAP_PROJECTION')
     self.datumID = Obj_New('DroplistWidget', mapBase, VALUE=map_datums, $
                             TITLE='Map Datum: ', LABEL_SIZE=labelsize, SPACES=[0,5], $
                             NAME='MAP_DATUM')
     self.semimajor_axisID = Obj_New('FieldWidget', mapBase, VALUE = 6370997.00D, DECIMAL=2, $
                             TITLE='Semi-Major Axis (m): ', LABELSIZE=labelsize, $
                             NAME='SEMIMAJOR_AXIS', /CR_EVENTS)
     self.semiminor_axisID = Obj_New('FieldWidget', mapBase, VALUE = 6370997.00D, DECIMAL=2, $
                             TITLE='Semi-Minor Axis (m): ', LABELSIZE=labelsize, $
                             NAME='SEMIMINOR_AXIS', /CR_EVENTS, SENSITIVE=0)
     self.map_ref_latID = Obj_New('FieldWidget', mapBase, VALUE = 90.0D, DECIMAL=6, $
                             TITLE='Map Origin Latitude: ', LABELSIZE=labelsize, $
                             NAME='MAP_CENTER_LATITUDE', /CR_EVENTS, SENSITIVE=0)
     self.map_ref_lonID = Obj_New('FieldWidget', mapBase, VALUE = 0.0D, DECIMAL=6, $
                             TITLE='Map Origin Longitude: ', LABELSIZE=labelsize, $
                             NAME='MAP_CENTER_LONGITUDE', /CR_EVENTS)
     self.map_truescaleID = Obj_New('FieldWidget', mapBase, VALUE = 70.0D, DECIMAL=6, $
                             TITLE='Latitude of True Scale: ', LABELSIZE=labelsize, $
                             NAME='MAP_TRUESCALE_LATITUDE', /CR_EVENTS, /LABEL_LEFT)
     self.map_truescaleID -> SetProperty, SENSITIVE=0
     self.metersPerGridCellID = Obj_New('FieldWidget', gridBase, VALUE = 5000.0D, DECIMAL=4, $
                             TITLE='Number of Meters per Grid Cell: ', LABELSIZE=labelsize, $
                             NAME='METERS_PER_GRID_CELL', /CR_EVENTS, /LABEL_LEFT)        
     self.gridColsID = Obj_New('FieldWidget', gridBase, VALUE = 1000, DIGITS=8, $
                             TITLE='Number of Grid Columns: ', LABELSIZE=labelsize, $
                             NAME='GRID_XSIZE', /CR_EVENTS, /LABEL_LEFT)
     self.gridRowsID = Obj_New('FieldWidget', gridBase, VALUE = 1000, DIGITS=8, $
                             TITLE='Number of Grid Rows: ', LABELSIZE=labelsize, $
                             NAME='GRID_YSIZE', /CR_EVENTS, /LABEL_LEFT)
     locateBase = Obj_New('BaseWidget', gridBase, ROW=1)
     label = Obj_New('LabelWidget', locateBase, Value='Position Grid By: ', XSIZE=labelsize)
     selectBase = Obj_New('BaseWidget', locateBase, /EXCLUSIVE, ROW=1, FRAME=1, XPAD=10, YPAD=0)
     latlonbutton = Obj_New('ButtonWidget', selectBase, Value='Lat/Lon', /NO_RELEASE, NAME='GRID_POSITION_SELECTOR')
     colrowbutton = Obj_New('ButtonWidget', selectBase, Value='UL X/Y', /NO_RELEASE, NAME='GRID_POSITION_SELECTOR')
     latlonbutton -> SetProperty, SET_BUTTON=1
     self.grid_ref_latID = Obj_New('FieldWidget', gridBase, VALUE = 90.0D, UVALUE = 90.0D, DECIMAL=6, $
                             TITLE='Grid Center Latitude: ', LABELSIZE=labelsize, $
                             NAME='GRID_CENTER_LATITUDE', /CR_EVENTS)
     self.grid_ref_lonID = Obj_New('FieldWidget', gridBase, VALUE = 0.0D, UVALUE=0.0D, DECIMAL=6, $
                             TITLE='Grid Center Longitude: ', LABELSIZE=labelsize, $
                             NAME='GRID_CENTER_LONGITUDE', /CR_EVENTS)
     self.grid_ref_xID = Obj_New('FieldWidget', gridBase, VALUE = 500.0D, DECIMAL=2, $
                             TITLE='UL X Grid Corner ', LABELSIZE=labelsize, $
                             NAME='GRID_X_CORNER', /CR_EVENTS, SENSITIVE=0)
     self.grid_ref_yID = Obj_New('FieldWidget', gridBase, VALUE = 500.0D, DECIMAL=2, $
                             TITLE='UL Y Grid Corner: ', LABELSIZE=labelsize, $
                             NAME='GRID_Y_CORNER', /CR_EVENTS, SENSITIVE=0)
                             
     latlonBase = Obj_New('BaseWidget', rightBase, /GRID_LAYOUT, ROW=5)
     child = Obj_New('LabelWidget', latlonBase, Value='Grid Boundary', /ALIGN_LEFT, /DYNAMIC_RESIZE)
     IF !Version.OS_Family EQ 'Windows' THEN BEGIN
         child = Obj_New('LabelWidget', latlonBase, Value='UL   ', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='UR   ', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='LR   ', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='LL   ', /ALIGN_CENTER)
     ENDIF ELSE BEGIN
         child = Obj_New('LabelWidget', latlonBase, Value='UL', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='UR', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='LR', /ALIGN_CENTER)
         child = Obj_New('LabelWidget', latlonBase, Value='LL', /ALIGN_CENTER)     
     ENDELSE
     child = Obj_New('LabelWidget', latlonBase, Value='Longitude', /ALIGN_LEFT, /DYNAMIC_RESIZE)
     self.lon_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lon_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     child = Obj_New('LabelWidget', latlonBase, Value='Latitude', /ALIGN_LEFT)
     self.lat_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.lat_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     child = Obj_New('LabelWidget', latlonBase, Value='X', /ALIGN_LEFT)
     self.x_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.x_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.x_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.x_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     child = Obj_New('LabelWidget', latlonBase, Value='Y', /ALIGN_LEFT)
     self.y_ul = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.y_ur = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.y_lr = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     self.y_ll = Obj_New('LabelWidget', latlonBase, Value='xxx', /ALIGN_CENTER, /DYNAMIC_RESIZE)
     ; Map zoom buttons.
     zoomBase = Obj_New('BaseWidget', rightBase, SPACE=10, ROW=1)
     self.zoomIn = Obj_New('ButtonWidget', zoomBase, Value='Zoom Map In', NAME='ZOOM_IN')
     self.zoomOut = Obj_New('ButtonWidget', zoomBase, Value='Zoom Map Out', NAME='ZOOM_OUT')
     button = Obj_New('ButtonWidget', zoomBase, Value='Defaults', NAME='DEFAULTS')
     
     
     ; Comment field.
     commentBase = Obj_New('BaseWidget', controlBase, XPAD=0, YPAD=5)
     label = Obj_New('LabelWidget', commentBase, XOFFSET=5, Value='   File Comments   ', $
        FONT=fancyfont)
     labelGeometry = Widget_Info(label->GetID(), /Geometry)
     labelYSize=labelGeometry.ysize
     commentBaseID = Obj_New('BaseWidget', commentBase, XPAD=0, YPAD=0, COLUMN=1, $
        /Frame, YOFFSET=labelYSize/2, SPACE=10)
     label = Obj_New('LabelWidget', commentBase, XOFFSET=5, Value='   File Comments   ', $
        FONT=fancyfont)
     blanklabel = Obj_New('labelWidget', commentBaseID, SCR_YSIZE=2, VALUE="")
     self.commentID = Obj_New('TextWidget', commentBaseID, YSIZE=5,/EDITABLE, $
        VALUE='GPD file created with GPD_Viewer.')
     mapbase -> GetProperty, GEOMETRY=geo
     self.commentID -> SetProperty, SCR_XSIZE=geo.scr_xsize - 4     
     
     ; Set up the initial map projection space.
     self.limit = [14.8, -180, 90, 180]
     self.mapCoord = Obj_New('MapCoord', 111, /GCTP,  CENTER_LATITUDE=90, $
                               CENTER_LONGITUDE=0, LIMIT=self.limit, NAME='BASEMAP')
     
     self.mapBase = Obj_New('Map_Outline', MAP_OBJECT=self.mapCoord, COLOR='burlywood', /FILL)
     self.mapGrid = Obj_New('Map_Grid', MAP_OBJECT=self.mapCoord, COLOR='charcoal', $
        CHARSIZE=0.75, LONLAB=60, /FIXED_MAP_GRID)     
     self.mapBox = Obj_New('Map_Plots', MAP_OBJECT=self.mapCoord, COLOR='indian red')
     
     IF StrUpCase(!Version.OS_Family) EQ 'WINDOWS' THEN BEGIN
        self.drawID = Obj_New('DrawWidget', drawBase, XSIZE=350, YSIZE=350, COORD_OBJECT=self.mapCoord, $
                       INITIAL_COLOR='Ivory', ERASE_WINDOW=1, BUTTON_EVENTS=1, NAME='DRAW_WIDGET')
     ENDIF ELSE BEGIN
      self.drawID = Obj_New('DrawWidget', drawBase, XSIZE=450, YSIZE=450, COORD_OBJECT=self.mapCoord, $
                       INITIAL_COLOR='Ivory', ERASE_WINDOW=1, BUTTON_EVENTS=1, NAME='DRAW_WIDGET')
     ENDELSE                  
     self.drawID -> Add, self.mapBase, POSITION=0
     self.drawID -> Add, self.mapGrid, POSITION=1
     self.drawID -> Add, self.mapBox, POSITION=2
     
      mapBaseGeometry = Widget_Info(mapBase->GetID(), /Geometry)
      gridBase -> SetProperty, SCR_XSIZE=mapBaseGeometry.scr_xsize
     
     
     ; Keep draw widgets from becoming active windows.
     Widget_Control, self->getID(), /Managed

     ; Set up the map projection.
     self -> SetMapProjection
     
     ; Display the entire application in the window.
     self -> Draw, /Center
     
     ; Save program information.
     self.map_projections = Ptr_New(map_projections, /NO_COPY)
     self.map_datums = Ptr_New(map_datums, /NO_COPY)
     self.currentDatum = 'Sphere'

END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::MapLabel_to_MapProj
;
; PURPOSE:
;
;       This method sets up the default map projection when a user selects a
;       map projection from the GUI interface.
;
; SYNTAX:
;
;       theObject -> MapLabel_to_MapProj, mapLabel
;
; ARGUMENTS:
;
;     mapLabel:   The name of the map projection selected by the user on the GUI.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
FUNCTION GPD_Viewer::MapLabel_to_MapProj, mapLabel, SKIP_LATLON_UPDATE=skip_latlon_update

    ; This function turns the Map Projection label into a map projection structure, with
    ; information suitable for MAP_PROJ_INIT. It also sets up various properties of the
    ; interface. The SKIP_LATLON_UPDATE keyword will allow you to skip the resetting of
    ; the map projection latitude and longitude, which is desireable under some conditions.
    
    @cat_func_error_handler
    
    ; Get the current datum.
    self.datumID -> GetProperty, SELECTION=currentDatum

    CASE mapLabel OF
        
        'Azimuthal Equal-Area (North Pole)': BEGIN
                map_projection = 111
                self.limit = [14.8, -180, 90, 180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=90, UValue=90
                    self.map_ref_lonID -> SetProperty, Value=0, UValue=0
                ENDIF
                IF StrPos(currentDatum, 'Sphere') EQ -1 THEN BEGIN
                    currentDatum = 'EASE-Grid Sphere'
                    self.datumID -> SetProperty, SELECTION='EASE-Grid Sphere'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=90
                self.grid_ref_lonID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=1
                self.zoomout -> SetProperty, Sensitive=1
                self.map_truescaleID -> SetProperty, Sensitive=0
                self.drawID -> SetProperty, XSIZE=450, YSIZE=450
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
                map_projection = 111
                self.limit = [-90, -180, -14.8, 180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=-90
                    self.map_ref_lonID -> SetProperty, Value=0
                ENDIF
                IF StrPos(currentDatum, 'Sphere') EQ -1 THEN BEGIN
                    currentDatum = 'EASE-Grid Sphere'
                    self.datumID -> SetProperty, SELECTION='EASE-Grid Sphere'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=-90
                self.grid_ref_lonID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=1
                self.zoomout -> SetProperty, Sensitive=1
                self.map_truescaleID -> SetProperty, Sensitive=0
                self.drawID -> SetProperty, XSIZE=450, YSIZE=450
            END
            
        'Cylindrical Equal-Area (Global)': BEGIN
                map_projection = 103 ; EASE grid uses a Behrmann Cylindrical Equal-Area projection.
                                     ; This is faked here using an Albers Equal-Area projection.
                self.limit = [-85.95, -180.0, 86.72, 180.0]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=0
                    self.map_ref_lonID -> SetProperty, Value=0
                    self.map_trueScaleID -> SetProperty, Value=30
                ENDIF
                IF StrPos(currentDatum, 'Sphere') EQ -1 THEN BEGIN
                    currentDatum = 'Sphere'
                    self.datumID -> SetProperty, SELECTION='Sphere'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=0
                self.grid_ref_lonID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=0
                self.zoomout -> SetProperty, Sensitive=0
                self.map_truescaleID -> SetProperty, Sensitive=0
                self.semimajor_axisID -> SetProperty, Value=6371228L
                self.semiminor_axisID -> SetProperty, Value=6371228L
                self.drawID -> SetProperty, XSIZE=450, YSIZE=225
            END

                       
        'Cylindrical Equirectangular (Global)': BEGIN
                map_projection = 117
                self.limit = [-85.95, -180.0, 86.72, 180.0]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=0
                    self.map_ref_lonID -> SetProperty, Value=0
                    self.map_trueScaleID -> SetProperty, Value=40
                ENDIF            
                IF StrPos(currentDatum, 'Sphere') EQ -1 THEN BEGIN
                    currentDatum = 'Sphere'
                    self.datumID -> SetProperty, SELECTION='Sphere'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=0
                self.grid_ref_lonID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=0
                self.zoomout -> SetProperty, Sensitive=0
                self.map_truescaleID -> SetProperty, Sensitive=1
                self.drawID -> SetProperty, XSIZE=450, YSIZE=225
            END

        'Mercator (Global)': BEGIN
                map_projection = 105
                self.limit = [-81, -180, 81,  180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=0
                    self.map_ref_lonID -> SetProperty, Value=0
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=0
                self.grid_ref_lonID -> SetProperty, Value=0
                self.map_trueScaleID -> SetProperty, Value=40
                self.zoomin -> SetProperty, Sensitive=0
                self.zoomout -> SetProperty, Sensitive=0
                self.map_truescaleID -> SetProperty, Sensitive=1
                self.drawID -> SetProperty, XSIZE=450, YSIZE=225
            END
            
        'Orthographic': BEGIN
                map_projection = 114
                self.limit = [-90, -180, 90, 180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=0
                ENDIF
                IF StrPos(currentDatum, 'Sphere') EQ -1 THEN BEGIN
                    currentDatum = 'EASE-Grid Sphere'
                    self.datumID -> SetProperty, SELECTION='EASE-Grid Sphere'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=0
                self.zoomout -> SetProperty, Sensitive=0
                self.map_ref_lonID -> SetProperty, Sensitive=1
                self.map_ref_latID -> SetProperty, Sensitive=1
                self.map_truescaleID -> SetProperty, Sensitive=0
                self.drawID -> SetProperty, XSIZE=450, YSIZE=450
            END
    
         'Polar Stereographic (North Pole)': BEGIN
                map_projection = 106
                self.limit = [14.8, -180, 90, 180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=90
                    self.map_ref_lonID -> SetProperty, Value=-45
                    self.map_trueScaleID -> SetProperty, Value=70
                ENDIF
                IF StrPos(currentDatum, 'Sphere') NE -1 THEN BEGIN
                    currentDatum = 'Hughes'
                    self.datumID -> SetProperty, SELECTION='Hughes'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=90
                self.grid_ref_lonID -> SetProperty, Value=-45
                self.zoomin -> SetProperty, Sensitive=1
                self.zoomout -> SetProperty, Sensitive=1
                self.map_truescaleID -> SetProperty, Sensitive=1
                self.drawID -> SetProperty, XSIZE=450, YSIZE=450
            END
            
        'Polar Stereographic (South Pole)': BEGIN
                map_projection = 106
                self.limit = [-90, -180, -14.8, 180]
                IF ~Keyword_Set(skip_latlon_update) THEN BEGIN
                    self.map_ref_latID -> SetProperty, Value=-90
                    self.map_ref_lonID -> SetProperty, Value=0
                    self.map_trueScaleID -> SetProperty, Value=-70
                ENDIF
                IF StrPos(currentDatum, 'Sphere') NE -1 THEN BEGIN
                    currentDatum = 'Hughes'
                    self.datumID -> SetProperty, SELECTION='Hughes'
                    self -> UpdateMapDatum
                ENDIF
                self.grid_ref_latID -> SetProperty, Value=-90
                self.grid_ref_lonID -> SetProperty, Value=0
                self.zoomin -> SetProperty, Sensitive=1
                self.zoomout -> SetProperty, Sensitive=1
                self.map_truescaleID -> SetProperty, Sensitive=1
                self.drawID -> SetProperty, XSIZE=450, YSIZE=450
            END
      
    ENDCASE
    
    ; Update the grid lines.
    self -> UpdateGridLines
    
    ; Set up return value.
    returnValue = {mapProjection:map_projection, limit:self.limit}
    
    RETURN, returnValue
    
END 



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::Open_GPD_File
;
; PURPOSE:
;
;       This method allows the user to open a GPD file that has been created by the
;       GPD_Viewer. The information in the file is used to populate the GUI.
;
; SYNTAX:
;
;       theObject -> Open_GPD_File, filename
;
; ARGUMENTS:
;
;     filename:   The name of the file to open.
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
    
    ; Need a filename?
    IF N_Elements(filename) EQ 0 THEN BEGIN
        filename = Dialog_Pickfile(TITLE='Select GPD File...', FILTER='*.gpd')
        IF filename EQ "" THEN RETURN
    ENDIF
    
    ; Check this filename against a list of files that you maintain and know something
    ; about. If you find it on your list. Go process it. If you don't find it on the
    ; list, assume it is a file you can read.
    processedCorrectly = self -> CheckKnownFileList(filename)
    IF processedCorrectly THEN RETURN
    
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
    ENDIF ELSE mapScale = 1.0
    loc = StrPos(upcaseText, 'MAP EQUATORIAL RADIUS:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        semimajor_axis = 0.0D
        ReadS, thisLine, semimajor_axis, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upcaseText, 'MAP POLAR RADIUS:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        semiminor_axis = 0.0D
        ReadS, thisLine, semiminor_axis, FORMAT ='(30x, D0)'
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
        ReadS, thisLine, xorigin, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upcaseText, 'MAP ORIGIN Y:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        yorigin = 0.0D
        ReadS, thisLine, yorigin, FORMAT ='(30x, D0)'
    ENDIF
    loc = StrPos(upcaseText, 'GRID CENTER LATITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lat = 0.0D
        ReadS, thisLine, grid_center_lat, FORMAT ='(30x, D0)'
        self.grid_ref_latID -> SetProperty, Value=grid_center_lat, UValue=grid_center_lat
    ENDIF
    loc = StrPos(upcaseText, 'MAP ORIGIN LATITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lat = 0.0D
        ReadS, thisLine, grid_center_lat, FORMAT ='(30x, D0)'
        self.grid_ref_latID -> SetProperty, Value=grid_center_lat, UValue=grid_center_lat
    ENDIF
    loc = StrPos(upcaseText, 'GRID CENTER LONGITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lon = 0.0D
        ReadS, thisLine, grid_center_lon, FORMAT ='(30x, D0)'
        self.grid_ref_lonID -> SetProperty, Value=grid_center_lon, UValue=grid_center_lon
     ENDIF
    loc = StrPos(upcaseText, 'MAP ORIGIN LONGITUDE:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_center_lon = 0.0D
        ReadS, thisLine, grid_center_lon, FORMAT ='(30x, D0)'
        self.grid_ref_lonID -> SetProperty, Value=grid_center_lon, UValue=grid_center_lon
     ENDIF
    loc = StrPos(upcaseText, 'GRID MAP ORIGIN COLUMN:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_column_at_map_origin = 0.0D
        ReadS, thisLine, grid_column_at_map_origin, FORMAT ='(30x, F0)'
        self.grid_ref_xID -> SetProperty, Value=grid_column_at_map_origin
     ENDIF
    loc = StrPos(upcaseText, 'GRID MAP ORIGIN ROW:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_row_at_map_origin = 0.0D
        ReadS, thisLine, grid_row_at_map_origin, FORMAT ='(30x, F0)'
        self.grid_ref_yID -> SetProperty, Value=grid_row_at_map_origin
     ENDIF
    loc = StrPos(upcaseText, 'GRID UPPER-LEFT X:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_ul_x = 0.0D
        ReadS, thisLine, grid_ul_x, FORMAT ='(30x, D0)'
        self.grid_ref_xID -> SetProperty, Value=grid_ul_x
     ENDIF
    loc = StrPos(upcaseText, 'GRID UPPER-LEFT Y:')
    index = Where(loc NE -1, count)
    IF count GT 0 THEN BEGIN
        thisLine = text[index[0]]
        grid_ul_y = 0.0D
        ReadS, thisLine, grid_ul_y, FORMAT ='(30x, D0)'
        self.grid_ref_yID -> SetProperty, Value=grid_ul_y
     ENDIF
     
     ; Calculate meters per grid cell.
     metersPerGridCell = mapScale * mapUnitsPerGrid
   
    ; Set up map projection information.
    self.projectionID -> SetProperty, SELECTION=map_projection
    
    ; Set the information to the display.
    self.datumID -> SetProperty, SELECTION=selected_datum
    self -> UpdateMapDatum
    
    ; Update the map. Necessary to get the grids correct and the map projection
    ; updated correctly.
    mapInfo = self -> MapLabel_to_MapProj(map_projection)
    self.mapCoord -> SetProperty, MAP_PROJECTION=mapInfo.mapProjection, LIMIT=mapInfo.limit
    
    ; Now fill in the other information.
    self.semimajor_axisID -> SetProperty, VALUE=semimajor_axis
    self.semiminor_axisID -> SetProperty, VALUE=semiminor_axis
    self.map_ref_latID -> SetProperty, Value=lat0
    self.map_ref_lonID -> SetProperty, Value=lon0
    self.metersPerGridCellID -> SetProperty, Value=metersPerGridCell
    self.gridColsID -> SetProperty, Value=xgridSize
    self.gridRowsID -> SetProperty, Value=ygridSize
    self.map_trueScaleID -> SetProperty, Value=truescale
    self.commentID -> SetProperty, Value=comment
    
    ; Locate the center of the map grid. Only have to do this if you can't
    ; find this information in the file.
    IF N_Elements(grid_center_lon) EQ 0 OR N_Elements(grid_center_lat) EQ 0 THEN BEGIN
        xcenter = xorigin + (xgridSize * metersPerGridCell) / 2.0
        ycenter = yorigin - (ygridSize * metersPerGridCell) / 2.0
        mapStruct = self.mapCoord -> GetMapStructure()
        ll = Map_Proj_Inverse(xcenter, ycenter, MAP_STRUCTURE=mapStruct)
        self.grid_ref_latID -> SetProperty, Value=grid_center_lat, UVALUE=grid_center_lat
        self.grid_ref_lonID -> SetProperty, Value=grid_center_lon, UVALUE=grid_center_lon
    ENDIF
    
    ; Update the map projection.
    self -> UpdateMapProjection
        
END





FUNCTION GPD_Viewer::ReadFileContents, filename

    ; Return to the caller with an error.
    On_Error, 2
    
    ; Open the file and read its contents.
    rows = File_Lines(filename)
    contents = StrArr(rows)
    OpenR, lun, filename, /Get_Lun
    ReadF, lun, contents
    Free_Lun, lun
    
    RETURN, contents
    
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SelectGridCenter
;
; PURPOSE:
;
;       This method allows the GPD grid to be positioned on the map by
;       locating the center of the grid with the mouse.
;
; SYNTAX:
;
;       theObject -> SelectGridCenter, event
;
; ARGUMENTS:
;
;     event:    The event structure.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::SelectGridCenter, event

    @cat_pro_error_handler
    
    ; Only interested in DOWN mouse events.
    IF event.type NE 0 THEN RETURN
    
    ; Where is this location. Convert device to data (lat/lon) coordinates.
    self.mapCoord -> Draw
    xy = Convert_Coord(event.x, event.y, /DEVICE, /TO_DATA)
    
;    ; The X and Y locations are in meters. If we divide by
;    ; number of meters / map unit, we have number of map units.
;    self.map_scaleID -> GetProperty, Value=metersPerMapUnit
;    xunits = xy[0] / metersPerMapUnit
;    yunits = xy[1] / metersPerMapUnit
;    
;    ; If we divide by map units / grid cell, we will get the number
;    ; of grid cells this location is from the origin.
;    self.mapUnitsPerGridID -> GetProperty, Value=mapUnitsPerGridCell
;    xcells = xunits / mapUnitsPerGridCell
;    ycells = yunits / mapUnitsPerGridCell
;    
;    ; The origin was orginally mapped to the center, so we subtract
;    ; this number from the center of the box to get the current
;    ; column and row location.
;    self.gridRowsID -> GetProperty, Value=grid_rows
;    self.gridColsID -> GetProperty, Value=grid_cols
;    self.grid_ref_yID -> SetProperty, Value=(grid_rows / 2.0) + ycells
;    self.grid_ref_xID -> SetProperty, Value=(grid_cols / 2.0) - xcells
    
    ; Convert this location to lat/lon coordinates.
    ll = Map_Proj_Inverse(xy[0], xy[1], MAP_STRUCTURE=self.mapCoord->GetMapStructure())

    ; Update the grid section of the GUI.
    self.grid_ref_latID -> SetProperty, Value=ll[1], UVALUE=ll[1]
    self.grid_ref_lonID -> SetProperty, Value=ll[0], UVALUE=ll[0]
    
    ; Redraw.
    self -> UpdateMapProjection
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SetDefaultGPDGrid
;
; PURPOSE:
;
;       This method draws a default GPD grid in the center of the current map
;       projection. It is probably only used if the user loses the GPD grid for
;       whatever reason.
;
; SYNTAX:
;
;       theObject -> SetDefaultGPDGrid
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
PRO GPD_Viewer::SetDefaultGPDGrid

    @cat_pro_error_handler

    ; Get the currently selected map projection.
    self.map_ref_latID -> GetProperty, Value=mapOriginLat
    self.map_ref_lonID -> GetProperty, Value=mapOriginLon
    
    ; Set current map settings.
    self.metersPerGridCellID -> SetProperty, Value=5000.0

    ; Set current grid settings on the display.
    self.gridRowsID -> SetProperty, Value=1000
    self.gridColsID -> SetProperty, Value=1000
    
    ; Update the map projection and draw.
    self -> UpdateMapProjection
    self.drawID -> Draw
    
END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::SetMapProjection
;
; PURPOSE:
;
;       This reads the current information from the GUI interface and calculates
;       the proper map projection and places the GPD box on the map in the proper 
;       place.
;
; SYNTAX:
;
;       theObject -> SetMapProjection
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     SKIP_LATLON_UPDATE:   If this keyword is set, it will set the same keyword to MAPLABEL_TO_MAPPROJ
;                           and by doing so prevent the new map projection from returning to the default
;                           center latitude and center longitude values.
;-
;*****************************************************************************************************
PRO GPD_Viewer::SetMapProjection, SKIP_LATLON_UPDATE=skip_latlon_update

   @cat_pro_error_handler

    ; Get the currently selected map projection.
    self.projectionID -> GetProperty, SELECTION=thisProjection 
    
    ; Get the real map projection. Don't move this because datums
    ; may have to change and the map information won't be current.
    projStruct = self -> MapLabel_To_MapProj(thisProjection, SKIP_LATLON_UPDATE=skip_latlon_update)
    
    ; Get current map settings on the display.
    self.map_ref_latID -> GetProperty, Value=mapOriginLat
    self.map_ref_lonID -> GetProperty, Value=mapOriginLon
    self.map_trueScaleID -> GetProperty, Value=trueScale
    self.semiminor_axisID -> GetProperty, Value=semiminor_axis
    self.semimajor_axisID -> GetProperty, Value=semimajor_axis
    
    ; Get current grid settings on the display.
    self.gridRowsID -> GetProperty, Value=grid_rows
    self.gridColsID -> GetProperty, Value=grid_cols

    ; There is weirdness with Polar Stereo projections.
    IF StrPos(thisProjection, 'Polar Stereo') NE -1 THEN mapOriginLat = trueScale
    
    ; Some map projections will need an update of their trueScale latitude
    ; and other properties.
    CASE 1 OF
       StrPos(thisProjection, 'Mercator') NE -1: BEGIN
            map_proj_keywords = {NULL:1, TRUE_SCALE_LATITUDE:truescale}
            END
       StrPos(thisProjection, 'Equirectangular') NE -1: BEGIN
            map_proj_keywords = {NULL:1, TRUE_SCALE_LATITUDE:truescale}
            END
       StrPos(thisProjection, 'Cylindrical Equal-Area') NE -1: BEGIN
            map_proj_keywords = {NULL:1, STANDARD_PAR1:60, STANDARD_PAR2:-59.5}
            END
       ELSE: map_proj_keywords = {NULL:1} 
     ENDCASE
     
     ; Set the new map projection.
     self.mapCoord -> SetProperty, $
                MAP_PROJECTION=projStruct.mapProjection, $
                SEMIMAJOR_AXIS=semimajor, $
                SEMIMINOR_AXIS=semiminor, $
                CENTER_LATITUDE=mapOriginLat, $
                CENTER_LONGITUDE=mapOriginLon, $
                LIMIT=projStruct.limit, $
                MAP_PROJ_KEYWORDS=map_proj_keywords

    ; Update grid center
    self -> UpdateGridCenterLatLon

    ; Update grid box location.
    self -> UpdateGridLocation
    
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
;       event:  The event structure
;
; KEYWORDS:
;
;      None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::ZoomMap, event

    ; Basically, change the map limits by 5 degrees, recognizing some reasonable min or max.

    @cat_pro_error_handler

    ; Collect information you will need.
    self.projectionID -> GetProperty, SELECTION=thisProjection
    self.map_ref_latID -> GetProperty, Value=lat0
    self.map_ref_lonID -> GetProperty, Value=lon0
    
    CASE thisProjection OF
    
        'Azimuthal Equal-Area (North Pole)': BEGIN
            IF event.name EQ 'ZOOM_IN' THEN BEGIN
                newlat = (self.limit[0]+5) < 70
            ENDIF ELSE BEGIN
                newlat = (self.limit[0]-5) > 0   
            ENDELSE
            self.limit = [newlat, -180, 90, 180]
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
            IF event.name EQ 'ZOOM_IN' THEN BEGIN
                newlat = (self.limit[2]-5) > (-70)
            ENDIF ELSE BEGIN
                newlat = (self.limit[2]+5) < 5  
            ENDELSE
            self.limit = [-90, -180, newlat, 180]                        
            END
            
        'Cylindrical Equirectangular (Global)': 
        
        'Cylindrical Equal-Area (Global)': 
        
        'Mercator (Global)': 
                    
        'Orthographic': 
        
        'Polar Stereographic (North Pole)': BEGIN
            IF event.name EQ 'ZOOM_IN' THEN BEGIN
                newlat = (self.limit[0]+5) < 70
            ENDIF ELSE BEGIN
                newlat = (self.limit[0]-5) > 5 
            ENDELSE
            self.limit = [newlat, -180, 90, 180]                        
            END
                    
        'Polar Stereographic (South Pole)': BEGIN
            IF event.name EQ 'ZOOM_IN' THEN BEGIN
                newlat = (self.limit[2]-5) > (-70)
            ENDIF ELSE BEGIN
                newlat = (self.limit[2]+5) < 1  
            ENDELSE
            self.limit = [-90, -180, newlat, 180]                        
            END
                    
    ENDCASE
    
    ; Update the range.
    self.mapCoord -> SetProperty, LIMIT=self.limit
    mapStruct = self.mapCoord -> GetMapStructure()
    self.mapCoord -> SetProperty, XRANGE=mapStruct.uv_box[[0,2]], YRANGE=mapStruct.uv_box[[1,3]]
    

    ; Update the display
    self.drawID -> Draw
    
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
;       GPD_VIEWER::UpdateGridCorner
;
; PURPOSE:
;
;       This method is called whenever the user changes the grid corner widgets.
;       It's purpose is to update the grid center latitude and longitude fields 
;       on the GUI.
;
; SYNTAX:
;
;     theObject -> UpdateGridCorner
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
PRO GPD_Viewer::UpdateGridCorner

    @cat_pro_error_handler

    ; Someone changed coner values of the grid box. We need to 
    ; update the display and the grid box.
     
    ; Get the current grid corner values.
    self.grid_ref_xID -> GetProperty, Value=x
    self.grid_ref_yID -> GetProperty, Value=y
    
    ; Where is the center of the grid in XY?
    self.metersPerGridCellID -> GetProperty, Value=metersPerGridCell
    self.gridRowsID -> GetProperty, Value=grid_rows
    self.gridColsID -> GetProperty, Value=grid_cols
    ycenter = y - ((grid_rows/2.0) * metersPerGridCell)
    xcenter = x + ((grid_cols/2.0) * metersPerGridCell)
    
    ; Convert the number of centers to lat/lon.
    ll = Map_Proj_Inverse(xcenter, ycenter, MAP_STRUCTURE=self.mapCoord->GetMapStructure())            
    
    ; Update the GUI.
    self.grid_ref_lonID -> SetProperty, Value=String(ll[0], Format='(D0.6)'), UVALUE=ll[0]
    self.grid_ref_latID -> SetProperty, Value=String(ll[1], Format='(D0.6)'), UVALUE=ll[1]
         
    ; Redraw.
    self -> UpdateMapProjection
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateGridCenterLatLon
;
; PURPOSE:
;
;       This method is called whenever the user changes the grid center latitude or longitude.
;       The purpose is to fill out the proper grid column and row at map origin values and
;       then to call the UpdateGridLocation method.
;
; SYNTAX:
;
;     theObject -> UpdateGridCenterLatLon
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
PRO GPD_Viewer::UpdateGridCenterLatLon

    @cat_pro_error_handler

    ; Someone changed the center of the grid box. We need to 
    ; update the display and the grid box.
     
    ; Get the current latitude and longitude and convert them to meters.
    self.grid_ref_latID -> GetProperty, Value=lat
    self.grid_ref_lonID -> GetProperty, Value=lon
    
    ; These need to go into the UVALUE to take effect in the map projection updating.
    self.grid_ref_latID -> SetProperty, UValue=lat
    self.grid_ref_lonID -> SetProperty, UValue=lon
    
;    ; Get a map structure.
;    xy = Map_Proj_Forward(lon, lat, MAP_STRUCTURE=self.mapCoord->GetMapStructure())            
;    
;    ; The X and Y locations are in meters. If we divide by
;    ; number of meters / map unit, we have number of map units.
;    self.map_scaleID -> GetProperty, Value=metersPerMapUnit
;    xunits = xy[0] / metersPerMapUnit
;    yunits = xy[1] / metersPerMapUnit
;    
;    ; If we divide by map units / grid cell, we will get the number
;    ; of grid cells this location is from the origin.
;    self.mapUnitsPerGridID -> GetProperty, Value=mapUnitsPerGridCell
;    xcells = xunits / mapUnitsPerGridCell
;    ycells = yunits / mapUnitsPerGridCell
;    
;    ; The origin was orginally mapped to the center, so we subtract
;    ; this number from the center of the box to get the current
;    ; column and row location.
;    self.gridRowsID -> GetProperty, Value=grid_rows
;    self.gridColsID -> GetProperty, Value=grid_cols
;    
;    row = (grid_rows / 2.0D) + (ycells - 0.5)
;    col = (grid_cols / 2.0D) - (xcells + 0.5)
;    self.grid_ref_yID -> SetProperty, Value=row
;    self.grid_ref_xID -> SetProperty, Value=col
        
    ; Redraw.
    self -> UpdateMapProjection
    
END



;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateGridLines
;
; PURPOSE:
;
;       This method simply specifies the latitude and longitude grid lines
;       for the various map projections. It is called whenever a map projection
;       changes, for whatever reason.
;
; SYNTAX:
;
;     theObject -> UpdateGridLines
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
PRO GPD_Viewer::UpdateGridLines

    @cat_pro_error_handler

    ; Get the current map projection
    self.projectionID -> GetProperty, SELECTION=thisProjection


    CASE thisProjection OF
        
        'Azimuthal Equal-Area (North Pole)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = 38, $
                    LATDEL=10, $
                    LATS = Indgen(4)*20+15, $
                    LatLab = 18, $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = -38, $
                    LATDEL=10, $
                    LATS = Indgen(4)*(-20)-15, $
                    LatLab = -(18+180), $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END
                       
        'Cylindrical Equirectangular (Global)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = -25, $
                    LATDEL=10, $
                    LATS = Indgen(9)*20-80, $
                    LatLab = 18, $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END

        'Cylindrical Equal-Area (Global)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = -25, $
                    LATDEL=10, $
                    LATS = Indgen(9)*20-80, $
                    LatLab = 18, $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END

        'Mercator (Global)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = -25, $
                    LATDEL=10, $
                    LATS = Indgen(10)*20-80, $
                    LatLab = 18, $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END
            
        'Orthographic': BEGIN
                self.map_ref_latID -> GetProperty, Value=currentLat
                self.map_ref_lonID -> GetProperty, Value=currentLon
                lats = Round(Scale_Vector(Indgen(9), currentLat-80, currentLat+80))
                IF currentLon EQ 0.0 THEN BEGIN
                   lons = Round(Scale_Vector(Indgen(9), currentLon-144, currentLon+144))
                ENDIF ELSE BEGIN
                   lons = Round(Scale_Vector(Indgen(17), currentLon-180, currentLon+180))
                ENDELSE
                self.mapGrid -> SetProperty, $
                    LonLab = currentLat + 25, $
                    LATS = lats,  $
                    LatLab = currentLon - 15, $
                    LONS = lons
            END
    
         'Polar Stereographic (North Pole)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = 38, $
                    LATDEL=10, $
                    LATS = Indgen(4)*20+15, $
                    LatLab = 18, $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
            END
            
        'Polar Stereographic (South Pole)': BEGIN
                self.mapGrid -> SetProperty, $
                    LonLab = -38, $
                    LATDEL=10, $
                    LATS = Indgen(4)*(-20)-15, $
                    LatLab = -(18+180), $
                    LONDEL=36, $
                    LONS = Indgen(11)*36
           END
      
    ENDCASE

END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateLatLonLocations
;
; PURPOSE:
;
;       This method simply updates the corners of the grid box in latitude
;       and longitude values on the GUI.
;
; SYNTAX:
;
;     theObject -> UpdateLatLonLocations
;
; ARGUMENTS:
;
;     ll:   A 2-by-n array containing the longitude (in the first column) and
;           latitude (in the second column) of the grid corners. LL, UL, UR, LR.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::UpdateLatLonLocations, ll, uv


    @cat_pro_error_handler

     ; Calculate the box latitude and longitudes. LL, UL, UR, LR in the reference 
 
    ; Display the information on the GUI.
    self.lon_ll -> SetProperty, Value=String(ll[0,0], Format='(F0.4)'), UVALUE=ll[0,0]
    self.lon_ul -> SetProperty, Value=String(ll[0,1], Format='(F0.4)'), UVALUE=ll[0,1]
    self.lon_ur -> SetProperty, Value=String(ll[0,2], Format='(F0.4)'), UVALUE=ll[0,2]
    self.lon_lr -> SetProperty, Value=String(ll[0,3], Format='(F0.4)'), UVALUE=ll[0,3]
    
    self.lat_ll -> SetProperty, Value=String(ll[1,0], Format='(F0.4)'), UVALUE=ll[1,0]
    self.lat_ul -> SetProperty, Value=String(ll[1,1], Format='(F0.4)'), UVALUE=ll[1,1]
    self.lat_ur -> SetProperty, Value=String(ll[1,2], Format='(F0.4)'), UVALUE=ll[1,2]
    self.lat_lr -> SetProperty, Value=String(ll[1,3], Format='(F0.4)'), UVALUE=ll[1,3]
    
    self.x_ll -> SetProperty, Value=String(uv[0,0], Format='(D0.2)'), UVALUE=uv[0,0]
    self.x_ul -> SetProperty, Value=String(uv[0,1], Format='(D0.2)'), UVALUE=uv[0,1]
    self.x_ur -> SetProperty, Value=String(uv[0,2], Format='(D0.2)'), UVALUE=uv[0,2]
    self.x_lr -> SetProperty, Value=String(uv[0,3], Format='(D0.2)'), UVALUE=uv[0,3]

    self.y_ll -> SetProperty, Value=String(uv[1,0], Format='(D0.2)'), UVALUE=uv[1,0]
    self.y_ul -> SetProperty, Value=String(uv[1,1], Format='(D0.2)'), UVALUE=uv[1,1]
    self.y_ur -> SetProperty, Value=String(uv[1,2], Format='(D0.2)'), UVALUE=uv[1,2]
    self.y_lr -> SetProperty, Value=String(uv[1,3], Format='(D0.2)'), UVALUE=uv[1,3]
    
    self.grid_ref_xID -> SetProperty, VALUE=uv[0,1]
    self.grid_ref_yID -> SetProperty, VALUE=uv[1,1]
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateMapDatum
;
; PURPOSE:
;
;       This method updates map data information when the user selects another
;       datum from the GUI. Not all datums are appropriate for a particular map
;       projection. This method sorts that out and only allows legal datums to be
;       used.
;
; SYNTAX:
;
;     theObject -> UpdateMapDatum
;
; ARGUMENTS:
;
;     event:   The event structure passed from XManager.
;
; KEYWORDS:
;
;     None.
;-
;*****************************************************************************************************
PRO GPD_Viewer::UpdateMapDatum, event

    @cat_pro_error_handler

    ; What kind of map projection do we have? This will determine what
    ; kind of datam we can select.
    self.projectionID -> GetProperty, SELECTION=thisProjection

    ; Get the new datum.
    self.datumID -> GetProperty, SELECTION=selected_datum

    ; It doesn't make sense to have certain datums with certain projections.
    ; Check and make sure you can do the right thing here.
    CASE thisProjection OF
        'Azimuthal Equal-Area (North Pole)': BEGIN
            IF StrPos(selected_datum, 'Sphere') EQ -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END
            
        'Azimuthal Equal-Area (South Pole)': BEGIN
            IF StrPos(selected_datum, 'Sphere') EQ -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END
                       
        'Cylindrical Equirectangular (Global)': BEGIN
            IF StrPos(selected_datum, 'Sphere') EQ -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END

        'Cylindrical Equal-Area (Global)': BEGIN
            IF StrPos(selected_datum, 'Sphere') EQ -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END

        'Mercator (Global)': BEGIN
            END
            
        'Orthographic': BEGIN
            IF StrPos(selected_datum, 'Sphere') EQ -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END
    
         'Polar Stereographic (North Pole)': BEGIN
            IF StrPos(selected_datum, 'Sphere') NE -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END
            
        'Polar Stereographic (South Pole)': BEGIN
            IF StrPos(selected_datum, 'Sphere') NE -1 THEN BEGIN
                void = Dialog_Message('Selected Datum: ' + selected_datum + ' is not allowed for this map projection.')
                selected_datum = self.currentDatum
                self.datumID -> SetProperty, SELECTION=self.currentDatum
            ENDIF
            END
      
    ENDCASE

    ; Get the currently selected datum.
    self.datumID -> GetProperty, SELECTION=selected_datum

    ; Do the right thing, based on the currently selected datum.
    CASE selected_datum OF
        'Sphere' : BEGIN
            semimajor_axis = '6370997.00'
            semiminor_axis = '6370997.00'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=0
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            datum = 'Sphere'
            END
        'EASE-Grid Sphere' : BEGIN
            semimajor_axis = '6371228.00'
            semiminor_axis = '6371228.00'
            datum = 'Sphere'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=0
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            END
        'Clarke 1866' : BEGIN
            semimajor_axis = '6378206.40'
            semiminor_axis = '6356583.80'
            datum = 'Clark 1866'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=1
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            END
        'WGS 84' : BEGIN
            semimajor_axis = '6378137.00'
            semiminor_axis = '6356752.31'
            datum = 'WGS 84'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=1
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            END
        'Hough' : BEGIN
            semimajor_axis = '6378270.00'
            semiminor_axis = '6356794.34'
            datum = 'Hough'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=1
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            END
        'Hughes' : BEGIN
            semimajor_axis = '6378273.00'
            semiminor_axis = '6356889.50'
            datum = 'Hough'
            self.semiminor_axisID -> SetProperty, Value=Double(semiminor_axis), SENSITIVE=1
            self.semimajor_axisID -> SetProperty, Value=Double(semimajor_axis)
            END
    ENDCASE

    self.mapCoord -> SetProperty, $
        SEMIMAJOR_AXIS=semimajor_axis, $
        SEMIMINOR_AXIS=semiminor_axis, $
        DATUM=datum
    self.drawID -> Draw
    self.currentDatum = datum
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateMapProjection
;
; PURPOSE:
;
;       This method updates map projection information from information on the GUI.
;       It is called, typically, whenever something changes on the GUI.
;
; SYNTAX:
;
;     theObject -> UpdateMapProjection
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     TRUESCALE:     If this keyword is set, the mapCoord object should use the TRUE_SCALE_LATITUDE keyword.
;-
;*****************************************************************************************************
PRO GPD_Viewer::UpdateMapProjection

   @cat_pro_error_handler

    ; Get the currently selected map projection.
    self.projectionID -> GetProperty, SELECTION=thisProjection 
    self.map_ref_latID -> GetProperty, Value=mapOriginLat
    self.map_ref_lonID -> GetProperty, Value=mapOriginLon
    
    ; Get current map settings on the display.
    self.map_trueScaleID -> GetProperty, Value=trueScale
    self.semiminor_axisID -> GetProperty, Value=semiminor_axis
    self.semimajor_axisID -> GetProperty, Value=semimajor_axis
    
    ; There is weirdness with Polar Stereo projections.
    IF StrPos(thisProjection, 'Polar Stereo') NE -1 THEN mapOriginLat = trueScale
    
    ; Some map projections will need an update of their trueScale latitude
    ; and other properties.
    CASE 1 OF
       StrPos(thisProjection, 'Mercator') NE -1: BEGIN
            map_proj_keywords = {NULL:1, TRUE_SCALE_LATITUDE:truescale}
            END
       StrPos(thisProjection, 'Equirectangular') NE -1: BEGIN
            map_proj_keywords = {NULL:1, TRUE_SCALE_LATITUDE:truescale}
            END
       StrPos(thisProjection, 'Cylindrical Equal-Area') NE -1: BEGIN
            map_proj_keywords = {NULL:1, STANDARD_PAR1:60, STANDARD_PAR2:-59.5}
            END
       ELSE: map_proj_keywords = {NULL:1} 
     ENDCASE
            
     ; Update the map projection.
     self.mapCoord -> SetProperty, $
            SEMIMAJOR_AXIS=semimajor_axis, $
            SEMIMINOR_AXIS=semiminor_axis, $
            CENTER_LATITUDE=mapOriginLat, $
            CENTER_LONGITUDE=mapOriginLon, $
            MAP_PROJ_KEYWORDS=map_proj_keywords
    
    ; Update the grid lines.
    self -> UpdateGridLines

    ; Update the location of the grid on the display.
    self -> UpdateGridLocation

    ; Draw everything.
    self.drawID -> Draw
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::UpdateGridLocation
;
; PURPOSE:
;
;       This method updates the grid box on this display from information on the GUI.
;       In particular, it requires the grid column and row that will be associated with 
;       the map origin.
;
; SYNTAX:
;
;     theObject -> UpdateGridLocation
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
PRO GPD_Viewer::UpdateGridLocation

    @cat_pro_error_handler

    ; Get current map settings.
    self.metersPerGridCellID -> GetProperty, Value=metersPerGridCell
    
    ; Get the current latitude and longitude and convert them to meters.
    self.grid_ref_latID -> GetProperty, UValue=lat
    self.grid_ref_lonID -> GetProperty, UValue=lon
    xy = Map_Proj_Forward(lon, lat, MAP_STRUCTURE=self.mapCoord->GetMapStructure())            
    
    ; Get number of columns and rows.
    self.gridRowsID -> GetProperty, Value=grid_rows
    self.gridColsID -> GetProperty, Value=grid_cols

    ; Find the XY limits of the given map projection. 
    ulx = xy[0] - ((grid_cols / 2.0D) * metersPerGridCell)
    uly = xy[1] + ((grid_rows / 2.0D) * metersPerGridCell)
    lrx = ulx + (grid_cols * metersPerGridCell) 
    lry = uly - (grid_rows * metersPerGridCell)
    
    ; Calculate the box latitude and longitudes. LL, UL, UR, LR in the reference 
    ; system as you look at the grid box on the display.
    uv = DblArr(2,5)
    uv[0,*] = [ulx, ulx, lrx, lrx, ulx]
    uv[1,*] = [lry, uly, uly, lry, lry]
    ll = Map_Proj_Inverse( [ulx, ulx, lrx, lrx, ulx], $
                           [lry, uly, uly, lry, lry], $
                           MAP_STRUCTURE=self.mapCoord -> GetMapStructure())
    self.mapBox -> SetProperty, LONS=ll[0,*], LATS=ll[1,*]
    
    ; Conversion to floats here prevents overly precise numbers
    ; from being different from what is shown in the UL grid corners.
    self -> UpdateLatLonLocations, Float(ll), Float(uv)
    
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
;*****************************************************************************************************
PRO GPD_Viewer::Write_GPD_File, event

    @cat_pro_error_handler
    
    ; What filename should we use?
    filename = Dialog_Pickfile(TITLE='Output GPD Filename...', /WRITE, $
        FILE=self->ConstructFilename('GPD'))
    IF filename EQ "" THEN RETURN
    
    ; Get the information from the display.
    self.projectionID -> GetProperty, SELECTION=selected_projection
    self.datumID -> GetProperty, SELECTION=selected_datum
    
    ; Get the radii.
    self.semimajor_axisID -> GetProperty, Value=map_equatorial_radius
    map_equatorial_radius =String(map_equatorial_radius, Format='(D0)')
    self.semiminor_axisID -> GetProperty, Value=map_polar_radius
    map_polar_radius = String(map_polar_radius, Format='(D0)')

    ; Get the projection.
    CASE selected_projection OF
        'Azimuthal Equal-Area (North Pole)': map_projection = 'Azimuthal Equal-Area'
        'Azimuthal Equal-Area (South Pole)': map_projection = 'Azimuthal Equal-Area'         
        'Polar Stereographic (North Pole)': map_projection = 'Polar Stereographic'         
        'Polar Stereographic (South Pole)': map_projection = 'Polar Stereographic'        
        'Orthographic': map_projection = 'Orthographic'   
        'Cylindrical Equirectangular (Global)': map_projection = 'Cylindrical Equidistant'               
        'Cylindrical Equal-Area (Global)': map_projection = 'Cylindrical Equal-Area'               
        'Mercator (Global)': map_projection = 'Mercator'  
     ENDCASE
     
     ; Add an indication that this uses an ellipoidal sphere, if necessary.
     IF StrPos(map_projection, 'Equal-Area') NE -1 OR $
        StrPos(map_projection, 'Stereographic') NE -1 THEN BEGIN
        IF Long(map_equatorial_radius) NE Long(map_polar_radius) THEN BEGIN
            map_projection = map_projection + ' (ellipsoid)'
        ENDIF
     ENDIF
     
    
    ; Gather needed information from the GUI.
    self.metersPerGridCellID -> GetProperty, Value=mapunits ; Don't change.
    mapScale = 1.0 ; Don't change.
    self.gridColsID -> GetProperty, Value=xgridSize
    self.gridRowsID -> GetProperty, Value=ygridSize
    self.map_trueScaleID -> GetProperty, Value=truescale
    self.commentID -> GetProperty, Value=theComment
    self.map_ref_latID -> GetProperty, Value=lat0
    self.map_ref_lonID -> GetProperty, Value=lon0
    self.grid_ref_latID -> GetProperty, UValue=grid_center_lat
    self.grid_ref_lonID -> GetProperty, UValue=grid_center_lon
    self.grid_ref_xID -> GetProperty, Value=grid_ul_x
    self.grid_ref_yID -> GetProperty, Value=grid_ul_y
    
    self.map_truescaleID -> GetProperty, Value=truescale
    
    ; Convert the values to strings.
    map_reference_latitude = String(lat0, Format='(D0)')
    map_reference_longitude = String(lon0, Format='(D0)')
    map_second_reference_latitude = String(truescale, Format='(D0)')
    grid_width = StrTrim(xgridSize, 2)
    grid_height = StrTrim(ygridSize, 2)
    map_scale = String(mapScale, Format='(D0)')
    map_units = String(mapunits, Format='(D0)')
    grid_center_lat = String(grid_center_lat, Format='(D0)')
    grid_center_lon = String(grid_center_lon, Format='(D0)')
    grid_row_at_grid_center = String(ygridSize/2.0 - 0.5, Format='(D0)') ; The center of the grid. Don't change.
    grid_col_at_grid_center = String(xgridSize/2.0 - 0.5, Format='(D0)') ; The center of the grid. Don't change.
    grid_ul_x = String(grid_ul_x, Format='(D0)') 
    grid_ul_y = String(grid_ul_y, Format='(D0)') 
    
     ; Write the file.
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
     IF (StrPos(map_projection, 'Polar Stereographic') EQ 0) OR $
        (StrPos(map_projection, 'Cylindrical Equal-Area') EQ 0) THEN $
        PrintF, lun, 'Map Second Reference Latitude:', map_second_reference_latitude, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Equatorial Radius:', map_equatorial_radius, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Polar Radius:', map_polar_radius, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Scale:', map_scale, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Origin Latitude:', grid_center_lat, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Map Origin Longitude:', grid_center_lon, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Units per Cell:', map_units, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Origin Column:', grid_col_at_grid_center, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Map Origin Row:', grid_row_at_grid_center, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Width:', grid_width, FORMAT='(A-30,x,A-45)'
     PrintF, lun, 'Grid Height:', grid_height, FORMAT='(A-30,x,A-45)'
     PrintF, lun, ''
     PrintF, lun, '; The following lines are added ONLY to allow easy access to GPD'
     PrintF, lun, '; information by the GPD_Viewer when reading GPD files. Do not remove'
     PrintF, lun, '; them if you wish to read this GPD file in the GPD_Viewer.'
     PrintF, lun, ''
     PrintF, lun, '; GPD Projection:', selected_projection, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Map Datum:', selected_datum, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Grid Upper-Left X:', grid_ul_x, FORMAT='(A-30,x,A-45)'
     PrintF, lun, '; Grid Upper-Left Y:', grid_ul_y, FORMAT='(A-30,x,A-45)'
     Free_Lun, lun
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::WRITE_LATLON_FILES
;
; PURPOSE:
;
;       This method will write either a latitude or longitude file for the grid
;       represented on the GUI. The files will consist of double-precision array of 
;       latitudes (or longitudes) for the center of each grid cell. The output
;       file will be a binary file of size gridColums-by-gridRows.
;
; SYNTAX:
;
;     theObject -> Write_LatLon_Files
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     EXPORT:   If set, the latitude and longitude arrays are exported to the
;               IDL main level.
;-
;*****************************************************************************************************
PRO GPD_Viewer::Write_LatLon_Files, EXPORT=export

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF N_Elements(latlun) NE 0 THEN Free_Lun, latlun
        IF N_Elements(lonlun) NE 0 THEN Free_Lun, lonlun
        void = Error_Message()
        RETURN
     ENDIF
     
     ; Get a root name for the file.
     filename = self -> ConstructFileName()
     
     ; Get number of columns and rows.
     self.gridcolsID -> GetProperty, Value=cols
     self.gridrowsID -> GetProperty, Value=rows
     
     ; Get the grid origin (lower-left). Put them into UV coordinates.
     self.lon_ll -> GetProperty, UValue=lon_origin
     self.lat_ll -> GetProperty, UValue=lat_origin
     mapStruct = self.mapCoord -> GetMapStructure()
     xy = Map_Proj_Forward(lon_origin, lat_origin, MAP_STRUCTURE=mapStruct)
     map_origin_x = xy[0]
     map_origin_y = xy[1]
 
     ; Collect data to calculate meters per grid cell.
     self.metersPerGridCellID -> GetProperty, Value=metersPerGridCell
     
     ; Construct the vectors. We want to create the arrays at the centers of
     ; the grid cells, not at the edges. Move the origin half a grid cell
     ; toward the center of the grid.
     yvector = (Dindgen(rows) * metersPerGridCell) + ( map_origin_y + (0.5 * metersPerGridCell) )
     xvector = (Dindgen(cols) * metersPerGridCell) + ( map_origin_x + (0.5 * metersPerGridCell) )
     
     ; Reverse the Y vector, since the 0th column and the 0th row should physically
     ; be upper-left corner of the grid. Thus, when I write the file I want the
     ; first Y value to be the largest Y value. 
     yvector = Reverse(yvector)
     
     IF Keyword_Set(export) THEN BEGIN

        ; Get the map structure.
        mapStruct = self.mapCoord -> GetMapStructure()
         
        ; If you have enough memory, you can do it all at once.
        ll = Map_Proj_Inverse(Rebin(xvector, cols, rows), Rebin(Reform(yvector, 1, rows), cols, rows), MAP_STRUCTURE=mapStruct)
        
        varName = TextBox(CANCEL=cancelled, LABEL='Variable Name: ', $
                TITLE='Export Latitude Array', VALUE='lats', GROUP_LEADER=self->GetID())
        IF cancelled THEN RETURN

        Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
        SaveToMain, Float(Reform(ll[1,*], cols, rows)), varName
        
        varName = TextBox(CANCEL=cancelled, LABEL='Variable Name: ', $
                TITLE='Export Longitude Array', VALUE='lons', GROUP_LEADER=self->GetID())
        IF cancelled THEN RETURN

        Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
        SaveToMain, Float(Reform(ll[0,*], cols, rows)), varName
        
     ENDIF ELSE BEGIN
     
         ; Write the file. Give the user the opportunity to change the name of the file.
         filename = Dialog_Pickfile(FILE=filename, /WRITE, TITLE='Select file root name...')
         IF filename EQ "" THEN RETURN
         
         ; Add "lat" and "lon" extensions to whatever name you got.
         rootname = cgRootName(filename, DIRECTORY=theDirectory)
         latFile = Filepath(Root_Dir=theDirectory, rootname + '.lat')
         lonFile = Filepath(Root_Dir=theDirectory, rootname + '.lon')
         
         ; Open the files for writing.
         OpenW, latlun, latFile, /GET_LUN
         OpenW, lonlun, lonFile, /GET_LUN
         Print, 'Latitude File:  ', latFile
         Print, 'Longitude File: ', lonFile
         
         ; Get the map structure.
         mapStruct = self.mapCoord -> GetMapStructure()
         
         ; If you have enough memory, you can do it all at once.
    ;     ll = Map_Proj_Inverse(Rebin(xvector, cols, rows), Rebin(Reform(yvector, 1, rows), cols, rows), MAP_STRUCTURE=mapStruct)
    ;     WriteU, latlun, Float(Reform(ll[1,*], cols, rows))
    ;     WriteU, lonlun, Float(Reform(ll[0,*], cols, rows))
    
         ; Some arrays are too big for MAP_PROJ_INVERSE. Do it row by row to save memory.
         FOR j=0,rows-1 DO BEGIN
            ll = Map_Proj_Inverse(xvector, Replicate(yvector[j], cols), MAP_STRUCTURE=mapStruct)
            WriteU, latlun, Reform(ll[1,*])
            WriteU, lonlun, Reform(ll[0,*])
         ENDFOR
         Free_Lun, latlun
         Free_Lun, lonlun
     
     ENDELSE
END


;*****************************************************************************************************
;+
; NAME:
;       GPD_VIEWER::WRITE_XYGRID_FILES
;
; PURPOSE:
;
;       This method will write either a xgrid or ygrid file for the grid
;       represented on the GUI. The files will consist of a double-precision vector of 
;       x values or y values for the center of each grid cell in meters from the
;       map projection origin. The x vector will be of size gridColums, and the 
;       y vector will be of size gridRows.
;
; SYNTAX:
;
;     theObject -> Write_XYGrid_Files
;
; ARGUMENTS:
;
;     None.
;
; KEYWORDS:
;
;     EXPORT:   If this keyword is set, the XGRID and YGRID are written to the main IDL level
;               and not into a file.
;-
;*****************************************************************************************************
PRO GPD_Viewer::Write_XYGrid_Files, EXPORT=export

    ; Error handling.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        IF N_Elements(latlun) NE 0 THEN Free_Lun, latlun
        IF N_Elements(lonlun) NE 0 THEN Free_Lun, lonlun
        void = Error_Message()
        RETURN
     ENDIF
     
     ; Get a root name for the file.
     filename = self -> ConstructFileName()
     
     ; Get number of columns and rows.
     self.gridcolsID -> GetProperty, Value=cols
     self.gridrowsID -> GetProperty, Value=rows
     
     ; Get the grid origin (lower-left). 
     self.x_ll -> GetProperty, Value=map_origin_x
     self.y_ll -> GetProperty, Value=map_origin_y
 
     ; Collect data to calculate meters per grid cell.
     self.metersPerGridCellID -> GetProperty, Value=metersPerGridCell
     
     ; Construct the vectors. We want to create the arrays at the centers of
     ; the grid cells, not at the edges. Move the origin half a grid cell
     ; toward the center of the grid.
     xvector = (Dindgen(cols) * metersPerGridCell) + ( map_origin_x + (0.5 * metersPerGridCell) )
     yvector = (Dindgen(rows) * metersPerGridCell) + ( map_origin_y + (0.5 * metersPerGridCell) )
     
     ; Reverse the Y vector, since the 0th column and the 0th row should physically
     ; be upper-left corner of the grid. Thus, when I write the file I want the
     ; first Y value to be the largest Y value. To use the longitude/latitude arrays
     ; you will have to reverse the Y direction in the normal IDL way.
     yvector = Reverse(yvector)

     ; Are you exporting or writing the files.?
     IF Keyword_Set(export) THEN BEGIN
     
        varName = TextBox(CANCEL=cancelled, LABEL='Variable Name: ', $
                TITLE='Export XGrid Vector', VALUE='xgrid', GROUP_LEADER=self->GetID())
        IF cancelled THEN RETURN

        Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
        SaveToMain, xvector, varName
        
        varName = TextBox(CANCEL=cancelled, LABEL='Variable Name: ', $
                TITLE='Export YGrid Vector', VALUE='ygrid', GROUP_LEADER=self->GetID())
        IF cancelled THEN RETURN

        Print, 'A variable named "' + varName + '" has been created at the main IDL level.'
        SaveToMain, yvector, varName
        
     ENDIF ELSE BEGIN
     
         ; Write the file. Give the user the opportunity to change the name of the file.
         filename = Dialog_Pickfile(FILE=filename, /WRITE, TITLE='Select file root name...')
         IF filename EQ "" THEN RETURN
         
         ; Add "xgrid" and "ygrid" extensions to whatever name you got.
         rootname = cgRootName(filename, DIRECTORY=theDirectory)
         xFile = Filepath(Root_Dir=theDirectory, rootname + '.xgrid')
         yFile = Filepath(Root_Dir=theDirectory, rootname + '.ygrid')
         
         ; Open the files for writing.
         OpenW, xlun, xFile, /GET_LUN
         OpenW, ylun, yFile, /GET_LUN
         Print, 'XGrid File: ', yFile
         Print, 'YGrid File: ', xFile
         
         ; Write the files.
         WriteU, xlun, xvector
         WriteU, ylun, yvector
         
         ; Clean up.
         Free_Lun, xlun
         Free_Lun, ylun
         
     ENDELSE
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
   Obj_Destroy, self.mapCoord
   Obj_Destroy, self.mapGrid
   Obj_Destroy, self.mapBox
   Obj_Destroy, self.mapBase
 
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
             projectionID: Obj_New(), $          ; Widget containing map projections.
             datumID: Obj_New(), $               ; Widget containing map datums.
             map_ref_latID: Obj_New(), $         ; Widget containing lat0.
             map_ref_lonID: Obj_New(), $         ; Widget containing lon0.
             metersPerGridCellID: Obj_New(), $   ; Widget containing meters per grid cell value.
             gridColsID: Obj_New(), $            ; Widget containing number of cols in the grid.
             gridRowsID: Obj_New(), $            ; Widget containing number of rows in the grid.
             lon_ur: Obj_New(), $                ; Widgets for specifying lat/lon grid corners.
             lon_ul: Obj_New(), $
             lon_lr: Obj_New(), $
             lon_ll: Obj_New(), $
             lat_ur: Obj_New(), $
             lat_ul: Obj_New(), $
             lat_lr: Obj_New(), $
             lat_ll: Obj_New(), $
             x_ur: Obj_New(), $                ; Widgets for specifying lat/lon grid corners.
             x_ul: Obj_New(), $
             x_lr: Obj_New(), $
             x_ll: Obj_New(), $
             y_ur: Obj_New(), $                ; Widgets for specifying lat/lon grid corners.
             y_ul: Obj_New(), $
             y_lr: Obj_New(), $
             y_ll: Obj_New(), $
             grid_ref_latID: Obj_New(), $        ; Widget for latitude of grid center.
             grid_ref_lonID: Obj_New(), $        ; Widget for longitude of grid center.
             grid_ref_yID: Obj_New(), $          ; Widget for UL Y grid corner.
             grid_ref_xID: Obj_New(), $          ; Widget for UL Y grid corner.
             commentID: Obj_New(), $             ; Widget containing file comments.
             map_truescaleID: Obj_New(), $       ; Widget containing true-scale value.
             drawID: Obj_New(), $                ; The draw widget for the map.
             map_projections: Ptr_New(), $       ; A list of map projections
             map_datums: Ptr_New(), $            ; A list of map datums
             mapCoord: Obj_New(), $              ; The mapCoord object specifying map cooridnates.
             mapGrid: Obj_New(), $               ; The mapGrid object, maintaining map grids.
             mapBox: Obj_New(), $                ; The PlotS object which draws grid box on map.
             mapBase: Obj_New(), $               ; The MapOutline object that draws continents on map.
             semiminor_axisID: Obj_New(), $      ; Widget containing semiminor axis value.
             semimajor_axisID: Obj_New(), $      ; Widget containing semimajor axis value.
             currentDatum: "", $                 ; The current datum. Used to restore illegal selections.
             limit: FltArr(4), $                 ; The current map limit. Required for zooming.
             zoomin: Obj_New(), $                ; The widget that allows zoom in.
             zoomout: Obj_New(), $               ; The widget that allows zoom out.
             INHERITS TopLevelBase }             ; This program inherits a TopLevelBase object.

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
