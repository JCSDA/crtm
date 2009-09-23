PRO GTOPO30_Location, $
  Dem

  ; Error handler
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /INFORMATIONAL
    RETURN
  ENDIF
  
  
  ; Specify geographic bounds
  xN = Dem.Hdr.ULXMAP + DOUBLE(Dem.Hdr.NCOLS-1)*Dem.Hdr.XDIM
  x = DINDGEN(Dem.Hdr.NCOLS)/DOUBLE(Dem.Hdr.NCOLS-1)
  x = x*(xN-Dem.Hdr.ULXMAP) + Dem.Hdr.ULXMAP

  y0 = Dem.Hdr.ULYMAP - DOUBLE(Dem.Hdr.NROWS-1)*Dem.Hdr.YDIM
  y = DINDGEN(Dem.Hdr.NROWS)/DOUBLE(Dem.Hdr.NROWS-1)
  y = y*(Dem.Hdr.ULYMAP-y0) + y0

  ; Create array for locations
  Dem.Longitude = PTR_NEW(REBIN(x,Dem.Hdr.ncols,Dem.Hdr.nrows))
  Dem.Latitude  = PTR_NEW(TRANSPOSE(REBIN(y,Dem.Hdr.nrows,Dem.Hdr.ncols)))

   map_set, 15.0,40.0,/cylindrical,scale=100e6
   contour, *dem.elevation, *dem.longitude, *dem.latitude, /overplot,/fill,nlevels=20
   map_grid,/label
map_continents
END

