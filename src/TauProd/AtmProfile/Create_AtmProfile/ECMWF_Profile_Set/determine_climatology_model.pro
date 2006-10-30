pro determine_climatology_model, mon, lat, clim

  n = n_elements( mon )
  clim = lonarr( n )

  TROPICAL           = 1
  MIDLATITUDE_SUMMER = 2
  MIDLATITUDE_WINTER = 3
  SUBARCTIC_SUMMER   = 4
  SUBARCTIC_WINTER   = 5
  US_STD_ATM         = 6

  JANUARY   =  1
  FEBRUARY  =  2
  MARCH     =  3
  APRIL     =  4
  MAY       =  5
  JUNE      =  6
  JULY      =  7
  AUGUST    =  8
  SEPTEMBER =  9
  OCTOBER   = 10
  NOVEMBER  = 11
  DECEMBER  = 12


  ; --------
  ; Tropical
  ; --------

  loc = where( abs(lat) lt 30.0, count )
  if ( count gt 0 ) then clim[loc ] = TROPICAL


  ; ---------------------------------
  ; Midlatitude - NORTHERN HEMISPHERE
  ; ---------------------------------

  ; -- Northern hemisphere default to US STD ATM for spring/autumn
  loc = where( lat ge 30.0 and lat le 60.0, count )
  if ( count gt 0 ) then clim[loc ] = US_STD_ATM

  ; -- Northern hemisphere SUMMER
  loc = where( lat ge 30.0 and lat le 60.0 and mon ge JUNE and mon le AUGUST, count )
  if ( count gt 0 ) then clim[loc ] = MIDLATITUDE_SUMMER

  ; -- Northern hemisphere WINTER
  loc = where( lat ge 30.0 and lat le 60.0 and ( mon ge DECEMBER or mon le FEBRUARY ), count )
  if ( count gt 0 ) then clim[loc ] = MIDLATITUDE_WINTER


  ; ---------------------------------
  ; Midlatitude - SOUTHERN HEMISPHERE
  ; ---------------------------------

  ; -- Southern hemisphere default to US STD ATM
  loc = where( lat le -30.0 and lat ge -60.0, count )
  if ( count gt 0 ) then clim[loc ] = US_STD_ATM

  ; -- Southern hemisphere SUMMER
  loc = where( lat le -30.0 and lat ge -60.0 and ( mon ge DECEMBER or mon le FEBRUARY ), count )
  if ( count gt 0 ) then clim[loc ] = MIDLATITUDE_SUMMER

  ; -- Southern hemisphere WINTER
  loc = where( lat le -30.0 and lat ge -60.0 and mon ge JUNE and mon le AUGUST, count )
  if ( count gt 0 ) then clim[loc ] = MIDLATITUDE_WINTER


  ; -------------------------------
  ; Subarctic - NORTHERN HEMISPHERE
  ; -------------------------------

  ; -- Northern hemisphere default to WINTER
  loc = where( lat gt 60.0, count )
  if ( count gt 0 ) then clim[loc ] = SUBARCTIC_WINTER

  ; -- Northern hemisphere SUMMER
  loc = where( lat gt 60.0 and mon ge MAY and mon le OCTOBER, count )
  if ( count gt 0 ) then clim[loc ] = SUBARCTIC_SUMMER


  ; -------------------------------
  ; Subarctic - SOUTHERN HEMISPHERE
  ; -------------------------------

  ; -- Southern hemisphere default to SUMMER
  loc = where( lat lt -60.0, count )
  if ( count gt 0 ) then clim[loc ] = SUBARCTIC_SUMMER

  ; -- Southern hemisphere WINTER
  loc = where( lat lt -60.0 and mon ge MAY and mon le OCTOBER, count )
  if ( count gt 0 ) then clim[loc ] = SUBARCTIC_WINTER


end
