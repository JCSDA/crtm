;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       create_tape5
;
; PURPOSE:
;       Function to create a TAPE5 file for input to LBLRTM
;
; CATEGORY:
;       LBLRTM
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       result = create_tape5( pressure, $                                       ; Input
;                              temperature, $                                    ; Input
;                              absorber_amount, $                                ; Input
;                              absorber_units, $                                 ; Input
;                              absorber_number, $                                ; Input
;                              surface_altitude, $                               ; Input
;                              begin_wavenumber, $                               ; Input
;                              end_wavenumber, $                                 ; Input
;                              altitude             = altitude, $                ; Input keyword
;                              co2ppmv              = co2ppmv, $                 ; Input keyword
;                              boundary_temperature = boundary_temperature, $    ; Input keyword
;                              climatology_model    = climatology_model, $       ; Input keyword
;                              upwelling            = upwelling, $               ; Input keyword
;                              downwelling          = downwelling, $             ; Input keyword
;                              zenith_angle         = zenith_angle, $            ; Input keyword
;                              observer_height      = observer_height, $         ; Input keyword
;                              boundary_height      = boundary_height, $         ; Input keyword
;                              calculation_levels   = calculation_levels, $      ; Input keyword
;                              use_boundary_levels  = use_boundary_levels, $     ; Input keyword
;                              merge_option         = merge_option, $            ; Input keyword
;                              xsection_option      = xsection_option, $         ; Input keyword
;                              header               = header, $                  ; Input keyword
;                              filename             = filename )                 ; Input keyword
;
; INPUTS:
;       pressure:           Atmospheric pressure profile in units of hPa (mb)
;       temperature:        Atmospheric temperature profile in units of Kelvin.
;       absorber_amount:    Array of absorber amounts to use in the LBLRTM calculation.
;                           The array should have dimensions of L x N where
;                             L == number of atmospheric profile layers and
;                                  must be greater than or equal to 1, and
;                             N == number of absorbers to include in the calculation and
;                                  can be 0 (i.e. a vector is passed).
;       absorber_units:     String vector of flags to indicate the units of the absorber
;                           amounts. The flag values correspond to those used by LBLRTM
;                           and are:
;                             'A':      Volume mixing ratio (ppmv)
;                             'B':      Number density (cm^-3)
;                             'C':      Mass mixing ratio (gm/kg)
;                             'D':      Mass density (gm/m^3)
;                             'E':      Partial pressure (mb)
;                             'F':      Dew point temperature (Td in K))     - H2O only
;                             'G':       "    "        "      (Td in deg. C) - H2O only
;                             'H':      Relative humidity (RH in %)          - H2O only
;                             '1'-'6':  Default to specified model atmosphere
;       absorber_number:    Molecule number of the absorber amounts. These values correspond
;                           to the HITRAN/LBLRTM molecular species numbers:
;                             1: H2O       9: SO2      17: HI       25: H2O2
;                             2: CO2      10: NO2      18: ClO      26: C2H2
;                             3: O3       11: NH3      19: OCS      27: C2H6
;                             4: N2O      12: HNO3     20: H2CO     28: PH3
;                             5: CO       13: OH       21: HOCl     29: COF2
;                             6: CH4      14: HF       22: N2       30: SF6
;                             7: O2       15: HCl      23: HCN      31: H2S
;                             8: NO       16: HBr      24: CH3Cl    32: HCOOH
;       surface_altitude:   Surface altitude in km.
;       begin_wavenumber:   Wavenumber in units of cm^-1 at which to start the calculation
;       end_wavenumber:     Wavenumber in units of cm^-1 at which to end the calculation
;
; KEYWORD PARAMETERS:
;       altitude:              Altitudes of input profile data points in km. If this keyword
;                                is set, it specifies that the LBLRTM layer calculation
;                                boundaries are altitudes. If not set, the layer calculation
;                                boundaries are assumed to be in units of pressure.
;       co2ppmv:               Set this keyword to the carbon dioxide concentration in units
;                                of ppmv. This value is used at EVERY level in the atmosphere
;                                (assumes well mixed gas).
;                              If not specified, the default value used is 360ppmv.
;       boundary_temperature:  Set this keyword to the boundary temperature in units of Kelvin.
;                              If not specified the defaults are:
;                                For uplooking calculations   = 3.9K
;                                For downlooking calculations = surface air temperature
;       climatology_model:     Set this keyword to a value for the climatological model to
;                                use for those molecules and layers where data is not
;                                available/provided. Valid values are:
;                                  1 = Tropical
;                                  2 = Midlatitude summer
;                                  3 = Midlatitude winter
;                                  4 = Subarctic summer
;                                  5 = Subarctic winter
;                                  6 = US Standard Atmosphere
;       upwelling:             Set this keyword to perform an upwelling/downlooking calculation.
;                              If both the upwelling AND downwelling keywords are set, upwelling
;                                takes precedence. If NEITHER the upwelling nor downwelling keyword
;                                is set, upwelling is assumed.
;       downwelling:           Set this keyword to perform an downwelling/uplooking calculation.
;                              If both the upwelling AND downwelling keywords are set, upwelling
;                                takes precedence. If NEITHER the upwelling nor downwelling keyword
;                                is set, upwelling is assumed.
;       zenith_angle:          Set this keyword to the zenith angle in units of degrees to be used.
;                                If not set a default value of 0.0degrees is used.
;                              The diagrams below show how the zenith angle, Z, is interpreted for the
;                                up- and downwelling cases:
;
;                                upwelling/downlooking          downwelling/uplooking
;                                        |                           zenith   view
;                                      -OAO-                            |      /
;                                        |\                             |     /
;                                        |Z\                            |    /
;                                        |  \                           |   /
;                                        |   \                          |  /
;                                        |    \                         |Z/
;                                        |     \                        |/
;                                        |      \                     -OVO-
;                                     --------------                --------------
;                                        ^       ^
;                                      nadir    view
;
;       observer_height:       Set this keyword to the height, in hPa, of the observer. If the
;                                altitude keyword is used, the units are in km.
;       boundary_height:       Set this keyword to the height, in hPa, of the boundary viewed
;                                by the observer. If the altitude keyword is used, the units
;                                are in km.
;       calculation_levels:    Set this keyword to an array of level heights, in hPa, at which
;                                the LBLRTM calculations are to be performed. If the
;                                altitude keyword is used, the units are in km. Note that
;                                the value of calculation_levels, if defined, has precedence
;                                over observer_ and boundary_height.
;       use_boundary_levels:   Set this keyword to use the input profile levels as the
;                                calculation levels. This keyword has no effect if both
;                                the calculation_levels and use_boundary_levels keywords
;                                are set.
;       merge_option:          Set this keyword to the LBLRTM merge option. See the LBLRTM
;                                instructions for valid values.
;       xsection_option:       Set this keyword to include cross section molecule data.
;                                Currently only default concentrations of CCl4, CCl3F, and
;                                CCl2F2 can be used.
;       header:                Set this keyword to a identifier string that is output in the first
;                                line of the TAPE5 file. If not set then the default value, on a UNIX
;                                system is:
;                                  'LBLRTM TAPE5 file created for ' + user_name + ' on ' + date
;                                where user_name and date are obtained from UNIX system calls. On
;                                non-UNIX systems, the header is set to ' '.
;       filename:              Set this keyword to a string or string variable to be used as the 
;                                output TAPE5 filename. If not set the output filename is 'tape5.rdk'.
;
; OUTPUTS:
;       The function returns a value of -1 if any problems are dicovered with the input data. If a 
;         TAPE5 file is successfully created a value of 1 is returned.
;
; CONTAINS:
;       invalid_dp_filter:           Procedure to filter out invalid input pressures where the
;                                      pressure difference between adjacent LEVELS is =0.0 or <0.0.
;       interpolate_profiles:        Procedure to linearly interpolate the input atmospheric profiles
;                                      to a lower number of LEVELS is the number of input LEVELS
;                                      exceeds the maximum allowed by LBLRTM.
;       compute_calculation_LEVELS:  Function to compute the altitudes that LBLRTM will use as its
;                                      calculation LEVELS.
; CALLS:
;       pressure_height:  Function to calculate geopotential altitudes.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       Certain keywords may be redefined as floating point values if they were not passed in
;         as such.
;
; RESTRICTIONS:
;       None know yet.
;
; PROCEDURE:
;       - The pressure data is checked by ensuring that the difference between adjacent LEVEL
;         pressures are consistent, e.g. monotonically decreasing from the ground up. If an
;         invalid LEVEL is found, it is removed from the atmospheric profile.
;       - The input temperature, water vapor, and ozone (if supplied) data is checked for
;         negative values.
;       - The profile data is output in LBLRTM TAPE5 format
;       - The X-section data for CCl4, F11, and F12 is output. At the moment this option is
;         hardwired in but it will be user selectable in future versions.
;       
; EXAMPLE:
;       Given pressure, temperature, profile data, and level altitudes:
;
;         IDL> HELP, pressure, temperature, profile_data, altitude
;         PRESSURE        FLOAT     = Array[43]
;         TEMPERATURE     FLOAT     = Array[43]
;         PROFILE_DATA    FLOAT     = Array[43, 8]
;         ALTITUDE        FLOAT     = Array[43]
;
;       and defining the profile data units and molecular species ids:
;
;         IDL> PRINT, profile_data_units
;         A A A A A A A A
;         IDL> PRINT, profile_data_molecule_index
;                1       2       3       4       5       6       7      22
;
;       with a spectral range of:
;
;         IDL> HELP, begin_wavenumber, end_wavenumber
;         BEGIN_WAVENUMBER
;                         FLOAT     =       658.500
;         END_WAVENUMBER  FLOAT     =       700.500
;
;       A TAPE5 file can be created by typing:
;
;         IDL> PRINT, create_tape5( pressure, $
;         IDL>                      temperature, $
;         IDL>                      profile_data, $
;         IDL>                      profile_data_units, $
;         IDL>                      profile_data_molecule_index, $
;         IDL>                      altitude, $
;         IDL>                      altitude[ n_levels - 1 ], $
;         IDL>                      begin_wavenumber, $
;         IDL>                      end_wavenumber, $
;         IDL>                      climatology_model = 6, $
;         IDL>                      boundary_temperature = temperature[ n_levels - 1 ], $
;         IDL>                      /upwelling, $
;         IDL>                      merge_option = 3, $
;         IDL>                      zenith_angle = 0.0, $
;         IDL>                      /use_boundary_levels, $
;         IDL>                      header = 'Test TAPE5 creation', $
;         IDL>                      filename = 'tape5.test' )
;                1
;
;       of which the beginning of the file is shown below. Note that all of the
;       molecular profiles that were passed have been used, even if the molecule
;       ids are not adjacent.
;
;         IDL> $head -20 tape5.test
;         $ Test TAPE5 creation
;          HI=1 F4=1 CN=1 AE=0 EM=1 SC=0 FI=0 PL=0 TS=0 AM=1 M=03 LA=0 OD=0 XS=0    0    0
;            658.500   700.500
;            299.710     1.000
;             0    2   43    1    1   22    1 0  0     0.000     0.000   679.500   360.000
;             66.300     0.000   180.000
;              0.000     0.070     0.240     0.500     0.820     1.200     1.630     2.100
;              2.600     3.140     3.700     4.290     4.900     5.540     6.210     6.910
;              7.630     8.380     9.160     9.970    10.810    11.690    12.580    13.520
;             14.480    15.450    16.500    17.550    18.710    19.980    21.370    22.910
;             24.620    26.530    28.680    31.130    33.950    37.250    41.190    45.900
;             51.700    58.490    66.300
;            43
;              0.000  1013.250   299.710     AA   AAAAAAA66666666666666A
;          2.590e+04 3.600e+02 2.870e-02 3.200e-01 1.500e-01 1.700e+00 2.090e+05
;
;                                                            7.810e+05
;              0.070  1005.430   299.300     AA   AAAAAAA66666666666666A
;          2.540e+04 3.600e+02 2.890e-02 3.200e-01 1.500e-01 1.700e+00 2.090e+05
; 
; MODIFICATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 24-Feb-1999
;                       paul.vandelst@ssec.wisc.edu
;
;
;-

PRO invalid_dp_filter, n_levels, $
                       pressure, $
                       temperature, $
                       absorber_amount, $
                       altitude


; ---------------------------------------------------
; Determine which way the initial pressure are stored
; ---------------------------------------------------

; -- Calculate the average layer pressure differential
  average_dp =  TOTAL( pressure[ 0 : n_levels - 2 ] - pressure[ 1 : n_levels - 1 ] ) / $
                                          FLOAT( n_levels - 1 )

; -- If the average differential is negative, reverse the arrays so the 
; -- pressures decrease as the array index increases, i.e. always make
; -- the arrays go from the ground up.
  IF ( average_dp LT 0.0 ) THEN BEGIN
    MESSAGE, 'Reversing profile arrays so data is from ground up', /INFO
    pressure        = REVERSE( pressure )
    temperature     = REVERSE( temperature )
    absorber_amount = REVERSE( absorber_amount, 1 )
    altitude        = REVERSE( altitude )
  ENDIF


; --------------------------------------------------------------------------
; Loop over levels checking the LAYER dP. If the value is not as expected,
;   toss the offending LEVEL out.
;
;        LEVEL Pressures      LAYER pressure
;                              differentials
;            -------                  
; i=N, ptop  |  N  |                  
;            ------          -----------------
;            | N-1 |         | p[N-1]-p[N]   |   if -ve, p[N] bad
;            -------         -----------------
;            | N-2 |         | p[N-2]-p[N-1] |   if -ve, p[N-1] bad
;            -------         -----------------
;            |  .  |         |       .       |
;            -------         -----------------
;            |  .  |         |       .       |
;            -------         -----------------
;            |  .  |         |       .       |
;            -------         -----------------
;            |  3  |         |   p[3]-p[4]   |   if -ve, p[4] bad
;            -------         -----------------
;            |  2  |         |   p[2]-p[3]   |   if -ve, p[3] bad
;            -------         -----------------
; i=0, psfc  |  1  |         |   p[1]-p[2]   |   if -ve, p[2] bad
;            -------         -----------------
;
; Note that once an LEVEL pressure is found to be bad an thrown out, it 
; is not used to determine if the next LEVEL pressure is valid, the PREVIOUS
; good LEVEL pressure is used.
; --------------------------------------------------------------------------

; -- Set the surface pressure as valid
  index = [ 0 ]

; -- Initialise array index pointers
  i = 0     ; Used to access index array
  j = 1     ; Used to access pressure array
  k = 0     ; Used to access invalid index array

; -- Loop over LEVELS
  WHILE ( j LE ( n_levels - 1 ) ) DO BEGIN

;   -- Calculate current LAYER dp
    dp = pressure[ index[ i ] ] - pressure[ j ]

;   -- Check if dp is valid.
    IF ( dp GT 0.0 ) THEN BEGIN

      index = [ index, j ]    ; Update valid pressure index array
      i = i + 1               ; Increment valid pressure index array pointer

    ENDIF ELSE BEGIN

      IF ( k EQ 0 ) THEN $    ; Update invalid pressure index array
        invalid_index = j $
      ELSE $
        invalid_index = [ invalid_index, j ]

      k = k + 1               ; Increment invalid pressure index array pointer

    ENDELSE

;   -- Increment pressure array index pointer
    j = j + 1

  ENDWHILE


; -------------------------------------------------------------
; O.K. Now we have an index array that references all the valid
; pressures. Use it to cull the crappy data
; -------------------------------------------------------------

; -- Save the old number of pressure levels
  old_n_levels = n_levels
  n_levels     = N_ELEMENTS( index )

; -- Print out some info for the user
  IF ( k GT 0 ) THEN BEGIN
    MESSAGE, 'No. of bad pressure levels discarded : ' + $
             STRING( old_n_levels - n_levels, FORMAT = '( i6 )' ), /INFO
    PRINT, FORMAT = '( 8( 2x, f8.3 ) )', pressure[ invalid_index ]
  ENDIF

; -- Get rid of bad data
  pressure        = pressure[ index ]
  temperature     = temperature[ index ]
  absorber_amount = absorber_amount[ index, * ]
  altitude        = altitude[ index ]

END




          
PRO interpolate_profiles, max_profile_levels, $
                          n_input_levels, $
                          n_model_levels, $
                          n_absorbers, $
                          pressure, $
                          temperature, $
                          absorber_amount


; ----------------------------------------------------------
; Determine the number of input profile levels required such
; the the total number of levels does not exceed the maximum
; ----------------------------------------------------------

  n_required_input_levels = max_profile_levels - n_model_levels


; ----------------------------------------------------------
; Determine the index values of the pressure array to sample
; ----------------------------------------------------------

  index = FLOAT( n_input_levels ) * $
            FINDGEN( n_required_input_levels ) / FLOAT( n_required_input_levels - 1 )


; ----------------------------------------------
; Now interpolate the atmospheric profile arrays
; to the new set of pressure levels
; ----------------------------------------------

; -- Pressure and temperature
  p = INTERPOLATE( pressure, index )
  t = INTERPOL( temperature, pressure, p )

; -- All the absorbers
  q = FLTARR( n_required_input_levels, n_absorbers )
  FOR i = 0, n_absorbers - 1 DO BEGIN
    x         = INTERPOL( absorber_amount[ *, i ], pressure, p )
    q[ *, i ] = TEMPORARY( x )
  ENDFOR


; --------------------------------------------
; Replace the original array data with the new
; --------------------------------------------

  pressure        = TEMPORARY( p )
  temperature     = TEMPORARY( t )
  absorber_amount = TEMPORARY( q )


; ---------------------
; Output some user info
; ---------------------

  MESSAGE, 'Maximum number of input profile levels (' + $
           STRING( max_profile_levels, FORMAT = '( i4 )' ) + ') exceeded. ' + $
           'Interpolated input profiles from ' + $
           STRING( n_input_levels, FORMAT = '( i6 )' ) + ' to ' + $
           STRING( n_required_input_levels, FORMAT = '( i6 )' ) + ' levels', /INFO


END





;-------------------------------------------------------------------------------
;
; This may be a brain dead way of calculating levels but it is a start!
;
; Calculation levels are computed using five different LAYER thicknesses for
;   different altitude regions:
;
;      Altitude span       LAYER thickness
;          (km)                 (km)
;     -------------------------------------
;      surface - 3.0            0.1
;        3.0 -  6.0             0.2
;        6.0 - 15.0             0.5
;       15.0 - 30.0             1.0
;       30.0 - max.             5.0
;-------------------------------------------------------------------------------

FUNCTION compute_calculation_levels, altitude, $
                                     observer_altitude, $
                                     boundary_altitude, $
                                     maximum_altitude_allowed


; ---------------------------------------
; Determine minimum and maximum altitudes
; ---------------------------------------

  minimum_altitude = MIN( [ observer_altitude, boundary_altitude ], MAX = maximum_altitude )

  IF ( maximum_altitude GT maximum_altitude_allowed ) THEN maximum_altitude = maximum_altitude_allowed


; ---------------------------------------------------------
; Set the different LAYER thicknesses, in km, and the LEVEL
; altitudes, in km, at which the LAYER thicknesses increase
; ---------------------------------------------------------

  calculation_LAYER_thickness = [ 0.1, 0.2,  0.5,  1.0,  5.0 ]
  calculation_LAYER_altitude  = [ 3.0, 6.0, 15.0, 30.0, 65.0 ]

  n_thicknesses = N_ELEMENTS( calculation_LAYER_thickness )


; ----------------------------------------------------------
; Determine the thickness at which to start the calculations
; ----------------------------------------------------------

  index = WHERE( calculation_LAYER_altitude - minimum_altitude GT 0.0, count )
  i_begin = index[ 0 ]


; -----------------------------------------
; Loop over the different LAYER thicknesses
; -----------------------------------------

  FOR i = i_begin, n_thicknesses - 1 DO BEGIN


;   -- Initialise altitude at which to start 

    next_altitude = minimum_altitude + calculation_LAYER_thickness[ i ]
    d_altitude    = calculation_LAYER_altitude[ i ] - next_altitude


;   -- Assemble calculation levels for current LAYER thickness

    n_calculation_levels = LONG( ( d_altitude / calculation_LAYER_thickness[ i ] ) ) + 1

    IF ( n_calculation_levels GT 1 ) THEN BEGIN
      current_calculation_levels = FINDGEN( n_calculation_levels ) / FLOAT( n_calculation_levels - 1 )
      current_calculation_levels = ( current_calculation_levels * d_altitude ) + next_altitude
    ENDIF ELSE BEGIN
      current_calculation_levels = calculation_LAYER_altitude[ i ]
    ENDELSE


;   -- Assemble final calculation LEVEL array

    IF ( i EQ i_begin ) THEN BEGIN
      calculation_levels = [ minimum_altitude, current_calculation_levels ]
    ENDIF ELSE BEGIN
      calculation_levels = [ calculation_levels, current_calculation_levels ]
    ENDELSE


;   -- Update minimum altitude

    minimum_altitude = calculation_LAYER_altitude[ i ]

  ENDFOR


; -----------------------------------------------------
; Remove calculation layers greater than that asked for
; -----------------------------------------------------

  index = WHERE( ( maximum_altitude - calculation_levels ) GT 0.0, count ) 
  IF ( count GT 0 ) THEN calculation_levels = [ calculation_levels[ index ], maximum_altitude ]


; ----
; Done
; ----

  RETURN, calculation_levels


END





FUNCTION create_tape5, pressure, $                                       ; Input
                       temperature, $                                    ; Input
                       absorber_amount, $                                ; Input
                       absorber_units, $                                 ; Input
                       absorber_number, $                                ; Input
                       altitude, $                                       ; Input
                       surface_altitude, $                               ; Input
                       begin_wavenumber, $                               ; Input
                       end_wavenumber, $                                 ; Input
                       co2ppmv              = co2ppmv, $                 ; Input keyword
                       boundary_temperature = boundary_temperature, $    ; Input keyword
                       climatology_model    = climatology_model, $       ; Input keyword
                       upwelling            = upwelling, $               ; Input keyword
                       downwelling          = downwelling, $             ; Input keyword
                       zenith_angle         = zenith_angle, $            ; Input keyword
                       observer_height      = observer_height, $         ; Input keyword
                       boundary_height      = boundary_height, $         ; Input keyword
                       calculation_levels   = calculation_levels, $      ; Input keyword
                       use_boundary_levels  = use_boundary_levels, $     ; Input keyword
                       merge_option         = merge_option, $            ; Input keyword
                       xsection_option      = xsection_option, $         ; Input keyword
                       header               = header, $                  ; Input keyword
                       filename             = filename                   ; Input keyword
                  

;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id$'



;------------------------------------------------------------------------------
;                   -- Set floating point precision (single )--
;------------------------------------------------------------------------------

  tolerance = ( MACHAR() ).EPS



;------------------------------------------------------------------------------
;                    -- Initialise molecular species array --
;------------------------------------------------------------------------------

  absorber_names = [ 'H2O',  'CO2',  'O3',   'N2O', 'CO',   'CH4', $
                     'O2',   'NO',   'SO2',  'NO2', 'NH3',  'HNO3', $
                     'OH',   'HF',   'HCl',  'HBR', 'HI',   'ClO', $
                     'OCS',  'H2CO', 'HOCl', 'N2',  'HCN',  'CH3Cl', $
                     'H2O2', 'C2H2', 'C2H6', 'PH3', 'COF2', 'SF6', $
                     'H2S',  'HCOOH' ]
  max_n_absorbers = N_ELEMENTS( absorber_names )



;------------------------------------------------------------------------------
;                          -- Check argument inputs --
;------------------------------------------------------------------------------

; -------------------
; Number of arguments
; -------------------

  n_arguments = 9
  IF ( N_PARAMS() NE n_arguments ) THEN BEGIN
    MESSAGE, 'Invalid number of input arguments', /INFO
    RETURN, -1
  ENDIF


; --------------------------------------
; Check for base input argument validity
; --------------------------------------

; -- Pressure
  n_input_pressure_levels = N_ELEMENTS( pressure )
  IF ( n_input_pressure_levels EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input level pressures.', /INFO
    RETURN, -1
  ENDIF

; -- Temperature
  n_input_temperature_levels = N_ELEMENTS( temperature )
  IF ( n_input_temperature_levels EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input level temperatures.', /INFO
    RETURN, -1
  ENDIF

; -- Absorber amount
  absorber_amount_info = SIZE( absorber_amount, /STRUCT )
  IF ( absorber_amount_info.N_ELEMENTS EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input level absorber amounts.', /INFO
    RETURN, -1
  ENDIF

; -- Absorber units
  n_absorber_units = N_ELEMENTS( absorber_units )
  IF ( n_absorber_units EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input absorber units vector.', /INFO
    RETURN, -1
  ENDIF

; -- Absorber number
  n_absorber_number = N_ELEMENTS( absorber_number )
  IF ( n_absorber_number EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input absorber number vector.', /INFO
    RETURN, -1
  ENDIF

; -- Altitude
  n_input_altitude_levels = N_ELEMENTS( altitude )
  IF ( n_input_altitude_levels EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify input level altitudes.', /INFO
    RETURN, -1
  ENDIF

; -- Surface altitude
  IF ( N_ELEMENTS( surface_altitude ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify surface altitude.', /INFO
    RETURN, -1
  ENDIF

; -- Wavenumber limits
  IF ( N_ELEMENTS( begin_wavenumber ) EQ 0 OR $
       N_ELEMENTS( end_wavenumber ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Must specify begin and end wavenumbers.', /INFO
    RETURN, -1
  ENDIF


; -------------------------------------
; Check for array dimension consistency
; -------------------------------------

; -- Input levels
  IF ( n_input_pressure_levels  NE n_input_temperature_levels OR $
       n_input_pressure_levels NE absorber_amount_info.DIMENSIONS[ 0 ] OR $
       n_input_pressure_levels  NE n_input_altitude_levels ) THEN BEGIN
    MESSAGE, 'Number of pressure levels inconsistent.', /INFO
    RETURN, -1
  ENDIF
  n_input_levels = n_input_pressure_levels

; -- Number of absorbers
  n_absorbers = absorber_amount_info.DIMENSIONS[ 1 ]

  IF ( n_absorbers LE 1 ) THEN n_absorbers = 1

  IF ( n_absorbers NE n_absorber_units OR $
       n_absorbers NE n_absorber_number ) THEN BEGIN
    MESSAGE, 'Number of absorbers inconsistent.', /INFO
    RETURN, -1
  ENDIF


; -------------------------------------------------
; Check for invalid pressure and temperature values
; -------------------------------------------------

; -- Pressure
  index = WHERE( pressure LT tolerance, count )
  IF ( count GT 0 ) THEN BEGIN
    MESSAGE, 'Found ' + STRING( count, FORMAT = '( i5 )' ) + $
             ' negative and/or zero value pressures (Input units must be hPa)', /INFO
    RETURN, -1
  ENDIF

; -- Temperature
  index = WHERE( temperature LT tolerance, count )
  IF ( count GT 0 ) THEN BEGIN
    MESSAGE, 'Found ' + STRING( count, FORMAT = '( i5 )' ) + $
             ' negative and/or zero value temperatures (Input units must be Kelvin)', /INFO
    RETURN, -1
  ENDIF


; ---------------------------------
; Check for invalid absorber values
; ---------------------------------

; -- Number of absorbers
  IF ( n_absorbers GT max_n_absorbers ) THEN BEGIN
    MESSAGE, 'Too many absorbers specified. Maximum # is ' + $
             STRING( max_n_absorbers, FORMAT = '( i2 )' ), /INFO
    RETURN, -1
  ENDIF

; -- Invalid absorber number
  absorber_number = LONG( absorber_number )
  index = WHERE( absorber_number LT 1 OR $
                 absorber_number GT max_n_absorbers, count )
  IF ( count GT 0 ) THEN BEGIN
    MESSAGE, 'Invalid absorber number specified.', /INFO
    RETURN, -1
  ENDIF

; -- Repeated absorber numbers
  index = UNIQ( absorber_number )
  IF ( N_ELEMENTS( index) NE n_absorber_number ) THEN BEGIN
    MESSAGE, 'Non-unique absorber numbers specified.', /INFO
    RETURN, -1
  ENDIF

; -- Water vapour must be one of the absorbers
  h2o_absorber_number = WHERE( absorber_names EQ 'H2O' ) + 1
  h2o_absorber_index  = WHERE( absorber_number EQ h2o_absorber_number, count )
  IF ( count EQ 0 ) THEN BEGIN
    MESSAGE, 'Water vapor MUST be one of the absorbers.', /INFO
    RETURN, -1
  ENDIF

; -- Absorber amounts and units
  absorber_units        = REFORM( STRUPCASE( STRING( absorber_units ) ) )
  valid_absorber_units  = [ BYTE( 'ABCDEFGH' ), BINDGEN( 6 ) + 1B ]

  FOR i = 0, n_absorbers - 1 DO BEGIN

;   -- Check for invalid absorber amounts
    index = WHERE( absorber_amount[ *, i ] LT tolerance, count )
    IF ( count GT 0 ) THEN BEGIN
      MESSAGE, 'Found ' + STRING( count, FORMAT = '( i5 )' ) + $
               ' negative and/or zero ' + $
               absorber_names[ absorber_number[ i ] - 1 ] + $
               'values.', /INFO
      RETURN, -1
    ENDIF

;   -- Absorber units
    index = WHERE( valid_absorber_units EQ ( BYTE( absorber_units[ i ] ) )[ 0 ], $
                   count )
    IF ( count EQ 0 ) THEN BEGIN
      MESSAGE, 'Invalid absorber unit, "' + $
               absorber_units[ i ] + '", specified for ' + $
               absorber_names[ absorber_number[ i ] - 1 ], /INFO
      RETURN, -1
    ENDIF

  ENDFOR


; ---------------------------------
; Check for invalid input altitudes
; ---------------------------------

; -- Altitude
  index = WHERE( altitude LT 0.0, count )
  IF ( count GT 0 ) THEN BEGIN
    MESSAGE, 'Found ' + STRING( count, FORMAT = '( i5 )' ) + $
             ' negative altitudes (Input units must be in km)', /INFO
    RETURN, -1
  ENDIF

; -- Surface altitude
  IF ( surface_altitude LT 0.0 ) THEN BEGIN
    MESSAGE, 'Input surface altitude < 0. Setting to 0.0km', /INFO
    surface_altitude = 0.0
  ENDIF


; ----------------
; Wavenumber check
; ----------------

; -- Calculate bandwidth and set some maximum values
  bandwidth         = end_wavenumber - begin_wavenumber
  max_bandwidth     = 2000.0
  max_wavenumber    = 20000.0


; -- Check for zero or negative bandwidth
  IF ( bandwidth LT tolerance ) THEN BEGIN
    MESSAGE, 'Invalid input wavenumber range. V1 > V2 or V1 = V2', /INFO
    RETURN, -1
  ENDIF


; -- Check for too large a bandwidth
  IF ( bandwidth GT max_bandwidth ) THEN BEGIN
    MESSAGE, 'Invalid input wavenumber range. V2 - V1 > ' + $
             STRING( max_bandwidth, FORMAT = '( f6.1 )' ) + 'cm-1', /INFO
    RETURN, -1
  ENDIF


; -- Check for negative or too large inputs
  IF ( begin_wavenumber LT tolerance OR $
       end_wavenumber LT tolerance ) THEN BEGIN
    MESSAGE, 'Invalid input wavenumbers. V1 and/or V2 < 0.0cm-1', /INFO
    RETURN, -1
  ENDIF

  IF ( begin_wavenumber GT max_wavenumber OR $
       end_wavenumber GT max_wavenumber ) THEN BEGIN
    MESSAGE, 'Invalid input wavenumbers. V1 and/or V2 > ' + $
             STRING( max_wavenumber, FORMAT = '( f7.1 )' ) + 'cm-1', /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;             -- Remove degenerate dimensions from inputs and --
;             -- transfer array data as contents will change  --
;             -- depending on data quality                    --
;------------------------------------------------------------------------------

  p   = REFORM( pressure )
  t   = REFORM( temperature )
  q   = REFORM( absorber_amount )
  alt = REFORM( altitude )



;------------------------------------------------------------------------------
;       -- Check that pressures are in order, ascending or descending --
;------------------------------------------------------------------------------

; --------------------------------------------
; Sort data according to pressure differential
; --------------------------------------------

  invalid_dp_filter, n_input_levels, p, t, q, alt

  
; ----------------------------------------------------------
; Sort data into descending pressure value, i.e. from the 
; ground up. Any errors in pressure should have been trapped
; by INVALID_DP_FILTER as that procedure checks if 
;   DP < OR = to 0.0
; but what the hell, do it one more time.
; ----------------------------------------------------------

; -- Sort data by pressure
  index = REVERSE( SORT( p ) )
  p     = p[ index ]
  t     = t[ index ]
  q     = q[ index, * ]
  alt   = alt[ index ]


; -- Make sure there are no repeated data (not really
; -- useful for FLOAT data but....)
  index = UNIQ( p )
  p     = p[ index ]
  t     = t[ index ]
  q     = q[ index, * ]
  alt   = alt[ index ]


; -- Set the new number of levels
  n_input_levels = N_ELEMENTS( p )



;------------------------------------------------------------------------------
;                          -- Check keyword inputs --
;------------------------------------------------------------------------------

; -----------------------
; Climatology model input
; -----------------------

; -- Set default climatology model
  default_climatology_model = 6

; -- Set to US Standard Atmosphere if not present
  IF ( N_ELEMENTS( climatology_model ) EQ 0 ) THEN $
    climatology_model = default_climatology_model

; -- Check for invalid climatology model number
  IF ( climatology_model LT 1 OR $
       climatology_model GT default_climatology_model ) THEN BEGIN
    MESSAGE, 'Invalid CLIMATOLOGY_MODEL keyword input. Using value of ' + $
             STRING( default_climatology_model, FORMAT = '( i1 )' ), /INFO
    climatology_model = default_climatology_model
  ENDIF


; ----------------------------------
; Carbon dioxide concentration value
; ----------------------------------

; -- Define CO2 absorber number
  co2_absorber_number = WHERE( absorber_names EQ 'CO2' ) + 1

; -- Was CO2 included as an absorber?
  index = WHERE( absorber_number EQ co2_absorber_number, count )
  IF ( count GT 0 ) THEN BEGIN

;   -- Yes. Disable use of CO2PPMV keyword
    use_co2ppmv = 0

  ENDIF ELSE BEGIN

;   -- No. Enable use of CO2PPMV keyword
    use_co2ppmv = 1

;   -- Set default CO2 ppmv concentration
    default_co2ppmv = 360.0

;   -- Was keyword set?
    IF ( N_ELEMENTS( co2ppmv ) GT 0 ) THEN BEGIN

;     -- Yes. Make sure it's real
      co2ppmv = FLOAT( co2ppmv )

;     -- Is supplied value o.k.?
      IF ( co2ppmv LT 0.0 ) THEN BEGIN
        MESSAGE, 'CO2 concentration < 0. Setting to ' + $
                 STRING( default_co2ppmv, FORMAT = '( f5.1 )' ) + 'ppmv', /INFO
        co2ppmv = default_co2ppmv
      ENDIF

    ENDIF ELSE BEGIN

;     -- No. Use default CO2 ppmv value
      co2ppmv = default_co2ppmv

    ENDELSE

  ENDELSE


; --------------------
; Boundary temperature
; --------------------

; -- Reset flag. 0 == do not use supplied value (if any).
  use_boundary_temperature = 0

; -- Check for valid temperature
  IF ( N_ELEMENTS( boundary_temperature ) NE 0 ) THEN BEGIN

    use_boundary_temperature = 1
    boundary_temperature     = FLOAT( boundary_temperature )

    IF ( boundary_temperature LT 0.0 ) THEN BEGIN
      MESSAGE, 'Negative boundary temperature value input. Will use profile extrema.', /INFO
      use_boundary_temperature = 0
    ENDIF

  ENDIF

      
; -------------------------------------------------------------
; Calculation direction. Upwelling has precedence so if they're
; both set, upwelling will be the calculation performed.
; -------------------------------------------------------------

; -- Default
  upwelling = 1

; -- Check keywords
  IF ( KEYWORD_SET( downwelling ) ) THEN upwelling = 0
  IF ( KEYWORD_SET( upwelling ) )   THEN upwelling = 1

  
; ------------
; Zenith angle
; ------------

  IF ( N_ELEMENTS( zenith_angle ) EQ 0 ) THEN BEGIN

;   -- Set default zenith angle if not specified
    zenith_angle = 0.0

  ENDIF ELSE BEGIN

;   -- Make sure angle isn't negative
    zenith_angle = FLOAT( zenith_angle )
    IF ( zenith_angle LT 0.0 ) THEN BEGIN
      MESSAGE, 'Input zenith angle negative. Using the absolute value.', /INFO
      zenith_angle = ABS( zenith_angle )
    ENDIF

  ENDELSE


; --------------------------------------------------------------------
; Check for height inputs. Note that the value of calculation_levels,
; if defined, has precedence over observer_ and boundary_height.
; --------------------------------------------------------------------

; -- Define the calculation levels if the boundaries are to be used
  IF ( KEYWORD_SET( use_boundary_levels ) ) THEN $
    calculation_levels = alt

; -- Define some maximums
  max_n_calculation_levels = 200    ; LBLRTM contraint
  maximum_altitude         = 65.0   ; in km

; -- Check if calculation_levels defined
  IF ( N_ELEMENTS( calculation_levels ) GT 0 ) THEN BEGIN

;   -- Sort them and extract out repeats
    calculation_levels = calculation_levels[ UNIQ( calculation_levels, $
                                                   SORT( calculation_levels ) ) ]

;   -- Make sure there is enough and not too many
    n_calculation_levels = N_ELEMENTS( calculation_levels )

    IF ( n_calculation_levels LE 1 OR $
         n_calculation_levels GT max_n_calculation_levels ) THEN BEGIN

      MESSAGE, 'Not enough (<2) or too many (>' + $
               STRING( max_n_calculation_levels, FORMAT = '( i3 )' ) + $
               ') calculation heights input', /INFO
      RETURN, -1

    ENDIF

;   -- Set observer and boundary heights
    CASE upwelling OF
      0: observer_altitude = MIN( calculation_levels, MAX = boundary_altitude )
      1: observer_altitude = MAX( calculation_levels, MIN = boundary_altitude )
    ENDCASE

  ENDIF ELSE BEGIN

;   -- Check if observer height supplied
    IF ( N_ELEMENTS( observer_height ) NE 0 ) THEN BEGIN

      observer_altitude = FLOAT( observer_height )

      IF ( observer_altitude LT 0.0 ) THEN BEGIN
        MESSAGE, 'Negative valued observer height input', /INFO
        RETURN, -1
      ENDIF
      IF ( observer_altitude GT maximum_altitude ) THEN BEGIN
        MESSAGE, 'Observer height input too large (>' + $
                 STRING( maximum_altitude, FORMAT = '( f4.1 )' ) + ').', /INFO
        RETURN, -1
      ENDIF

    ENDIF ELSE BEGIN

      CASE upwelling OF
        0: observer_altitude = surface_altitude
        1: observer_altitude = maximum_altitude
      ENDCASE

    ENDELSE

;   -- Check if boundary height supplied
    IF ( N_ELEMENTS( boundary_height ) NE 0 ) THEN BEGIN

      boundary_altitude = FLOAT( boundary_height )

      IF ( boundary_altitude LT 0.0 ) THEN BEGIN
        MESSAGE, 'Negative valued boundary height input', /INFO
        RETURN, -1
      ENDIF
      IF ( boundary_altitude GT maximum_altitude ) THEN BEGIN
        MESSAGE, 'Boundary height input too large (>' + $
                 STRING( maximum_altitude, FORMAT = '( f4.1 )' ) + ').', /INFO
        RETURN, -1
      ENDIF

    ENDIF ELSE BEGIN

      CASE upwelling OF
        0: boundary_altitude = maximum_altitude
        1: boundary_altitude = surface_altitude
      ENDCASE

    ENDELSE

;   -- Make sure final observer/boundary heights are consistent. Shouldn't
;   -- need to do this but what the hell.
    CASE upwelling OF
      0: observer_altitude = MIN( [ observer_altitude, boundary_altitude ], $
                                  MAX = boundary_altitude )
      1: observer_altitude = MAX( [ observer_altitude, boundary_altitude ], $
                                  MIN = boundary_altitude )
    ENDCASE

;   -- Determine calculation levels
    calculation_levels = compute_calculation_levels( alt, $
                                                     observer_altitude, $
                                                     boundary_altitude, $
                                                     maximum_altitude )

  ENDELSE


; ------------
; Merge option
; ------------

  IF ( N_ELEMENTS( merge_option ) EQ 0 ) THEN BEGIN

;   -- Set default merge_option option if not specified
    merge_option = 0

  ENDIF ELSE BEGIN

;   -- Make sure angle is an integer
    merge_option = FIX( merge_option )

  ENDELSE


; --------------------
; Cross section option
; --------------------

  IF ( N_ELEMENTS( xsection_option ) EQ 0 ) THEN BEGIN

;   -- Set default xsection_option option if not specified
    xsection_option = 0

  ENDIF ELSE BEGIN

;   -- Turn on cross-sections if needed
    IF ( xsection_option NE 0 ) THEN xsection_option = 1

  ENDELSE


; -------------------------------
; Check if header string supplied
; -------------------------------

  IF ( KEYWORD_SET( header ) ) THEN BEGIN

    header = STRING( header )

  ENDIF ELSE BEGIN

;   -- Define a header if OS is unix.
    CASE STRUPCASE( !VERSION.OS_FAMILY ) OF
      'UNIX': BEGIN
                SPAWN, 'whoami', user_name
                SPAWN, 'date "+%a %b %e, %Y"', date
                header = 'LBLRTM TAPE5 file created for ' + user_name + ' on ' + date
      END
      ELSE: header = ' '
    ENDCASE

  ENDELSE


; ---------------------------------
; Check if output filename supplied
; ---------------------------------

  IF ( KEYWORD_SET( filename ) ) THEN BEGIN
    filename = STRING( filename )
  ENDIF ELSE BEGIN
    filename = 'tape5.rdk'
  ENDELSE



;------------------------------------------------------------------------------
;                           -- Open output file -- 
;------------------------------------------------------------------------------

  OPENW, lun_tape5, filename, /GET_LUN, $
         WIDTH = 100, $
         ERROR = open_error

  IF ( open_error NE 0 )  THEN BEGIN
    MESSAGE, 'Error opening output TAPE5 file ' + $
             filename + ': ' + !ERR_STRING, /INFO
    RETURN, -1
  ENDIF



;------------------------------------------------------------------------------
;                       -- Output pre-profile TAPE 5 records -- 
;------------------------------------------------------------------------------

; --------------------------
; Header record (record 1.1)
; --------------------------

  PRINTF, lun_tape5, FORMAT = '( "$ ", a )', header


; ---------------------------------
; Calculation switches (Record 1.2)
; ---------------------------------

  hirac     = 1
  lblf4     = 1
  continuum = 1
  aerosol   = 0
  emit      = 1
  scan      = 0
  filter    = 0
  plot_out  = 0
  test      = 0
  atm       = 1
  merge     = merge_option
  laser     = 0
  od_layer  = 0
  xsection  = xsection_option
  od_mpts   = 0
  od_npts   = 0

  print_format = '(" HI=",i1," F4=",i1," CN=",i1," AE=",i1," EM=",i1," SC=",i1," FI=",i1,"' + $
                 ' PL=",i1," TS=",i1," AM=",i1," M=",i2.2," LA=",i1," OD=",i1," XS=",i1,' + $
                 ' 1x, i4, 1x, i4 )'

  PRINTF, lun_tape5, FORMAT = print_format, $
                     hirac, lblf4, continuum, aerosol, emit, scan, filter, plot_out, test, atm, $
                     merge, laser, od_layer, xsection, od_mpts, od_npts


; ------------------------------
; Wavenumber limits (Record 1.3)
; ------------------------------

  PRINTF, lun_tape5, FORMAT = '( 2( f10.3 ) )', $
                     begin_wavenumber, end_wavenumber


; --------------------------------
; Boundary conditions (Record 1.4)
; --------------------------------

; -- Set the boundary temperature to use if conditions are met

  IF ( use_boundary_temperature EQ 1 ) THEN BEGIN

    b_t = boundary_temperature

  ENDIF ELSE BEGIN

    CASE upwelling OF
      0: b_t = 3.9          ; Uplooking:   Cold space temperature
      1: b_t = t[ 0 ]       ; Downlooking: Surface air temperature
    ENDCASE

  ENDELSE


; -- Set the boundary emissivity.
; -- Right now it is simply 1.0

  boundary_emissivity = 1.0
  b_e = boundary_emissivity


; -- Output the record 

  PRINTF, lun_tape5, FORMAT = '( 2( f10.3 ) )', $
                     b_t, b_e


; ------------------------------------
; LBLATM routine switches (Record 3.1)
; ------------------------------------

; -- Set the maximum molecule number
  max_molecule_number = MAX( absorber_number )

; -- Determine profile type. Valid options are:

  profile_type = 0   ; User supplied atmospheric profile
;                1     Tropical
;                2     Midlatitude summer
;                3     Midlatitude winter
;                4     Subarctic summer
;                5     Subarctic winter
;                6     US Standard Atmosphere


; -- Set path type ( slant path from observer height to boundary height)

  path_type = 2


; -- Number of calculation levels

  n_calculation_levels = N_ELEMENTS( calculation_levels )


; -- Other switches

  no_zero = 1               ; Suppresses zeroing of absorber amounts which are less
                            ; than 0.1% of total

  no_print = 1              ; Short print out to TAPE6 (no refractive index information)

  n_molecules = max_molecule_number > 7  ; Number of molecular species (minimum of 7).

  i_punch = 1               ; Layer data written to TAPE7

  i_fixtype = 0             ; Suppresses the output of information (ITYPE) to TAPE7 that
                            ; overrides the LBLRTM internal calculation controlling the
                            ; DV ratio of the previous layer to the current layer. A blank
                            ; field in the TAPE7 output means default to the internal
                            ; calculation.

  units = 0                 ; Output layer molecular column amounts to TAPE7 (molecules/cm^2).
                            ; If UNITS is set to 1, molecular mixing ratio are output.
                            ; The molecular column amount are preferable as these are the
                            ; units used by LBLRTM so no conversion is necessary.

  earth_radius = 0.0        ; Radius of earth in km. Defaults for a zero value:
                            ; a) climatology_model = 0,2,3,6  earth_radius = 6371.23km
                            ; b)                   = 1        earth_radius = 6378.39km
                            ; c)                   = 4,5      earth_radius = 6356.91km

  altitude_of_space = 0.0   ; Altitude definition for space. Default value is 100km.

  average_wavenumber = 0.5 * ( begin_wavenumber + end_wavenumber )   ; Frequency for refractive
                                                                     ; geometry calculation.


; -- Output record

  IF ( use_co2ppmv EQ 1 ) THEN BEGIN
    PRINTF, lun_tape5, FORMAT = '( 7( i5 ), i2, 1x, i2, 4( f10.3 ) )', $
                       profile_type, path_type, n_calculation_levels, $
                       no_zero, no_print, n_molecules, i_punch, i_fixtype, $
                       units, earth_radius, altitude_of_space, average_wavenumber, $
                       co2ppmv
  ENDIF ELSE BEGIN
    PRINTF, lun_tape5, FORMAT = '( 7( i5 ), i2, 1x, i2, 3( f10.3 ) )', $
                       profile_type, path_type, n_calculation_levels, $
                       no_zero, no_print, n_molecules, i_punch, i_fixtype, $
                       units, earth_radius, altitude_of_space, average_wavenumber
  ENDELSE


; ----------------------------------
; Slant path parameters (Record 3.2)
; ----------------------------------

; -- Set correct zenith angle

  CASE upwelling OF
    0: z_a = zenith_angle            ; Downwelling/uplooking
    1: z_a = 180.0 - zenith_angle    ; Upwelling/downlooking
  ENDCASE


; -- Output record

  PRINTF, lun_tape5, FORMAT = '( 3( f10.3 ) )', $
                     observer_altitude, boundary_altitude, z_a


; --------------------------------
; Calculation levels (Record 3.3B)
; --------------------------------

  PRINTF, lun_tape5, FORMAT = '( 8( f10.3 ) )', calculation_levels



;------------------------------------------------------------------------------
;     -- Determine the number of profile levels required (Record 3.4) --
;------------------------------------------------------------------------------

; ----------------------------------------------------------
; Determine the total number of profile + climatology levels
; ----------------------------------------------------------

  index = WHERE( calculation_levels GT MAX( alt ), n_climatology_model_levels )
  n_profile_levels = n_input_levels + n_climatology_model_levels 


; ------------------------------------------------------------
; Now check if the total number of profile levels is too large
; ------------------------------------------------------------

; -- Set maximum number of profile levels (LBLRTM constraint)
  max_profile_levels = 3400

; -- Are there too many levels?
  IF ( n_profile_levels GT max_profile_levels ) THEN BEGIN

;   -- Yes. Interpolate profiles to a valid number of levels
    interpolate_profiles, max_profile_levels, $
                          n_input_levels, $
                          n_climatology_model_levels, $
                          n_absorbers, $
                          p, t, q

;   -- Calculate new altitudes
    result = geopotential_altitude( p, t, q[ *, h2o_absorber_index ], $
                                    surface_altitude, alt )

;   -- Determine new level totals
    n_input_levels   = N_ELEMENTS( p )
    n_profile_levels = n_input_levels + n_climatology_model_levels 
    
  ENDIF

; -- Output record
  PRINTF, lun_tape5, FORMAT = '( i5 )', n_profile_levels



;------------------------------------------------------------------------------
;           -- Construct the "JCHAR" string used for the molecular --
;           -- species for which no profile data was provided.     --
;------------------------------------------------------------------------------

; ------------------------
; Initialise JCHAR strings
; ------------------------

  jchar_profile = ''
  jchar_model   = ''


; -----------------------
; Construct JCHAR strings
; -----------------------

  FOR i = 0, n_molecules - 1 DO BEGIN

;   -- Is current molecule number included?
    index = WHERE( absorber_number EQ ( i + 1 ), count )
    IF ( count GT 0 ) THEN BEGIN

;     -- Yes. Add units to JCHAR
      jchar_profile = jchar_profile + absorber_units[ index[ 0 ] ]

    ENDIF ELSE BEGIN

;     -- No. Add climatology
      jchar_profile = jchar_profile + STRING( climatology_model, FORMAT = '(i1)' )

    ENDELSE

;   -- Create model JCHAR string
    jchar_model = jchar_model + STRING( climatology_model, FORMAT = '(i1)' )

  ENDFOR



;------------------------------------------------------------------------------
;                        -- Output profile TAPE 5 records -- 
;------------------------------------------------------------------------------

; -------------------------------------
; Output the USER provided profile data
; -------------------------------------

  FOR i = 0, n_profile_levels - n_climatology_model_levels - 1 DO BEGIN

;   -- Output altitude, pressure, temperature and JCHAR string
;;;;PRINTF, lun_tape5, FORMAT = '( 3( f10.3 ), 5x, "AA", 3x, a )', $
    PRINTF, lun_tape5, FORMAT = '( 3( f10.3 ), 5x, "AA L", 1x, a )', $
                       alt[ i ], p[ i ], t[ i ], jchar_profile
    max_profile_altitude = alt[ i ]

;   -- Create array of molecular data for the maximum number of
;   -- molecules for current level. Make sure that the number of
;   -- spaces in each array element is 10.
;;;;q_level = REPLICATE( '          ', max_molecule_number )
    q_level = REPLICATE( '               ', max_molecule_number )

;   -- Fill current level data array in the correct positions (user may
;   -- have specified "non-adjacent" molecules
    q_level[ absorber_number - 1 ] = STRING( q[ i, * ], $
                                             FORMAT = '( e15.8 )' )
;;;;                                         FORMAT = '( e10.3 )' )

;   -- Output molecular information with a CR/LF every 8 molecules
    PRINTF, lun_tape5, FORMAT = '( 8( a, : ) )', q_level

  ENDFOR


; -----------------------------
; Output the MODEL profile data
; -----------------------------

; -- Determine the number of blank lines required
; -- for molecular species output
  n_molecules_per_line = 8
  n_blank_lines        = max_molecule_number / n_molecules_per_line
  IF ( ( max_molecule_number MOD n_molecules_per_line ) GT 0 ) THEN $
    n_blank_lines = n_blank_lines + 1
  blank_line_format = '( ' + STRING( n_blank_lines ) + '(/))'

; -- Loop over remaining layers
  FOR i = 0, n_climatology_model_levels - 1 DO BEGIN

;   -- Output altitude, pressure, temperature and JCHAR string line
    PRINTF, lun_tape5, FORMAT = '( f10.3, 25x, a, 3x, a )', $
                       calculation_levels[ index[ i ] ], $
                       STRMID( jchar_model, 0, 2 ), $
                       jchar_model
    max_profile_altitude = calculation_levels[ index[ i ] ]

;   -- Output blank lines
    PRINTF, lun_tape5, FORMAT = blank_line_format
   
  ENDFOR



;------------------------------------------------------------------------------
;                    -- Output X-section profile TAPE 5 records -- 
;------------------------------------------------------------------------------

  IF ( xsection_option EQ 1 ) THEN BEGIN

;   ----------------------------------------------------------
;   Define X-section names (left-justified) and mass (Don't
;   need the mass right now but future versions of this code
;   might need them for amount conversions, e.g. ppmv -> g/kg)
;   ----------------------------------------------------------

;;    xs_molecule_names = [ 'CCL4      ', $   ; No alias
;;                          'CCL3F     ', $   ; Alias: CFCL3, CFC11, F11
;;                          'CCL2F2    ', $   ; Alias: CF2CL2, CFC12, F12
;;                          'CCLF3     ', $   ; Alias: CF3CL, CFC13, F13
;;                          'CF4       ', $   ; Alias: CFC14, F14
;;                          'CHCL2F    ', $   ; Alias: CHFCL2, CFC21, F21
;;                          'CHCLF2    ', $   ; Alias: CHF2CL, CFC22, F22
;;                          'C2CL3F3   ', $   ; Alias: C2F3CL3, CFC113, F113
;;                          'C2CL2F4   ', $   ; Alias: C2F4CL2, CFC114, F114
;;                          'C2CLF5    ', $   ; Alias: C2F5CL, CFC115, F115
;;                          'CLONO2    ', $   ; Alias: CLNO3
;;                          'HNO3      ', $   ; No alias
;;                          'HNO4      ', $   ; No alias
;;                          'N2O5      ' ]    ; No alias

    xs_molecule_names = [ 'CCL4      ', $     ; No alias
                          'CCL3F     ', $     ; Alias: CFCL3, CFC11, F11
                          'CCL2F2    ' ]      ; Alias: CF2CL2, CFC12, F12
    n_xs_molecules = N_ELEMENTS( xs_molecule_names )

;;    xs_molecule_mass = [  153.82, $      ; CCL4
;;                          137.37, $      ; CCL3F
;;                          120.91, $      ; CCL2F2
;;                          104.46, $      ; CCLF3
;;                           88.00, $      ; CF4
;;                          102.92, $      ; CHCL2F
;;                           86.47, $      ; CHCLF2
;;                          187.38, $      ; C2CL3F3
;;                          170.92, $      ; C2CL2F4
;;                          154.47, $      ; C2CLF5
;;                           97.46, $      ; CLONO2
;;                           63.01, $      ; HNO3
;;                           79.01, $      ; HNO4
;;                          108.01 ]       ; N2O5


;   ---------------------------------------------------------------
;   Define some concentrations for some of the X-section molecules.
;   These data are taken from the WMO Global Ozone Research and
;   Monitoring Project - Report No.20, "Scientific Assemssment
;   of Stratospheric Ozone: 1989", Vol.1 (ISBN 92 807 1255 1)
;   ---------------------------------------------------------------

;;    xs_molecule_pptv = [ 140.0, $        ; CCL4
;;                         240.0, $        ; CCL3F
;;                         415.0, $        ; CCL2F2
;;                           5.0, $        ; CCLF3
;;                          -1.0, $        ; CF4
;;                          -1.0, $        ; CHCL2F
;;                         100.0, $        ; CHCLF2
;;                          45.0, $        ; C2CL3F3
;;                          15.0, $        ; C2CL2F4
;;                           5.0, $        ; C2CLF5
;;                          -1.0, $        ; CLONO2
;;                          -1.0, $        ; HNO3
;;                          -1.0, $        ; HNO4
;;                          -1.0 ]         ; N2O5

    xs_molecule_pptv = [ 140.0, $        ; CCL4
                         240.0, $        ; CCL3F
                         415.0 ]         ; CCL2F2

    index_data = WHERE( xs_molecule_pptv GT 0 )
    xs_molecule_ppmv = xs_molecule_pptv
    xs_molecule_ppmv[ index_data ] = xs_molecule_pptv[ index_data ] / 1.0e+06

;   -- For now I'm using these numbers which are the 1995 UNEP values.

;;    xs_molecule_ppmv = [ 1.105e-04, $    ; CCL4
;;                         2.783e-09, $    ; CCL3F
;;                         5.027e-04 ]     ; CCL2F2


;   ---------------------------------------
;   Set some X-section calculation switches
;   ---------------------------------------

    profile_type = 0          ; User supplied atmospheric profile (using xs_molecule_ppmv)
;                  1            Tropical
;                  2            Midlatitude summer
;                  3            Midlatitude winter
;                  4            Subarctic summer
;                  5            Subarctic winter
;                  6            US Standard Atmosphere
 
    pressure_convolution = 0  ; Flag to deselect pressure convolution of cross-sections
                              ;   = 0 cross-sections convolved with pressure
                              ;   = 1 cross-sections not convolved with pressure

;   -----------------
;   Output Record 3.7
;   -----------------

    PRINTF, lun_tape5, FORMAT = '( 3( i5 ), 2x, "The following cross-sections were selected:" )', $
                       n_xs_molecules, profile_type, pressure_convolution


;   -------------------
;   Output record 3.7.1
;   -------------------

    PRINTF, lun_tape5, FORMAT = '( 7a10, ( /, 8a10 ) )', xs_molecule_names


;   -----------------
;   Output record 3.8
;   -----------------

;   -- Set the number of profile levels. Right now this is
;   -- fixed to 2 and the same concentrations are used for
;   -- the surface and TOA altitudes - mixed gas assumption.

    xs_profile_LEVELS   = [ surface_altitude, max_profile_altitude ]
    n_xs_profile_LEVELS = N_ELEMENTS( xs_profile_LEVELS )


;   -- Set the other record 3.8 flags

    boundary_units = 0     ; Flag which determines the boundary level units in the profile
                           ;   = 0 boundary level is defined as altitude in km,
                           ;   = 1 boundary level is defined as pressure in hPa (mb)

    xs_profile_title = 'Using 1989 WMO levels'   ;'Using 1995 UNEP values'


;   -- Write the record

    PRINTF, lun_tape5, FORMAT = '( 2( i5 ), 5x, a )', $
                       n_xs_profile_LEVELS, boundary_units, xs_profile_title


;   --------------------------------------------------------------
;   Output X-section profile data (Record 3.8.1 and 3.8.2 - 3.8.N)
;   --------------------------------------------------------------

    jchar = 'A'
    xs_jchar_profile = ''
    FOR i = 1, n_xs_molecules DO xs_jchar_profile = xs_jchar_profile + jchar

    FOR i = 0, n_xs_profile_LEVELS - 1 DO BEGIN

      PRINTF, lun_tape5, FORMAT = '( f10.3, 5x, a, /, 5( 8e10.3, :, / ) )', $
                         xs_profile_LEVELS[ i ], xs_jchar_profile, xs_molecule_ppmv

    ENDFOR

  ENDIF


;------------------------------------------------------------------------------
;        -- Done. Print end-of-input marker, close file and exit -- 
;------------------------------------------------------------------------------

  PRINTF, lun_tape5, FORMAT = '( "%" )'

  FREE_LUN, lun_tape5

  RETURN, 1
    
END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Log: create_tape5.pro,v $
; Revision 2.3  2005/06/29 22:28:12  paulv
; Massive commit to resync repository.
;
; Revision 2.2  1999/10/08 18:06:23  paulv
; Corrected bug in altitude variable naming throughout code.
;
; Revision 2.1  1999/08/31 17:52:39  paulv
; Updated header documentation.
;
; Revision 2.0  1999/07/08 15:50:15  paulv
; New version for multiple absorber amount input.
;
; Revision 1.4  1999/06/03 21:44:02  paulv
; - Added MERGE_OPTION and XSECTION_OPTION keywords to allow the user to
;   set these values if required.
; - Boundary temperature specified by user is now used regardless of whether
;   the calculation direction is up- or downwelling. Previously the boundary
;   temperature was set only if an upwelling calculation was done but this
;   caused the definition of boundary temperature under the conditions of
;   a) yes, the keyword is set and b) no, it is not an upwelling calc, to
;   not occur and ==> error/crash.
;
; Revision 1.3  1999/06/03 17:52:40  paulv
; - Added USE_BOUNDARY_LEVELS keyword to allow user to specify the input
;   profile boundaries as the calculation levels.
; - Removed call to rh_to_mr function that was used as stop-gap for
;   previous calculations.
;
; Revision 1.2  1999/05/24 18:02:54  paulv
; Next version. Added more in-line functions.
;
; Revision 1.1  1999/02/26 01:09:37  paulv
; Initial version
;
;==============================================================================
