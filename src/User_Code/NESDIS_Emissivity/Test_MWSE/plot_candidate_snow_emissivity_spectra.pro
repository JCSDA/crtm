PRO Plot_Candidate_Snow_Emissivity_Spectra


  ; --------------------------------------------
  ; Define the emissivity spectra and their type
  ; --------------------------------------------

  Frequency   = [4.9d,6.93d,10.65d,18.7d,23.8d,31.4d,50.3d,52.5d,89.0d,150.0d]

  Emissivity = [[0.87d,0.89d,0.91d,0.93d,0.94d,0.94d,0.94d,0.93d,0.92d,0.90d],$
                [0.91d,0.91d,0.92d,0.91d,0.90d,0.90d,0.91d,0.91d,0.91d,0.86d],$
                [0.90d,0.89d,0.88d,0.87d,0.86d,0.86d,0.85d,0.85d,0.82d,0.82d],$
                [0.91d,0.91d,0.93d,0.93d,0.93d,0.93d,0.89d,0.88d,0.79d,0.79d],$
                [0.90d,0.89d,0.88d,0.85d,0.84d,0.83d,0.83d,0.82d,0.79d,0.73d],$
                [0.90d,0.89d,0.86d,0.82d,0.80d,0.79d,0.78d,0.78d,0.77d,0.77d],$
                [0.88d,0.86d,0.85d,0.80d,0.78d,0.77d,0.77d,0.76d,0.72d,0.72d],$
                [0.93d,0.94d,0.96d,0.96d,0.95d,0.93d,0.87d,0.86d,0.74d,0.65d],$
                [0.87d,0.86d,0.84d,0.80d,0.76d,0.76d,0.75d,0.75d,0.70d,0.69d],$
                [0.87d,0.86d,0.83d,0.77d,0.73d,0.68d,0.66d,0.66d,0.68d,0.67d],$
                [0.89d,0.89d,0.88d,0.87d,0.86d,0.82d,0.77d,0.76d,0.69d,0.64d],$
                [0.88d,0.87d,0.86d,0.83d,0.81d,0.77d,0.74d,0.73d,0.69d,0.64d],$
                [0.86d,0.86d,0.86d,0.85d,0.82d,0.78d,0.69d,0.68d,0.51d,0.47d],$
                [0.89d,0.88d,0.87d,0.83d,0.80d,0.75d,0.70d,0.70d,0.64d,0.60d],$
                [0.91d,0.92d,0.93d,0.88d,0.84d,0.76d,0.66d,0.64d,0.48d,0.44d],$
                [0.94d,0.95d,0.97d,0.91d,0.86d,0.74d,0.63d,0.63d,0.50d,0.45d] ]

  Emissivity_Type = [' 1:Wet Snow',$
                     ' 2:Grass_after_Snow',$
                     ' 3:RS_Snow (A)',$
                     ' 4:Powder Snow',$
                     ' 5:RS_Snow (B)',$
                     ' 6:RS_Snow (C)',$
                     ' 7:RS_Snow (D)',$
                     ' 8:Thin Crust Snow',$
                     ' 9:RS_Snow (E)',$
                     '10:Bottom Crust Snow (A)',$
                     '11:Shallow Snow',$
                     '12:Deep Snow',$
                     '13:Crust Snow',$
                     '14:Medium Snow',$
                     '15:Bottom Crust Snow (B)',$
                     '16:Thick Crust Snow' ]


  ; -----------------------------
  ; Define the spectra dimensions
  ; -----------------------------

  Emissivity_Info = SIZE( Emissivity, /STRUCTURE )
  n_Frequencies = (Emissivity_Info.DIMENSIONS)[0]
  n_Spectra     = (Emissivity_Info.DIMENSIONS)[1]


  ; -------------------------------
  ; Define some plotting parameters
  ; -------------------------------

  ; -- The line thickness
  IF ( !D.NAME EQ 'PS' ) THEN $
    Thick = 3 $
  ELSE $
    Thick = 1

  Emissivity_Thick = REPLICATE( Thick, n_Spectra )

  ; -- Define a set of unique colors
  Unique_Color    = [ 1, 2, 4, 5, 6, !P.COLOR, 10, 13 ]
  n_Unique_Colors = N_ELEMENTS( Unique_Color )

  ; -- Define a set of unique linestyles
  Unique_Linestyle    = [ 0, 3, 2, 1, 4, 5 ]
  n_Unique_Linestyles = N_ELEMENTS( Unique_Linestyle )

  ; -- Using the number of colors as the base, determine
  ; -- how many multiples of colored lines are required
  IF ( ( n_Spectra MOD n_Unique_Colors ) EQ 0 ) THEN $
    n_Sets = n_Spectra / n_Unique_Colors $
  ELSE $
    n_Sets = n_Spectra / n_Unique_Colors + 1

  IF ( n_Sets GT n_Unique_Linestyles ) THEN $
    MESSAGE, 'Not enough unique color/linestyle combinations'

  ; -- Create the color and linestyle arrays
  Emissivity_Color     = Unique_Color
  Emissivity_Linestyle = REPLICATE( Unique_Linestyle[0], n_Unique_Colors )

  FOR i = 1, n_Sets - 1 DO BEGIN
    Emissivity_Color = [ Emissivity_Color, Unique_Color ]
    Emissivity_Linestyle = [ Emissivity_Linestyle, REPLICATE( Unique_Linestyle[i], n_Unique_Colors ) ]
  ENDFOR

  ; -- Trim the color and linestyle arrays
  Emissivity_Color     = Emissivity_Color[0:n_Spectra-1]
  Emissivity_Linestyle = Emissivity_Linestyle[0:n_Spectra-1]

  ; -- Determine the emissivity range
  Emissivity_yRange = [ MIN( Emissivity ), MAX( Emissivity ) ]


  ; ----------------
  ; Plot the spectra
  ; ----------------

  ; -- Create a plotting space, nodata
  PLOT, Frequency, Emissivity[ *, 0 ], $
        TITLE = 'Candidate Microwave Snow Emissivity Spectra', $
        XTITLE = 'Frequency (GHz)', $
        YTITLE = 'Emissivity', $
        YRANGE = Emissivity_yRange, $
        /NODATA

  ; -- Loop over spectra and plot with assigned attributes
  FOR i = 0, n_Spectra - 1 DO BEGIN
    OPLOT, Frequency, Emissivity[ *, i ], $
           COLOR = Emissivity_Color[i], $
           LINESTYLE = Emissivity_Linestyle[i], $
           THICK = Emissivity_Thick[i]
  ENDFOR


  ; -------------
  ; Plot a legend
  ; -------------

  MyLegend, 0.05, 0.44, $
            Emissivity_Type, $
            dx_Pos = 0.06, $
            COLOR     = Emissivity_Color, $
            LINESTYLE = Emissivity_Linestyle, $
            THICK     = Emissivity_Thick

END ; PRO Plot_Candidate_Snow_Emissivity_Spectra
