  ; FitStats Parameters

  MOL_SET_NAME = ['wet','dry','ozo']
  MAX_N_MOL_SETS = N_ELEMENTS(MOL_SET_NAME)
  ; Plotting colors
  MOL_SET_COLOR = (!D.NAME EQ 'PS') ? [BLUE,RED,GREEN] : [CYAN,RED,GREEN]
  ; Plot labels
  PLOT_LABEL = '('+ASCII_Indgen(MAX_N_MOL_SETS*2 + 2)+')'
  XLABEL = 0.925
  YLABEL = 0.9
  ; Literal constants
  ZERO = 0.0d0
  ONE  = 1.0d0
