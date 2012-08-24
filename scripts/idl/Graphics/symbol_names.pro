PRO Symbol_Names
  @symbol_db
  FOR i = 0, N_ELEMENTS(SYMBOL_NAMES)-1 DO BEGIN
    PRINT, FORMAT='("Index ",i2," => ",a)', i, SYMBOL_NAMES[i]
  ENDFOR
END
