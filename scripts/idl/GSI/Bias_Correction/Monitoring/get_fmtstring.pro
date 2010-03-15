FUNCTION Get_FmtString, Number

  CASE 1 OF
    ( ABS(Number) LT 10.0 )      : FmtString = '(f7.4)'

    ( ABS(Number) GE 10.0 AND $
      ABS(Number) LT 100.0    )  : FmtString = '(f7.3)'

    ( ABS(Number) GE 100.0 AND $
      ABS(Number) LT 1000.0    ) : FmtString = '(f7.2)'

    ELSE:                          FmtString = '(e8.1)'
  ENDCASE

  RETURN, FmtString

END

