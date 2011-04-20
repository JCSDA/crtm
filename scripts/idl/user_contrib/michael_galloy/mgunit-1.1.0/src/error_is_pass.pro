error = 0L
catch, error
if (error ne 0L) then begin
  catch, /cancel
  return, 1
endif

on_ioerror, ioError
goto, startTest
ioError: return, 1
startTest:
