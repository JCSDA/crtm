CD, 'C:\IDL\Catalyst\source\applications\DataViewer\source\', CURRENT=thisDir
Restore, 'C:\IDL\catalyst\catalyst.sav', /Verbose
Restore, 'C:\IDL\coyote\coyote.sav', /Verbose

Resolve_Routine, 'dataviewer'
resolve_all, CLASS=['gridwindow', 'nsidc_image']
Save, File='../dataviewer.sav', /ROUTINES
CD, thisDir
END


