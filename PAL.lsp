(defun c:PAL ()
(foreach lay (layoutlist)
(setvar 'CTab lay)
(COMMAND "-PLOT"
"Y"
""
"AutoCAD PDF (General Documentation).pc3"
"ANSI full bleed A (8.50 x 11.00 Inches)"
"Inches"
"PORTRAIT"
"N"
"L"
"1=25.4"
"0.00,0.00"
"Y"
"acad.ctb"
"N"
"Y"
"N"
"N"
"N"
""; Name of file
""
"N"
"y" )
)
)