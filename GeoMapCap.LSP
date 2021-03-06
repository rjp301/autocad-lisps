(defun c:geomapcap (/ Ctab Layout)
	(princ "\nCapture Geomap of all viewports")
 	(setq Ctab (getvar "CTAB"))
 	(foreach Layout (reverse (layoutlist))
   		(command "LAYOUT" "S" Layout)
   		(command "MSPACE")
		(command "GEOMAP" "H")
   		(command "GEOMAPIMAGE" "V")
		(command "PSPACE")
 	)
	(setvar "CTAB" Ctab)
	(princ)
)