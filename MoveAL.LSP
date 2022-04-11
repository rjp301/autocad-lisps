(defun c:moveal (/ Ctab Layout)
	(princ "\nRepeat move operation across layouts")
 	(setq Ctab (getvar "CTAB"))
	(setq pt1 (getpoint "Pick first point: "))
	(setq pt2 (getpoint "Pick other corner point: "))
	(setq bp1 (getpoint "Specify base point: "))
	(setq bp2 (getpoint "Specify second point: "))
 	(foreach Layout (reverse (layoutlist))
		(command "LAYOUT" "S" Layout)
		(setq sel1 (ssget "c" pt1 pt2))
  		(command "MOVE" sel1 bp1 bp2)
 	)
	(setvar "CTAB" Ctab)
	(princ)
)

;Degrees to Radians
(defun dtr (x)
	;define degrees to radians function
 
	(* pi (/ x 180.0))
	;divide the angle by 180 then
	;multiply the result by the constant PI
 
)	;end of function

(princ)