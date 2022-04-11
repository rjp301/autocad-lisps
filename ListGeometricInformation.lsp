(defun C:LGDO (/ *error* source data txtline) ; = Log of Geometric Data Only
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break"))
      (princ (strcat "\nError: " errmsg))
    ); if
    (setvar 'qaflags 0); reset
    (princ)
  ); defun -- *error*
  (if (findfile (getvar 'logfilename)); log file already exists
    (progn ; then
      (setvar 'logfilemode 0); in case currently on
      (setq source (open (getvar 'logfilename) "w")); empty any current content
      (close source)
    ); progn
  ); if
  (prompt "\nTo Export their Properties,")
  (if (setq ss (ssget '((0 . "*LINE,ARC,CIRCLE,ELLIPSE"))))
    ; [will provide its own "Select objects:" prompt]
    ; *LINE covers Line/Polyline/Spline/Xline/Mline
    (progn ; then
      (setvar 'qaflags 2)
      (setvar 'logfilemode 1); turn on log file recording
      (command "_.list" ss "")
      (setvar 'logfilemode 0); turn off
      (setvar 'qaflags 0)
    ); progn
  ); if
  (setq
    source (open (getvar 'logfilename) "r"); log file just created
    data (open "C:\Users\riley\Desktop\ACAD_data.txt" "w"); reduced-content file <-- EDIT this
  ); setq
  (while (setq txtline (read-line source)); step through lines in file
    (if
      (not
        (wcmatch
          (vl-string-left-trim " " txtline); remove leading spaces
          "`[ A*,_.li*,Sel*,Spa*,Col*,Line*,Tra*,Thi*,Han*,Pre*,Ent*,Com*"
            ;; beginnings of unwanted-line content [could be more]
        ); wcmatch
      ); not
      (write-line txtline data); then -- put line into geometric data file
    ); if
  ); while
  (close source)
  (close data)
  (princ)
); defun