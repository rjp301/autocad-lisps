; APPEND command - APPEND TEXT TO STRINGS SELECTED

(defun C:APPEND (/ p l n e sw sl chm cont)
   (setq chm 0 p (ssget))            ; Select objects
   (if p (progn                      ; If any objects selected
      (setq cont t)
      (while cont
	 (setq sl (strlen (setq sw (getstring "\nNew string: " t))))
	 (if (= sl 0)
            (princ "Null input invalid")
            (setq cont nil)
         )
      )
      (setq l 0 n (sslength p))
      (while (< l n)                 ; For each selected object...
         (if (= "TEXT"               ; Look for TEXT entity type (group 0)
                (cdr (assoc 0 (setq e (entget (ssname p l))))))
            (progn
		  (setq e (subst (cons 1 (strcat (CDR (assoc 1 e)) sw)) (assoc 1 e) e))
                  (entmod e)         ; Modify the TEXT entity
		  (setq chm (1+ chm))
	    )
	 )
         (setq l (1+ l))
      )
   ))
   (setq chm (strcat "Changed " (itoa chm) " text lines."))
   (princ chm)
   (terpri)
chm
)
