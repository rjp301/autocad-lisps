; BEFORE command - INSERT TEXT INTO BEGINNING OF STRING POINTED TO

(defun C:BEFORE (/ p l n e sw sl chm cont)
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
		  (setq e (subst (cons 1 (strcat sw (CDR (assoc 1 e)))) (assoc 1 e) e))
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