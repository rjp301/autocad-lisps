(defun c:ChangeMtextBox (/ temp1 Ent)

	(if (setq temp1 (GetMTextBB (setq Ent (car (entsel "\n Select Mtext: ")))))
	(progn
		(if (equal (caar temp1) (caadr temp1) 0.0001)
	(progn
	(entmod (subst (cons 43 (distance (car temp1) (cadr temp1))) (assoc 43
	(entget Ent)) (entget Ent)))
	(entmod (subst (cons 41 (distance (car temp1) (caddr temp1))) (assoc 41
	(entget Ent)) (entget Ent)))
	)
	(progn
	(entmod (subst (cons 43 (distance (car temp1) (caddr temp1))) (assoc 43
	(entget Ent)) (entget Ent)))
	(entmod (subst (cons 41 (distance (car temp1) (cadr temp1))) (assoc 41
	(entget Ent)) (entget Ent)))
	)
	)
	)
	)
(princ)
)