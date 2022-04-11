(defun c:-txt2mtxt (/ ss cnt)
  (setq ss (ssget '((0 . "TEXT"))))
  (if ss
    (repeat (setq cnt (sslength ss))
      (command "txt2mtxt" (ssname ss (setq cnt (1- cnt))) "")
    )
  )
)