;; Dictionary Editing tools
;; by Irné Barnard

(vl-load-com)
(setq	editable_dict_items
			 nil
			deletable_dict_items
			 nil
			editable_dicts
			 nil
			deletable_dicts
			 nil
) ;_ end of setq

;; Command to view / delete / edit dictionaries
(defun c:DictEdit	(/					 dictlst1		 dcl				 DE:Disp
									 DE:Dict		 DE:Item		 DE:Save		 DE:Delete
									 DE:ItemEdit DE:ItemDel	 DE:ItemDisp DE:Data
									 cmdecho
									)
	(setq cmdecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)
	(command "_UNDO" "_BEGIN")
	;; Get the list of dictionaries
	(setq dictlst (GetAllDicts))

	;; Start loading the dialog
	(if	(not (setq dcl (load_dialog "DictEdit.DCL")))
		(exit)
	) ;_ if
	(if	(not (new_dialog "ListDict" dcl))
		(exit)
	) ;_ if

	;; Function to display Dictionary data
	(defun DE:Data (/ dnum d val)
		(setq	dnum (get_tile "dicts")
		) ;_ end of setq
		(if	(and dnum
						 (/= dnum "")
						 (new_dialog "ItemData" dcl)
				) ;_ end of and
			(progn
				(setq d (cadr (nth (atoi dnum) dictlst)))
				(set_tile	"main"
									(strcat "Raw Data - " (car (nth (atoi dnum) dictlst)))
				) ;_ end of set_tile
				(start_list "dlist" 3)
				(foreach val d
					(add_list (vl-prin1-to-string val))
				) ;_ end of foreach
				(end_list)
				(start_dialog)
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "disp" "(DE:Data)")

	;; Function to display Item data
	(defun DE:ItemDisp (/ dnum inum d val)
		(setq	dnum (get_tile "dicts")
					inum (get_tile "items")
		) ;_ end of setq
		(if	(and inum
						 (/= inum "")
						 dnum
						 (/= dnum "")
						 (new_dialog "ItemData" dcl)
				) ;_ end of and
			(progn
				(setq d (nth (atoi inum) (cddr (nth (atoi dnum) dictlst))))
				(set_tile "main" (strcat "Raw Data - " (car d)))
				(start_list "dlist" 3)
				(foreach val d
					(add_list (vl-prin1-to-string val))
				) ;_ end of foreach
				(end_list)
				(start_dialog)
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "dispitem" "(DE:ItemDisp)")

	;; Function to select item
	(defun DE:Item (inum / dnum dict item)
		(setq dnum (get_tile "dicts"))
		(if	(and inum (/= inum "") dnum (/= dnum ""))
			(progn
				(mode_tile "dispitem" 0)
				(if	(and
							(setq dict (nth (atoi dnum) dictlst))
							(setq item (nth (atoi inum) (cddr dict)))

						) ;_ end of and
					(progn
						(if	(assoc (cdr (assoc 0 (cdr item))) editable_dict_items)
							(mode_tile "edtitem" 0)
							(mode_tile "edtitem" 1)
						) ;_ end of if
						(if	(assoc (cdr (assoc 0 (cdr item))) deletable_dict_items)
							(mode_tile "delitem" 0)
							(mode_tile "delitem" 1)
						) ;_ end of if
					) ;_ end of progn
					(progn
						(mode_tile "delitem" 1)
						(mode_tile "edtitem" 1)
					) ;_ end of progn
				) ;_ end of if
			) ;_ end of progn
			(progn
				(mode_tile "delitem" 1)
				(mode_tile "edtitem" 1)
				(mode_tile "dispitem" 1)
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "items" "(DE:Item $value)")
																				;Action for selecting an item

	;; Function to view dictionary details
	(defun DE:Dict (dnum / dict item)
		(if	(and dnum (/= dnum ""))
			(progn
				(setq dict (nth (atoi dnum) dictlst))
				(set_tile "name" (car dict))
				(if	(assoc 280 (cadr dict))
					(progn
						(set_tile "owned" (itoa (cdr (assoc 280 (cadr dict)))))
						(mode_tile "owned" 0)
					) ;_ end of progn
					(mode_tile "owned" 1)
				) ;_ end of if
				(if	(assoc 281 (cadr dict))
					(progn
						(set_tile "duplicate" (itoa (cdr (assoc 281 (cadr dict)))))
						(mode_tile "duplicate" 0)
					) ;_ end of progn
					(mode_tile "duplicate" 1)
				) ;_ end of if
				(start_list "items" 3)
				(foreach item	(cddr dict)
					(add_list (car item))
				) ;_ end of foreach
				(end_list)
				(if	(> (length dict) 2)
					(mode_tile "items" 0)
					(mode_tile "items" 1)
				) ;_ end of if
				(set_tile "items" "")
				(DE:Item "")
				(if	(assoc (car dict) editable_dicts)
					(mode_tile "save" 0)
					(mode_tile "save" 1)
				) ;_ end of if
				(if	(assoc (car dict) deletable_dicts)
					(mode_tile "delete" 0)
					(mode_tile "delete" 1)
				) ;_ end of if
				(mode_tile "disp" 0)
			) ;_ end of progn
			(progn
				(set_tile "name" "")
				(set_tile "owned" "")
				(set_tile "duplicate" "")
				(start_list "items" 3)
				(end_list)
				(set_tile "items" "")
				(DE:Item "")
				(mode_tile "owned" 1)
				(mode_tile "duplicate" 1)
				(mode_tile "items" 1)
				(mode_tile "delitem" 1)
				(mode_tile "edtitem" 1)
				(mode_tile "dispitem" 1)
				(mode_tile "save" 1)
				(mode_tile "delete" 1)
				(mode_tile "disp" 1)
			) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "dicts" "(DE:Dict $value)")
																				;Action for selecting a dictionary

	;; Function to re-display
	(defun DE:Disp (dnum inum / dict)
		(start_list "dicts" 3)
		(foreach dict	dictlst
			(add_list (car dict))
		) ;_ end of foreach
		(end_list)
		(if	(> (length dictlst) 0)
			(mode_tile "dicts" 0)
			(mode_tile "dicts" 1)
		) ;_ end of if
		(if	dnum
			(set_tile "dicts" dnum)
			(set_tile "dicts" "")
		) ;_ end of if
		(DE:Dict dnum)
		(if	inum
			(set_tile "items" inum)
			(set_tile "items" "")
		) ;_ end of if
		(DE:Item inum)
	) ;_ end of defun
	(DE:Disp "" "")

	;; Function for saving edits of Dictionary
	(defun DE:Save (/ dnum dict func)
		(if
			(and (setq dnum (get_tile "dicts"))
					 (/= dnum "")
					 (setq dict (nth (atoi dnum) dictlst))
					 (setq func (cdr (assoc (car dict) editable_dicts)))
					 (=	1
							(WarnMsg
								(strcat	"You're about to overwrite "
												(car dict)
												"'s raw data. Are you sure you want to do this?"
								) ;_ end of strcat
							) ;_ end of WarnMsg
					 ) ;_ end of =
					 (apply
						 func
						 (list (cadr dict) (get_tile "owned") (get_tile "duplicate"))
					 ) ;_ end of apply
			) ;_ end of and
			 (progn
				 (setq dictlst (GetAllDicts))
				 (DE:Disp dnum "")
			 ) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "save" "(DE:Save)")

	;; Function for deleting Dictionary
	(defun DE:Delete (/ dnum dict func)
		(if
			(and (setq dnum (get_tile "dicts"))
					 (/= dnum "")
					 (setq dict (nth (atoi dnum) dictlst))
					 (setq func (cdr (assoc (car dict) deletable_dicts)))
					 (=	1
							(WarnMsg
								(strcat	"You're about to delete all "
												(car dict)
												"'s raw data. Are you sure you want to do this?"
								) ;_ end of strcat
							) ;_ end of WarnMsg
					 ) ;_ end of =
					 (apply func (list (cadr dict)))
			) ;_ end of and
			 (progn
				 (setq dictlst (GetAllDicts))
				 (if (setq dict (nth (atoi dnum) dictlst))
					 (DE:Disp dnum "")
					 (DE:Disp "" "")
				 ) ;_ end of if
			 ) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "delete" "(DE:Delete)")

	;; Function for editing items of Dictionary
	(defun DE:ItemEdit (/ dnum dict inum item func)
		(if
			(and (setq dnum (get_tile "dicts"))
					 (/= dnum "")
					 (setq dict (nth (atoi dnum) dictlst))
					 (setq inum (get_tile "items"))
					 (/= inum "")
					 (setq item (nth (atoi inum) (cddr dict)))
					 (setq func
									(cdr
										(assoc (cdr (assoc 0 (cdr item))) deletable_dict_items)
									) ;_ end of cdr
					 ) ;_ end of setq
					 (apply func (list (cadr dict) item))
			) ;_ end of and
			 (progn
				 (apply func (list (cadr dict) item))
				 (setq dictlst (GetAllDicts))
				 (DE:Disp dnum inum)
			 ) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "edtitem" "(DE:ItemEdit)")

	;; Function for deleting items of Dictionary
	(defun DE:ItemDel	(/ dnum dict inum item func)
		(if
			(and (setq dnum (get_tile "dicts"))
					 (/= dnum "")
					 (setq dict (nth (atoi dnum) dictlst))
					 (setq inum (get_tile "items"))
					 (/= inum "")
					 (setq item (nth (atoi inum) (cddr dict)))
					 (setq func
									(cdr
										(assoc (cdr (assoc 0 (cdr item))) deletable_dict_items)
									) ;_ end of cdr
					 ) ;_ end of setq
					 (=	1
							(WarnMsg
								(strcat	"You're about to delete "
												(car dict)
												"'s  - item "
												(car item)
												". Are you sure you want to do this?"
								) ;_ end of strcat
							) ;_ end of WarnMsg
					 ) ;_ end of =
					 (apply func (list (cadr dict) item))
			) ;_ end of and
			 (progn
				 (setq dictlst (GetAllDicts))
				 (if (setq dict (nth (atoi dnum) dictlst))
					 (if (setq item (nth (atoi inum) dict))
						 (DE:Disp dnum inum)
						 (DE:Disp dnum "")
					 ) ;_ end of if
					 (DE:Disp "" "")
				 ) ;_ end of if
			 ) ;_ end of progn
		) ;_ end of if
	) ;_ end of defun
	(action_tile "delitem" "(DE:ItemDel)")

	(start_dialog)
	(unload_dialog dcl)
	(command "_UNDO" "_END")
	(setvar "CMDECHO" cmdecho)
	(princ)
) ;_ end of defun

;; Helper function to ask user with warnimg message
(defun WarnMsg (msg / dcl ret)
	(if	(and (setq dcl (load_dialog "DictEdit.DCL"))
					 (new_dialog "YesNoMessage" dcl)
			) ;_ end of and
		(progn
			(set_tile "msg" msg)
			(setq ret (start_dialog))
			(unload_dialog dcl)
		) ;_ end of progn
		(setq ret nil)
	) ;_ end of if
	ret
) ;_ end of defun

;; Helper function to get all dictionary data
(defun GetAllDicts (/ lst dn dd dict name elem item dlst n)
	(setq	dn	 (namedobjdict)
				dd	 (entget dn)
				lst	 nil
				dict (dictnext dn t)
	) ;_ end of setq
	(while dict
		(setq	name
					 (cdr
						 (nth	(1- (vl-position (cons 350 (cdr (assoc -1 dict))) dd))
									dd
						 ) ;_ end of nth
					 ) ;_ end of cdr
		) ;_ end of setq
		(setq item (list dict name))
		(setq elem (dictnext (cdr (assoc -1 dict)) t))
		(while elem
			(cond
				((member (cons 350 (cdr (assoc -1 elem))) dict)
				 (setq name	(cdr (nth	(1-	(vl-position
																		(cons 350 (cdr (assoc -1 elem)))
																		dict
																	) ;_ end of vl-position
															) ;_ end of 1-
															dict
												 ) ;_ end of nth
										) ;_ end of cdr
				 ) ;_ end of setq
				)
				((member (cons 360 (cdr (assoc -1 elem))) dict)
				 (setq name	(cdr (nth	(1-	(vl-position
																		(cons 360 (cdr (assoc -1 elem)))
																		dict
																	) ;_ end of vl-position
															) ;_ end of 1-
															dict
												 ) ;_ end of nth
										) ;_ end of cdr
				 ) ;_ end of setq
				)
				(t (setq name ""))
			) ;_ end of cond
			(setq item (cons (cons name elem) item))
			(setq elem (dictnext (cdr (assoc -1 dict))))
		) ;_ end of while
		(if	(assoc 90 dict)
			(progn
				(setq	dlst (member (assoc 90 dict) dict)
							n		 1
				) ;_ end of setq
				(while (< n (length dlst))
					(if	(= 330 (car (nth n dlst)))
						(progn
							(setq	name (strcat "Item" (itoa n))
										elem (entget (cdr (nth n dlst)))
							) ;_ end of setq
							(setq item (cons (cons name elem) item))
						) ;_ end of progn
					) ;_ end of if
					(setq n (1+ n))
				) ;_ end of while
			) ;_ end of progn
		) ;_ end of if
		(setq lst (cons (reverse item) lst))
		(setq dict (dictnext (namedobjdict)))
	) ;_ end of while
	(reverse lst)
) ;_ end of defun

;; Function for editing DataLink Items
(defun DictItemEdit_DATALINK ($dict $item / dcl)
	(alert "ToDo")
) ;_ end of defun
(setq	editable_dict_items
			 (cons (cons "DATALINK" 'DictItemEdit_DATALINK)
						 editable_dict_items
			 ) ;_ end of cons
) ;_ end of setq

;; Function for deleting DataLink Items
(defun DictItemDelete_DATALINK ($dict $item / n lst en)
	(setq n (cdr (assoc 94 (cdr $item))))
	(setq en (cdr (assoc -1 (cdr $item))))
	(if
		(or
			(= n 1)
			(and
				(= n 2)
				(=
					1
					(WarnMsg
						"There's one table linked to this, do you want to delete this?"
					) ;_ end of WarnMsg
				) ;_ end of =
			) ;_ end of and
			(and
				(> n 2)
				(= 1
					 (WarnMsg
						 (strcat
							 "There are "
							 (itoa (1- n))
							 " tables linked to this, do you want to deletete these?"
						 ) ;_ end of strcat
					 ) ;_ end of WarnMsg
				) ;_ end of =
			) ;_ end of and
		) ;_ end of or
		 (progn
			 (setq lst (member (assoc 94 (cdr $item)) (cdr $item)))
			 (while	(> n 0)
				 (entdel (cdr (nth n lst)))
				 (setq n (1- n))
			 ) ;_ end of while
			 (entdel (cdr (assoc 360 (cdr $item))))
			 (entdel (cdr (assoc -1 (cdr $item))))
		 ) ;_ end of progn
		 (if (= n 0)
			 (entdel (cdr (assoc -1 (cdr $item))))
		 ) ;_ end of if
	) ;_ end of if
	(not (entget en))
) ;_ end of defun
(setq	deletable_dict_items
			 (cons (cons "DATALINK" 'DictItemDelete_DATALINK)
						 deletable_dict_items
			 ) ;_ end of cons
) ;_ end of setq



(princ)
 ;|«Visual LISP© Format Options»
(72 2 40 2 T "end of " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
