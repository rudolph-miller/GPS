(defun executing-p (x)
  (starts-with x 'executing))

(defun starts-with (list x)
  (and (consp list) (eql (car list) x)))

(defun convert-op (op)
  (unless (some #'executing-p (op-add-list op))
	(push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op 
	(make-op :action action :preconds preconds
			 :add-list add-list :del-list del-list)))


(defvar *ops* nil)

(defstruct op
  (action nil)
  (preconds nil)
  (add-list nil)
  (del-list nil))

(defun GPS (state goals &optional (*ops* *ops*))
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

(defun achieve-all (state goals goal-stack)
  (let ((current-state state))
	(if (and (every #'(lambda (g)
						(setf current-state 
							  (achieve current-state g goal-stack)))
					goals)
			 (subsetp goals current-state :test #'equal))
	  current-state)))

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
		((member-equal goal goal-stack) nil)
		(t (some #'(lambda (op) (apply-op state goal op goal-stack))
				 (find-all goal *ops* :test #'appropriate-p)))))

(defun find-all (item sequence &rest keyword-args
					  &key (test #'eql) test-not &allow-other-keys)
  (if test-not
	(apply #'remove item sequence
		   :test-not (complement test-not) keyword-args)
	(apply #'remove item sequence
		   :test  (complement test) keyword-args)))

(defun complement (fn)
  #'(lambda (&rest args) (not (apply fn args))))

(setf (symbol-function 'find-all-if) #'remove-if-not)

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider: ~a" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op)
							 (cons goal goal-stack))))
	(unless (null state2)
	  (dbg-indent :gps (length goal-stack) "Action: ~a" (op-action op))
	  (append (remove-if #'(lambda (x)
							 (member-equal x (op-del-list op)))
						 state2)
			  (op-add-list op)))))

(defun appropriate-p (goal op)
  (member-equal goal (op-add-list op)))

(defun use (oplist)
  (length (setf *ops* oplist)))

(defvar *dbg-ids* nil)

(defun dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids) nil
					(set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
	(fresh-line *debug-io*)
	(dotimes (i indent) (princ " " *debug-io*))
	(apply #'format *debug-io* format-string args)))


(defun GPS (state goals &optional (ops *ops*))
  (let ((old-ops *ops*))
	(setf *ops* ops)
	(let ((result (remove-if #'atom (achieve-all 
									   (cons '(start) state)
									   goals nil))))
	  (setf *ops* old-ops)
	  result)))

(defparameter *school-ops*
  (list
	(make-op :action 'drive-son-to-school
			 :preconds '(son-at-home car-works)
			 :add-list '(son-at-school)
			 :del-list '(son-at-home))
	(make-op :action 'shop-installs-battery
			 :preconds '(car-needs-battery shop-knows-problem shop-has-money)
			 :add-list '(car-works))
	(make-op :action 'tell-shop-problem
			 :preconds '(in-communication-with-shop)
			 :add-list '(shop-knows-problem))
	(make-op :action 'telephone-shop
			 :preconds '(know-phone-number)
			 :add-list '(in-communication-with-shop))
	(make-op :action 'look-up-number
			 :preconds '(have-phone-book)
			 :add-list '(know-phone-number))
	(make-op :action 'give-shop-money
			 :preconds '(have-money)
			 :add-list '(shop-has-money)
			 :del-list '(have-money))))

(push (make-op :action 'ask-phone-number
			   :preconds '(in-communication-with-shop)
			   :add-list '(know-phone-number))
	  *school-ops*)
	 
(mapc #'convert-op *school-ops*)

(defparameter *banana-ops*
  (list 
	(op 'climb-on-chair
		:preconds '(chair-at-middle-room at-middle-room on-floor)
		:add-list '(at-bananas on-chair)
		:del-list '(at-middle-room on-floor))
	(op 'push-chair-from-door-to-middle-room
		:preconds '(chair-at-door at-door)
		:add-list '(chair-at-middle-room at-middle-room)
		:del-list '(chair-at-door at-door))
	(op 'walk-from-door-to-middle-room
		:preconds '(at-door on-foor)
		:add-list '(at-middle-room)
		:del-list '(at-door))
	(op 'grasp-bananas
		:preconds '(at-bananas empty-handed)
		:add-list '(has-bananas)
		:del-list '(empty-handed))
	(op 'drop-ball
		:preconds '(has-ball)
		:add-list '(empty-handed)
		:del-list '(has-ball))
	(op 'eat-bananas
		:preconds '(has-bananas)
		:add-list '(empty-handed not-hungry)
		:del-list '(has-bananas hungry))))


(defun make-maze-ops (pair)
  (list (make-maze-op (car pair) (cadr pair))
		(make-maze-op (cadr pair) (car pair))))

(defun make-maze-op (here there)
  (op `(move from ,here to ,there)
	  :preconds `((at ,here))
	  :add-list `((at ,there))
	  :del-list `((at ,here))))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defparameter *maze-ops*
  (mappend #'make-maze-ops
		   '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13) (12 11) (11 6) (11 16)
				   (16 17) (17 22) (22 21) (22 23) (23 18) (23 24) (24 19) (19 20) (20 25) (20 15) (15 10) (10 5))))
(defun gps (state goals &optional (*ops* *ops*))
  (find-all-if #'action-p
			   (achieve-all (cons '(start) state) goals nil)))

(defun action-p (x)
  (or (equal x '(start)) (executing-p x)))

(defun find-path (start end)
  (let ((results (gps `((at ,start)) `((at ,end)))))
	(unless (null results)
	  (cons start (mapcar #'destination 
						  (remove '(start) results :test #'equal))))))

(defun destination (action)
  (fifth (cadr action)))

(defun make-block-ops (blocks)
  (let ((ops nil))
	(dolist (a blocks)
	  (dolist (b blocks)
		(unless (equal a b)
		  (dolist (c blocks)
			(unless (or (equal c a) (equal c b))
			  (push (move-op a b c) ops)))
		  (push (move-op a 'table b) ops)
		  (push (move-op a b 'table) ops))))
	ops))

(defun move-op (a b c)
  (op `(move ,a from ,b to ,c)
	  :preconds `((space on ,a) (space on ,c) (,a on ,b))
	  :add-list (move-ons a b c)
	  :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eql b 'table)
	`((,a on ,c))
	`((,a on ,c) (space on ,b))))

(defun achieve-all (state goals goal-stack)
  (some #'(lambda (goals) (achieve-each state goals goal-stack))
		(ordering goals)))

(defun achieve-each (state goals goal-stack)
  (let ((current-state state))
	(if (and (every #'(lambda (g)
						(setf current-state 
							  (achieve current-state g goal-stack)))
					goals)
			 (subsetp goals current-state :test #'equal))
	  current-state)))

(defun ordering (l)
  (if (> (length l) 1)
	(list l (reverse l))
	(list l)))

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~a" goal)
  (cond ((member-equal goal state) state)
		((member-equal goal goal-stack) nil)
		(t (some #'(lambda (op) (apply-op state goal op goal-stack))
				 (appropriate-ops goal state)))))

(defun appropriate-ops (goal state)
  (sort (copy-list (find-all goal *ops* :test #'appropriate-p)) #'<
		:key #'(lambda (op)
				 (count-if #'(lambda (precond)
							   (not (member-equal precond state)))
						   (op-preconds op)))))

(setf start '((c on a) (a on table) (b on table) (space on c) (space on b) (space on table)))




			  
