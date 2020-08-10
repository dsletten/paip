;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               norvig-gps2.lisp
;;;;
;;;;   Started:            Thu Aug  6 02:41:54 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/books/PAIP/debug.lisp")

(defpackage :norvig-gps2 (:use :common-lisp :test :debug) (:shadowing-import-from :debug :debug))

(in-package :norvig-gps2)

;;;
;;;    Rig the existing operator structures to meet new (EXECUTING ...) pattern.
;;;    
(defun executingp (expr)
  "Is EXPR of the form: (EXECUTING ...)?"
  (starts-with-p expr 'executing))

(defun starts-with-p (list expr)
  "Is this a list whose first element is EXPR?"
  (and (consp list) (eql (first list) expr)))

(defun convert-op (op)
  "Make OP conform to the (EXECUTING OP) convention."
  (unless (some #'executingp (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconditions add-list delete-list)
  "Make a new operator that obeys the (EXECUTING OP) convention."
  (convert-op
   (make-op :action action :preconditions preconditions :add-list add-list :delete-list delete-list)))

;;;
;;;    Do this!
;;;    
;(mapc #'convert-op *school-ops*)

(defvar *ops* '() "A list of available operators.") ; _Current_ operators???

(defstruct op "An operation"
  (action nil)
  (preconditions nil)
  (add-list nil)
  (delete-list nil))

(defun gps (state goals &optional (*ops* *ops*))
  "General Problem Solver 2: from STATE achieve GOALS using *OPS*"
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil))) ; ?

(defun achieve-all (state goals goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (let ((current-state state))
    (if (and (every #'(lambda (goal)
                        (setf current-state (achieve current-state goal goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state
        nil)))
;        (list 'huh))))
;        'huh)))

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~A" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil) ; Cycle!
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriatep)))) )

(defun member-equal (item list)
  (member item list :test #'equal))

;;;
;;;    ch. 3
;;;    
(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun appropriatep (goal op)
  "An op is appropriate to a goal if it is in the add-list of the goal."
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~A" (op-action op))
  (let ((state2 (achieve-all state (op-preconditions op) (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~A" (op-action op))
      (append (remove-if #'(lambda (x)
                             (member-equal x (op-delete-list op)))
                         state2)
              (op-add-list op)))) )

(defun use (oplist)
  "Use oplist as the default list of operators."
  (length (setf *ops* oplist)))

(defparameter *school-ops* (list (make-op :action 'drive-son-to-school
                                          :preconditions '(son-at-home car-works)
                                          :add-list '(son-at-school)
                                          :delete-list '(son-at-home))
                                 (make-op :action 'shop-installs-battery
                                          :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                          :add-list '(car-works))
                                 (make-op :action 'tell-shop-problem
                                          :preconditions '(in-communication-with-shop)
                                          :add-list '(shop-knows-problem))
                                 (make-op :action 'telephone-shop
                                          :preconditions '(know-phone-number)
                                          :add-list '(in-communication-with-shop))
                                 (make-op :action 'look-up-number
                                          :preconditions '(have-phone-book)
                                          :add-list '(know-phone-number))
                                 (make-op :action 'give-shop-money
                                          :preconditions '(have-money)
                                          :add-list '(shop-has-money)
                                          :delete-list '(have-money))))
            
;; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school))
;; (gps '(son-at-home car-works) '(son-at-school))
;; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(have-money son-at-school))
;; (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school have-money))
;; (gps '(son-at-home car-needs-battery have-money) '(son-at-school))
;; (gps '(son-at-home) '(son-at-home))
