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

(defstruct op "An operation"
  (action nil)
  (preconditions nil)
  (add-list nil)
  (delete-list nil))

;;;
;;;    Rig the existing operator structures to meet new (EXECUTING ...) pattern.
;;;    
;;;    These functions are used in isolation to either modify operators defined with MAKE-OP
;;;    or produce operators via OP. None of them is used during execution of GPS itself.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;    GPS 2 proper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *ops* '() "A list of available operators.") ; _Current_ operators???

(defun gps (state goals &optional (*ops* *ops*))
  "General Problem Solver 2: from STATE achieve GOALS using *OPS*"
  (let ((success (achieve-all (cons '(start) state) goals)))
(format t "Success: ~A~%" success)
    (if (null success)
        nil
        (remove-if #'atom success))))

(defun achieve-all (state goals &optional goal-stack)
  "Achieve each goal, and make sure they still hold at the end."
  (labels ((do-achieve-all (state goals)
             (dbg :achieve "Current state: ~A~%" state)
             (dbg :achieve "Goals: ~A~%" goals)
             (if (endp goals)
                 state
                 (let ((new-state (achieve state (first goals) goal-stack)))
                   (if (null new-state)
                       nil
                       (do-achieve-all new-state (rest goals)))) )))
    (let ((new-state (do-achieve-all state goals)))
      (cond ((null new-state) nil)
            ((subsetp goals new-state :test #'equal) new-state)
            (t nil)))) )

(defun achieve (state goal goal-stack)
  "A goal is achieved if it already holds or if there is an appropriate op for it that is applicable."
  (dbg-indent :gps (length goal-stack) "Goal: ~A" goal)
  (cond ((goal-present-p goal state) state)
        ((goal-in-stack-p goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (candidate-ops goal)))) )

(defun goal-present-p (goal state)
  (member-equal goal state))

(defun goal-in-stack-p (goal stack)
  (member-equal goal stack))

(defun member-equal (item list)
  (member item list :test #'equal))

(defun candidate-ops (goal)
  (find-all goal *ops* :test #'appropriatep))

;;;
;;;    ch. 3
;;;    
(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun appropriatep (goal op)
  "An op is appropriate to a goal if the goal is in the add-list of the op."
  (member-equal goal (op-add-list op)))

(defun apply-op (state goal op goal-stack)
  "Return a new, transformed state if op is applicable."
  (dbg-indent :gps (length goal-stack) "Consider: ~A~%" (op-action op))
  (let ((state2 (achieve-all state (op-preconditions op) (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~A~%" (op-action op))
      (update-state state2 (op-delete-list op) (op-add-list op)))) )

(defun update-state (state delete-list add-list)
  (append (apply-delete-list delete-list state) add-list))

(defun apply-delete-list (delete-list state)
  (remove-if #'(lambda (elt) (member-equal elt delete-list)) state))

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

;; * (gps '(at-door on-floor has-ball hungry chair-at-door) '(not-hungry))
;; Success: ((START) (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
;;           CHAIR-AT-MIDDLE-ROOM (EXECUTING CLIMB-ON-CHAIR) AT-BANANAS ON-CHAIR
;;           (EXECUTING DROP-BALL) (EXECUTING GRASP-BANANAS)
;;           (EXECUTING EAT-BANANAS) EMPTY-HANDED NOT-HUNGRY)
;; ((START) (EXECUTING PUSH-CHAIR-FROM-DOOR-TO-MIDDLE-ROOM)
;;  (EXECUTING CLIMB-ON-CHAIR) (EXECUTING DROP-BALL) (EXECUTING GRASP-BANANAS)
;;  (EXECUTING EAT-BANANAS))

;; * (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops*)
;; Success: ((START) CAR-NEEDS-BATTERY HAVE-PHONE-BOOK (EXECUTING LOOK-UP-NUMBER)
;;           KNOW-PHONE-NUMBER (EXECUTING TELEPHONE-SHOP)
;;           IN-COMMUNICATION-WITH-SHOP (EXECUTING TELL-SHOP-PROBLEM)
;;           SHOP-KNOWS-PROBLEM (EXECUTING GIVE-SHOP-MONEY) SHOP-HAS-MONEY
;;           (EXECUTING SHOP-INSTALLS-BATTERY) CAR-WORKS
;;           (EXECUTING DRIVE-SON-TO-SCHOOL) SON-AT-SCHOOL)
;; ((START) (EXECUTING LOOK-UP-NUMBER) (EXECUTING TELEPHONE-SHOP)
;;  (EXECUTING TELL-SHOP-PROBLEM) (EXECUTING GIVE-SHOP-MONEY)
;;  (EXECUTING SHOP-INSTALLS-BATTERY) (EXECUTING DRIVE-SON-TO-SCHOOL))

;;;
;;;    Without applying CONVERT-OP!!
;;;    
;; * (gps '(son-at-home car-needs-battery have-money have-phone-book) '(son-at-school) *school-ops*)
;; Success: ((START) CAR-NEEDS-BATTERY HAVE-PHONE-BOOK KNOW-PHONE-NUMBER
;;           IN-COMMUNICATION-WITH-SHOP SHOP-KNOWS-PROBLEM SHOP-HAS-MONEY
;;           CAR-WORKS SON-AT-SCHOOL)
;; ((START))

(deftest test-candidate-ops ()
  (check
   (eq (op-action (first (candidate-ops 'son-at-school))) 'drive-son-to-school)
   (eq (op-action (first (candidate-ops 'shop-knows-problem))) 'tell-shop-problem)
   (eq (op-action (first (candidate-ops 'car-works))) 'shop-installs-battery)))
