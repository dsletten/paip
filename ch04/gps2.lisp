;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               gps2.lisp
;;;;
;;;;   Started:            Thu Sep 27 23:11:01 2012
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :gps2 (:use :common-lisp :test) (:export :op :gps :use :debug :undebug :start :executing) (:shadow :debug))

(in-package :gps2)

(defun executingp (x)
  (starts-with x 'executing))

(defun starts-with (list x)
  (and (consp list) (eql (first list) x)))

(defun convert-op (op)
  (unless (some #'executingp (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  (convert-op (make-op :action action :preconds preconds :add-list add-list :del-list del-list)))

(defvar *ops* '())

(defstruct op
  (action nil)
  (preconds '())
  (add-list '())
  (del-list '()))

(defun gps (state goals &optional (*ops* *ops*))
  (remove-if-not #'actionp (achieve-all (cons '(start) state) goals '())))

;; (defun gps (state goals &optional (*ops* *ops*))
;;   (remove-if #'atom (achieve-all (cons '(start) state) goals '())))

(defun actionp (x)
  (or (equal x '(start)) (executingp x)))

;;;
;;;    This change allows a retry if ordering of goals in one case leads to "clobbered sibling"
;;;    problem. See pg. 139
;;;    
(defun achieve-all (state goals goal-stack)
  (some #'(lambda (goals) (achieve-each state goals goal-stack)) (orderings goals)))

(defun orderings (l)
  (if (> (length l) 1)
      (list l (reverse l))
      (list l)))

(defun achieve-each (state goals goal-stack)
  (let ((current-state state))
    (if (and (every #'(lambda (g)
                        (setf current-state (achieve current-state g goal-stack)))
                    goals)
             (subsetp goals current-state :test #'equal))
        current-state))) ; Else?!

;; (defun achieve-all (state goals goal-stack)
;;   (let ((current-state state))
;;     (if (and (every #'(lambda (g)
;;                         (setf current-state (achieve current-state g goal-stack)))
;;                     goals)
;;              (subsetp goals current-state :test #'equal))
;;         current-state))) ; Else?!

(defun achieve (state goal goal-stack)
  (dbg-indent :gps (length goal-stack) "Goal: ~A" goal)
  (cond ((member-equal goal state) state)
        ((member-equal goal goal-stack) nil)
        (t (some #'(lambda (op) (apply-op state goal op goal-stack))
                 (find-all goal *ops* :test #'appropriatep)))) )

(defun member-equal (item list)
  (member item list :test #'equal))

(defun apply-op (state goal op goal-stack)
  (dbg-indent :gps (length goal-stack) "Consider: ~A" (op-action op))
  (let ((state2 (achieve-all state (op-preconds op) (cons goal goal-stack))))
    (unless (null state2)
      (dbg-indent :gps (length goal-stack) "Action: ~A" (op-action op))
      (append (remove-if #'(lambda (x) (member-equal x (op-del-list op))) state2)
              (op-add-list op)))) )

(defun appropriatep (goal op)
  (member-equal goal (op-add-list op)))

;;;
;;;    Immediately after calling USE, *OPS* refers to the same list as say *SCHOOL-OPS*.
;;;    But if the program is reloaded, a new list is assigned to *SCHOOL-OPS* by
;;;    DEFPARAMETER. Consequently, mapping CONVERT-OP across *SCHOOL-OPS* then has no
;;;    effect on *OPS*. Must call USE again!
;;;    
(defun use (oplist)
  (length (setf *ops* oplist)))

(defun gps (state goals &optional (ops *ops*))
  (let ((old-ops *ops*))
    (setf *ops* ops)
    (let ((result (remove-if-not #'actionp (achieve-all (cons '(start) state) goals '()))) )
      (setf *ops* old-ops)
      result)))

;; (defun gps (state goals &optional (ops *ops*))
;;   (let ((old-ops *ops*))
;;     (setf *ops* ops)
;;     (let ((result (remove-if #'atom (achieve-all (cons '(start) state) goals '()))) )
;;       (setf *ops* old-ops)
;;       result)))

(defvar *dbg-ids* '())

(defun dbg (id format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)
    (force-output *debug-io*)))

(defun debug (&rest ids)
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  (setf *dbg-ids* (if (null ids) '() (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)
    (force-output *debug-io*)))

;;;
;;;    Pg. 101
;;;    
(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) args)
      (apply #'remove item sequence :test (complement test) args)))
