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

(defpackage :norvig-gps2 (:use :common-lisp :test))

(in-package :norvig-gps2)

(defvar *debug-ids* '() "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *debug-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *debug-ids* (union ids *debug-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *debug-ids* (if (null ids)
                        '()
                        (set-difference *debug-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *debug-ids*)
    (fresh-line *debug-io*)
    ;; (dotimes (i indent)
    ;;   (princ "  " *debug-io*))
    (format *debug-io* "~VT" (* indent 2))
    (apply #'format *debug-io* format-string args)))

(defun executingp (expr)
  "Is EXPR of the form: (EXECUTING ...)?"
  (starts-with expr 'executing))

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

;(mapc #'convert-op *school-ops*)

(defvar *ops* '() "A list of available operators.") ; _Current_ operators???

(defstruct op "An operation"
  (action nil)
  (preconditions nil)
  (add-list nil)
  (delete-list nil))

(defun gps (state goals &optional (*ops* *ops*))
  "General Problem Solver: from STATE achieve GOALS using *OPS*"
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil))) ; ?

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op (find-all goal *ops* :test #'appropriatep))))

;;;
;;;    ch. 3
;;;    
(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun appropriatep (goal op)
  (member goal (op-add-list op)))

(defun apply-op (op)
  (cond ((every #'achieve (op-preconditions op))
         (print `(executing ,(op-action op)))
         ;; (setf *state* (set-difference *state* (op-delete-list op)))
         ;; (setf *state* (union *state* (op-add-list op)))
         (setf *state* (union (set-difference *state* (op-delete-list op)) (op-add-list op)))
         t)
        (t nil)))

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
            
