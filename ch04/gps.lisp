;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               gps.lisp
;;;;
;;;;   Started:            Wed Aug 29 16:36:34 2012
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

(defpackage :gps (:use :common-lisp :test))

(in-package :gps)

;;;
;;;    Pg. 101
;;;    
(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) args)
      (apply #'remove item sequence :test (complement test) args)))

(defvar *state* '())
(defvar *operators* '())

(defstruct operator
  (action nil)
  (preconditions '())
  (add-list '())
  (delete-list '()))

(defun gps (*state* goals *operators*)
;; (defun gps (state goals ops)
  (if (every #'achieve goals)
      'solved
      nil))

(defun achieve (goal)
  (or (member goal *state*)
      (some #'apply-op (find-all goal *operators* :test #'appropriatep))))

(defun appropriatep (goal op)
  (member goal (operator-add-list op)))

(defun apply-op (op)
  (cond ((every #'achieve (operator-preconditions op)) ; Should be recursive call to gps?
         (format t "Executing: ~A~%" (operator-action op))
         (setf *state* (set-difference *state* (operator-delete-list op)))
         (setf *state* (union *state* (operator-add-list op)))
         t)
        (t nil)))

(defparameter *school-ops* (list (make-operator :action 'drive-son-to-school
                                                :preconditions '(son-at-home car-works)
                                                :add-list '(son-at-school)
                                                :delete-list '(son-at-home))
                                 (make-operator :action 'shop-installs-battery
                                                :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                                :add-list '(car-works))
                                 (make-operator :action 'tell-shop-problem
                                                :preconditions '(in-communication-with-shop)
                                                :add-list '(shop-knows-problem))
                                 (make-operator :action 'telephone-shop
                                                :preconditions '(know-phone-number)
                                                :add-list '(in-communication-with-shop))
                                 (make-operator :action 'look-up-number
                                                :preconditions '(have-phone-book)
                                                :add-list '(know-phone-number))
                                 (make-operator :action 'give-shop-money
                                                :preconditions '(have-money)
                                                :add-list '(shop-has-money)
                                                :delete-list '(have-money))))

