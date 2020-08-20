;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               norvig-gps.lisp
;;;;
;;;;   Started:            Wed Jul 29 02:32:54 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")

;(defpackage :norvig-gps (:use :common-lisp :test))
(defpackage :norvig-gps (:use :common-lisp))

(in-package :norvig-gps)

(defvar *state* '())

(defvar *ops*)

(defstruct op
  (action nil)
  (preconditions nil)
  (add-list nil)
  (delete-list nil))

(defun gps (*state* goals *ops*)
  (if (every #'achieve goals)
      'solved
      'failed))

(defun achieve (goal)
;  (or (member goal *state*) ; Must change call to MEMBER for maze domain.
  (or (member goal *state* :test #'equal)
      (some #'apply-op (find-all goal *ops* :test #'appropriatep))))

;;;
;;;    ch. 3
;;;    
(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

(defun appropriatep (goal op)
;  (member goal (op-add-list op)))
  (member goal (op-add-list op) :test #'equal))

(defun apply-op (op)
  (cond ((every #'achieve (op-preconditions op))
         (print `(executing ,(op-action op)))
         (setf *state* (union (set-difference *state* (op-delete-list op)) (op-add-list op)))
         t)
        (t nil)))

(defparameter *school-ops* (list (make-op :action 'drive-son-to-school
                                          :preconditions '(son-at-home car-works)
                                          :add-list '(son-at-school)
                                          :delete-list '(son-at-home))
                                 (make-op :action 'shop-installs-battery
                                          :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                          :add-list '(car-works)
                                          :delete-list '(car-needs-battery)) ; Norvig missed this one!
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
