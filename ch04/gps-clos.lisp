;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               gps-clos.lisp
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
;;;;   Notes: STATE can be a _set_ of conditions, however, GOALS must be a list and is, in fact, sensitive
;;;;   to the ordering of goals (particularly in preconditions to operators). For example, using the first example from
;;;;   the book (pg. 118) results in the following sequence of actions:
;;;;   Executing: LOOK-UP-NUMBER
;;;;   Executing: TELEPHONE-SHOP
;;;;   Executing: TELL-SHOP-PROBLEM
;;;;   Executing: GIVE-SHOP-MONEY
;;;;   Executing: SHOP-INSTALLS-BATTERY
;;;;   Executing: DRIVE-SON-TO-SCHOOL
;;;;   
;;;;   But by simply switching the order of preconditions for SHOP-INSTALLS-BATTERY from:
;;;;   (car-needs-battery shop-knows-problem shop-has-money)
;;;;   to
;;;;   (car-needs-battery shop-has-money shop-knows-problem)
;;;;   the "solution" becomes:
;;;;   Executing: GIVE-SHOP-MONEY
;;;;   Executing: LOOK-UP-NUMBER
;;;;   Executing: TELEPHONE-SHOP
;;;;   Executing: TELL-SHOP-PROBLEM
;;;;   Executing: SHOP-INSTALLS-BATTERY
;;;;   Executing: DRIVE-SON-TO-SCHOOL
;;;;
;;;;   It might make sense to add another precondition to GIVE-SHOP-MONEY to ensure that SHOP-KNOWS-PROBLEM.
;;;;   But that causes this solution:
;;;;   Executing: LOOK-UP-NUMBER
;;;;   Executing: TELEPHONE-SHOP
;;;;   Executing: TELL-SHOP-PROBLEM
;;;;   Executing: GIVE-SHOP-MONEY
;;;;   Executing: LOOK-UP-NUMBER
;;;;   Executing: TELEPHONE-SHOP
;;;;   Executing: TELL-SHOP-PROBLEM
;;;;   Executing: SHOP-INSTALLS-BATTERY
;;;;   Executing: DRIVE-SON-TO-SCHOOL

(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/books/PAIP/ch04/operator.lisp")

(defpackage :gps-clos
  (:shadowing-import-from :collections :intersection :set :subsetp :union)
  (:use :common-lisp :collections :operator :test))

(in-package :gps-clos)

;(setf *print-pretty* nil)

;;;
;;;    Pg. 101
;;;    
(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) args)
      (apply #'remove item sequence :test (complement test) args)))

(defun gps (state goals operators)
  (declare (special state operators))
  (let ((indent "|"))
    (declare (special indent))
    (gps-aux goals)))

(defun gps-aux (goals)
  (declare (special state indent))
  (if (every #'achieve goals)
      (values 'solved state)
      nil))

(defun achieve (goal)
  (declare (special state operators indent))
  (format t "~A-> Goal: ~A State: ~A~%" indent goal state)
  (cond ((or (contains state goal)
             (some #'apply-op (possible-solutions goal)))
         (format t "~A-> Goal satisfied~%" indent)
         t)
        (t (format t "~A-> Goal failed~%" indent)
           nil)))

(defun possible-solutions (goal)
  (declare (special operators))
  (remove-if-not #'(lambda (op) (solutionp op goal)) operators))  
;  (find-all goal operators :test #'(lambda (goal op) (solutionp op goal)))) ; Changed signature of SOLUTIONP (APPROPRIATEP)

(defun solutionp (op goal)
  (contains (add-to-state op) goal))

;;;
;;;    If it's possible to achieve all of the preconditions for this operator, then we
;;;    can apply the operator. This changes the state by adding and removing certain conditions.
;;;    
(defun apply-op (op)
  (declare (special state indent))
  (format t "~A-- Attempting action: ~A Preconditions: ~A~%" indent (action op) (preconditions op))
  (let ((indent (concatenate 'string indent "  |")))
    (declare (special indent))
    (cond ((gps-aux (preconditions op))
           (format t "~A Executing: ~A [~A]~%" indent (action op) state)
           (setf state (union (difference state (remove-from-state op)) (add-to-state op)))
           t)
          (t nil))))

(defparameter *school-ops* (list (make-operator 'drive-son-to-school '(son-at-home car-works) #{'son-at-school} #{'son-at-home})
                                 (make-operator 'shop-installs-battery '(car-needs-battery shop-knows-problem shop-has-money) #{'car-works} #{'car-needs-battery 'shop-knows-problem})
                                 (make-operator 'tell-shop-problem '(car-needs-battery in-communication-with-shop) #{'shop-knows-problem} #{'in-communication-with-shop})
                                 (make-operator 'telephone-shop '(know-phone-number) #{'in-communication-with-shop})
                                 (make-operator 'ask-phone-number '(in-communication-with-shop) #{'know-phone-number})
                                 (make-operator 'look-up-number '(have-phone-book) #{'know-phone-number})
                                 (make-operator 'give-shop-money '(have-money shop-knows-problem) #{'shop-has-money} #{'have-money})
                                 (make-operator 'walk-son-to-school '(son-at-home) #{'son-at-school} #{'son-at-home})))

(deftest test-gps ()
  (check
   (gps #{'son-at-home 'car-needs-battery 'have-money 'have-phone-book} '(son-at-school) *school-ops*)
   (gps #{'son-at-home 'car-needs-battery 'have-phone-book} '(son-at-school) *school-ops*)
   (gps #{'car-needs-battery 'have-phone-book} '(shop-knows-problem) *school-ops*)
   (gps #{'son-at-home 'car-needs-battery 'have-money} '(son-at-school) *school-ops*)
;   (not (gps #{'son-at-home 'car-needs-battery 'have-money} '(son-at-school) *school-ops*))
   (not (gps #{'have-phone-book} '(shop-knows-problem) *school-ops*))
   (gps #{'son-at-home 'car-works} '(son-at-school) *school-ops*)))