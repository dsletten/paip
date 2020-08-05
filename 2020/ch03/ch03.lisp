;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sat Jul 25 22:54:17 2020
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch03 (:use :common-lisp :test) (:shadow :length))

(in-package :ch03)

;;;
;;;    3.3
;;;    
(defun dot-print (obj)
  (cond ((atom obj) (princ obj))
        (t (princ "(")
           (dot-print (car obj))
           (princ " . ")
           (dot-print (cdr obj))
           (princ ")")
           obj)))

;;;
;;;    3.4
;;;
(defun normal-print (obj)
  (labels ((cons-print (obj)
             (princ "(")
             (print-chain obj)
             (princ ")")
             obj)
           (print-chain (obj)
             (normal-print (car obj))
             (cond ((null (cdr obj)))
                   ((atom (cdr obj))
                    (princ " . ")
                    (normal-print (cdr obj)))
                   (t (princ " ") (print-chain (cdr obj)))) ))
    (if (atom obj)
        (princ obj)
        (cons-print obj))))

;;;
;;;    Norvig 3.3
;;;    His is more complex than mine in this case, but he lays the foundation for 3.4
;;;    
(defun dprint (x)
  (cond ((atom x) (princ x))
        (t (princ "(")
           (dprint (first x))
           (pr-rest (rest x))
           (princ ")")
           x)))

(defun pr-rest (x)
  (princ " . ")
  (dprint x))

;;;
;;;    Norvig 3.4 (Only PR-REST2 is different)
;;;    
(defun dprint2 (x)
  (cond ((atom x) (princ x))
        (t (princ "(")
           (dprint2 (first x))
           (pr-rest2 (rest x))
           (princ ")")
           x)))

(defun pr-rest2 (x)
  (cond ((null x)) ; Do nothing
        ((atom x) (princ " . ") (princ x))
        (t (princ " ") (dprint2 (first x)) (pr-rest2 (rest x)))) )

;;;
;;;    Pg. 101
;;;    
;;;    Norvig's version.
;;;    
(defun find-all (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence :test (complement test) keyword-args)))

;;;
;;;    This is awkward splicing in two values...
;;;    
(defun find-all-a (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (apply #'remove item sequence (append (if test-not 
                                            (list :test-not (complement test-not))
                                            (list :test (complement test)))
                                        keyword-args)))

;;;
;;;    This seemed pretty straightforward, but it's a mess...
;;;    It looked like inverting the keyword arg would be a simple way to invert the sense of the test.
;;;    In the :TEST-NOT case we instead add a :TEST keyword, but this results in REMOVE receiving both :TEST and :TEST-NOT args.
;;;    REMOVE does not seem to inspect a TEST-NOT-SUPPLIED-P variable. If the value is NIL, then it has no impact.
;;;    But otherwise it conflicts:
;;;    (remove 1 '(1 2 3 2 1) :test #'= :test-not nil) => (2 3 2)  OK
;;;    (remove 1 '(1 2 3 2 1) :test #'= :test-not #'=) => (1 1)    Wrong
;;;
;;;    The &REST parameter can be treated as a p-list of the keyword args (using REMF to eliminate the conflicting property).
;;;    But this is brittle as a stray arg in KEYWORD-ARGS shifts all of the even/odd elements!
;;;    
(defun find-all-b (item sequence &rest keyword-args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (progn (remf keyword-args :test-not)
             (apply #'remove item sequence :test test-not keyword-args))
      (apply #'remove item sequence :test-not test keyword-args)))

(deftest test-find-all ()
  (check
   (equal (find-all 1 '(1 2 3 2 1) :test #'=) '(1 1))
   (equal (find-all 1 '(1 2 3 2 -1) :test #'= :key #'abs) '(1 -1))
   (equal (find-all 1 '(1 2 3 2 1) :test-not #'=) '(2 3 2))))

(deftest test-find-all-a ()
  (check
   (equal (find-all-a 1 '(1 2 3 2 1) :test #'=) '(1 1))
   (equal (find-all-a 1 '(1 2 3 2 -1) :test #'= :key #'abs) '(1 -1))
   (equal (find-all-a 1 '(1 2 3 2 1) :test-not #'=) '(2 3 2))))

(deftest test-find-all-b ()
  (check
   (equal (find-all-b 1 '(1 2 3 2 1) :test #'=) '(1 1))
   (equal (find-all-b 1 '(1 2 3 2 -1) :test #'= :key #'abs) '(1 -1))
   (equal (find-all-b 1 '(1 2 3 2 1) :test-not #'=) '(2 3 2))))

;;;
;;;    3.9
;;;
(defun length (l)
  (reduce #'(lambda (counter elt) (declare (ignore elt)) (1+ counter)) l :initial-value 0))

;;;
;;;    Norvig
;;;    
(defun length (l)
  (reduce #'+ l :key (constantly 1)))

(deftest test-length ()
  (check
   (= (length '()) 0)
   (= (length '(a)) 1)
   (= (length '(a b)) 2)
   (= (length '(a b c d)) 4)))

