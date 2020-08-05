;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch03.lisp
;;;;
;;;;   Started:            Sat Aug 11 02:29:07 2012
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

(defpackage :ch03
  (:use :common-lisp :test)
  (:shadow :length :print))

(in-package :ch03)

;;;
;;;    3.3
;;;
(defun dot-print (exp)
  (if (atom exp)
      (princ exp)
      (progn
        (princ "(")
        (dot-print (car exp))
        (princ " . ")
        (dot-print (cdr exp))
        (princ ")")
        exp)))

(defun dot-print (exp)
  (cond ((atom exp) (princ exp))
        (t (princ "(")
           (dot-print (car exp))
           (princ " . ")
           (dot-print (cdr exp))
           (princ ")")
           exp)))

;;;
;;;    3.4
;;;
;; (defun print (exp)
;;   (if (atom exp)
;;       (princ exp)
;;       (progn
;;         (princ "(")
;;         (print (car exp))
;;         (cond ((null (cdr exp)) (princ ")"))
;;               ((atom (cdr exp)) (princ " . ") (print (cdr exp)))
;;               (t (princ " ") (print (cdr exp)))) )))

(defun print (exp)
  (if (atom exp)
      (princ exp)
      (progn
        (princ "(")
        (print (car exp))
        (cdr-print (cdr exp))
        exp)))

(defun print (exp)
  (cond ((atom exp) (princ exp))
        (t (princ "(")
           (print (car exp))
           (cdr-print (cdr exp))
           exp)))

(defun cdr-print (exp)
  (cond ((null exp) (princ ")"))
        ((atom exp) (princ " . ") (print exp) (princ ")"))
        (t (princ " ") (print (car exp)) (cdr-print (cdr exp)))))

;;;
;;;    See Touretzky ch. 9
;;;
(defun print (obj)
  (labels ((car-print (obj)
             (cond ((atom obj) (princ obj))
                   (t (princ "(")
                      (car-print (car obj))
                      (cdr-print (cdr obj))
                      obj)))
           (cdr-print (obj)
             (cond ((null obj) (princ ")"))
                   ((atom obj) (princ " . ") (princ obj) (princ ")"))
                   (t (princ " ") (car-print (car obj)) (cdr-print (cdr obj)))) ))
    (car-print obj)))

(defun print (obj)
  (labels ((car-print (obj)
             (cond ((atom obj) (princ obj))
                   (t (cons-print "(" obj))))
           (cdr-print (obj)
             (cond ((atom obj)
                    (unless (null obj)
                      (princ " . ")
                      (princ obj))
                    (princ ")"))
                   (t (cons-print " " obj))))
           (cons-print (s cons)
             (princ s)
             (car-print (car cons))
             (cdr-print (cdr cons))))
    (car-print obj)
    obj))

;;;
;;;    Pg. 101
;;;    
(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence :test-not (complement test-not) args)
      (apply #'remove item sequence :test (complement test) args)))

(deftest test-find-all ()
  (check
   (equal (find-all 1 '(1 2 3 2 1)) '(1 1))
   (equal (find-all 1 '(1 2 3 2 1) :test #'=) '(1 1))
   (equal (find-all 1 '(1 2 3 2 1) :test-not #'=) '(2 3 2))))

;;;
;;;    Bleah...
;;;
;;;    REMOVE-IF and REMOVE-IF-NOT don't take :TEST or :TEST-NOT keys!
;;;    
(defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (remf args :test)
  (remf args :test-not)
  (if test-not
      (apply #'remove-if #'(lambda (elt) (funcall test-not item elt)) sequence args)
      (apply #'remove-if-not #'(lambda (elt) (funcall test item elt)) sequence args)))

;;;
;;;    Nonsense...
;;;    
;; (defun find-all (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
;;   (if test-not
;;       (apply #'remove item sequence :test test-not args)
;;       (apply #'remove item sequence :test-not test args)))

;;;
;;;    Ex. 3.8
;;;
(defun find-all-kcl (item sequence &rest args &key (test #'eql) test-not &allow-other-keys)
  (if test-not
      (apply #'remove item sequence (append args (list :test-not (complement test-not))))
      (apply #'remove item sequence (append args (list :test (complement test)))) ))

;;;
;;;    This doesn't really test since SBCL isn't broken!
;;;    
(deftest test-find-all-kcl ()
  (check
   (equal (find-all-kcl 1 '(1 2 3 2 1)) '(1 1))
   (equal (find-all-kcl 1 '(1 2 3 2 1) :test #'=) '(1 1))
   (equal (find-all-kcl 1 '(1 2 3 2 1) :test-not #'=) '(2 3 2))))

;;;
;;;    Ex. 3.9
;;;
;;;    This handles '() but does not handle the first application of the function, i.e., computing
;;;    the initial value of SUM. I just got lucky in my tests at first since the initial element was
;;;    1 in every case!!
;;;    
;; (defun length (seq)
;;   (reduce #'(lambda (&optional sum elt) (declare (ignore elt)) (if (null sum) 0 (1+ sum))) seq))

(defun length (seq)
  (reduce #'(lambda (sum elt) (declare (ignore elt)) (1+ sum)) seq :initial-value 0))

(deftest test-length ()
  (check
   (= (length '(1 2 3)) 3)
   (= (length #[1 10]) 10)
   (= (length #[1 1000]) 1000)
   (= (length '()) 0)
   (= (length "asdfasd") 7)
   (= (length '(2 3 4)) 3)
   (= (length [1 2 3 4]) 4)))

;;;
;;;    Norvig hints at this.
;;;    
(defun length (seq)
  (reduce #'+ seq :key (constantly 1)))

;;;
;;;    3.12
;;;
(defun print-sentence (words)
  (apply #'format t "~:(~A~) ~@{~(~A~)~^ ~}.~%" words))

;;;
;;;    Norvig's
;;;    
(defun print-sentence (words)
  (format t "~@(~{~A~^ ~}.~)~%" words))

;(print-sentence '(the quick brown fox))