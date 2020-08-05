;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Sat Dec 11 15:45:03 2010
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
(load "/Users/dsletten/lisp/packages/test")

(defpackage ch01 (:use common-lisp test))

(in-package ch01)

;;;
;;;    Pg. 19
;;;    
;; (defun mappend (f list)
;;   (apply #'append (mapcar f list)))

(defun mappend (f &rest lists)
  (apply #'append (apply #'mapcar f lists)))

;; (defun mappend (f &rest lists)
;;   (apply #'mapcan f lists))
;;    I.e., (mappend f l1 l2 ... ln) -> (mapcan f l1 l2 ... ln)

;;;
;;;    Pg. 20
;;;    
(defun number-and-negation (obj)
  (if (numberp obj)
      (list obj (- obj))
      nil))

(deftest test-mappend ()
  (check
   (equal (mappend #'number-and-negation '(testing 1 2 3 test))
          (mapcan #'number-and-negation '(testing 1 2 3 test)))
   (equal (mappend #'list '(a b c d) '(1 2 3 4))
          (mapcan #'list '(a b c d) '(1 2 3 4)))
   (equal (mappend #'list '(a b c d) '(1 2 3))
          (mapcan #'list '(a b c d) '(1 2 3)))
   (equal (mappend #'list '(a b c) '(1 2 3 4))
          (mapcan #'list '(a b c) '(1 2 3 4)))) )

;; (defun mappend-r (f list)
;;   (if (endp list)
;;       '()
;;       (append (funcall f (first list))
;;               (mappend-r f (rest list)))) )

;;;
;;;    See pg. 280 of AMOP!
;;;    
(defun mappend-r (f &rest lists)
  (if (some #'endp lists)
      '()
      (append (apply f (mapcar #'first lists))
              (apply #'mappend-r f (mapcar #'rest lists)))) )

;;;
;;;    Note:
;;;    1. The outer function has a &REST parameter which bundles possibly multiple list args.
;;;       However, the auxiliary function treats this list of lists as a required parameter.
;;;       Thus, recursive calls don't require APPLY to unpack the list of lists as above.
;;;    2. The order of appending is opposite here compared to above. In the non-tail-recursive
;;;       version above, the current value is appended to the result of evaluating the rest
;;;       of the args. But here RESULT already contains all of the previously computed values
;;;       from earlier in the lists.
;;;       
(defun mappend-r (f &rest lists)
  (labels ((mappend-aux (lists result)
             (if (empty-input-list-p lists)
                 result
                 (mappend-aux (rests lists)
                              (append result (apply f (firsts lists)))) )))
    (mappend-aux lists '())))

(defun empty-input-list-p (lists)
  (cond ((endp lists) nil)
        ((null (first lists)) t)
        (t (empty-input-list-p (rest lists)))) )

(deftest test-empty-input-list-p ()
  (check
   (empty-input-list-p '((a b c) (1 2) () (5 6 8 9)))
   (not (empty-input-list-p '((a) (1) (2) (d)))) ))

(defun rests (lists)
  (if (endp lists)
      '()
      (cons (rest (first lists)) (rests (rest lists)))) )

(deftest test-rests ()
  (check
   (equal (rests '((a b c) (1 2 3) (:r :s :t))) '((b c) (2 3) (:s :t)))) )

(defun firsts (lists)
  (if (endp lists)
      '()
      (cons (first (first lists)) (firsts (rest lists)))) )

(deftest test-firsts ()
  (check
   (equal (firsts '((a b c) (1 2 3) (:r :s :t))) '(a 1 :r))))

(deftest test-mappend-r ()
  (check
   (equal (mappend-r #'number-and-negation '(testing 1 2 3 test))
          (mapcan #'number-and-negation '(testing 1 2 3 test)))
   (equal (mappend-r #'list '(a b c d) '(1 2 3 4))
          (mapcan #'list '(a b c d) '(1 2 3 4)))
   (equal (mappend-r #'list '(a b c d) '(1 2 3))
          (mapcan #'list '(a b c d) '(1 2 3)))
   (equal (mappend-r #'list '(a b c) '(1 2 3 4))
          (mapcan #'list '(a b c) '(1 2 3 4)))) )

;;;
;;;    1.1
;;;
(defconstant suffixes '(jr md phd II III esquire))
(defun last-name (name)
  (let ((reversed-name (reverse name)))
    (if (member (first reversed-name) suffixes)
        (second reversed-name)
        (first reversed-name))))

(deftest test-last-name ()
  (check
   (equal (last-name '(rex morgan md)) 'morgan)
   (equal (last-name '(morton downey jr)) 'downey)
   (equal (last-name '(pheenix alma)) 'alma)
   (equal (last-name '(peter norvig phd)) 'norvig)))

;;;
;;;    1.2
;;;
(defun power (m n)
  (if (zerop n)
      1
      (* m (power m (1- n)))) )

(defun power (m n)
  (cond ((zerop n) 1)
        ((= n 2) (* m m))
        ((evenp n) (power (power m (/ n 2)) 2))
        (t (* m (power m (1- n)))) ))

(deftest test-power ()
  (check
   (= (power 3 2) (expt 3 2))
   (= (power 4 9) (expt 4 9))
   (= (power 9 0) (expt 9 0))))

;;;
;;;    1.3
;;;
(defun count-atoms (obj)
  (cond ((null obj) 0)
        ((atom obj) 1)
        (t (+ (count-atoms (car obj))
              (count-atoms (cdr obj)))) ))

(deftest test-count-atoms ()
  (check
   (= (count-atoms '(a (b) c)) 3)
   (= (count-atoms '(a () c)) 2)
   (= (count-atoms '(a . ((b . c) . (d . e)))) 5)))

;;;
;;;    1.4
;;;
(defun count-anywhere (o1 o2)
  (cond ((equal o1 o2) 1)
        ((atom o2) 0)
        (t (+ (count-anywhere o1 (car o2))
              (count-anywhere o1 (cdr o2)))) ))

(deftest test-count-anywhere ()
  (check
   (= (count-anywhere 'a '(a ((a) b) a)) 3)
   (= (count-anywhere '(a b) '(a b)) 1)
   (= (count-anywhere '(a b) '((a b) (a b) (c (a b)))) 3)
   (= (count-anywhere 'x '(1 2 3 4)) 0)))

;;;
;;;    1.5
;;;
(defun dot-product (u v)
  (cond ((and (endp u) (endp v)) 0)
        ((or (endp u) (endp v)) (error "Length mismatch."))
        (t (+ (* (first u) (first v)) (dot-product (rest u) (rest v)))) ))

(deftest test-dot-product ()
  (check
   (= (dot-product '() '()) 0)
   (= (dot-product '(10 20) '(3 4)) 110)))

   