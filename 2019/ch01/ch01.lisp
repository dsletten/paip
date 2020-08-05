;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Sat Sep 21 04:18:37 2019
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

(defpackage :ch01 (:use :common-lisp :test) (:shadow :first :last))

(in-package :ch01)

;;;
;;;    1.1
;;;
(defparameter *titles* '("Mr" "Mrs" "Miss" "Ms" "Sir" "Madam" "Dr" "Admiral" "Major" "General" "Captain") "A list of titles that may appear in representation of name.")
(defparameter *suffixes* '("Jr" "Md" "II" "III" "Esquire" "PhD"))
(defparameter *people* '(("John" "Q" "Public")
                         ("Malcolm" "X")
                         ("Admiral" "Grace" "Murray" "Hopper")
                         ("Spot")
                         ("Aristotle")
                         ("Elvis")
                         ("A" "A" "Milne")
                         ("Z" "Z" "Top")
                         ("Sir" "Lawrence" "Olivier")
                         ("Miss" "Scarlett")
                         ("Major" "Tom")
                         ("Captain" "Kangaroo")
                         ("Madam" "Major" "General" "Paula" "Jones")
                         ("Mr" "Blue" "Jeans")
                         ("Morton" "Downey" "Jr")
                         ("Rob" "Robertson" "Jr" "PhD")
                         ("Rex" "Morgan" "Md")
                         ("Jeff" "Abernathy" "III")
                         ("Lee" "Lerner" "Esquire")))

(defun titlep (component)
  (member component *titles* :test #'string-equal))

(defun suffixp (component)
  (member component *suffixes* :test #'string-equal))

(defun last-name (name)
  "Select last name from name represented as list."
  (cl:first (cl:last name)))

(defun last-name (name)
  "Select last name from name represented as list."
  (labels ((ignore-titles (name)
             (cond ((endp name) (error "Something is wrong with this name..."))
                   ((titlep (cl:first name)) (ignore-titles (rest name)))
                   (t name)))
           (ignore-first-name (name)
             (cond ((endp name) (error "Something is wrong with this name..."))
                   (t (rest name))))
           (ignore-middle-name (name)
             (cond ((endp name) (error "Something is wrong with this name..."))
                   (t (rest name)))) )
    (let ((candidate (ignore-first-name (ignore-titles name))))
      (if (or (endp candidate) (suffixp (cl:first candidate)))
          ""
          (let ((candidate-2 (ignore-middle-name candidate)))
            (if (or (endp candidate-2) (suffixp (cl:first candidate-2)))
                (cl:first candidate)
                (cl:first candidate-2)))) )))

(deftest test-last-name ()
  (check
   (equal (last-name '("John" "Q" "Public")) "Public")
   (equal (last-name '("Malcolm" "X")) "X")
   (equal (last-name '("Admiral" "Grace" "Murray" "Hopper")) "Hopper")
   (equal (last-name '("Spot")) "")
   (equal (last-name '("Aristotle")) "")
   (equal (last-name '("A" "A" "Milne")) "Milne")
   (equal (last-name '("Z" "Z" "Top")) "Top")
   (equal (last-name '("Sir" "Lawrence" "Olivier")) "Olivier")
   (equal (last-name '("Miss" "Scarlett")) "")
   (equal (last-name '("Madam" "Major" "General" "Paula" "Jones")) "Jones")
   (equal (last-name '("Mr" "Blue" "Jeans")) "Jeans")
   (equal (last-name '("Morton" "Downey" "Jr")) "Downey")
   (equal (last-name '("Rex" "Morgan" "Md")) "Morgan")
   (equal (last-name '("Jeff" "Abernathy" "III")) "Abernathy")
   (equal (last-name '("Lee" "Lerner" "Esquire")) "Lerner")))

(defun first-name (name)
  "Select first name from name represented as list."
  (cl:first name))

(defun first-name (name)
  "Select first name from name represented as list."
  (cond ((endp name) (error "Something is wrong with this name..."))
        ((titlep (cl:first name)) (first-name (rest name)))
        (t (cl:first name))))

(deftest test-first-name ()
  (check
   (equal (first-name '("John" "Q" "Public")) "John")
   (equal (first-name '("Malcolm" "X")) "Malcolm")
   (equal (first-name '("Admiral" "Grace" "Murray" "Hopper")) "Grace")
   (equal (first-name '("Spot")) "Spot")
   (equal (first-name '("Aristotle")) "Aristotle")
   (equal (first-name '("A" "A" "Milne")) "A")
   (equal (first-name '("Z" "Z" "Top")) "Z")
   (equal (first-name '("Sir" "Lawrence" "Olivier")) "Lawrence")
   (equal (first-name '("Miss" "Scarlett")) "Scarlett")
   (equal (first-name '("Madam" "Major" "General" "Paula" "Jones")) "Paula")
   (equal (first-name '("Mr" "Blue" "Jeans")) "Blue")
   (equal (first-name '("Morton" "Downey" "Jr")) "Morton")
   (equal (first-name '("Rex" "Morgan" "Md")) "Rex")
   (equal (first-name '("Jeff" "Abernathy" "III")) "Jeff")
   (equal (first-name '("Lee" "Lerner" "Esquire")) "Lee")))

(defclass name () ())
(defclass traditional-name (name)
  ((titles :initarg :titles :reader titles :initform '())
   (first :initarg :first :reader first)
   (middle :initarg :middle :reader middle :initform "")
   (last :initarg :last :reader last :initform "")
   (suffixes :initarg :suffixes :reader suffixes :initform '())))

(defmethod print-object ((name traditional-name) stream)
  (print-unreadable-object (name stream :type t)
    (format stream "~S ~S ~S ~S ~S" (titles name) (first name) (middle name) (last name) (suffixes name))))

(defclass single-name (name)
  ((name :initarg :name :reader name)))

(defmethod print-object ((name single-name) stream)
  (print-unreadable-object (name stream :type t)
    (format stream "~S" (name name))))

(defun pad-name (stream string colon-p at-p &rest params)
  (declare (ignore colon-p at-p params))
  (if (string= string "")
      (format stream "")
      (format stream " ~A" string)))

(defgeneric print-name (name stream)
  (:documentation "Print human readable version of name."))
(defmethod print-name ((name single-name) stream)
  (format stream "~A" (name name)))
(defmethod print-name ((name traditional-name) stream)
  (format stream "~{~A ~^~}~A~/ch01:pad-name/~/ch01:pad-name/~{ ~A~^~}" (titles name) (first name) (middle name) (last name) (suffixes name)))

(defun make-name (&rest components)
  (labels ((collect-titles (components)
             (loop for cons on components
                   when (titlep (cl:first cons))
                   collect (cl:first cons) into titles
                   else return (collect-first-name cons titles)))
           (collect-first-name (components titles)
             (cond ((endp components) (error "Malformed name."))
                   ((suffixp (cl:first components)) (error "Malformed name."))
                   (t (collect-middle-name (rest components) titles (cl:first components)))) )
           (collect-middle-name (components titles first)
             (cond ((endp components) (make-instance 'traditional-name :titles titles :first first))
                   ((suffixp (cl:first components)) (make-instance 'traditional-name :titles titles :first first :suffixes components))
                   ((suffixp (second components)) (collect-suffixes (rest components) titles first "" (cl:first components)))
                   ((endp (rest components)) (make-instance 'traditional-name :titles titles :first first :last (cl:first components)))
                   (t (collect-last-name (rest components) titles first (cl:first components)))) )
           (collect-last-name (components titles first middle)
             (cond ((suffixp (cl:first components)) (make-instance 'traditional-name :titles titles :first first :middle middle :suffixes components)) ; ??
                   (t (collect-suffixes (rest components) titles first middle (cl:first components)))) )
           (collect-suffixes (components titles first middle last)
             (make-instance 'traditional-name :titles titles :first first :middle middle :last last :suffixes components)))
  (if (= (length components) 1)
      (make-instance 'single-name :name (cl:first components))
      (collect-titles components))))

;;;
;;;    1.2
;;;
(defun power (x n)
  (cond ((zerop n) 1)
        ((evenp n) (power (* x x) (/ n 2)))
        (t (* x (power x (1- n)))) ))

;;
;;    This does not produce a value identical to EXPT without the coercion to double float.
;;    
(defun power (x n)
  (labels ((pow (x n)
             (cond ((zerop n) 1)
                   ((evenp n) (pow (* x x) (/ n 2)))
                   (t (* x (pow x (1- n)))) )))
    (assert (realp x))
    (assert (typep n '(integer 0)))
    (if (floatp x)
        (float (pow (coerce x 'double-float) n) x)
        (pow x n))))

(deftest test-power ()
  (check
   (= (power 0 8) (expt 0 8))
   (= (power 1 3) (expt 1 3))
   (= (power 2 0) (expt 2 0))
   (= (power 2 1) (expt 2 1))
   (= (power 2 2) (expt 2 2))
   (= (power 2 3) (expt 2 3))
   (= (power 3.4 0) (expt 3.4 0))
   (= (power 3.4 1) (expt 3.4 1))
   (= (power 3.4 2) (expt 3.4 2))
   (= (power 3.4 3) (expt 3.4 3))
   (= (power -4 1) (expt -4 1))
   (= (power -4 2) (expt -4 2))
   (= (power -4 3) (expt -4 3))))

;;;
;;;    1.3
;;;
(defun count-atoms (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (+ (count-atoms (car tree))
              (count-atoms (cdr tree)))) ))

(deftest test-count-atoms ()
  (check
   (= (count-atoms '()) 0)
   (= (count-atoms '(())) 0)
   (= (count-atoms '(a b c)) 3)
   (= (count-atoms '(a b c ())) 3)
   (= (count-atoms '(a (b) c)) 3)
   (= (count-atoms '(a () c)) 2)
   (= (count-atoms '(a (b (c) (d (e ((f)))) ))) 6)
   (= (count-atoms '(a (b () (d (e (()))) ))) 4)))

;;;
;;;    See Lisp notes 93
;;;    
(defun count-all-atoms-1 (tree)
  (cond ((null tree) 0)
        ((atom (car tree)) (1+ (count-all-atoms-1 (cdr tree))))
        (t (+ (count-all-atoms-1 (car tree))
              (count-all-atoms-1 (cdr tree)))) ))

(deftest test-count-all-atoms-1 ()
  (check
   (= (count-all-atoms-1 '()) 0)
   (= (count-all-atoms-1 '(())) 1)
   (= (count-all-atoms-1 '(a b c)) 3)
   (= (count-all-atoms-1 '(a b c ())) 4)
   (= (count-all-atoms-1 '(a (b) c)) 3)
   (= (count-all-atoms-1 '(a () c)) 3)
   (= (count-all-atoms-1 '(a (b (c) (d (e ((f)))) ))) 6)
   (= (count-all-atoms-1 '(a (b () (d (e (()))) ))) 6)))

(defun count-all-atoms-2 (tree)
  (cond ((atom tree) 1)
        ((null (cdr tree)) (count-all-atoms-2 (car tree)))
        (t (+ (count-all-atoms-2 (car tree))
              (count-all-atoms-2 (cdr tree)))) ))

(deftest test-count-all-atoms-2 ()
  (check
   (= (count-all-atoms-2 '()) 1)
   (= (count-all-atoms-2 '(())) 1)
   (= (count-all-atoms-2 '(a b c)) 3)
   (= (count-all-atoms-2 '(a b c ())) 4)
   (= (count-all-atoms-2 '(a (b) c)) 3)
   (= (count-all-atoms-2 '(a () c)) 3)
   (= (count-all-atoms-2 '(a (b (c) (d (e ((f)))) ))) 6)
   (= (count-all-atoms-2 '(a (b () (d (e (()))) ))) 6)))

(defun count-all-atoms-norvig (tree &optional (if-null 1))
  (cond ((null tree) if-null)
        ((atom tree) 1)
        (t (+ (count-all-atoms-norvig (car tree) 1)
              (count-all-atoms-norvig (cdr tree) 0)))) )

(deftest test-count-all-atoms-norvig ()
  (check
   (= (count-all-atoms-norvig '()) 1)
   (= (count-all-atoms-norvig '(())) 1)
   (= (count-all-atoms-norvig '(a b c)) 3)
   (= (count-all-atoms-norvig '(a b c ())) 4)
   (= (count-all-atoms-norvig '(a (b) c)) 3)
   (= (count-all-atoms-norvig '(a () c)) 3)
   (= (count-all-atoms-norvig '(a (b (c) (d (e ((f)))) ))) 6)
   (= (count-all-atoms-norvig '(a (b () (d (e (()))) ))) 6)))

;;;
;;;    1.4
;;;
(defun count-anywhere (obj tree)
  (cond ((null tree) 0)
        ((consp tree) (+ (count-anywhere obj (car tree))
                         (count-anywhere obj (cdr tree))))
        ((eq obj tree) 1)
        (t 0)))

;;
;;    Norvig's is cleaner.
;;    
(defun count-anywhere (obj tree)
  (cond ((eq obj tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere obj (car tree))
              (count-anywhere obj (cdr tree)))) ))

(deftest test-count-anywhere ()
  (check
   (= (count-anywhere 'a '(b b b)) 0)
   (= (count-anywhere 'a '(a b c)) 1)
   (= (count-anywhere 'a '(a ((a) b) a)) 3)))

;;;
;;;    1.5
;;;
(defun dot-product (u v)
  (reduce #'+ (mapcar #'* u v)))

(defun dot-product (u v)
  (labels ((sum-squares (u v result)
             (cond ((or (endp u) (endp v)) result)
                   (t (sum-squares (rest u) (rest v) (+ result (* (cl:first u) (cl:first v)))) ))))
    (sum-squares u v 0)))

(defun dot-product (u v)
  (loop for ui in u
        for vi in v
        summing (* ui vi)))

(deftest test-dot-product ()
  (check
   (= (dot-product '(10 20) '(3 4)) 110)
   (= (dot-product '(10 20 8) '(3 4)) 110)
   (= (dot-product '(10 20) '(3 4 6)) 110)
   (= (dot-product '(1 3 -5) '(4 -2 -1)) 3)))

(defun dot-product-v (u v)
  (reduce #'+ (map 'vector #'* u v)))

(defun dot-product-v (u v)
  (loop for ui across u
        for vi across v
        summing (* ui vi)))

(defun dot-product-v (u v)
  (labels ((sum-squares (i n result)
             (cond ((= i n) result)
                   (t (sum-squares (1+ i) n (+ result (* (aref u i) (aref v i)))) ))))
    (sum-squares 0 (min (length u) (length v)) 0)))

(deftest test-dot-product-v ()
  (check
   (= (dot-product-v #(10 20) #(3 4)) 110)
   (= (dot-product-v #(10 20 8) #(3 4)) 110)
   (= (dot-product-v #(10 20) #(3 4 6)) 110)
   (= (dot-product-v #(1 3 -5) #(4 -2 -1)) 3)))

