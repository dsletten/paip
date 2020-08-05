;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Fri Jun 29 02:19:05 2012
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/strings.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch01
  (:use :common-lisp :lang :strings :test)
  (:shadow :expt))

(in-package :ch01)

;;;
;;;    1.1
;;;
(defun last-name (name)
  (first (last name)))

(defun first-name (name)
  (first name))

(defparameter *titles* '(mr mrs miss ms sir madam dr admiral major general))

;; (defun first-name (name)
;;   (if (member (first name) *titles*)
;;       (first-name (rest name))
;;       (first name)))

(defun first-name (name)
  (first (remove-titles name)))

(defparameter *suffixes* '(jr sr ii iii md phd esq))

;; (defun last-name (name)
;;   (cond ((= (length name) 1) nil)
;;         ((member (first (last name)) *suffixes*) (last-name (butlast name)))
;;         (t (first (last name)))) )

(defun remove-titles (name)
  (if (member (first name) *titles*)
      (remove-titles (rest name))
      name))

;; (defun last-name (name)
;;   (cond ((= (length name) 1) nil)
;;         ((member (first (last name)) *suffixes*) (last-name (butlast name)))
;;         (t (first (last name)))) )

(defun remove-suffixes (name)
  (labels ((remove-suffixes-aux (name)
             (if (member (first name) *suffixes*)
                 (remove-suffixes-aux (rest name))
                 (reverse name))))
    (remove-suffixes-aux (reverse name))))

(defun last-name (name)
  (let ((plain-name (remove-suffixes (remove-titles name))))
    (if (= (length plain-name) 1)
        nil
        (first (last plain-name)))) )

(defparameter *test-names* '((john q public) (malcolm x) (admiral grace murray hopper) (spot) (aristotle) (a a milne) (z z top) (sir lawrence olivier) (miss scarlett)))
(deftest test-first-name ()
  (check
   (equal (mapcar #'first-name *test-names*) '(john malcolm grace spot aristotle a z lawrence scarlett))))

(deftest test-last-name ()
  (check
   (equal (mapcar #'last-name *test-names*) '(public x hopper nil nil milne top olivier nil))))

(fmakunbound 'first-name)
(fmakunbound 'last-name)
(defclass name ()
  ((first-name :accessor first-name :initarg :first-name)
   (middle-name :accessor middle-name :initarg :middle-name)
   (last-name :accessor last-name :initarg :last-name)
   (prefixes :accessor prefixes :initarg :prefixes)
   (suffixes :accessor suffixes :initarg :suffixes)))

(defun make-name (&key first middle last prefixes suffixes)
  (make-instance 'name :first-name first :middle-name middle :last-name last :prefixes prefixes :suffixes suffixes))

;;;
;;;    This won't work since the logic regarding where to put spaces requires knowledge of the presence of multiple
;;;    previous fields not just the immediately preceding field.
;;;    
;; (defmethod print-object ((n name) stream)
;;   (print-unreadable-object (n stream :type t)
;;     (format stream "~@[~{~A ~}~]~@[~A~]~@[ ~A~]~@[ ~A~]~@[~{~^ ~A~}~]" (prefixes n) (first-name n) (middle-name n) (last-name n) (suffixes n))))

(defmethod print-object ((n name) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "~A" (join (append (prefixes n) (remove '() (list (first-name n) (middle-name n) (last-name n))) (suffixes n)) " "))))

(defparameter *test-names* (list (make-name :first "John" :middle "Q" :last "Public")
                                 (make-name :first "Malcolm X")
                                 (make-name :prefixes '("Admiral") :first "Grace" :middle "Murray" :last "Hopper")
                                 (make-name :first "Spot")
                                 (make-name :first "Aristotle")
                                 (make-name :first "A" :middle "A" :last "Milne")
                                 (make-name :first "ZZ Top")
                                 (make-name :prefixes '("Sir") :first "Lawrence" :last "Olivier")
                                 (make-name :prefixes '("Miss") :first "Scarlett")
                                 (make-name :prefixes '("Madam" "Major" "General") :first "Paula" :last "Jones")
                                 (make-name :prefixes '("Mr") :first "Blue" :last "Jeans") ; ?!
                                 (make-name :first "Morton" :last "Downey" :suffixes '("Jr"))
                                 (make-name :first "Rex" :last "Morgan" :suffixes '("MD"))
                                 (make-name :first "Jeff" :last "Abernathy" :suffixes '("III"))
                                 (make-name :first "Lee" :last "Lerner" :suffixes '("Esquire"))))

(deftest test-first-name ()
  (check
   (equal (mapcar #'first-name *test-names*) '("John" "Malcolm X" "Grace" "Spot" "Aristotle" "A" "ZZ Top" "Lawrence" "Scarlett" "Paula" "Blue" "Morton" "Rex" "Jeff" "Lee"))))

(deftest test-last-name ()
  (check
   (equal (mapcar #'last-name *test-names*) '("Public" nil "Hopper" nil nil "Milne" nil "Olivier" nil "Jones" "Jeans" "Downey" "Morgan" "Abernathy" "Lerner"))))

;;;
;;;    1.2
;;;
(defun expt (m n)
  (if (zerop n)
      1
      (* m (expt m (1- n)))) )

(deftest test-expt ()
  (check
   (= (expt 2 3) (cl:expt 2 3))
   (= (expt 2 13) (cl:expt 2 13))
   (= (expt 1 1) (cl:expt 1 1))
   (= (expt 1 8) (cl:expt 1 8))
   (= (expt 2 1) (cl:expt 2 1))
   (= (expt 3 4) (cl:expt 3 4))))

(defun expt (m n)
  (case n
    (0 1)
    (otherwise (* m (expt m (1- n)))) ))

(defun expt (m n)
  (cond ((zerop n) 1)
        ((evenp n) (let ((result (expt m (/ n 2))))
                     (* result result)))
        (t (* m (expt m (1- n)))) ))

;;;
;;;    1.3
;;;
(defun count-atoms (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (+ (count-atoms (car tree))
              (count-atoms (cdr tree)))) ))

(defun count-atoms (tree)
  (if (atom tree)
      1
      (reduce #'+ (mapcar #'count-atoms tree))))

;;;
;;;    1.4
;;;
(defun count-anywhere (object tree)
  (cond ((eql tree object) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere object (car tree))
              (count-anywhere object (cdr tree)))) ))

(deftest test-count-anywhere ()
  (check
   (= (count-anywhere 'a '(a ((a) b) a)) 3)
   (= (count-anywhere 'a '(a a a)) 3)
   (= (count-anywhere 'a '(a b a c a)) 3)
   (= (count-anywhere 'a '()) 0)
   (= (count-anywhere 'a '(() (()))) 0)))

;;;
;;;    1.5
;;;
(defun dot-product (l1 l2)
  (cond ((and (endp l1) (endp l2)) 0)
        ((or (endp l1) (endp l2)) (error "Length mismatch!"))
        (t (+ (* (first l1) (first l2)) (dot-product (rest l1) (rest l2)))) ))

(defun dot-product (l1 l2)
  (reduce #'+ (mapcar #'* l1 l2)))

(defun dot-product (l1 l2)
  (cond ((and (endp l1) (endp l2)) 0)
        ((or (endp l1) (endp l2)) (error "Length mismatch!"))
        (t (destructure ((h1 . t1) l1
                         (h2 . t2) l2)
             (+ (* h1 h2) (dot-product t1 t2)))) ))

(deftest test-dot-product ()
  (check
   (= (dot-product '(10 20) '(3 4)) 110)
   (handler-case (progn (dot-product '(10 20) '(3)) nil)
     (error () t))))
