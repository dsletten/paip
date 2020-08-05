;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch01.lisp
;;;;
;;;;   Started:            Fri Sep 30 01:57:40 2011
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
(load "/Users/dsletten/lisp/packages/io.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch01 (:use :common-lisp :io :lang :test))

(in-package :ch01)

;;;
;;;    1.1
;;;
;;    pg. 12
(defun last-name (name)
  (first (last name)))

;;    pg. 13
(defun first-name (name)
  (first name))

(defvar *names* '((john q public)
                  (malcolm x)
                  (admiral grace murray hopper)
                  (spot)
                  (aristotle)
                  (a a milne)
                  (z z top)
                  (sir lawrence olivier)
                  (miss scarlett)
                  (madam major general paula jones)
                  (mr blue jeans)
                  (morton downey jr)
                  (rex morgan md)
                  (jeff abernathy iii)
                  (lee lerner esquire)))

(defparameter *titles* '(mr mrs miss ms sir madam dr admiral brigadier major lieutenant general captain colonel sergeant))

;;    pg. 16
(defun first-name (name)
  (if (member (first name) *titles*)
      (first-name (rest name))
      (first name)))

(defparameter *suffixes* '(md jr ii iii esquire phd))

(defun last-name (name)
  (cond ((endp (rest name)) nil)
        (t (last-reversed-name (reverse name)))) )

(defun last-reversed-name (reversed)
  (if (member (first reversed) *suffixes*)
      (last-reversed-name (rest reversed))
      (first reversed)))

;;;
;;;    CLOS approach
;;;
;;;    This version pushes the "intelligence" into the class definition and the format of the text file of people.
;;;    
(defclass person ()
  ((first-name :reader first-name :initarg :first-name)
   (middle-name :reader middle-name :initarg :middle-name :initform "")
   (last-name :reader last-name :initarg :last-name :initform "")
   (title :reader title :initarg :title :initform "")
   (suffix :reader suffix :initarg :suffix :initform "")))

(defmethod print-object ((p person) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A" (join (remove "" (list (title p) (first-name p) (middle-name p) (last-name p) (suffix p)) :test #'string=) " "))))

(defun parse-record (record)
  (split record #\|))

(defun make-person (name-list)
  (destructuring-bind (title first-name middle-name last-name suffix) name-list
    (make-instance 'person :title title :first-name first-name :middle-name middle-name :last-name last-name :suffix suffix)))

(defun get-people ()
  (mapcar #'(lambda (record) (make-person (parse-record record)))
          (read-file "/Users/dsletten/lisp/books/PAIP/ch01/people.txt")))

;;;
;;;    1.2
;;;
(defun power-1 (m n)
  (if (zerop n)
      1
      (* m (power-1 m (1- n)))) )

(defun power-2 (m n)
  (cond ((zerop n) 1)
        ((= n 2) (* m m))
        ((evenp n) (power-2 (power-2 m (/ n 2)) 2))
        (t (* m (power-2 m (1- n)))) ))

(defun power (m n)
  (cond ((zerop n) 1)
        ((evenp n) (square (power m (/ n 2))))
        (t (* m (power m (1- n)))) ))

(defun square (m)
  (* m m))

;;;
;;;    1.3
;;;
;;
;;    This doesn't really work. Compare 2012 version.
;;    
(defun count-atoms (tree)
  (if (atom tree)
      1
      (+ (count-atoms (car tree))
         (count-atoms (cdr tree)))) )

(defun count-atoms (tree)
  (cond ((null tree) 0)
        ((atom tree) 1)
        (t (+ (count-atoms (car tree))
              (count-atoms (cdr tree)))) ))

(deftest test-count-atoms ()
  (check
   (= (count-atoms '(a b c)) 3)
   (= (count-atoms '(a (b) c)) 3)
   (= (count-atoms 'a) 1)
   (= (count-atoms "foo") 1)
   (= (count-atoms '()) 0)
   (= (count-atoms '(a . b)) 2)))

;;;
;;;    1.4
;;;
(defun count-anywhere (obj tree)
  (cond ((eql obj tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere obj (car tree))
              (count-anywhere obj (cdr tree)))) ))

(deftest test-count-anywhere ()
  (check
   (= (count-anywhere 'a 'a) 1)
   (= (count-anywhere 'a 'b) 0)
   (= (count-anywhere 'a '(a)) 1)
   (= (count-anywhere 'a '(a b c)) 1)
   (= (count-anywhere 'a '(a ((a) b) a)) 3)
   (= (count-anywhere 'a '(a b a c a d)) 3)
   (= (count-anywhere 'a '((a) a (a (b (a)) a) ((c)))) 5)))

;;;
;;;    1.5
;;;
(defun dot-product (u v)
  (cond ((endp u) (if (endp v)
                      0
                      (error "Length mismatch.")))
        ((endp v) (error "Length mismatch."))
        (t (+ (* (first u) (first v)) (dot-product (rest u) (rest v)))) ))

(defun dot-product (u v)
  (loop for ui on u
        for vi on v
        sum (* (first ui) (first vi)) into total
        finally (if (and (endp ui) (endp vi)) (return total) (error "Length mismatch."))))

(defun dot-product (u v)
  (reduce #'+ (mapcar #'* u v)))

(defun dot-product (u v)
  (reduce #'+ (map 'list #'* u v)))

(deftest test-dot-product ()
  (check
   (= (dot-product '(10 20) '(3 4)) 110)
   (= (dot-product (vector 10 20) (vector 3 4)) 110)
   (= (dot-product (vector 10 20) '(3 4)) 110)
   (= (dot-product (vector 1 3 -5) (vector 4 -2 -1)) 3)
   (= (dot-product '(10 20) (vector 3 4)) 110)))


