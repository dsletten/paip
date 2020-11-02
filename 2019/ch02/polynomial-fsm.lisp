;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               polynomial-fsm.lisp
;;;;
;;;;   Started:            Sun Nov  1 19:28:28 2020
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

(defpackage :polynomial-fsm (:use :common-lisp :test))

(in-package :polynomial-fsm)

(defun infix-to-prefix (tokens)
  "Convert infix list of algebraic tokens into a list of terms in prefix form. 
   The list is interpreted as a sum of terms--no explicit + is present. 
   Terms need not be in any order. No collection of like terms. 
   All implicit powers and coefficients (i.e., 1) are explicit in result."
  (labels ((start (tokens)
             (cond ((null tokens) (fail "Empty token list."))
                   ((numberp (first tokens)) (num0 (rest tokens) (list (first tokens))))
                   ((symbolp (first tokens)) (var (first tokens) 1 (rest tokens) '()))
                   (t (malformed))))
           (fail (msg)
             (error msg))
           (num0 (tokens result)
             (cond ((null tokens) result)
                   (t (case (first tokens)
                        (+ (term0 (rest tokens) result))
                        (- (term0- (rest tokens) result))
                        (* (coeff0 (first result) (rest tokens) (rest result)))
                        (otherwise (malformed)))) ))
           (term0 (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num0 (rest tokens) (cons (first tokens) result)))
                   ((symbolp (first tokens)) (var (first tokens) 1 (rest tokens) result))
                   (t (malformed))))
           (term0- (tokens result)
             "Handle difference of terms -> negative coefficient."
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num0 (rest tokens) (cons (- (first tokens)) result)))
                   ((symbolp (first tokens)) (var (first tokens) -1 (rest tokens) result))
                   (t (malformed))))
           (malformed ()
             (fail "Malformed token list."))
           (coeff0 (coeff tokens result)
             (cond ((null tokens) (malformed))
                   ((symbolp (first tokens)) (var (first tokens) coeff (rest tokens) result))
                   (t (malformed))))
           (var (symbol coeff tokens result)
             (cond ((null tokens) (cons `(* ,coeff (** ,symbol 1)) result))
                   (t (case (first tokens)
                        (+ (term1 (rest tokens) (cons `(* ,coeff (** ,symbol 1)) result)))
                        (- (term1- (rest tokens) (cons `(* ,coeff (** ,symbol 1)) result)))
                        (** (exponent `(* ,coeff ,symbol) (rest tokens) result))
                        (otherwise (malformed)))) ))
           (validate-var (symbol terms)
             (every #'(lambda (term) (cond ((numberp term) t)
                                           ((consp term) (destructuring-bind (times coeff (exp sym pow)) term
                                                           (declare (ignore times coeff exp pow))
                                                           (eq sym symbol)))
                                           (t (malformed))))
                    terms))
           (exponent (term tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) 
                    (destructuring-bind (op coeff sym) term
                      (num1 (rest tokens) (cons `(,op ,coeff (** ,sym ,(first tokens))) result))))
                   (t (malformed))))
           (num1 (tokens result)
             (cond ((null tokens) result)
                   (t (case (first tokens)
                        (+ (term1 (rest tokens) result))
                        (- (term1- (rest tokens) result))
                        (* (coeff1 (first result) (rest tokens) (rest result)))
                        (otherwise (malformed)))) ))
           (coeff1 (coeff tokens result)
             (cond ((null tokens) (malformed))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) coeff (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed))))
           (term1 (tokens result)
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num1 (rest tokens) (cons (first tokens) result)))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) 1 (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed))))
           (term1- (tokens result)
             "Handle difference of terms -> negative coefficient."
             (cond ((null tokens) (malformed))
                   ((numberp (first tokens)) (num1 (rest tokens) (cons (- (first tokens)) result)))
                   ((symbolp (first tokens))
                    (if (validate-var (first tokens) result)
                        (var (first tokens) -1 (rest tokens) result)
                        (fail (format nil "Variable does not match: ~A" (first tokens)))) )
                   (t (malformed)))) )
    (start tokens)))

(deftest test-infix-to-prefix ()
  (check
   (equal (infix-to-prefix '(55)) '(55))
   (equal (infix-to-prefix '(-34 * x)) '((* -34 (** x 1))))
   (equal (infix-to-prefix '(3 * x ** 2 + 7 * x + 8)) '(8 (* 7 (** X 1)) (* 3 (** X 2))))
   (equal (infix-to-prefix '(x ** 3 - 5 * x ** 2 - 17)) '(-17 (* -5 (** X 2)) (* 1 (** X 3))))
   (equal (infix-to-prefix '(x ** 3 - 4 * x ** 2 - 17 + 9 * x ** 12)) '((* 9 (** X 12)) -17 (* -4 (** X 2)) (* 1 (** X 3))))
   (equal (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2)) '((* -4 (** X 2)) (* 9 (** X 1)) (* 1 (** X 2)) 2))
   (equal (infix-to-prefix '(2 + x ** 2 + 9 * x - 4 * x ** 2 + 4 - x)) '((* -1 (** X 1)) 4 (* -4 (** X 2)) (* 9 (** X 1)) (* 1 (** X 2)) 2))))
