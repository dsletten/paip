;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               maze.lisp
;;;;
;;;;   Started:            Fri Oct 12 23:50:56 2012
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
(load "/Users/dsletten/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/books/PAIP/ch04/gps2.lisp")

(defpackage :maze (:use :common-lisp :gps2 :lang :test) (:shadowing-import-from :gps2 :debug))

(in-package :maze)

(defun make-maze-ops (pair)
  (destructuring-bind (first second) pair
    (list (make-maze-op first second)
          (make-maze-op second first))))

(defun make-maze-op (here there)
  (op `(move from ,here to ,there)
      :preconds `((at ,here))
      :add-list `((at ,there))
      :del-list `((at ,here))))

(defparameter *maze-ops* (mappend #'make-maze-ops '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
                                                    (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
                                                    (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(defun find-path (start end)
  (let ((result (gps `((at ,start)) `((at ,end)))) )
    (if (null result)
        nil
        (cons start (mapcar #'destination (remove '(start) result :test #'equal)))) ))

(defun destination (action)
  (fifth (second action)))

                       