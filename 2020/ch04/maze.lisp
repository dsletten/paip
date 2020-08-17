;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               maze.lisp
;;;;
;;;;   Started:            Sun Aug 16 22:04:07 2020
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

(in-package :norvig-gps2)

(defun make-maze-ops (pair)
  "Make maze ops (symmetric relation)"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconditions `((at ,here))
      :add-list `((at ,there))
      :delete-list `((at ,here))))

(defparameter *maze-ops*
  (mapcan #'make-maze-ops
          '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
            (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
            (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))


;;;
;;;    Cycles!
;;;
;;(gps '((at 25)) '((at 1)))
;;(gps '((at 25)) '((at 5)))
