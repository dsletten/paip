;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               grammar2.lisp
;;;;
;;;;   Started:            Sat Jun 27 23:11:02 2020
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
;;;;   Notes: Add 0+ adjectives/prepositional phrases
;;;;
;;;;

(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :grammar2 (:use :common-lisp :test))

(in-package :grammar2)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (determiner) (adj*) (noun) (pp*)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun adj* ()
  (if (zerop (random 2))
      '()
      (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      '()))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun determiner ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table)))

;;;
;;;    All transitive!
;;;    
(defun verb ()
  (one-of '(hit took saw liked)))

(defun adj ()
  (one-of '(big little blue green)))

(defun prep ()
  (one-of '(to in by with on)))

(defun one-of (set)
  "Pick one element of a set and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose a random element from a list."
  (elt choices (random (length choices))))
