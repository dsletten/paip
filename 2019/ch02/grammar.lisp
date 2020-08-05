;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               grammar.lisp
;;;;
;;;;   Started:            Tue Oct  8 00:08:16 2019
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
;;;;   Notes: Basically grammar2.lisp with more fancier words...
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :grammar (:use :common-lisp :test))

(in-package :grammar)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

;; (defun noun-phrase ()
;;   (append (determiner) (noun)))

(defun noun-phrase ()
  (append (determiner) (adj*) (noun) (pp*)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun adj* ()
  (if (zerop (random 2))
      '()
      (append (adj) (adj*))))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      '()))

(defun determiner ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table book website banana vector radio)))

;;;
;;;    All transitive!
;;;    
(defun verb ()
  (one-of '(hit took saw liked pardoned invited sold)))

(defun adj ()
  (one-of '(big little blue green wet smelly fabricated inverted simple pleasant fragrant abusive)))

(defun prep ()
  (one-of '(to in by with on under above below along beside within into onto)))

(defun one-of (set)
  "Pick one element of a set and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose a random element from a list."
  (elt choices (random (length choices))))
