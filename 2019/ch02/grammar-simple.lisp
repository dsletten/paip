;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               grammar-simple.lisp
;;;;
;;;;   Started:            Thu Jun 25 03:04:36 2020
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
;;;;   Notes: The first, simplest version
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")

(defpackage :grammar-simple (:use :common-lisp :test))

(in-package :grammar-simple)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (determiner) (noun)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun determiner ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table)))

;;;
;;;    All transitive!
;;;    
(defun verb ()
  (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of a set and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose a random element from a list."
  (elt choices (random (length choices))))
