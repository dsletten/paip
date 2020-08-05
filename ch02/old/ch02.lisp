;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               ch02.lisp
;;;;
;;;;   Started:            Sat Oct 15 00:14:09 2011
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :ch02 (:use :common-lisp :test))

(in-package :ch02)

(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (article) (noun)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun article ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table)))

(defun verb ()
  (one-of '(hit took saw liked)))

(defun one-of (list)
  (list (random-elt list)))

(defun random-elt (list)
  (nth (random (length list)) list))
