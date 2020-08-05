;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is the medium of choice for people who enjoy free style and flexibility.
;;;;   -- Gerald Jay Sussman
;;;;
;;;;   Name:               norvig-questions.lisp
;;;;
;;;;   Started:            Tue Jul 28 04:09:40 2020
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

(defpackage :norvig-questions (:use :common-lisp))

(in-package :norvig-questions)

(defstruct node
  name
  (yes nil)
  (no nil))

(defvar *db* (make-node :name 'animal
                        :yes (make-node :name 'mammal)
                        :no (make-node :name 'mineral)))

(defun questions (&optional (node *db*))
  (format *query-io* "Is it a ~A? " (node-name node))
  (force-output *query-io*)
  (case (read)
    ((y yes) (if (not (null (node-yes node)))
                 (questions (node-yes node))
                 (setf (node-yes node) (give-up))))
    ((n no) (if (not (null (node-no node)))
                (questions (node-no node))
                (setf (node-no node) (give-up))))
    (it 'damn-straight)
    (otherwise (format t "Reply with YES, NO, or IT if I have guessed it.~%")
               (questions node))))

(defun give-up ()
  (format *query-io* "I give up - what is it? ")
  (force-output *query-io*)
  (make-node :name (read)))
