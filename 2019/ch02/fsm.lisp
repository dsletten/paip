;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               fsm.lisp
;;;;
;;;;   Started:            Sun Sep 20 18:41:32 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;      FSM example from Touretzky ch. 14
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

(defpackage :fsm (:use :common-lisp :test))

(in-package :fsm)

(defclass fsm ()
  ((states :reader states :initarg :states)
   (transition :reader transition :initarg :transition)
   (test :reader test :initarg :test)
   (current-node :reader current-node :initarg :start)))

(defun make-fsm (state-list transition &key (test #'eql) (start :start))
  (let ((states (make-hash-table)))
    (dolist (state state-list)
      (destructuring-bind (node input next &optional action) state
      (unless (gethash node states)
        (setf (gethash node states) (make-hash-table)))
      (let ((input-map (gethash node states)))
        (if (gethash input input-map)
            (error "Duplicate rule for ~A" node)
            (setf (gethash input input-map) (list next action)))) ))
    (make-instance 'fsm :states states :transition transition :test test :start start)))

(defun accept (fsm input)
  (declare (optimize debug))
  (with-slots (current-node states) fsm
    (let ((input-map (gethash current-node states)))
      (if (null input-map)
          (error "Invalid node: ~A" current-node)
          (let ((response (gethash input input-map)))
            (if (null response)
                nil
                (destructuring-bind (next action) response
                  (setf current-node next) ; TRANSITION???
                  (unless (null action)
                    (funcall action input))
                  current-node)))) )))
               
(defparameter *rooms* '((living-room north front-stairs) 
                        (living-room east kitchen)
                        (living-room south dining-room)
                        (upstairs-bedroom west library)
                        (upstairs-bedroom south front-stairs)
                        (dining-room north living-room)
                        (dining-room east pantry)
                        (dining-room west downstairs-bedroom)
                        (kitchen west living-room)
                        (kitchen south pantry)
                        (pantry north kitchen)
                        (pantry west dining-room)
                        (downstairs-bedroom north back-stairs) 
                        (downstairs-bedroom east dining-room)
                        (back-stairs north library)
                        (back-stairs south downstairs-bedroom)
                        (front-stairs north upstairs-bedroom)
                        (front-stairs south living-room)
                        (library south back-stairs)
                        (library east upstairs-bedroom)))

