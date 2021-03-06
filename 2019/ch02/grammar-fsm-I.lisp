;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               grammar-fsm-I.lisp
;;;;
;;;;   Started:            Sun Nov  1 20:07:21 2020
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

(defpackage :grammar-fsm-I (:use :common-lisp :test))

(in-package :grammar-fsm-I)

(defvar *determiners* '(the a))
(defvar *nouns* '(man ball woman table))
(defvar *verbs* '(hit took saw liked))

;;;
;;;    This is to facilitate testing sentences generated by other packages...
;;;    
(defun symbol-member (sym set)
  (if (member sym set :test #'(lambda (s1 s2) (string= (symbol-name s1) (symbol-name s2))))
      t 
      nil))

(defun determinerp (word)
  (symbol-member word *determiners*))

(defun nounp (word)
  (symbol-member word *nouns*))

(defun verbp (word)
  (symbol-member word *verbs*))

;;;
;;;   I. STATE is simply a keyword keeping track of where FSM is currently.
;;;      INPUT is part of the recognizer's state.
;;;   
(defclass recognizer ()
  ((states :reader states :initform (make-hash-table))
   (current-state :reader current-state :initform :start)
   (input :accessor input)))

(defun make-recognizer (state-machine)
  (let ((recognizer (make-instance 'recognizer)))
    (with-slots (states) recognizer
      (loop for (state action) in state-machine
            do (setf (gethash state states) action)))
    recognizer))

(defun set-input (recognizer sentence)
  (with-slots (current-state input) recognizer
    (setf input sentence
          current-state :start)))

(defun process (recognizer)
  (with-slots (states current-state input) recognizer
    (if (null input)
        nil
        (let* ((word (pop input))
               (next-state (funcall (gethash current-state states) word)))
          (values (cond ((null next-state) nil)
                        ((eq next-state :end) t)
                        (t (setf current-state next-state)))
                  word)))) )

(defparameter *simple-recognizer* (make-recognizer `((:start ,#'(lambda (word)
                                                                  (cond ((null word) (error "Sentence is empty."));: ~A" sentence))
                                                                        ((determinerp word) :determiner0)
                                                                        (t nil))))
                                                     (:determiner0 ,#'(lambda (word)
                                                                        (cond ((null word) nil)
                                                                              ((nounp word) :noun0)
                                                                              (t nil))))
                                                     (:noun0 ,#'(lambda (word)
                                                                  (cond ((null word) nil)
                                                                        ((verbp word) :verb)
                                                                        (t nil))))
                                                     (:verb ,#'(lambda (word)
                                                                 (cond ((null word) nil)
                                                                       ((determinerp word) :determiner1)
                                                                       (t nil))))
                                                     (:determiner1 ,#'(lambda (word)
                                                                        (cond ((null word) nil)
                                                                              ((nounp word) :end)
;                                                                              ((nounp word) :noun1)
                                                                              (t nil)))) )))
                                                     ;; (:noun1 ,#'(lambda (word)
                                                     ;;              (cond ((null word) :end)
                                                     ;;                    (t nil)))) )))


;;;
;;;    Example execution. (First load GRAMMAR-SIMPLE package.)
;;;    
;(set-input *simple-recognizer* (grammar-simple::sentence))
;(process *simple-recognizer*)
