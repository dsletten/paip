;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               grammar-fsm-II.lisp
;;;;
;;;;   Started:            Sun Nov  1 20:07:23 2020
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

(defpackage :grammar-fsm-II (:use :common-lisp :test))

(in-package :grammar-fsm-II)

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

(defvar *adjectives* '(big little blue green adiabatic))
(defvar *prepositions* '(to in by with on))

(defun adjectivep (word)
  (symbol-member word *adjectives*))

(defun prepositionp (word)
  (symbol-member word *prepositions*))

;;;
;;;    II. STATE is separate from FSM. It incorporates input as well as list of tokens.
;;;    
(defclass state ()
  ((current-state :reader current-state :initform :start)
   (original-input :accessor original-input)
   (input :accessor input)
   (tokens :initform (collections:make-linked-queue))))

(defmethod tokens ((s state))
  (with-slots (tokens) s
    (collections:elements tokens)))

(defun exhaustedp (state)
  (null (input state)))

(defclass recognizer ()
  ((states :reader states :initform (make-hash-table))))

(defun make-recognizer (state-machine)
  (let ((recognizer (make-instance 'recognizer)))
    (with-slots (states) recognizer
      (loop for (state action) in state-machine
            do (setf (gethash state states) action)))
    recognizer))

(defmacro defrecognizer (&rest states)
  (let ((state-machine (loop for (state args action) in states
                             collect `(list ,state #'(lambda ,args ,action)))) )
;                             collect `(,state ,#'(lambda `,args `,action)))))
    `(make-recognizer (list ,@state-machine))))

(defun set-input (state sentence)
  (with-slots (current-state original-input input tokens) state
    (collections:make-empty tokens)
    (setf original-input sentence
          input sentence
          current-state :start)))

(defun process (recognizer state)
  (with-slots (states) recognizer
    (with-slots (current-state tokens) state
      (cond ((exhaustedp state) (if (eq current-state :end)
                                    nil
                                    (error "Input is exhausted in non-terminal state")))
            ((eq current-state :fail) (error "Input is invalid: ~A" (original-input state)))
            (t (let ((word (pop (input state))))
                 (multiple-value-bind (next-state category) (funcall (gethash current-state states) word)
                   (setf current-state next-state)
                   (cond ((eq next-state :fail) nil)
                         (t (collections:enqueue tokens (list category word)))) )))) )))

(defparameter *simple-recognizer* (make-recognizer `((:start ,#'(lambda (word)
                                                                  (cond ((determinerp word) (values :determiner0 'determiner))
                                                                        (t :fail))))
                                                     (:determiner0 ,#'(lambda (word)
                                                                        (cond ((null word) :fail)
                                                                              ((nounp word) (values :noun0 'noun))
                                                                              (t :fail))))
                                                     (:noun0 ,#'(lambda (word)
                                                                  (cond ((null word) :fail)
                                                                        ((verbp word) (values :verb 'verb))
                                                                        (t :fail))) )
                                                     (:verb ,#'(lambda (word)
                                                                 (cond ((null word) :fail)
                                                                       ((determinerp word) (values :determiner1 'determiner))
                                                                       (t :fail))) )
                                                     (:determiner1 ,#'(lambda (word)
                                                                        (cond ((null word) :fail)
                                                                              ((nounp word) (values :end 'noun))
                                                                              (t :fail)))) )))

(defparameter *simple-recognizer2* (defrecognizer
                                      (:start (word)
                                              (cond ((determinerp word) (values :determiner0 'determiner))
                                                    (t :fail)))
                                      (:determiner0 (word)
                                                    (cond ((null word) :fail)
                                                          ((nounp word) (values :noun0 'noun))
                                                          (t :fail)))
                                    (:noun0 (word)
                                            (cond ((null word) :fail)
                                                  ((verbp word) (values :verb 'verb))
                                                  (t :fail)))
                                    (:verb (word)
                                           (cond ((null word) :fail)
                                                 ((determinerp word) (values :determiner1 'determiner))
                                                 (t :fail)))
                                    (:determiner1 (word)
                                                  (cond ((null word) :fail)
                                                        ((nounp word) (values :end 'noun))
                                                        (t :fail)))) )
(defparameter *recognizer-2a* (defrecognizer
                                  (:start (word)
                                          (cond ((determinerp word) (values :determiner0 'determiner))
                                                (t :fail)))
                                  (:determiner0 (word)
                                                (cond ((null word) :fail)
                                                      ((nounp word) (values :noun0 'noun))
                                                      ((adjectivep word) (values :adjective0 'adjective))
                                                      (t :fail)))
                                (:noun0 (word)
                                        (cond ((null word) :fail)
                                              ((verbp word) (values :verb 'verb))
                                              ((prepositionp word) (values :preposition0 'preposition))
                                              (t :fail)))
                                (:adjective0 (word)
                                             (cond ((null word) :fail)
                                                   ((nounp word) (values :noun0 'noun))
                                                   ((adjectivep word) (values :adjective0 'adjective))
                                                   (t :fail)))
                                (:preposition0 (word)
                                               (cond ((null word) :fail)
                                                     ((determinerp word) (values :determiner0 'determiner))
                                                     (t :fail)))
                                (:verb (word)
                                       (cond ((null word) :fail)
                                             ((determinerp word) (values :determiner1 'determiner))
                                             (t :fail)))
                                (:determiner1 (word)
                                              (cond ((null word) :fail)
                                                    ((nounp word) (values :noun1 'noun))
                                                    ((adjectivep word) (values :adjective1 'adjective))
                                                    (t :fail)))
                                (:noun1 (word)
                                        (cond ((null word) (values :end nil))
                                              ((prepositionp word) (values :preposition1 'preposition))
                                              (t :fail)))
                                (:adjective1 (word)
                                             (cond ((null word) :fail)
                                                   ((nounp word) (values :noun1 'noun))
                                                   ((adjectivep word) (values :adjective1 'adjective))
                                                   (t :fail)))
                                (:preposition1 (word)
                                               (cond ((null word) :fail)
                                                     ((determinerp word) (values :determiner1 'determiner))
                                                     (t :fail)))) )

;; (defvar *s* (make-instance 'state))
;; (set-input *s* (grammar-simple::sentence))
;; (process *simple-recognizer* *s*)

;; (set-input *s* (grammar2::sentence))
;; (process *recognizer-2a* *s*)
