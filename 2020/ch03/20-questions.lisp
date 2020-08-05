;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               20-questions.lisp
;;;;
;;;;   Started:            Mon Jul 27 02:25:42 2020
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
;(load "/home/slytobias/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :20-questions (:use :common-lisp :lang :test))

(in-package :20-questions)

(defclass node ()
  ((question :reader question :initarg :question)
   (yes :reader yes :initform nil :initarg :yes)
   (no :reader no :initform nil :initarg :no)))

(defmethod print-object ((n node) stream)
  (print-unreadable-object (n stream :type t)
    (format stream "~A yes: ~A no: ~A" 
            (question n) 
            (print-branch (yes n))
            (print-branch (no n)))) )

(defun print-branch (branch)
  (typecase branch
    ;; (node (print-unreadable-object (branch nil :type t)
    ;;         (format nil "")))
    (node (format nil "#<~A>" (type-of branch)))
    (otherwise branch)))

(defun add-yes-node (node branch)
  (with-slots (yes) node
    (let ((old-branch yes))
      (setf yes branch)
      old-branch)))

(defun add-no-node (node branch)
  (with-slots (no) node
    (let ((old-branch no))
      (setf no branch)
      old-branch)))

(defclass game ()
  ((category :reader category :initarg :category)
   (root :initform nil)))

(defmethod print-object ((g game) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "~A" (category g))))

(defun initialize ()
  (let* ((category (prompt-read "Enter category for game: " :allow-empty nil))
         (game (make-instance 'game :category category))
         (question (prompt-read "Enter initial question: " :allow-empty nil)))
    (with-slots (root) game
      (setf root (make-instance 'node :question question))
      (let ((answer (prompt-read "How do you answer that question? " :allow-empty nil))
            (object (prompt-read "What is the object? " :allow-empty nil)))
        (if (affirmative answer)
            (add-yes-node root object)
            (add-no-node root object))))
    game))
          
(defun play (&optional (game (initialize)))
;  (let ((game (initialize)))
    (with-slots (root) game
      (let ((node root))
        (loop
             (let ((reply (ask-question node)))
               (cond ((affirmative reply)
                      (typecase (yes node)
                        (null (add-yes-response node) (return game))
                        (node (setf node (yes node)))
                        (otherwise (validate-yes node (yes node)) (return game))))
                     ((negative reply)
                      (typecase (no node)
                        (null (add-no-response node) (return game))
                        (node (setf node (no node)))
                        (otherwise (validate-no node (no node)) (return game)))) )))) ))
                      
(defun ask-question (node)
  (with-slots (question) node
    (prompt-read (format nil "~A " question) :allow-empty nil)))

(defun affirmative (reply)
  (member reply '("y" "yes") :test #'string-equal))

(defun negative (reply)
  (member reply '("n" "no") :test #'string-equal))

(defun validate-yes (node answer)
  (let ((reply (prompt-read (format nil "Is it a ~A? " answer) :allow-empty nil)))
    (cond ((affirmative reply) (format t "Awesome!~%"))
          (t (let* ((object (prompt-read "What were you thinking of? " :allow-empty nil))
                    (new-question (prompt-read (format nil "What is a question to distinguish a ~A from a ~A? " object answer) :allow-empty nil))
                    (new-branch (prompt-read (format nil "How would you answer that question for a ~A? " object) :allow-empty nil)))
               (cond ((affirmative new-branch) (add-yes-node node (make-instance 'node :question new-question :yes object :no answer)))
                     ((negative new-branch) (add-yes-node node (make-instance 'node :question new-question :yes answer :no object)))
                     (t (error "Bad response")))) ))))

(defun validate-no (node answer)
  (let ((reply (prompt-read (format nil "Is it a ~A? " answer) :allow-empty nil)))
    (cond ((affirmative reply) (format t "Awesome!~%"))
          (t (let* ((object (prompt-read "What were you thinking of? " :allow-empty nil))
                    (new-question (prompt-read (format nil "What is a question to distinguish a ~A from a ~A? " object answer) :allow-empty nil))
                    (new-branch (prompt-read (format nil "How would you answer that question for a ~A? " object) :allow-empty nil)))
               (cond ((affirmative new-branch) (add-no-node node (make-instance 'node :question new-question :yes object :no answer)))
                     ((negative new-branch) (add-no-node node (make-instance 'node :question new-question :yes answer :no object)))
                     (t (error "Bad response")))) ))))

(defun add-yes-response (node)
  (format t "I have no idea...~%")
  (let ((object (prompt-read "What were you thinking of? " :allow-empty nil)))
    (add-yes-node node object)))

(defun add-no-response (node)
  (format t "I have no idea...~%")
  (let ((object (prompt-read "What were you thinking of? " :allow-empty nil)))
    (add-no-node node object)))

