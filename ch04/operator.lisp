;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               operator.lisp
;;;;
;;;;   Started:            Thu Sep 20 18:14:48 2012
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
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/collections.lisp")

(defpackage :operator
  (:shadowing-import-from :collections :set)  
  (:use :common-lisp :lang)
  (:export :action :add-to-state :make-operator :preconditions :remove-from-state))

(in-package :operator)

(defclass operator ()
  ((action :reader action :initarg :action :type symbol)
   (preconditions :reader preconditions :initarg :preconditions :type list)
   (add-to-state :reader add-to-state :initarg :add-to-state :type set)
   (remove-from-state :reader remove-from-state :initarg :remove-from-state :type set)))

(defmethod print-object ((op operator) stream)
  (print-unreadable-object (op stream :type t)
    (format stream "~A" (action op))))

(defun make-operator (action &optional (preconditions '()) (add-to-state #{}) (remove-from-state #{}))
  (make-instance 'operator :action action :preconditions preconditions :add-to-state add-to-state :remove-from-state remove-from-state))

