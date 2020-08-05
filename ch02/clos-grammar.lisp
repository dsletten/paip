;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               clos-grammar.lisp
;;;;
;;;;   Started:            Sun Jul  8 15:43:29 2012
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
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :clos-grammar (:use :common-lisp :lang :test))

(in-package :clos-grammar)

(defconstant delimiter '->)

(defclass rule ()
  ((lhs :accessor lhs :initarg :lhs :initform (error "Must supply LHS"))
   (rhs :accessor rhs :initarg :rhs :initform (error "Must supply RHS"))))

(defun make-rule (lhs rhs)
  (make-instance 'rule :lhs lhs :rhs rhs))

(defmethod print-object ((r rule) stream)
  (format stream "~A ~A~{~^ ~A~}" (lhs r) delimiter (rhs r)))

;; (defmethod print-object ((r rule) stream)
;;   (print-unreadable-object (r stream :type t)
;;     (format stream "~A ~A ~A" (lhs r) delimiter (rhs r))))

;; (defrule (s -> (np vp)))
;; (defmacro defrule ((lhs delim &rest rhs))
;;   (if (eql delim delimiter)
;;       `(make-rule ',lhs ',rhs)
;;       (error "Unrecognized rule delimiter")))

;; (defrule s -> (np vp))
(defmacro defrule (lhs delim &rest rhs)
  (if (eql delim delimiter)
      `(make-rule ',lhs ',rhs)
      (error "Unrecognized rule delimiter")))

(defclass grammar ()
  ((rules :accessor rules :initarg :rules :initform (error "Must supply rules for grammar"))))

(defun make-grammar (&rest rules)
  (let ((rule-table (make-hash-table)))
    (dolist (rule rules)
      (setf (gethash (lhs rule) rule-table) rule))
    (make-instance 'grammar :rules rule-table)))

(defmethod print-object ((g grammar) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "with ~D rule~:P" (hash-table-count (rules g)))) )

(defmacro defgrammar (name &body body)
  (let ((rules (loop for rule in body
                     collect `(defrule ,@rule))))
    `(progn (declaim (special ,name))
            (setf (symbol-value ',name) (make-grammar ,@rules))
           ',name)))

(defmacro defgrammar (name &body body)
  (let ((rules (mapcar #'(lambda (rule) `(defrule ,@rule)) body)))
    `(progn (declaim (special ,name))
            (setf (symbol-value ',name) (make-grammar ,@rules))
           ',name)))

;;;
;;;    Why is this not relying on PRINT-OBJECT for RULE?
;;;    
(defun print-grammar (g)
  (dohash ((_ rule) (rules g))
    (format t "~A~%" rule)))
;    (format t "~A ~A~{~^ ~A~}~%" (lhs rule) delimiter (rhs rule))))

(defgrammar *simple-grammar*
  (s -> (np vp))
  (np -> (d n))
  (vp -> (v np))
  (d -> the a)
  (n -> man ball woman table)
  (v -> hit took saw liked))

(defun get-rule (grammar symbol)
  (gethash symbol (rules grammar)))

(defun rule-rewrites (grammar symbol)
  (let ((rewrites (get-rule grammar symbol)))
    (if (null rewrites)
        rewrites
        (rhs rewrites))))

(defun generate (grammar phrase)
  (if (listp phrase)
      (loop for term in phrase
            nconc (generate grammar term))
      (let ((rewrites (rule-rewrites grammar phrase)))
        (if (null rewrites)
            (list phrase)
            (generate grammar (random-elt rewrites)))) ))

(defun random-elt (choices)
  (elt choices (random (length choices))))

(defgrammar *bigger-grammar*
  (s -> (np vp))
  (np -> (d adj* n pp*) (name) (pronoun))
  (vp -> (v np pp*))
  (pp* -> () (pp pp*))
  (adj* -> () (adj adj*))
  (pp -> (p np))
  (p -> to in by with on)
  (adj -> big little blue green adiabatic)
  (d -> the a)
  (name -> pat kim lee terry rubin)
  (n -> man ball woman table)
  (v -> hit took saw liked)
  (pronoun -> he she it these those that))
