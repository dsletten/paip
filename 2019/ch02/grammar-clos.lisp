;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   LISP has been jokingly described as "the most intelligent way to misuse a computer".
;;;;   -- Edsger W. Dijkstra
;;;;
;;;;   Name:               grammar-clos.lisp
;;;;
;;;;   Started:            Fri Jul 10 02:27:59 2020
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

(defpackage :grammar-clos (:use :common-lisp :test))

(in-package :grammar-clos)

(defclass grammar ()
  ((rules :initform (make-hash-table))))

(defmethod initialize-instance :after ((g grammar) &rest initargs &key defrules)
  (declare (ignore initargs))
  (with-slots (rules) g
  (dolist (rule defrules)
    (destructuring-bind (marker category arrow . rewrites) rule
      (assert (eq marker 'defrule))
      (assert (eq arrow '->))
      (setf (gethash category rules) rewrites)))) )
           
(defclass rule ()
  ((category :reader category :initarg :category)
   (rewrites :reader rewrites :initarg :rewrites)))

(defmethod print-object ((r rule) stream)
  (format stream "~A -> ~A" (category r) (rewrites r)))

;; (defmacro defrule (category arrow &rest rewrites)
;;   `(

;; (defmacro defgrammar (name &rest rules)
;;   `(progn (defvar ,name (make-hash-table))
;;           (dolist (rule ',rules ,name)
;;             (destructuring-bind (marker category arrow . rewrites) rule
;;               (assert (eq marker 'defrule))
;;               (assert (eq arrow '->))
;;               (setf (gethash category ,name) rewrites)))) )

(defmacro defgrammar (name &rest rules)
  `(defvar ,name (make-instance 'grammar :defrules ',rules)))

;; (defun retrieve-rule (grammar category)
;;   (with-slots (rules) grammar
;;     (gethash category rules)))

(defun retrieve-rule (grammar category)
  (with-slots (rules) grammar
    (let ((rewrites (gethash category rules)))
      (if (null rewrites)
          nil
          (make-instance 'rule :category category :rewrites rewrites)))) )

(defun random-elt (choices)
  "Choose a random element from a list."
  (elt choices (random (length choices))))

;;;
;;;    Terminal vs. nonsense??
;;;    
(defun terminalp (grammar symbol)
  (null (retrieve-rule grammar symbol)))

(defun generate (grammar symbol)
  (if (terminalp grammar symbol)
      symbol
      (let ((rewrite (random-elt (rewrites (retrieve-rule grammar symbol)))) )
        (if (listp rewrite)
            (mapcan #'(lambda (category) (generate grammar category)) rewrite)
            (list rewrite)))) )

(defgrammar *simple-grammar* 
  (defrule sentence -> (noun-phrase verb-phrase))
  (defrule noun-phrase -> (article noun))
  (defrule verb-phrase -> (verb noun-phrase))
  (defrule article -> the a)
  (defrule noun -> man ball woman table)
  (defrule verb -> hit took saw liked))

(defgrammar *bigger-grammar*
  (defrule sentence -> (noun-phrase verb-phrase))
  (defrule noun-phrase -> (article adj* noun pp*) (name) (pronoun))
  (defrule verb-phrase -> (verb noun-phrase pp*))
  (defrule adj* -> () (adj adj*))
  (defrule pp* -> () (pp pp*))
  (defrule pp -> (prep noun-phrase))
  (defrule article -> the a)
  (defrule noun -> man ball woman table)
  (defrule name -> cassandra jessica astrid james jacob paige david)
  (defrule pronoun -> he she it these those that)
  (defrule adj -> big little blue green adiabatic)
  (defrule prep -> to in by with on)
  (defrule verb -> hit took saw liked))
