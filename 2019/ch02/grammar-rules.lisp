;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               grammar-rules.lisp
;;;;
;;;;   Started:            Tue Jul  7 01:57:17 2020
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
;;;;   Notes: Data-driven implementation. Rules define structure of grammar.
;;;;   Only one main function GENERATE operates on rules.
;;;;
;;;;   Easier to extend *simple-grammar* -> *bigger-grammar*
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp") ; How to test these randomly-generated sentences?!

(defpackage :grammar-rules (:use :common-lisp :test))

(in-package :grammar-rules)

;;;
;;;    Grammar is a table (association list) of rules.
;;;    Each rule associates a category with a list of rewrites. E.g., (ignoring the purely symbolic ->)
;;;    (sentence . ((noun-phrase verb-phrase)))
;;;    (article . (the a))
;;;    
(defparameter *simple-grammar* '((sentence -> (noun-phrase verb-phrase))
                                 (noun-phrase -> (article noun))
                                 (verb-phrase -> (verb noun-phrase))
                                 (article -> the a)
                                 (noun -> man ball woman table)
                                 (verb -> hit took saw liked)))

;;;
;;;    *GRAMMAR* refers to current grammar in use.
;;;    
(defvar *grammar* *simple-grammar*)

(defun retrieve-rule (category)
  (assoc category *grammar*))

(deftest test-retrieve-rule ()
  (let ((*grammar* *simple-grammar*))
    (check
     (equal (retrieve-rule 'verb-phrase) '(verb-phrase -> (verb noun-phrase)))
     (equal (retrieve-rule 'noun) '(noun -> man ball woman table))
     (null (retrieve-rule 'pung)))) )

;; (defun rule-lhs (rule)
;;   (first rule))

(defun rule-lhs (rule)
  (destructuring-bind (category arrow . rewrites) rule
    (declare (ignore arrow rewrites))
    category))

(deftest test-rule-lhs ()
  (let ((*grammar* *simple-grammar*))
    (check
     (eq (rule-lhs (retrieve-rule #1='verb-phrase)) #1#)
     (eq (rule-lhs (retrieve-rule #2='noun)) #2#))))

;; (defun rule-rhs (rule)
;;   (rest (rest rule)))

(defun rule-rhs (rule)
  (destructuring-bind (category arrow . rewrites) rule
    (declare (ignore category arrow))
    rewrites))

(deftest test-rule-rhs ()
  (let ((*grammar* *simple-grammar*))
    (check
     (equal (rule-rhs (retrieve-rule 'verb-phrase)) '((verb noun-phrase)))
     (equal (rule-rhs (retrieve-rule 'noun)) '(man ball woman table)))) )

;(defun rewrites (grammar category)
;; (defun rewrites (category)
;;   (rule-rhs (assoc category *grammar*)))

(defun rewrites (category)
  (let ((rule (retrieve-rule category)))
    (if (null rule)
        '()
        (rule-rhs rule))))

(deftest test-rewrites ()
  (let ((*grammar* *simple-grammar*))
    (check
     (equal (rewrites 'verb-phrase) '((verb noun-phrase)))
     (equal (rewrites 'noun) '(man ball woman table))
     (null (rewrites 'pung)))) )

(defun random-elt (choices)
  "Choose a random element from a list."
  (elt choices (random (length choices))))

;(defun generate (grammar phrase)  CLOS!

;;;
;;;    Norvig original
;;;    
;; (defun generate (phrase)
;;   (cond ((listp phrase) (mapcan #'generate phrase)) ; He uses his MAPPEND...
;;         ((rewrites phrase) (generate (random-elt (rewrites phrase))))
;;         (t (list phrase))))

;;;
;;;    Norvig 2
;;;    
;; (defun generate (phrase)
;;   (if (listp phrase)
;;       (mapcan #'generate phrase)
;;       (let ((choices (rewrites phrase)))
;;         (if (null choices)
;;             (list phrase)
;;             (generate (random-elt (rewrites phrase)))) )))

;;;
;;;    My first version.
;;;    
(defun generate (phrase)
  (if (listp phrase)
      (loop for term in phrase
            nconc (generate term))
      (let ((rewrites (rewrites phrase)))
        (if (null rewrites)
            (list phrase)
            (generate (random-elt rewrites)))) ))

;;;
;;;    Ex. 2.1
;;;
(defun generate (phrase)
  (cond ((listp phrase) (mapcan #'generate phrase))
        (t (let ((choices (rewrites phrase)))
             (cond ((null choices) (list phrase))
                   (t (generate (random-elt (rewrites phrase)))) )))) )

;;;
;;;    Ex. 2.2
;;;    
(defun terminalp (symbol)
  (null (rewrites symbol)))

;;;
;;;    Argument to GENERATE is always a symbol now. In fact, it is always a non-terminal symbol
;;;    unless GENERATE is called directly with a terminal:
;;;    (generate 'ball) => BALL
;;;    No need to produce a list in this case.
;;;    
;;;    This eliminates some recursive calls from original version.
;;;    
;; (defun generate (symbol)
;;   (if (terminalp symbol)
;;       symbol
;;       (let ((rewrite (random-elt (rewrites symbol))))
;;         (if (listp rewrite)
;;             (mapcan #'generate rewrite)
;;             (list rewrite)))) )

;;;
;;;    The outer IF ensures that non-terminal symbol will have rewrites, but the chosen rewrite
;;;    may itself be (), e.g., in *BIGGER-GRAMMAR*.
;;;    
(defun generate (symbol)
  (if (terminalp symbol)
      symbol
      (let ((rewrite (random-elt (rewrites symbol))))
        (cond ((null rewrite) '())
              ((listp rewrite) (mapcan #'generate rewrite))
              (t (list rewrite)))) ))

(defparameter *bigger-grammar* '((sentence -> (noun-phrase verb-phrase))
                                 (noun-phrase -> (article adj* noun pp*) (name) (pronoun))
                                 (verb-phrase -> (verb noun-phrase pp*))
                                 (adj* -> () (adj adj*))
                                 (pp* -> () (pp pp*))
                                 (pp -> (prep noun-phrase))
                                 (article -> the a)
                                 (noun -> man ball woman table)
                                 (name -> cassandra jessica astrid james jacob paige david)
                                 (pronoun -> he she it these those that)
                                 (adj -> big little blue green adiabatic)
                                 (prep -> to in by with on)
                                 (verb -> hit took saw liked)))

;;;;
;;;;    Show phrase structure as parse tree
;;;;    

;;;
;;;    Norvig original
;;;    
(defun generate-tree (phrase)
  (cond ((listp phrase) (mapcar #'generate-tree phrase))
        ((rewrites phrase) (cons phrase (generate-tree (random-elt (rewrites phrase)))) )
        (t (list phrase))))

;;;
;;;    Simple rewrite
;;;
(defun generate-tree (phrase)
  (if (listp phrase)
      (mapcar #'generate-tree phrase)
      (let ((rewrites (rewrites phrase)))
        (if (null rewrites)
            (list phrase)
            (cons phrase (generate-tree (random-elt rewrites)))) )))

(defun generate-tree (symbol)
  (if (terminalp symbol)
      symbol
      (let ((rewrite (random-elt (rewrites symbol))))
        (if (listp rewrite)
            (cons symbol (mapcar #'generate-tree rewrite))
            (list symbol rewrite)))) )


