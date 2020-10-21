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

;;;
;;; The presence of NAME and PRONOUN as alternatives for NOUN-PHRASE dramatically reduces
;;; the runaway sentences produced by grammar2.lisp!
;;; 
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
(defun generate-tree-norvig (phrase)
  (cond ((listp phrase) (mapcar #'generate-tree-norvig phrase))
        ((rewrites phrase) (cons phrase (generate-tree-norvig (random-elt (rewrites phrase)))) )
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


;;;
;;;    Finite yet bigger than *simple-grammar*
;;;    
(defparameter *medium-grammar* '((sentence -> (subject-noun-phrase verb-phrase))
                                 (subject-noun-phrase -> (article noun) (name) (subject-pronoun))
                                 (object-noun-phrase -> (article noun) (name) (object-pronoun))
                                 (verb-phrase -> (transitive-verb object-noun-phrase) (intransitive-verb))
                                 (article -> the a)
                                 (noun -> man ball woman table)
                                 (name -> cassandra jessica astrid james jacob paige david)
                                 (subject-pronoun -> he she it these those that)
                                 (object-pronoun -> him her it these those that)
                                 (transitive-verb -> hit took saw liked)
                                 (intransitive-verb -> sang walked pondered)))

;;;
;;;    Norvig
;;;
;;;    Typically the top-level call of GENERATE-ALL will be a symbol. This is either:
;;;    1. A terminal symbol. The fourth clause wraps in a nested list: ((<TERMINAL>))
;;;    2. A non-terminal symbol. Such a symbol has rewrites, so the 3rd clause is executed. Either:
;;;       a. A high-level non-terminal whose rewrites are further non-terminals.
;;;       b. A low-level non-terminal whose rewrites are just terminal symbols.
;;;
;;;    In the case of 2b, the function maps over the list of rewrites (symbols) and invokes recursive calls
;;;    which execute the 4th clause for each terminal.
;;;    E.g., ARTICLE: Rewrites => (the a). Recursive calls (generate-all 'the), (generate-all 'a) => ((THE) (A))
;;;
;;;    In the case of 2a, the rewrites are lists of 1+ lists. As clause 3 maps over these rewrites, each produces a recursive call that triggers
;;;    the 2nd clause, which results in cartesian products of its possible values. Yet these cartesian products are not
;;;    combined with the results of the other rewrites.
;;;    E.g., NOUN-PHRASE: Rewrites => ((article noun) (name) (pronoun)) yields 3 cartesian products, namely (article X noun), (name), (pronoun).
;;;
;;;    [Norvig leaves it unstated whether it's possible to mix rewrites. In other words, rewrites are _all_ further rewrites
;;;     or terminals. Does it even make sense to think of mixing them? (noun-phrase -> (article noun) (name) (pronoun) tom)
;;;     This does work...Everything results in lists of lists...]
;;;    
(defun generate-all-norvig (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list '()))
        ((listp phrase) (combine-all (generate-all-norvig (first phrase)) (generate-all-norvig (rest phrase)))) ; List of terminals -> A X B
        ((rewrites phrase) (mapcan #'generate-all-norvig (rewrites phrase))) ; Map over list of rewrites -- No calls to COMBINE-ALL.
        (t (list (list phrase)))) )

;;;
;;;    Norvig's 2nd clause does this:
;        ((listp phrase) (reduce #'combine-all (mapcar #'generate-all phrase)))


(defun combine-all (xs ys)
  (mapcan #'(lambda (y)
               (mapcar #'(lambda (x) (append x y)) xs))
           ys))

;;;
;;;    The substantial (?) difference between Norvig's version and mine is the corner case where a terminal symbol is used as the initial argument.
;;;    Norvig: (generate-all-norvig 'ball) => ((BALL))
;;;    vs.
;;;    Me: (generate-all 'ball) => BALL
;;;
;;;    Otherwise both versions generate the same set of results for a given grammar.
;;;
;;;    Another corner case...He accepts (and handles) rewrites as recursive (and thus as top-level) arguments. I only accept symbols.
;;;    (generate-all-norvig '(article noun)) => ((THE MAN) (A MAN) (THE BALL) (A BALL) (THE WOMAN) (A WOMAN) (THE TABLE) (A TABLE))
;;;    vs.
;;;    (generate-all '(article noun)) => (ARTICLE NOUN)
;;;    This could be perhaps considered to be a bug:
;;;    (terminalp '(article noun)) => T
;;;
;;;    I think that mine is more logically consistent. Every grammar rule consists of a symbol on the LHS. It doesn't make
;;;    sense to "generate" based on the RHS of a rule...How did you reach that RHS in the first place?
;;;    
(defun generate-all (symbol)
  (if (terminalp symbol)
      symbol
      (let ((rewrites (rewrites symbol)))
        (mapcan #'(lambda (rewrite)
                    (if (listp rewrite)
                        (reduce #'cartesian-product (mapcar #'generate-all rewrite))
                        (list (list rewrite))))
                rewrites))))

(defun cartesian-product (&optional as bs)
  (cond ((null as) bs)
        ((null bs) as)
        (t (loop for a in as
                 nconc (loop for b in bs collect (append a b)))) ))

;; (defun cross-product (f xs ys)
;;   (mapcan #'(lambda (y)
;;                (mapcar #'(lambda (x)
;;                            (funcall f x y))
;;                        xs))
;;            ys))

;; (defun combine-all (xs ys)
;;   (cross-product #'append xs ys))

;; (defun generate-all (phrase)
;;   "Generate a list of all possible expansions of this phrase."
;;   (cond ((null phrase) (list '()))
;;         ((listp phrase) (apply #'combine-all (mapcar #'generate-all phrase)))
;;         ((rewrites phrase) (mapcan #'generate-all (rewrites phrase)))
;;         (t (list (list phrase)))) )

;; (defun combine-all (as bs)
;;   (cartesian-product as bs))
;; ;  (apply #'nconc (cartesian-product as bs)))
;; ;  (cartesian-product (apply #'nconc as) (apply #'nconc bs)))

;; (defun combine-all (&rest rewrites)
;;   (reduce #'cartesian-product rewrites))

;;;
;;;    See 2012 grammar.lisp (LPN ch. 7)
;;;
;;;    Norvig's version blows up with the not -> not rule!!
;;;    
;; (defparameter *logic-grammar*
;;   '((proposition -> (term) (not term) (term connective term))
;;     (not -> not)
;;     (connective -> and or => <=> xor)
;;     (term -> p q r s)))

(defparameter *logic-grammar*
  '((proposition -> (term) (not term) (term connective term))
    (not -> !)
    (connective -> and or => <=> xor)
    (term -> p q r s)))
