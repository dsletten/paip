;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a language for doing what you've been told is impossible.
;;;;   -- Kent Pitman
;;;;
;;;;   Name:               grammar.lisp
;;;;
;;;;   Started:            Sat Jul  7 17:36:33 2012
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
(load "/Users/dsletten/lisp/packages/iterators.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :grammar
  (:shadowing-import-from :collections :intersection :set :set-difference :subsetp :union)
  (:shadowing-import-from :iterators :next)
  (:use :collections :common-lisp :iterators :lang :test))

(in-package :grammar)

(defun one-of (set)
  (list (random-elt set)))

;;;
;;;    This is the only function needed below in the data-driven grammar.
;;;    
(defun random-elt (choices)
  (elt choices (random (length choices))))

;;;
;;;    Simple grammar
;;;    
(defun sentence ()
  (append (noun-phrase) (verb-phrase)))

(defun noun-phrase ()
  (append (determiner) (noun)))

(defun verb-phrase ()
  (append (verb) (noun-phrase)))

(defun determiner ()
  (one-of '(the a)))

(defun noun ()
  (one-of '(man ball woman table)))

(defun verb ()
  (one-of '(hit took saw liked)))

;;;
;;;    Extended grammar
;;;    
(defun noun-phrase ()
  (append (determiner) (adj*) (noun) (pp*)))

(defun adj* ()
  (if (zerop (random 2))
      '()
      (append (adj) (adj*))))

(defun pp* ()
  (if (random-elt '(t nil))
      (append (pp) (pp*))
      '()))

;;;
;;;    Alternative to above ADJ*
;;;    
;; (defun adj* ()
;;   (funcall (random-elt (list #'(lambda () '())
;;                              #'(lambda ()
;;                                  (append (adj) (adj*)))) )))

(defun pp ()
  (append (prep) (noun-phrase)))

(defun adj ()
  (one-of '(big little blue green)))

(defun prep ()
  (one-of '(to in by with on)))

;;;
;;;    !!!!!
;;;    
;; * (sentence)

;; (A BALL
;; HIT
;; THE BIG MAN ON A MAN TO A GREEN MAN ON THE LITTLE MAN TO A BALL IN
;;  A MAN ON A BLUE TABLE TO A GREEN TABLE BY A GREEN WOMAN ON A BIG LITTLE MAN ON
;;  A GREEN MAN IN A WOMAN TO THE BLUE BLUE WOMAN BY THE WOMAN WITH A WOMAN BY THE
;;  BIG BALL WITH A MAN IN THE BIG MAN WITH THE MAN IN A LITTLE BALL IN A GREEN
;;  GREEN MAN BY THE LITTLE TABLE IN THE LITTLE BIG BALL ON THE GREEN GREEN TABLE
;;  WITH THE TABLE IN THE GREEN LITTLE GREEN BIG BLUE TABLE ON THE BIG TABLE BY A
;;  LITTLE WOMAN TO A LITTLE GREEN BALL BY THE MAN IN A MAN TO THE GREEN MAN BY A
;;  TABLE ON A BIG WOMAN IN A BLUE WOMAN ON A WOMAN BY THE TABLE ON THE GREEN BIG
;;  LITTLE LITTLE BALL TO A BIG BIG BALL TO THE BALL BY A BALL BY A BLUE BALL ON A
;;  BIG BALL WITH THE BALL BY A MAN BY THE MAN WITH A WOMAN BY A GREEN BALL TO A
;;  BIG TABLE BY THE BIG TABLE BY A BALL ON THE MAN TO THE BIG MAN WITH THE LITTLE
;;  WOMAN TO THE MAN ON THE BALL WITH A GREEN TABLE IN A LITTLE MAN BY A GREEN
;;  LITTLE BALL WITH THE LITTLE BLUE BLUE LITTLE BALL TO THE BALL ON A BLUE LITTLE
;;  LITTLE TABLE BY A GREEN WOMAN IN A MAN BY A BLUE BLUE BALL WITH A LITTLE BIG
;;  BLUE GREEN TABLE ON THE LITTLE MAN TO THE WOMAN IN THE TABLE ON THE BLUE GREEN
;;  BALL IN THE BALL ON A MAN BY THE BALL WITH A BLUE GREEN WOMAN WITH THE BALL IN
;;  A WOMAN ON A TABLE TO THE LITTLE BLUE WOMAN TO A GREEN GREEN TABLE IN A BIG
;;  BIG MAN WITH A GREEN BLUE BALL TO THE LITTLE BIG BIG BLUE BLUE BLUE TABLE IN A
;;  TABLE ON A MAN BY A GREEN LITTLE GREEN BIG WOMAN TO A WOMAN TO A TABLE WITH A
;;  TABLE WITH A BLUE MAN WITH A GREEN BLUE BIG MAN WITH A MAN IN THE BALL WITH
;;  THE BIG BALL IN THE LITTLE BIG MAN BY A GREEN LITTLE BIG BLUE BLUE BLUE MAN ON
;;  A GREEN LITTLE BIG LITTLE BLUE WOMAN ON A BALL IN A BLUE BLUE BIG BLUE WOMAN
;;  TO THE GREEN TABLE ON THE MAN ON A BIG BLUE MAN WITH THE GREEN WOMAN ON A
;;  TABLE WITH A MAN BY A GREEN BLUE BIG BLUE BIG BIG BALL IN THE BALL BY A GREEN
;;  BIG GREEN WOMAN ON THE BALL IN A TABLE TO THE BLUE BLUE LITTLE BLUE GREEN BALL
;;  IN THE MAN IN A WOMAN ON THE WOMAN BY THE GREEN TABLE TO THE LITTLE LITTLE
;;  LITTLE MAN BY THE MAN BY THE BALL TO THE LITTLE GREEN BIG BALL BY A MAN WITH
;;  THE TABLE TO THE BLUE BLUE BIG WOMAN WITH THE MAN WITH THE BLUE BLUE BLUE
;;  WOMAN BY THE LITTLE BALL BY THE MAN IN A GREEN BALL BY A BLUE GREEN LITTLE BIG
;;  MAN IN THE BIG MAN IN A MAN BY THE BALL WITH A BALL BY THE GREEN MAN ON A
;;  WOMAN IN THE GREEN BALL IN THE MAN IN THE BALL TO THE WOMAN TO THE WOMAN IN
;;  THE LITTLE LITTLE MAN WITH THE BLUE BALL TO THE WOMAN IN THE MAN WITH A BIG
;;  TABLE WITH A BIG GREEN LITTLE GREEN MAN ON A MAN WITH THE BALL TO A WOMAN WITH
;;  A TABLE ON A LITTLE MAN WITH THE WOMAN WITH THE WOMAN ON THE TABLE BY A GREEN
;;  BALL TO THE WOMAN WITH THE BALL ON A BLUE TABLE TO A MAN WITH A BALL ON A BLUE
;;  BLUE BALL ON THE WOMAN TO THE WOMAN TO A WOMAN TO A BIG BALL ON THE LITTLE
;;  BALL IN THE GREEN BIG GREEN BLUE BLUE BLUE BALL TO THE GREEN BALL BY A WOMAN
;;  BY A LITTLE GREEN BLUE WOMAN ON A WOMAN BY THE BIG BLUE BIG LITTLE BALL IN A
;;  LITTLE BLUE GREEN MAN WITH A TABLE ON A BALL BY A TABLE BY A LITTLE BIG BALL
;;  ON A TABLE BY THE GREEN GREEN BLUE BALL IN A WOMAN TO A BALL TO THE MAN IN THE
;;  MAN WITH A MAN WITH THE TABLE ON THE MAN BY THE BLUE BIG LITTLE LITTLE BIG
;;  TABLE BY A BLUE WOMAN IN THE MAN BY A GREEN BLUE BALL ON THE BIG BALL BY THE
;;  BLUE BALL TO THE BALL WITH THE GREEN BLUE BIG BIG GREEN MAN IN THE BLUE BLUE
;;  TABLE WITH A BLUE BLUE BIG LITTLE BLUE BIG WOMAN BY A LITTLE BALL IN THE
;;  LITTLE BLUE TABLE BY A GREEN MAN BY A GREEN MAN BY THE BIG MAN IN A BALL IN
;;  THE WOMAN BY THE MAN IN THE BIG BIG BIG BIG WOMAN TO A GREEN MAN BY THE LITTLE
;;  LITTLE MAN BY THE GREEN MAN IN THE WOMAN IN A GREEN GREEN WOMAN WITH A GREEN
;;  BLUE BALL BY A LITTLE LITTLE MAN ON A BIG TABLE BY THE LITTLE TABLE BY THE BIG
;;  LITTLE WOMAN IN A MAN ON THE TABLE TO THE LITTLE TABLE BY THE BALL TO THE BALL
;;  BY A MAN WITH A BLUE BLUE GREEN GREEN BIG BLUE GREEN TABLE BY THE MAN BY THE
;;  BALL IN THE LITTLE GREEN BIG LITTLE BLUE BLUE LITTLE BLUE TABLE ON THE BIG
;;  BALL IN THE BIG BLUE TABLE ON A GREEN MAN IN THE LITTLE BLUE GREEN TABLE WITH
;;  THE WOMAN IN THE TABLE WITH A MAN BY A TABLE IN A LITTLE LITTLE LITTLE BIG
;;  BALL BY A WOMAN BY THE TABLE ON A GREEN LITTLE BLUE BALL BY A BALL IN A MAN
;;  WITH A BIG TABLE BY A TABLE TO THE BIG BLUE GREEN WOMAN ON A BLUE BLUE BLUE
;;  MAN WITH A LITTLE BIG LITTLE BALL WITH A LITTLE GREEN TABLE TO THE WOMAN IN A
;;  BLUE MAN TO THE BALL WITH A LITTLE BALL TO A TABLE BY THE TABLE WITH THE TABLE
;;  TO A TABLE WITH THE TABLE ON A BIG WOMAN TO THE BALL IN THE BALL BY THE TABLE
;;  WITH A TABLE WITH THE LITTLE MAN WITH THE BALL ON THE MAN WITH THE TABLE TO
;;  THE GREEN LITTLE WOMAN TO THE BLUE BLUE BLUE WOMAN WITH A MAN BY A TABLE IN
;;  THE BLUE BIG TABLE IN THE BIG BLUE TABLE BY A TABLE TO A TABLE WITH A BIG MAN
;;  IN A WOMAN IN THE BALL ON THE LITTLE BLUE WOMAN WITH THE WOMAN ON A BLUE MAN
;;  IN THE TABLE TO THE BLUE BIG TABLE TO THE MAN BY A WOMAN TO A TABLE BY THE
;;  TABLE TO A WOMAN BY THE LITTLE BIG LITTLE LITTLE LITTLE WOMAN ON THE GREEN BIG
;;  LITTLE WOMAN BY THE TABLE WITH THE LITTLE MAN BY THE GREEN BLUE BIG MAN TO A
;;  TABLE BY A WOMAN TO A MAN IN A GREEN WOMAN BY A WOMAN IN A LITTLE MAN IN A
;;  TABLE ON THE BLUE LITTLE BALL WITH A BLUE MAN BY THE BLUE BLUE BLUE WOMAN TO A
;;  BLUE BALL TO A GREEN BLUE LITTLE GREEN LITTLE GREEN BALL ON A BLUE TABLE IN A
;;  WOMAN TO THE MAN IN A WOMAN BY A BIG TABLE WITH A GREEN BLUE WOMAN WITH THE
;;  BIG BLUE BALL BY A GREEN BALL WITH A GREEN GREEN BLUE TABLE BY THE WOMAN TO
;;  THE BLUE BALL IN A GREEN WOMAN IN THE TABLE ON THE BALL TO A GREEN WOMAN WITH
;;  A MAN TO THE WOMAN WITH THE BLUE BALL WITH A TABLE ON THE BLUE MAN TO THE
;;  LITTLE TABLE IN A MAN BY A WOMAN BY THE BLUE MAN ON THE WOMAN TO A LITTLE
;;  LITTLE BLUE WOMAN TO THE GREEN WOMAN TO A MAN TO A TABLE WITH THE WOMAN WITH
;;  THE BIG BALL WITH THE BIG BALL IN A BALL ON A BLUE BLUE MAN ON A BLUE BALL TO
;;  A TABLE ON A GREEN BLUE BALL IN THE TABLE IN A LITTLE MAN BY A TABLE BY THE
;;  MAN BY A BLUE BIG TABLE ON A LITTLE BALL TO A TABLE TO THE BLUE BALL BY THE
;;  BIG BLUE TABLE WITH A GREEN WOMAN BY A TABLE IN A LITTLE BALL TO THE WOMAN BY
;;  THE TABLE TO THE GREEN BIG BLUE TABLE BY THE TABLE WITH A BLUE WOMAN IN A
;;  WOMAN WITH THE MAN BY THE MAN BY A LITTLE WOMAN IN THE BIG BIG WOMAN TO A
;;  LITTLE BLUE GREEN BIG MAN TO A TABLE IN THE BIG BIG TABLE BY THE MAN BY A BALL
;;  TO THE LITTLE LITTLE GREEN BALL WITH THE BIG WOMAN ON THE MAN BY A TABLE TO A
;;  TABLE TO A WOMAN ON THE LITTLE LITTLE LITTLE BLUE BIG MAN WITH A TABLE IN THE
;;  WOMAN TO A LITTLE WOMAN TO A BLUE LITTLE GREEN GREEN MAN BY THE TABLE WITH A
;;  BIG WOMAN WITH A BLUE MAN WITH THE GREEN BLUE BIG BIG BALL BY A BIG GREEN
;;  LITTLE WOMAN BY A BIG BLUE BALL ON A TABLE ON THE BLUE BALL IN THE TABLE IN
;;  THE MAN TO A LITTLE MAN WITH A BLUE LITTLE BIG BIG TABLE TO THE BLUE BIG BLUE
;;  WOMAN ON THE TABLE ON THE MAN IN A WOMAN IN A GREEN MAN BY A WOMAN IN THE
;;  GREEN MAN WITH THE BALL BY THE BIG WOMAN ON THE WOMAN IN THE GREEN GREEN WOMAN
;;  TO THE GREEN BLUE WOMAN IN A BLUE WOMAN IN THE GREEN WOMAN BY A BALL WITH A
;;  LITTLE LITTLE MAN IN A GREEN MAN IN A BIG WOMAN ON A BALL ON THE MAN ON THE
;;  WOMAN ON THE GREEN LITTLE LITTLE BIG BLUE LITTLE LITTLE TABLE TO THE GREEN
;;  BALL BY A BIG TABLE WITH THE MAN ON A BLUE BALL IN A GREEN GREEN MAN IN A
;;  TABLE TO A BALL IN A MAN TO A TABLE TO THE TABLE TO THE TABLE BY A GREEN BIG
;;  WOMAN ON THE LITTLE BALL WITH THE LITTLE GREEN GREEN BLUE LITTLE BALL ON A
;;  BLUE MAN IN A WOMAN BY THE TABLE IN THE MAN IN THE MAN ON THE BALL ON THE BIG
;;  BLUE TABLE WITH THE WOMAN BY A MAN BY THE MAN IN A BLUE BALL TO A MAN ON THE
;;  BIG BLUE BALL IN A BIG BLUE LITTLE LITTLE BLUE TABLE ON THE WOMAN TO THE MAN
;;  TO THE BLUE WOMAN BY A LITTLE TABLE TO THE BLUE BLUE WOMAN TO A GREEN BIG BALL
;;  IN A BALL BY THE MAN WITH A TABLE IN A WOMAN WITH A TABLE IN A BALL BY A WOMAN
;;  BY A GREEN BLUE TABLE TO THE MAN BY A TABLE IN A BLUE BALL ON A BLUE BALL IN A
;;  BIG BLUE GREEN BIG WOMAN TO A WOMAN WITH A BIG BALL TO THE WOMAN BY THE WOMAN
;;  IN THE LITTLE LITTLE MAN IN A LITTLE GREEN WOMAN TO A MAN ON THE BLUE MAN ON
;;  THE WOMAN TO THE MAN TO A BALL WITH A GREEN GREEN GREEN BIG MAN BY THE GREEN
;;  WOMAN IN THE GREEN BALL WITH THE MAN WITH THE GREEN LITTLE BALL ON A BLUE BIG
;;  TABLE ON THE BLUE TABLE IN A WOMAN TO A GREEN TABLE IN THE MAN TO A BIG MAN TO
;;  THE GREEN BIG MAN ON THE TABLE TO A LITTLE BALL WITH THE BIG MAN ON A BALL IN
;;  THE LITTLE WOMAN WITH A TABLE ON THE MAN IN THE BLUE TABLE TO A WOMAN IN THE
;;  LITTLE BIG LITTLE MAN BY A BLUE WOMAN ON A WOMAN BY A GREEN BALL TO A WOMAN ON
;;  THE GREEN TABLE IN A GREEN BLUE MAN ON A LITTLE WOMAN IN A LITTLE BIG LITTLE
;;  BALL WITH A TABLE IN A BIG BALL TO THE BALL BY A WOMAN WITH THE WOMAN BY A
;;  WOMAN IN THE TABLE ON THE WOMAN ON A WOMAN BY THE LITTLE WOMAN ON A BIG BLUE
;;  TABLE IN A GREEN BALL BY THE TABLE WITH A TABLE WITH A MAN)

;;;
;;;    Section 2.3 rule-based grammar (data-driven)
;;;    
(defparameter *simple-grammar*
  '((s -> (np vp))
    (np -> (d n))
    (vp -> (v np))
    (d -> the a)
    (n -> man ball woman table)
    (v -> hit took saw liked)))

(defvar *grammar* *simple-grammar*)

(defun rule-lhs (rule)
  (first rule))

(defun rule-rhs (rule)
  (rest (rest rule)))

(defun rewrites (category)
  (rule-rhs (assoc category *grammar*)))

;;;
;;;    This will cause problems if any terminal symbol is the same as some nonterminal, e.g., 'a' for adjective
;;;    and the indefinite article.
;;;    
(defun generate (phrase)
  (cond ((listp phrase) (mappend #'generate phrase))
        ((rewrites phrase) (generate (random-elt (rewrites phrase)))) ; Don't call ONE-OF since we want to return a symbol here for terminals.
        (t (list phrase)))) ; Terminal symbol

;;;
;;;    PHRASE is either a symbol or a list.
;;;    1. As a symbol, PHRASE is either a terminal symbol or a nonterminal.
;;;       a. Return a terminal as a singleton list. It will be appended into the final result.
;;;       b. A nonterminal has rewrites, either a set of terminals:
;;;            (the a)
;;;          or nested lists of nonterminals:
;;;            ((v np) ...)
;;;          Either case triggers a recursive call to GENERATE. In the first case, the randomly chosen terminal will result in case 1a above.
;;;          In the second case, one of the nested lists chosen at random will result in case 2 below.
;;;    2. As a list, PHRASE comes from a recursive call to GENERATE from the rewrite rules of a nonterminal. Recursively
;;;       call GENERATE with each of the nonterminals in PHRASE and append the results.
;;;       
(defun generate (phrase)
  (if (listp phrase)
      (mappend #'generate phrase)
      (let ((rewrites (rewrites phrase)))
        (if (null rewrites)
            (list phrase)
            (generate (random-elt rewrites)))) ))

;; * (generate 's)
;;   0: (GENERATE S)
;;     1: (GENERATE (NP VP))
;;       2: (GENERATE NP)
;;         3: (GENERATE (D N))
;;           4: (GENERATE D)
;;             5: (GENERATE THE)
;;             5: GENERATE returned (THE)
;;           4: GENERATE returned (THE)
;;           4: (GENERATE N)
;;             5: (GENERATE MAN)
;;             5: GENERATE returned (MAN)
;;           4: GENERATE returned (MAN)
;;         3: GENERATE returned (THE MAN)
;;       2: GENERATE returned (THE MAN)
;;       2: (GENERATE VP)
;;         3: (GENERATE (V NP))
;;           4: (GENERATE V)
;;             5: (GENERATE LIKED)
;;             5: GENERATE returned (LIKED)
;;           4: GENERATE returned (LIKED)
;;           4: (GENERATE NP)
;;             5: (GENERATE (D N))
;;               6: (GENERATE D)
;;                 7: (GENERATE THE)
;;                 7: GENERATE returned (THE)
;;               6: GENERATE returned (THE)
;;               6: (GENERATE N)
;;                 7: (GENERATE WOMAN)
;;                 7: GENERATE returned (WOMAN)
;;               6: GENERATE returned (WOMAN)
;;             5: GENERATE returned (THE WOMAN)
;;           4: GENERATE returned (THE WOMAN)
;;         3: GENERATE returned (LIKED THE WOMAN)
;;       2: GENERATE returned (LIKED THE WOMAN)
;;     1: GENERATE returned (THE MAN LIKED THE WOMAN)
;;   0: GENERATE returned (THE MAN LIKED THE WOMAN)
;; (THE MAN LIKED THE WOMAN)

;;;
;;;    Ex. 2.2
;;;
(defun terminalp (symbol)
  (null (rewrites symbol)))

;;;
;;;    Now input to GENERATE is always a symbol.
;;;    The function is only called recursively when the selected rewrite is a list, i.e., a rewrite which is a terminal is handled directly.
;;;    Consequently, the only way GENERATE can be called with a terminal as arg is a direct call. In this case there is no need to wrap
;;;    the terminal in a list anymore.
;;;
;;;    The function above returns:
;;;    (generate 'man) => (MAN)
;;;    Whereas the new version yields:
;;;    (generate 'man) => MAN
;;;    
(declaim (ftype (function (symbol) t) generate))

(defun generate (symbol)
  (if (terminalp symbol)
      symbol
      (let ((rewrite (random-elt (rewrites symbol))))
        (if (listp rewrite)
            (mappend #'generate rewrite) ; This also handles the case in *BIGGER-GRAMMAR* where () is a rewrite rule.
            (list rewrite)))) )

(defparameter *bigger-grammar*
  '((s -> (np vp))
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
    (pronoun -> he she it these those that)))

;;;
;;;    Section 2.6
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

(defun extract-sentence (tree)
  (cond ((null tree) '())
        ((atom tree) (list tree))
        (t (destructuring-bind (category &rest terms) tree
             (declare (ignore category))
             (mappend #'extract-sentence terms)))) )

(defun terms (tree)
  (rest tree))

(defun extract-sentence (tree)
  (cond ((null tree) '())
        ((atom tree) (list tree))
        (t (mappend #'extract-sentence (terms tree)))) )

;;;
;;;    Generate the syntax tree but also return the human readable sentence.
;;;    
(defun parse (symbol)
  (let ((tree (generate-tree symbol)))
    (values tree (extract-sentence tree))))

;;;
;;;    GENERATE-ALL generates all of the "phrases" defined by the grammar for a given category. These are not complete sentences
;;;    of course unless the initial category is S.
;;;    
;;;    Do not use on infinite grammar!! (e.g., *bigger-grammar*)
;;;
;;;    1. Initially PHRASE is a category (symbol), e.g., NP.
;;;       It should have rewrites, so this causes recursive calls for each rewrite as MAPPEND maps over the list REWRITES.
;;;       
;;;       When REWRITES is simply a list of terminals, the result is just a list of the terminals each wrapped in a nested list.
;;;       In other words, a "set" of singleton "sets":
;;;       (mappend #'generate-all '(the a)) => ((THE) (A))
;;;       
;;;    2. On a recursive call, the function is dealing with rewrites produced from earlier calls.
;;;       Each rewrite itself will either be a list of further nonterminal categories, it will be a terminal symbol, or it will
;;;       be () having reached the end of a list of nonterminals.
;;;
;;;       In the first case where PHRASE is a list, the function first CDRs down this list recursively, generating results for each
;;;       subcategory. Finally it uses COMBINE-ALL to create the Cartesian product of the results for the first elt of PHRASE and
;;;       the results of the rest of PHRASE.
;;;       
;;;       Alternatively, PHRASE will just be a terminal symbol. In this case, it has no rewrites, and it is packaged as a
;;;       singleton set consisting of the singleton set of the symbol.
;;;
;;;       The final case where PHRASE is () occurs when the function has reached the end of a list of nonterminals in the preceding
;;;       call. The function returns (()) here to represent a singleton set containing the empty set. This will be the basis for
;;;       producing the Cartesian product with additional results.
;;;
;;;       Note: *BIGGER-GRAMMAR* has lists of rewrites which contain () as an element itself rather than just as the end of list
;;;       sentinel. However, GENERATE-ALL is not equipped to handle such infinite languages. An empty list passed as an argument
;;;       to GENERATE-ALL is therefore treated as the end of a list being handled by the second clause of the COND in a call above
;;;       the current one.
;;;
;;;       One minor quirk is that unrecognized symbols can be introduced on top-level calls:
;;;       (generate-all 'pung) => ((PUNG))
;;;
;;;    
(defun generate-all (phrase)
  (cond ((null phrase) (list '())) ; <-- Final REST of PHRASE on recursive call. No () rewrites as with *BIGGER-GRAMMAR*.
        ((listp phrase) (combine-all (generate-all (first phrase)) ; PHRASE is list of nonterminals, e.g., (D N)
                                     (generate-all (rest phrase))))
        (t (let ((rewrites (rewrites phrase))) ; REWRITES is a (possibly empty) list.
             (if (null rewrites)
                 (list (list phrase))
                 (mappend #'generate-all rewrites)))) ))

;;;
;;;    Cartesian product...
;;;    (combine-all '((a) (b)) '((1) (2))) => ((A 1) (B 1) (A 2) (B 2))
;;;
;;;    (combine-all '((a) (b) (c)) '(())) => ((A) (B) (C))
;;;    In other words, if YS is (()), then the output is XS.
;;;    
(defun combine-all (xs ys)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x)
                           (append x y))
                       xs))
           ys))

;; (cartesian-product #{1 2} #{'a 'b}) => #{(1 A) (1 B) (2 A) (2 B)}
;; (cartesian-product #{'a 'b} #{1 2}) => #{(A 1) (A 2) (B 1) (B 2)}
;; (cartesian-product #{'a 'b} #{#{}}) => #{(A #{}) (B #{})}
(defgeneric cartesian-product (s1 s2))
(defmethod cartesian-product ((s1 set) (s2 set))
  (do ((result (make-set :test (hash-table-test (slot-value s1 'elements))))
       (iter1 (iterators:make-set-iterator s1)))
      ((null (iterators:has-next-p iter1)) result)
    (do ((elt1 (iterators:next iter1))
         (iter2 (iterators:make-set-iterator s2)))
        ((null (iterators:has-next-p iter2)))
      (add-elt result (list elt1 (iterators:next iter2)))) ))

;;;
;;;    Ex. 2.3
;;;    See Learn Prolog Now! Ch. 7 ex. 3
;;;
(defparameter *logic-grammar*
  '((proposition -> (term) (not term) (term connective term))
    (not -> not)
    (connective -> and or => <=> xor)
    (term -> p q r s)))

(defparameter *logic-grammar*
  '((proposition -> p q r (negation proposition) (proposition connective proposition))
    (negation -> not)
    (connective -> and or => <=> xor)))

;;;
;;;    Ex. 2.4
;;;
(defun cross-product (f xs ys)
  (mappend #'(lambda (y)
               (mapcar #'(lambda (x)
                           (funcall f x y))
                       xs))
           ys))

(defun combine-all (xs ys)
  (cross-product #'append xs ys))
