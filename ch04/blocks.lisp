;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               blocks.lisp
;;;;
;;;;   Started:            Wed Oct 17 19:00:31 2012
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
;(load "/Users/dsletten/lisp/packages/test.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/books/PAIP/ch04/gps2.lisp")

(defpackage :blocks (:use :common-lisp :gps2 :lang) (:shadowing-import-from :gps2 :debug))

(in-package :blocks)

(defun make-block-ops (blocks)
  (let ((ops '()))
    (dolist (a blocks)
      (dolist (b blocks)
        (unless (eq a b)
          (dolist (c blocks)
            (unless (or (eq c a) (eq c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops))))
    ops))

;;;
;;;    (SPACE ON <X>) means that there is nothing on top of <X>:
;;;    -Another block can be placed on <X>
;;;    -<X> can be moved without having to remove anything from it first.
;;;    
(defun move-op (a b c)
  (op `(move ,a from ,b to ,c)
      :preconds `((space on ,a) (space on ,c) (,a on ,b))
      :add-list (move-ons a b c)
      :del-list (move-ons a c b)))

(defun move-ons (a b c)
  (if (eq b 'table)
      `((,a on ,c)) ; Always space on the table?...
      `((,a on ,c) (space on ,b))))

;;;
;;;    Given n blocks, n * n(n-1) operators
;;;    n blocks can each start at n starting points (n-1 other blocks + 1 table)
;;;    and move to n-1 destinations (n-2 other blocks + table).
;;;    
;; * (make-block-ops '(a b))

;; (#S(OP
;;     :ACTION (MOVE B FROM A TO TABLE)
;;     :PRECONDS ((SPACE ON B) (SPACE ON TABLE) (B ON A))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM A TO TABLE)) (B ON TABLE)
;;                (SPACE ON A))
;;     :DEL-LIST ((B ON A)))
;;  #S(OP
;;     :ACTION (MOVE B FROM TABLE TO A)
;;     :PRECONDS ((SPACE ON B) (SPACE ON A) (B ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM TABLE TO A)) (B ON A))
;;     :DEL-LIST ((B ON TABLE) (SPACE ON A)))
;;  #S(OP
;;     :ACTION (MOVE A FROM B TO TABLE)
;;     :PRECONDS ((SPACE ON A) (SPACE ON TABLE) (A ON B))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM B TO TABLE)) (A ON TABLE)
;;                (SPACE ON B))
;;     :DEL-LIST ((A ON B)))
;;  #S(OP
;;     :ACTION (MOVE A FROM TABLE TO B)
;;     :PRECONDS ((SPACE ON A) (SPACE ON B) (A ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM TABLE TO B)) (A ON B))
;;     :DEL-LIST ((A ON TABLE) (SPACE ON B))))
;; * (make-block-ops '(a b c))

;; (#S(OP
;;     :ACTION (MOVE C FROM B TO TABLE)
;;     :PRECONDS ((SPACE ON C) (SPACE ON TABLE) (C ON B))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM B TO TABLE)) (C ON TABLE)
;;                (SPACE ON B))
;;     :DEL-LIST ((C ON B)))
;;  #S(OP
;;     :ACTION (MOVE C FROM TABLE TO B)
;;     :PRECONDS ((SPACE ON C) (SPACE ON B) (C ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM TABLE TO B)) (C ON B))
;;     :DEL-LIST ((C ON TABLE) (SPACE ON B)))
;;  #S(OP
;;     :ACTION (MOVE C FROM B TO A)
;;     :PRECONDS ((SPACE ON C) (SPACE ON A) (C ON B))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM B TO A)) (C ON A) (SPACE ON B))
;;     :DEL-LIST ((C ON B) (SPACE ON A)))
;;  #S(OP
;;     :ACTION (MOVE C FROM A TO TABLE)
;;     :PRECONDS ((SPACE ON C) (SPACE ON TABLE) (C ON A))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM A TO TABLE)) (C ON TABLE)
;;                (SPACE ON A))
;;     :DEL-LIST ((C ON A)))
;;  #S(OP
;;     :ACTION (MOVE C FROM TABLE TO A)
;;     :PRECONDS ((SPACE ON C) (SPACE ON A) (C ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM TABLE TO A)) (C ON A))
;;     :DEL-LIST ((C ON TABLE) (SPACE ON A)))
;;  #S(OP
;;     :ACTION (MOVE C FROM A TO B)
;;     :PRECONDS ((SPACE ON C) (SPACE ON B) (C ON A))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE C FROM A TO B)) (C ON B) (SPACE ON A))
;;     :DEL-LIST ((C ON A) (SPACE ON B)))
;;  #S(OP
;;     :ACTION (MOVE B FROM C TO TABLE)
;;     :PRECONDS ((SPACE ON B) (SPACE ON TABLE) (B ON C))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM C TO TABLE)) (B ON TABLE)
;;                (SPACE ON C))
;;     :DEL-LIST ((B ON C)))
;;  #S(OP
;;     :ACTION (MOVE B FROM TABLE TO C)
;;     :PRECONDS ((SPACE ON B) (SPACE ON C) (B ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM TABLE TO C)) (B ON C))
;;     :DEL-LIST ((B ON TABLE) (SPACE ON C)))
;;  #S(OP
;;     :ACTION (MOVE B FROM C TO A)
;;     :PRECONDS ((SPACE ON B) (SPACE ON A) (B ON C))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM C TO A)) (B ON A) (SPACE ON C))
;;     :DEL-LIST ((B ON C) (SPACE ON A)))
;;  #S(OP
;;     :ACTION (MOVE B FROM A TO TABLE)
;;     :PRECONDS ((SPACE ON B) (SPACE ON TABLE) (B ON A))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM A TO TABLE)) (B ON TABLE)
;;                (SPACE ON A))
;;     :DEL-LIST ((B ON A)))
;;  #S(OP
;;     :ACTION (MOVE B FROM TABLE TO A)
;;     :PRECONDS ((SPACE ON B) (SPACE ON A) (B ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM TABLE TO A)) (B ON A))
;;     :DEL-LIST ((B ON TABLE) (SPACE ON A)))
;;  #S(OP
;;     :ACTION (MOVE B FROM A TO C)
;;     :PRECONDS ((SPACE ON B) (SPACE ON C) (B ON A))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE B FROM A TO C)) (B ON C) (SPACE ON A))
;;     :DEL-LIST ((B ON A) (SPACE ON C)))
;;  #S(OP
;;     :ACTION (MOVE A FROM C TO TABLE)
;;     :PRECONDS ((SPACE ON A) (SPACE ON TABLE) (A ON C))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM C TO TABLE)) (A ON TABLE)
;;                (SPACE ON C))
;;     :DEL-LIST ((A ON C)))
;;  #S(OP
;;     :ACTION (MOVE A FROM TABLE TO C)
;;     :PRECONDS ((SPACE ON A) (SPACE ON C) (A ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM TABLE TO C)) (A ON C))
;;     :DEL-LIST ((A ON TABLE) (SPACE ON C)))
;;  #S(OP
;;     :ACTION (MOVE A FROM C TO B)
;;     :PRECONDS ((SPACE ON A) (SPACE ON B) (A ON C))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM C TO B)) (A ON B) (SPACE ON C))
;;     :DEL-LIST ((A ON C) (SPACE ON B)))
;;  #S(OP
;;     :ACTION (MOVE A FROM B TO TABLE)
;;     :PRECONDS ((SPACE ON A) (SPACE ON TABLE) (A ON B))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM B TO TABLE)) (A ON TABLE)
;;                (SPACE ON B))
;;     :DEL-LIST ((A ON B)))
;;  #S(OP
;;     :ACTION (MOVE A FROM TABLE TO B)
;;     :PRECONDS ((SPACE ON A) (SPACE ON B) (A ON TABLE))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM TABLE TO B)) (A ON B))
;;     :DEL-LIST ((A ON TABLE) (SPACE ON B)))
;;  #S(OP
;;     :ACTION (MOVE A FROM B TO C)
;;     :PRECONDS ((SPACE ON A) (SPACE ON C) (A ON B))
;;     :ADD-LIST ((GPS2::EXECUTING (MOVE A FROM B TO C)) (A ON C) (SPACE ON B))
;;     :DEL-LIST ((A ON B) (SPACE ON C))))

;;;
;;;    How specific does this need to be?
;;;
;;;    The first one declares (b on table) as a goal, but the 2nd one leaves (a on table) implicit.
;;;    When given explicitly, the order doesn't seem to matter.
;;;
;;;             A
;;;      A B => B
;;;      
;; * (gps '((a on table) (b on table) (space on a) (space on b) (space on table)) '((a on b) (b on table)))
;;;
;;;      A    B
;;;      B => A
;; ((START) (EXECUTING (MOVE A FROM TABLE TO B)))
;; * (gps '((a on b) (b on table) (space on a) (space on table)) '((b on a)))

;; ((START) (EXECUTING (MOVE A FROM B TO TABLE))
;;  (EXECUTING (MOVE B FROM TABLE TO A)))
;; * (gps '((a on b) (b on table) (space on a) (space on table)) '((b on a) (a on table)))

;; ((START) (EXECUTING (MOVE A FROM B TO TABLE))
;;  (EXECUTING (MOVE B FROM TABLE TO A)))
;; * (gps '((a on b) (b on table) (space on a) (space on table)) '((a on table) (b on a)))

;; ((START) (EXECUTING (MOVE A FROM B TO TABLE))
;;  (EXECUTING (MOVE B FROM TABLE TO A)))

;;;
;;;    Goal order matters here:
;;;
;;;      A    C
;;;      B => B
;;;      C    A
;;;    
;; * (use (make-block-ops '(a b c)))
;; * (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) '((b on a) (c on b)))
;; ((START) (EXECUTING (MOVE A FROM B TO TABLE)) (EXECUTING (MOVE B FROM C TO A))
;;  (EXECUTING (MOVE C FROM TABLE TO B)))
;; * (gps '((a on b) (b on c) (c on table) (space on a) (space on table)) '((c on b) (b on a)))
;; NIL

