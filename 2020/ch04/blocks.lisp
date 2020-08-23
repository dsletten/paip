;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp...not just beautiful, but strangely beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               blocks.lisp
;;;;
;;;;   Started:            Sun Aug 23 07:11:44 2020
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
;; (load "/home/slytobias/lisp/packages/test.lisp")

;; (defpackage :blocks (:use :common-lisp :test))

;; (in-package :blocks)

(in-package :norvig-gps2)

(defun make-block-ops (blocks)
  (let ((ops '()))
    (dolist (a blocks ops)
      (dolist (b blocks)
        (unless (eq a b)
          (dolist (c blocks)
            (unless (or (eq c a)
                        (eq c b))
              (push (move-op a b c) ops)))
          (push (move-op a 'table b) ops)
          (push (move-op a b 'table) ops)))) ))

;;;
;;;    Space on table is not a thing. It's always true--never a precondition...
;;;    
(defun move-op (obj src dest)
  "Create operator to move OBJ from SRC to DEST."
  (op `(move ,obj from ,src to ,dest)
      :preconditions `((space on ,obj) ,@(if (eq dest 'table) '() `((space on ,dest))) (,obj on ,src))
      :add-list (move-ons obj src dest)
      :delete-list (move-ons obj dest src)))

;; (defun move-op (obj src dest)
;;   "Create operator to move OBJ from SRC to DEST."
;;   (op `(move ,obj from ,src to ,dest)
;;       :preconditions `((space on ,obj) (space on ,dest) (,obj on ,src))
;;       :add-list (move-ons obj src dest)
;;       :delete-list (move-ons obj dest src)))

(defun move-ons (obj src dest)
  (if (eq src 'table)
      `((,obj on ,dest))
      `((,obj on ,dest) (space on ,src))))

;(gps '((a on table) (b on table) (space on a) (space on b) (space on table)) '((a on b) (b on table)))
;(gps '((a on table) (b on table) (space on a) (space on b)) '((a on b) (b on table)))
;; ???
;(gps '((a on table) (b on table) (space on a) (space on b)) '((a on b))) 
;(gps '((a on b) (b on table) (space on a)) '((b on a)))
;(use (make-block-ops '(a b c)))
;(gps '((a on b) (b on c) (c on table) (space on a)) '((b on a) (c on b)))
