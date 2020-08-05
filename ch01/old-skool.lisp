;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               old-skool.lisp
;;;;
;;;;   Started:            Tue Dec 28 00:31:08 2010
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

(defpackage old-skool (:use common-lisp))

(in-package old-skool)

(prog (list depth temp restlist)
   (setq restlist (list (cons (read) 0)))
   a
   (cond ((not restlist) (return 'done))
         (t (setq list (uncons (uncons restlist restlist) depth))
            (cond ((atom list)
                   (mapc 'prin1 (list '"atom:" list '"." 'depth depth))
                   (terpri))
                  (t (setq temp (uncons list list))
                     (cond (list (setq restlist (cons (cons list depth) restlist))))
                     (setq restlist (cons (cons temp (add1 depth)) restlist)) ))))
   (go a))

(prog nil
   ((label atomprint (lambda (restlist)
                       (cond ((not restlist) (return 'done))
                             ((atom (caar restlist))
                              (mapc 'prin1
                                    (list '"atom:" (caar restlist)
                                          '"." 'depth (cdar restlist)))
                              (terpri)
                              (atomprint (cdr restlist)))
                             (t (atomprint (graft (list (cons (caaar restlist)
                                                              (add1 (cdar restlist))))
                                                  (and (cdaar restlist) (list (cons (cdaar restlist)
                                                                                    (cdar restlist))))
                                                  (cdr restlist)))) )))
    (list (cons (read) 0))))

(defun atomprint (expression &optional (depth 0))
  "Print each atom in EXPRESSION, along with its depth of nesting."
  (if (atom expression)
      (format t "~&ATOM: ~A, DEPTH ~D" expression depth)
      (dolist (elt expression)
        (atomprint elt (1+ depth)))) )
