;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   What I like about Lisp is that you can feel the bits between your toes.
;;;;   -- Drew McDermott
;;;;
;;;;   Name:               comment-fsm.lisp
;;;;
;;;;   Started:            Sat Oct 24 17:49:10 2020
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;   A FSM for recognizing C/Java comments: /* ... */, //
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
(load "/home/slytobias/lisp/packages/io.lisp")

(defpackage :comment-fsm (:use :common-lisp :test :io))

(in-package :comment-fsm)

(defun recognize-comment (s)
  (let ((in (make-string-input-stream s)))
    (labels ((consume ()
               (read-char in nil nil))
             (start (ch)
               (case ch
                 (#\/ (slash0 (consume)))
                 ((nil) t)
                 (otherwise (start (consume)))) )
             (slash0 (ch)
               (case ch
                 (#\/ (slash1 (consume)))
                 (#\* (star0 (consume)))
                 ((nil) nil)
                 (otherwise (start (consume)))) )
             (slash1 (ch)
               (case ch
                 (#\newline (start (consume)))
                 ((nil) t) ; No newline OK...
                 (otherwise (slash1 (consume)))) )
             (star0 (ch)
               (case ch
                 (#\* (star1 (consume)))
                 ((nil) nil)
                 (otherwise (star0 (consume)))) )
             (star1 (ch)
               (case ch
                 (#\* (star1 (consume)))
                 (#\/ (start (consume)))
                 ((nil) nil)
                 (otherwise (star0 (consume)))) ))
      (start (consume)))) )

(deftest test-recognize-comment ()
  (check
   (not (recognize-comment "/"))
   (not (recognize-comment "/*"))
   (not (recognize-comment "/*/"))
   (not (recognize-comment "/* *x/"))
   (recognize-comment "/**/")
   (recognize-comment "/* comment */")
   (recognize-comment "/****/")
   (recognize-comment "/*** x ***/")
   (recognize-comment "Is //this not pung?")
   (recognize-comment "Is / /this not pung?")
   (recognize-comment "Is /* this */ not pung?")
   (recognize-comment "Is /* this /**/ not pung?")
   (recognize-comment "Is /* this ****/ not pung?")
   (not (recognize-comment "Is /* this "))
   (not (recognize-comment "Is /* this *"))))

(defun remove-comment (s)
  (let ((in (make-string-input-stream s))
        (out (make-string-output-stream)))
    (labels ((consume ()
               (read-char in nil nil))
             (start (ch)
               (case ch
                 (#\/ (slash0 (consume) ch))
                 ((nil) t)
                 (otherwise (write-char ch out)
                            (start (consume)))) )
             (slash0 (ch ch0)
               (case ch
                 (#\/ (slash1 (consume)))
                 (#\* (star0 (consume)))
                 ((nil) nil)
                 (otherwise (write-char ch0 out)
                            (write-char ch out)
                            (start (consume)))) )
             (slash1 (ch)
               (case ch
                 (#\newline (write-char #\Space out)
                            (write-char ch out)
                            (start (consume)))
                 ((nil) (write-char #\Space out)
                  t) ; No newline OK...
                 (otherwise (slash1 (consume)))) )
             (star0 (ch)
               (case ch
                 (#\* (star1 (consume)))
                 ((nil) nil)
                 (otherwise (star0 (consume)))) )
             (star1 (ch)
               (case ch
                 (#\* (star1 (consume)))
                 (#\/ (write-char #\Space out) (start (consume)))
                 ((nil) nil)
                 (otherwise (star0 (consume)))) ))
      (start (consume))
      (get-output-stream-string out))))

(deftest test-remove-comment ()
  (check
   (remove-comment "Is //this not pung?")
   (remove-comment "Is / /this not pung?")
   (remove-comment "Is /* this */ not pung?")
   (remove-comment "Is /* this /**/ not pung?")
   (remove-comment "Is /* this ****/ not pung?")
   (not (remove-comment "Is /* this "))
   (not (remove-comment "Is /* this *"))))

