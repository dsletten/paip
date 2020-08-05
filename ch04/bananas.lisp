;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp isn't a language, it's a building material.
;;;;   -- Alan Kay
;;;;
;;;;   Name:               bananas.lisp
;;;;
;;;;   Started:            Tue Oct 16 15:03:03 2012
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
(load "/Users/dsletten/lisp/books/PAIP/ch04/gps2.lisp")
(load "/Users/dsletten/lisp/packages/lang.lisp")

(defpackage :bananas (:use :common-lisp :gps2 :lang) (:shadowing-import-from :gps2 :debug))

(in-package :bananas)

(defparameter *banana-ops* (list (op 'climb-on-chair
                                     :preconds '(chair-at-middle-room at-middle-room on-floor)
                                     :add-list '(at-bananas on-chair)
                                     :del-list '(at-middle-room on-floor))
                                 (op 'push-chair-from-door-to-middle-room
                                     :preconds '(chair-at-door at-door)
                                     :add-list '(chair-at-middle-room at-middle-room)
                                     :del-list '(chair-at-door at-door))
                                 (op 'walk-from-door-to-middle-room
                                     :preconds '(at-door on-floor)
                                     :add-list '(at-middle-room)
                                     :del-list '(at-door))
                                 (op 'grasp-bananas
                                     :preconds '(at-bananas empty-handed)
                                     :add-list '(has-bananas)
                                     :del-list '(empty-handed))
                                 (op 'drop-ball
                                     :preconds '(has-ball)
                                     :add-list '(empty-handed)
                                     :del-list '(has-ball))
                                 (op 'eat-bananas
                                     :preconds '(has-bananas)
                                     :add-list '(empty-handed not-hungry)
                                     :del-list '(has-bananas hungry))))
