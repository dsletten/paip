;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Of all the languages I know, I like Lisp the best, simply because it's the most beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               bananas.lisp
;;;;
;;;;   Started:            Mon Aug 10 17:31:10 2020
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

;; (defpackage :bananas (:use :common-lisp))

;; (in-package :bananas)
(in-package :norvig-gps2)

(defparameter *banana-ops* (list (op 'climb-on-chair
                                     :preconditions '(chair-at-middle-room at-middle-room on-floor)
                                     :add-list '(at-bananas on-chair)
                                     :delete-list '(at-middle-room on-floor))
                                 (op 'push-chair-from-door-to-middle-room
                                     :preconditions '(chair-at-door at-door)
                                     :add-list '(chair-at-middle-room at-middle-room)
                                     :delete-list '(chair-at-door at-door))
                                 (op 'walk-from-door-to-middle-room
                                     :preconditions '(at-door on-floor)
                                     :add-list '(at-middle-room)
                                     :delete-list '(at-door))
                                 (op 'grasp-bananas
                                     :preconditions '(at-bananas empty-handed)
                                     :add-list '(has-bananas)
                                     :delete-list '(empty-handed))
                                 (op 'drop-ball
                                     :preconditions '(has-ball)
                                     :add-list '(empty-handed)
                                     :delete-list '(has-ball))
                                 (op 'eat-bananas
                                     :preconditions '(has-bananas)
                                     :add-list '(empty-handed not-hungry)
                                     :delete-list '(has-bananas hungry))))
