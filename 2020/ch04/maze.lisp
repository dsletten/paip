;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               maze.lisp
;;;;
;;;;   Started:            Sun Aug 16 22:04:07 2020
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

(in-package :norvig-gps2)

(defun make-maze-ops (pair)
  "Make maze ops (symmetric relation)"
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(defun make-maze-op (here there)
  "Make an operator to move between two places"
  (op `(move from ,here to ,there)
      :preconditions `((at ,here))
      :add-list `((at ,there))
      :delete-list `((at ,here))))

(defparameter *maze-ops*
  (mapcan #'make-maze-ops
          '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
            (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
            (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))


;;;
;;;    Cycles!
;;;
;;(gps '((at 25)) '((at 1)))
;;(gps '((at 25)) '((at 5)))

;; (#S(OP
;;     :ACTION (MOVE FROM 1 TO 2)
;;     :PRECONDITIONS ((AT 1))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 1 TO 2)) (AT 2))
;;     :DELETE-LIST ((AT 1)))
;;  #S(OP
;;     :ACTION (MOVE FROM 2 TO 1)
;;     :PRECONDITIONS ((AT 2))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 2 TO 1)) (AT 1))
;;     :DELETE-LIST ((AT 2)))
;;  #S(OP
;;     :ACTION (MOVE FROM 2 TO 3)
;;     :PRECONDITIONS ((AT 2))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 2 TO 3)) (AT 3))
;;     :DELETE-LIST ((AT 2)))
;;  #S(OP
;;     :ACTION (MOVE FROM 3 TO 2)
;;     :PRECONDITIONS ((AT 3))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 3 TO 2)) (AT 2))
;;     :DELETE-LIST ((AT 3)))
;;  #S(OP
;;     :ACTION (MOVE FROM 3 TO 4)
;;     :PRECONDITIONS ((AT 3))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 3 TO 4)) (AT 4))
;;     :DELETE-LIST ((AT 3)))
;;  #S(OP
;;     :ACTION (MOVE FROM 4 TO 3)
;;     :PRECONDITIONS ((AT 4))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 4 TO 3)) (AT 3))
;;     :DELETE-LIST ((AT 4)))
;;  #S(OP
;;     :ACTION (MOVE FROM 4 TO 9)
;;     :PRECONDITIONS ((AT 4))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 4 TO 9)) (AT 9))
;;     :DELETE-LIST ((AT 4)))
;;  #S(OP
;;     :ACTION (MOVE FROM 9 TO 4)
;;     :PRECONDITIONS ((AT 9))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 9 TO 4)) (AT 4))
;;     :DELETE-LIST ((AT 9)))
;;  #S(OP
;;     :ACTION (MOVE FROM 9 TO 14)
;;     :PRECONDITIONS ((AT 9))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 9 TO 14)) (AT 14))
;;     :DELETE-LIST ((AT 9)))
;;  #S(OP
;;     :ACTION (MOVE FROM 14 TO 9)
;;     :PRECONDITIONS ((AT 14))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 14 TO 9)) (AT 9))
;;     :DELETE-LIST ((AT 14)))
;;  #S(OP
;;     :ACTION (MOVE FROM 9 TO 8)
;;     :PRECONDITIONS ((AT 9))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 9 TO 8)) (AT 8))
;;     :DELETE-LIST ((AT 9)))
;;  #S(OP
;;     :ACTION (MOVE FROM 8 TO 9)
;;     :PRECONDITIONS ((AT 8))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 8 TO 9)) (AT 9))
;;     :DELETE-LIST ((AT 8)))
;;  #S(OP
;;     :ACTION (MOVE FROM 8 TO 7)
;;     :PRECONDITIONS ((AT 8))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 8 TO 7)) (AT 7))
;;     :DELETE-LIST ((AT 8)))
;;  #S(OP
;;     :ACTION (MOVE FROM 7 TO 8)
;;     :PRECONDITIONS ((AT 7))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 7 TO 8)) (AT 8))
;;     :DELETE-LIST ((AT 7)))
;;  #S(OP
;;     :ACTION (MOVE FROM 7 TO 12)
;;     :PRECONDITIONS ((AT 7))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 7 TO 12)) (AT 12))
;;     :DELETE-LIST ((AT 7)))
;;  #S(OP
;;     :ACTION (MOVE FROM 12 TO 7)
;;     :PRECONDITIONS ((AT 12))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 12 TO 7)) (AT 7))
;;     :DELETE-LIST ((AT 12)))
;;  #S(OP
;;     :ACTION (MOVE FROM 12 TO 13)
;;     :PRECONDITIONS ((AT 12))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 12 TO 13)) (AT 13))
;;     :DELETE-LIST ((AT 12)))
;;  #S(OP
;;     :ACTION (MOVE FROM 13 TO 12)
;;     :PRECONDITIONS ((AT 13))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 13 TO 12)) (AT 12))
;;     :DELETE-LIST ((AT 13)))
;;  #S(OP
;;     :ACTION (MOVE FROM 12 TO 11)
;;     :PRECONDITIONS ((AT 12))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 12 TO 11)) (AT 11))
;;     :DELETE-LIST ((AT 12)))
;;  #S(OP
;;     :ACTION (MOVE FROM 11 TO 12)
;;     :PRECONDITIONS ((AT 11))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 11 TO 12)) (AT 12))
;;     :DELETE-LIST ((AT 11)))
;;  #S(OP
;;     :ACTION (MOVE FROM 11 TO 6)
;;     :PRECONDITIONS ((AT 11))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 11 TO 6)) (AT 6))
;;     :DELETE-LIST ((AT 11)))
;;  #S(OP
;;     :ACTION (MOVE FROM 6 TO 11)
;;     :PRECONDITIONS ((AT 6))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 6 TO 11)) (AT 11))
;;     :DELETE-LIST ((AT 6)))
;;  #S(OP
;;     :ACTION (MOVE FROM 11 TO 16)
;;     :PRECONDITIONS ((AT 11))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 11 TO 16)) (AT 16))
;;     :DELETE-LIST ((AT 11)))
;;  #S(OP
;;     :ACTION (MOVE FROM 16 TO 11)
;;     :PRECONDITIONS ((AT 16))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 16 TO 11)) (AT 11))
;;     :DELETE-LIST ((AT 16)))
;;  #S(OP
;;     :ACTION (MOVE FROM 16 TO 17)
;;     :PRECONDITIONS ((AT 16))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 16 TO 17)) (AT 17))
;;     :DELETE-LIST ((AT 16)))
;;  #S(OP
;;     :ACTION (MOVE FROM 17 TO 16)
;;     :PRECONDITIONS ((AT 17))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 17 TO 16)) (AT 16))
;;     :DELETE-LIST ((AT 17)))
;;  #S(OP
;;     :ACTION (MOVE FROM 17 TO 22)
;;     :PRECONDITIONS ((AT 17))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 17 TO 22)) (AT 22))
;;     :DELETE-LIST ((AT 17)))
;;  #S(OP
;;     :ACTION (MOVE FROM 22 TO 17)
;;     :PRECONDITIONS ((AT 22))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 22 TO 17)) (AT 17))
;;     :DELETE-LIST ((AT 22)))
;;  #S(OP
;;     :ACTION (MOVE FROM 21 TO 22)
;;     :PRECONDITIONS ((AT 21))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 21 TO 22)) (AT 22))
;;     :DELETE-LIST ((AT 21)))
;;  #S(OP
;;     :ACTION (MOVE FROM 22 TO 21)
;;     :PRECONDITIONS ((AT 22))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 22 TO 21)) (AT 21))
;;     :DELETE-LIST ((AT 22)))
;;  #S(OP
;;     :ACTION (MOVE FROM 22 TO 23)
;;     :PRECONDITIONS ((AT 22))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 22 TO 23)) (AT 23))
;;     :DELETE-LIST ((AT 22)))
;;  #S(OP
;;     :ACTION (MOVE FROM 23 TO 22)
;;     :PRECONDITIONS ((AT 23))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 23 TO 22)) (AT 22))
;;     :DELETE-LIST ((AT 23)))
;;  #S(OP
;;     :ACTION (MOVE FROM 23 TO 18)
;;     :PRECONDITIONS ((AT 23))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 23 TO 18)) (AT 18))
;;     :DELETE-LIST ((AT 23)))
;;  #S(OP
;;     :ACTION (MOVE FROM 18 TO 23)
;;     :PRECONDITIONS ((AT 18))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 18 TO 23)) (AT 23))
;;     :DELETE-LIST ((AT 18)))
;;  #S(OP
;;     :ACTION (MOVE FROM 23 TO 24)
;;     :PRECONDITIONS ((AT 23))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 23 TO 24)) (AT 24))
;;     :DELETE-LIST ((AT 23)))
;;  #S(OP
;;     :ACTION (MOVE FROM 24 TO 23)
;;     :PRECONDITIONS ((AT 24))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 24 TO 23)) (AT 23))
;;     :DELETE-LIST ((AT 24)))
;;  #S(OP
;;     :ACTION (MOVE FROM 24 TO 19)
;;     :PRECONDITIONS ((AT 24))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 24 TO 19)) (AT 19))
;;     :DELETE-LIST ((AT 24)))
;;  #S(OP
;;     :ACTION (MOVE FROM 19 TO 24)
;;     :PRECONDITIONS ((AT 19))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 19 TO 24)) (AT 24))
;;     :DELETE-LIST ((AT 19)))
;;  #S(OP
;;     :ACTION (MOVE FROM 19 TO 20)
;;     :PRECONDITIONS ((AT 19))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 19 TO 20)) (AT 20))
;;     :DELETE-LIST ((AT 19)))
;;  #S(OP
;;     :ACTION (MOVE FROM 20 TO 19)
;;     :PRECONDITIONS ((AT 20))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 20 TO 19)) (AT 19))
;;     :DELETE-LIST ((AT 20)))
;;  #S(OP
;;     :ACTION (MOVE FROM 20 TO 15)
;;     :PRECONDITIONS ((AT 20))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 20 TO 15)) (AT 15))
;;     :DELETE-LIST ((AT 20)))
;;  #S(OP
;;     :ACTION (MOVE FROM 15 TO 20)
;;     :PRECONDITIONS ((AT 15))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 15 TO 20)) (AT 20))
;;     :DELETE-LIST ((AT 15)))
;;  #S(OP
;;     :ACTION (MOVE FROM 15 TO 10)
;;     :PRECONDITIONS ((AT 15))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 15 TO 10)) (AT 10))
;;     :DELETE-LIST ((AT 15)))
;;  #S(OP
;;     :ACTION (MOVE FROM 10 TO 15)
;;     :PRECONDITIONS ((AT 10))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 10 TO 15)) (AT 15))
;;     :DELETE-LIST ((AT 10)))
;;  #S(OP
;;     :ACTION (MOVE FROM 10 TO 5)
;;     :PRECONDITIONS ((AT 10))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 10 TO 5)) (AT 5))
;;     :DELETE-LIST ((AT 10)))
;;  #S(OP
;;     :ACTION (MOVE FROM 5 TO 10)
;;     :PRECONDITIONS ((AT 5))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 5 TO 10)) (AT 10))
;;     :DELETE-LIST ((AT 5)))
;;  #S(OP
;;     :ACTION (MOVE FROM 20 TO 25)
;;     :PRECONDITIONS ((AT 20))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 20 TO 25)) (AT 25))
;;     :DELETE-LIST ((AT 20)))
;;  #S(OP
;;     :ACTION (MOVE FROM 25 TO 20)
;;     :PRECONDITIONS ((AT 25))
;;     :ADD-LIST ((EXECUTING (MOVE FROM 25 TO 20)) (AT 20))
;;     :DELETE-LIST ((AT 25))))

(defparameter *gps-maze-ops* '(#S(OP
                                  :ACTION (MOVE FROM 1 TO 2)
                                  :PRECONDITIONS ((AT 1))
                                  :ADD-LIST ((AT 2))
                                  :DELETE-LIST ((AT 1)))
                               #S(OP
                                  :ACTION (MOVE FROM 2 TO 1)
                                  :PRECONDITIONS ((AT 2))
                                  :ADD-LIST ((AT 1))
                                  :DELETE-LIST ((AT 2)))
                               #S(OP
                                  :ACTION (MOVE FROM 2 TO 3)
                                  :PRECONDITIONS ((AT 2))
                                  :ADD-LIST ((AT 3))
                                  :DELETE-LIST ((AT 2)))
                               #S(OP
                                  :ACTION (MOVE FROM 3 TO 2)
                                  :PRECONDITIONS ((AT 3))
                                  :ADD-LIST ((AT 2))
                                  :DELETE-LIST ((AT 3)))
                               #S(OP
                                  :ACTION (MOVE FROM 3 TO 4)
                                  :PRECONDITIONS ((AT 3))
                                  :ADD-LIST ((AT 4))
                                  :DELETE-LIST ((AT 3)))
                               #S(OP
                                  :ACTION (MOVE FROM 4 TO 3)
                                  :PRECONDITIONS ((AT 4))
                                  :ADD-LIST ((AT 3))
                                  :DELETE-LIST ((AT 4)))) )
