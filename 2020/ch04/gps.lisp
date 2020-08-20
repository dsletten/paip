;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               gps.lisp
;;;;
;;;;   Started:            Fri Jul 31 03:59:19 2020
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
;;;;   Notes: My revision of Norvig's version -> Recursive GPS function
;;;;
;;;;   Solution is sensitive to ordering of operations. When neighbor op precedes fixing car,
;;;;   that becomes the chosen solution.
;;;;
;;;;
(load "/home/slytobias/lisp/packages/test.lisp")
(load "/home/slytobias/lisp/packages/collections.lisp")

(defpackage :gps (:use :common-lisp :collections :test) (:shadowing-import-from :collections :union :set :subsetp :intersection))

(in-package :gps)

(defclass operator ()
  ((action :reader action :initarg :action :type symbol) ; Possibly a list... See maze operators
   (preconditions :reader preconditions :initarg :preconditions :type list)
   (add-to-state :reader add-to-state :initarg :add-to-state :initform #{} :type set)
   (remove-from-state :reader remove-from-state :initarg :remove-from-state :initform #{} :type set)))

(defmethod initialize-instance :after ((op operator) &rest initargs)
  (declare (ignore initargs))
  (with-slots (add-to-state remove-from-state) op
    (when (null add-to-state)
      (setf add-to-state #{}))
    (when (null remove-from-state)
      (setf remove-from-state #{}))))

(defmethod print-object ((op operator) stream)
  (print-unreadable-object (op stream :type t)
    (format stream "~A" (action op))))

(defun make-operator (action &key preconditions add-to-state remove-from-state)
  (make-instance 'operator :action action :preconditions preconditions :add-to-state add-to-state :remove-from-state remove-from-state))

(defun gps (state goals operators)
  (declare (special state operators))
  (if (gps-aux goals)
      'solved
      'failed))

(defun gps-aux (goals)
  (every #'achieve goals))

(defun achieve (goal)
  (declare (special state))
  (or (contains state goal)
      (some #'apply-op (possible-solutions goal))))

(defun possible-solutions (goal)
  (declare (special operators))
  (remove-if-not #'(lambda (op) (solutionp op goal)) operators))

(defun solutionp (op goal)
  "Does the given operator OP satisfy GOAL?"
  (contains (add-to-state op) goal))

(defun apply-op (op)
  (declare (special state))
  (cond ((gps-aux (preconditions op))
         (print `(executing ,(action op)))
         (setf state (union (difference state (remove-from-state op))
                            (add-to-state op))))
        (t nil)))

(defparameter *school-ops* (list (make-operator 'drive-son-to-school
                                                :preconditions '(son-at-home car-works)
                                                :add-to-state #{'son-at-school}
                                                :remove-from-state #{'son-at-home})
                                 (make-operator 'shop-installs-battery
                                                :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                                :add-to-state #{'car-works})
                                 (make-operator 'tell-shop-problem
                                                :preconditions '(in-communication-with-shop)
                                                :add-to-state #{'shop-knows-problem})
                                 (make-operator 'telephone-shop
                                                :preconditions '(know-phone-number)
                                                :add-to-state #{'in-communication-with-shop})
                                 (make-operator 'look-up-number
                                                :preconditions '(have-phone-book)
                                                :add-to-state #{'know-phone-number})
                                 (make-operator 'give-shop-money
                                                :preconditions '(have-money)
                                                :add-to-state #{'shop-has-money}
                                                :remove-from-state #{'have-money})))

(defparameter *school-neighbor-ops1* (list (make-operator 'drive-son-to-school
                                                          :preconditions '(son-at-home car-works)
                                                          :add-to-state #{'son-at-school}
                                                          :remove-from-state #{'son-at-home})
                                           (make-operator 'shop-installs-battery
                                                          :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                                          :add-to-state #{'car-works})
                                           (make-operator 'tell-shop-problem
                                                          :preconditions '(in-communication-with-shop)
                                                          :add-to-state #{'shop-knows-problem})
                                           (make-operator 'telephone-shop
                                                          :preconditions '(know-phone-number)
                                                          :add-to-state #{'in-communication-with-shop})
                                           (make-operator 'look-up-number
                                                          :preconditions '(have-phone-book)
                                                          :add-to-state #{'know-phone-number})
                                           (make-operator 'give-shop-money
                                                          :preconditions '(have-money)
                                                          :add-to-state #{'shop-has-money}
                                                          :remove-from-state #{'have-money})
                                           (make-operator 'neighbor-drives-son
                                                          :preconditions '(son-at-home)
                                                          :add-to-state #{'son-at-school}
                                                          :remove-from-state #{'son-at-school})))

(defparameter *school-neighbor-ops2* (list (make-operator 'neighbor-drives-son
                                                          :preconditions '(son-at-home)
                                                          :add-to-state #{'son-at-school}
                                                          :remove-from-state #{'son-at-school})
                                           (make-operator 'drive-son-to-school
                                                          :preconditions '(son-at-home car-works)
                                                          :add-to-state #{'son-at-school}
                                                          :remove-from-state #{'son-at-home})
                                           (make-operator 'shop-installs-battery
                                                          :preconditions '(car-needs-battery shop-knows-problem shop-has-money)
                                                          :add-to-state #{'car-works})
                                           (make-operator 'tell-shop-problem
                                                          :preconditions '(in-communication-with-shop)
                                                          :add-to-state #{'shop-knows-problem})
                                           (make-operator 'telephone-shop
                                                          :preconditions '(know-phone-number)
                                                          :add-to-state #{'in-communication-with-shop})
                                           (make-operator 'look-up-number
                                                          :preconditions '(have-phone-book)
                                                          :add-to-state #{'know-phone-number})
                                           (make-operator 'give-shop-money
                                                          :preconditions '(have-money)
                                                          :add-to-state #{'shop-has-money}
                                                          :remove-from-state #{'have-money})))

(defparameter *famous-ops* (list (make-operator 'strike-it-rich
                                                :preconditions '(poor)
                                                :add-to-state #{'rich}
                                                :remove-from-state #{'poor})
                                 (make-operator 'achieve-fame
                                                :preconditions '(unknown)
                                                :add-to-state #{'famous}
                                                :remove-from-state #{'unknown})))

;; * (gps #{'poor 'unknown} '(rich famous) *famous-ops*)
;; (gps #{'poor 'unknown} '(rich famous) *famous-ops*)

;; (EXECUTING STRIKE-IT-RICH) 
;; (EXECUTING ACHIEVE-FAME) 
;; SOLVED
;; * (gps #{'poor 'unknown} '(famous rich) *famous-ops*)
;; (gps #{'poor 'unknown} '(famous rich) *famous-ops*)

;; (EXECUTING ACHIEVE-FAME) 
;; (EXECUTING STRIKE-IT-RICH) 
;; SOLVED

(defparameter *gps-maze-ops* (list (make-operator '(move from 1 to 2)
                                                  :preconditions '((at 1))
                                                  :add-to-state #{'(at 2)}
                                                  :remove-from-state #{'(at 1)})
                                   (make-operator '(move from 2 to 1)
                                                  :preconditions '((at 2))
                                                  :add-to-state #{'(at 1)}
                                                  :remove-from-state #{'(at 2)})
                                   (make-operator '(move from 2 to 3)
                                                  :preconditions '((at 2))
                                                  :add-to-state #{'(AT 3)}
                                                  :remove-from-state #{'(AT 2)})
                                   (make-operator '(move from 3 to 2)
                                                  :preconditions '((at 3))
                                                  :add-to-state #{'(at 2)}
                                                  :remove-from-state #{'(at 3)})
                                   (make-operator '(move from 3 to 4)
                                                  :preconditions '((at 3))
                                                  :add-to-state #{'(at 4)}
                                                  :remove-from-state #{'(at 3)})
                                   (make-operator '(move from 4 to 3)
                                                  :preconditions '((at 4))
                                                  :add-to-state #{'(at 3)}
                                                  :remove-from-state #{'(at 4)})))

(defparameter *banana-ops* (list (make-operator 'climb-on-chair
                                                :preconditions '(chair-at-middle-room at-middle-room on-floor)
                                                :add-to-state #{'at-bananas 'on-chair}
                                                :remove-from-state #{'at-middle-room 'on-floor})
                                 (make-operator 'push-chair-from-door-to-middle-room
                                                :preconditions '(chair-at-door at-door)
                                                :add-to-state #{'chair-at-middle-room 'at-middle-room}
                                                :remove-from-state #{'chair-at-door 'at-door})
                                 (make-operator 'walk-from-door-to-middle-room
                                                :preconditions '(at-door on-floor)
                                                :add-to-state #{'at-middle-room}
                                                :remove-from-state #{'at-door})
                                 (make-operator 'grasp-bananas
                                                :preconditions '(at-bananas empty-handed)
                                                :add-to-state #{'has-bananas}
                                                :remove-from-state #{'empty-handed})
                                 (make-operator 'drop-ball
                                                :preconditions '(has-ball)
                                                :add-to-state #{'empty-handed}
                                                :remove-from-state #{'has-ball})
                                 (make-operator 'eat-bananas
                                                :preconditions '(has-bananas hungry)
                                                :add-to-state #{'empty-handed 'not-hungry}
                                                :remove-from-state #{'has-bananas 'hungry})))


