;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Lisp is a programmable programming language.
;;;;   -- John Foderaro
;;;;
;;;;   Name:               20-questions.lisp
;;;;
;;;;   Started:            Tue Aug 14 22:00:12 2012
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
;;;;   Notes: Should be able to serialize/deserialize game.
;;;;
;;;;   Compare Norvig's short but sweet implementation!!!
;;;;
;;;;
(load "/Users/dsletten/lisp/packages/lang.lisp")
(load "/Users/dsletten/lisp/packages/collections.lisp")
(load "/Users/dsletten/lisp/packages/test.lisp")

(defpackage :20-questions
  (:shadowing-import-from :collections :intersection :set :set-difference :subsetp :union)
  (:use :common-lisp :lang :collections :test))

(in-package :20-questions)

;;;
;;;    QUESTION class (binary tree node)
;;;    
(defclass question ()
  ((text :reader text :initarg :text :initform (error "Must supply text for question."))
   (yes :accessor yes :initarg :yes :initform nil)
   (no :accessor no :initarg :no :initform nil)))

(defmethod print-object ((q question) stream)
  (print-unreadable-object (q stream :type t)
    (format stream "~S" (text q))))

(defun make-question (&key text yes no)
  (make-instance 'question :text text :yes yes :no no))

(defun questionp (obj)
  (typep obj 'question))

;;;
;;;    ANSWER class
;;;    
(defclass answer ()
  ((value :reader value :initarg :value :initform (error "Must supply value for answer."))))

(defmethod print-object ((a answer) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~S" (value a))))

(defun make-answer (value)
  (make-instance 'answer :value value))

(defun answerp (obj)
  (typep obj 'answer))

;;;
;;;    GAME class
;;;    
(defclass game ()
  ((db :initform nil)
   (count :accessor question-count :initform 0)
   (answers :initform (make-set :test #'equalp))))

(defmethod print-object ((g game) stream)
  (print-unreadable-object (g stream :type t)
    (format stream "(~D)" (question-count g))))

(defun make-game ()
  (make-instance 'game))

(defun gamep (obj)
  (typep obj 'game))

;;;
;;;    Generic function in COLLECTIONS package...
;;;    A GAME is a COLLECTION??
;;;    
(defmethod contains ((g game) (a answer) &key test)
  (declare (ignore test)) ; ?!?!?
  (with-slots (answers) g
    (contains answers (value a))))

;;;
;;;    Generic function in COLLECTIONS package...
;;;    
(defmethod emptyp ((g game))
  (with-slots (db) g
    (null db)))

(defun play (&optional (game (make-game)))
  (with-slots (db) game
    (when (emptyp game)
      (format t "Let's get this game started.~%")
      (initialize game))
    (play-game game db))
  game)

(defun play-game (game question)
  (let ((response (ask-question question)))
    (if (affirmativep response)
        (evaluate-response game question 'yes (yes question))
        (evaluate-response game question 'no (no question)))) )

;;;
;;;    This approach inspired by Slade ch. 9 PROCESS-NODE
;;;    The YES and NO slots of a QUESTION may contain:
;;;        1. Another QUESTION
;;;        2. An ANSWER
;;;        3. NIL
;;;    
(defgeneric evaluate-response (game question failed-branch node))

(defmethod evaluate-response ((game game) (question question) failed-branch (new-question question))
  (declare (ignore question failed-branch))
  (play-game game new-question))
  
(defmethod evaluate-response ((game game) (question question) failed-branch (answer answer))
  (if (affirmativep (confirm-answer answer))
      (print "I got it!")
      (shift-question game question answer failed-branch)))

(defmethod evaluate-response ((game game) (question question) failed-branch dead-end)
  (declare (ignore dead-end))
  (add-answer game question failed-branch))

(defun shift-question (game question answer failed-branch)
  (let* ((new-answer (prompt-for-answer game))
         (new-question (prompt-for-question new-answer))
         (new-branch (prompt-for-branch new-answer)))
    (update-question question failed-branch new-question)
    (update-question new-question new-branch new-answer)
    (update-question new-question (opposite-branch new-branch) answer)
    (update-game game new-answer)))

(defun opposite-branch (branch)
  (if (affirmativep branch)
      'no
      'yes))

(defun update-game (game answer)
  (with-slots (count answers) game
    (incf count)
    (setf answers (add-elt answers (value answer)))) )
    
(defun ask-question (question)
  (get-yes-no-response (format nil "~A " (text question))))

(defun confirm-answer (answer)
  (get-yes-no-response (format nil "Were you thinking of ~A ~A? " (get-article (value answer)) (value answer))))

(defun get-yes-no-response (prompt)
  (let ((response (prompt-read prompt :allow-empty nil)))
    (if (or (affirmativep response)
            (negativep response))
        response
        (get-yes-no-response prompt))))

(defun initialize (game)
  (let* ((answer (prompt-for-answer game))
         (question (prompt-for-question answer))
         (branch (prompt-for-branch answer)))
    (with-slots (db count answers) game
      (setf db question
            answers (add-elt answers (value answer)))
      (incf count))
    (update-question question branch answer)))

(defun update-question (question branch update)
  (if (affirmativep branch)
      (setf (yes question) update)
      (setf (no question) update)))

(defun add-answer (game question failed-branch)
  (let ((answer (prompt-for-answer game)))
    (update-question question failed-branch answer)
    (update-game game answer)))

;;;
;;;    Not used. Replaced by SHIFT-QUESTION above??
;;;    
(defun add-question (game question failed-branch)
  (let* ((answer (prompt-for-answer game))
         (new-question (prompt-for-question answer))
         (new-branch (prompt-for-branch answer)))
    (update-question question failed-branch new-question)
    (update-question new-question new-branch answer)
    (update-game game answer)))

(defun prompt-for-answer (game)
  (let ((answer (make-answer (strip-article (prompt-read "What are you thinking of? " :allow-empty nil)))) )
    (if (contains game answer)
        (progn
          (format t "There is already ~A ~A in this game.~%" (get-article (value answer)) (value answer))
          (prompt-for-answer game))
        answer)))

(defun prompt-for-question (answer)
  (make-question :text
                 (prompt-read (format nil
                                      "Enter a question to differentiate between ~A ~A and other objects: "
                                      (get-article (value answer))
                                      (value answer))
                              :allow-empty nil)))

(defun prompt-for-branch (answer)
  (get-yes-no-response (format nil
                               "How would you answer that question for ~A ~A? "
                               (get-article (value answer)) (value answer))))

(defun affirmativep (s)
  (or (eql s 'yes)
      (string-equal s "yes")
      (string-equal s "y")))

(defun negativep (s)
  (or (eql s 'no)
      (string-equal s "no")
      (string-equal s "n")))

(defun get-article (phrase)
  (cond ((or (special-case-p phrase) (and (not (unicornp phrase)) (starts-with-vowel-p phrase))) "an")
        (t "a")))

(defun unicornp (phrase)
  (some #'(lambda (oddball)
            (search oddball phrase :test #'char-equal))
        '("unicorn" "uniform" "unique" "union" "united" "unilateral")))

(defun special-case-p (phrase)
  (some #'(lambda (oddball)
            (search oddball phrase :test #'char-equal))
        '("hour" "honor" "herb" "honest" "honorable" "honorary" "hourglass" "hourly" "hors d'oeuvre")))

(defun starts-with-vowel-p (phrase)
  (find (char phrase 0) "aeiou" :test #'char-equal))

(defun strip-article (phrase)
  (cond ((starts-with phrase "an " :test #'char-equal) (subseq phrase 3))
        ((starts-with phrase "a " :test #'char-equal) (subseq phrase 2))
        (t phrase)))
