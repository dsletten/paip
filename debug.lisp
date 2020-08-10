;#!/usr/local/bin/sbcl --script

;;;;   Hey, Emacs, this is a -*- Mode: Lisp; Syntax: Common-Lisp -*- file!
;;;;
;;;;   Programming should be fun. Programs should be beautiful.
;;;;   -- Paul Graham
;;;;
;;;;   Name:               debug.lisp
;;;;
;;;;   Started:            Mon Aug 10 02:45:56 2020
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

(defpackage :debug
  (:use :common-lisp)
  (:shadow :debug)
  (:export :debug :undebug :dbg :dbg-indent))

(in-package :debug)

(defvar *debug-ids* '() "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *debug-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *debug-ids* (union ids *debug-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *debug-ids* (if (null ids)
                        '()
                        (set-difference *debug-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *debug-ids*)
    (fresh-line *debug-io*)
    ;; (dotimes (i indent)
    ;;   (princ "  " *debug-io*))
    (format *debug-io* "~VT" (* indent 2))
    (apply #'format *debug-io* format-string args)))

