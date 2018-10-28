;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; McCLIM example (https://github.com/mcclim/mcclim
;;
;; Running:
;; 1. (ql:quickload "mcclim")
;; 2. load, run:
;;     (app:app-main)
;;
(in-package :common-lisp-user)

(defpackage "APP"
  (:use :clim :clim-lisp)
  (:export "APP-MAIN"))

(in-package :app)

;;; Define a application-frame (a.k.a. application window in traditional GUI's).

(define-application-frame superapp ()
  ()
  ;; :panes section describes different parts of the
  ;; application-frame. This application has only one pane.
  (:panes
   (int :interactor :height 400 :width 600))

  ;; :layouts section describes how the panes are layed out.
  ;; This application has one layout named "default" which has a single pane.
  (:layouts
   (default int)))

;;; Following function launches an instance of "superapp" application-frame.
(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))
