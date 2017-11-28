(ql:quickload :jonathan)
(ql:quickload :cl-ppcre)
(ql:quickload :ltk)

(defpackage :mogegui
  (:use :common-lisp :cl-user :ltk)
  (:export #:gui-start))
(in-package :mogegui)

(load "moge-gui.lisp" :external-format :utf-8)

(gui-start)
(sb-ext:exit)
