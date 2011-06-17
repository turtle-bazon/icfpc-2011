;;;; -*- mode: lisp -*-
(defpackage #:icfpc-system
  (:use #:cl #:asdf))

(in-package #:icfpc-system)

(defsystem :icfpc
  :name "icfpc"
  :author "Skobochka"
  :licence "Lessor Lisp General Public License"
  :version "0.1"
  :description "Solution for ICFPC-2011"
  :depends-on ()
  :serial t
  :components ((:file "package")
	       (:file "game")
	       (:file "cards")
	       (:file "io")))
