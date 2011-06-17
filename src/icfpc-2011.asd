;;;; -*- mode: lisp -*-
(defpackage #:icfpc-2011-system
  (:use #:cl #:asdf))

(in-package #:icfpc-2011-system)

(defsystem :icfpc-2011
  :name "icfpc-2011"
  :author "Skobochka"
  :licence "Lessor Lisp General Public License"
  :version "0.1"
  :description "Solution for ICFPC-2011"
  :depends-on (#:bordeaux-threads #:iterate #:cl-match)
  :serial t
  :components ((:file "package")
	       (:file "cards")
	       (:file "run")))

