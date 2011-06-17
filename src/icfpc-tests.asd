;;;; -*- mode: lisp -*-
(defpackage #:icfpc-tests-system
  (:use #:cl #:asdf))

(in-package #:icfpc-tests-system)

(defsystem :icfpc-tests
  :name "icfpc-tests"
  :author "Skobochka"
  :licence "Lessor Lisp General Public License"
  :version "0.1"
  :description "Tests for Solution for ICFPC-2011"
  :depends-on (icfpc lift)
  :serial t
  :components ((:file "tests")))
