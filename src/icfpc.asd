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
	       (:file "cards")
	       (:file "game")
	       (:file "io")
	       (:file "brains"))
  :in-order-to ((test-op (test-op icfpc-tests)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern (symbol-name '#:run-all-tests)
                             :icfpc))))
