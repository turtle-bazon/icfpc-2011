;;;; -*- mode: lisp -*-

(defsystem :icfpc-2011
  :name "icfpc-2011"
  :author "Skobochka"
  :licence "Lessor Lisp General Public License"
  :version "0.1"
  :description "Solution for ICFPC-2011"
  :depends-on (bordeaux-threads iterate cl-match)
  :components ((:file "package")
	       (:file "run" (:depends-on ("package")))))

