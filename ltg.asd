(defpackage #:ltg-system
  (:use #:cl #:asdf))

(in-package #:ltg-system)
 
(defsystem :ltg
  :serial t
  :depends-on ()
  :components ((:module "src"
			:serial t
			:components ((:file "packages" )
				     (:file "cards")))))
