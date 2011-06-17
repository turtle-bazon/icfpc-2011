;;;; -*- mode: lisp -*-

(defpackage #:icfpc
    (:use #:cl)
    (:export
      #:run-all-tests)
  )

;; don't hide warnings
(declaim (optimize (sb-ext:inhibit-warnings 0)))
