;;;; big-string.asd
;;;; Copyright (c) 2013 Robert Smith

(asdf:defsystem #:big-string
  :serial t
  :description "Big strings, similar to Java's StringBuilder."
  :author "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (see LICENSE)"
  :components ((:file "package")
               (:file "big-string")))

