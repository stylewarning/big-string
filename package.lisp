;;;; package.lisp
;;;; Copyright (c) 2013 Robert Smith

(defpackage #:big-string
  (:use #:cl)

  ;; Type and Data Structure Operations
  (:export #:big-string
           #:make-big-string
           #:big-string-p
           #:copy-big-string)
  
  ;; String-Like Operations
  (:export #:big-string-length
           #:big-string-append
           #:big-string-char
           #:big-string-substring
           #:string-of-big-string))

