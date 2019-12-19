;;;; package.lisp

(defpackage #:datamuse
  (:use #:cl
        #:alexandria)
  (:export #:+words-query-parameters+
           #:parameter-documentation
           #:words
           #:words*
           #:suggestions
           #:suggestions*))
