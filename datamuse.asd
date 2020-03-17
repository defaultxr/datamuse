;;;; datamuse.asd

(asdf:defsystem #:datamuse
  :description "Common Lisp library for accessing the Datamuse word-finding API"
  :author "modula t."
  :license "MIT"
  :version "1.1"
  :depends-on (#:alexandria
               #:drakma
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "datamuse")
               ;; swank-extensions.lisp conditionally loaded at the end of datamuse.lisp
               ))
