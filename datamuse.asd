;;;; datamuse.asd

(asdf:defsystem #:datamuse
  :description "Common Lisp library for accessing the Datamuse word-finding API"
  :author "modula t."
  :license "MIT"
  :version "0.1"
  :depends-on (#:alexandria
               #:drakma
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "datamuse")))
