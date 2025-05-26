;;;; datamuse.asd

(defsystem #:datamuse
  :name "datamuse"
  :description "Common Lisp library for accessing the Datamuse word-finding API"
  :author "modula t."
  :license "MIT"
  :version "1.2"
  :homepage "https://github.com/defaultxr/datamuse"
  :bug-tracker "https://github.com/defaultxr/datamuse/issues"
  :mailto "modula-t at pm dot me"
  :source-control (:git "git@github.com:defaultxr/datamuse.git")
  :depends-on (#:alexandria
               #:drakma
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "datamuse")))
