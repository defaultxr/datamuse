;;;; datamuse.lisp

(in-package #:datamuse)

(alexandria:define-constant +words-query-parameters+
    '((meaning "ml")
      (sound "sl")
      (spelling "sp")
      (popular-adjectives "rel_jja")
      (popular-nouns "rel_jjb")
      (synonyms "rel_syn")
      (triggers "rel_trg")
      (antonyms "rel_ant")
      (hypernyms "rel_spc")
      (hyponyms "rel_gen")
      (holonyms "rel_com")
      (meronyms "rel_par")
      (popular-followers "rel_bga")
      (popular-predecessors "rel_bgb")
      (rhymes "rel_rhy")
      (approximate-rhymes "rel_nry")
      (homophones "rel_hom")
      (consonant-match "rel_cns")
      (vocabulary "v")
      (topics "topics")
      (left-context "lc")
      (right-context "rc")
      (maximum "max")
      (with-definitions "md") ;; FIX: implement these
      (with-parts-of-speech "md")
      (with-syllable-count "md")
      (with-pronunciation "md")
      (with-frequency "md")
      (query-echo "qe"))
  :test 'equal
  :documentation "Alist mapping `words' parameter names to the API's query parameter names.

See https://www.datamuse.com/api/ for a detailed explanation of the API.

See also: `words', `words*'")

(defun words* (&rest args)
  "Perform a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ .

Each keyword argument maps to one of the endpoint's query parameters via the `+words-query-parameters+' alist. They are defined in this function in the same order they are defined in the API documentation, so refer to that for a detailed explanation of each.

Note that this function includes all of the data returned by the API as Sexprs (i.e. including various metadata for each result). If you want a list of just the words themselves, use `words'.

See also: `words', `+words-query-parameters+'."
  (yason:parse
   (let ((drakma:*text-content-types* '(("application" . "json") ("text"))))
     (drakma:http-request "https://api.datamuse.com/words"
                          :parameters (loop :for (param value) :on args :by #'cddr
                                         :collect (cons (cadr (assoc (intern (symbol-name param) :datamuse)
                                                                     +words-query-parameters+))
                                                        value))))
   :object-as :alist))

(defun words (&rest args)
  "Perform a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ , to get a list of words matching the query specified.

Each keyword argument maps to one of the endpoint's query parameters via the `+words-query-parameters+' alist. They are defined in this function in the same order they are defined in the API documentation, so refer to that for a detailed explanation of each.

Note that this function returns just a list of words. To get all the results including the metadata, use `words*'.

Examples:

;; Get a list of words that rhyme with \"foobar\":
;; (words :rhymes \"foobar\")

;; Get a list of words that start with \"f\" and end with \"uck\":
;; (words :spelling \"f*uck\")

See also: `words*', `+words-query-parameters+'."
  (mapcar (lambda (x)
            (cdr (assoc "word" x :test #'string-equal)))
          (apply 'words* args)))

;; make slime/swank show the "correct" argument list
#+swank
(let ((arglist
       (swank::make-arglist :key-p t :keyword-args (loop :for param :in (mapcar 'car +words-query-parameters+)
                                                      :collect (swank::make-keyword-arg (alexandria:make-keyword param) param nil)))))
  (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words)) argument-forms)
    arglist)
  (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words*)) argument-forms)
    arglist))
