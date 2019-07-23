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
      (with-definitions "md" "d")
      (with-parts-of-speech "md" "p")
      (with-syllable-count "md" "s")
      (with-pronunciation "md" "r")
      (with-frequency "md" "f")
      (query-echo "qe"))
  :test 'equal
  :documentation "Alist mapping `words' parameter names to the API's query parameter names.

See https://www.datamuse.com/api/ for a detailed explanation of the API.

See also: `words', `words*'")

(defun words* (&rest parameters)
  "Perform a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ .

Each keyword argument maps to one of the endpoint's query parameters via the `+words-query-parameters+' alist. They are defined in this function in the same order they are defined in the API documentation, so refer to that for a detailed explanation of each.

Note that this function includes all of the data returned by the API as Sexprs (i.e. including various metadata for each result). If you want a list of just the words themselves, use `words'.

See also: `words', `+words-query-parameters+', `suggestions'."
  (yason:parse
   (let ((drakma:*text-content-types* '(("application" . "json") ("text"))))
     (drakma:http-request "https://api.datamuse.com/words"
                          :parameters (loop :for (param value) :on parameters :by #'cddr
                                         :for lookup = (assoc param +words-query-parameters+ :test 'string=)
                                         :if (string= (cadr lookup) "md")
                                         :collect (caddr lookup) :into md
                                         :else
                                         :collect (cons (cadr lookup) value) :into result
                                         :finally (return (append result (when md (list (cons "md" (apply 'concatenate 'string md)))))))))
   :object-as :alist))

(defun words (&rest parameters)
  "Perform a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ , to get a list of words matching the query specified.

Each keyword argument maps to one of the endpoint's query parameters via the `+words-query-parameters+' alist. They are defined in this function in the same order they are defined in the API documentation, so refer to that for a detailed explanation of each.

Note that this function returns just a list of words. To get all the results including the metadata, use `words*'.

Examples:

;; Get a list of words that rhyme with \"foobar\":
;; (words :rhymes \"foobar\")

;; Get a list of words that start with \"f\" and end with \"uck\":
;; (words :spelling \"f*uck\")

See also: `words*', `+words-query-parameters+', `suggestions'."
  (mapcar (lambda (x)
            (cdr (assoc "word" x :test #'string-equal)))
          (apply 'words* parameters)))

(defun suggestions* (string &key (max 10) vocabulary)
  "Performs a query to the Datamuse API's /sug endpoint as described at https://www.datamuse.com/api/ , to get a list of suggestions to partially-typed queries, similar to the auto-suggest feature of some search engines.

STRING is the input string to generate suggestions for, MAX is the maximum number of results, VOCABULARY is the vocabulary (which defaults to English).

See also: `suggestions', `words'."
  (yason:parse
   (let ((drakma:*text-content-types* '(("application" . "json") ("text"))))
     (drakma:http-request "https://api.datamuse.com/sug"
                          :parameters (append
                                       (list
                                        (cons "s" string)
                                        (cons "max" (write-to-string max)))
                                       (when vocabulary
                                         (cons "v" vocabulary)))))
   :object-as :alist))

(defun suggestions (string &key (max 10) vocabulary)
  "Performs a query to the Datamuse API's /sug endpoint as described at https://www.datamuse.com/api/ , to get a list of suggestions to partially-typed queries, similar to the auto-suggest feature of some search engines.

STRING is the input string to generate suggestions for, MAX is the maximum number of results, VOCABULARY is the vocabulary (which defaults to English).

Examples:

;; (suggestions \"jack of\")
=> (\"jack of all trades\" \"jack off\" \"jack of the buttery\" \"jack of all trades and master of none\" \"back off\" \"back office\" \"jack frost\" \"jack o lantern\" \"jack-of-all-trades\")

See also: `words', `suggestions*'."
  (mapcar (lambda (x)
            (cdr (assoc "word" x :test #'string-equal)))
          (suggestions* string :max max :vocabulary vocabulary)))

;; make slime/swank show the "correct" argument list
#+swank
(let ((arglist
       (swank::make-arglist :key-p t :keyword-args (loop :for param :in (mapcar 'car +words-query-parameters+)
                                                      :collect (swank::make-keyword-arg (alexandria:make-keyword param) param nil)))))
  (dolist (s (list 'words 'words*))
    (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql s)) argument-forms)
      arglist)))
