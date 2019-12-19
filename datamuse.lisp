;;;; datamuse.lisp
;; FIX: add functionality to catch errors, invalid responses, etc.

(in-package #:datamuse)

(define-constant +words-query-parameters+
    '((meaning "ml" nil "Means like constraint: require that the results have a meaning related to this string value, which can be any word or sequence of words. (This is effectively the reverse dictionary feature of OneLook.)
")
      (sound "sl" nil "Sounds like constraint: require that the results are pronounced similarly to this string of characters. (If the string of characters doesn't have a known pronunciation, the system will make its best guess using a text-to-phonemes algorithm.)")
      (spelling "sp" nil "Spelled like constraint: require that the results are spelled similarly to this string of characters, or that they match this wildcard pattern. A pattern can include any combination of alphanumeric characters, spaces, and two reserved characters that represent placeholders — * (which matches any number of characters) and ? (which matches exactly one character).")
      (popular-adjectives "rel_jja" nil "Popular nouns modified by the given adjective, per Google Books Ngrams. i.e. gradual → increase")
      (popular-nouns "rel_jjb" nil "Popular adjectives used to modify the given noun, per Google Books Ngrams. i.e. beach → sandy")
      (synonyms "rel_syn" nil "Synonyms (words contained within the same WordNet synset). i.e. ocean → sea")
      (triggers "rel_trg" nil "\"Triggers\" (words that are statistically associated with the query word in the same piece of text.). i.e. cow → milking")
      (antonyms "rel_ant" nil "Antonyms (per WordNet). i.e. late → early")
      (hypernyms "rel_spc" nil "\"Kind of\" (direct hypernyms, per WordNet). i.e. gondola → boat")
      (hyponyms "rel_gen" nil "\"More general than\" (direct hyponyms, per WordNet). i.e. boat → gondola")
      (holonyms "rel_com" nil "\"Comprises\" (direct holonyms, per WordNet). i.e. car → accelerator")
      (meronyms "rel_par" nil "\"Part of\" (direct meronyms, per WordNet). i.e. trunk → tree")
      (popular-followers "rel_bga" nil "Frequent followers (w′ such that P(w′|w) ≥ 0.001, per Google Books Ngrams). i.e. wreak → havoc")
      (popular-predecessors "rel_bgb" nil "Frequent predecessors (w′ such that P(w|w′) ≥ 0.001, per Google Books Ngrams). i.e. havoc → wreak")
      (rhymes "rel_rhy" nil "Rhymes (\"perfect\" rhymes, per RhymeZone). i.e. spade → aid")
      (approximate-rhymes "rel_nry" nil "Approximate rhymes (per RhymeZone). i.e. forest → chorus")
      (homophones "rel_hom" nil "Homophones (sound-alike words). i.e. course → coarse")
      (consonant-match "rel_cns" nil "Consonant match. i.e. sample → simple")
      (vocabulary "v" nil "Identifier for the vocabulary to use. If none is provided, a 550,000-term vocabulary of English words and multiword expressions is used. (The value es specifies a 500,000-term vocabulary of words from Spanish-language books. The value enwiki specifies an approximately 6 million-term vocabulary of article titles from the English-language Wikipedia, updated monthly.) Please contact us to set up a custom vocabulary for your application.")
      (topics "topics" nil "Topic words: An optional hint to the system about the theme of the document being written. Results will be skewed toward these topics. At most 5 words can be specified. Space or comma delimited. Nouns work best.")
      (left-context "lc" nil "Left context: An optional hint to the system about the word that appears immediately to the left of the target word in a sentence. (At this time, only a single word may be specified.)")
      (right-context "rc" nil "Right context: An optional hint to the system about the word that appears immediately to the right of the target word in a sentence. (At this time, only a single word may be specified.)")
      (maximum "max" nil "Maximum number of results to return, not to exceed 1000. (default: 100)")
      (with-definitions "md" "d" "Include definitions in the results. The definitions are from WordNet. If the word is an inflected form (such as the plural of a noun or a conjugated form of a verb), then an additional defHeadword field will be added indicating the base form from which the definitions are drawn.")
      (with-parts-of-speech "md" "p" "Include parts of speech in the results. \"n\" means noun, \"v\" means verb, \"adj\" means adjective, \"adv\" means adverb, and \"u\" means that the part of speech is none of these or cannot be determined. Multiple entries will be added when the word's part of speech is ambiguous, with the most popular part of speech listed first. This field is derived from an analysis of Google Books Ngrams data.")
      (with-syllable-count "md" "s" "Include syllable count in the results. In certain cases the number of syllables may be ambiguous, in which case the system's best guess is chosen based on the entire query.")
      (with-pronunciation "md" "r" "Include pronunciation in the results. This is the system's best guess for the pronunciation of the word or phrase. The format of the pronunication is a space-delimited list of Arpabet phoneme codes. If you add \"&ipa=1\" to your API query, the pronunciation string will instead use the International Phonetic Alphabet. Note that for terms that are very rare or outside of the vocabulary, the pronunciation will be guessed based on the spelling. In certain cases the pronunciation may be ambiguous, in which case the system's best guess is chosen based on the entire query.")
      (with-frequency "md" "f" "Include word frequency in the results. The value is the number of times the word (or multi-word phrase) occurs per million words of English text according to Google Books Ngrams. The API makes an effort to ensure that metadata values are consistent with the sense or senses of the word that best match the API query. For example, the word \"refuse\" is tagged as a verb (\"v\") in the results of a search for words related to \"deny\" but as a noun (\"n\") in the results of a search for words related to \"trash\". And \"resume\" is shown to have 2 syllables in a search of rhymes for \"exhume\" but 3 syllables in a search of rhymes for \"macrame\". There are occasional errors in this guesswork, particularly with pronunciations. Metadata is available for both English (default) and Spanish (v=es) vocabularies.")
      (query-echo "qe" nil "Query echo: The presence of this parameter asks the system to prepend a result to the output that describes the query string from some other parameter, specified as the argument value. This is useful for looking up metadata about specific words. For example, /words?sp=flower&qe=sp&md=fr can be used to get the pronunciation and word frequency for flower."))
  :test 'equal
  :documentation "Alist mapping `words' and `words*' parameter names to the API's query parameter names and the official documentation for each. You can also use the `parameter-documentation' function to easily get the documentation for a specific parameter.

See https://www.datamuse.com/api/ for more information about the API.

See also: `parameter-documentation', `words', `words*'")

(defun parameter-documentation (parameter)
  "Get the documentation string for the specified parameter of the `words' function.

See also: `+words-query-parameters+', `words', `words*'"
  (cadddr (find-if (lambda (x) (position parameter x :test #'string-equal)) +words-query-parameters+)))

(defun words* (&rest parameters)
  "Get a list of results matching the query specified, including the metadata for each. See the `+words-query-parameters+' alist or the `parameter-documentation' function for documentation on each parameter. This function works by performing a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ .

Note that the with-* parameters are simply boolean toggles that set whether specific metadata is included in the results; they don't actually change which results are returned.

This function includes all of the data returned by the API as alists (i.e. including various metadata for each result). If you just want a list of the words themselves, use `words'.

See also: `words', `parameter-documentation', `+words-query-parameters+', `suggestions'."
  (yason:parse
   (let ((drakma:*text-content-types* '(("application" . "json") ("text"))))
     (drakma:http-request "https://api.datamuse.com/words"
                          :parameters (loop :for (param value) :on parameters :by #'cddr
                                         :for lookup = (assoc param +words-query-parameters+ :test 'string=)
                                         :if (caddr lookup)
                                         :collect (caddr lookup) :into md
                                         :else
                                         :collect (cons (cadr lookup) value) :into result
                                         :finally (return (append result (when md (list (cons "md" (apply 'concatenate 'string md)))))))))
   :object-as :alist))

(defun words (&rest parameters)
  "Get a list of words matching the query specified. See the `+words-query-parameters+' alist or the `parameter-documentation' function for documentation on each parameter. This function works by performing a query to the Datamuse API's /words endpoint as described at https://www.datamuse.com/api/ .

Note that this function returns just a list of words. To get the full results which include the metadata, use `words*'.

Examples:

;; Get a list of words that rhyme with \"foobar\":
;; (words :rhymes \"foobar\")

;; Get a list of words that start with \"s\" and end with \"ing\":
;; (words :spelling \"s*ing\")

See also: `words*', `parameter-documentation', `+words-query-parameters+', `suggestions'."
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
  "Get a list of suggestions to a partially-type phrase, similar to the auto-suggest feature of some search engines. This function works by querying the Datamuse API's /sug endpoint as described at https://www.datamuse.com/api/ .

STRING is the input string to generate suggestions for, MAX is the maximum number of results, VOCABULARY is the vocabulary (which defaults to English).

Examples:

;; (dm:suggestions \"rei\")
=> (\"rein\" \"reiterate\" \"reign\" \"reinforce\" \"reimbursement\" \"reimburse\" \"reinstate\" \"reify\" \"reins\" \"reiki\")

See also: `words', `suggestions*'."
  (mapcar (lambda (x)
            (cdr (assoc "word" x :test #'string-equal)))
          (suggestions* string :max max :vocabulary vocabulary)))

;; make slime/swank show the "correct" argument list instead of just &rest
#+swank
(flet ((arglist (args)
         (swank::make-arglist
          :key-p t
          :keyword-args (loop :for param :in args
                           :collect (swank::make-keyword-arg (make-keyword param) param nil)))))
  (let* ((arglist (mapcar 'car +words-query-parameters+))
         ;; `words' doesn't include metadata, so we remove metadata-related arguments to avoid user confusion.
         (words-arglist (make-arglist (reverse (set-difference arglist (list
                                                                        'with-definitions
                                                                        'with-parts-of-speech
                                                                        'with-syllable-count
                                                                        'with-pronunciation
                                                                        'with-frequency)))))
         (words*-arglist (arglist arglist)))
    (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words)) argument-forms)
      words-arglist)
    (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words*)) argument-forms)
      words*-arglist)))
