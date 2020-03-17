(in-package #:datamuse)

;; make slime/swank show the "correct" argument list instead of just &rest
(flet ((swank-arglist (args)
         (swank::make-arglist
          :key-p t
          :keyword-args (loop :for param :in args
                           :collect (swank::make-keyword-arg (make-keyword param) param nil)))))
  (let* ((arglist (mapcar 'car +words-query-parameters+))
         ;; `words' doesn't include metadata, so we remove metadata-related arguments to avoid user confusion.
         (words-arglist (swank-arglist (reverse (set-difference arglist (list
                                                                         'with-definitions
                                                                         'with-parts-of-speech
                                                                         'with-syllable-count
                                                                         'with-pronunciation
                                                                         'with-frequency)))))
         (words*-arglist (swank-arglist arglist)))
    (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words)) argument-forms)
      words-arglist)
    (defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'words*)) argument-forms)
      words*-arglist)))
