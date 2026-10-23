;;
;; Building a document from the intermediate representation, and
;; transforming it with filters.
;;
;; Documents need not come from SXML: a program can build one with the
;; IR constructors, much as a pandoc filter would.  Filters are
;; procedures from documents to documents, written with walk-blocks
;; and walk-inlines; queries fold over the elements with query-blocks
;; and query-inlines.
;;
;;   csi -s build-ir.scm
;;

(import scheme (chicken base) (chicken format) (chicken string)
        (srfi 1) (srfi 13)
        datatype
        multidoc)

;; [text s] is the inlines of a string: words separated by spaces.
(define (text s) (string->inlines s))

(define (para . parts)
  (Para (append-map (lambda (p) (if (string? p) (text p) (list p))) parts)))

;; A report built by hand.  Sections take a division kind, an
;; attribute (whose identifier is the anchor), a numbering flag, a
;; title and their contents.
(define report
  (make-doc
   '((title . "Build report"))
   (list
    (Section 'section (make-attr "summary" '() '() #f) #t (text "Summary")
             (list (para "All " (Strong (text "42")) " tests passed. TODO: add timings.")
                   (BulletList (list (list (Plain (text "multidoc: 12 groups")))
                                     (list (Plain (text "scbib: 14 groups")))))))
    (Section 'section null-attr #t (text "Details")
             (list (para "See " (Ref 'section "summary" '()) " for the totals. TODO: link logs.")
                   (CodeBlock (make-attr #f '("shell") '() #f)
                              "chicken-install -test")
                   (Section 'subsection null-attr #t (text "Environment")
                            (list (DefinitionList
                                   (list (cons (text "CHICKEN") (list (list (Plain (text "6.0.0")))))
                                         (cons (text "Platform") (list (list (Plain (text "Linux"))))))))))))))

;; A filter that marks every TODO in bold.  walk-inlines visits the
;; inlines bottom-up; returning a list splices it in place of the
;; element.
(define (highlight-todos d)
  (make-doc (doc-meta d)
            (walk-inlines (lambda (x)
                            (cases inline x
                              (Str (s) (if (string-prefix? "TODO" s)
                                           (list (Strong (list (Str s))))
                                           x))
                              (else x)))
                          (doc-blocks d))))

;; A filter that drops code blocks.
(define (drop-code d)
  (make-doc (doc-meta d)
            (walk-blocks (lambda (b)
                           (cases block b
                             (CodeBlock (a s) '())
                             (else b)))
                         (doc-blocks d))))

;; A query: the titles of all sections, in document order.
(define (section-titles d)
  (reverse
   (query-blocks (lambda (b acc)
                   (cases block b
                     (Section (division a numbered title body)
                       (cons (inlines->string title) acc))
                     (else acc)))
                 '() (doc-blocks d))))

;; A query: the number of words.
(define (word-count d)
  (query-inlines (lambda (x n) (cases inline x (Str (s) (+ n 1)) (else n)))
                 0 (doc-blocks d)))

(printf "Sections: ~a~%" (string-intersperse (section-titles report) ", "))
(printf "Words: ~a~%~%" (word-count report))

;; Filters compose like any procedures.  shift-divisions is a filter
;; provided by multidoc: here it turns sections into subsections.
(define filtered ((shift-divisions 1) (drop-code (highlight-todos report))))

(display (layout->string (render filtered to: 'markdown width: 60) 60))
(newline)

;; The native writer shows the IR itself, which helps when writing
;; filters.
(display (layout->string (render (make-doc '() (list (para "TODO: check")))
                                 to: 'native width: 60)
                         60))
(newline)
(display (layout->string (render (highlight-todos (make-doc '() (list (para "TODO: check"))))
                                 to: 'native width: 60)
                         60))
(newline)
