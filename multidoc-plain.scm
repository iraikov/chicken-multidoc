;;
;; multidoc.plain - the plain text writer
;;
;; Paragraphs are filled to the width option.  The top two levels of
;; sections are underlined with = and -; deeper sections are written
;; as a title line.  Code, block quotes and definition bodies are
;; indented; tables are drawn as ASCII grids.
;;
;; Options:
;;
;;   standalone       write the document title first (default #t)
;;   top-division     the division kind of wiki level 2 (default
;;                    section)
;;   number-sections  prefix section titles with their numbers
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.plain

  (plain-writer)

  (import scheme (chicken base) (chicken format) (chicken string)
          (srfi 1) (srfi 13)
          datatype matchable
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


(define (standard-rank kind)
  (list-index (lambda (k) (eq? k kind)) standard-divisions))

(define (section-depth c division)
  (max 0 (- (standard-rank (standard-division division))
            (standard-rank (standard-division (ctx-option-symbol c 'top-division 'section))))))

(define (number-string c id)
  (let ((t (and id (ref-target c id))))
    (if (and (ctx-option c 'number-sections #f) t (xref-target-number t))
        (string-append (string-intersperse (map number->string (xref-target-number t)) ".") " ")
        "")))

;; [heading title underline] is a title line, underlined with the
;; given character if it is not #f.
(define (heading title underline)
  (if underline
      (vcat (list (pp:text title) (pp:text (make-string (string-length title) underline))))
      (pp:text title)))

(define (indented n doc)
  (pp:indent n doc))

(define (list-items c markers item-lists)
  (let ((tight (every (lambda (blocks)
                        (match blocks
                          (((? block? b)) (cases block b (Plain (xs) #t) (else #f)))
                          (else #f)))
                      item-lists))
        (docs (map (lambda (marker blocks)
                     (pp:^^ (pp:text marker) (pp:align (vsep (render-blocks c blocks)))))
                   markers item-lists)))
    (if tight (vcat docs) (vsep docs))))

(define (toc-list c entries)
  (vcat (map (lambda (e)
               (let ((line (pp:text (string-append (number-string c (toc-entry-id e))
                                                   (render-flat c (toc-entry-title e))))))
                 (if (null? (toc-entry-children e))
                     line
                     (vcat (list line (indented 2 (toc-list c (toc-entry-children e))))))))
             entries)))

(define (plain-block self b c)
  (let ((c (ctx-deeper c)))
    (cases block b
      (Plain (xs) (render-inlines c xs))
      (Para (xs) (render-inlines c xs))
      (CodeBlock (a s) (indented 4 (verbatim-lines s)))
      (RawBlock (format s) pp:empty)
      (BlockQuote (bs) (indented 4 (vsep (render-blocks c bs))))
      (BulletList (is) (list-items c (map (lambda (i) "  * ") is) is))
      (OrderedList (is)
        (list-items c (map (lambda (k) (sprintf "~a. " (+ k 1))) (iota (length is))) is))
      (DefinitionList (is)
        (vsep (map (lambda (item)
                     (vcat (cons (pp:text (render-flat c (car item)))
                                 (map (lambda (def) (indented 4 (vsep (render-blocks c def))))
                                      (cdr item)))))
                   is)))
      (Section (division a numbered title body)
        (let ((depth (section-depth c division))
              (title (string-append (if numbered (number-string c (attr-id a)) "")
                                    (render-flat c title))))
          (vsep (cons (heading title (case depth ((0) #\=) ((1) #\-) (else #f)))
                      (render-blocks c body)))))
      (Definition (a sigs body)
        (vsep (append (list (vcat (map (lambda (sig)
                                         (pp:text (string-append "[" (symbol->string (car sig)) "] "
                                                                 (render-flat c (cdr sig)))))
                                       sigs)))
                      (let ((body (render-blocks c body)))
                        (if (null? body) '() (list (indented 4 (vsep body))))))))
      (Table (a caption colspecs head bodies foot)
        (vcat (append (list (render-text-table c b head-rule: #\=))
                      (if (null? caption) '() (list (render-inlines c caption))))))
      (TableOfContents () (toc-list c (resolved-toc (ctx-resolved c))))
      (Bibliography (a) pp:empty)
      (HorizontalRule () (pp:text (make-string (min 72 (ctx-option c 'width 80)) #\-)))
      (Div (a bs) (vsep (render-blocks c bs)))
      (Null () pp:empty)
      (CustomBlock (tag a payload) pp:empty))))

(define (plain-inline self x c)
  (cases inline x
    (Str (s) (pp:text s))
    (Space () pp:space)
    (SoftBreak () pp:space)
    (LineBreak () pp:hardline)
    (NBSpace () pp:space)
    (Emph (xs) (pp:dcat (pp:text "_") (render-inlines c xs) (pp:text "_")))
    (Strong (xs) (pp:dcat (pp:text "*") (render-inlines c xs) (pp:text "*")))
    (Small (xs) (render-inlines c xs))
    (Code (a s) (pp:text s))
    (Link (a xs target kind)
      (if (or (eq? kind 'internal) (equal? (inlines->string xs) target))
          (if (eq? kind 'internal) (render-inlines c xs) (pp:text (string-append "<" target ">")))
          (pp:dcat (render-inlines c xs) (pp:group (pp:^^ pp:break1 (pp:text (string-append "<" target ">")))))))
    (Image (a alt src)
      (pp:text (string-append "[image: " (if (null? alt) src (inlines->string alt)) "]")))
    (Span (a xs) (render-inlines c xs))
    (RawInline (format s) pp:empty)
    (Cite (cs xs) (render-inlines c (if (null? xs) (cite-fallback-inlines c cs) xs)))
    (Ref (kind key xs)
      (let ((t (ref-target c key)))
        (if t
            (render-inlines c (if (null? xs) (ref-label-inlines c t) xs))
            (if (null? xs) (pp:text "???") (render-inlines c xs)))))
    (CustomInline (tag a payload) pp:empty)))

(define (plain-doc self d c)
  (let ((body  (vsep (render-blocks c (doc-blocks d))))
        (title (doc-meta-ref d 'title #f)))
    (if (and (ctx-option c 'standalone #t) title)
        (vsep (list (pp:text title) body))
        body)))

(define plain-writer
  (register-writer!
   (make-writer name: 'plain
                extension: "txt"
                doc: plain-doc
                block: plain-block
                inline: plain-inline)))

)
