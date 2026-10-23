;;
;; multidoc.markdown - the Markdown writer
;;
;; Output is CommonMark with the pandoc extensions for heading
;; attributes, fenced divs, bracketed spans, definition lists, pipe
;; and grid tables, raw attributes and citations.
;;
;; Options:
;;
;;   standalone             write a YAML metadata block with the title
;;                          and author (default #t)
;;   top-division           the division kind of wiki level 2 (default
;;                          section)
;;   markdown-top-heading   the heading level of top-division (default 1)
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.markdown

  (markdown-writer markdown-escape)

  (import scheme (chicken base) (chicken format) (chicken string)
          (chicken irregex)
          (srfi 1) (srfi 13)
          datatype matchable
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


;; ---------------------------------------------------------------------
;; Escaping
;; ---------------------------------------------------------------------

(define markdown-escape
  (make-escaper '((#\\ . "\\\\") (#\` . "\\`") (#\* . "\\*") (#\_ . "\\_")
                  (#\[ . "\\[") (#\] . "\\]") (#\< . "\\<") (#\> . "\\>")
                  (#\# . "\\#"))))

(define escape-in-table
  (make-escaper '((#\\ . "\\\\") (#\` . "\\`") (#\* . "\\*") (#\_ . "\\_")
                  (#\[ . "\\[") (#\] . "\\]") (#\< . "\\<") (#\> . "\\>")
                  (#\# . "\\#") (#\| . "\\|"))))

;; A word that would start a list item, a thematic break or a setext
;; underline if it began a line.  Paragraph filling may put any word
;; at the start of a line, so such words are always escaped.
(define line-start-marker
  (irregex "^([-+=]+|[0-9]+[.)])$"))

(define (escape-word s)
  (if (irregex-match line-start-marker s)
      (if (char-numeric? (string-ref s 0))
          (string-append (substring s 0 (- (string-length s) 1))
                         "\\" (substring s (- (string-length s) 1) (string-length s)))
          (string-append "\\" s))
      s))


;; ---------------------------------------------------------------------
;; Attributes and helpers
;; ---------------------------------------------------------------------

;; [attr-string a extra-classes] is a pandoc attribute block such as
;; {#id .class key="value"}, or "" if there is nothing to write.
(define (attr-string a #!optional (extra-classes '()))
  (let ((parts (append (if (attr-id a) (list (string-append "#" (attr-id a))) '())
                       (map (lambda (cl) (string-append "." cl))
                            (append (attr-classes a) extra-classes))
                       (map (lambda (kv) (sprintf "~a=~s" (car kv) (cdr kv)))
                            (attr-kvs a)))))
    (if (null? parts) "" (string-append "{" (string-intersperse parts " ") "}"))))

(define (anchor a)
  (if (attr-id a)
      (pp:text (string-append "<a id=\"" (attr-id a) "\"></a>"))
      pp:empty))

;; An HTML block continues to the next blank line, so the anchor is
;; set apart from the element it marks.
(define (with-anchor a doc)
  (if (attr-id a) (vsep (list (anchor a) doc)) doc))

;; [fence s] is a backtick fence longer than any run of backticks in s.
(define (fence s)
  (let ((longest (fold max 0 (map string-length
                                  (irregex-extract (irregex "`+") s)))))
    (make-string (max 3 (+ longest 1)) #\`)))

;; [prefix-lines prefix doc width] prefixes every line of doc, laid
;; out at the given width.
(define (prefix-lines prefix doc width)
  (vcat (map (lambda (line)
               (pp:text (if (string-null? line) (string-trim-right prefix) (string-append prefix line))))
             (string-split (layout->string doc width) "\n" #t))))

(define (standard-rank kind)
  (list-index (lambda (k) (eq? k kind)) standard-divisions))

(define (heading-level c division)
  (let ((top  (standard-division (ctx-option-symbol c 'top-division 'section)))
        (base (ctx-option c 'markdown-top-heading 1)))
    (max 1 (min 6 (+ base (- (standard-rank (standard-division division))
                             (standard-rank top)))))))


;; ---------------------------------------------------------------------
;; Lists
;; ---------------------------------------------------------------------

(define (single-plain? blocks)
  (match blocks
    (((? block? b)) (cases block b (Plain (xs) #t) (else #f)))
    (else #f)))

;; A list is tight (items on consecutive lines) if every item is a
;; single line of plain text.
(define (list-items c markers item-lists)
  (let ((docs (map (lambda (marker blocks)
                     (pp:^^ (pp:text marker) (pp:align (vsep (render-blocks c blocks)))))
                   markers item-lists)))
    (if (every single-plain? item-lists) (vcat docs) (vsep docs))))


;; ---------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------

;; A pipe table can hold the table if it has one header row, no spans,
;; and only single lines of text in its cells.
(define (pipe-table? tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (and (= 1 (length (table-part-rows head)))
           (null? (table-part-rows foot))
           (not (any cell-spans? (table-cells tbl)))
           (every (lambda (ref)
                    (match (table-cell-blocks (cell-ref-cell ref))
                      (() #t)
                      (((? block? b)) (cases block b (Plain (xs) #t) (Para (xs) #t) (else #f)))
                      (else #f)))
                  (table-cells tbl))))
    (else #f)))

(define (pipe-table c tbl)
  (let* ((c     (ctx-with-option c 'in-pipe-table #t))
         (ncols (table-width tbl))
         (text-of (lambda (cell)
                    (match (table-cell-blocks cell)
                      (() "")
                      (((? block? b)) (cases block b
                                        (Plain (xs) (render-flat c xs))
                                        (Para (xs) (render-flat c xs))
                                        (else "")))
                      (else ""))))
         ;; Rows as lists of cell texts, padded to the table width.
         (rows-of (lambda (part-name)
                    (let* ((refs (table-cells tbl part: part-name))
                           (key  (lambda (ref) (cons (cell-ref-part-index ref) (cell-ref-row ref)))))
                      (map (lambda (k)
                             (let ((texts (map (lambda (ref) (text-of (cell-ref-cell ref)))
                                               (filter (lambda (ref) (equal? (key ref) k)) refs))))
                               (append texts (make-list (max 0 (- ncols (length texts))) ""))))
                           (delete-duplicates (map key refs))))))
         (head   (car (rows-of 'head)))
         (body   (rows-of 'body))
         (aligns (map (lambda (col)
                        (let ((refs (table-cells tbl part: 'head col: col)))
                          (if (null? refs) 'default (table-cell-align (cell-ref-cell (car refs))))))
                      (iota ncols)))
         (widths (map (lambda (col)
                        (fold max 3 (map (lambda (row) (string-length (list-ref row col)))
                                         (cons head body))))
                      (iota ncols)))
         (pad    (lambda (s w) (string-append s (make-string (- w (string-length s)) #\space))))
         (line   (lambda (cells)
                   (pp:text (string-append "| " (string-intersperse (map pad cells widths) " | ") " |"))))
         (rule   (pp:text (string-append
                           "|"
                           (string-intersperse
                            (map (lambda (align w)
                                   (case align
                                     ((center) (string-append ":" (make-string w #\-) ":"))
                                     ((right) (string-append (make-string (+ w 1) #\-) ":"))
                                     ((left) (string-append ":" (make-string (+ w 1) #\-)))
                                     (else (make-string (+ w 2) #\-))))
                                 aligns widths)
                            "|")
                           "|"))))
    (vcat (append (list (line head) rule) (map line body)))))

(define (markdown-table c tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (with-anchor
       a
       (vsep (append (list (if (pipe-table? tbl)
                               (pipe-table c tbl)
                               (render-text-table c tbl)))
                     (if (null? caption) '()
                         (list (pp:^^ (pp:text ": ") (render-inlines c caption))))))))
    (else pp:empty)))


;; ---------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------

(define (toc-list c entries)
  (vcat (map (lambda (e)
               (let ((link (pp:text (string-append "[" (render-flat c (toc-entry-title e)) "](#"
                                                   (toc-entry-id e) ")"))))
                 (pp:^^ (pp:text "- ")
                        (pp:align (if (null? (toc-entry-children e))
                                      link
                                      (vcat (list link (toc-list c (toc-entry-children e)))))))))
             entries)))

(define (markdown-block self b c)
  (let ((c (ctx-deeper c)))
    (cases block b
      (Plain (xs) (render-inlines c xs))
      (Para (xs) (render-inlines c xs))
      (CodeBlock (a s)
        (let ((f (fence s))
              (info (match (attr-classes a) ((lang . _) lang) (else ""))))
          (with-anchor a (vcat (list (pp:text (string-append f info))
                                     (verbatim-lines s)
                                     (pp:text f))))))
      (RawBlock (format s)
        (if (eq? format 'html)
            (verbatim-lines s)
            (let ((f (fence s)))
              (vcat (list (pp:text (sprintf "~a{=~a}" f format)) (verbatim-lines s) (pp:text f))))))
      (BlockQuote (bs)
        (prefix-lines "> " (vsep (render-blocks c bs)) (max 20 (- (ctx-option c 'width 80) 2))))
      (BulletList (is) (list-items c (map (lambda (i) "- ") is) is))
      (OrderedList (is)
        (list-items c (map (lambda (k) (sprintf "~a. " (+ k 1))) (iota (length is))) is))
      (DefinitionList (is)
        (vsep (map (lambda (item)
                     (vcat (cons (pp:text (render-flat c (car item)))
                                 (map (lambda (def)
                                        (pp:^^ (pp:text ":   ") (pp:align (vsep (render-blocks c def)))))
                                      (if (null? (cdr item)) (list (list (Plain '()))) (cdr item))))))
                   is)))
      (Section (division a numbered title body)
        (let ((extra (if numbered '() '("unnumbered"))))
          (vsep (cons (pp:text (string-append (make-string (heading-level c division) #\#) " "
                                              (render-flat c title)
                                              (let ((s (attr-string a extra)))
                                                (if (string-null? s) "" (string-append " " s)))))
                      (render-blocks c body)))))
      (Definition (a sigs body)
        (vsep (cons (with-anchor
                     a
                     (pp:fold1 (lambda (x y) (pp:dcat x (pp:text "\\") pp:hardline y))
                               (map (lambda (sig)
                                      (pp:dcat (pp:text (string-append "*" (symbol->string (car sig)) ":* "))
                                               (render-inlines c (cdr sig))))
                                    sigs)))
                    (render-blocks c body))))
      (Table (a caption colspecs head bodies foot) (markdown-table c b))
      (TableOfContents () (toc-list c (resolved-toc (ctx-resolved c))))
      (Bibliography (a) pp:empty)
      (HorizontalRule () (pp:text "* * *"))
      (Div (a bs)
        (let ((s (attr-string a)))
          (if (string-null? s)
              (vsep (render-blocks c bs))
              (vcat (list (pp:text (string-append "::: " s))
                          (vsep (render-blocks c bs))
                          (pp:text ":::"))))))
      (Null () pp:empty)
      (CustomBlock (tag a payload) pp:empty))))


;; ---------------------------------------------------------------------
;; Inlines
;; ---------------------------------------------------------------------

(define (code-span s)
  (let* ((longest (fold max 0 (map string-length (irregex-extract (irregex "`+") s))))
         (ticks (make-string (+ longest 1) #\`))
         (pad (if (or (string-prefix? "`" s) (string-suffix? "`" s)) " " "")))
    (string-append ticks pad s pad ticks)))

(define (citation-string c cit)
  (let ((key (string-append (if (eq? (citation-mode cit) 'suppress-author) "-@" "@")
                            (citation-key cit))))
    (if (eq? (citation-mode cit) 'author-in-text)
        key
        (string-append
         (let ((p (render-flat c (citation-prefix cit)))) (if (string-null? p) "" (string-append p " ")))
         key
         (if (citation-locator cit) (string-append ", " (citation-locator cit)) "")
         (let ((s (render-flat c (citation-suffix cit)))) (if (string-null? s) "" (string-append " " s)))))))

(define (markdown-inline self x c)
  (cases inline x
    (Str (s) (pp:text (escape-word (render-string c s))))
    (Space () pp:space)
    (SoftBreak () pp:space)
    (LineBreak () (pp:^^ (pp:text "\\") pp:hardline))
    (NBSpace () (pp:text "&nbsp;"))
    (Emph (xs) (pp:dcat (pp:text "*") (render-inlines c xs) (pp:text "*")))
    (Strong (xs) (pp:dcat (pp:text "**") (render-inlines c xs) (pp:text "**")))
    (Small (xs)
      (pp:dcat (pp:text "[") (render-inlines c xs) (pp:text "]{.small}")))
    (Code (a s) (pp:text (code-span s)))
    (Link (a xs target kind)
      (if (and (eq? kind 'external) (equal? (inlines->string xs) target))
          (pp:text (string-append "<" target ">"))
          (pp:dcat (pp:text "[") (render-inlines c xs) (pp:text (string-append "](" target ")")))))
    (Image (a alt src)
      (pp:dcat (pp:text "![") (render-inlines c alt) (pp:text (string-append "](" src ")"))))
    (Span (a xs)
      (let ((s (attr-string a)))
        (if (string-null? s)
            (render-inlines c xs)
            (pp:dcat (pp:text "[") (render-inlines c xs) (pp:text (string-append "]" s))))))
    (RawInline (format s)
      (if (eq? format 'html)
          (pp:text s)
          (pp:text (string-append (code-span s) "{=" (symbol->string format) "}"))))
    (Cite (cs xs)
      (cond ((pair? xs) (render-inlines c xs))
            ((and (= (length cs) 1) (eq? (citation-mode (car cs)) 'author-in-text))
             (pp:text (citation-string c (car cs))))
            (else (pp:text (string-append "["
                                          (string-intersperse (map (lambda (cit) (citation-string c cit)) cs)
                                                              "; ")
                                          "]")))))
    (Ref (kind key xs)
      (let ((t (ref-target c key)))
        (if t
            (pp:dcat (pp:text "[")
                     (render-inlines c (if (null? xs) (ref-label-inlines c t) xs))
                     (pp:text (string-append "](" (xref-href c t) ")")))
            (if (null? xs) (pp:text "???") (render-inlines c xs)))))
    (CustomInline (tag a payload) pp:empty)))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

(define (yaml-string s)
  (string-append "\"" (string-translate* s '(("\\" . "\\\\") ("\"" . "\\\""))) "\""))

(define (markdown-doc self d c)
  (let ((body   (vsep (render-blocks c (doc-blocks d))))
        (fields (filter-map (lambda (key)
                              (let ((v (doc-meta-ref d key #f)))
                                (and v (pp:text (string-append (string-downcase (symbol->string key))
                                                               ": " (yaml-string v))))))
                            '(title Author description))))
    (if (and (ctx-option c 'standalone #t) (pair? fields))
        (vcat (append (list (pp:text "---")) fields (list (pp:text "---") pp:empty body)))
        body)))

(define markdown-writer
  (register-writer!
   (make-writer name: 'markdown
                extension: "md"
                doc: markdown-doc
                block: markdown-block
                inline: markdown-inline
                text: (lambda (self s c)
                        (if (ctx-option c 'in-pipe-table #f)
                            (escape-in-table s)
                            (markdown-escape s))))))

)
