;;
;; multidoc.html - the HTML writer
;;
;; Output is XHTML 1.0 Strict.
;; With the option (standalone . #f), only the body content is
;; written.  Other options:
;;
;;   top-division      the division kind rendered as html-top-heading
;;                     (default section)
;;   html-top-heading  the heading level of top-division (default 2)
;;   number-sections   prefix section headings with their numbers
;;   style, print-style, canonical
;;                     stylesheet and canonical links in the header;
;;                     also taken from the document metadata
;;
;; Style properties under the key `html' are applied to elements:
;; `class' adds a class, `style' adds CSS, and `attributes' adds a
;; list of (name value) attributes.  The table style vocabulary is
;; rendered as CSS on tables and cells.
;;
;; Copyright (c) 2026 Ivan Raikov
;; This is a reimplementation of the CHICKEN 4 multidoc by Peter Bex,
;; Ivan Raikov.

(module multidoc.html

  (html-writer html-escape html-attrs)

  (import scheme (chicken base) (chicken format) (chicken string)
          (srfi 1) (srfi 13)
          datatype matchable
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


;; ---------------------------------------------------------------------
;; Escaping and attributes
;; ---------------------------------------------------------------------

(define html-escape
  (make-escaper '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;") (#\" . "&quot;"))))

;; [html-attrs a #!key classes extra css] is the attribute string
;; (with a leading space, or empty) for attribute a.
;;
;;   classes  further class names
;;   extra    further (name . value) attributes
;;   css      further CSS declarations
;;
;; Key/value pairs of a become attributes, except that a "style"
;; pair joins the CSS.
(define (html-attrs a #!key (classes '()) (extra '()) (css '()))
  (let* ((kvs      (attr-kvs a))
         (classes  (append (attr-classes a) classes
                           (let ((c (style-property a 'html 'class))) (if c (list c) '()))))
         (css      (append css
                           (let ((s (assoc "style" kvs))) (if s (list (cdr s)) '()))
                           (let ((s (style-property a 'html 'style))) (if s (list s) '()))))
         (others   (append (remove (lambda (kv) (equal? (car kv) "style")) kvs)
                           extra
                           (map (lambda (p) (cons (->string (car p)) (->string (cadr p))))
                                (style-property a 'html 'attributes '()))))
         (attr     (lambda (name value)
                     (string-append " " name "=\"" (html-escape value) "\""))))
    (string-append
     (if (attr-id a) (attr "id" (attr-id a)) "")
     (if (pair? classes) (attr "class" (string-intersperse classes " ")) "")
     (apply string-append (map (lambda (kv) (attr (car kv) (cdr kv))) others))
     (if (pair? css) (attr "style" (string-intersperse css "; ")) ""))))

;; [tag name attrs body] encloses body in an element.
(define (tag name attrs body)
  (pp:dcat (pp:text (string-append "<" name attrs ">"))
           body
           (pp:text (string-append "</" name ">"))))

;; [block-tag name attrs docs] encloses a list of block documents in
;; an element, one per line.
(define (block-tag name attrs docs)
  (vcat (append (list (pp:text (string-append "<" name attrs ">")))
                docs
                (list (pp:text (string-append "</" name ">"))))))

(define (empty-tag name attrs)
  (pp:text (string-append "<" name attrs " />")))


;; ---------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------

;; [table-style-css a] renders the table style vocabulary of attribute
;; a as CSS declarations.
(define (table-style-css a)
  (define (prop key) (style-property a 'table key #f))
  (define (border side)
    (let ((w (prop side)))
      (and w (sprintf "~a: ~apt solid" side w))))
  (filter-map
   (lambda (x) x)
   (list (and (prop 'bold) "font-weight: bold")
         (and (prop 'italic) "font-style: italic")
         (and (prop 'fg) (string-append "color: " (prop 'fg)))
         (and (prop 'bg) (string-append "background-color: " (prop 'bg)))
         (border 'border-top) (border 'border-bottom)
         (border 'border-left) (border 'border-right)
         (and (prop 'halign) (sprintf "text-align: ~a" (prop 'halign)))
         (and (prop 'valign) (sprintf "vertical-align: ~a" (prop 'valign)))
         (and (prop 'padding) (sprintf "padding: ~apt" (prop 'padding))))))

(define (cell-contents c blocks)
  (match blocks
    (() pp:empty)
    ((b) (cases block b
           (Plain (xs) (render-inlines c xs))
           (else (render-block c b))))
    (else (vcat (render-blocks c blocks)))))

(define (html-cell c name cell)
  (let* ((a     (table-cell-attr cell))
         (align (table-cell-align cell))
         (extra (append (if (> (table-cell-rowspan cell) 1)
                            (list (cons "rowspan" (number->string (table-cell-rowspan cell))))
                            '())
                        (if (> (table-cell-colspan cell) 1)
                            (list (cons "colspan" (number->string (table-cell-colspan cell))))
                            '())))
         (css   (append (if (eq? align 'default) '()
                            (list (string-append "text-align: " (symbol->string align))))
                        (table-style-css a))))
    (tag name (html-attrs a extra: extra css: css)
         (cell-contents c (table-cell-blocks cell)))))

(define (html-part c name cell-name part)
  (if (null? (table-part-rows part))
      pp:empty
      (block-tag name (html-attrs (table-part-attr part))
                 (map (lambda (row)
                        (block-tag "tr" (html-attrs (table-row-attr row))
                                   (map (lambda (cell) (html-cell c cell-name cell))
                                        (table-row-cells row))))
                      (table-part-rows part)))))

(define (html-table c a caption head bodies foot)
  (let* ((border (style-property a 'table 'border #f))
         (align  (attr-kv a "align"))
         (a      (make-attr (attr-id a) (attr-classes a)
                            (remove (lambda (kv) (equal? (car kv) "align")) (attr-kvs a))
                            (attr-style a)))
         (extra  (if border (list (cons "border" (->string border))) '()))
         (css    (append (if (equal? align "center")
                             '("margin-left: auto" "margin-right: auto")
                             '())
                         (table-style-css a))))
    (block-tag "table" (html-attrs a extra: extra css: css)
               (remove (lambda (d) (eq? d pp:empty))
                       (append
                        (list (if (null? caption)
                                  pp:empty
                                  (tag "caption" "" (render-inlines c caption)))
                              (html-part c "thead" "th" head)
                              (html-part c "tfoot" "td" foot))
                        (map (lambda (body) (html-part c "tbody" "td" body)) bodies))))))


;; ---------------------------------------------------------------------
;; Sections and tables of contents
;; ---------------------------------------------------------------------

(define (standard-rank kind)
  (list-index (lambda (k) (eq? k kind)) standard-divisions))

;; [heading-level ctx division] is the h1..h6 level of a division.
(define (heading-level c division)
  (let ((top  (standard-division (ctx-option-symbol c 'top-division 'section)))
        (base (ctx-option c 'html-top-heading 2)))
    (max 1 (min 6 (+ base (- (standard-rank (standard-division division))
                             (standard-rank top)))))))

(define (number-prefix c id)
  (let ((t (and id (ref-target c id))))
    (if (and (ctx-option c 'number-sections #f) t (xref-target-number t))
        (pp:text (string-append
                  (string-intersperse (map number->string (xref-target-number t)) ".")
                  " "))
        pp:empty)))

(define (html-toc c)
  (define (entries es)
    (block-tag "ol" ""
               (map (lambda (e)
                      (let ((link (tag "a" (html-attrs null-attr
                                                       extra: (list (cons "href" (string-append "#" (toc-entry-id e)))))
                                       (render-inlines c (toc-entry-title e)))))
                        (if (null? (toc-entry-children e))
                            (tag "li" "" link)
                            (block-tag "li" "" (list link (entries (toc-entry-children e)))))))
                    es)))
  (let ((toc (resolved-toc (ctx-resolved c))))
    (block-tag "div" " id=\"toc\"" (if (null? toc) '() (list (entries toc))))))


;; ---------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------

(define (list-item c blocks)
  (match blocks
    (((? block? b))
     (cases block b
       (Plain (xs) (tag "li" "" (render-inlines c xs)))
       (else (block-tag "li" "" (render-blocks c blocks)))))
    (else (block-tag "li" "" (render-blocks c blocks)))))

(define (html-block self b c)
  (let ((c (ctx-deeper c)))
    (cases block b
      (Plain (xs) (render-inlines c xs))
      (Para (xs) (tag "p" "" (render-inlines c xs)))
      (CodeBlock (a s)
        (tag "pre" (html-attrs a) (render-text (ctx-in-verbatim c) s)))
      (RawBlock (format s)
        (if (eq? format 'html) (verbatim-lines s) pp:empty))
      (BlockQuote (bs) (block-tag "blockquote" "" (render-blocks c bs)))
      (BulletList (items) (block-tag "ul" "" (map (lambda (i) (list-item c i)) items)))
      (OrderedList (items) (block-tag "ol" "" (map (lambda (i) (list-item c i)) items)))
      (DefinitionList (items)
        (block-tag "dl" ""
                   (append-map
                    (lambda (item)
                      (cons (tag "dt" "" (render-inlines c (car item)))
                            (map (lambda (def)
                                   (match def
                                     (((? block? b))
                                      (cases block b
                                        (Plain (xs) (tag "dd" "" (render-inlines c xs)))
                                        (else (block-tag "dd" "" (render-blocks c def)))))
                                     (else (block-tag "dd" "" (render-blocks c def)))))
                                 (cdr item))))
                    items)))
      (Section (division a numbered title body)
        (let ((h (string-append "h" (number->string (heading-level c division)))))
          (vcat (cons (tag h (html-attrs a)
                           (pp:^^ (number-prefix c (and numbered (attr-id a)))
                                  (render-inlines c title)))
                      (render-blocks c body)))))
      (Definition (a sigs body)
        (block-tag "div" (html-attrs a classes: '("definition"))
                   (append
                    (map (lambda (sig)
                           (let ((kind (symbol->string (car sig))))
                             (tag "span" (html-attrs null-attr classes: (list "definition" kind))
                                  (pp:dcat (pp:text (string-append "<em>[" (html-escape kind) "]</em> "))
                                           (render-inlines c (cdr sig))
                                           (pp:text "<br />")))))
                         sigs)
                    (render-blocks c body))))
      (Table (a caption colspecs head bodies foot)
        (html-table c a caption head bodies foot))
      (TableOfContents () (html-toc c))
      (Bibliography (a) pp:empty)
      (HorizontalRule () (empty-tag "hr" ""))
      (Div (a bs) (block-tag "div" (html-attrs a) (render-blocks c bs)))
      (Null () pp:empty)
      (CustomBlock (tag a payload) pp:empty))))


;; ---------------------------------------------------------------------
;; Inlines
;; ---------------------------------------------------------------------

(define (href-attrs href class)
  (html-attrs null-attr classes: (list class) extra: (list (cons "href" href))))

(define (html-inline self x c)
  (cases inline x
    (Str (s) (pp:text (render-string c s)))
    (Space () pp:space)
    (SoftBreak () pp:space)
    (LineBreak () (pp:^^ (pp:text "<br />") pp:hardline))
    (NBSpace () (pp:text "&#160;"))
    (Emph (xs) (tag "em" "" (render-inlines c xs)))
    (Strong (xs) (tag "strong" "" (render-inlines c xs)))
    (Small (xs) (tag "small" "" (render-inlines c xs)))
    (Code (a s) (tag "tt" (html-attrs a) (pp:text (render-string (ctx-in-verbatim c) s))))
    (Link (a xs target kind)
      (tag "a" (html-attrs (make-attr (attr-id a) (attr-classes a) '() (attr-style a))
                           classes: (list (symbol->string kind))
                           extra: (list (cons "href" target)))
           (render-inlines c xs)))
    (Image (a alt src)
      (let ((alt (inlines->string alt)))
        (empty-tag "img" (html-attrs a extra: (append (list (cons "src" src))
                                                     (if (string-null? alt) '()
                                                         (list (cons "alt" alt) (cons "title" alt))))))))
    (Span (a xs) (tag "span" (html-attrs a) (render-inlines c xs)))
    (RawInline (format s) (if (eq? format 'html) (verbatim-lines s) pp:empty))
    (Cite (cs xs)
      (tag "span" " class=\"citation\""
           (render-inlines c (if (null? xs) (cite-fallback-inlines c cs) xs))))
    (Ref (kind key xs)
      (let ((t (ref-target c key)))
        (if t
            (tag "a" (href-attrs (xref-href c t) "internal")
                 (render-inlines c (if (null? xs) (ref-label-inlines c t) xs)))
            (if (null? xs) (pp:text "???") (render-inlines c xs)))))
    (CustomInline (tag a payload) pp:empty)))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

(define xhtml-doctype
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")

;; The header: the title, the content type, stylesheets, a canonical
;; link, and meta elements for the descriptive metadata.
(define (html-head c d)
  (define (meta-or-option key)
    (or (ctx-option c key #f) (doc-meta-ref d key #f)))
  (define (link rel href #!optional media)
    (empty-tag "link" (html-attrs null-attr
                                  extra: (append (list (cons "rel" rel))
                                                 (if (equal? rel "stylesheet")
                                                     (list (cons "type" "text/css")) '())
                                                 (if media (list (cons "media" media)) '())
                                                 (list (cons "href" href))))))
  (define (meta attrs) (empty-tag "meta" (html-attrs null-attr extra: attrs)))
  (block-tag
   "head" ""
   (filter-map
    (lambda (x) x)
    (append
     (list (tag "title" "" (pp:text (html-escape (or (doc-meta-ref d 'title #f) "multidoc"))))
           (meta '(("http-equiv" . "Content-Style-Type") ("content" . "text/css")))
           (meta (list (cons "http-equiv" "Content-Type")
                       (cons "content" (or (meta-or-option 'Content-Type)
                                           "text/html; charset=UTF-8"))))
           (let ((s (meta-or-option 'style))) (and s (link "stylesheet" s)))
           (let ((s (meta-or-option 'print-style))) (and s (link "stylesheet" s "print")))
           (let ((s (meta-or-option 'canonical))) (and s (link "canonical" s))))
     (map (lambda (key)
            (let ((v (doc-meta-ref d key #f)))
              (and v (meta (list (cons "name" (symbol->string key)) (cons "content" v))))))
          '(description Author keywords Date-Revision-yyyymmdd Date-Creation-yyyymmdd))))))

(define (html-doc self d c)
  (let ((body (vcat (render-blocks c (doc-blocks d)))))
    (if (ctx-option c 'standalone #t)
        (vcat (list (pp:text xhtml-doctype)
                    (pp:text "<html xmlns=\"http://www.w3.org/1999/xhtml\">")
                    (html-head c d)
                    (pp:text "<body>")
                    body
                    (pp:text "</body>")
                    (pp:text "</html>")))
        body)))

(define html-writer
  (register-writer!
   (make-writer name: 'html
                extension: "html"
                doc: html-doc
                block: html-block
                inline: html-inline
                text: (lambda (self s c) (html-escape s)))))

)
