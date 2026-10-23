;;
;; multidoc.latex - the LaTeX writer
;;
;; Options:
;;
;;   standalone       write a preamble and the document environment
;;                    (default #t)
;;   document-class   the document class (default "article")
;;   class-options    options of the document class (default "12pt")
;;   latex-packages   further packages: a list of names, or of
;;                    (name option ...) lists
;;   latex-preamble   further preamble lines, a list of strings
;;   maketitle        emit \title, \author and \maketitle from the
;;                    document metadata (default #f)
;;   top-division     the division kind of wiki level 2 (default
;;                    section)
;;   ref-style        title: refer to targets by title; otherwise
;;                    numbered targets are referred to with \autoref
;;   latex-citations  cite (the default), natbib or biblatex
;;   bibliography     the BibTeX database file for the bibliography
;;   bibliography-style
;;                    the BibTeX style (default plain, or plainnat
;;                    with natbib)
;;
;; Packages needed by the document are added from the feature list
;; of the resolution table: hyperref for links and references,
;; graphicx for images, multirow for spanning table cells, and xcolor
;; and colortbl for coloured cells.
;;
;; Division kinds map to the sectioning commands of the class.  For a
;; class without chapters (article), a document whose top division is
;; chapter is shifted down one level, so that chapters become
;; sections.
;;
;; Copyright (c) 2026 Ivan Raikov
;; This is a reimplementation of the CHICKEN 4 multidoc by Peter Bex,
;; Ivan Raikov.

(module multidoc.latex

  (latex-writer latex-escape latex-division-command)

  (import scheme (chicken base) (chicken format) (chicken string) (chicken pathname)
          (srfi 1) (srfi 13)
          datatype matchable
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


;; ---------------------------------------------------------------------
;; Escaping
;; ---------------------------------------------------------------------

;; Characters special to LaTeX are escaped.  The circumflex,
;; backslash, angle brackets and bar are written as text commands,
;; since the OT1 and T1 encodings do not print them as themselves.
(define latex-escape
  (make-escaper
   '((#\# . "\\#") (#\$ . "\\$") (#\% . "\\%") (#\& . "\\&")
     (#\~ . "\\textasciitilde{}") (#\_ . "\\_") (#\^ . "\\textasciicircum{}")
     (#\\ . "\\textbackslash{}") (#\{ . "\\{") (#\} . "\\}")
     (#\< . "\\textless{}") (#\> . "\\textgreater{}") (#\| . "\\textbar{}"))))

;; Characters that must be escaped in the URL argument of \href.
(define url-escape
  (make-escaper '((#\# . "\\#") (#\% . "\\%"))))

(define (cmd name . args)
  (pp:dcat (pp:text (string-append "\\" name))
           (fold-right (lambda (a acc) (pp:dcat pp:lbrace a pp:rbrace acc))
                       pp:empty args)))

(define (env name docs #!optional (args ""))
  (vcat (append (list (pp:text (string-append "\\begin{" name "}" args)))
                docs
                (list (pp:text (string-append "\\end{" name "}"))))))


;; ---------------------------------------------------------------------
;; Sectioning
;; ---------------------------------------------------------------------

(define (standard-rank kind)
  (list-index (lambda (k) (eq? k kind)) standard-divisions))

(define (class-has-chapters? c)
  (member (ctx-option c 'document-class "article") '("book" "report" "memoir" "scrbook" "scrreprt")))

;; [latex-division-command ctx division] is the sectioning command
;; name for a division kind.
(define (latex-division-command c division)
  (let* ((top   (standard-rank (standard-division (ctx-option-symbol c 'top-division 'section))))
         (shift (if (and (not (class-has-chapters? c)) (= top 1)) 1 0))
         (rank  (+ (standard-rank (standard-division division)) shift))
         (kind  (list-ref standard-divisions (min rank (- (length standard-divisions) 1)))))
    (symbol->string
     (if (and (eq? kind 'chapter) (not (class-has-chapters? c))) 'section kind))))


;; ---------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------

(define (hex-color s)
  (string-upcase (string-trim-both s #\#)))

(define (align-letter align)
  (case align ((center) "c") ((right) "r") (else "l")))

;; A column needs a paragraph (p) column if a cell starting in it
;; holds more than one block, or anything but plain text.
(define (paragraph-column? tbl col)
  (any (lambda (ref)
         (match (table-cell-blocks (cell-ref-cell ref))
           (() #f)
           (((? block? b)) (cases block b (Plain (xs) #f) (Para (xs) #f) (else #t)))
           (else #t)))
       (table-cells tbl col: col)))

(define (column-spec tbl colspecs ncols border)
  (let ((specs
         (map (lambda (col)
                (let* ((cs    (and (< col (length colspecs)) (list-ref colspecs col)))
                       (align (if cs (colspec-align cs) 'default))
                       (width (and cs (colspec-width cs))))
                  (cond (width (sprintf "p{~a\\linewidth}" width))
                        ((paragraph-column? tbl col)
                         (sprintf "p{~a\\linewidth}" (/ (round (* 900 (/ 1 ncols))) 1000.)))
                        (else (align-letter align)))))
              (iota ncols))))
    (if border
        (string-append "|" (string-intersperse specs "|") "|")
        (apply string-append specs))))

(define (cell-block c b)
  (cases block b
    (Plain (xs) (render-inlines c xs))
    (Para (xs) (render-inlines c xs))
    (CodeBlock (a s)
      (pp:fold1 (lambda (x y) (pp:dcat x (pp:text "\\newline ") y))
                (map (lambda (line) (cmd "texttt" (pp:text (latex-escape line))))
                     (string-split s "\n" #t))))
    (else (render-block c b))))

;; [cell-color cell] sets the background of a cell, or is empty.  It
;; is repeated in the slots a row span covers, since \cellcolor only
;; colours the row it appears in.
(define (cell-color cell)
  (let ((bg (style-property (table-cell-attr cell) 'table 'bg #f)))
    (if bg (pp:text (sprintf "\\cellcolor[HTML]{~a}" (hex-color bg))) pp:empty)))

;; [cell-content ctx cell col border] is the content of a cell with
;; its style, alignment and spans applied.
(define (cell-content c cell col border)
  (let* ((a      (table-cell-attr cell))
         (prop   (lambda (k) (style-property a 'table k #f)))
         (body   (pp:fold1 (lambda (x y) (pp:dcat x (pp:text "\\newline ") y))
                           (map (lambda (b) (cell-block c b)) (table-cell-blocks cell))))
         (body   (if (prop 'bold) (cmd "textbf" body) body))
         (body   (if (prop 'italic) (cmd "textit" body) body))
         (body   (if (prop 'fg)
                     (pp:dcat (pp:text (sprintf "\\textcolor[HTML]{~a}" (hex-color (prop 'fg))))
                              pp:lbrace body pp:rbrace)
                     body))
         (rs     (table-cell-rowspan cell))
         (cs     (table-cell-colspan cell))
         (body   (if (> rs 1) (pp:dcat (pp:text (sprintf "\\multirow{~a}{*}" rs)) pp:lbrace body pp:rbrace) body))
         (body   (pp:^^ (cell-color cell) body))
         (align  (table-cell-align cell)))
    (if (or (> cs 1) (not (eq? align 'default)))
        (multicolumn cs (align-letter align) col border body)
        body)))

(define (multicolumn span letter col border body)
  (pp:dcat (pp:text (sprintf "\\multicolumn{~a}{~a~a~a}" span
                             (if (and border (= col 0)) "|" "") letter (if border "|" "")))
           pp:lbrace body pp:rbrace))

;; [grid-row ctx g r border] renders row r of grid g.  Slots covered
;; by a cell of the same row are part of that cell's \multicolumn;
;; slots covered from a row above get an empty cell (spanning as
;; many columns as the covering cell).
(define (grid-row c g r border)
  (let loop ((col 0) (acc '()))
    (if (= col (grid-ncols g))
        (pp:dcat (pp:fold1 (lambda (x y) (pp:dcat x (pp:text " & ") y)) (reverse acc))
                 (pp:text " \\\\"))
        (cases grid-slot (grid-ref g r col)
          (Origin (cell)
            (loop (+ col (table-cell-colspan cell)) (cons (cell-content c cell col border) acc)))
          (Covered (cell orow ocol)
            (cond ((= orow r) (loop (+ col 1) acc))
                  ((> (table-cell-colspan cell) 1)
                   (loop (+ col (table-cell-colspan cell))
                         (cons (multicolumn (table-cell-colspan cell) "l" col border (cell-color cell))
                               acc)))
                  (else (loop (+ col 1) (cons (cell-color cell) acc)))))
          (Missing () (loop (+ col 1) (cons pp:empty acc)))))))

;; [row-rule g r] is the horizontal rule below row r: \hline, or
;; \cline over the columns not continued by a row span.
(define (row-rule g r)
  (let* ((continued? (lambda (col)
                       (and (< (+ r 1) (grid-nrows g))
                            (cases grid-slot (grid-ref g (+ r 1) col)
                              (Covered (cell orow ocol) (<= orow r))
                              (else #f)))))
         (open (remove continued? (iota (grid-ncols g)))))
    (if (= (length open) (grid-ncols g))
        (pp:text "\\hline")
        (pp:text (apply string-append
                        (map (lambda (run) (sprintf "\\cline{~a-~a}" (+ 1 (car run)) (+ 1 (cdr run))))
                             (runs open)))))))

;; [runs cols] groups a sorted list of integers into (first . last)
;; runs of consecutive values.
(define (runs cols)
  (fold-right (lambda (col acc)
                (if (and (pair? acc) (= (+ col 1) (caar acc)))
                    (cons (cons col (cdar acc)) (cdr acc))
                    (cons (cons col col) acc)))
              '() cols))

(define (latex-table c tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (let* ((c      (ctx-with-option c 'in-table #t))
             (ncols  (table-width tbl))
             (border (style-property a 'table 'border #f))
             (rows   (append-map
                      (lambda (named)
                        (let ((g (table-grid (caddr named) ncols)))
                          (append-map (lambda (r)
                                        (cons (grid-row c g r border)
                                              (if border (list (row-rule g r)) '())))
                                      (iota (grid-nrows g)))))
                      (table-parts tbl)))
             (tabular (env "tabular"
                           (append (if border (list (pp:text "\\hline")) '()) rows)
                           (string-append "{" (column-spec tbl colspecs ncols border) "}")))
             (centered (equal? (attr-kv a "align") "center")))
        (if (or (pair? caption) (attr-id a))
            (env "table"
                 (list (pp:text "\\centering")
                       tabular
                       (pp:dcat (cmd "caption" (render-inlines c caption))
                                (if (attr-id a) (cmd "label" (pp:text (attr-id a))) pp:empty)))
                 "[htbp]")
            (if centered (env "center" (list tabular)) tabular))))
    (else pp:empty)))


;; ---------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------

(define (items c docs-of-items)
  (map (lambda (blocks)
         (pp:^^ (pp:text "\\item ") (vsep (render-blocks c blocks))))
       docs-of-items))

;; An anchor for elements that are not numbered by LaTeX.
(define (anchor a)
  (if (attr-id a)
      (pp:^^ (pp:text "\\phantomsection") (cmd "label" (pp:text (attr-id a))))
      pp:empty))

(define (wrap-style a c doc)
  (let ((e (style-property a 'latex 'env #f))
        (m (style-property a 'latex 'macro #f)))
    (cond (e (env e (list doc)))
          (m (cmd m doc))
          (else doc))))

(define (latex-block self b c)
  (let ((c (ctx-deeper c)))
    (cases block b
      (Plain (xs) (render-inlines c xs))
      (Para (xs) (render-inlines c xs))
      (CodeBlock (a s)
        (if (ctx-option c 'in-table #f)
            (cell-block c b)
            (pp:^^ (anchor a) (env "verbatim" (list (verbatim-lines s))))))
      (RawBlock (format s) (if (eq? format 'latex) (verbatim-lines s) pp:empty))
      (BlockQuote (bs) (env "quote" (list (vsep (render-blocks c bs)))))
      (BulletList (is) (env "itemize" (items c is)))
      (OrderedList (is) (env "enumerate" (items c is)))
      (DefinitionList (is)
        (env "description"
             (map (lambda (item)
                    (pp:dcat (pp:text "\\item[{") (render-inlines c (car item)) (pp:text "}] ")
                             (vsep (append-map (lambda (def) (render-blocks c def)) (cdr item)))))
                  is)))
      (Section (division a numbered title body)
        (vsep (cons (pp:dcat (pp:text (string-append "\\" (latex-division-command c division)
                                                     (if numbered "" "*")))
                             pp:lbrace (render-inlines c title) pp:rbrace
                             (if (attr-id a) (cmd "label" (pp:text (attr-id a))) pp:empty))
                    (render-blocks c body))))
      (Definition (a sigs body)
        (vsep (cons (env "description"
                         (map (lambda (sig)
                                (pp:dcat (pp:text (string-append "\\item[" (symbol->string (car sig)) "] "))
                                         (render-inlines c (cdr sig))))
                              sigs))
                    (append (if (attr-id a) (list (anchor a)) '())
                            (render-blocks c body)))))
      (Table (a caption colspecs head bodies foot) (latex-table c b))
      (TableOfContents () (pp:text "\\tableofcontents{}"))
      (Bibliography (a) (latex-bibliography c))
      (HorizontalRule ()
        (pp:text "\\begin{center}\\rule{0.8\\textwidth}{0.4pt}\\end{center}"))
      (Div (a bs)
        (let ((body (vsep (render-blocks c bs))))
          (pp:^^ (anchor a)
                 (if (attr-has-class? a "center")
                     (env "center" (list body))
                     (wrap-style a c body)))))
      (Null () pp:empty)
      (CustomBlock (tag a payload) pp:empty))))


;; ---------------------------------------------------------------------
;; Inlines
;; ---------------------------------------------------------------------

(define (remote? src)
  (or (string-prefix? "http://" src) (string-prefix? "https://" src)))

;; Citations are written for the package given by the option
;; latex-citations: cite (plain LaTeX, the default), natbib or
;; biblatex.  A group of citations without prefixes or locators
;; becomes one command; otherwise each citation gets its own.
(define (latex-cite c cs)
  (let ((style (ctx-option-symbol c 'latex-citations 'cite)))
    (define (command mode)
      (case style
        ((natbib) (case mode ((author-in-text) "citet") ((suppress-author) "citeyearpar") (else "citep")))
        ((biblatex) (case mode ((author-in-text) "textcite") ((suppress-author) "autocite*") (else "autocite")))
        (else "cite")))
    (define (optional cit)
      (let ((pre  (inlines->string (citation-prefix cit)))
            (post (string-trim-both
                   (string-append (or (citation-locator cit) "") " "
                                  (inlines->string (citation-suffix cit))))))
        (cond ((and (string-null? pre) (string-null? post)) "")
              ((or (eq? style 'cite) (string-null? pre))
               (string-append "[" (latex-escape post) "]"))
              (else (string-append "[" (latex-escape pre) "][" (latex-escape post) "]")))))
    (define (one mode cits)
      (pp:text (string-append "\\" (command mode) (optional (car cits))
                              "{" (string-intersperse (map citation-key cits) ",") "}")))
    (if (every (lambda (cit) (and (string-null? (optional cit))
                                  (eq? (citation-mode cit) (citation-mode (car cs)))))
               cs)
        (one (citation-mode (car cs)) cs)
        (pp:fold1 (lambda (x y) (pp:dcat x (pp:text "; ") y))
                  (map (lambda (cit) (one (citation-mode cit) (list cit))) cs)))))

;; [latex-bibliography ctx] is the bibliography for the citation
;; package in use, taking the database file from the option
;; bibliography and the style from bibliography-style.
(define (latex-bibliography c)
  (let ((file  (ctx-option c 'bibliography #f))
        (style (->string (ctx-option c 'bibliography-style
                                     (if (eq? (ctx-option-symbol c 'latex-citations 'cite) 'natbib)
                                         "plainnat" "plain")))))
    (case (ctx-option-symbol c 'latex-citations 'cite)
      ((biblatex) (pp:text "\\printbibliography"))
      (else (if file
                (vcat (list (pp:text (string-append "\\bibliographystyle{" style "}"))
                            (pp:text (string-append "\\bibliography{" (pathname-strip-extension file) "}"))))
                pp:empty)))))

;; An internal link resolves if its target names a target of this
;; document, either as "#anchor" or as a bare key.
(define (internal-target c target)
  (let ((key (if (string-prefix? "#" target) (substring target 1 (string-length target)) target)))
    (let ((t (ref-target c key)))
      (and t (not (xref-target-doc t)) t))))

(define (latex-inline self x c)
  (cases inline x
    (Str (s) (pp:text (render-string c s)))
    (Space () pp:space)
    (SoftBreak () pp:space)
    (LineBreak () (pp:^^ (pp:text "\\\\") pp:hardline))
    (NBSpace () pp:tilde)
    (Emph (xs) (cmd "emph" (render-inlines c xs)))
    (Strong (xs) (cmd "textbf" (render-inlines c xs)))
    (Small (xs) (pp:dcat (pp:text "{\\small ") (render-inlines c xs) pp:rbrace))
    (Code (a s) (cmd "texttt" (pp:text (render-string c s))))
    (Link (a xs target kind)
      (if (eq? kind 'external)
          (if (equal? (inlines->string xs) target)
              (cmd "url" (pp:text (url-escape target)))
              (cmd "href" (pp:text (url-escape target)) (render-inlines c xs)))
          (let ((t (internal-target c target)))
            (if t
                (pp:dcat (pp:text (string-append "\\hyperref[" (xref-target-anchor t) "]"))
                         pp:lbrace (render-inlines c xs) pp:rbrace)
                (render-inlines c xs)))))
    (Image (a alt src)
      (if (remote? src)
          (cmd "href" (pp:text (url-escape src))
               (render-inlines c (if (null? alt) (list (Str src)) alt)))
          (cmd "includegraphics" (pp:text src))))
    (Span (a xs) (wrap-style a c (render-inlines c xs)))
    (RawInline (format s) (if (eq? format 'latex) (verbatim-lines s) pp:empty))
    (Cite (cs xs)
      (if (null? xs) (latex-cite c cs) (render-inlines c xs)))
    (Ref (kind key xs)
      (let ((t (ref-target c key)))
        (cond ((not t) (if (null? xs) (pp:text "???") (render-inlines c xs)))
              ((xref-target-doc t)
               (cmd "href" (pp:text (url-escape (xref-href c t)))
                    (render-inlines c (if (null? xs) (ref-label-inlines c t) xs))))
              ((and (null? xs) (xref-target-number t)
                    (not (eq? (ctx-option-symbol c 'ref-style #f) 'title)))
               (cmd "autoref" (pp:text (xref-target-anchor t))))
              (else
               (pp:dcat (pp:text (string-append "\\hyperref[" (xref-target-anchor t) "]"))
                        pp:lbrace
                        (render-inlines c (if (null? xs) (ref-label-inlines c t) xs))
                        pp:rbrace)))))
    (CustomInline (tag a payload) pp:empty)))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

;; [use-package spec] renders a package given as a name or as a
;; (name option ...) list.
(define (use-package spec)
  (match spec
    ((name . options)
     (pp:text (sprintf "\\usepackage~a{~a}"
                       (if (null? options) "" (string-append "[" (string-intersperse (map ->string options) ",") "]"))
                       name)))
    (name (pp:text (sprintf "\\usepackage{~a}" name)))))

;; Packages needed by the features of the document.
(define (feature-packages c)
  (let ((r (ctx-resolved c)))
    (append
     (if (resolved-has-feature? r 'images) '("graphicx") '())
     (if (resolved-has-feature? r 'table-spans) '("multirow") '())
     (if (resolved-has-feature? r 'table-colors) '("xcolor" "colortbl") '())
     (if (resolved-has-feature? r 'citations)
         (case (ctx-option-symbol c 'latex-citations 'cite)
           ((natbib) '("natbib"))
           ((biblatex) '("biblatex"))
           (else '()))
         '())
     ;; hyperref is conventionally loaded last
     (if (or (resolved-has-feature? r 'links) (resolved-has-feature? r 'refs))
         '("hyperref") '()))))

(define (latex-doc self d c)
  (let ((body (vsep (render-blocks c (doc-blocks d)))))
    (if (not (ctx-option c 'standalone #t))
        body
        (let ((title  (doc-meta-ref d 'title #f))
              (author (doc-meta-ref d 'Author #f))
              (packages (append '(("fontenc" "T1"))
                                (ctx-option c 'latex-packages '())
                                (feature-packages c))))
          (vcat
           (append
            (list (pp:text (sprintf "\\documentclass[~a]{~a}"
                                    (ctx-option c 'class-options "12pt")
                                    (ctx-option c 'document-class "article"))))
            (map use-package (delete-duplicates packages equal?))
            (map pp:text (ctx-option c 'latex-preamble '()))
            (if (and (eq? (ctx-option-symbol c 'latex-citations 'cite) 'biblatex)
                     (ctx-option c 'bibliography #f))
                (list (pp:text (string-append "\\addbibresource{" (ctx-option c 'bibliography) "}")))
                '())
            (list (pp:text "\\sloppy"))
            (if (ctx-option c 'maketitle #f)
                (append (if title (list (cmd "title" (pp:text (latex-escape title)))) '())
                        (if author (list (cmd "author" (pp:text (latex-escape author)))) '()))
                '())
            (list pp:empty
                  (pp:text "\\begin{document}")
                  (if (and (ctx-option c 'maketitle #f) title) (pp:text "\\maketitle") pp:empty)
                  body
                  (pp:text "\\end{document}"))))))))

(define latex-writer
  (register-writer!
   (make-writer name: 'latex
                extension: "tex"
                doc: latex-doc
                block: latex-block
                inline: latex-inline
                text: (lambda (self s c) (latex-escape s)))))

)
