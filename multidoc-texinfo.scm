;;
;; multidoc.texinfo - the Texinfo writer
;;
;; Every section down to the subsubsection level becomes a node.  The
;; top division of the document is written as @chapter, whatever its
;; kind, since Texinfo documents are organized in chapters; deeper
;; divisions follow.  Divisions below @subsubsection become headings
;; without nodes.  Each node with child nodes gets a menu, placed
;; after the text that precedes the first child section.
;;
;; Node names are section titles, with the characters that Texinfo
;; does not allow in node names removed, made unique by a numeric
;; suffix.
;;
;; Options:
;;
;;   standalone       write the header, the Top node and @bye
;;                    (default #t)
;;   setfilename      the Info file name (default: from the title)
;;   texinfo-headers  further header lines, a list of strings
;;   top-division     the division kind of wiki level 2 (default
;;                    section)
;;
;; Copyright (c) 2026 Ivan Raikov
;; This is a reimplementation of the CHICKEN 4 multidoc by Peter Bex,
;; Ivan Raikov.

(module multidoc.texinfo

  (texinfo-writer texinfo-escape texinfo-division-command)

  (import scheme (chicken base) (chicken format) (chicken string)
          (chicken irregex)
          (srfi 1) (srfi 13) (srfi 69)
          datatype matchable
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


;; ---------------------------------------------------------------------
;; Escaping
;; ---------------------------------------------------------------------

(define texinfo-escape
  (make-escaper '((#\@ . "@@") (#\{ . "@{") (#\} . "@}"))))

;; Within the arguments of commands such as @uref, commas separate
;; arguments and must be escaped.
(define texinfo-escape-arg
  (let ((esc (make-escaper '((#\@ . "@@") (#\{ . "@{") (#\} . "@}") (#\, . "@comma{}")))))
    esc))

(define (cmd name body)
  (pp:dcat (pp:text (string-append "@" name "{")) body pp:rbrace))

(define (env name docs #!optional (args ""))
  (vcat (append (list (pp:text (string-append "@" name (if (string-null? args) "" " ") args)))
                docs
                (list (pp:text (string-append "@end " name))))))


;; ---------------------------------------------------------------------
;; Sectioning and nodes
;; ---------------------------------------------------------------------

(define (standard-rank kind)
  (list-index (lambda (k) (eq? k kind)) standard-divisions))

;; Texinfo sectioning commands by rank relative to the top division:
;; 0 is @chapter.
(define numbered-commands   '("chapter" "section" "subsection" "subsubsection"))
(define unnumbered-commands '("unnumbered" "unnumberedsec" "unnumberedsubsec" "unnumberedsubsubsec"))

;; [section-depth ctx division] is the depth of a division below the
;; top division of the document (0 for the top).
(define (section-depth c division)
  (max 0 (- (standard-rank (standard-division division))
            (standard-rank (standard-division (ctx-option-symbol c 'top-division 'section))))))

(define (node-depth? depth) (< depth (length numbered-commands)))

;; [texinfo-division-command ctx division numbered] is the command
;; name for a division, or a heading command below @subsubsection.
(define (texinfo-division-command c division numbered)
  (let ((depth (section-depth c division)))
    (if (node-depth? depth)
        (list-ref (if numbered numbered-commands unnumbered-commands) depth)
        "subsubheading")))

;; Characters not allowed in node names.
(define (node-name-of title)
  (let* ((s (inlines->string title))
         (s (irregex-replace/all "[,:.()@{}]" s ""))
         (s (string-trim-both s)))
    (if (string-null? s) "Section" s)))

;; [node-names ctx] is a hash table from section identifiers to
;; unique node names, for the sections that become nodes.
(define (node-names c)
  (let ((names (make-hash-table equal?))
        (seen  (make-hash-table equal?)))
    (hash-table-set! seen "Top" #t)
    (let walk ((entries (resolved-toc (ctx-resolved c))))
      (for-each
       (lambda (e)
         (when (node-depth? (section-depth c (toc-entry-division e)))
           (let loop ((n 1))
             (let* ((base (node-name-of (toc-entry-title e)))
                    (name (if (= n 1) base (string-append base " " (number->string n)))))
               (if (hash-table-exists? seen name)
                   (loop (+ n 1))
                   (begin (hash-table-set! seen name #t)
                          (hash-table-set! names (toc-entry-id e) name))))))
         (walk (toc-entry-children e)))
       entries))
    names))

(define (node-name c id)
  (and id (hash-table-ref/default (ctx-option c 'texinfo-node-names (make-hash-table)) id #f)))

;; [menu ctx blocks] is a menu of the sections among blocks that are
;; nodes, or empty.
(define (menu c blocks)
  (let ((entries (filter-map
                  (lambda (b)
                    (cases block b
                      (Section (division a numbered title body)
                        (let ((name (node-name c (attr-id a))))
                          (and name (pp:text (string-append "* " name "::")))))
                      (else #f)))
                  blocks)))
    (if (null? entries) pp:empty (env "menu" entries))))

(define (section? b)
  (cases block b (Section (division a numbered title body) #t) (else #f)))

;; [node-body ctx blocks] renders the blocks of a node: the text up to
;; the first section, the menu, and then the rest.
(define (node-body c blocks)
  (receive (intro rest) (break section? blocks)
    (vsep (append (render-blocks c intro)
                  (let ((m (menu c blocks))) (if (eq? m pp:empty) '() (list m)))
                  (render-blocks c rest)))))


;; ---------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------

(define (cell-inlines c cell)
  (pp:fold1 (lambda (x y) (pp:dcat x pp:space y))
            (map (lambda (b)
                   (cases block b
                     (Plain (xs) (render-inlines c xs))
                     (Para (xs) (render-inlines c xs))
                     (CodeBlock (a s) (cmd "code" (pp:text (texinfo-escape s))))
                     (else (render-block c b))))
                 (table-cell-blocks cell))))

;; Multitable has no spans: each slot of the grid gets a cell, and
;; slots covered by a span are left empty.
(define (texinfo-table c tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (let* ((ncols (table-width tbl))
             (rows  (append-map
                     (lambda (named)
                       (let ((g (table-grid (caddr named) ncols))
                             (item (if (eq? (car named) 'head) "@headitem " "@item ")))
                         (map (lambda (row)
                                (pp:^^ (pp:text item)
                                       (pp:fold1 (lambda (x y) (pp:dcat x (pp:text " @tab ") y))
                                                 (map (lambda (slot)
                                                        (cases grid-slot slot
                                                          (Origin (cell) (cell-inlines c cell))
                                                          (else pp:empty)))
                                                      row))))
                              (grid-rows g))))
                     (table-parts tbl)))
             (fraction (/ (round (* 1000 (/ 1 (max ncols 1)))) 1000.)))
        (vcat (append
               (if (attr-id a) (list (pp:text (string-append "@anchor{" (attr-id a) "}"))) '())
               (list (env "multitable" rows
                          (string-append "@columnfractions "
                                         (string-intersperse
                                          (map (lambda (k) (number->string fraction)) (iota ncols))
                                          " "))))
               (if (null? caption) '()
                   (list (pp:^^ (pp:text "@noindent ") (render-inlines c caption))))))))
    (else pp:empty)))


;; ---------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------

(define (anchor a)
  (if (attr-id a) (pp:text (string-append "@anchor{" (attr-id a) "}")) pp:empty))

(define (items c item-lists)
  (map (lambda (blocks) (pp:^^ (pp:text "@item ") (vsep (render-blocks c blocks))))
       item-lists))

(define (texinfo-block self b c)
  (let ((c (ctx-deeper c)))
    (cases block b
      (Plain (xs) (render-inlines c xs))
      (Para (xs) (render-inlines c xs))
      (CodeBlock (a s)
        (vcat (remove (lambda (d) (eq? d pp:empty))
                      (list (anchor a) (env "example" (list (verbatim-lines (texinfo-escape s))))))))
      (RawBlock (format s) (if (eq? format 'texinfo) (verbatim-lines s) pp:empty))
      (BlockQuote (bs) (env "quotation" (list (vsep (render-blocks c bs)))))
      (BulletList (is) (env "itemize" (items c is) "@bullet"))
      (OrderedList (is) (env "enumerate" (items c is)))
      (DefinitionList (is)
        (env "table"
             (map (lambda (item)
                    (vcat (cons (pp:^^ (pp:text "@item ") (render-inlines c (car item)))
                                (append-map (lambda (def) (render-blocks c def)) (cdr item)))))
                  is)
             "@asis"))
      (Section (division a numbered title body)
        (let* ((command (texinfo-division-command c division numbered))
               (name    (node-name c (attr-id a)))
               (heading (pp:dcat (pp:text (string-append "@" command " "))
                                 (render-inlines c title))))
          (vsep (list (if name
                          (vcat (list (pp:text (string-append "@node " name)) heading))
                          (vcat (list (anchor a) heading)))
                      (node-body c body)))))
      (Definition (a sigs body)
        (let ((lines (map (lambda (sig k)
                            (pp:text (sprintf "@deffn~a {~a} {~a}" (if (= k 0) "" "x") (car sig)
                                              (texinfo-escape (inlines->string (cdr sig))))))
                          sigs (iota (length sigs)))))
          (if (null? lines)
              (vsep (render-blocks c body))
              (vcat (append (if (attr-id a) (list (anchor a)) '())
                            lines
                            (let ((body (render-blocks c body)))
                              (if (null? body) '() (list (vsep body))))
                            (list (pp:text "@end deffn")))))))
      (Table (a caption colspecs head bodies foot) (texinfo-table c b))
      (TableOfContents () (pp:text "@contents"))
      (Bibliography (a) pp:empty)
      (HorizontalRule () pp:empty)
      (Div (a bs)
        (vcat (remove (lambda (d) (eq? d pp:empty))
                      (list (anchor a) (vsep (render-blocks c bs))))))
      (Null () pp:empty)
      (CustomBlock (tag a payload) pp:empty))))


;; ---------------------------------------------------------------------
;; Inlines
;; ---------------------------------------------------------------------

(define (remote? src)
  (or (string-prefix? "http://" src) (string-prefix? "https://" src)))

;; The node name or anchor to use in @ref for a target.
(define (ref-node c t)
  (or (node-name c (xref-target-anchor t)) (xref-target-anchor t)))

(define (texinfo-inline self x c)
  (cases inline x
    (Str (s) (pp:text (render-string c s)))
    (Space () pp:space)
    (SoftBreak () pp:space)
    (LineBreak () (pp:^^ (pp:text "@*") pp:hardline))
    (NBSpace () (pp:text "@tie{}"))
    (Emph (xs) (cmd "emph" (render-inlines c xs)))
    (Strong (xs) (cmd "strong" (render-inlines c xs)))
    (Small (xs) (render-inlines c xs))
    (Code (a s) (cmd "code" (pp:text (render-string c s))))
    (Link (a xs target kind)
      (if (eq? kind 'external)
          (if (equal? (inlines->string xs) target)
              (cmd "uref" (pp:text (texinfo-escape-arg target)))
              (cmd "uref" (pp:text (string-append (texinfo-escape-arg target) ", "
                                                  (texinfo-escape-arg (inlines->string xs))))))
          (let* ((key (if (string-prefix? "#" target) (substring target 1 (string-length target)) target))
                 (t   (ref-target c key)))
            (if (and t (not (xref-target-doc t)))
                (cmd "ref" (pp:text (string-append (ref-node c t) ",,"
                                                   (texinfo-escape-arg (inlines->string xs)))))
                (render-inlines c xs)))))
    (Image (a alt src)
      (if (remote? src)
          (cmd "uref" (pp:text (string-append (texinfo-escape-arg src) ", "
                                              (texinfo-escape-arg (inlines->string alt)))))
          (cmd "image" (pp:text (texinfo-escape-arg src)))))
    (Span (a xs) (render-inlines c xs))
    (RawInline (format s) (if (eq? format 'texinfo) (verbatim-lines s) pp:empty))
    (Cite (cs xs) (render-inlines c (if (null? xs) (cite-fallback-inlines c cs) xs)))
    (Ref (kind key xs)
      (let ((t (ref-target c key)))
        (cond ((not t) (if (null? xs) (pp:text "???") (render-inlines c xs)))
              ((xref-target-doc t)
               (cmd "uref" (pp:text (string-append
                                     (texinfo-escape-arg (xref-href c t))
                                     ", "
                                     (texinfo-escape-arg
                                      (inlines->string (if (null? xs) (ref-label-inlines c t) xs)))))))
              ((null? xs) (cmd "ref" (pp:text (ref-node c t))))
              (else (cmd "ref" (pp:text (string-append (ref-node c t) ",,"
                                                       (texinfo-escape-arg (inlines->string xs)))))))))
    (CustomInline (tag a payload) pp:empty)))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

(define (info-file-name c d)
  (or (ctx-option c 'setfilename #f)
      (string-append (title->anchor-id (or (doc-meta-ref d 'title #f) "multidoc")) ".info")))

(define (texinfo-doc self d c)
  (let* ((c      (ctx-with-option c 'texinfo-node-names (node-names c)))
         (blocks (doc-blocks d)))
    (if (not (ctx-option c 'standalone #t))
        (node-body c blocks)
        (let ((title  (doc-meta-ref d 'title #f))
              (author (doc-meta-ref d 'Author #f)))
          (vcat
           (append
            (list (pp:text "\\input texinfo")
                  (pp:text (string-append "@setfilename " (info-file-name c d))))
            (if title (list (pp:text (string-append "@settitle " (texinfo-escape title)))) '())
            (map pp:text (ctx-option c 'texinfo-headers '()))
            (list pp:empty
                  (env "titlepage"
                       (append (if title (list (pp:text (string-append "@title " (texinfo-escape title)))) '())
                               (if author (list (pp:text (string-append "@author " (texinfo-escape author)))) '())))
                  pp:empty
                  (pp:text "@node Top")
                  (pp:text (string-append "@top " (texinfo-escape (or title "Top"))))
                  pp:empty
                  (node-body c blocks)
                  pp:empty
                  (pp:text "@bye"))))))))

(define texinfo-writer
  (register-writer!
   (make-writer name: 'texinfo
                extension: "texi"
                doc: texinfo-doc
                block: texinfo-block
                inline: texinfo-inline
                text: (lambda (self s c) (texinfo-escape s)))))

)
