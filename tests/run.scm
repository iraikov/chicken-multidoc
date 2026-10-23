;;
;; Tests for multidoc.
;;
;; Run from the tests directory:  csi -s run.scm
;; With MULTIDOC_REGENERATE set, the golden files in expected/ are
;; rewritten from the current output instead of compared.
;;

(import scheme (only (scheme base) open-output-string open-input-string get-output-string)
        (chicken base) (chicken file) (chicken pathname)
        (chicken port) (chicken io) (chicken string) (chicken format) (chicken sort)
        (chicken process-context)
        (srfi 1) (srfi 13)
        datatype test
        (prefix wl-pprint pp:)
        multidoc multidoc.cli)

(define regenerate? (get-environment-variable "MULTIDOC_REGENERATE"))

(define (read-sxml file) (with-input-from-file file read))

;; [output d to #!key width options] renders document d to a string.
(define (output d to #!key (width 72) (options '()))
  (layout->string (render d to: to width: width options: options) width))

(define (sxml-output sxml to #!key (width 72) (options '()) (top-division 'section))
  (call-with-output-string
   (lambda (port)
     (convert sxml to: to width: width options: options top-division: top-division port: port))))

(define (contains? s part) (and (string-contains s part) #t))

(define (first-inlines d)
  (let loop ((bs (doc-blocks d)))
    (cases block (car bs)
      (Section (division a numbered title body) (loop body))
      (Para (xs) xs)
      (else (loop (cdr bs))))))

(define (block-tag b)
  (cases block b
    (Plain (xs) 'Plain) (Para (xs) 'Para) (BulletList (is) 'BulletList)
    (Section (division a numbered title body) 'Section)
    (else 'other)))

(define (inline-tag x)
  (cases inline x (Str (s) s) (Space () '_) (Emph (xs) 'Emph) (Code (a s) 'Code) (else 'other)))


(test-begin "multidoc")

;; ---------------------------------------------------------------------
(test-group "reader"

  (test "text is split into words and spaces"
        '("Two" _ "words," _ Emph _ "here.")
        (map inline-tag (first-inlines (sxml->doc '(body (p "Two  words, " (em "x") " here."))))))

  (test "list items with text become Plain blocks"
        '((Plain) (Plain Para))
        (let ((d (sxml->doc '(body (ul (li "one") (li "two" (p "three")))))))
          (cases block (car (doc-blocks d))
            (BulletList (items) (map (lambda (bs) (map block-tag bs)) items))
            (else #f))))

  (test "section* is unnumbered"
        '(#t #f)
        (map (lambda (b) (cases block b (Section (d a numbered t body) numbered) (else 'x)))
             (doc-blocks (sxml->doc '(body (section 2 "A") (section* 2 "B"))))))

  (test "top-division maps wiki levels to division kinds"
        '(chapter section)
        (let ((d (sxml->doc '(body (section 2 "A" (section 3 "B"))) top-division: 'chapter)))
          (cases block (car (doc-blocks d))
            (Section (division a n t body)
              (list division (cases block (car body) (Section (dv a n t b) dv) (else #f))))
            (else #f))))

  (test "repeated titles get unique identifiers"
        '("page-title" "first-section" "subsection" "second-section" "subsection-2" "nested-lists")
        (receive (d r) (resolve-doc (sxml->doc (read-sxml "simple.sxml")))
          (let walk ((es (resolved-toc r)))
            (append-map (lambda (e) (cons (toc-entry-id e) (walk (toc-entry-children e)))) es))))

  (test "int-link targets are normalized to anchor identifiers"
        "Page#first-section"
        (let ((d (sxml->doc '(body (p (int-link "Page#First Section" "x"))))))
          (cases inline (car (first-inlines d))
            (Link (a xs target kind) target)
            (else #f))))

  (test "labels give elements identifiers"
        '(#t #t)
        (receive (d r) (resolve-doc (sxml->doc '(body (table (label "t") (tr (td "x")))
                                                      (def (label "f") (sig (procedure "(f)")) "F."))))
          (list (xref-target? (resolved-lookup r "t")) (xref-target? (resolved-lookup r "f")))))

  (test "user reader rules take precedence"
        '("note")
        (parameterize ((sxml-reader-rules (sxml-reader-rules)))
          (sxml-reader-add-rule! 'note (lambda (tag attrs children recur)
                                         (Div (make-attr #f '("note") '() #f) (recur children 'blocks))))
          (cases block (car (doc-blocks (sxml->doc '(body (note "careful")))))
            (Div (a bs) (attr-classes a))
            (else #f)))))

;; ---------------------------------------------------------------------
(test-group "every test document renders with every writer"
  (for-each
   (lambda (file)
     (let ((sxml (read-sxml file)))
       (for-each
        (lambda (name)
          (test-assert (sprintf "~a with ~a" file name)
                       (> (string-length (sxml-output sxml name width: 80)) 0)))
        (registered-writers))))
   (sort (glob "*.sxml") string<?)))

;; ---------------------------------------------------------------------
(test-group "golden files"
  (let ((sxml (read-sxml "simple.sxml")))
    (define (golden file to width)
      (let ((path (make-pathname "expected" file))
            (text (sxml-output sxml to width: width)))
        (if regenerate?
            (begin (create-directory "expected")
                   (with-output-to-file path (lambda () (display text)))
                   (test-assert (sprintf "regenerated ~a" path) #t))
            (test-assert (sprintf "~a matches ~a" to path)
                         (equal? (with-input-from-file path (lambda () (read-string #f)))
                                 text)))))
    (golden "simple.html" 'html 72)
    (golden "simple.tex" 'latex 72)
    (golden "simple.texi" 'texinfo 72)
    (golden "simple.md" 'markdown 72)
    (golden "simple.txt" 'plain 72)
    (golden "simple-40.txt" 'plain 40)
    (golden "simple.native" 'native 72)))

;; ---------------------------------------------------------------------
(test-group "registry and derived writers"

  (define toy
    (make-writer name: 'toy extension: "toy"
                 doc: (lambda (self d c) (vcat (render-blocks c (doc-blocks d))))
                 block: (lambda (self b c)
                          (cases block b
                            (Para (xs) (render-inlines c xs))
                            (Section (dv a n title body)
                              (vcat (cons (pp:^^ (pp:text "# ") (render-inlines c title))
                                          (render-blocks c body))))
                            (else pp:empty)))
                 inline: (lambda (self x c)
                           (cases inline x (Str (s) (pp:text s)) (else pp:empty)))))
  (register-writer! toy)

  (test "a registered writer is listed" #t (and (memq 'toy (registered-writers)) #t))
  (test "a registered writer renders" "# Hi\nthere"
        (output (sxml->doc '(body (section 2 "Hi" (p "there")))) 'toy))

  (let* ((fancy (derive-writer html-writer name: 'fancy
                               block: (lambda (self b c next)
                                        (cases block b
                                          (Definition (a sigs body) (pp:text "FANCY"))
                                          (else (next b c))))))
         (out (output (sxml->doc '(body (ul (li (def (sig (procedure "(f)")) "F.")))))
                      fancy options: '((standalone . #f)))))
    (test "an override applies inside a nested list" #t (contains? out "<li>\nFANCY\n</li>"))
    (test "other elements use the parent writer" #t (contains? out "<ul>"))
    (test "the derived writer knows its parent" 'html (writer-name (writer-parent fancy)))))

;; ---------------------------------------------------------------------
(test-group "write-output"
  (let* ((d (sxml->doc '(body (p "Some text."))))
         (w plain-writer))
    (define (write-to dest #!optional (w w))
      (receive (d c) (make-render-context w d '((standalone . #f)))
        (write-output w d c dest)))
    (let* ((tmp       (create-temporary-directory))
           (path      (make-pathname tmp "out.txt"))
           (to-port   (call-with-output-string (lambda (p) (write-to p))))
           (to-stdout (with-output-to-string (lambda () (write-to #f)))))
      (write-to path)
      (test "a port, a path and #f receive the same text" #t
            (and (equal? to-port to-stdout)
                 (equal? to-port (with-input-from-file path (lambda () (read-string #f))))))
      (test "an output override can call the default"
            (string-append to-port "-- end\n")
            (call-with-output-string
             (lambda (p)
               (write-to p (derive-writer w name: 'trailer
                                          output: (lambda (self d c dest next)
                                                    (next d c dest)
                                                    (display "-- end\n" dest)))))))

      ;; A writer of kind directory: one file per top-level section.
      (let ((pages (derive-writer
                    plain-writer name: 'pages output-kind: 'directory
                    output: (lambda (self d c dest next)
                              (create-directory dest #t)
                              (for-each (lambda (b k)
                                          (with-output-to-file (make-pathname dest (sprintf "page~a.txt" k))
                                            (lambda ()
                                              (display (layout->string (render-block c b) 72))
                                              (newline))))
                                        (doc-blocks d) (iota (length (doc-blocks d))))))))
        (register-writer! pages)
        (let ((dir (make-pathname tmp "pages")))
          (receive (d c) (make-render-context
                          pages (sxml->doc '(body (section 2 "One" (p "a")) (section 2 "Two" (p "b"))))
                          '())
            (write-output pages d c dir))
          (test "a directory writer writes one file per section" '("page0.txt" "page1.txt")
                (sort (directory dir) string<?)))
        (test "the command line refuses a directory writer without -o" 2
              (parameterize ((current-error-port (open-output-string)))
                (multidoc-main '("-t" "pages" "simple.sxml"))))
        (test "the command line accepts it with -o" 0
              (multidoc-main (list "-t" "pages" "-o" (make-pathname tmp "cli-pages") "simple.sxml"))))))
  (let ((run (lambda args
               (let ((out (open-output-string)))
                 (list (parameterize ((current-output-port out) (current-error-port (open-output-string))
                                      (current-input-port (open-input-string "(body (p \"from stdin\"))")))
                         (multidoc-main args))
                       (get-output-string out))))))
    (test "long options take their value after =" '(0 "from stdin\n")
          (run "--to=plain" "--width=40" "--option=standalone=#f"))
    (test "a long option without = has no value" 2 (car (run "--to" "plain")))
    (test "- reads standard input" '(0 "from stdin\n") (run "-t" "plain" "-V" "standalone=#f" "-"))
    (test "-o - writes to standard output" '(0 "from stdin\n")
          (run "-t" "plain" "-V" "standalone=#f" "-o" "-" "-"))
    (test "--output=- writes to standard output" '(0 "from stdin\n")
          (run "-t" "plain" "-V" "standalone=#f" "--output=-"))
    (test "short options may be combined" 0 (car (run "-hw" "40")))
    (test "-V overrides the defaults" #f (contains? (cadr (run "-t" "html" "-V" "standalone=#f")) "<html"))
    (test "a later -V wins" #t
          (contains? (cadr (run "-t" "html" "-V" "standalone=#f" "--option=standalone=#t")) "<html"))
    (test "bad values are usage errors" '(2 2 2)
          (map (lambda (args) (car (apply run args))) '(("-w" "0") ("-V" "novalue") ("--bogus"))))))

;; ---------------------------------------------------------------------
(test-group "custom nodes"
  (define-custom-node 'note
    lower: (lambda (a payload) (list (Para (list (Strong (list (Str "Note:"))) (Space) (Str payload))))))
  (define-node-writer 'note 'html (lambda (a payload c) (pp:text (string-append "<aside>" payload "</aside>"))))
  (let ((d (make-doc '() (list (CustomBlock 'note null-attr "careful"))))
        (fancy (derive-writer html-writer name: 'fancy2)))
    (test "html uses its handler" "<aside>careful</aside>" (output d 'html options: '((standalone . #f))))
    (test "a derived writer inherits the handler" "<aside>careful</aside>"
          (output d fancy options: '((standalone . #f))))
    (test "other writers lower the node" "*Note:* careful" (output d 'plain options: '((standalone . #f))))
    (test "coverage is native for html" '((note . native)) (writer-coverage html-writer d))
    (test "coverage is inherited for the derived writer" '((note inherited . html))
          (writer-coverage fancy d))
    (test "coverage is lowered for latex" '((note . lowered)) (writer-coverage latex-writer d))
    (test "undeclared nodes are reported" '((mystery . undeclared))
          (writer-coverage html-writer (make-doc '() (list (CustomBlock 'mystery null-attr #f)))))))

;; ---------------------------------------------------------------------
(test-group "divisions"
  (define sxml '(body (section 2 "A" (section 3 "B" (section 4 "C")))))
  (define (latex top class)
    (sxml-output sxml 'latex top-division: top
                 options: `((standalone . #f) (document-class . ,class))))
  (test "article, top section" #t
        (every (lambda (s) (contains? (latex 'section "article") s))
               '("\\section{A}" "\\subsection{B}" "\\subsubsection{C}")))
  (test "article, top chapter: chapters become sections" #t
        (every (lambda (s) (contains? (latex 'chapter "article") s))
               '("\\section{A}" "\\subsection{B}")))
  (test "book, top chapter" #t
        (every (lambda (s) (contains? (latex 'chapter "book") s))
               '("\\chapter{A}" "\\section{B}")))
  (test "html headings are relative to the top division" #t
        (every (lambda (s) (contains? (sxml-output sxml 'html top-division: 'chapter
                                                   options: '((standalone . #f))) s))
               '("<h2 id=\"a\">" "<h3 id=\"b\">")))
  (test "texinfo always starts at @chapter" #t
        (every (lambda (s) (contains? (sxml-output sxml 'texinfo options: '((standalone . #f))) s))
               '("@chapter A" "@section B" "@subsection C")))
  (test "shift-divisions moves sections down"
        '(subsection subsubsection)
        (let ((d ((shift-divisions 1) (sxml->doc '(body (section 2 "A" (section 3 "B")))))))
          (cases block (car (doc-blocks d))
            (Section (dv a n t body)
              (list dv (cases block (car body) (Section (dv2 a n t b) dv2) (else #f))))
            (else #f))))
  (test "shift-divisions moves sections up"
        'chapter
        (cases block (car (doc-blocks ((shift-divisions -1) (sxml->doc '(body (section 2 "A"))))))
          (Section (dv a n t body) dv)
          (else #f)))
  (test "a custom kind falls back to the standard kind of the same rank" 'subsection
        (parameterize ((division-scheme '(part chapter section unit)))
          (standard-division 'unit))))

;; ---------------------------------------------------------------------
(test-group "styles"
  (let ((d (make-doc '() (list (Div (make-attr #f '() '()
                                               (make-style "note" '((html (class "admonition"))
                                                                    (latex (env "quote")))))
                                    (list (Para (list (Str "x")))))))))
    (let ((html  (output d 'html options: '((standalone . #f))))
          (latex (output d 'latex options: '((standalone . #f)))))
      (test "html applies its own properties" #t (contains? html "class=\"admonition\""))
      (test "html ignores latex properties" #f (contains? html "quote"))
      (test "latex applies its own properties" #t (contains? latex "\\begin{quote}"))
      (test "latex ignores html properties" #f (contains? latex "admonition")))))

;; ---------------------------------------------------------------------
(test-group "tables"
  (define (cell name #!key (rs 1) (cs 1) (a null-attr))
    (make-table-cell a 'default rs cs (list (Plain (list (Str name))))))
  (define (row . cells) (make-table-row null-attr cells))
  (define hot (make-attr #f '() '() (make-style "hot" '((table (bg "#ff0000") (bold #t))))))
  (define t
    (Table null-attr (list (Str "Styled")) '()
           (make-table-part null-attr (list (row (cell "A" cs: 2) (cell "B" rs: 2))
                                            (row (cell "C") (cell "D"))))
           (list (make-table-part null-attr (list (row (cell "x") (cell "y" rs: 2 a: hot) (cell "z"))
                                                  (row (cell "u") (cell "w")))))
           (make-table-part null-attr (list (row (cell "total" cs: 3))))))
  (define d (make-doc '() (list t)))
  (define (slot-names part)
    (map (lambda (r)
           (map (lambda (s) (cases grid-slot s
                              (Origin (c) 'O) (Covered (c row col) (list row col)) (Missing () '-)))
                r))
         (grid-rows (table-grid part (table-width t)))))

  (test "the grid is rectangular and marks covered slots"
        '((O (0 0) O) (O O (0 2)))
        (slot-names (caddr (car (table-parts t)))))
  (test "cells are selected by part and column" '("B" "z" "w")
        (map (lambda (ref) (inlines->string (cases block (car (table-cell-blocks (cell-ref-cell ref)))
                                              (Plain (xs) xs) (else '()))))
             (table-cells t col: 2)))
  (test "resolve-doc records span and colour features" '(tables table-spans table-colors)
        (receive (d r) (resolve-doc d) (resolved-features r)))

  (let ((html (output d 'html options: '((standalone . #f)))))
    (test "html has spans" #t (and (contains? html "colspan=\"2\"") (contains? html "rowspan=\"2\"")))
    (test "html has the cell style" #t (contains? html "background-color: #ff0000")))
  (let ((latex (output d 'latex)))
    (test "latex uses multicolumn and multirow" #t
          (and (contains? latex "\\multicolumn{2}") (contains? latex "\\multirow{2}")))
    (test "latex loads the packages for spans and colours" #t
          (and (contains? latex "\\usepackage{multirow}") (contains? latex "\\usepackage{colortbl}"))))
  (let ((md (output d 'markdown options: '((standalone . #f)))))
    (test "markdown falls back to a grid table" #t
          (and (contains? md "+=====") (contains? md "| A     | B |"))))
  (test "a simple table becomes a pipe table" #t
        (contains? (sxml-output '(body (table (tr (th "a") (th "b")) (tr (td "1") (td "2"))))
                                'markdown options: '((standalone . #f)))
                   "| a   | b   |\n|-----|-----|"))
  (let* ((wide (make-doc '() (list (Table null-attr '() '()
                                          (make-table-part null-attr (list (row (cell "Name") (cell "Description"))))
                                          (list (make-table-part
                                                 null-attr
                                                 (list (row (cell "alpha")
                                                            (make-table-cell null-attr 'default 1 1
                                                                             (list (Plain (string->inlines "A rather long description that has to wrap inside its cell when the table is narrow."))))))))
                                          empty-table-part))))
         (lines-at (lambda (width)
                     (string-split (output wide 'plain width: width options: '((standalone . #f))) "\n"))))
    (for-each
     (lambda (width)
       (let ((lines (lines-at width)))
         (test-assert (sprintf "plain table rows line up at width ~a" width)
                      (let ((n (string-length (car lines))))
                        (every (lambda (l) (= (string-length l) n)) lines)))
         (test-assert (sprintf "plain table fits in width ~a" width)
                      (every (lambda (l) (<= (string-length l) width)) lines))))
     '(72 40)))
  (test "the bb table renders with every writer" #t
        (let ((sxml (read-sxml "bb.sxml")))
          (every (lambda (w) (> (string-length (sxml-output sxml w)) 0))
                 (filter (lambda (w) (eq? (writer-output-kind (lookup-writer w)) 'text))
                         (registered-writers))))))


;; ---------------------------------------------------------------------
(test-group "references"

  (test "a labelled section can also be found by its title" "intro"
        (receive (d r) (resolve-doc (sxml->doc '(body (section 2 "Introduction" (label "intro")))))
          (xref-target-anchor (resolved-lookup r "introduction"))))
  (test "a title never hides the identifier of another element" "introduction"
        (receive (d r) (resolve-doc (sxml->doc '(body (section 2 "Introduction" (label "intro"))
                                                     (section 2 "Other" (label "introduction")))))
          (xref-target-anchor (resolved-lookup r "introduction"))))
  (define sxml
    '(body (section 2 "Intro" (@ (id "intro"))
             (p "See " (ref "deep") ", " (ref "tbl") ", " (ref "foo") ", " (ref "deep" "the deep part")
                " and " (ref "nope") ".")
             (def (label "foo") (sig (procedure "(foo x)")) "Foo.")
             (table (label "tbl") (@ (caption "Numbers")) (tr (th "n")) (tr (td "1")))
             (section 3 "Deep" (@ (id "deep")) (p "x")))))
  (define (quietly thunk) (parameterize ((current-error-port (open-output-string))) (thunk)))
  (let ((html    (quietly (lambda () (sxml-output sxml 'html width: 1000 options: '((standalone . #f))))))
        (latex   (quietly (lambda () (sxml-output sxml 'latex width: 1000 options: '((standalone . #f))))))
        (texinfo (quietly (lambda () (sxml-output sxml 'texinfo width: 1000 options: '((standalone . #f)))))))
    (test "html links references with generated text" #t
          (every (lambda (s) (contains? html s))
                 '("<a class=\"internal\" href=\"#deep\">Deep</a>"
                   "<a class=\"internal\" href=\"#tbl\">Numbers</a>"
                   "<a class=\"internal\" href=\"#foo\"><tt>(foo x)</tt></a>")))
    (test "html keeps explicit reference text" #t (contains? html ">the deep part</a>"))
    (test "latex uses autoref for numbered targets" #t
          (and (contains? latex "\\autoref{deep}") (contains? latex "\\autoref{tbl}")))
    (test "latex uses hyperref for other targets" #t
          (and (contains? latex "\\hyperref[foo]") (contains? latex "\\hyperref[deep]{the deep part}")))
    (test "texinfo refers to nodes and anchors" #t
          (every (lambda (s) (contains? texinfo s)) '("@ref{Deep}" "@ref{tbl}" "@ref{foo}")))
    (test "an unresolved reference renders as ???" #t (contains? html " and ???.")))
  (test "an unresolved reference is reported" #t
        (contains? (call-with-output-string
                    (lambda (port)
                      (parameterize ((current-error-port port))
                        (sxml-output sxml 'plain))))
                   "unresolved reference: \"nope\""))
  (test "ref-style number refers by kind and number" #t
        (contains? (sxml-output '(body (section 2 "A" (@ (id "a")) (p (ref "a"))))
                                'plain options: '((standalone . #f) (ref-style . number)))
                   "section 1")))

;; ---------------------------------------------------------------------
(test-group "citations"
  (define sxml
    '(body (p (cite (@ (mode "author-in-text")) "Wu1992") " showed "
              (cite "Wu1992" "Weinberg1986") " and "
              (cite (@ (prefix "see") (locator "p. 3")) "Weinberg1986") ".")
           (bibliography)))
  (define (quietly thunk) (parameterize ((current-error-port (open-output-string))) (thunk)))
  (test "the reader makes citations" '(("Wu1992") ("Wu1992" "Weinberg1986") ("Weinberg1986"))
        (query-inlines (lambda (x acc)
                         (cases inline x
                           (Cite (cs xs) (append acc (list (map citation-key cs))))
                           (else acc)))
                       '() (doc-blocks (sxml->doc sxml))))
  (test "cited keys are in order of first citation" '("Wu1992" "Weinberg1986")
        (cited-keys (sxml->doc sxml)))
  (let ((natbib (sxml-output sxml 'latex options: '((latex-citations . natbib) (bibliography . "refs.bib")))))
    (test "natbib commands" #t
          (every (lambda (s) (contains? natbib s))
                 '("\\citet{Wu1992}" "\\citep{Wu1992,Weinberg1986}" "\\citep[see][p. 3]{Weinberg1986}"
                   "\\usepackage{natbib}" "\\bibliographystyle{plainnat}" "\\bibliography{refs}"))))
  (let ((biblatex (sxml-output sxml 'latex options: '((latex-citations . "biblatex") (bibliography . "refs.bib")))))
    (test "biblatex commands" #t
          (every (lambda (s) (contains? biblatex s))
                 '("\\textcite{Wu1992}" "\\autocite{Wu1992,Weinberg1986}" "\\addbibresource{refs.bib}"
                   "\\printbibliography"))))
  (test "markdown uses pandoc citation syntax" #t
        (contains? (sxml-output sxml 'markdown width: 200 options: '((standalone . #f)))
                   "@Wu1992 showed [@Wu1992; @Weinberg1986] and [see @Weinberg1986, p. 3]."))
  (test "writers without citation syntax warn once per key" 2
        (length (string-split (call-with-output-string
                               (lambda (port)
                                 (parameterize ((current-error-port port))
                                   (sxml-output sxml 'plain))))
                              "\n")))
  (let ((plain (sxml-output sxml 'plain width: 200 options: '((standalone . #f) (citations . plain-keys)))))
    (test "plain-keys writes keys in brackets" #t
          (contains? plain "[Wu1992] showed [Wu1992; Weinberg1986] and [Weinberg1986]."))
    (test "plain-keys lists the cited keys as the bibliography" #t
          (contains? plain "[Wu1992]\n\n[Weinberg1986]"))))

;; ---------------------------------------------------------------------
(test-group "cross-document references"
  (define page-a '(wiki-page (Header (title "Page A"))
                    (body (section 2 "Intro" (p "a")
                            (table (label "tbl-a") (@ (caption "Numbers")) (tr (td "1")))))))
  (define page-b '(body (p "Back to " (int-link "A#Intro") ", the page " (int-link "A")
                           ", its table " (ref "A#tbl-a") ", and " (int-link "A#Nowhere" "a missing part") ".")))
  (define (quietly thunk) (parameterize ((current-error-port (open-output-string))) (thunk)))
  (let* ((tmp  (create-temporary-directory))
         (file (make-pathname tmp "a.xref"))
         (targets (export-xrefs (sxml->doc page-a) "A")))
    (refdb-save (make-xref-db targets) file)
    (test "exported targets include the page, its sections and its table" '("A" "intro" "tbl-a")
          (sort (map xref-target-key targets) string<?))
    (test "a saved database reads back the same targets" #t
          (let ((back (refdb-targets (load-xref-db file))))
            (and (= (length back) (length targets))
                 (every (lambda (t)
                          (let ((u (find (lambda (u) (equal? (xref-target-key u) (xref-target-key t))) back)))
                            (and u (equal? (xref-target-number u) (xref-target-number t))
                                 (equal? (xref-target-anchor u) (xref-target-anchor t))
                                 (equal? (inlines->string (xref-target-title u))
                                         (inlines->string (xref-target-title t))))))
                        targets))))
    (let ((html (quietly (lambda ()
                           (sxml-output page-b 'html width: 200
                                        options: `((standalone . #f) (refdb . ,(load-xref-db file))
                                                   (xref-suffix . ".html")))))))
      (test "int-links resolve through the database" #t
            (every (lambda (s) (contains? html s))
                   '("<a class=\"internal\" href=\"A.html#intro\">Intro</a>"
                     "<a class=\"internal\" href=\"A.html\">Page A</a>"
                     "<a class=\"internal\" href=\"A.html#tbl-a\">Numbers</a>")))
      (test "an unresolved int-link keeps its text" #t (contains? html ", and a missing part.")))
    (test "without a database, int-links stay plain links" #t
          (contains? (sxml-output page-b 'html width: 200 options: '((standalone . #f)))
                     "href=\"A#nowhere\">a missing part</a>"))
    ;; The command line: one run over both pages writes a database and
    ;; resolves the links between them.
    (let ((a (make-pathname tmp "A.sxml")) (b (make-pathname tmp "B.sxml")) (out (make-pathname tmp "all.xref")))
      (with-output-to-file a (lambda () (write page-a)))
      (with-output-to-file b (lambda () (write page-b)))
      (test "the command line converts both pages" 0
            (quietly (lambda () (multidoc-main (list "-t" "markdown" "-w" "200" "--auto-ext" (string-append "--xref-out=" out) a b)))))
      (test "links between the pages resolve in one run" #t
            (contains? (with-input-from-file (make-pathname tmp "B.md") (lambda () (read-string #f)))
                       "[Intro](A#intro)"))
      (test "the database of the run is written" #t
            (and (refdb-lookup-target (load-xref-db out) "A" "intro") #t))
      (test "a later run reads it with --xref-in" #t
            (begin
              (quietly (lambda () (multidoc-main (list "-t" "plain" "-w" "200" (string-append "--xref-in=" out) "-o"
                                                       (make-pathname tmp "B.txt") b))))
              (contains? (with-input-from-file (make-pathname tmp "B.txt") (lambda () (read-string #f)))
                         "Back to Intro, the page Page A, its table Numbers"))))))

(test-end "multidoc")

(test-exit)
