;;
;; multidoc.ir - the intermediate representation of multidoc documents
;;
;; A document is read from SXML source format into the algebraic types
;; defined here, transformed by IR-to-IR filters, resolved once by
;; `resolve-doc', and then handed to a writer.
;;
;; The core is a pair of datatypes, `inline' and `block', in
;; the style of pandoc.  Three features make it extensible:
;;
;;  - Sections carry a named division kind (part, chapter, section,
;;    ...) rather than a bare number, so that a document can be
;;    placed at any depth of a larger hierarchy.
;;
;;  - `CustomBlock' and `CustomInline' carry user-defined node kinds.
;;    Each kind is declared with `define-custom-node' and must supply
;;    a lowering into core IR, so that every writer can render it.
;;
;;  - Attributes carry a style: a name plus properties addressed to
;;    particular writers.  A writer reads the properties under its own
;;    name and ignores the rest.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.ir

  (
   ;; predicates used in field declarations
   list-of any? inlines? blocks?

   ;; styles and attributes
   make-style style? style-name style-props
   make-attr attr? attr-id attr-classes attr-kvs attr-style
   null-attr attr-with-id attr-add-class attr-kv attr-has-class?
   style-property

   ;; citations
   make-citation citation? citation-key citation-prefix
   citation-suffix citation-locator citation-mode

   ;; inline elements
   inline inline?
   Str Space SoftBreak LineBreak NBSpace
   Emph Strong Small Code Link Image Span RawInline
   Cite Ref CustomInline

   ;; tables
   make-colspec colspec? colspec-align colspec-width
   make-table-part table-part? table-part-attr table-part-rows
   make-table-row table-row? table-row-attr table-row-cells
   make-table-cell table-cell? table-cell-attr table-cell-align
   table-cell-rowspan table-cell-colspan table-cell-blocks
   simple-cell empty-table-part
   grid-slot grid-slot? Origin Covered Missing
   make-grid grid? grid-nrows grid-ncols grid-ref grid-free?
   grid-next-free grid-place! grid-rows grid-fold-origins
   table-grid table-ncols

   ;; table access
   make-cell-ref cell-ref? cell-ref-part cell-ref-part-index
   cell-ref-row cell-ref-col cell-ref-cell
   table-parts table-width table-fold-cells table-cells
   cell-spans? cell-colored?

   ;; block elements
   block block?
   Plain Para CodeBlock RawBlock BlockQuote
   BulletList OrderedList DefinitionList
   Section Definition Table
   TableOfContents Bibliography HorizontalRule
   Div Null CustomBlock

   ;; documents
   make-doc doc? doc-meta doc-blocks doc-meta-ref

   ;; division kinds
   standard-divisions division-scheme
   define-division-scheme define-division-alias!
   division-rank division-at-rank standard-division
   level->division shift-divisions

   ;; custom nodes
   define-custom-node custom-node-declared? custom-node-block?
   custom-node-lower custom-node-collect
   lower-custom-block lower-custom-inline

   ;; traversals
   map-block-children map-inline-children
   walk-blocks walk-inlines query-blocks query-inlines
   flatten-sections

   ;; text utilities
   inlines->string title->anchor-id string->inlines

   ;; the collect/resolve pass
   make-xref-target xref-target? xref-target-doc xref-target-key
   xref-target-kind xref-target-number xref-target-title
   xref-target-anchor
   make-toc-entry toc-entry? toc-entry-division toc-entry-id
   toc-entry-number toc-entry-numbered? toc-entry-title
   toc-entry-children
   resolved? resolved-toc resolved-features resolved-cite-keys
   resolved-extras resolved-missing-refs
   resolved-lookup resolved-has-feature? resolved-targets
   resolve-doc
   )

  (import scheme (only (scheme base) make-parameter)
          (chicken base) (chicken format) (chicken string) (chicken irregex)
          (srfi 1) (srfi 13) (srfi 69) datatype matchable)

  (include "box.scm")
  (include "stack.scm")


;; ---------------------------------------------------------------------
;; Field predicates
;; ---------------------------------------------------------------------

;; [list-of pred] returns a predicate recognizing proper lists whose
;; elements all satisfy pred.
(define (list-of pred)
  (lambda (x) (and (list? x) (every pred x))))

(define (any? x) #t)

(define (inlines? x) ((list-of inline?) x))
(define (blocks? x)  ((list-of block?) x))


;; ---------------------------------------------------------------------
;; Styles and attributes
;; ---------------------------------------------------------------------

;; A style names a presentation, and carries properties addressed to
;; particular writers.  `props' is an alist from a writer name (or
;; the shared key `table' for the table style vocabulary) to an alist
;; of (key value) entries, for example
;;
;;   ((html (class "note")) (latex (env "lyxcode")))

(define-record-type style
  (make-style name props)
  style?
  (name  style-name)
  (props style-props))

;; An attribute is an optional identifier, a list of class names
;; (strings), a list of (key . value) string pairs, and an optional
;; style.

(define-record-type attr
  (make-attr id classes kvs style)
  attr?
  (id      attr-id)
  (classes attr-classes)
  (kvs     attr-kvs)
  (style   attr-style))

(define null-attr (make-attr #f '() '() #f))

(define (attr-with-id a id)
  (make-attr id (attr-classes a) (attr-kvs a) (attr-style a)))

(define (attr-add-class a class)
  (if (member class (attr-classes a))
      a
      (make-attr (attr-id a) (append (attr-classes a) (list class))
                 (attr-kvs a) (attr-style a))))

(define (attr-has-class? a class)
  (and (member class (attr-classes a)) #t))

(define (attr-kv a key #!optional default)
  (cond ((assoc key (attr-kvs a)) => cdr)
        (else default)))

;; [style-property a section key default] looks up `key' among the
;; style properties addressed to `section' (a writer name, or `table').
(define (style-property a section key #!optional default)
  (let ((s (and a (attr-style a))))
    (cond ((and s (assq section (style-props s)))
           => (lambda (entry)
                (cond ((assq key (cdr entry)) => cadr)
                      (else default))))
          (else default))))


;; ---------------------------------------------------------------------
;; Citations
;; ---------------------------------------------------------------------

;; A single citation within a `Cite' inline.  `prefix' and `suffix'
;; are inline lists; `locator' is a string such as "p. 3" or #f;
;; `mode' is one of normal, author-in-text or suppress-author.

(define-record-type citation
  (make-citation key prefix suffix locator mode)
  citation?
  (key     citation-key)
  (prefix  citation-prefix)
  (suffix  citation-suffix)
  (locator citation-locator)
  (mode    citation-mode))


;; ---------------------------------------------------------------------
;; Inline elements
;; ---------------------------------------------------------------------

(define-datatype inline inline?
  (Str       (s string?))
  (Space)
  (SoftBreak)
  (LineBreak)
  (NBSpace)
  (Emph      (xs inlines?))
  (Strong    (xs inlines?))
  (Small     (xs inlines?))
  (Code      (a attr?) (s string?))
  ;; kind is external or internal
  (Link      (a attr?) (xs inlines?) (target string?) (kind symbol?))
  (Image     (a attr?) (alt inlines?) (src string?))
  (Span      (a attr?) (xs inlines?))
  (RawInline (format symbol?) (s string?))
  ;; xs is the fallback text, replaced by a citation processor
  (Cite      (citations (list-of citation?)) (xs inlines?))
  ;; xs is optional explicit link text
  (Ref       (kind symbol?) (key string?) (xs inlines?))
  (CustomInline (tag symbol?) (a attr?) (payload any?)))


;; ---------------------------------------------------------------------
;; Tables
;; ---------------------------------------------------------------------

;; The table model follows pandoc 2.10: a header part, one or more
;; body parts and a footer part, each a list of rows of cells.  Cells
;; hold blocks and may span several rows or columns.

;; align is one of left, center, right or default; width is #f or a
;; fraction of the text width.
(define-record-type colspec
  (make-colspec align width)
  colspec?
  (align colspec-align)
  (width colspec-width))

(define-record-type table-part
  (make-table-part attr rows)
  table-part?
  (attr table-part-attr)
  (rows table-part-rows))

(define-record-type table-row
  (make-table-row attr cells)
  table-row?
  (attr  table-row-attr)
  (cells table-row-cells))

(define-record-type table-cell
  (make-table-cell attr align rowspan colspan blocks)
  table-cell?
  (attr    table-cell-attr)
  (align   table-cell-align)
  (rowspan table-cell-rowspan)
  (colspan table-cell-colspan)
  (blocks  table-cell-blocks))

(define (simple-cell blocks)
  (make-table-cell null-attr 'default 1 1 blocks))

(define empty-table-part (make-table-part null-attr '()))

;; The grid of a table part
;;
;; Writers that cannot express spans work on a rectangular grid, as
;; pandoc's grid-table writers do.  A grid has one slot per row and
;; column.  A cell occupies an `Origin' slot at its top-left position
;; and `Covered' slots over the rest of its span; each `Covered' slot
;; records the coordinates of its origin.  Slots that no cell reaches
;; (in rows shorter than the table) are `Missing'.
;;
;; A grid is built by one rule, applied to the cells of each row in
;; order: find the next free slot in the row, and place the cell
;; there.  Slots may already be taken by row spans from the rows
;; above.

(define-datatype grid-slot grid-slot?
  (Origin  (cell table-cell?))
  (Covered (cell table-cell?) (row fixnum?) (col fixnum?))
  (Missing))

(define-record-type grid
  (%make-grid nrows ncols slots)
  grid?
  (nrows grid-nrows)
  (ncols grid-ncols)
  (slots grid-slots))                   ; vector of row vectors; #f = free

(define (make-grid nrows ncols)
  (%make-grid nrows ncols
              (list->vector (list-tabulate nrows (lambda (r) (make-vector ncols #f))))))

;; [grid-ref g row col] is the slot at (row, col); free slots read as
;; `Missing'.
(define (grid-ref g row col)
  (or (vector-ref (vector-ref (grid-slots g) row) col) (Missing)))

(define (grid-free? g row col)
  (not (vector-ref (vector-ref (grid-slots g) row) col)))

;; [grid-next-free g row col] is the first free column at or after
;; col in the given row, or #f if the row is full.
(define (grid-next-free g row col)
  (let loop ((col col))
    (cond ((>= col (grid-ncols g)) #f)
          ((grid-free? g row col) col)
          (else (loop (+ col 1))))))

;; [grid-place! g row col cell] puts cell at (row, col) and marks the
;; rest of its span as covered.  The span is clipped to the grid.
(define (grid-place! g row col cell)
  (do ((r row (+ r 1)))
      ((or (= r (+ row (table-cell-rowspan cell))) (>= r (grid-nrows g))))
    (do ((c col (+ c 1)))
        ((or (= c (+ col (table-cell-colspan cell))) (>= c (grid-ncols g))))
      (vector-set! (vector-ref (grid-slots g) r) c
                   (if (and (= r row) (= c col))
                       (Origin cell)
                       (Covered cell row col))))))

;; [grid-rows g] is the grid as a list of rows, each a list of slots.
(define (grid-rows g)
  (list-tabulate (grid-nrows g)
                 (lambda (r)
                   (list-tabulate (grid-ncols g)
                                  (lambda (c) (grid-ref g r c))))))

;; [grid-fold-origins f seed g] folds f over the cells of the grid in
;; row-major order, calling (f row col cell acc) at each origin.
(define (grid-fold-origins f seed g)
  (fold (lambda (r acc)
          (fold (lambda (c acc)
                  (cases grid-slot (grid-ref g r c)
                    (Origin (cell) (f r c cell acc))
                    (else acc)))
                acc (iota (grid-ncols g))))
        seed (iota (grid-nrows g))))

;; [table-ncols colspecs parts] is the number of columns: the number
;; of column specs if given, otherwise the widest row, counting spans.
(define (table-ncols colspecs parts)
  (define (row-width row)
    (fold + 0 (map table-cell-colspan (table-row-cells row))))
  (if (pair? colspecs)
      (length colspecs)
      (fold max 0 (map row-width (append-map table-part-rows parts)))))

;; [table-grid part ncols] is the grid of a table part.  Row spans do
;; not extend beyond the part, and cells that do not fit in a full
;; row are dropped.
(define (table-grid part ncols)
  (let* ((rows (table-part-rows part))
         (g    (make-grid (length rows) ncols)))
    (define (place-row! row r)
      (fold (lambda (cell col)
              (let ((col (and col (grid-next-free g r col))))
                (and col
                     (begin (grid-place! g r col cell)
                            (+ col (table-cell-colspan cell))))))
            0 (table-row-cells row)))
    (for-each place-row! rows (iota (length rows)))
    g))

;; ---------------------------------------------------------------------
;; Block elements
;; ---------------------------------------------------------------------

;; Signatures of a `Definition': a list of (kind . inlines), where
;; kind is a symbol such as procedure, macro or constant.
(define (sigs? x)
  ((list-of (lambda (s) (and (pair? s) (symbol? (car s)) (inlines? (cdr s))))) x))

;; Items of a `DefinitionList': a list of (term-inlines . definitions),
;; where definitions is a list of block lists.
(define (def-items? x)
  ((list-of (lambda (d) (and (pair? d) (inlines? (car d))
                             ((list-of blocks?) (cdr d)))))
   x))

(define-datatype block block?
  (Plain          (xs inlines?))
  (Para           (xs inlines?))
  (CodeBlock      (a attr?) (s string?))
  (RawBlock       (format symbol?) (s string?))
  (BlockQuote     (bs blocks?))
  (BulletList     (items (list-of blocks?)))
  (OrderedList    (items (list-of blocks?)))
  (DefinitionList (items def-items?))
  (Section        (division symbol?) (a attr?) (numbered boolean?)
                  (title inlines?) (body blocks?))
  (Definition     (a attr?) (sigs sigs?) (body blocks?))
  (Table          (a attr?) (caption inlines?)
                  (colspecs (list-of colspec?))
                  (head table-part?) (bodies (list-of table-part?))
                  (foot table-part?))
  (TableOfContents)
  (Bibliography   (a attr?))
  (HorizontalRule)
  (Div            (a attr?) (bs blocks?))
  (Null)
  (CustomBlock    (tag symbol?) (a attr?) (payload any?)))


;; ---------------------------------------------------------------------
;; Table access
;; ---------------------------------------------------------------------

;; Operations on tables go through a cell cursor, after the selectors
;; of R's flextable: every cell is addressed by the part it belongs to
;; (head, body or foot), the index of that part (bodies are counted
;; from 0; head and foot are 0), and its row and grid column within
;; the part.  The column is the grid column of the cell's origin, so
;; it accounts for spans in the rows above and to the left.

(define-record-type cell-ref
  (make-cell-ref part part-index row col cell)
  cell-ref?
  (part       cell-ref-part)
  (part-index cell-ref-part-index)
  (row        cell-ref-row)
  (col        cell-ref-col)
  (cell       cell-ref-cell))

;; [table-parts tbl] is the list of parts of a Table block, each as
;; (name index part), in order: head, bodies, foot.
(define (table-parts tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (append (list (list 'head 0 head))
              (map (lambda (part k) (list 'body k part)) bodies (iota (length bodies)))
              (list (list 'foot 0 foot))))
    (else (error 'table-parts "not a table" tbl))))

;; [table-width tbl] is the number of columns of a Table block.
(define (table-width tbl)
  (cases block tbl
    (Table (a caption colspecs head bodies foot)
      (table-ncols colspecs (map caddr (table-parts tbl))))
    (else (error 'table-width "not a table" tbl))))

;; [table-fold-cells f seed tbl] folds f over the cells of a Table
;; block, calling (f cell-ref acc) for each cell in document order.
(define (table-fold-cells f seed tbl)
  (let ((ncols (table-width tbl)))
    (fold (lambda (named acc)
            (match named
              ((name index part)
               (grid-fold-origins
                (lambda (row col cell acc)
                  (f (make-cell-ref name index row col cell) acc))
                acc (table-grid part ncols)))))
          seed (table-parts tbl))))

;; A selector matches a value if it is #f (anything), equal to the
;; value, a list containing it, or a predicate true of it.
(define (selects? sel x)
  (cond ((not sel) #t)
        ((procedure? sel) (sel x))
        ((list? sel) (and (member x sel) #t))
        (else (equal? sel x))))

;; [table-cells tbl #!key part row col] is the list of cell-refs of a
;; Table block selected by part name, row and column.  For example
;; (table-cells t part: 'head) or (table-cells t col: 0).
(define (table-cells tbl #!key (part #f) (row #f) (col #f))
  (reverse
   (table-fold-cells
    (lambda (ref acc)
      (if (and (selects? part (cell-ref-part ref))
               (selects? row (cell-ref-row ref))
               (selects? col (cell-ref-col ref)))
          (cons ref acc)
          acc))
    '() tbl)))

;; [cell-spans? ref] is true if the cell spans several rows or columns.
(define (cell-spans? ref)
  (let ((cell (cell-ref-cell ref)))
    (or (> (table-cell-rowspan cell) 1) (> (table-cell-colspan cell) 1))))

;; [cell-colored? ref] is true if the cell's style sets a foreground
;; or background colour.
(define (cell-colored? ref)
  (let ((a (table-cell-attr (cell-ref-cell ref))))
    (and (or (style-property a 'table 'bg) (style-property a 'table 'fg)) #t)))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

;; `meta' is an alist from symbols to strings, taken from the SXML
;; (Header ...) element: title, Author, description and so on.

(define-record-type doc
  (make-doc meta blocks)
  doc?
  (meta   doc-meta)
  (blocks doc-blocks))

(define (doc-meta-ref d key #!optional default)
  (cond ((assq key (doc-meta d)) => cdr)
        (else default)))


;; ---------------------------------------------------------------------
;; Division kinds
;; ---------------------------------------------------------------------

;; The standard division kinds, from the outermost to the innermost.
;; Writers know how to render these.
(define standard-divisions
  '(part chapter section subsection subsubsection paragraph subparagraph))

;; The current division scheme: an ordered list of kinds.  A custom
;; kind in the scheme is rendered as the standard kind of the same
;; rank, unless an alias says otherwise.
(define division-scheme (make-parameter standard-divisions))

(define division-aliases (make-hash-table eq?))

;; [define-division-scheme kinds] makes `kinds' the current scheme.
(define (define-division-scheme kinds)
  (unless (and (list? kinds) (every symbol? kinds) (pair? kinds))
    (error 'define-division-scheme "expected a non-empty list of symbols" kinds))
  (division-scheme kinds))

;; [define-division-alias! kind standard] renders `kind' as the
;; standard kind `standard', whatever its rank.
(define (define-division-alias! kind standard)
  (unless (memq standard standard-divisions)
    (error 'define-division-alias! "not a standard division kind" standard))
  (hash-table-set! division-aliases kind standard))

;; [division-rank kind] is the 0-based position of `kind' in the
;; current scheme.  Unknown kinds rank below all known ones.
(define (division-rank kind)
  (or (list-index (lambda (k) (eq? k kind)) (division-scheme))
      (length (division-scheme))))

(define (division-at-rank rank)
  (let ((scheme (division-scheme)))
    (list-ref scheme (max 0 (min rank (- (length scheme) 1))))))

;; [standard-division kind] is the standard kind a writer should use
;; to render `kind'.
(define (standard-division kind)
  (cond ((memq kind standard-divisions) kind)
        ((hash-table-ref/default division-aliases kind #f))
        (else
         (list-ref standard-divisions
                   (min (division-rank kind)
                        (- (length standard-divisions) 1))))))

;; [level->division level top-division] maps a wiki heading level
;; (where level 2 is the top of a page) to a division kind, given the
;; kind that level 2 should become.
(define (level->division level #!optional (top-division 'section))
  (division-at-rank (+ (division-rank top-division) (- level 2))))

;; [shift-divisions n] returns a filter that moves every section of a
;; document n ranks down the hierarchy (up, if n is negative).
(define (shift-divisions n)
  (lambda (d)
    (make-doc (doc-meta d)
              (walk-blocks
               (lambda (b)
                 (cases block b
                   (Section (division a numbered title body)
                     (Section (division-at-rank (+ (division-rank division) n))
                              a numbered title body))
                   (else b)))
               (doc-blocks d)))))


;; ---------------------------------------------------------------------
;; Custom nodes
;; ---------------------------------------------------------------------

(define-record-type custom-spec
  (make-custom-spec tag block? lower collect)
  custom-spec?
  (tag     custom-spec-tag)
  (block?  custom-spec-block?)
  (lower   custom-spec-lower)
  (collect custom-spec-collect))

(define custom-nodes (make-hash-table eq?))

;; [define-custom-node tag lower: proc block?: bool collect: proc]
;; declares a custom node kind.
;;
;; `lower' takes an attribute and a payload and returns core IR: a
;; list of blocks for a block node, a list of inlines for an inline
;; node.  It is required, so that every writer can render the node.
;;
;; `collect', if given, is called by `resolve-doc' with the attribute,
;; the payload and a procedure (add! key value) that records an entry
;; in the extras of the resolution table.
(define (define-custom-node tag #!key lower (block? #t) (collect #f))
  (unless (procedure? lower)
    (error 'define-custom-node "a lowering procedure is required" tag))
  (hash-table-set! custom-nodes tag (make-custom-spec tag block? lower collect)))

(define (custom-node-declared? tag)
  (hash-table-exists? custom-nodes tag))

(define (custom-spec-of tag)
  (hash-table-ref/default custom-nodes tag #f))

(define (custom-node-block? tag)
  (let ((s (custom-spec-of tag))) (and s (custom-spec-block? s))))

(define (custom-node-lower tag)
  (let ((s (custom-spec-of tag))) (and s (custom-spec-lower s))))

(define (custom-node-collect tag)
  (let ((s (custom-spec-of tag))) (and s (custom-spec-collect s))))

(define (lower-custom tag a payload what)
  (let ((lower (custom-node-lower tag)))
    (cond (lower (lower a payload))
          (else
           (warning (sprintf "multidoc: undeclared custom ~a node, skipped" what) tag)
           '()))))

(define (lower-custom-block tag a payload)
  (lower-custom tag a payload "block"))

(define (lower-custom-inline tag a payload)
  (lower-custom tag a payload "inline"))


;; ---------------------------------------------------------------------
;; Traversals
;; ---------------------------------------------------------------------

;; [map-cell-blocks f cell] and friends rebuild table structure with
;; the blocks of each cell passed through f.
(define (map-cell-blocks f cell)
  (make-table-cell (table-cell-attr cell) (table-cell-align cell)
                   (table-cell-rowspan cell) (table-cell-colspan cell)
                   (f (table-cell-blocks cell))))

(define (map-part-blocks f part)
  (make-table-part
   (table-part-attr part)
   (map (lambda (row)
          (make-table-row (table-row-attr row)
                          (map (lambda (c) (map-cell-blocks f c))
                               (table-row-cells row))))
        (table-part-rows part))))

(define (part-blocks part)
  (append-map (lambda (row) (append-map table-cell-blocks (table-row-cells row)))
              (table-part-rows part)))

;; [map-block-children fb fi b] rebuilds block b with its child block
;; lists passed through fb and its child inline lists through fi.
;; The payload of a custom block is opaque and left alone.
(define (map-block-children fb fi b)
  (cases block b
    (Plain (xs) (Plain (fi xs)))
    (Para (xs) (Para (fi xs)))
    (BlockQuote (bs) (BlockQuote (fb bs)))
    (BulletList (items) (BulletList (map fb items)))
    (OrderedList (items) (OrderedList (map fb items)))
    (DefinitionList (items)
      (DefinitionList (map (lambda (d) (cons (fi (car d)) (map fb (cdr d)))) items)))
    (Section (division a numbered title body)
      (Section division a numbered (fi title) (fb body)))
    (Definition (a sigs body)
      (Definition a (map (lambda (s) (cons (car s) (fi (cdr s)))) sigs) (fb body)))
    (Table (a caption colspecs head bodies foot)
      (Table a (fi caption) colspecs
             (map-part-blocks fb head)
             (map (lambda (p) (map-part-blocks fb p)) bodies)
             (map-part-blocks fb foot)))
    (Div (a bs) (Div a (fb bs)))
    (else b)))

;; [map-inline-children fi x] rebuilds inline x with its child inline
;; lists passed through fi.
(define (map-inline-children fi x)
  (cases inline x
    (Emph (xs) (Emph (fi xs)))
    (Strong (xs) (Strong (fi xs)))
    (Small (xs) (Small (fi xs)))
    (Link (a xs target kind) (Link a (fi xs) target kind))
    (Image (a alt src) (Image a (fi alt) src))
    (Span (a xs) (Span a (fi xs)))
    (Cite (cs xs)
      (Cite (map (lambda (c)
                   (make-citation (citation-key c) (fi (citation-prefix c))
                                  (fi (citation-suffix c)) (citation-locator c)
                                  (citation-mode c)))
                 cs)
            (fi xs)))
    (Ref (kind key xs) (Ref kind key (fi xs)))
    (else x)))

;; The result of a filter function is either one element or a list
;; of elements to splice in its place.
(define (splice result)
  (if (list? result) result (list result)))

;; [walk-blocks f blocks] applies f bottom-up to every block, like a
;; pandoc filter.  f returns a block or a list of blocks.
(define (walk-blocks f blocks)
  (define (walk-list bs) (append-map walk-one bs))
  (define (walk-one b) (splice (f (map-block-children walk-list identity b))))
  (walk-list blocks))

;; [walk-inlines f blocks] applies f bottom-up to every inline in
;; blocks.  f returns an inline or a list of inlines.
(define (walk-inlines f blocks)
  (define (walk-xs xs)
    (append-map (lambda (x) (splice (f (map-inline-children walk-xs x)))) xs))
  (define (walk-bs bs)
    (map (lambda (b) (map-block-children walk-bs walk-xs b)) bs))
  (walk-bs blocks))

;; [query-blocks f seed blocks] folds f over every block, parents
;; before children, in document order.
(define (query-blocks f seed blocks)
  (fold (lambda (b acc)
          (let ((acc (f b acc)))
            (fold (lambda (bs acc) (query-blocks f acc bs))
                  acc
                  (child-block-lists b))))
        seed blocks))

;; [query-inlines f seed blocks] folds f over every inline, parents
;; before children, in document order.
(define (query-inlines f seed blocks)
  (define (q-xs xs acc)
    (fold (lambda (x acc)
            (fold (lambda (xs acc) (q-xs xs acc)) (f x acc) (child-inline-lists x)))
          acc xs))
  (query-blocks
   (lambda (b acc) (fold q-xs acc (own-inline-lists b)))
   seed blocks))

(define (child-block-lists b)
  (cases block b
    (BlockQuote (bs) (list bs))
    (BulletList (items) items)
    (OrderedList (items) items)
    (DefinitionList (items) (append-map cdr items))
    (Section (division a numbered title body) (list body))
    (Definition (a sigs body) (list body))
    (Table (a caption colspecs head bodies foot)
      (map part-blocks (cons head (append bodies (list foot)))))
    (Div (a bs) (list bs))
    (else '())))

(define (own-inline-lists b)
  (cases block b
    (Plain (xs) (list xs))
    (Para (xs) (list xs))
    (DefinitionList (items) (map car items))
    (Section (division a numbered title body) (list title))
    (Definition (a sigs body) (map cdr sigs))
    (Table (a caption colspecs head bodies foot) (list caption))
    (else '())))

(define (child-inline-lists x)
  (cases inline x
    (Emph (xs) (list xs))
    (Strong (xs) (list xs))
    (Small (xs) (list xs))
    (Link (a xs target kind) (list xs))
    (Image (a alt src) (list alt))
    (Span (a xs) (list xs))
    (Cite (cs xs) (append (append-map (lambda (c) (list (citation-prefix c)
                                                        (citation-suffix c)))
                                      cs)
                          (list xs)))
    (Ref (kind key xs) (list xs))
    (else '())))

;; [flatten-sections blocks] replaces each section by a headless
;; section (with an empty body) followed by its body, recursively.
;; It gives writers the flat, pandoc-style view of the hierarchy.
(define (flatten-sections blocks)
  (append-map
   (lambda (b)
     (cases block b
       (Section (division a numbered title body)
         (cons (Section division a numbered title '())
               (flatten-sections body)))
       (else (list b))))
   blocks))


;; ---------------------------------------------------------------------
;; Text utilities
;; ---------------------------------------------------------------------

;; [inlines->string xs] is the plain text of an inline list, with
;; markup dropped.
(define (inlines->string xs)
  (define (one x)
    (cases inline x
      (Str (s) s)
      (Space () " ")
      (SoftBreak () " ")
      (LineBreak () " ")
      (NBSpace () " ")
      (Code (a s) s)
      (Cite (cs xs)
        (if (null? xs)
            (string-intersperse (map citation-key cs) "; ")
            (inlines->string xs)))
      (Ref (kind key xs) (if (null? xs) key (inlines->string xs)))
      (RawInline (format s) "")
      (CustomInline (tag a payload) "")
      (else (inlines->string (car (child-inline-lists x))))))
  (apply string-append (map one xs)))

;; [string->inlines s] splits s on runs of whitespace into `Str' and
;; `Space' elements.
(define (string->inlines s)
  (let ((words (string-tokenize s)))
    (if (null? words)
        (if (string-null? s) '() (list (Space)))
        (let* ((lead  (if (char-whitespace? (string-ref s 0)) (list (Space)) '()))
               (trail (if (char-whitespace? (string-ref s (- (string-length s) 1)))
                          (list (Space)) '())))
          (append lead
                  (cdr (append-map (lambda (w) (list (Space) (Str w))) words))
                  trail)))))

;; [title->anchor-id title] turns a section title (a string or a list
;; of inlines) into a lowercase, hyphen-separated identifier that is
;; safe to use as an anchor: "First section" becomes "first-section".
;; Leading characters other than letters are dropped, as are
;; characters other than letters, digits, underscore and hyphen.
(define (title->anchor-id title)
  (let* ((s (if (string? title) title (inlines->string title)))
         (s (string-downcase s))
         (s (irregex-replace "^[^a-z]+" s ""))
         (s (irregex-replace/all "[^a-z0-9_ \t-]" s ""))
         (s (irregex-replace/all "[ \t]+" s "-")))
    (if (string-null? s) "section" s)))


;; ---------------------------------------------------------------------
;; The collect/resolve pass
;; ---------------------------------------------------------------------

;; A reference target: an element of a document that can be referred
;; to.  `doc' is the name of the containing document (#f for the
;; current one); `kind' is section, table, definition, div or a
;; custom tag; `number' is a list of integers or #f; `title' is an
;; inline list; `anchor' is the identifier used in the output.

(define-record-type xref-target
  (make-xref-target doc key kind number title anchor)
  xref-target?
  (doc    xref-target-doc)
  (key    xref-target-key)
  (kind   xref-target-kind)
  (number xref-target-number)
  (title  xref-target-title)
  (anchor xref-target-anchor))

;; An entry of the section tree, used for tables of contents and for
;; Texinfo menus.
(define-record-type toc-entry
  (make-toc-entry division id number numbered? title children)
  toc-entry?
  (division  toc-entry-division)
  (id        toc-entry-id)
  (number    toc-entry-number)
  (numbered? toc-entry-numbered?)
  (title     toc-entry-title)
  (children  toc-entry-children))

;; The resolution table computed by `resolve-doc'.
;;
;;  targets:      hash table from key strings to xref-target records
;;  toc:          list of top-level toc-entry records
;;  features:     list of feature symbols, and (custom . tag) pairs
;;  cite-keys:    citation keys in order of first citation
;;  extras:       alist of entries added by custom node collect hooks
;;  missing-refs: keys of `Ref' elements with no target
;;  external:     procedure (key) -> xref-target or #f, for targets in
;;                other documents

(define-record-type resolved
  (make-resolved targets toc features cite-keys extras missing-refs external)
  resolved?
  (targets      resolved-targets)
  (toc          resolved-toc)
  (features     resolved-features)
  (cite-keys    resolved-cite-keys)
  (extras       resolved-extras)
  (missing-refs resolved-missing-refs)
  (external     resolved-external))

;; [resolved-lookup r key] finds the target of a reference key, first
;; in the document itself, then through the external lookup.
(define (resolved-lookup r key)
  (or (hash-table-ref/default (resolved-targets r) key #f)
      (let ((ext (resolved-external r)))
        (and ext (ext key)))))

(define (resolved-has-feature? r feature)
  (and (member feature (resolved-features r)) #t))

;; [unique-id! seen id] returns id, or id with a numeric suffix if id
;; has been seen before, and records the result.
(define (unique-id! seen id)
  (let loop ((n 1))
    (let ((candidate (if (= n 1) id (string-append id "-" (number->string n)))))
      (if (hash-table-exists? seen candidate)
          (loop (+ n 1))
          (begin (hash-table-set! seen candidate #t) candidate)))))

;; [resolve-doc d #!key external quiet] makes one pass over document d
;; and returns two values: the document with a unique identifier on
;; every section, and its resolution table.  Unresolved references
;; are reported as warnings unless quiet is true.
;;
;; Sections without an identifier get one from their title; a section
;; with an identifier of its own can also be referred to by the
;; identifier its title would give, unless another element has it.
;; Numbers are assigned to numbered sections by rank, and to tables
;; that have an identifier, in document order.
(define (resolve-doc d #!key (external #f) (quiet #f))
  (let ((seen      (make-hash-table equal?))
        (targets   (make-hash-table equal?))
        (features    (make-stack))
        (cite-keys   (make-stack))
        (extras      (make-stack))
        (aliases     (make-stack))
        (table-count (box 0)))

    (define (feature! f)
      (unless (member f (stack->list features)) (stack-push! features f)))

    (define (extra! k v)
      (stack-push! extras (cons k v)))

    (define (target! key kind number title)
      (when key
        (hash-table-set! targets key
                         (make-xref-target #f key kind number title key))))

    ;; Assign an identifier to every section, and to every other
    ;; element that already has one, while numbering sections.
    ;; `counters' holds the section numbers of the enclosing sections.
    (define (number-blocks bs counters)
      (let loop ((bs bs) (n 0) (acc '()) (entries '()))
        (if (null? bs)
            (values (reverse acc) (reverse entries))
            (let ((b (car bs)))
              (cases block b
                (Section (division a numbered title body)
                  (let* ((n      (if numbered (+ n 1) n))
                         (number (and numbered (append counters (list n))))
                         (id     (unique-id! seen (or (attr-id a) (title->anchor-id title)))))
                    (receive (body children) (number-blocks body (or number counters))
                      (target! id 'section number title)
                      ;; A section with an identifier of its own can
                      ;; also be found by its title, as int-link does.
                      (let ((by-title (title->anchor-id title)))
                        (unless (or (equal? by-title id) (string-null? by-title))
                          (stack-push! aliases
                                       (make-xref-target #f by-title 'section number title id))))
                      (loop (cdr bs) n
                            (cons (Section division (attr-with-id a id) numbered title body) acc)
                            (cons (make-toc-entry division id number numbered title children)
                                  entries)))))
                (else
                 (let ((b (number-other b counters)))
                   (loop (cdr bs) n (cons b acc) entries))))))))

    ;; Elements other than sections: register identifiers, and
    ;; descend into child blocks, which may contain further sections.
    (define (number-other b counters)
      (define (descend bs)
        (receive (bs entries) (number-blocks bs counters) bs))
      (let ((b (map-block-children descend identity b)))
        (cases block b
          (Table (a caption colspecs head bodies foot)
            (if (attr-id a)
                (let ((id (unique-id! seen (attr-id a))))
                  (set-box! table-count (+ (unbox table-count) 1))
                  (target! id 'table (list (unbox table-count)) caption)
                  (Table (attr-with-id a id) caption colspecs head bodies foot))
                b))
          (Definition (a sigs body)
            (if (attr-id a)
                (let ((id (unique-id! seen (attr-id a))))
                  (target! id 'definition #f (if (pair? sigs) (cdar sigs) '()))
                  (Definition (attr-with-id a id) sigs body))
                b))
          (Div (a bs)
            (if (attr-id a)
                (let ((id (unique-id! seen (attr-id a))))
                  (target! id 'div #f '())
                  (Div (attr-with-id a id) bs))
                b))
          (CustomBlock (tag a payload)
            (if (attr-id a)
                (let ((id (unique-id! seen (attr-id a))))
                  (target! id tag #f '())
                  (CustomBlock tag (attr-with-id a id) payload))
                b))
          (else b))))

    ;; Record features and citation keys, and run collect hooks.
    (define (collect-features! blocks)
      (query-blocks
       (lambda (b acc)
         (cases block b
           (TableOfContents () (feature! 'toc))
           (Bibliography (a) (feature! 'bibliography))
           (Definition (a sigs body) (feature! 'definitions))
           (CodeBlock (a s) (feature! 'code))
           (Table (a caption colspecs head bodies foot)
             (feature! 'tables)
             (let ((cells (table-cells b)))
               (when (any cell-spans? cells) (feature! 'table-spans))
               (when (any cell-colored? cells) (feature! 'table-colors))))
           (CustomBlock (tag a payload)
             (feature! (cons 'custom tag))
             (let ((collect (custom-node-collect tag)))
               (when collect
                 (collect a payload extra!))))
           (else #f))
         acc)
       #f blocks)
      (query-inlines
       (lambda (x acc)
         (cases inline x
           (Link (a xs target kind)
             (feature! 'links)
             (when (eq? kind 'external) (feature! 'external-links)))
           (Image (a alt src) (feature! 'images))
           (Code (a s) (feature! 'code))
           (Cite (cs xs)
             (feature! 'citations)
             (for-each (lambda (c)
                         (unless (member (citation-key c) (stack->list cite-keys))
                           (stack-push! cite-keys (citation-key c))))
                       cs))
           (Ref (kind key xs) (feature! 'refs))
           (CustomInline (tag a payload)
             (feature! (cons 'custom tag))
             (let ((collect (custom-node-collect tag)))
               (when collect
                 (collect a payload extra!))))
           (else #f))
         acc)
       #f blocks))

    ;; Keys of references with no target.
    (define (missing-refs blocks r)
      (reverse
       (query-inlines
        (lambda (x acc)
          (cases inline x
            (Ref (kind key xs)
              (if (or (resolved-lookup r key) (member key acc)) acc (cons key acc)))
            (else acc)))
        '() blocks)))

    (receive (blocks toc) (number-blocks (doc-blocks d) '())
      ;; Title aliases are added last, so that they never hide the
      ;; identifier of another element.
      (for-each (lambda (t)
                  (unless (hash-table-exists? targets (xref-target-key t))
                    (hash-table-set! targets (xref-target-key t) t)))
                (reverse (stack->list aliases)))
      (collect-features! blocks)
      (let* ((features  (reverse (stack->list features)))
             (cite-keys (reverse (stack->list cite-keys)))
             (extras    (reverse (stack->list extras)))
             (r (make-resolved targets toc features cite-keys extras '() external))
             (missing (missing-refs blocks r)))
        (unless quiet
          (for-each (lambda (key)
                      (warning "multidoc: unresolved reference" key))
                    missing))
        (values (make-doc (doc-meta d) blocks)
                (make-resolved targets toc features cite-keys extras missing external))))))

)
