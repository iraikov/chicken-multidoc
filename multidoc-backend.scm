;;
;; multidoc.backend - the writer protocol, the writer registry, and
;; the rendering driver
;;
;; A writer turns a resolved document into a wl-pprint document,
;; which is laid out at a chosen width.  Writers are Yasos objects
;; implementing the operations below.  They are usually built with
;; `make-writer', and specialized with `derive-writer', whose
;; overrides receive the parent's method as a `next' argument.
;;
;; Writers recurse through `render-block' and `render-inline' rather
;; than calling their own methods, so that
;;
;;  - the outermost writer (the one rendering) receives every nested
;;    element, and overrides in a derived writer apply at any depth;
;;
;;  - custom nodes are dispatched in one place: to a handler
;;    registered for the writer with `define-node-writer', else to a
;;    handler registered for one of its ancestors, else to the node's
;;    lowering into core IR.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.backend

  (
   ;; the writer protocol
   writer? writer-name writer-extension writer-parent writer-output-kind
   write-doc write-block write-inline write-text write-output
   make-writer derive-writer default-write-output

   ;; render context
   make-ctx ctx? ctx-self ctx-options ctx-resolved ctx-doc
   ctx-depth ctx-verbatim? ctx-option ctx-option-symbol ctx-deeper ctx-in-verbatim ctx-with-option

   ;; registry
   register-writer! lookup-writer registered-writers

   ;; custom nodes
   define-node-writer find-node-writer writer-coverage

   ;; rendering helpers for writers
   render-block render-blocks render-inline render-inlines
   render-text render-string render-flat layout->string
   render-text-table
   vcat vsep verbatim-lines make-escaper
   ref-target ref-label-inlines xref-href cite-fallback-inlines

   ;; entry points
   make-render-context render convert
   )

  (import scheme (only (scheme base) make-parameter)
          (chicken base) (chicken format) (chicken string) (chicken port) (chicken sort)
          (srfi 1) (srfi 13) (srfi 69)
          datatype matchable yasos
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.sxml multidoc.refs)

  (include "box.scm")
  (include "stack.scm")


;; ---------------------------------------------------------------------
;; The writer protocol
;; ---------------------------------------------------------------------

(define-predicate writer?)

;; A symbol naming the writer in the registry, e.g. html.
(define-operation (writer-name w))

;; The usual file extension of the output, without the dot.
(define-operation (writer-extension w) "txt")

;; The writer this one was derived from, or #f.
(define-operation (writer-parent w) #f)

;; What write-output produces: text (a single stream), file or
;; directory.  Writers of kind file or directory need a path.
(define-operation (writer-output-kind w) 'text)

;; [write-doc w d ctx] renders a whole document as a wl-pprint
;; document.
(define-operation (write-doc w d ctx))

;; [write-block w b ctx] and [write-inline w x ctx] render one core
;; element.  Custom nodes never reach them; see `render-block'.
(define-operation (write-block w b ctx))
(define-operation (write-inline w x ctx))

;; [write-text w s ctx] escapes a string for the output format.
(define-operation (write-text w s ctx) s)

;; [write-output w d ctx dest] renders d and delivers the result to
;; dest: a port, a path, or #f for the current output port.
(define-operation (write-output w d ctx dest)
  (default-write-output w d ctx dest))

;; The default output: lay out the document at the width given in
;; the options, and end it with a newline.
(define (default-write-output w d ctx dest)
  (let ((layout (write-doc w d ctx))
        (width  (ctx-option ctx 'width 80)))
    (define (emit port)
      (pp:pretty 1.0 width port layout)
      (newline port))
    (cond ((output-port? dest) (emit dest))
          ((string? dest) (call-with-output-file dest emit))
          ((not dest) (emit (current-output-port)))
          (else (error 'write-output "invalid destination" dest)))))

;; [make-writer #!key name extension parent output-kind doc block
;; inline text output] builds a writer.  doc, block and inline are
;; required; each is a procedure taking the writer itself followed by
;; the arguments of the operation, e.g. (block self b ctx).
(define (make-writer #!key name (extension "txt") (parent #f) (output-kind 'text)
                     doc block inline
                     (text (lambda (self s ctx) s))
                     (output default-write-output))
  (unless (and (symbol? name) doc block inline)
    (error 'make-writer "a name and the doc, block and inline procedures are required" name))
  (operations ()
    ((writer? self) #t)
    ((writer-name self) name)
    ((writer-extension self) extension)
    ((writer-parent self) parent)
    ((writer-output-kind self) output-kind)
    ((write-doc self d ctx) (doc self d ctx))
    ((write-block self b ctx) (block self b ctx))
    ((write-inline self x ctx) (inline self x ctx))
    ((write-text self s ctx) (text self s ctx))
    ((write-output self d ctx dest) (output self d ctx dest))))

;; [parent-method base op op-name default] is the method of base for
;; op, or default if base does not define op.
(define (parent-method base op op-name default)
  (if (protocol base op-name) (operate-as base op) default))

;; [derive-writer base #!key name extension output-kind doc block
;; inline text output] builds a writer that behaves like base except
;; for the given overrides.  An override is called like the base
;; method, with one more argument: `next', a procedure that runs the
;; base method with the same writer, e.g.
;;
;;   (derive-writer html-writer
;;     block: (lambda (self b ctx next)
;;              (cases block b
;;                (Definition (a sigs body) ...)
;;                (else (next b ctx)))))
(define (derive-writer base #!key name extension output-kind doc block inline text output)
  (define (method op op-name default override)
    (let ((inherited (parent-method base op op-name default)))
      (if override
          (lambda (self . args)
            (apply override self
                   (append args (list (lambda args (apply inherited self args))))))
          inherited)))
  (make-writer
   name:        (or name (writer-name base))
   extension:   (or extension (writer-extension base))
   parent:      base
   output-kind: (or output-kind (writer-output-kind base))
   doc:    (method write-doc 'write-doc #f doc)
   block:  (method write-block 'write-block #f block)
   inline: (method write-inline 'write-inline #f inline)
   text:   (method write-text 'write-text (lambda (self s ctx) s) text)
   output: (method write-output 'write-output default-write-output output)))


;; ---------------------------------------------------------------------
;; The render context
;; ---------------------------------------------------------------------

;; self:      the outermost writer, which receives every element
;; options:   alist of rendering options, e.g. (width . 80)
;; resolved:  the resolution table from `resolve-doc'
;; doc:       the document being rendered
;; depth:     nesting depth of block containers
;; verbatim?: true inside code, where different escaping may apply

(define-record-type ctx
  (make-ctx self options resolved doc depth verbatim?)
  ctx?
  (self      ctx-self)
  (options   ctx-options)
  (resolved  ctx-resolved)
  (doc       ctx-doc)
  (depth     ctx-depth)
  (verbatim? ctx-verbatim?))

(define (ctx-option c key #!optional default)
  (cond ((assq key (ctx-options c)) => cdr)
        (else default)))

;; [ctx-option-symbol ctx key default] is an option that names a
;; choice, as a symbol, whether it was given as a symbol or a string
;; (as from the command line).
(define (ctx-option-symbol c key #!optional default)
  (let ((v (ctx-option c key default)))
    (if (string? v) (string->symbol v) v)))

(define (ctx-deeper c)
  (make-ctx (ctx-self c) (ctx-options c) (ctx-resolved c) (ctx-doc c)
            (+ 1 (ctx-depth c)) (ctx-verbatim? c)))

(define (ctx-with-option c key value)
  (make-ctx (ctx-self c) (cons (cons key value) (ctx-options c)) (ctx-resolved c)
            (ctx-doc c) (ctx-depth c) (ctx-verbatim? c)))

(define (ctx-in-verbatim c)
  (make-ctx (ctx-self c) (ctx-options c) (ctx-resolved c) (ctx-doc c)
            (ctx-depth c) #t))


;; ---------------------------------------------------------------------
;; The registry
;; ---------------------------------------------------------------------

(define writers (make-hash-table eq?))

;; [register-writer! w] makes w available under its name.
(define (register-writer! w)
  (unless (writer? w) (error 'register-writer! "not a writer" w))
  (hash-table-set! writers (writer-name w) w)
  w)

;; [lookup-writer name] is the writer registered under name (a symbol
;; or a string).
(define (lookup-writer name)
  (let ((key (if (string? name) (string->symbol name) name)))
    (or (hash-table-ref/default writers key #f)
        (error 'lookup-writer "no writer registered under this name" name))))

(define (registered-writers)
  (sort (hash-table-keys writers)
        (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))


;; ---------------------------------------------------------------------
;; Custom node writers
;; ---------------------------------------------------------------------

;; Handlers for custom nodes, keyed by (tag . writer-name).
(define node-writers (make-hash-table equal?))

;; [define-node-writer tag writer-name handler] registers handler,
;; called as (handler attr payload ctx) and returning a wl-pprint
;; document, for custom nodes with the given tag rendered by the
;; writer with the given name or by writers derived from it.
(define (define-node-writer tag writer-name handler)
  (hash-table-set! node-writers (cons tag writer-name) handler))

;; [find-node-writer w tag] is (values handler owner-name) for the
;; handler that w uses for tag, searching w and then its ancestors,
;; or (values #f #f).
(define (find-node-writer w tag)
  (let loop ((w w))
    (if (not w)
        (values #f #f)
        (let ((h (hash-table-ref/default node-writers (cons tag (writer-name w)) #f)))
          (if h (values h (writer-name w)) (loop (writer-parent w)))))))

;; [writer-coverage w d] reports, for each custom tag used in document
;; d, how writer w will render it: an alist from tags to one of
;;   native          - a handler registered for w
;;   (inherited . n) - a handler registered for ancestor n
;;   lowered         - the node's lowering into core IR
;;   undeclared      - nothing; the node is skipped with a warning
(define (writer-coverage w d)
  (let ((tags (make-stack)))
    (define (note! tag)
      (unless (memq tag (stack->list tags)) (stack-push! tags tag)))
    (query-blocks (lambda (b acc)
                    (cases block b
                      (CustomBlock (tag a payload) (note! tag))
                      (else #f))
                    acc)
                  #f (doc-blocks d))
    (query-inlines (lambda (x acc)
                     (cases inline x
                       (CustomInline (tag a payload) (note! tag))
                       (else #f))
                     acc)
                   #f (doc-blocks d))
    (map (lambda (tag)
           (receive (handler owner) (find-node-writer w tag)
             (cons tag
                   (cond ((and handler (eq? owner (writer-name w))) 'native)
                         (handler (cons 'inherited owner))
                         ((custom-node-declared? tag) 'lowered)
                         (else 'undeclared)))))
         (reverse (stack->list tags)))))


;; ---------------------------------------------------------------------
;; Rendering helpers for writers
;; ---------------------------------------------------------------------

;; [render-block ctx b] renders one block with the writer of ctx.
(define (render-block c b)
  (cases block b
    (CustomBlock (tag a payload)
      (receive (handler owner) (find-node-writer (ctx-self c) tag)
        (if handler
            (handler a payload c)
            (vcat (render-blocks c (lower-custom-block tag a payload))))))
    (else (write-block (ctx-self c) b c))))

;; [render-blocks ctx bs] renders a list of blocks as a list of
;; documents, leaving out those that render as nothing.
(define (render-blocks c bs)
  (remove (lambda (d) (eq? d pp:empty))
          (map (lambda (b) (render-block c b)) bs)))

(define (render-inline c x)
  (cases inline x
    (CustomInline (tag a payload)
      (receive (handler owner) (find-node-writer (ctx-self c) tag)
        (if handler
            (handler a payload c)
            (render-inlines c (lower-custom-inline tag a payload)))))
    (else (write-inline (ctx-self c) x c))))

(define (space-inline? x)
  (cases inline x
    (Space () #t)
    (SoftBreak () #t)
    (else #f)))

;; [render-inlines ctx xs] renders an inline list as filled text: each
;; space becomes a line break only if the following element does not
;; fit on the current line.
(define (render-inlines c xs)
  (let loop ((xs xs) (acc pp:empty) (space #f))
    (cond ((null? xs)
           (if space (pp:^^ acc (pp:group pp:break1)) acc))
          ((space-inline? (car xs))
           (loop (cdr xs) acc #t))
          (else
           (let ((d (render-inline c (car xs))))
             (loop (cdr xs)
                   (if space (pp:^^ acc (pp:group (pp:^^ pp:break1 d))) (pp:^^ acc d))
                   #f))))))

;; [layout->string doc width] lays out a document at the given width
;; and returns the text, without a final newline.
(define (layout->string doc width)
  (call-with-output-string (lambda (port) (pp:pretty 1.0 width port doc))))

;; [render-flat ctx xs] is an inline list rendered on a single line,
;; as a string.  It is used where the output format does not allow
;; line breaks, such as Markdown headings and table cells.
(define (render-flat c xs)
  (string-translate (layout->string (render-inlines c xs) 1000000) "\n" " "))

;; [render-string ctx s] is the escaped string s, as a string.
(define (render-string c s)
  (write-text (ctx-self c) s c))

;; [render-text ctx s] is the escaped string s, as a document.  Line
;; breaks in s become hard line breaks.
(define (render-text c s)
  (verbatim-lines (render-string c s)))

;; [vcat docs] joins documents with line breaks; [vsep docs] with
;; blank lines.
(define (vcat docs)
  (pp:fold1 (lambda (x y) (pp:dcat x pp:hardline y)) docs))

(define (vsep docs)
  (pp:fold1 (lambda (x y) (pp:dcat x pp:hardline pp:hardline y)) docs))

;; [verbatim-lines s] lays out s line by line, as given.
(define (verbatim-lines s)
  (vcat (map pp:text (string-split s "\n" #t))))

;; [make-escaper table] is a procedure that replaces each character of
;; a string that appears in table, an alist from characters to
;; strings.
(define (make-escaper table)
  (lambda (s)
    (if (not (string-any (lambda (ch) (assv ch table)) s))
        s
        (call-with-output-string
         (lambda (port)
           (string-for-each
            (lambda (ch)
              (cond ((assv ch table) => (lambda (e) (display (cdr e) port)))
                    (else (write-char ch port))))
            s))))))

;; [render-text-table ctx tbl #!key head-rule] draws a Table block as
;; a grid of ASCII boxes, one per cell, in the form of pandoc's grid
;; tables:
;;
;;   +-----+-----+
;;   | a   | b   |
;;   +=====+=====+
;;   | c         |
;;   +-----------+
;;
;; A cell spanning rows or columns gets one box over the whole span.
;; The line below the header rows, and the lines above and below the
;; footer rows, are drawn with head-rule.  Column
;; widths start from the widest cell content and are narrowed to fit
;; the width option; cell content is rendered by the writer of ctx at
;; the width of its box.
(define (render-text-table c tbl #!key (head-rule #\=))
  (let* ((c      (ctx-with-option c 'in-table #t))
         (ncols  (max 1 (table-width tbl)))
         (width  (ctx-option c 'width 80))
         (parts  (table-parts tbl))
         (grids  (map (lambda (named) (table-grid (caddr named) ncols)) parts))
         (nrows  (fold + 0 (map grid-nrows grids)))
         (head-rows (grid-nrows (car grids)))
         (foot-rows (grid-nrows (last grids)))
         ;; Every box as (row col rowspan colspan cell), rows counted
         ;; across all parts; slots no cell reaches get an empty box.
         (boxes  (let loop ((grids grids) (offset 0) (acc '()))
                   (if (null? grids)
                       (reverse acc)
                       (let ((g (car grids)))
                         (loop (cdr grids) (+ offset (grid-nrows g))
                               (fold (lambda (r acc)
                                       (fold (lambda (col acc)
                                               (cases grid-slot (grid-ref g r col)
                                                 (Origin (cell)
                                                   (cons (list (+ offset r) col
                                                               (min (table-cell-rowspan cell) (- (grid-nrows g) r))
                                                               (min (table-cell-colspan cell) (- ncols col))
                                                               cell)
                                                         acc))
                                                 (Missing () (cons (list (+ offset r) col 1 1 #f) acc))
                                                 (else acc)))
                                             acc (iota ncols)))
                                     acc (iota (grid-nrows g))))))))
         (widths  (make-vector ncols 1))
         (heights (make-vector (max nrows 1) 1)))

    (define (box-row b) (car b))
    (define (box-col b) (cadr b))
    (define (box-rs b)  (caddr b))
    (define (box-cs b)  (cadddr b))
    (define (box-cell b) (list-ref b 4))

    (define (cell-lines cell w)
      (if (not cell)
          '()
          (let ((s (layout->string (vsep (render-blocks c (table-cell-blocks cell))) w)))
            (if (string-null? s) '() (string-split s "\n" #t)))))

    (define (span-width b)
      (+ (fold + 0 (map (lambda (k) (vector-ref widths k))
                        (iota (box-cs b) (box-col b))))
         (* 3 (- (box-cs b) 1))))

    (define (span-height b)
      (+ (fold + 0 (map (lambda (k) (vector-ref heights k))
                        (iota (box-rs b) (box-row b))))
         (- (box-rs b) 1)))

    (define (longest lines) (fold max 0 (map string-length lines)))

    ;; Grow the last column (or row) of a span until the span holds n.
    (define (grow-span! vec first count size n)
      (let ((have (+ (fold + 0 (map (lambda (k) (vector-ref vec k)) (iota count first)))
                     (* size (- count 1)))))
        (when (> n have)
          (let ((last (+ first count -1)))
            (vector-set! vec last (+ (vector-ref vec last) (- n have)))))))

    ;; Natural widths, then narrow the widest column until the table
    ;; fits, keeping every column at least 6 characters wide.
    (for-each (lambda (b)
                (let ((n (longest (cell-lines (box-cell b) 1000000))))
                  (if (= (box-cs b) 1)
                      (vector-set! widths (box-col b) (max (vector-ref widths (box-col b)) n))
                      (grow-span! widths (box-col b) (box-cs b) 3 n))))
              (sort boxes (lambda (a b) (< (box-cs a) (box-cs b)))))
    (let shrink ()
      (let* ((ws (vector->list widths))
             (total (+ (fold + 0 ws) (* 3 ncols) 1))
             (widest (fold max 0 ws)))
        (when (and (> total width) (> widest 6))
          (let ((k (list-index (lambda (w) (= w widest)) ws)))
            (vector-set! widths k (- widest 1))
            (shrink)))))

    ;; Render each cell at the width of its box; words longer than the
    ;; box widen it again.  Then size the rows to hold the lines.
    (let* ((rendered (map (lambda (b) (cons b (cell-lines (box-cell b) (span-width b)))) boxes)))
      (for-each (lambda (bl)
                  (let ((b (car bl)))
                    (grow-span! widths (box-col b) (box-cs b) 3 (longest (cdr bl)))))
                rendered)
      (for-each (lambda (bl)
                  (let ((b (car bl)))
                    (grow-span! heights (box-row b) (box-rs b) 1 (length (cdr bl)))))
                (sort rendered (lambda (a b) (< (box-rs (car a)) (box-rs (car b))))))

      (let* ((xs (list->vector (reverse (fold (lambda (k acc) (cons (+ (car acc) (vector-ref widths k) 3) acc))
                                              '(0) (iota ncols)))))
             (ys (list->vector (reverse (fold (lambda (k acc) (cons (+ (car acc) (vector-ref heights k) 1) acc))
                                              '(0) (iota nrows)))))
             (canvas (list->vector (map (lambda (y) (make-string (+ 1 (vector-ref xs ncols)) #\space))
                                        (iota (+ 1 (vector-ref ys nrows))))))
             (put! (lambda (y x ch) (string-set! (vector-ref canvas y) x ch))))

        (define (box-edges b)
          (values (vector-ref xs (box-col b)) (vector-ref xs (+ (box-col b) (box-cs b)))
                  (vector-ref ys (box-row b)) (vector-ref ys (+ (box-row b) (box-rs b)))))

        ;; Lines first, corners last, so that corners win where the
        ;; lines of neighbouring boxes meet.
        (for-each
         (lambda (b)
           (receive (x0 x1 y0 y1) (box-edges b)
             (for-each (lambda (y)
                         (let ((ch (if (or (and (> head-rows 0) (= y (vector-ref ys head-rows)))
                                           (and (> foot-rows 0)
                                                (or (= y (vector-ref ys (- nrows foot-rows)))
                                                    (= y (vector-ref ys nrows)))))
                                       head-rule #\-)))
                           (do ((x (+ x0 1) (+ x 1))) ((>= x x1)) (put! y x ch))))
                       (list y0 y1))
             (do ((y (+ y0 1) (+ y 1))) ((>= y y1))
               (put! y x0 #\|) (put! y x1 #\|))))
         boxes)
        (for-each
         (lambda (b)
           (receive (x0 x1 y0 y1) (box-edges b)
             (for-each (lambda (y) (put! y x0 #\+) (put! y x1 #\+)) (list y0 y1))))
         boxes)

        ;; Cell text, aligned within the box.
        (for-each
         (lambda (bl)
           (let ((b (car bl)))
             (receive (x0 x1 y0 y1) (box-edges b)
               (let ((inner (- x1 x0 3))
                     (align (if (box-cell b) (table-cell-align (box-cell b)) 'default)))
                 (for-each
                  (lambda (line i)
                    (let* ((pad (- inner (string-length line)))
                           (x   (+ x0 2 (case align
                                          ((right) (max 0 pad))
                                          ((center) (max 0 (quotient pad 2)))
                                          (else 0)))))
                      (for-each
                       (lambda (ch k)
                         (when (< (+ x k) x1) (put! (+ y0 1 i) (+ x k) ch)))
                       (string->list line) (iota (string-length line)))))
                  (cdr bl) (iota (length (cdr bl))))))))
         rendered)

        (vcat (map pp:text (vector->list canvas)))))))

;; [ref-target ctx key] is the xref-target a reference key denotes, or
;; #f.
(define (ref-target c key)
  (resolved-lookup (ctx-resolved c) key))

;; [ref-label-inlines ctx target] is the generated text of a reference
;; to target.  With the option (ref-style . number), numbered targets
;; are referred to by kind and number ("section 1.2"); otherwise by
;; title, falling back to the number.
(define (ref-label-inlines c target)
  (let* ((number (xref-target-number target))
         (title  (xref-target-title target))
         (numstr (and number (string-intersperse (map number->string number) ".")))
         (by-number (and numstr
                         (list (Str (symbol->string (xref-target-kind target)))
                               (NBSpace) (Str numstr)))))
    (if (eq? (ctx-option-symbol c 'ref-style 'title) 'number)
        (or by-number title)
        (if (pair? title) title (or by-number (list (Str (xref-target-key target))))))))

;; [xref-href ctx target] is the link target of an xref-target: its
;; anchor in this document, or the name of another document followed
;; by the anchor if the target is not the whole document.  The option
;; xref-suffix (for example ".html") is appended to document names.
(define (xref-href c t)
  (let ((anchor (xref-target-anchor t))
        (doc    (xref-target-doc t)))
    (string-append (if doc (string-append doc (->string (ctx-option c 'xref-suffix ""))) "")
                   (if anchor (string-append "#" anchor) ""))))

;; [cite-fallback-inlines ctx cs] renders citations as their keys in
;; brackets, for writers with no citation syntax of their own when no
;; citation processor has filled in the citation text.  Each key is
;; reported once per rendering.
(define (cite-fallback-inlines c cs)
  (let ((warned (ctx-option c 'warned-citations #f)))
    (for-each (lambda (cit)
                (when (and warned (not (member (citation-key cit) (stack->list warned))))
                  (stack-push! warned (citation-key cit))
                  (warning "multidoc: citation not processed; use a citation processor"
                           (citation-key cit))))
              cs))
  (list (Str (string-append "["
                            (string-intersperse (map citation-key cs) "; ")
                            "]"))))


;; ---------------------------------------------------------------------
;; Entry points
;; ---------------------------------------------------------------------

;; [make-render-context w d options] prepares document d for rendering
;; with writer w and returns (values prepared-doc ctx).  It runs the
;; citation processor named by the option citations (default native),
;; and resolves the document.  With the option refdb, a reference
;; database, internal links become checked references and keys the
;; document does not define are looked up in the database.
(define (make-render-context w d options)
  (let* ((option (lambda (key default) (cond ((assq key options) => cdr) (else default))))
         (refdb  (option 'refdb #f))
         (cp     (let ((p (option 'citations 'native)))
                   (if (citation-processor? p) p (lookup-citation-processor p))))
         (d      (if refdb (int-links->refs d) d))
         (d      (cp-process cp d refdb options)))
    (receive (d r) (resolve-doc d external: (or (option 'xref-lookup #f)
                                               (and refdb (refdb-resolver refdb))))
      (values d (make-ctx w (cons (cons 'warned-citations (make-stack)) options) r d 0 #f)))))

(define (writer-of to)
  (if (writer? to) to (lookup-writer to)))

;; [render d #!key to width options] renders document d with the
;; writer `to' (a writer or a registered name) and returns the
;; wl-pprint document.
(define (render d #!key (to 'html) (width 80) (options '()))
  (let ((w (writer-of to)))
    (receive (d c) (make-render-context w d (cons (cons 'width width) options))
      (write-doc w d c))))

;; [convert sxml #!key to width options filters top-division port]
;; reads an SXML document, applies filters (procedures from documents
;; to documents) in order, and writes it with the writer `to' to port
;; (a port, a path, or #f for the current output port).
(define (convert sxml #!key (to 'html) (width 80) (options '()) (filters '())
                 (top-division 'section) (port #f))
  (let* ((w (writer-of to))
         (d (fold (lambda (f d) (f d))
                  (sxml->doc sxml top-division: top-division)
                  filters)))
    (receive (d c) (make-render-context
                    w d (append (list (cons 'width width) (cons 'top-division top-division))
                                options))
      (write-output w d c port))))

)
