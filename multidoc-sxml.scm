;;
;; multidoc.sxml - read SXML into the multidoc IR
;;
;; The reader maps each SXML element to IR values through a table of
;; rules, keyed by tag.  A rule receives the tag, the attribute list,
;; the children and a procedure for reading nested content, and
;; returns a block, an inline, or a list of either.  Users may add or
;; replace rules with `sxml-reader-add-rule!'.  Elements with no rule
;; become a `Div' or `Span' whose class is the tag name, so that no
;; content is lost.
;;
;; Blocks and inlines are kept apart: where a block is expected, each
;; run of inlines is wrapped in a paragraph (or a `Plain' block inside
;; list items and table cells); where inlines are expected, a
;; paragraph contributes its inlines.
;;
;; Copyright (c) 2026 Ivan Raikov
;;
;; This is a reimplementation of the CHICKEN 4 multidoc by Peter Bex,
;; Ivan Raikov.

(module multidoc.sxml

  (sxml->doc
   sxml-reader-rules sxml-reader-add-rule! reader-top-division
   read-sxml-nodes items->blocks items->inlines tidy-inlines
   sxml-text sxml->html-string attr-value)

  (import scheme (only (scheme base) make-parameter)
          (chicken base) (chicken format) (chicken string)
          (srfi 1) (srfi 13) datatype matchable multidoc.ir)

  (include "box.scm")
  (include "stack.scm")


;; ---------------------------------------------------------------------
;; SXML accessors
;; ---------------------------------------------------------------------

(define (attr-list? x) (and (pair? x) (eq? (car x) '@)))

;; [element-attrs children] is the attribute list at the head of an
;; element's children, as a list of (name value ...) entries.
(define (element-attrs children)
  (if (and (pair? children) (attr-list? (car children)))
      (cdar children)
      '()))

(define (element-children children)
  (if (and (pair? children) (attr-list? (car children)))
      (cdr children)
      children))

;; [attr-value attrs name] is the value of attribute `name' as a
;; string, or #f.
(define (attr-value attrs name)
  (cond ((assq name attrs)
         => (lambda (a) (if (pair? (cdr a)) (sxml-text (cdr a)) "")))
        (else #f)))

;; [sxml-text nodes] is the concatenated text of an SXML node list,
;; without attributes.
(define (sxml-text nodes)
  (define (text node)
    (cond ((string? node) node)
          ((char? node) (string node))
          ((number? node) (number->string node))
          ((symbol? node) "")
          ((attr-list? node) "")
          ((and (pair? node) (symbol? (car node))) (sxml-text (cdr node)))
          ((pair? node) (sxml-text node))
          (else "")))
  (if (list? nodes)
      (apply string-append (map text nodes))
      (text nodes)))

;; [sxml->html-string node] serializes an SXML node as HTML, for
;; elements such as `script' that pass through to HTML output.  The
;; text content of script and style elements is not escaped.
(define (sxml->html-string node)
  (define (escape s)
    (string-translate* s '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;"))))
  (define (ser node #!optional (raw #f))
    (cond ((string? node) (if raw node (escape node)))
          ((attr-list? node) "")
          ((and (pair? node) (symbol? (car node)))
           (let ((tag (symbol->string (car node)))
                 (attrs (element-attrs (cdr node)))
                 (kids (element-children (cdr node))))
             (string-append
              "<" tag
              (apply string-append
                     (map (lambda (a)
                            (string-append " " (symbol->string (car a)) "=\""
                                           (escape (sxml-text (cdr a))) "\""))
                          attrs))
              ">"
              (let ((raw (memq (car node) '(script style))))
                (apply string-append (map (lambda (k) (ser k raw)) kids)))
              "</" tag ">")))
          ((pair? node) (apply string-append (map (lambda (k) (ser k raw)) node)))
          (else "")))
  (ser node))


;; ---------------------------------------------------------------------
;; Items: the mixed block and inline results of reading
;; ---------------------------------------------------------------------

(define (space? x)
  (and (inline? x)
       (cases inline x
         (Space () #t)
         (SoftBreak () #t)
         (else #f))))

;; [tidy-inlines xs trim?] merges runs of spaces into one, and if
;; trim? is true, removes spaces at both ends.
(define (tidy-inlines xs #!optional (trim? #t))
  (let* ((merged (let loop ((xs xs) (acc '()) (prev-space #f))
                   (cond ((null? xs) (reverse acc))
                         ((space? (car xs))
                          (if prev-space
                              (loop (cdr xs) acc #t)
                              (loop (cdr xs) (cons (Space) acc) #t)))
                         (else (loop (cdr xs) (cons (car xs) acc) #f))))))
    (if trim?
        (reverse (drop-while space? (reverse (drop-while space? merged))))
        merged)))

;; [block->inlines b] is the inline content of a block that appears
;; where inlines are expected.
(define (block->inlines b)
  (cases block b
    (Plain (xs) xs)
    (Para (xs) xs)
    (CodeBlock (a s) (list (Code a s)))
    (RawBlock (format s) (list (RawInline format s)))
    (Null () '())
    (else
     (warning "multidoc: block element in inline context, dropped" b)
     '())))

;; [items->inlines items trim?] converts reader results to inlines.
(define (items->inlines items #!optional (trim? #f))
  (tidy-inlines (append-map (lambda (x) (if (block? x) (block->inlines x) (list x)))
                            items)
                trim?))

;; [items->blocks items wrap] converts reader results to blocks,
;; wrapping each run of inlines with `wrap' (Para or Plain).  Runs
;; consisting only of spaces are dropped.
(define (items->blocks items #!optional (wrap Para))
  (let loop ((items items) (run '()) (acc '()))
    (define (flush acc)
      (let ((xs (tidy-inlines (reverse run))))
        (if (null? xs) acc (cons (wrap xs) acc))))
    (cond ((null? items) (reverse (flush acc)))
          ((block? (car items))
           (loop (cdr items) '() (cons (car items) (flush acc))))
          (else (loop (cdr items) (cons (car items) run) acc)))))


;; ---------------------------------------------------------------------
;; The reader
;; ---------------------------------------------------------------------

;; The division kind that wiki heading level 2 (the top of a page)
;; becomes.
(define reader-top-division (make-parameter 'section))

;; [read-sxml-nodes nodes] reads a list of SXML nodes and returns the
;; list of blocks and inlines they denote.
(define (read-sxml-nodes nodes)
  (append-map read-node nodes))

(define (read-node node)
  (cond ((string? node) (string->inlines node))
        ((char? node) (string->inlines (string node)))
        ((number? node) (list (Str (number->string node))))
        ((symbol? node) (list (Str (symbol->string node))))
        ((attr-list? node) '())
        ((and (pair? node) (symbol? (car node)))
         (read-element (car node) (cdr node)))
        ((pair? node) (read-sxml-nodes node))
        (else '())))

;; [recur nodes mode] is the procedure passed to rules for reading
;; nested content.  mode is one of:
;;   items   - blocks and inlines as read
;;   inlines - inlines, spaces merged
;;   title   - inlines, spaces merged and trimmed
;;   blocks  - blocks, inline runs wrapped in Para
;;   plain   - blocks, inline runs wrapped in Plain
(define (recur nodes #!optional (mode 'items))
  (let ((items (read-sxml-nodes (if (list? nodes) nodes (list nodes)))))
    (case mode
      ((items)   items)
      ((inlines) (items->inlines items))
      ((title)   (items->inlines items #t))
      ((blocks)  (items->blocks items Para))
      ((plain)   (items->blocks items Plain))
      (else (error 'multidoc "unknown reader mode" mode)))))

(define (label-node? x)
  (and (pair? x) (eq? (car x) 'label)))

;; Elements may contain (label "id") children, which give the result
;; an identifier.
(define (read-element tag body)
  (let* ((label    (find label-node? body))
         (body     (if label (remove label-node? body) body))
         (attrs    (element-attrs body))
         (children (element-children body))
         (rule     (assq tag (sxml-reader-rules)))
         (result   (if rule
                       ((cdr rule) tag attrs children recur)
                       (read-unknown tag attrs children)))
         (items    (cond ((or (block? result) (inline? result)) (list result))
                         ((list? result) (filter (lambda (x) (or (block? x) (inline? x))) result))
                         (else '()))))
    (if label
        (give-id (sxml-text (cdr label)) items)
        items)))

;; [give-id id items] attaches an identifier to a single labelled
;; element, or wraps several in a Div or Span with that identifier.
(define (give-id id items)
  (define (with-id b)
    (cases block b
      (Section (division a numbered title body)
        (Section division (attr-with-id a id) numbered title body))
      (Table (a caption colspecs head bodies foot)
        (Table (attr-with-id a id) caption colspecs head bodies foot))
      (Definition (a sigs body) (Definition (attr-with-id a id) sigs body))
      (Div (a bs) (Div (attr-with-id a id) bs))
      (CodeBlock (a s) (CodeBlock (attr-with-id a id) s))
      (CustomBlock (tag a payload) (CustomBlock tag (attr-with-id a id) payload))
      (else (Div (attr-with-id null-attr id) (list b)))))
  (cond ((and (= (length items) 1) (block? (car items)))
         (list (with-id (car items))))
        ((any block? items)
         (list (Div (attr-with-id null-attr id) (items->blocks items))))
        (else
         (list (Span (attr-with-id null-attr id) (items->inlines items))))))

;; HTML elements that are blocks even when they hold only text.
(define html-block-tags
  '(center address article aside figure footer header nav))

;; Elements with no rule become a Div (if they contain blocks, or are
;; HTML block elements) or a Span, with the tag name as class and the
;; attributes as key/value pairs.
(define (read-unknown tag attrs children)
  (let* ((items (recur children))
         (a     (sxml-attrs->attr attrs (list (symbol->string tag)))))
    (if (or (any block? items) (memq tag html-block-tags))
        (Div a (items->blocks items))
        (Span a (items->inlines items)))))

;; [sxml-attrs->attr attrs classes] converts SXML attributes: id and
;; class are taken apart, the rest become key/value pairs.
(define (sxml-attrs->attr attrs #!optional (classes '()))
  (make-attr (attr-value attrs 'id)
             (append classes
                     (let ((c (attr-value attrs 'class)))
                       (if c (string-split c) '())))
             (filter-map (lambda (a)
                           (and (not (memq (car a) '(id class)))
                                (cons (symbol->string (car a))
                                      (attr-value attrs (car a)))))
                         attrs)
             #f))

;; Text of verbatim content: one leading newline and trailing
;; newlines are removed.
(define (verbatim-text children)
  (let* ((s (sxml-text children))
         (s (if (string-prefix? "\n" s) (substring s 1 (string-length s)) s)))
    (string-trim-right s #\newline)))


;; ---------------------------------------------------------------------
;; Built-in rules
;; ---------------------------------------------------------------------

(define (section-rule numbered)
  (lambda (tag attrs children recur)
    (match children
      (((? integer? level) head . contents)
       (let* ((more     (element-attrs contents))
              (contents (element-children contents))
              (id       (or (attr-value attrs 'id) (attr-value more 'id))))
         (Section (level->division level (reader-top-division))
                  (make-attr id '() '() #f)
                  numbered
                  (recur (list head) 'title)
                  (recur contents 'blocks))))
      (else
       (error 'multidoc "section elements must be of the form (section level head-word . contents)"
              (cons tag children))))))

(define (list-rule make-list)
  (lambda (tag attrs children recur)
    (make-list
     (filter-map
      (lambda (child)
        (cond ((and (pair? child) (eq? (car child) 'li))
               (recur (element-children (cdr child)) 'plain))
              ((and (string? child) (string-null? (string-trim-both child))) #f)
              (else (recur (list child) 'plain))))
      children))))

;; (dl (dt term) (dd definition) ...): each dt starts an item, each
;; dd adds a definition to the current item.
(define (dl-rule tag attrs children recur)
  (let loop ((children children) (items '()))
    (define (add-def blocks)
      (if (null? items)
          (list (list '() blocks))
          (cons (append (car items) (list blocks)) (cdr items))))
    (cond ((null? children)
           (DefinitionList (reverse (map (lambda (item) (cons (car item) (cdr item))) items))))
          ((and (pair? (car children)) (eq? (caar children) 'dt))
           (loop (cdr children)
                 (cons (list (recur (element-children (cdar children)) 'title)) items)))
          ((and (pair? (car children)) (eq? (caar children) 'dd))
           (loop (cdr children) (add-def (recur (element-children (cdar children)) 'plain))))
          ((and (string? (car children)) (string-null? (string-trim-both (car children))))
           (loop (cdr children) items))
          (else
           (loop (cdr children) (add-def (recur (list (car children)) 'plain)))))))

;; (def (sig (procedure "(proc ARG ...)") ...) body ...)
(define (def-rule tag attrs children recur)
  (let* ((sig?  (lambda (x) (and (pair? x) (eq? (car x) 'sig))))
         (sigs  (append-map
                 (lambda (s)
                   (filter-map
                    (lambda (spec)
                      (and (pair? spec) (symbol? (car spec))
                           (cons (car spec)
                                 (list (Code null-attr (sxml-text (element-children (cdr spec))))))))
                    (element-children (cdr s))))
                 (filter sig? children))))
    (Definition (sxml-attrs->attr attrs) sigs (recur (remove sig? children) 'blocks))))

(define (align-symbol s)
  (cond ((not s) 'default)
        ((member s '("left" "center" "right")) (string->symbol s))
        (else 'default)))

(define (span-value s)
  (let ((n (and s (string->number s))))
    (if (and n (exact? n) (> n 0)) n 1)))

;; (table [(@ (border ..) (align ..) (caption ..))] (tr (th ..) (td ..)) ...)
;; Leading rows made only of th cells form the header part.
(define (table-rule tag attrs children recur)
  (define (element? x name) (and (pair? x) (eq? (car x) name)))
  (define (read-row tr)
    (let ((cells (filter (lambda (c) (or (element? c 'td) (element? c 'th)))
                         (element-children (cdr tr)))))
      (cons (and (pair? cells) (every (lambda (c) (element? c 'th)) cells))
            (make-table-row
             null-attr
             (map (lambda (c)
                    (let ((cattrs (element-attrs (cdr c))))
                      (make-table-cell (sxml-attrs->attr
                                        (remove (lambda (x) (memq (car x) '(align rowspan colspan)))
                                                cattrs))
                                       (align-symbol (attr-value cattrs 'align))
                                       (span-value (attr-value cattrs 'rowspan))
                                       (span-value (attr-value cattrs 'colspan))
                                       (recur (element-children (cdr c)) 'plain))))
                  cells)))))
  (let* ((rows    (map read-row (filter (lambda (x) (element? x 'tr)) children)))
         (head    (map cdr (take-while car rows)))
         (body    (map cdr (drop-while car rows)))
         (border  (attr-value attrs 'border))
         (caption (attr-value attrs 'caption))
         (a       (make-attr (attr-value attrs 'id) '()
                             (filter-map (lambda (k)
                                           (let ((v (attr-value attrs k)))
                                             (and v (cons (symbol->string k) v))))
                                         '(align summary))
                             (and border (not (equal? border "0"))
                                  (make-style "bordered"
                                              `((table (border ,(or (string->number border) 1)))))))))
    (Table a
           (if caption (string->inlines caption) '())
           '()
           (make-table-part null-attr head)
           (list (make-table-part null-attr body))
           empty-table-part)))

(define (link-rule tag attrs children recur)
  (match children
    ((url . text)
     (let ((url (sxml-text (list url))))
       (Link null-attr
             (if (null? text) (list (Str url)) (recur text 'title))
             url 'external)))
    (else '())))

;; (int-link "Page#Section" [text]): the section part is normalized
;; with `title->anchor-id', so that it matches the section's
;; identifier; the page part is kept as given.
(define (int-link-rule tag attrs children recur)
  (match children
    ((target . text)
     (let* ((target (sxml-text (list target)))
            (parts  (string-split target "#" #t))
            (href   (if (null? (cdr parts))
                        target
                        (string-append (car parts) "#"
                                       (title->anchor-id (string-intersperse (cdr parts) "#"))))))
       (Link (make-attr #f '() (list (cons "int-link" target)) #f)
             (if (null? text) (list (Str target)) (recur text 'title))
             href 'internal)))
    (else '())))

(define (image-link-rule tag attrs children recur)
  (match children
    ((src . alt)
     (Image null-attr (recur alt 'title) (sxml-text (list src))))
    (else '())))

(define (img-rule tag attrs children recur)
  (Image (sxml-attrs->attr (remove (lambda (a) (memq (car a) '(src alt))) attrs))
         (string->inlines (or (attr-value attrs 'alt) ""))
         (or (attr-value attrs 'src) "")))

;; (examples (example (init ..) (expr ..) (result ..)) ...)
;; The payload is a list of examples, each a list of (role . text).
(define (examples-rule tag attrs children recur)
  (define (parts example)
    (filter-map (lambda (x)
                  (and (pair? x) (memq (car x) '(init expr result))
                       (cons (car x) (verbatim-text (element-children (cdr x))))))
                (element-children (cdr example))))
  (CustomBlock 'examples (sxml-attrs->attr attrs)
               (map parts (filter (lambda (x) (and (pair? x) (eq? (car x) 'example)))
                                  children))))

;; (ref "key" [text ...]): a reference to a target of this or another
;; document.  The attribute kind, if given, names the kind of target.
(define (ref-rule tag attrs children recur)
  (match children
    ((key . text)
     (Ref (let ((k (attr-value attrs 'kind))) (if k (string->symbol k) 'any))
          (sxml-text (list key))
          (recur text 'title)))
    (else '())))

;; (cite [(@ (mode ..) (prefix ..) (suffix ..) (locator ..))] "key" ...):
;; mode is normal, author-in-text or suppress-author.  The prefix goes
;; with the first key; the suffix and locator with the last.
(define (cite-rule tag attrs children recur)
  (let* ((keys   (filter-map (lambda (x) (let ((s (string-trim-both (sxml-text (list x)))))
                                           (and (not (string-null? s)) s)))
                             children))
         (mode   (let ((m (attr-value attrs 'mode))) (if m (string->symbol m) 'normal)))
         (n      (length keys)))
    (Cite (map (lambda (key k)
                 (make-citation key
                                (if (= k 0) (string->inlines (or (attr-value attrs 'prefix) "")) '())
                                (if (= k (- n 1)) (string->inlines (or (attr-value attrs 'suffix) "")) '())
                                (and (= k (- n 1)) (attr-value attrs 'locator))
                                mode))
               keys (iota n))
          '())))

;; (highlight language "code")
(define (highlight-rule tag attrs children recur)
  (match children
    (((? symbol? lang) . code)
     (CustomBlock 'highlight (make-attr #f (list (symbol->string lang)) '() #f)
                  (verbatim-text code)))
    (else (CodeBlock null-attr (verbatim-text children)))))

(define (raw-rule format)
  (lambda (tag attrs children recur)
    (RawBlock format (sxml-text children))))

(define (constant-rule value)
  (lambda (tag attrs children recur) (value)))

(define (inline-rule make)
  (lambda (tag attrs children recur)
    (make (recur children 'inlines))))

(define builtin-rules
  `((section    . ,(section-rule #t))
    (section*   . ,(section-rule #f))
    (p          . ,(lambda (tag attrs children recur) (recur children 'blocks)))
    (pre        . ,(lambda (tag attrs children recur)
                     (CodeBlock (sxml-attrs->attr attrs) (verbatim-text children))))
    (verbatim   . ,(lambda (tag attrs children recur)
                     (CodeBlock (sxml-attrs->attr attrs) (verbatim-text children))))
    (highlight  . ,highlight-rule)
    (examples   . ,examples-rule)
    (ul         . ,(list-rule BulletList))
    (ol         . ,(list-rule OrderedList))
    (dl         . ,dl-rule)
    (def        . ,def-rule)
    (table      . ,table-rule)
    (blockquote . ,(lambda (tag attrs children recur)
                     (BlockQuote (recur children 'blocks))))
    (div        . ,(lambda (tag attrs children recur)
                     (Div (sxml-attrs->attr attrs) (recur children 'blocks))))
    (hr         . ,(constant-rule HorizontalRule))
    (toc        . ,(constant-rule TableOfContents))
    (bibliography . ,(lambda (tag attrs children recur)
                       (Bibliography (sxml-attrs->attr attrs))))
    (br         . ,(constant-rule LineBreak))
    (n_         . ,(lambda (tag attrs children recur)
                     (cons (NBSpace) (recur children 'inlines))))
    (tt         . ,(lambda (tag attrs children recur)
                     (Code (sxml-attrs->attr attrs) (sxml-text children))))
    (em         . ,(inline-rule Emph))
    (i          . ,(inline-rule Emph))
    (strong     . ,(inline-rule Strong))
    (b          . ,(inline-rule Strong))
    (small      . ,(inline-rule Small))
    (ref        . ,ref-rule)
    (cite       . ,cite-rule)
    (link       . ,link-rule)
    (int-link   . ,int-link-rule)
    (image-link . ,image-link-rule)
    (img        . ,img-rule)
    (tex        . ,(raw-rule 'latex))
    (texinfo    . ,(raw-rule 'texinfo))
    (script     . ,(lambda (tag attrs children recur)
                     (RawBlock 'html (sxml->html-string (cons tag (cons (cons '@ attrs) children))))))
    ))

;; The rule table: an alist from tags to rules.  Earlier entries take
;; precedence.
(define sxml-reader-rules (make-parameter builtin-rules))

;; [sxml-reader-add-rule! tag rule] adds or replaces the rule for
;; `tag'.  A rule is called as (rule tag attrs children recur), where
;; attrs is the SXML attribute list and (recur nodes mode) reads
;; nested content; see `recur' for the modes.  The rule returns a
;; block, an inline, or a list of blocks and inlines.
(define (sxml-reader-add-rule! tag rule)
  (sxml-reader-rules (cons (cons tag rule) (sxml-reader-rules))))


;; ---------------------------------------------------------------------
;; Documents
;; ---------------------------------------------------------------------

;; [sxml->doc tree #!key top-division] reads a whole document.  tree
;; may be a (wiki-page (Header ...) (body ...)) element, a (body ...)
;; element, a single element, or a list of elements.  (Header ...)
;; fields and top-level (tags ...) become document metadata.
(define (sxml->doc tree #!key (top-division 'section))
  (let ((meta (make-stack)))
    (define (add-meta! key value)
      (stack-push! meta (cons key value)))
    (define (top node)
      (cond ((and (pair? node) (eq? (car node) 'wiki-page))
             (append-map top (element-children (cdr node))))
            ((and (pair? node) (eq? (car node) 'Header))
             (for-each (lambda (field)
                         (when (and (pair? field) (symbol? (car field)))
                           (add-meta! (car field) (sxml-text (cdr field)))))
                       (cdr node))
             '())
            ((and (pair? node) (eq? (car node) 'tags))
             (add-meta! 'tags (sxml-text (cdr node)))
             '())
            ((and (pair? node) (eq? (car node) 'body))
             (append-map top (element-children (cdr node))))
            ((and (pair? node) (pair? (car node)))
             (append-map top node))
            (else (list node))))
    (parameterize ((reader-top-division top-division))
      (let ((content (top tree)))
        (make-doc (reverse (stack->list meta))
                  (items->blocks (read-sxml-nodes content) Para))))))


;; ---------------------------------------------------------------------
;; Custom nodes introduced by the reader
;; ---------------------------------------------------------------------

;; Examples lower to a Div of class "examples", holding one Div of
;; class "example" per example, whose code blocks have the classes
;; init, expr and result.
(define-custom-node 'examples
  lower: (lambda (a payload)
           (list
            (Div (attr-add-class a "examples")
                 (map (lambda (example)
                        (Div (attr-add-class null-attr "example")
                             (map (lambda (part)
                                    (CodeBlock (attr-add-class null-attr
                                                               (symbol->string (car part)))
                                               (cdr part)))
                                  example)))
                      payload)))))

;; Highlighted code lowers to a code block whose class names the
;; language.
(define-custom-node 'highlight
  lower: (lambda (a payload) (list (CodeBlock a payload))))

)
