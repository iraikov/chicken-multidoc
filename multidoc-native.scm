;;
;; multidoc.native - print the intermediate representation
;;
;; The native writer shows a resolved document as S-expressions that
;; mirror the IR constructors, laid out to the width option.  It is
;; meant for inspecting what a reader produced and what filters did.
;; Custom nodes are printed as they are, with their payload, rather
;; than lowered.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.native

  (native-writer)

  (import scheme (chicken base) (chicken port)
          (srfi 1)
          datatype
          (prefix wl-pprint pp:)
          multidoc.ir multidoc.backend)


;; ---------------------------------------------------------------------
;; S-expression layout
;; ---------------------------------------------------------------------

;; [sx head . items] is a parenthesized form: on one line if it fits,
;; otherwise with one item per line, indented under the head.
(define (sx head . items)
  (pp:group
   (pp:dcat pp:lparen (pp:text head)
            (pp:nest 2 (fold-right (lambda (d acc) (pp:dcat pp:break1 d acc)) pp:empty items))
            pp:rparen)))

;; [sx-list docs] is a parenthesized list of forms.
(define (sx-list docs)
  (if (null? docs)
      (pp:text "()")
      (pp:group (pp:dcat pp:lparen
                         (pp:align (pp:fold1 (lambda (x y) (pp:dcat x pp:break1 y)) docs))
                         pp:rparen))))

(define (datum x)
  (pp:text (with-output-to-string (lambda () (write x)))))

(define (attr-doc a)
  (sx "attr" (datum (attr-id a)) (datum (attr-classes a)) (datum (attr-kvs a))
      (let ((s (attr-style a)))
        (if s (sx "style" (datum (style-name s)) (datum (style-props s))) (datum #f)))))


;; ---------------------------------------------------------------------
;; Elements
;; ---------------------------------------------------------------------

;; Children are printed with the writer's own methods, not through
;; render-block, so that custom nodes are not lowered.
(define (blocks-doc c bs)
  (sx-list (map (lambda (b) (write-block (ctx-self c) b c)) bs)))

;; Inline lists are filled like text, as many elements per line as fit.
(define (inlines-doc c xs)
  (let ((docs (map (lambda (x) (write-inline (ctx-self c) x c)) xs)))
    (if (null? docs)
        (pp:text "()")
        (pp:dcat pp:lparen
                 (pp:align (fold (lambda (d acc) (pp:^^ acc (pp:group (pp:^^ pp:break1 d))))
                                 (car docs) (cdr docs)))
                 pp:rparen))))

(define (cell-doc c cell)
  (sx "cell" (attr-doc (table-cell-attr cell)) (datum (table-cell-align cell))
      (datum (table-cell-rowspan cell)) (datum (table-cell-colspan cell))
      (blocks-doc c (table-cell-blocks cell))))

(define (part-doc c part)
  (sx "part" (attr-doc (table-part-attr part))
      (sx-list (map (lambda (row)
                      (sx "row" (attr-doc (table-row-attr row))
                          (sx-list (map (lambda (cell) (cell-doc c cell)) (table-row-cells row)))))
                    (table-part-rows part)))))

(define (native-block self b c)
  (cases block b
    (Plain (xs) (sx "Plain" (inlines-doc c xs)))
    (Para (xs) (sx "Para" (inlines-doc c xs)))
    (CodeBlock (a s) (sx "CodeBlock" (attr-doc a) (datum s)))
    (RawBlock (format s) (sx "RawBlock" (datum format) (datum s)))
    (BlockQuote (bs) (sx "BlockQuote" (blocks-doc c bs)))
    (BulletList (is) (sx "BulletList" (sx-list (map (lambda (i) (blocks-doc c i)) is))))
    (OrderedList (is) (sx "OrderedList" (sx-list (map (lambda (i) (blocks-doc c i)) is))))
    (DefinitionList (is)
      (sx "DefinitionList"
          (sx-list (map (lambda (item)
                          (sx-list (cons (inlines-doc c (car item))
                                         (map (lambda (d) (blocks-doc c d)) (cdr item)))))
                        is))))
    (Section (division a numbered title body)
      (sx "Section" (datum division) (attr-doc a) (datum numbered)
          (inlines-doc c title) (blocks-doc c body)))
    (Definition (a sigs body)
      (sx "Definition" (attr-doc a)
          (sx-list (map (lambda (sig) (sx-list (list (datum (car sig)) (inlines-doc c (cdr sig)))))
                        sigs))
          (blocks-doc c body)))
    (Table (a caption colspecs head bodies foot)
      (sx "Table" (attr-doc a) (inlines-doc c caption)
          (sx-list (map (lambda (cs) (sx "colspec" (datum (colspec-align cs)) (datum (colspec-width cs))))
                        colspecs))
          (part-doc c head)
          (sx-list (map (lambda (p) (part-doc c p)) bodies))
          (part-doc c foot)))
    (TableOfContents () (pp:text "TableOfContents"))
    (Bibliography (a) (sx "Bibliography" (attr-doc a)))
    (HorizontalRule () (pp:text "HorizontalRule"))
    (Div (a bs) (sx "Div" (attr-doc a) (blocks-doc c bs)))
    (Null () (pp:text "Null"))
    (CustomBlock (tag a payload) (sx "CustomBlock" (datum tag) (attr-doc a) (datum payload)))))

(define (citation-doc c cit)
  (sx "citation" (datum (citation-key cit)) (inlines-doc c (citation-prefix cit))
      (inlines-doc c (citation-suffix cit)) (datum (citation-locator cit))
      (datum (citation-mode cit))))

(define (native-inline self x c)
  (cases inline x
    (Str (s) (sx "Str" (datum s)))
    (Space () (pp:text "Space"))
    (SoftBreak () (pp:text "SoftBreak"))
    (LineBreak () (pp:text "LineBreak"))
    (NBSpace () (pp:text "NBSpace"))
    (Emph (xs) (sx "Emph" (inlines-doc c xs)))
    (Strong (xs) (sx "Strong" (inlines-doc c xs)))
    (Small (xs) (sx "Small" (inlines-doc c xs)))
    (Code (a s) (sx "Code" (attr-doc a) (datum s)))
    (Link (a xs target kind) (sx "Link" (attr-doc a) (inlines-doc c xs) (datum target) (datum kind)))
    (Image (a alt src) (sx "Image" (attr-doc a) (inlines-doc c alt) (datum src)))
    (Span (a xs) (sx "Span" (attr-doc a) (inlines-doc c xs)))
    (RawInline (format s) (sx "RawInline" (datum format) (datum s)))
    (Cite (cs xs) (sx "Cite" (sx-list (map (lambda (cit) (citation-doc c cit)) cs)) (inlines-doc c xs)))
    (Ref (kind key xs) (sx "Ref" (datum kind) (datum key) (inlines-doc c xs)))
    (CustomInline (tag a payload) (sx "CustomInline" (datum tag) (attr-doc a) (datum payload)))))

(define (native-doc self d c)
  (sx "doc" (datum (doc-meta d)) (blocks-doc c (doc-blocks d))))

(define native-writer
  (register-writer!
   (make-writer name: 'native
                extension: "native"
                doc: native-doc
                block: native-block
                inline: native-inline)))

)
