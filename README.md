# multidoc: Document Conversion for CHICKEN Scheme

[![Chicken Scheme](https://img.shields.io/badge/Chicken-Scheme-orange.svg)](https://call-cc.org/)

A document converter for CHICKEN Scheme in the spirit of pandoc.
multidoc reads documents written in SXML, turns them into an
algebraic intermediate representation (IR), and writes them as HTML,
LaTeX, Texinfo, Markdown or plain text. The output is laid out by the
Wadler-Leijen pretty printer (wl-pprint), so text is filled to any
width, and new output formats can be added as objects in a registry.

## Features

- **Pandoc-style IR**: Algebraic datatypes for blocks and inlines, with nested sections, definitions, tables, citations and cross-references
- **Six Writers**: HTML (XHTML 1.0 Strict), LaTeX, Texinfo, pandoc Markdown, plain text, and the IR itself as S-expressions
- **Pretty-Printed Output**: Paragraphs are filled to the chosen width
- **Extensible Writers**: Writers are Yasos objects in a registry; derive a writer by overriding single methods, or write a new one
- **Custom Nodes**: Declare new block and inline elements with a lowering into core IR, so every writer renders them, and add native renderings for chosen writers
- **Division Kinds**: Sections are parts, chapters, sections and so on rather than bare levels, so a page can become a chapter of a book
- **Rich Tables**: The pandoc 2.10 table model, with header, body and footer parts, row and column spans, and styled cells
- **References**: Labels and references within a document, links between documents checked against a cross-reference database
- **Citations**: A citation-processor protocol; LaTeX can emit natbib or biblatex commands, and the companion egg scbib formats bibliographies for every writer
- **Command Line**: The `multidoc` program converts files in any registered format

## Installation

multidoc requires CHICKEN 6.

```bash
chicken-install multidoc
```

OR

```bash
chicken-install datatype matchable yasos wl-pprint getopt-long srfi-1 srfi-13 srfi-14 srfi-69

# Clone the repository
git clone https://github.com/iraikov/chicken-multidoc.git
cd chicken-multidoc

chicken-install

# Run the tests
chicken-install -test
```

## Quick Start

### Converting a Page

Documents are written in the SXML vocabulary of the CHICKEN wiki:

```scheme
(import multidoc)

(define page
  '(wiki-page
    (Header (title "Queues"))
    (body
     (section 2 "Operations"
      (def (sig (procedure "(enqueue q x)"))
           (p "Returns " (tt "q") " with " (tt "x") " added at the back."))
      (p "See " (ref "tbl-costs") " for running times."))
     (section 2 "Costs"
      (table (label "tbl-costs") (@ (caption "Running times"))
             (tr (th "Operation") (th "Time"))
             (tr (td (tt "enqueue")) (td "O(1)")))))))

(convert page to: 'plain width: 72
         options: '((standalone . #f) (number-sections . #t) (ref-style . number)))
```

```
1 Operations
============

[procedure] (enqueue q x)

    Returns q with x added at the back.

See table 1 for running times.

2 Costs
=======

+-----------+------+
| Operation | Time |
+===========+======+
| enqueue   | O(1) |
+-----------+------+
Running times
```

The same call with `to: 'latex` gives

```latex
\section{Operations}\label{operations}

\begin{description}
\item[procedure] \texttt{(enqueue q x)}
\end{description}

Returns \texttt{q} with \texttt{x} added at the back.

See \autoref{tbl-costs} for running times.
...
```

### From the Command Line

```bash
multidoc -t latex -o page.tex page.sxml
multidoc --to=markdown -w 60 page.sxml
multidoc -t html --number-sections -V style=style.css -o page.html page.sxml
multidoc -t plain --fragment - < page.sxml

# Convert a set of pages, resolving the links between them
multidoc -t html --auto-ext -V xref-suffix=.html --xref-out=site.xref *.sxml
```

### Reading, Filtering and Rendering

`convert` reads, resolves and writes in one step. The stages can also
be run separately:

```scheme
(define doc (sxml->doc page top-division: 'chapter))

;; A filter is a procedure from documents to documents.
(define (drop-code d)
  (make-doc (doc-meta d)
            (walk-blocks (lambda (b)
                           (cases block b
                             (CodeBlock (a s) '())   ; a list is spliced in place
                             (else b)))
                         (doc-blocks d))))

;; render returns a wl-pprint document; layout->string lays it out.
(display (layout->string (render (drop-code doc) to: 'markdown width: 60) 60))
```

### Building Documents Directly

Programs can build documents with the IR constructors instead of
reading SXML:

```scheme
(define report
  (make-doc '((title . "Build report"))
            (list (Section 'section null-attr #t (string->inlines "Summary")
                           (list (Para (list (Str "All") (Space)
                                             (Strong (list (Str "42")))
                                             (Space) (Str "tests") (Space) (Str "passed."))))))))

(display (layout->string (render report to: 'html) 72))
```

### Extending a Writer

```scheme
(import multidoc datatype (prefix wl-pprint pp:))

;; HTML in which definitions are drawn in boxes.  Each override receives
;; the base method as next.
(define boxed-html
  (derive-writer html-writer
    name: 'boxed-html
    block: (lambda (self b ctx next)
             (cases block b
               (Definition (a sigs body)
                 (vcat (list (pp:text "<div class=\"box\">")
                             (next b ctx)
                             (pp:text "</div>"))))
               (else (next b ctx))))))

(register-writer! boxed-html)
(convert page to: 'boxed-html)
```

### Custom Nodes

```scheme
;; A keyboard key: lowered to code for every writer...
(define-custom-node 'kbd
  block?: #f
  lower: (lambda (attr keys) (list (Code null-attr keys))))

;; ...but written natively in Texinfo.
(define-node-writer 'kbd 'texinfo
  (lambda (attr keys ctx)
    (pp:text (string-append "@kbd{" (render-string ctx keys) "}"))))

;; Read (kbd "C-x C-s") from SXML as the custom node.
(sxml-reader-add-rule! 'kbd
  (lambda (tag attrs children recur)
    (CustomInline 'kbd null-attr (car children))))
```

### Cross-References Between Documents

```scheme
;; Collect the targets (sections, labelled tables, ...) of page A...
(define db (make-xref-db (export-xrefs (sxml->doc page-a) "A")))
(refdb-save db "site.xref")

;; ...and resolve (int-link "A#Intro") and (ref "A#tbl-costs") in page B.
(convert page-b to: 'html
         options: `((refdb . ,(load-xref-db "site.xref"))
                    (xref-suffix . ".html")))
```

## Architecture

### Pipeline

```
SXML --sxml->doc--> IR --filters--> IR --resolve-doc--> IR + resolution table
                                                             |
                                              writer (Yasos object)
                                                             |
                                              wl-pprint document --layout--> text
```

`resolve-doc` makes one pass over the document, after the filters,
and records unique anchors, section numbers, the table of contents,
reference targets, citation keys and the features the document uses
(the LaTeX writer chooses its packages from them).

### Module Structure

- **`multidoc.ir`**: The intermediate representation
  - Inline and block datatypes, attributes and styles
  - Tables: parts, rows, cells, spans, and a grid view
  - Division kinds and `shift-divisions`
  - Custom node declarations and lowering
  - Traversals: `walk-blocks`, `walk-inlines`, `query-blocks`, `query-inlines`
  - The resolution pass, `resolve-doc`

- **`multidoc.sxml`**: The SXML reader, extensible with rules for new elements

- **`multidoc.refs`**: Reference databases and citation processors
  - The reference-database protocol, and a database of cross-references kept in a file
  - `export-xrefs`, and checked links between documents
  - The citation-processor registry, with the `native` and `plain-keys` processors

- **`multidoc.backend`**: The writer protocol, registry and rendering
  - `make-writer`, `derive-writer`, `register-writer!`, `lookup-writer`
  - Custom node writers and `writer-coverage`
  - Helpers for writers: filling, verbatim lines, escapes, ASCII tables
  - `render`, `convert`, `make-render-context`, `write-output`

- **`multidoc.html`, `multidoc.latex`, `multidoc.texinfo`, `multidoc.markdown`, `multidoc.plain`, `multidoc.native`**: The writers

- **`multidoc`**: Re-exports all of the above, and registers the writers

- **`multidoc.cli`**: The `multidoc` program

### Design Principles

1. **One Reader, Many Writers**: The input vocabulary is interpreted once, by the reader; writers see only the IR
2. **Algebraic Data**: The IR is made of `datatype` values, taken apart with `cases` and `match`
3. **Total Rendering**: Every custom node has a lowering into core IR, so every writer can render every document
4. **Objects for Writers**: Writers are Yasos objects; recursion goes through the outermost writer, so overrides apply at any depth
5. **Layout, not Strings**: Writers produce wl-pprint documents, laid out at the end
6. **Immutable Values**: Filters and resolution return new documents; reference databases are values too

## API Reference

### Reading

```scheme
(sxml->doc tree #:key (top-division 'section))   ; SXML -> document
(sxml-reader-add-rule! tag rule)                 ; rule: (tag attrs children recur) -> IR
```

### Rendering

```scheme
(convert sxml #:key to width options filters top-division port)
(render doc #:key (to 'html) (width 80) (options '()))   ; -> wl-pprint document
(layout->string pp-doc width)
(make-render-context writer doc options)                 ; -> (values doc ctx)
(write-output writer doc ctx dest)                       ; dest: port, path or #f
```

### The IR

```scheme
;; Inlines
(Str s) (Space) (SoftBreak) (LineBreak) (NBSpace)
(Emph xs) (Strong xs) (Small xs) (Code attr s)
(Link attr xs target kind) (Image attr alt src) (Span attr xs)
(RawInline format s) (Cite citations xs) (Ref kind key xs)
(CustomInline tag attr payload)

;; Blocks
(Plain xs) (Para xs) (CodeBlock attr s) (RawBlock format s)
(BlockQuote blocks) (BulletList items) (OrderedList items) (DefinitionList items)
(Section division attr numbered? title body)
(Definition attr sigs body)
(Table attr caption colspecs head bodies foot)
(TableOfContents) (Bibliography attr) (HorizontalRule) (Div attr blocks) (Null)
(CustomBlock tag attr payload)

;; Documents, attributes and styles
(make-doc meta blocks)
(make-attr id classes kvs style)
(make-style name '((html (class "note")) (latex (env "quote")) (table (bold #t))))
```

### Traversals and Filters

```scheme
(walk-blocks f blocks)        ; bottom-up map; f may return a list to splice
(walk-inlines f blocks)
(query-blocks f seed blocks)  ; fold in document order
(query-inlines f seed blocks)
(shift-divisions n)           ; a filter: move sections n levels down
(resolve-doc doc)             ; -> (values doc resolution-table)
```

### Writers

```scheme
(make-writer #:key name extension doc block inline text output)
(derive-writer base #:key name doc block inline text output)
(register-writer! writer)
(lookup-writer name)
(registered-writers)
(define-custom-node tag #:key lower (block? #t) collect)
(define-node-writer tag writer-name handler)
(writer-coverage writer doc)  ; how each custom node will be rendered
```

### References and Citations

```scheme
(export-xrefs doc doc-name)          ; -> list of targets
(make-xref-db targets)
(load-xref-db path ...)
(refdb-save db path)
(register-citation-processor! (make-citation-processor name process))
```

## Output Formats and Options

Options are given as an alist, or with `-V KEY=VALUE` on the command line.

| Writer | Output | Options |
|---|---|---|
| `html` | XHTML 1.0 Strict | `standalone`, `html-top-heading`, `number-sections`, `style`, `print-style`, `canonical` |
| `latex` | LaTeX | `standalone`, `document-class`, `class-options`, `latex-packages`, `latex-preamble`, `maketitle`, `latex-citations` (`cite`, `natbib`, `biblatex`), `bibliography`, `bibliography-style` |
| `texinfo` | Texinfo, with nodes and menus | `standalone`, `setfilename`, `texinfo-headers` |
| `markdown` | pandoc Markdown | `standalone`, `markdown-top-heading` |
| `plain` | Filled text, ASCII tables | `standalone`, `number-sections` |
| `native` | The IR as S-expressions | |

All writers understand `width`, `top-division`, `ref-style`, `refdb`,
`citations` and `xref-suffix`.

## Examples

The [`examples`](examples) directory holds programs to run with
`csi -s` from that directory:

- **`convert.scm`**: A sample page converted with every writer
- **`build-ir.scm`**: A document built from IR constructors, with filters and queries
- **`styled-table.scm`**: Header and footer parts, spans and styled cells in four formats
- **`extend.scm`**: Custom nodes, node writers, a derived writer and a new writer
- **`cli.sh`**: The `multidoc` program, including a two-page site with checked links

## Limitations

- The input is SXML; there is no Markdown or LaTeX reader
- Long command-line options take their value after `=` (`--to=latex`)
- Markdown output leaves section numbering to the Markdown processor
- In Texinfo, text following a subsection belongs to that subsection's node
- Plain LaTeX `\cite` has no place for a citation prefix; natbib and biblatex keep it
- In plain text, a link's text and its URL are kept together when filling

## Dependencies

- **datatype**: Algebraic datatypes
- **matchable**: Pattern matching
- **yasos**: Object system for writers and reference databases
- **wl-pprint**: Wadler-Leijen pretty printer
- **getopt-long**: Command-line parsing (version 4.1 or later)
- **srfi-1**, **srfi-13**, **srfi-14**, **srfi-69**: Lists, strings, character sets and hash tables
- **test**: For the tests

## Related

- **scbib**: A bibliography and cross-reference database that reads and
  writes BibTeX and provides the `scbib` citation processor, with numeric,
  author-year and user-defined styles

## License

BSD 3-Clause License

## Acknowledgments

The multidoc library is inspired by:
- [pandoc](https://pandoc.org/): The intermediate representation, the table model and the citation syntax
- [Scribble](https://docs.racket-lang.org/scribble/): Styles addressed to particular writers, and the collect and resolve passes
- [docutils](https://docutils.sourceforge.io/) and [Skribilo](https://www.nongnu.org/skribilo/): Writers for individual node kinds
- [flextable](https://davidgohel.github.io/flextable/): Styled table cells selected by part, row and column
- Philip Wadler's *A Prettier Printer*: The layout of the output

The CHICKEN 4 versions of multidoc were written by Peter Bex and Ivan Raikov.
