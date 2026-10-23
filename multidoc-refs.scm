;;
;; multidoc.refs - cross-reference databases and citation processors
;;
;; A reference database answers two questions: where is a target
;; (a section, table or other labelled element) of some document, and
;; what is the bibliography entry for a citation key.  multidoc
;; defines the protocol and a small database of cross-references kept
;; in a file of S-expressions; bibliography databases are provided by
;; other libraries.
;;
;; A citation processor rewrites the `Cite' and `Bibliography'
;; elements of a document before it is rendered.  Two processors are
;; built in: `native' leaves citations to the writer (LaTeX emits
;; \cite commands), and `plain-keys' writes each citation as its keys
;; in brackets.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.refs

  (
   ;; the reference database protocol
   reference-db? refdb-lookup-target refdb-add-targets refdb-lookup-entry
   refdb-targets refdb-save refdb-resolver

   ;; the built-in cross-reference database
   make-xref-db load-xref-db xref-target->sexp sexp->xref-target
   export-xrefs

   ;; int-link resolution
   int-links->refs

   ;; citation processors
   citation-processor? cp-name cp-process
   make-citation-processor register-citation-processor!
   lookup-citation-processor registered-citation-processors
   cited-keys
   )

  (import scheme (chicken base) (chicken format) (chicken string)
          (chicken pathname) (chicken sort)
          (srfi 1) (srfi 13) (srfi 69)
          datatype matchable yasos
          multidoc.ir)

  (include "box.scm")
  (include "stack.scm")


;; ---------------------------------------------------------------------
;; The reference database protocol
;; ---------------------------------------------------------------------

(define-predicate reference-db?)

;; [refdb-lookup-target db doc-name key] is the xref-target with the
;; given key in the named document, or #f.  The target of a document
;; as a whole has the document name as its key.
(define-operation (refdb-lookup-target db doc-name key) #f)

;; [refdb-add-targets db doc-name targets] is a database with the
;; targets of the named document added, replacing its earlier ones.
(define-operation (refdb-add-targets db doc-name targets))

;; [refdb-lookup-entry db key] is the bibliography entry for a
;; citation key, in the form the database uses, or #f.
(define-operation (refdb-lookup-entry db key) #f)

;; [refdb-targets db] is the list of all xref-targets in the database.
(define-operation (refdb-targets db) '())

;; [refdb-save db path] writes the database to a file.
(define-operation (refdb-save db path))

;; [refdb-resolver db] is a procedure (key) -> xref-target or #f, for
;; use as the external lookup of `resolve-doc'.  A key "page#key"
;; names a target of another document, and a key alone may name a
;; whole document.
(define (refdb-resolver db)
  (lambda (key)
    (let ((i (string-index key #\#)))
      (if i
          (refdb-lookup-target db (substring key 0 i) (substring key (+ i 1) (string-length key)))
          (refdb-lookup-target db key key)))))


;; ---------------------------------------------------------------------
;; The built-in cross-reference database
;; ---------------------------------------------------------------------

;; Targets are written one per line as
;;
;;   (xref (doc "page") (key "intro") (kind section) (number (1))
;;         (title "Introduction") (anchor "intro"))
;;
;; Titles are stored as plain text.  Forms other than (xref ...) are
;; ignored when a file is read, so a file may hold other records too.

(define (xref-target->sexp t)
  `(xref (doc ,(xref-target-doc t)) (key ,(xref-target-key t))
         (kind ,(xref-target-kind t)) (number ,(xref-target-number t))
         (title ,(inlines->string (xref-target-title t)))
         (anchor ,(xref-target-anchor t))))

(define (sexp->xref-target x)
  (match x
    (('xref . fields)
     (let ((field (lambda (name) (cond ((assq name fields) => cadr) (else #f)))))
       (make-xref-target (field 'doc) (field 'key) (or (field 'kind) 'any) (field 'number)
                         (string->inlines (or (field 'title) ""))
                         (field 'anchor))))
    (else #f)))

;; [make-xref-db #!optional targets] is a database holding the given
;; xref-targets.  Adding targets returns a new database.
(define (make-xref-db #!optional (targets '()))
  (let ((table (make-hash-table equal?)))
    (for-each (lambda (t)
                (hash-table-set! table (cons (xref-target-doc t) (xref-target-key t)) t))
              targets)
    (operations ()
      ((reference-db? self) #t)
      ((refdb-lookup-target self doc-name key)
       (hash-table-ref/default table (cons doc-name key) #f))
      ((refdb-add-targets self doc-name new)
       (make-xref-db
        (append (remove (lambda (t) (equal? (xref-target-doc t) doc-name))
                        (hash-table-values table))
                new)))
      ((refdb-targets self) (hash-table-values table))
      ((refdb-save self path)
       (with-output-to-file path
         (lambda ()
           (for-each (lambda (t) (write (xref-target->sexp t)) (newline))
                     (sort-targets (hash-table-values table)))))))))

(define (sort-targets targets)
  (let ((name (lambda (t) (string-append (or (xref-target-doc t) "") "#" (xref-target-key t)))))
    (sort targets (lambda (a b) (string<? (name a) (name b))))))

;; [load-xref-db path ...] reads the (xref ...) records of the given
;; files into one database.
(define (load-xref-db . paths)
  (make-xref-db
   (append-map (lambda (path)
                 (with-input-from-file path
                   (lambda ()
                     (let loop ((acc (make-stack)))
                       (let ((x (read)))
                         (if (eof-object? x)
                             (reverse (stack->list acc))
                             (let ((t (sexp->xref-target x)))
                               (loop (if t (stack-push! acc t) acc)))))))))
               paths)))

;; [export-xrefs d doc-name] is the list of targets of document d,
;; with doc-name as their document, together with a target for the
;; document as a whole.
(define (export-xrefs d doc-name)
  (receive (d r) (resolve-doc d quiet: #t)
    (cons (make-xref-target doc-name doc-name 'page #f
                            (string->inlines (doc-meta-ref d 'title doc-name))
                            #f)
          (map (lambda (t)
                 (make-xref-target doc-name (xref-target-key t) (xref-target-kind t)
                                   (xref-target-number t) (xref-target-title t)
                                   (xref-target-anchor t)))
               (hash-table-values (resolved-targets r))))))


;; ---------------------------------------------------------------------
;; int-link resolution
;; ---------------------------------------------------------------------

;; [int-links->refs d] replaces the internal links read from
;; (int-link ...) elements with references, so that they are checked
;; against the targets of this document and of a reference database.
;; A link to "Page#section" becomes a reference to key "Page#section",
;; a link to "Page" a reference to the page, and a link to "#section"
;; a reference to a target of this document.  A link written without
;; text of its own gets the generated text of its target.
(define (int-links->refs d)
  (make-doc
   (doc-meta d)
   (walk-inlines
    (lambda (x)
      (cases inline x
        (Link (a xs target kind)
          (if (and (eq? kind 'internal) (attr-kv a "int-link"))
              (let ((key (if (string-prefix? "#" target)
                             (substring target 1 (string-length target))
                             target))
                    (text (if (equal? (inlines->string xs) (attr-kv a "int-link")) '() xs)))
                (Ref (if (string-index target #\#) 'section 'page) key text))
              x))
        (else x)))
    (doc-blocks d))))


;; ---------------------------------------------------------------------
;; Citation processors
;; ---------------------------------------------------------------------

(define-predicate citation-processor?)

(define-operation (cp-name cp))

;; [cp-process cp d refdb options] is document d with its citations
;; and bibliography processed, using the reference database refdb
;; (or #f) and the rendering options.
(define-operation (cp-process cp d refdb options))

;; [make-citation-processor name process] builds a processor from a
;; procedure (process d refdb options) -> document.
(define (make-citation-processor name process)
  (operations ()
    ((citation-processor? self) #t)
    ((cp-name self) name)
    ((cp-process self d refdb options) (process d refdb options))))

(define processors (make-hash-table eq?))

(define (register-citation-processor! cp)
  (hash-table-set! processors (cp-name cp) cp)
  cp)

;; [lookup-citation-processor name] is the processor registered under
;; name (a symbol or a string).
(define (lookup-citation-processor name)
  (let ((key (if (string? name) (string->symbol name) name)))
    (or (hash-table-ref/default processors key #f)
        (error 'lookup-citation-processor "no citation processor registered under this name" name))))

(define (registered-citation-processors)
  (hash-table-keys processors))

;; [cited-keys d] is the list of citation keys of document d, in the
;; order they are first cited.
(define (cited-keys d)
  (reverse
   (stack->list
    (query-inlines (lambda (x acc)
                     (cases inline x
                       (Cite (cs xs)
                         (for-each (lambda (c)
                                     (unless (member (citation-key c) (stack->list acc))
                                       (stack-push! acc (citation-key c))))
                                   cs)
                         acc)
                       (else acc)))
                   (make-stack) (doc-blocks d)))))

;; The native processor leaves citations and the bibliography to the
;; writer.
(register-citation-processor!
 (make-citation-processor 'native (lambda (d refdb options) d)))

;; The plain-keys processor writes each citation as its keys in
;; brackets, and the bibliography as the list of cited keys.
(register-citation-processor!
 (make-citation-processor
  'plain-keys
  (lambda (d refdb options)
    (let ((keys (cited-keys d)))
      (make-doc
       (doc-meta d)
       (walk-blocks
        (lambda (b)
          (cases block b
            (Bibliography (a)
              (Div (attr-with-id a (or (attr-id a) "refs"))
                   (map (lambda (k) (Para (list (Str (string-append "[" k "]"))))) keys)))
            (else b)))
        (walk-inlines
         (lambda (x)
           (cases inline x
             (Cite (cs xs)
               (Cite cs (list (Str (string-append "["
                                                  (string-intersperse (map citation-key cs) "; ")
                                                  "]")))))
             (else x)))
         (doc-blocks d))))))))

)
