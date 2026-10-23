;;
;; multidoc.cli - the multidoc command-line program
;;
;;   multidoc [OPTION ...] [FILE.sxml ...]
;;
;; Converts SXML documents with a registered writer.  With no files,
;; the document is read from standard input.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc.cli

  (multidoc-main multidoc-usage)

  (import scheme (only (scheme base) exact-integer?)
          (chicken base) (chicken format) (chicken string)
          (chicken pathname) (chicken condition) (chicken port)
          (srfi 1) (srfi 13) (only (srfi 14) char-set:full)
          getopt-long
          multidoc)


;; [parse-value s] reads an option value given on the command line.
(define (parse-value s)
  (cond ((member s '("#t" "true" "yes")) #t)
        ((member s '("#f" "false" "no")) #f)
        ((string->number s) => identity)
        ((string-prefix? "(" s) (with-input-from-string s read))
        (else s)))

(define (positive-integer-string? s)
  (let ((n (string->number s))) (and n (exact-integer? n) (> n 0))))

(define (integer-string? s)
  (let ((n (string->number s))) (and n (exact-integer? n))))

(define (key=value? s)
  (let ((i (string-index s #\=))) (and i (> i 0))))

;; [parse-option s] turns "KEY=VALUE" into (KEY . VALUE).
(define (parse-option s)
  (let ((i (string-index s #\=)))
    (cons (string->symbol (substring s 0 i))
          (parse-value (substring s (+ i 1) (string-length s))))))

;; The command-line grammar, in the form read by getopt-long.
(define multidoc-grammar
  `((to "output format (default html); see --list-formats"
        (single-char #\t) (value (required FORMAT)))
    (width "line width for filling text (default 80)"
           (single-char #\w)
           (value (required N) (predicate ,positive-integer-string?) (transformer ,string->number)))
    (output "write to PATH instead of standard output (-)"
            (single-char #\o) (value (required PATH)))
    (auto-ext "write each FILE.sxml to FILE.<extension>")
    (fragment "omit headers and wrappers (no standalone document)")
    (top-division "division kind of wiki level 2 (default section)"
                  (value (required KIND) (transformer ,string->symbol)))
    (shift-divisions "move all sections N levels down (up if negative)"
                     (value (required N) (predicate ,integer-string?) (transformer ,string->number)))
    (number-sections "number section headings, where the format allows")
    (citations "citation processor (default native)"
               (value (required NAME) (transformer ,string->symbol)))
    (xref-in "read cross-references of other documents (repeatable)"
             (multiple #t) (value (required FILE)))
    (xref-out "write the cross-references of the input documents"
              (value (required FILE)))
    (doc-name "document name for cross-references of standard input"
              (value (required NAME)))
    (option "set a writer option (repeatable)"
            (single-char #\V) (multiple #t)
            (value (required KEY=VALUE) (predicate ,key=value?) (transformer ,parse-option)))
    (check "report how the writer renders custom nodes")
    (list-formats "list the registered output formats")
    (help "show this help" (single-char #\h))))

(define multidoc-usage
  (string-append
   "Usage: multidoc [OPTION ...] [FILE.sxml ...]\n\n"
   "Convert SXML documents.  With no FILE, read standard input.\n"
   "A long option takes its value after =, as in --to=latex.\n\n"
   (parameterize ((width 26)) (usage multidoc-grammar))
   "\nA writer option VALUE is read as a number, #t, #f, a list, or else a string."))

;; [parse-args args] is (values opt files): opt is a procedure from an
;; option name to its value (#f if not given, a list for options that
;; may be given several times, newest first), and files are the other
;; arguments.  It signals an error for bad usage.
(define (parse-args args)
  (let* ((opts (getopt-long args
                            multidoc-grammar
                            long-option-value-cset: char-set:full
                            unknown-option-handler:
                            (lambda (names) (apply usage-error "unknown option" names))))
         (opt (make-option-dispatch opts multidoc-grammar)))
    (values opt (opt '@))))

;; [setting opt key #!optional default] is the value of an option, or
;; default when it was not given.
(define (setting opt key #!optional default)
  (let ((v (opt key)))
    (if (or (not v) (null? v)) default v)))

;; [output-path opt] is the path given with -o, or #f for standard
;; output, which is also what "-o -" asks for.
(define (output-path opt)
  (let ((path (setting opt 'output #f)))
    (and path (not (equal? path "-")) path)))

;; [doc-name-of opt file] is the name under which the targets of
;; an input document are recorded.
(define (doc-name-of opt file)
  (if (equal? file "-")
      (setting opt 'doc-name "stdin")
      (pathname-file file)))

(define (read-input file)
  (if (equal? file "-")
      (read)
      (with-input-from-file file read)))

;; [report-coverage w file d] prints, for each custom node tag of
;; document d, how writer w renders it.
(define (report-coverage w file d)
  (let ((coverage (writer-coverage w d)))
    (if (null? coverage)
        (printf "~a: no custom nodes~%" file)
        (for-each (lambda (entry)
                    (printf "~a: ~a: ~a~%" file (car entry)
                            (let ((how (cdr entry)))
                              (if (pair? how) (sprintf "inherited from ~a" (cdr how)) how))))
                  coverage))))

;; [report-error e] prints the message of condition e on one line.
(define (report-error e)
  (let ((message   (and (condition? e) (get-condition-property e 'exn 'message #f)))
        (arguments (and (condition? e) (get-condition-property e 'exn 'arguments '()))))
    (fprintf (current-error-port) "multidoc: ~a~a~%"
             (or message e)
             (if (null? arguments)
                 ""
                 (string-append ": " (string-intersperse
                                      (map (lambda (x) (if (string? x) x (->string x))) arguments)
                                      " "))))))

(define (usage-error message . arguments)
  (signal (make-composite-condition
           (make-property-condition 'exn 'message message 'arguments arguments)
           (make-property-condition 'usage))))

;; [multidoc-main args] runs the program on a list of command-line
;; arguments and returns the exit status: 0 on success, 1 if a
;; document could not be converted, 2 for bad usage.
(define (multidoc-main args)
  (handle-exceptions e
      (begin
        (report-error e)
        (if (and (condition? e) ((condition-predicate 'usage) e)) 2 1))
    (receive (opt files)
        (handle-exceptions e
            (signal (make-composite-condition e (make-property-condition 'usage)))
          (parse-args args))
      (cond
       ((setting opt 'help)
        (display multidoc-usage) (newline) 0)
       ((setting opt 'list-formats)
        (for-each (lambda (name)
                    (printf "~a~a.~a~%" name
                            (make-string (max 1 (- 12 (string-length (symbol->string name)))) #\space)
                            (writer-extension (lookup-writer name))))
                  (registered-writers))
        0)
       (else
        (let* ((w        (let ((name (string->symbol (setting opt 'to "html"))))
                           (if (memq name (registered-writers))
                               (lookup-writer name)
                               (usage-error "unknown format; see --list-formats" name))))
               (files    (if (null? files) '("-") files))
               (output   (output-path opt))
               (auto-ext (setting opt 'auto-ext #f))
               (top      (setting opt 'top-division 'section))
               (shift    (setting opt 'shift-divisions 0))
               ;; Options given with -V come first, newest first, so
               ;; that they override the defaults and each other.
               (options  (append (setting opt 'option '())
                                 (list (cons 'standalone (not (setting opt 'fragment #f)))
                                       (cons 'number-sections (setting opt 'number-sections #f))))))
          (cond
           ((and output (> (length files) 1))
            (usage-error "-o cannot be used with several input files; use --auto-ext"))
           ((and (not (eq? (writer-output-kind w) 'text)) (not output) (not auto-ext)
                 (not (setting opt 'check #f)))
            (usage-error (sprintf "the ~a format writes ~a output and needs -o or --auto-ext"
                                  (writer-name w) (writer-output-kind w))))
           ((setting opt 'check #f)
            (for-each (lambda (file)
                        (report-coverage w file (sxml->doc (read-input file) top-division: top)))
                      files)
            0)
           (else
            ;; With cross-references, the targets of all input
            ;; documents are collected first, so that links between
            ;; them resolve in a single run.
            (let* ((sxmls    (map read-input files))
                   (filters  (if (= shift 0) '() (list (shift-divisions shift))))
                   (docs     (map (lambda (sxml)
                                    (fold (lambda (f d) (f d)) (sxml->doc sxml top-division: top) filters))
                                  sxmls))
                   (xref-in  (setting opt 'xref-in '()))
                   (xref-out (setting opt 'xref-out #f))
                   (own      (and (or xref-out (pair? xref-in))
                                  (append-map (lambda (d file) (export-xrefs d (doc-name-of opt file)))
                                              docs files)))
                   (refdb    (and own (make-xref-db (append (if (null? xref-in) '()
                                                                (refdb-targets (apply load-xref-db (reverse xref-in))))
                                                            own))))
                   (options  (append options
                                     (if refdb (list (cons 'refdb refdb)) '())
                                     (let ((cp (setting opt 'citations #f)))
                                       (if cp (list (cons 'citations cp)) '())))))
              (for-each
               (lambda (d file)
                 (receive (d c) (make-render-context
                                 w d (append (list (cons 'width (setting opt 'width 80))
                                                   (cons 'top-division top)
                                                   (cons 'doc-name (doc-name-of opt file)))
                                             options))
                   (write-output w d c (cond ((and auto-ext (not (equal? file "-")))
                                              (pathname-replace-extension file (writer-extension w)))
                                             (output output)
                                             (else #f)))))
               docs files)
              (when xref-out
                (refdb-save (make-xref-db own) xref-out))
              0)))))))))

)
