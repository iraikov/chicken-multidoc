;;
;; A table with header and footer parts, spanning cells and styles.
;;
;; The table model follows pandoc: a header part, body parts and a
;; footer part, each made of rows of cells, where a cell may span
;; several rows and columns.  Styles carry properties for the writers;
;; the properties under the key table are understood by all of them,
;; each as far as its format allows.  The program prints the table as
;; plain text, Markdown, HTML and LaTeX, and lists the cells of one
;; column with table-cells.
;;
;;   csi -s styled-table.scm
;;

(import scheme (chicken base) (chicken format)
        (srfi 1)
        datatype
        multidoc)

(define (style name . props) (make-attr #f '() '() (make-style name `((table ,@props)))))

(define heading (style "heading" '(bold #t) '(bg "#dddddd") '(border-bottom 1)))
(define total   (style "total" '(bold #t) '(border-top 1)))
(define warm    (style "warm" '(bg "#ffe0c0")))

;; [cell text #!key attr align rows cols] is a cell holding one line
;; of text.
(define (cell s #!key (attr null-attr) (align 'default) (rows 1) (cols 1))
  (make-table-cell attr align rows cols (list (Plain (string->inlines s)))))

(define (row . cells) (make-table-row null-attr cells))

(define rainfall
  (Table (make-attr "tbl-rain" '() '() (make-style "grid" '((table (border 1)))))
         (string->inlines "Rainfall in millimetres")
         (list (make-colspec 'left #f) (make-colspec 'right #f)
               (make-colspec 'right #f) (make-colspec 'right #f))
         ;; header: "City" spans two header rows; "Season" spans two columns
         (make-table-part null-attr
                          (list (row (cell "City" attr: heading rows: 2)
                                     (cell "Season" attr: heading align: 'center cols: 2)
                                     (cell "Year" attr: heading rows: 2))
                                (row (cell "Wet" attr: heading) (cell "Dry" attr: heading))))
         (list (make-table-part null-attr
                                (list (row (cell "Lisbon") (cell "520") (cell "205") (cell "725"))
                                      (row (cell "Seville") (cell "430" attr: warm) (cell "105") (cell "535"))
                                      (row (cell "Porto") (cell "1010") (cell "240") (cell "1250")))))
         (make-table-part null-attr
                          (list (row (cell "Mean" attr: total) (cell "653" attr: total)
                                     (cell "183" attr: total) (cell "837" attr: total))))))

(define doc
  (make-doc '((title . "Rainfall"))
            (list (Para (append (string->inlines "Mean yearly rainfall is given in ")
                                (list (Ref 'table "tbl-rain" '()) (Str "."))))
                  rainfall)))

;; A reference without text of its own is labelled with the target's
;; title, or with its kind and number when ref-style is number.
(for-each (lambda (format)
            (printf "--- ~a~%" format)
            (display (layout->string (render doc to: format width: 72
                                             options: '((standalone . #f) (ref-style . number)))
                                     72))
            (newline))
          '(plain markdown html latex))

;; The inlines of a Plain or Para block.
(define (block-inlines b)
  (cases block b
    (Plain (xs) xs)
    (Para (xs) xs)
    (else '())))

;; table-cells selects cells by part, row and grid column, counting
;; from 0.  Columns count spans: "Year" is the third cell of its row but
;; is in column 3, because "Season" covers columns 1 and 2.
(printf "--- the cells of column 3~%")
(for-each (lambda (r)
            (printf "~a row ~a: ~a~%"
                    (cell-ref-part r) (cell-ref-row r)
                    (inlines->string
                     (append-map block-inlines (table-cell-blocks (cell-ref-cell r))))))
          (table-cells rainfall col: 3))
