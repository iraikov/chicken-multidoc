;;
;; Extending multidoc: new SXML elements, custom nodes, node writers,
;; a derived writer and a new writer.
;;
;; - Reader rules turn the new elements (note ...) and (kbd ...) into
;;   custom nodes.
;; - Each custom node has a lowering into core IR, so that every writer
;;   can render it, and node writers give it a native form in HTML and
;;   Texinfo.
;; - boxed-html is the HTML writer with definitions drawn in boxes.
;; - outline is a new writer that prints only the section titles.
;;
;;   csi -s extend.scm
;;

(import scheme (chicken base) (chicken format) (chicken string)
        (srfi 1)
        datatype
        (prefix wl-pprint pp:)
        multidoc)

(define page
  '(body
    (section 2 "Editing"
     (p "Save the file with " (kbd "C-x C-s") ".")
     (note (p "Unsaved buffers are marked with " (tt "**") ".")))
    (section 2 "Commands"
     (def (sig (procedure "(save-buffer)"))
          (p "Saves the current buffer."))
     (section 3 "Keys"
      (p "Press " (kbd "C-h k") " to describe a key.")))))


;; ---------------------------------------------------------------------
;; Custom nodes
;; ---------------------------------------------------------------------

;; A note is a block whose payload is its content, a list of blocks.
;; Its lowering is a Div that starts with "Note:".
(define-custom-node 'note
  lower: (lambda (attr blocks)
           (list (Div (attr-add-class attr "note")
                      (cons (Para (list (Strong (list (Str "Note:")))))
                            blocks)))))

;; A key is an inline node whose payload is the key's name.  It lowers
;; to code.
(define-custom-node 'kbd
  block?: #f
  lower: (lambda (attr keys) (list (Code null-attr keys))))

;; Reader rules make the custom nodes from SXML.  recur reads nested
;; content; 'blocks makes paragraphs of runs of text.
(sxml-reader-add-rule! 'note
  (lambda (tag attrs children recur)
    (CustomBlock 'note null-attr (recur children 'blocks))))

(sxml-reader-add-rule! 'kbd
  (lambda (tag attrs children recur)
    (CustomInline 'kbd null-attr (string-intersperse (filter string? children) " "))))


;; ---------------------------------------------------------------------
;; Node writers
;; ---------------------------------------------------------------------

;; A node writer returns a wl-pprint document.  render-blocks renders
;; nested IR with the writer of the context.
(define-node-writer 'note 'html
  (lambda (attr blocks ctx)
    (vcat (append (list (pp:text "<div class=\"note\">"))
                  (render-blocks ctx blocks)
                  (list (pp:text "</div>"))))))

(define-node-writer 'kbd 'html
  (lambda (attr keys ctx)
    (pp:text (string-append "<kbd>" (render-string ctx keys) "</kbd>"))))

(define-node-writer 'kbd 'texinfo
  (lambda (attr keys ctx)
    (pp:text (string-append "@kbd{" (render-string ctx keys) "}"))))


;; ---------------------------------------------------------------------
;; A derived writer
;; ---------------------------------------------------------------------

;; derive-writer overrides some methods of a writer.  Each override
;; gets the base method as next.  Nested content goes through the
;; derived writer, and the derived writer inherits the node writers of
;; its base, so notes and keys keep their HTML form.
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


;; ---------------------------------------------------------------------
;; A new writer
;; ---------------------------------------------------------------------

;; make-writer takes procedures for the whole document, for blocks and
;; for inlines.  Each receives the writer itself first.  This writer
;; prints the section titles, indented by depth, and nothing else;
;; render-blocks leaves out the empty documents of the other blocks.
(define outline-writer
  (make-writer
   name: 'outline
   extension: "outline"
   doc: (lambda (self d ctx)
          (vcat (render-blocks ctx (doc-blocks d))))
   block: (lambda (self b ctx)
            (cases block b
              (Section (division a numbered title body)
                (vcat (cons (pp:text (string-append "- " (inlines->string title)))
                            (let ((inner (render-blocks ctx body)))
                              (if (null? inner) '() (list (pp:indent 2 (vcat inner))))))))
              (else pp:empty)))
   inline: (lambda (self x ctx) (pp:text (inlines->string (list x))))))

(register-writer! outline-writer)


;; ---------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------

(define doc (sxml->doc page))

(define (show writer)
  (printf "--- ~a~%" writer)
  (display (layout->string (render doc to: writer width: 72 options: '((standalone . #f))) 72))
  (newline))

(for-each show '(boxed-html plain texinfo outline))

;; writer-coverage tells how a writer renders each custom node: with
;; its own node writer, one inherited from its base, or the lowering.
(printf "--- coverage~%")
(for-each (lambda (w)
            (printf "~a: ~s~%" w (writer-coverage (lookup-writer w) doc)))
          '(html boxed-html texinfo markdown))
