;;
;; Converting a page to every output format.
;;
;; Reads guide.sxml and writes guide.html, guide.tex, guide.texi,
;; guide.md, guide.txt and guide.native, one for each registered
;; writer, then prints the plain text version at a narrow width.
;;
;;   csi -s convert.scm
;;

(import scheme (chicken base) (chicken format)
        multidoc)

(define page (with-input-from-file "guide.sxml" read))

;; convert reads the SXML, resolves references and writes the result.
;; The options are read by the writers; number-sections is understood
;; by most of them.
(for-each
 (lambda (name)
   (let* ((w (lookup-writer name))
          (file (string-append "guide." (writer-extension w))))
     (with-output-to-file file
       (lambda ()
         (convert page to: name width: 72 options: '((number-sections . #t)))))
     (printf "wrote ~a~%" file)))
 (registered-writers))

;; A document can also be read once and rendered many times.  render
;; returns a wl-pprint document; layout->string lays it out at a given
;; width.
(define doc (sxml->doc page))

(newline)
(display (layout->string (render doc to: 'plain width: 50) 50))
(newline)

;; Writer options go in the options alist.  With standalone set to
;; #f, the HTML writer makes a fragment without the page header.
(newline)
(display (layout->string (render (sxml->doc '(body (p "A " (em "small") " fragment.")))
                                 to: 'html options: '((standalone . #f)))
                         72))
(newline)
