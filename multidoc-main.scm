;;
;; The multidoc command-line program; see multidoc.cli.
;;

(import scheme (chicken base) (chicken process-context) multidoc.cli)

(exit (multidoc-main (command-line-arguments)))
