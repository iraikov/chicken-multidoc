;;
;; multidoc - convert SXML to output in various document formats
;;
;; This aggregate module re-exports the public interface of the
;; component modules, and loads the built-in writers so that they
;; register themselves.
;;
;; Copyright (c) 2026 Ivan Raikov

(module multidoc ()

  (import scheme (chicken module)
          multidoc.ir multidoc.sxml multidoc.refs multidoc.backend
          multidoc.html multidoc.latex multidoc.texinfo
          multidoc.markdown multidoc.plain multidoc.native)

  (reexport multidoc.ir multidoc.sxml multidoc.refs multidoc.backend
            multidoc.html multidoc.latex multidoc.texinfo
            multidoc.markdown multidoc.plain multidoc.native)

)
