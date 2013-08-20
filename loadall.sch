(define parsegen-directory (make-parameter "ParseGen/"))
(define lexgen-directory (make-parameter "LexGen/"))
(define host-implementation (make-parameter 'MacScheme))
(define parsegen-targets (make-parameter '(java scheme)))

(load (string-append (parsegen-directory) "loadparsegen.sch"))
(load (string-append (lexgen-directory) "loadlexgen.sch"))
