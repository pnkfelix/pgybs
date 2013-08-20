(define parsegen-directory (make-parameter "ParseGen/"))
(define lexgen-directory (make-parameter "LexGen/"))
(define host-implementation (make-parameter 'MacScheme))
(define parsegen-targets (make-parameter '(java scheme)))

(load (string-append (parsegen-directory) "loadparsegen.sch"))
(load (string-append (lexgen-directory) "loadlexgen.sch"))

;; Example descriptions
(define ab-outofline '((a #\a) (b #\b) (ab (!   a   b))))
(define ab-inlined   '((a #\a) (b #\b) (ab (! #\a #\b))))

;; Example invocations
;; (generate-scheme-lexer ab-outofline)
;; (generate-scheme-lexer ab-inlined)
;; (generate-java-lexer   ab-outofline)
