(define parsegen-directory (make-parameter "ParseGen/"))
(define lexgen-directory (make-parameter "LexGen/"))
(define host-implementation (make-parameter 'MacScheme))
(define parsegen-targets (make-parameter '(java c scheme)))

(load (string-append (parsegen-directory) "loadparsegen.sch"))
(load (string-append (lexgen-directory) "loadlexgen.sch"))

;; Example descriptions
(define ab-outofline '((a #\a) (b #\b) (ab (!   a   b))))
(define ab-inlined   '((a #\a) (b #\b) (ab (! #\a #\b))))

;; Example invocations
;; (generate-scheme-lexer ab-outofline)
;; (generate-scheme-lexer ab-inlined "made-inlined.sch")
;; (generate-java-lexer   ab-outofline "made-outofline.java")

(define (wrap-scanner0 scanner0-defn)
  (define accum '())
  (define (push! c) (set! accum (cons c accum)) (unspecified))
  (let ((link-scanner0
         (eval `(lambda (consumeChar scanChar accept
                         resetAccumulator
                         scannerError
                         errIllegalChar errLexGenBug errIncompleteToken)
                  (let ()
                    ,scanner0-defn
                    scanner0))))
        ;; -> unspecified
        (consumeChar (lambda () (push! (read-char (current-input-port)))))
        ;; -> char or eof-object
        (scanChar (lambda () (peek-char (current-input-port))))
        ;; Symbol -> Result
        (accept (lambda (token)
                  (let ((result (list->string (reverse accum))))
                    (set! accum '())
                    (cons token result))))

        (resetAccumulator (lambda () (set! accum '())))
        (scannerError (lambda (x) (error 'scanner0 x)))
        (errIllegalChar     "errIllegalChar")
        (errLexGenBug       "errLexGenBug")
        (errIncompleteToken "errIncompleteToken"))
    (link-scanner0 consumeChar scanChar accept
                   resetAccumulator scannerError
                   errIllegalChar errLexGenBug errIncompleteToken)))
