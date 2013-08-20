(trace load)
(load "loadall.sch")

(load "pgy-tokens.sch")
(define (isSkippableWhitespaceChar c)
  (and (char-whitespace? c) (not (char=? c #\newline))))

(generate-scheme-lexer pgy-tokens "generated-lex-pgy.sch")
(generate-scheme "parsegen-y.pgy"
                 "generated-parse-pgy.sch" "generated-tables.sch")

(load "lex-pgy-prefix.sch")
(load "generated-lex-pgy.sch")

(load "parse-pgy-prefix.sch")
(load "generated-parse-pgy.sch")
(load "parse-pgy-suffix.sch")

; (for-each trace (list accept next-token consume-token!))

(define (scan-all accum-cell)
  (define (push! x tok) (set-car! accum-cell
                                  (cons (cons x tok) (car accum-cell))))
  (let loop ()
    (let ((x (scanner0)))
      (case x
        ((eof) (reverse (car accum-cell)))
        (else
         (let ((tok (accumulatedToken)))
           (resetAccumulator)
           (push! x tok)
           (loop)))))))

(define (pgy-test test-arg test-thunk)
  (call-with-input-string
   test-arg
   (lambda (p)
     (parameterize ((current-input-port p))
       (let ((accum (list '())))
         (guard (x ((scannerError? x)
                    (let ((x (reverse (car accum))))
                      (resetAccumulator)
                      (list 'test-failed x))))
                ;;(scan-all accum)
                ;;(parse-input)
                (test-thunk)
                ))))))

(pgy-test "Hm *terminals id goesto *productions boo ::= goesto id #list\n\n*end"
          (lambda () (consume-token!) (parse-input)))
