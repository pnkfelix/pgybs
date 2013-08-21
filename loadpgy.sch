(load "stashenv.sch")

(define-snapshot-environment env:0)

(trace load)
(load "loadall.sch")

(define-snapshot-environment env:parsegen-loaded)

(load "pgy-tokens.sch")

(generate-scheme-lexer pgy-tokens "generated-lex-pgy.sch")
(generate-scheme "parsegen-y.pgy"
                 "generated-parse-pgy.sch" "generated-tables.sch")


(swap-environment! env:0 'env:pgy-generation)

(define (isSkippableWhitespaceChar c)
  (and (char-whitespace? c) (not (char=? c #\newline))))

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

(define (resetScannerAndParser)
  (resetAccumulator)
  (consume-token!))

(define (pgy-test test-arg test-thunk)
  (resetScannerAndParser)
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

(pgy-test "Hm *terminals term1 term2 *productions boo ::= goesto id #list\n\n*end"
          parse-input)

(pgy-test "123" parse-number)
(pgy-test "hithere" parse-id)
(pgy-test "foo" (lambda () (parse-actionproc)))
(pgy-test "this then #foo whoa #andthen yep\n" (lambda () (parse-rhs)))
