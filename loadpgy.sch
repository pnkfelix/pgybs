(trace load)
(load "loadall.sch")

(load "pgy-tokens.sch")
(generate-scheme-lexer pgy-tokens "generated-lex-pgy.sch")
(generate-scheme "pgy.pg" "generated-parse-pgy.sch" "generated-tables.sch")

(load "lex-pgy-prefix.sch")
(load "generated-lex-pgy.sch")

(load "parse-pgy-prefix.sch")
(load "generated-parse-pgy.sch")
(load "parse-pgy-suffix.sch")

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

(define (pgy-test test-arg)
  (call-with-input-string
   test-arg
   (lambda (p)
     (parameterize ((current-input-port p))
       (let ((accum (list '())))
         (guard (x ((scannerError? x)
                    (let ((x (reverse (car accum))))
                      (resetAccumulator)
                      (values 'test-failed x))))
                #;(scan-all accum)
                (parse-input)
                ))))))
