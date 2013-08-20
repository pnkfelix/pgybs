;; VERSION 1: Token Accumulated in a string
#;
(define-values (scanChar consumeChar resetAccumulator accumulatedToken
                         scannerError
                         errIllegalChar errLexGenBug errIncompleteToken)
  (let ((accum (make-string 128 #\nul))
        (accum-next 0))
    (define (push! c)
      (if (>= accum-next (string-length accum))
          (let ((s accum))
            (set! accum (make-string (* 2 (string-length s)) #\nul))
            (let loop ((i 0))
              (cond ((< i (string-length s))
                     (string-set! accum i (string-ref s i))
                     (loop (+ i 1)))))))
      (string-set! accum accum-next c)
      (set! accum-next (+ 1 accum-next)))

    (define (scanChar)    (peek-char (current-input-port)))
    (define (consumeChar) (push! (read-char (current-input-port))))
    (define (resetAccumulator) (set! accum-next 0))
    (define (accumulatedToken) (substring accum 0 accum-next))

    (define (scannerError x) (error 'scanner x))

    (define errIllegalChar     "illegal char")
    (define errLexGenBug       "lexgen internal error")
    (define errIncompleteToken "incomplete token")

    (values scanChar consumeChar resetAccumulator accumulatedToken
            scannerError
            errIllegalChar errLexGenBug errIncompleteToken)))

(define-values (scanChar consumeChar resetAccumulator accumulatedToken
                         scannerError scannerError?
                         errIllegalChar errLexGenBug errIncompleteToken)
  (let ((accum '()))
    (define (push! c) (set! accum (cons c accum)) c)
    (define (scanChar)    (peek-char (current-input-port)))
    (define (consumeChar) (push! (read-char (current-input-port))))
    (define (resetAccumulator) (set! accum '()))
    (define (accumulatedToken) (list->string (reverse accum)))

    ;; Guard syntax: (guard (x ((scannerError? x) <handler-result>)) <inner>)
    (define (scannerError x) (raise x))

    (define (scannerError? x)
      (case x
        ((err-illegal-char err-lexgen-bug err-incomplete-token) #t)
        (else #f)))

    (define errIllegalChar     'err-illegal-char)
    (define errLexGenBug       'err-lexgen-bug)
    (define errIncompleteToken 'err-incomplete-token)

    (values scanChar consumeChar resetAccumulator accumulatedToken
            scannerError scannerError?
            errIllegalChar errLexGenBug errIncompleteToken)))
