#|

; Lexical analyzer, parser, and action procedures for inputs to ParseGen.

(define (string-downcase s)
  (list->string
   (map char-downcase (string->list s))))

; Main entry points.

(define (make-generator thunk)
  (lambda (file0 . rest)
    (let ((grammar (parse-file file0))
          (generator (thunk)))
      (cond ((null? rest)
             (generator grammar))
            ((null? (cdr rest))
             (call-with-output-file
              (car rest)
              (lambda (p1)
                (generator grammar p1))))
            (else
             (call-with-output-file
              (car rest)
              (lambda (p1)
                (call-with-output-file
                 (cadr rest)
                 (lambda (p2)
                   (generator grammar p1 p2))))))))))

(define generate-c       (make-generator (lambda () generate-c-parser)))
(define generate-java    (make-generator (lambda () generate-java-parser)))
(define generate-modula3 (make-generator (lambda () generate-modula3-parser)))
(define generate-pascal  (make-generator (lambda () generate-pascal-parser)))
(define generate-scheme  (make-generator (lambda () generate-scheme-parser)))

; Lexical analyzer.
; See Appendix C of Fischer and LeBlanc for the lexical syntax of LLGen.
; Restriction:  The "..." feature of LLGen is not yet supported here.

(define (init-all)
  (init-scanner)
  (init-parse-error))

(define (parse-file filename)
  (call-with-input-file filename parse-port))

(define (parse-port p)
  (init-all)
  (set! parser-input p)
  (let ((ast (parse-input)))
    (if (positive? total-errors)
        (mk-error)
        ast)))

; LEXICAL ANALYZER.  Public entry points.

(define (consume-token!)
  (set! next-token-is-ready #f))

(define (next-token)
  (if next-token-is-ready
      kind-of-next-token
      (begin (set! size-of-next-token 0)
             (scanner0))))

; These global variables are used to communicate with
; make-identifier and make-number,
; and should otherwise be considered private.

(define token-value 'a)

; These global variables are used to communicate with parse-error,
; and should otherwise be considered private.

(define line-number 1)
(define line-number-of-last-error 0)

; Private to the lexical analyzer.

(define parser-input (current-input-port))

; Identifiers and numerals are limited to maxtokensize characters, sorry.
; Overflow is indeed caught.

(define maxtokensize 1024)
(define maxtokensize-1 (- maxtokensize 1))
(define eoftoken 'eof)

(define kind-of-next-token eoftoken)
(define text-of-next-token (make-string maxtokensize))
(define size-of-next-token 0)
(define next-token-is-ready #f)

; This must be called before next-token is called.

(define (init-scanner)
  (set! line-number 1)
  (set! line-number-of-last-error 0)
  (set! kind-of-next-token eoftoken)
  (set! size-of-next-token 0)
  (set! next-token-is-ready #f)
  (set! last-lhs #f))

(define (scan-char)
  (peek-char parser-input))

(define (consume-char)
  (if (< size-of-next-token maxtokensize-1)
      (let ((c (read-char parser-input)))
        (if (not (eof-object? c))
            (begin (if (char=? c #\newline)
                       (set! line-number (+ line-number 1)))
                   (string-set! text-of-next-token size-of-next-token c)
                   (set! size-of-next-token (+ size-of-next-token 1))))
        c)
      (scanner-error "amazingly long token" text-of-next-token)))

(define (accept kind)
  (set! kind-of-next-token kind)
  (set! next-token-is-ready #t)
  kind)

(define (accept-constant kind)
  (set! token-value
        (string->symbol
         (string-downcase
          (substring text-of-next-token 0 size-of-next-token))))
  (accept kind))

(define (accept-id)
  (let* ((s (substring text-of-next-token 0 size-of-next-token))
         (name (string->symbol (string-downcase s)))
         (probe (assq name reserved-words)))
    (if probe
        (accept (cdr probe))
        (begin (set! token-value (string->symbol s))
               (accept 'id)))))

(define reserved-words
  '((*end . end)
    (*productions . productions)
    (*terminals . terminals)))

; Initial state for lexical analyzer's state machine.

(define (scanner0)
  (let ((c (consume-char)))
    (cond ((eof-object? c)      (accept eoftoken))
          ((char=? c #\newline) (accept 'newline))
          ((char-whitespace? c) (set! size-of-next-token 0) (scanner0))
          ((char=? c #\#)       (accept 'sharpsign))
          ((char=? c #\<)       (scanner1))
          ((char=? c #\:)       (scanner2))
          ((char-numeric? c)    (scanner3))
          (else                 (scanner4)))))

; Scanning an identifier that began with a left angle bracket.

(define (scanner1)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (scanner-error "incomplete identifier"))
          ((char=? c #\>)       (consume-char)
                                (let ((c (scan-char)))
                                  (if (or (eof-object? c)
                                          (char-whitespace? c))
                                      (accept-id)
                                      (begin (scanner-warning)
                                             (consume-char)
                                             (scanner1)))))
          (else                 (consume-char) (scanner1)))))

; Scanning a token that began with a colon.

(define (scanner2)
  (let ((c (scan-char)))
    (cond ((eof-object? c)
           (accept-id))
          ((char=? c #\:)
           (consume-char)
           (let ((c (scan-char)))
             (cond ((eof-object? c)
                    (accept-id))
                   ((char=? c #\=)
                    (consume-char)
                    (let ((c (scan-char)))
                      (if (or (eof-object? c)
                              (char-whitespace? c))
                          (accept 'goesto)
                          (scanner4))))
                   (else (scanner4)))))
          (else (scanner4)))))

; Scanning an int or identifier.

(define (scanner3)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (accept-constant 'number))
          ((char-whitespace? c) (accept-constant 'number))
          ((char-numeric? c)    (consume-char) (scanner3))
          (else                 (scanner4)))))

; Scanning an identifier or reserved word.

(define (scanner4)
  (let ((c (scan-char)))
    (cond ((eof-object? c)      (accept-id))
          ((char-whitespace? c) (accept-id))
          ((char=? c #\<)       (consume-char) (scanner-warning) (scanner4))
          ((char=? c #\>)       (consume-char) (scanner-warning) (scanner4))
          (else                 (consume-char) (scanner4)))))

; Reporting and recovery of lexical errors.

(define (scanner-error msg . values)
  (display "ERROR: ")
  (display msg)
  (display " ")
  (for-each (lambda (v)
              (display v)
              (newline))
            values)
  (consume-token!)
  (next-token))

(define (scanner-warning)
  (display "WARNING: Possible misuse of < or >: ")
  (display (substring text-of-next-token 0 size-of-next-token))
  (newline))

|#
