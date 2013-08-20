; PARSE-ERROR.

; These variables are assigned.

(define total-errors 0)

(define (init-parse-error)
  (set! total-errors 0))

; To avoid a cascade of spurious error messages, only one error
; is counted or reported for any one line of the program.

(define (parse-error nonterminal expected)
  (if (< line-number-of-last-error line-number)
      (begin (set! line-number-of-last-error line-number)
             (set! total-errors (+ total-errors 1))
             (display "ERROR in line ")
             (display line-number)
             (display " while parsing ")
             (display nonterminal)
             (display ":")
             (newline)
             (display "    Encountered ")
             (display (next-token))
             (display " while expecting: ")
             (display expected)
             (newline)))
  (parse-error-recovery nonterminal expected)
  (mk-error))

; For now the error recovery has to be hand-coded.  This is the
; simplest possible error recovery: none, just raise an exception.
; One might consider making it a continuable exception, though it is
; not clear how to properly incorporate that into the framework as it
; stands.

(define (parse-error-recovery nonterminal expected)
  (raise (list 'parse-error-recovery
               (list 'nonterminal: nonterminal
                     'expected: expected
                     'next-token: (next-token)
                     ))))

; Action procedures.

(define (identity x) x)

(define (ignore2 x y) '())

(define (make-actionnum <number>)
  (make-actionproc
   (string->symbol
    (string-append "action-" (symbol->string <number>)))))

(define (make-actionproc <id>)
  (list 'action <id>))

(define (make-default-lhs <rhs>)
  (if last-lhs
      (make-production last-lhs <rhs>)
      (semantic-error "LHS cannot be omitted from first production")))

(define (mk-error)
  (make-grammar '() (make-production 'S '())))

(define (make-grammar0 <garbage> <terminals> <productions>)
  (make-grammar <terminals> <productions>))

(define (make-identifier)
  (string->symbol (accumulatedToken)))

(define (make-number)
  (string->number (accumulatedToken)))

(define (make-production <id> <rhs>)
  (set! last-lhs <id>)
  (list <id> <rhs>))

(define last-lhs #f)

; Conversion to the internal representation.

(define (make-grammar terminals productions)
  (define (terminal? x) (memq x terminals))
  (define (action? x) (pair? x))
  (define (action-name x)
    (let* ((name (cadr x))
           (n (string->number (symbol->string name))))
      (if (and n
               (exact? n)
               (integer? n)
               (not (negative? n)))
          (string->symbol (string-append "action-" name))
          name)))
  (define (generate-nonterminal)
    (set! nonterminal-counter (+ nonterminal-counter 1))
    (generate-identifier "nonterminal" nonterminal-counter))
  (define (generate-action-name)
    (if (actions-take-arguments)
        (set! action-counter (+ action-counter 1)))
    (generate-identifier "action" action-counter))
  (define (generate-identifier s n)
    (string->symbol
     (string-append "anonymous_" s "_" (number->string n))))
  (define (do-production! lhs rhs)
    (let* ((entry (assq lhs grammar)))
      (if entry
          (set-cdr! entry
                    (cons (rhs-loop rhs '())
                          (cdr entry)))
          (begin (set! grammar
                       (cons (list lhs) grammar))
                 (do-production! lhs rhs)))))
  (define (rhs-loop rhs new-rhs)
    (cond ((null? rhs)
           (rhs-loop (list (list 'action (generate-action-name)))
                     new-rhs))
          ((and (action? (car rhs))
                (null? (cdr rhs)))
           (list (reverse new-rhs)
                 (action-name (car rhs))))
          ((action? (car rhs))
           (let ((nt (generate-nonterminal)))
             (set! grammar
                   (cons (list nt
                               (list (reverse new-rhs)
                                     (action-name (car rhs))))
                         grammar))
             (rhs-loop (cons nt (cdr rhs)) '())))
          (else (rhs-loop (cdr rhs)
                          (cons (car rhs) new-rhs)))))
  (define grammar '())
  (define nonterminal-counter 0)
  (define action-counter 0)
  (for-each do-production!
            (map car productions)
            (map cadr productions))
  (reverse (append (map (lambda (t)
                          (list t (list (list (symbol->string t)) '*)))
                        terminals)
                   grammar)))
