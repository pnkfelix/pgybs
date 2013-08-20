; Modified from lexgen.java.sch

; Given a dfa in the representation generated by regexp.sch,
; construct a scanner written in Scheme.
;
; In short, this is quick-and-dirty code to save me the trouble
; of writing lexical analyzers in Scheme for my compiler classes.
; I may clean it up later, and make it more general.

; Inelegant hacks.
; Whitespace is handled specially.
; The following symbols are recognized and treated specially.
; This table can be overridden to change or to add more character classes.

(define character-classes
  `((letter char-alphabetic?)
    (digit char-numeric?)
    (space char-whitespace?)
    (lowercase char-lower-case?)
    (uppercase char-upper-case?)))

(define (generate-scheme-for-lexer dfa)
  (let ((dfa (mysort (lambda (entry1 entry2)
                       (let ((blocked1? (null? (nfa-entry-transitions entry1)))
                             (blocked2? (null? (nfa-entry-transitions entry2))))
                         (or (eq? entry1 (car dfa))
                             (and (not blocked1?) blocked2?)
                             (and (not blocked1?)
                                  (not blocked2?)
                                  (<= (nfa-entry-state entry1)
                                      (nfa-entry-state entry2)))
                             (and blocked1?
                                  blocked2?
                                  (<= (nfa-entry-state entry1)
                                      (nfa-entry-state entry2))))))
                     dfa)))
    `(define (scanner0)
       ,@(map (lambda (entry)
                (generate-scheme-for-lexer-state entry dfa))
              dfa)
       (let loop ((c (scanChar)))
         (if (char-whitespace? c)
             (begin (consumeChar)
                    (set! string_accumulator_length 0)
                    (loop (scanChar)))))
       (let ((c (scanChar)))
         (if (char=? c EOF)
             (accept 'eof)
             (state0 c))))))

(define (generate-scheme-for-lexer-state entry dfa)
  (let ((transitions (nfa-entry-transitions entry))
        (gen-char-transitions
         (lambda (transitions)

           ; Given a list of transitions sorted by target,
           ; groups them by target and returns a list of
           ; the groups; each group is itself a list

           (define (group-by-target transitions)
             (define (loop target group groups transitions)
               (cond ((null? transitions)
                      (cons group groups))
                     ((= target (transition-target (car transitions)))
                      (loop target
                            (cons (car transitions) group)
                            groups
                            (cdr transitions)))
                     (else
                      (loop (transition-target (car transitions))
                            '()
                            (cons group groups)
                            transitions))))
             (if (null? transitions)
                 transitions
                 (loop (transition-target (car transitions))
                       '() '() transitions)))

           (let* ((transitions
                   (mysort (lambda (t1 t2)
                             (<= (transition-target t1)
                                 (transition-target t2)))
                           transitions))
                  (grouped-transitions
                   (group-by-target transitions)))

             (generate-scheme-for-char-transitions grouped-transitions dfa))))

        (char-transition? (lambda (t)
                            (char? (transition-token t))))
        (symbol-transition? (lambda (t)
                              (not (char? (transition-token t))))))

    `(define (,(string->symbol (string-append "state"
                                              (number->string
                                               (nfa-entry-state entry))))
              c)
       (case c
         ,@(gen-char-transitions
            (filter char-transition? transitions))
         (else
          ,(let loop ((transitions (filter symbol-transition? transitions))
                      (exp (generate-scheme-for-blocked-state entry)))
             (if (null? transitions)
                 exp
                 (loop (cdr transitions)
                       (generate-scheme-for-lexer-escape-transition
                        exp (car transitions) dfa)))))))))

(define (generate-scheme-for-blocked-state entry)
  (let ((accepts (nfa-entry-accepts entry)))
    (cond ((or (not accepts)
               (null? accepts))
           '(scannerError errIncompleteToken))
          ((eq? accepts 'whitespace)
           '(begin (set! string_accumulator_length 0)
                   (state0 (scanChar))))
          ((symbol? accepts)
           `(accept ',accepts))
          ((and (pair? accepts) (symbol? (car accepts)))
           (generate-scheme-for-blocked-state
            (make-nfa-entry (nfa-entry-state entry)
                            (car accepts)
                            '())))
          (else '???))))

; Given a list of grouped char transitions,
; returns a list of case clauses.

(define (generate-scheme-for-char-transitions transitions dfa)
  (map (lambda (group) (generate-scheme-for-lexer-transition group dfa))
       transitions))

; Returns a case clause.

(define (generate-scheme-for-lexer-transition group dfa)
  (let* ((transition (car group))
         (tokens (map transition-token group))
         (target (transition-target transition))
         (entry (select (lambda (entry)
                          (eqv? target (nfa-entry-state entry)))
                        dfa)))
    `(,tokens
      (consumeChar)
      ,(if (null? (nfa-entry-transitions entry))
           (generate-scheme-for-blocked-state entry)
           `(,(string->symbol
               (string-append "state" (number->string target)))
             (scanChar))))))

; Returns an expression.

(define (generate-scheme-for-lexer-escape-transition exp transition dfa)
  (let* ((token (transition-token transition))
         (target (transition-target transition))
         (entry (select (lambda (entry)
                          (eqv? target (nfa-entry-state entry)))
                        dfa)))
    (let ((handler (assq token character-classes))
          (exp2 (if (null? (nfa-entry-transitions entry))
                    `(begin (consumeChar)
                            ,(generate-scheme-for-blocked-state entry))
                    `(begin (consumeChar)
                            (,(string->symbol
                               (string-append "state"
                                              (number->string target)))
                             (scanChar))))))
      (if handler
          `(if (,(cadr handler) c) ,exp2 ,exp)
          exp2))))

; To do:
;
; Transitions to states that always block can be replaced by in-line code.
; Then the cases for such states are never used.
; (This assumes the start state is not such a state, but big deal.)
;
; Handling of output-port1.
;
; Messages for scannerError.
