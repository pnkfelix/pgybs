(define pgy-tokens
  (let* ((char-range (lambda (char-a char-b)
                       (let ((a (char->integer char-a))
                             (b (char->integer char-b)))
                         (let loop ((i a))
                           (cond ((> i b) '())
                                 (else (cons (integer->char i)
                                             (loop (+ i 1)))))))))
         (digit `(! ,@(char-range #\0 #\9)))
         (letter `(! ,@(char-range #\a #\z) ,@(char-range #\A #\Z))))
    `((id-a        (+ (! #\a)))
      (id-b        (+ (! #\b)))

      ;; old token classes
      (goesto      ( #\: #\: #\= ))
      (newline     #\newline)
      (sharpsign   #\#)

      (end         ,(string->list "*end"))
      (productions ,(string->list "*productions"))
      (terminals   ,(string->list "*terminals"))

      (id          (,letter (* (! ,letter ,digit))))
      (number      (+ ,digit))

      )))
