;; This nasty expression stashes away a copy of a "pristine"
;; environment the "first" time it is called (assuming the global
;; env:0 is not bound elsewhere)

(let* ((i-e interaction-environment)
       (e-get environment-get)
       (e-set! environment-set!)
       (e-copy environment-copy)
       (env (i-e))
       (stashed-env-name 'stashed-env:0)
       (load-count-name 'load-count))
  (cond ((environment-variable? env stashed-env-name)
         (let ((load-count  (e-get env load-count-name)))
           (set-car! load-count (+ 1 (car load-count))))
         (i-e (e-copy (e-get env stashed-env-name))))
        (else
         (let ((e0 (e-copy env)))
           (e-set! e0 stashed-env-name e0)
           (e-set! e0 load-count-name (list 1))
           (i-e e0)))))

(display `(load-count: ,load-count)) (newline)

(define (environment-set-name! env name)
  (vector-like-set! env 3 name)) ;; See env.sch for field offsets.
(define (snapshot-environment name)
  (let ((e (environment-copy (interaction-environment))))
    (environment-set-name! e name)
    e))
(define (rec-snapshot-environment name)
  (let ((e (snapshot-environment name)))
    (environment-set! e name e)
    e))
(define-syntax define-snapshot-environment
  (syntax-rules ()
    ((_ NAME)
     (define NAME (rec-snapshot-environment 'NAME)))))

(define (swap-environment! env saved-name)
  (let ((old-env (environment-copy (interaction-environment)))
        (new-env (environment-copy env)))
    ;; (environment-set-name! old-env saved-name)
    ;; (environment-set! new-env saved-name old-env)
    (interaction-environment new-env)
    (unspecified)))
