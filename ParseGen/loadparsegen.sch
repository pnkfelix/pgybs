; Loads the entire parser generator.

; Note you need to define the parsegen-directory, host-implementation,
; and parsegen-targets parameters before loading this.

; The parser generator is written in ANSI/IEEE Scheme,
; I think, but parts of it are conditionalized to take advantage
; of extensions found in the three implementations I use.
; The recognized values for *host-implementation* are:
;
;     IEEEScheme
;     ChezScheme
;     Gambit
;     MacScheme

;(define *host-implementation* 'MacScheme)
;(define *host-implementation* 'ChezScheme)
;(define *host-implementation* 'IEEEScheme)

(let ((load (lambda (filename)
              (load (string-append (parsegen-directory) filename)))))

  (load "sets.sch")
  (load "follow.sch")
  (load "parsegen.sch")
  (load "parsegen0.sch")

  (let ((targets (parsegen-targets)))

    ;; There is no need to load "parsegen.c.sch" unless a C parser
    ;; is to be generated.  Similarly for "parsegen.pascal.sch"
    ;; and "parsegen.scheme.sch".

    (cond ((memq 'c targets)
           (load "parsegen.c.sch")))
    (cond ((memq 'java targets)
           (load "parsegen.java.sch")))
    (cond ((memq 'modula3 targets)
           (load "parsegen.modula3.sch")))
    (cond ((memq 'pascal targets)
           (load "parsegen.pascal.sch")))
    (cond ((memq 'scheme targets)
           (load "parsegen.scheme.sch")))
    ))
