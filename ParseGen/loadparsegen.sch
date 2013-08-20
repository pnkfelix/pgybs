; Loads the entire parser generator.

; The parser generator is written in ANSI/IEEE Scheme,
; I think, but parts of it are conditionalized to take advantage
; of extensions found in the three implementations I use.
; The recognized values for *host-implementation* are:
;
;     IEEEScheme
;     ChezScheme
;     Gambit
;     MacScheme

(define *host-implementation* 'MacScheme)
;(define *host-implementation* 'ChezScheme)
;(define *host-implementation* 'IEEEScheme)

(load "/course/csg262/ParseGen/sets.sch")
(load "/course/csg262/ParseGen/follow.sch")
(load "/course/csg262/ParseGen/parsegen.sch")
(load "/course/csg262/ParseGen/parsegen0.sch")

; There is no need to load "parsegen.c.sch" unless a C parser
; is to be generated.  Similarly for "parsegen.pascal.sch"
; and "parsegen.scheme.sch".

;(load "/course/csg262/ParseGen/parsegen.c.sch")
(load "/course/csg262/ParseGen/parsegen.java.sch")
;(load "/course/csg262/ParseGen/parsegen.modula3.sch")
;(load "/course/csg262/ParseGen/parsegen.pascal.sch")
(load "/course/csg262/ParseGen/parsegen.scheme.sch")

