(trace load)
(load "loadall.sch")
(generate-scheme-lexer '((id (* (! #\a #\b)))) "generated-lex-pgy.sch")
(generate-scheme "pgy.pg" "generated-parse-pgy.sch" "generated-tables.sch")

(load "lex-pgy-prefix.sch")
(load "generated-lex-pgy.sch")

(load "parse-pgy-prefix.sch")
(load "generated-parse-pgy.sch")
(load "parse-pgy-suffix.sch")
