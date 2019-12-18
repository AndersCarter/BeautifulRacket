#lang br
(require jsonic/parser jsonic/tokenizer brag/support)
(require test-engine/racket-tests)

;; Tests
(check-expect (parse-to-datum (apply-tokenizer-maker make-tokenizer "// line commment\n")) '(jsonic-program))
(check-expect (parse-to-datum (apply-tokenizer-maker make-tokenizer "@$ 42 $@")) '(jsonic-program (jsonic-sexp " 42 ")))
(check-expect (parse-to-datum (apply-tokenizer-maker make-tokenizer "hi")) '(jsonic-program (jsonic-char "h") (jsonic-char "i")))
(check-expect (parse-to-datum (apply-tokenizer-maker make-tokenizer "hi\n// comment\n@$ 42 $@"))
              '(jsonic-program (jsonic-char "h")
                               (jsonic-char "i")
                               (jsonic-char "\n")
                               (jsonic-sexp " 42 ")))

(test)
